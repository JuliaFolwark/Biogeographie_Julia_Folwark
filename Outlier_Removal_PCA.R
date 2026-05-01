# PCA-basiertes Outlier Removal

### Liste von Arten

### "Soldanella_minima", 440  x
### "Phleum paniculatum", 3025  x 
### "Ruta graveolens", 5231 x
### "Linaria arvensis", 7879  x
### "Lepidium graminifolium", 12122 x 
### "Rumex palustris", 17014  x
### "Lepidium latifolium" 28802  x


## 📦 1) Pakete laden

library(sf)
library(dplyr)
library(ggplot2)
library(gbif.range)
library(CoordinateCleaner)
library(tidyverse)
library(terra)
library(conflicted)
library(rnaturalearthdata)

library(geometry)     # für polyarea()
library(magrittr)
library(FactoMineR)
library(factoextra)
library(megaSDM)
library(data.table)

## 📂 2) Daten einlesen und „nicht-native“ ausschließen

# CSV einlesen (Pfad ggf. anpassen)

file_path = "Soldanella_minima_GBIF_WCVP_ENV.csv"

vorkommen_env <- read.csv(file_path)

# Ausschließen von Vorkommen, die nicht "native" sind
vorkommen_filtered <- vorkommen_env %>%
  dplyr::filter(occurrence_type == "native")

## 🧭 3) Auf lon, lat und ENVIREM-Spalten reduzieren In unserem Beispiel starten die ENVIREM-Variablen bei annualPET.
##Daher wählen wir ab lon, lat und die erste bis letzte ENVIREM-Spalte aus.

# Nur Standort + ENVIREM-Spalten behalten
df_env <- vorkommen_filtered %>%
  select(lon, lat, annualPET:thermicityIndex)   # letzter ENVIREM-Wert laut Beispiel

glimpse(df_env)

## 📊 4) PCA auf ENVIREM-Werten


# Extrahiere nur die numerischen ENVIREM-Werte
env_vars <- df_env %>% select(-lon, -lat)

env_vars
summary(env_vars)

env_vars_clean <- env_vars %>%
  drop_na()

df_env_clean <- df_env %>%
  drop_na()


# PCA mit Skalierung (Standardisierung)
pca_res <- prcomp(env_vars_clean, scale. = TRUE)

# Kombiniere lon/lat mit den ersten 3 PCA-Komponenten
pca_df <- df_env_clean %>%
  select(lon, lat) %>%
  bind_cols(as.data.frame(pca_res$x[, 1:3]))

colnames(pca_df)[3:5] <- c("PC1", "PC2", "PC3")

head(pca_df)

## 🧹 5) Outlier-Entfernung per Convex-Hull Peeling (auf PCA1 & PCA2)


# Funktion definieren
outlier_removal <- function(df, max_iterations = 20, elbow_threshold = 0.05) {
  
  df <- na.omit(df)
  hull_list <- list()
  area_list <- c()
  
  for (i in 1:max_iterations) {
    if (nrow(df) < 10) break
    
    hull_indices <- chull(df$PC1, df$PC2)
    hull_points <- df[hull_indices, ]
    hull_area <- polyarea(hull_points$PC1, hull_points$PC2)
    
    area_list <- c(area_list, hull_area)
    hull_points$Iteration <- as.factor(i)
    hull_list[[i]] <- hull_points
    
    df <- df[-hull_indices, ]
    
    if (i > 1) {
      area_change <- (area_list[i - 1] - area_list[i]) / area_list[i - 1]
      if (area_change < elbow_threshold) break
    }
  }
  
  hull_df <- do.call(rbind, hull_list)
  
  # Visualisierung
  p1 <- ggplot(df, aes(x = PC1, y = PC2)) +
    geom_point(alpha = 0.6, color = "grey") +
    geom_polygon(data = hull_df, aes(x = PC1, y = PC2, fill = Iteration, group = Iteration),
                 color = "black", alpha = 0.3) +
    scale_fill_manual(values = rainbow(length(unique(hull_df$Iteration)))) +
    theme_minimal() +
    labs(title = "Convex Hull Peeling Process", x = "PCA 1", y = "PCA 2")
  
  print(p1)
  
  return(df)
}

# Anwenden der Outlier-Funktion
pca_peeled <- outlier_removal(pca_df, max_iterations = 20, elbow_threshold = 0.25)

## 🌍 6) Eigene Funktion mit dplyr + base R für Umweltfilterung erzeugen

# 📘 Funktion: 3D Environment-based Subsampling
samp_env_3D <- function(df, 
                        vars = c("PC1", "PC2", "PC3"),
                        nbins = 10,
                        n_per_bin = 1,
                        seed = NULL,
                        lon = "lon", lat = "lat") {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Prüfen, ob alle gewünschten Spalten vorhanden sind
  stopifnot(all(c(vars, lon, lat) %in% names(df)))
  
  # Zugriff vereinfachen
  env <- df[, vars]
  
  # Gleiche Klassenbreiten pro Dimension
  # wichtig, um räumlich induzierte Dichten nicht zu übernehmen
  bin_edges <- lapply(env, function(x)
    seq(from = min(x, na.rm = TRUE),
        to   = max(x, na.rm = TRUE),
        length.out = nbins + 1))
  
  DT <- data.table::as.data.table(df)
  for (i in seq_along(vars)) {
    DT[[paste0("bin_", vars[i])]] <- 
      findInterval(DT[[vars[i]]], bin_edges[[i]], all.inside = TRUE)
  }
  
  # 3D Join der drei Bins = "Voxel"-ID
  DT[, voxel_id := paste(bin_PC1, bin_PC2, bin_PC3, sep = "_")]
  
  # Pro Voxel zufällige Stichprobe
  subsampled <- DT[, .SD[sample(.N, min(.N, n_per_bin))], by = voxel_id]
  
  # Optional einfache Visualisierung (Projektion PC1, PC2)
  g <- ggplot() +
    geom_point(data = df, aes_string(x = vars[1], y = vars[2]), 
               color = "grey70", alpha = 0.3) +
    geom_point(data = subsampled, 
               aes_string(x = vars[1], y = vars[2], color = vars[3]),
               size = 1.5, alpha = 0.8) +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(title = "3D Binning-based Environmental Subsampling",
         subtitle = paste0("Bins per axis = ", nbins, 
                           ", max points per voxel = ", n_per_bin),
         x = vars[1], y = vars[2], color = vars[3])
  
  print(g)
  
  return(as.data.frame(subsampled))
}

subsampled_df <- samp_env_3D(pca_peeled, 
                             vars = c("PC1","PC2","PC3"),
                             nbins = 10,
                             n_per_bin = 5,
                             seed = 42,
                             lon = "lon", lat = "lat")

dim(pca_peeled)
dim(subsampled_df)

## ✳️ 7) Visualisierung

ggplot() +
  geom_point(data = pca_df, aes(PC1, PC2), 
             color = "grey80", 
             alpha = 0.5, size = 4) +
  geom_point(data = pca_peeled, aes(PC1, PC2), 
             color = "red", 
             alpha = 0.7, size = 3) +
  geom_point(data = subsampled_df, aes(PC1, PC2), 
             color = "blue", 
             alpha = 0.9, size = 2) +
  theme_minimal() +
  labs(title = "PCA1 vs PCA2 — Raw (grau), 
       Nach Outlier-Filtering (rot), 
       Nach Subsampling (blau)",
       x = "PCA 1", y = "PCA 2")

## Speichern des finalen Dataframes

file_out <- sub("\\.csv$", "_subsampled.csv", file_path)
file_out

write.csv(subsampled_df, file_out, row.names = FALSE)

