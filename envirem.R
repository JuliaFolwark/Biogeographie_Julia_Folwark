# Artenvorkommen und Umweltlayer (ENVIREM) mit R

library(sf)
library(dplyr)
library(ggplot2)
library(gbif.range)
library(CoordinateCleaner)
library(tidyverse)
library(terra)
library(conflicted)
library(rnaturalearthdata)

### Liste von Arten

### "Soldanella minima", 440  >  x
### "Phleum paniculatum", 3025  >  x
### "Ruta graveolens", 5231 > x
### "Linaria arvensis", 7879 > x
### "Lepidium graminifolium", 12122 > x
### "Rumex palustris", 17014 > x
### "Lepidium latifolium" 28802 >  x


## 1. CSV‑Datei mit Koordinaten einlesen

### Beispielhafte Datei:
vorkommen <- read_csv("Lepidium_latifolium_GBIF_WCVP.csv")

### Tabellenstruktur erkunden
View(vorkommen)
head(vorkommen)

## 2. Punktkarte mit Weltkarte

vorkommen.sf <- st_as_sf(vorkommen,
                         coords = c("lon",
                                    "lat"),
                         crs = 4326)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +
  geom_sf(data = vorkommen.sf, color = "darkred", size = 1.5, alpha = 0.6) +
  coord_sf() +
  theme_bw() +
  labs(title = "Vorkommenspunkte für Lepidium latifolium")

## 3. Genauigkeit der Koordinaten prüfen

summary(vorkommen.sf$coordinateUncertaintyInMeters)

ggplot(vorkommen.sf, aes(x = coordinateUncertaintyInMeters)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_bw() +
  labs(x = "Koordinaten-Ungenauigkeit (m)", y = "Anzahl Datensätze")

# Filter: nur genaue Koordinaten behalten
vorkommen_clean <- vorkommen.sf %>%
  dplyr::filter(!is.na(coordinateUncertaintyInMeters),
         coordinateUncertaintyInMeters <= 10000)
nrow(vorkommen); nrow(vorkommen_clean)

## 4. ENVIREM‑Umweltdaten laden

# Lade Rasterlayer

env_files <- list.files("ENVIREM/",
                        pattern = "\\.tif$",
                        full.names = TRUE)

env <- rast(env_files)
env

names(env) # irgendwie unnötig lang und kompliziert

names(env) <- sub("^\\d+_env_current_", "", names(env)) # was passiert hier?
names(env)

## 5. Datenexploration ausgewählter Umweltvariablen

# Auswahl dreier Variablen
sel_env <- env[[c("annualPET",
                  "aridityIndexThornthwaite",
                  "climaticMoistureIndex")]]

# Histogramme
plot_hist_var <- function(r, title) {
  tibble(x = values(r, na.rm = TRUE)) %>%
    ggplot(aes(x)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    theme_bw() +
    labs(title = title, x = "Wert", y = "Häufigkeit")
}

p1 <- plot_hist_var(sel_env[[1]], "Annual PET (mm)")
p2 <- plot_hist_var(sel_env[[2]], "Aridity Index (Thornthwaite)")
p3 <- plot_hist_var(sel_env[[3]], "climaticMoistureIndex")
p1; p2; p3

### 📘 Hinweis: In ariden Regionen (kleine Werte) hat eine Änderung von 50-100 mm eine viel größere ökologische Bedeutung als 2000-2050 mm im humiden Bereich. 
### Wie macht der climaticMoistureIndex diese Unterschiede sichtbar?

## 5a. Globale Karte der Niederschlagsverteilung

plot(sel_env[["climaticMoistureIndex"]],
     main = "climaticMoistureIndex",
     col = hcl.colors(64, "BluGrn", rev = TRUE))

## 6. Raster für ein ausgewähltes Land zuschneiden

### Zusatz: Exportieren Sie die Karte als PNG (png(“cMI.png”) … dev.off()). Erklären Sie in 2-3 Sätzen, warum Niederschlag im unteren Bereich größere ökologische Bedeutung hat.

country_name <- "Italy"  # <- frei wählbar

country <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(admin == country_name)
land_vect <- vect(country)

sel_country <- crop(sel_env, land_vect, mask = TRUE)

plot(sel_country)

## 7. Extraktion von Zellwerten an Vorkommenspunkten

points_vect <- vect(vorkommen_clean)

vals <- terra::extract(env, points_vect, ID = FALSE)

vorkommen_env <- cbind(vorkommen_clean, vals)

head(vorkommen_env)

## 8. Ergebnis speichern

# Coords= extrahieren & CRS behalten
env_coords <- vorkommen_env %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()  # Geometriespalte entfernen

# Export
write.csv(
  env_coords,
  "Lepidium_latifolium_GBIF_WCVP_ENV.csv",
  row.names = FALSE
)
