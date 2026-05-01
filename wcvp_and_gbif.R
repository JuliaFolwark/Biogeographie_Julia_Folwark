# Einmalig installieren (von GitHub)
# remotes::install_github("KewScience/rwcvp")
# devtools::install_github("matildabrown/rWCVPdata")
# Für Export auch sf installieren


install.packages("sf")

library(sf)
library(rWCVP)
library(dplyr)
library(ggplot2)

# Verbreitungsdaten für Teucrium scordium abrufen
map <- wcvp_distribution("Teucrium scordium", wcvp_names = wcvp_names, wcvp_distribution = wcvp_distribution)
# Karte anzeigen
wcvp_distribution_map(map)
# Als Shapefile exportieren (optional)
st_write(map, "Teucrium_scordium_WCVP.shp")

# Struktur des sf-Objekts anzeigen
print(map)
# Attributtabelle anzeigen
head(map)
# Nur native Vorkommen anzeigen

native_only <- map %>%
  filter(introduced == 0)
print(native_only)


# Für mehrere Arten produzieren: 

# Liste von Arten
arten <- c("Soldanella minima",
           "Phleum paniculatum",
           "Ruta graveolens",
           "Linaria arvensis",
           "Lepidium graminifolium",
           "Rumex palustris",
           "Lepidium latifolium")

# Schleife über alle Arten
for (art in arten) {
  # Daten abrufen
  map <- wcvp_distribution(art, wcvp_names = wcvp_names, wcvp_distribution = wcvp_distribution)
  # Karte erstellen und speichern
  p <- wcvp_distribution_map(map)
  file_name <- paste0("R_Karte_", gsub(" ", "_", art), ".png")
  ggsave(filename = file_name, p, width = 12, height = 8, dpi = 300)
  # Shapefile speichern
  dateiname <- paste0(gsub(" ", "_", art), "_WCVP.shp")
  st_write(map, dateiname, delete_dsn = TRUE)
  print(paste("Fertig:", art))
}


# ________________________________________________________________
# Ab hier Skript für GBIF Datenbank-Integration

# Notizen: Wv Datenpunkte rein theoretisch für die einzelnen Arten in der Excel-Tabelle? 
# Liste von Arten
#"Soldanella minima", 440 <- erstmal darauf Fokus > Fertig
#"Phleum paniculatum", 3025 > Fertig
#"Ruta graveolens", 5231 > Fertig
#"Linaria arvensis", 7879 > Fertig
#"Lepidium graminifolium", 12122 > Fertig
#"Rumex palustris", 17014 > Fertig
#"Lepidium latifolium" 28802  > Fertig


remotes::install_github("8Ginette8/gbif.range")

library(sf)
library(ggplot2)
library(gbif.range)
library(CoordinateCleaner)
library(tidyverse)
library(terra)
library(conflicted)

# Durchgang für Soldanella minima
# am 26.11.2025: Passe an für weitere Durchgänge 

## 1 WCVP-Polygon laden

wcvp <- st_read("Soldanella_minima_WCVP.shp")
st_crs(wcvp)
# Falls notwendig: in WGS84 umprojizieren
wcvp <- st_transform(wcvp, 4326)
# Von sf in Spatial Vector konvertieren
wcvp_v <- terra::vect(wcvp)

## 2 GBIF-Daten per gbif.range herunterladen

obs.pt <- get_gbif(sp_name = "Soldanella minima",   #Artname
                   conf_match = 95,                #%Übereinstimmung der Namen
                   geo = wcvp_v,                   #auf WCVP-Regionen beschränken 
                   centroids = TRUE)               #Rasterpunkte mitnehmen

## 3 Visualisierung des Rohdatensatzes

countries <- rnaturalearth::ne_countries(type = "countries", 
                                         returnclass = "sv")
terra::plot(countries, 
            col = "#bcbddc")
points(obs.pt[, c("decimalLongitude","decimalLatitude")], 
       pch = 20, 
       col = "#99340470", 
       cex = 1.5)

## 4 Datenbereinigung mit CoordinateCleaner

obs.clean <- clean_coordinates(
  x = obs.pt,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "acceptedScientificName",
  tests = c(
    "cen", 
    "institutions", 
    #"seas", 
    "zeros",
    "capitals"
  )
)

summary(obs.clean$.summary)               # checken, wieviele als TRUE (gut)

mean(obs.clean$.summary, na.rm = TRUE)    # Proportion

obs.clean <- obs.pt[obs.clean$.summary, ] # Nur summary==TRUE behalten

## 5 Visualisierung der bereinigten GBIF-Daten + WCVP Regionen

terra::plot(countries, 
            col = "#bcbddc", 
            border = NA)    # Basiskarte

terra::plot(wcvp, 
            col = "#fc0303",  
            border = NA, 
            add = TRUE)    # WCVP-Regionen

### 1. Rohpunkte groß, grau, transparent
points(
  obs.pt[, c("decimalLongitude", "decimalLatitude")],
  pch = 20,
  col = "#4d4d4d60",   # grau, 60 = starke Transparenz
  cex = 1.5
)

### 2. Bereinigte Punkte klein und farbig
points(
  obs.clean[, c("decimalLongitude", "decimalLatitude")],
  pch = 20,
  col = "#d95f02",     # orange
  cex = 0.7
)

## 6 GBIF-obs in ein sf-Objekt umwandeln und mit WCVP verschneiden

obs.sf <- st_as_sf(
  obs.clean,                       # Tabellenobjekt
  coords = c("decimalLongitude",   # Argument Koordinatenspalten
             "decimalLatitude"),
  crs = 4326                       # Argument Koordinatensystem
)

# Falls noch nicht geladen:
# wcvp <- st_read("./wcvp_level3.shp")  #dort wo Shapefile gespeichert wurde

# Wichtig!!!: prüfen, wie die Attributspalte NATIVE/INTRODUCED heißt:
names(wcvp)

# Jetzt räumlich GBIF & WCVP verschneiden (Spatial Join)
# Das gibt eine Liste: für jeden Punkt die ID(s) der getroffenen Polygone.
ix <- st_intersects(obs.sf, wcvp)

# Nun ordnen wir jedem Punkt den Status zu:

# Welche Polygone? (NA = kein Treffer)
poly_id <- sapply(ix, function(x) if (length(x) == 0) NA_integer_ else x[1])

# Punktvektor bekommt neue Spalte "occurrence_type" zugeordnet
obs.sf$occurrence_type <- wcvp$occrrn_[poly_id]

# Region Level 3 Namen zuordnen
obs.sf$Level3_N <- wcvp$Level3_N[poly_id]

# Coords= extrahieren & CRS behalten
obs_coords <- obs.sf %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()  # Geometriespalte entfernen -> vermeidet schlechte Kommatrennung

# Export
write.csv(
  obs_coords,
  "Lepidium_latifolium_GBIF_WCVP.csv",
  row.names = FALSE
)

##### Für Bericht: 
library(kableExtra)  # Für kable_styling
library(dplyr)      # Für Datenmanipulation

# Liste der Arten
arten <- c(
  "Soldanella minima",
  "Phleum paniculatum",
  "Ruta graveolens",
  "Linaria arvensis",
  "Lepidium graminifolium",
  "Rumex palustris",
  "Lepidium latifolium"
)

# Leere Datenframes für die Ergebnisse erstellen
ergebnisse <- data.frame(
  Art = character(),
  Rohdaten = numeric(),
  Bereinigte_Daten = numeric(),
  Reduktion_Perzent = numeric(),
  stringsAsFactors = FALSE
)

# Funktion zum Zählen der Zeilen in den CSV-Dateien
zaehle_datensaetze <- function(art_name) {
  # Dateinamen konstruieren
  original_file <- paste0(gsub(" ", "_", art_name), "_GBIF_WCVP.csv")
  
  # Prüfen, ob die Datei existiert
  if (file.exists(original_file)) {
    # Daten einlesen
    daten <- read_csv(original_file)
    
    # Anzahl der Rohdaten (ursprüngliche GBIF-Daten)
    roh_daten <- switch(art_name,
                        "Soldanella minima" = 440,
                        "Phleum paniculatum" = 3025,
                        "Ruta graveolens" = 5231,
                        "Linaria arvensis" = 7879,
                        "Lepidium graminifolium" = 12122,
                        "Rumex palustris" = 17014,
                        "Lepidium latifolium" = 28802,
                        NA)
    
    # Anzahl der bereinigten Daten (Zeilen in der CSV-Datei)
    bereinigt <- nrow(daten)
    
    # Reduktion in Prozent berechnen
    reduktion <- round((1 - bereinigt / roh_daten) * 100, 1)
    
    # Ergebnisse zurückgeben
    return(data.frame(
      Art = art_name,
      Rohdaten = roh_daten,
      Bereinigte_Daten = bereinigt,
      Reduktion_Perzent = reduktion
    ))
  } else {
    message(paste("Datei nicht gefunden:", original_file))
    return(NULL)
  }
}

# Für jede Art die Daten zählen
for (art in arten) {
  ergebnis <- zaehle_datensaetze(art)
  if (!is.null(ergebnis)) {
    ergebnisse <- rbind(ergebnisse, ergebnis)
  }
}

# LaTeX-Tabelle erstellen
latex_tabelle <- kable(ergebnisse, format = "latex", booktabs = TRUE, digits = 1) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Ausgabe der LaTeX-Tabelle
cat(latex_tabelle)





