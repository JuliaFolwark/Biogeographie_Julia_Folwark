# Absence Sampling

### Liste von Arten

### "Soldanella minima", 440  x
### "Phleum paniculatum", 3025  x 
### "Ruta graveolens", 5231 x
### "Linaria arvensis", 7879  x
### "Lepidium graminifolium", 12122 x 
### "Rumex palustris", 17014  x
### "Lepidium latifolium" 28802  x


## 📦 0) Pakete laden

# Load required libraries
library(readr)       # For reading CSV files
library(sf)          # For spatial vector handling
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(rnaturalearth)  # For natural earth data   
library(lwgeom)

## 1) Creating Sampling Zone for Pseudo-Absences

csv_names = c("Soldanella_minima", "Phleum_paniculatum", "Ruta_graveolens", "Linaria_arvensis","Lepidium_graminifolium", "Rumex_palustris", "Lepidium_latifolium")
print(csv_names[1])
current_name = paste(csv_names[1],"_GBIF_WCVP_ENV_subsampled.csv", sep="")

THINNEDOTS <- read_delim(current_name, #### HIER ANPASSEN
                         delim = ",", 
                         escape_double = FALSE, 
                         trim_ws = TRUE)

THINDOTS <- st_as_sf(THINNEDOTS, 
                     coords = c("lon", "lat"), 
                     crs = 4326)

### ---- Important ----
### Buffer distances in degrees are unreliable at large scales,
### so reproject to an equal-area coordinate system
THINDOTS_EA <- st_transform(THINDOTS, "ESRI:102025")  
### EPSG:102025 = Asia North Albers Equal-Area Conic projection

NO.SAMPLE <- THINDOTS_EA |> 
  st_buffer(dist = 50000) |>          ### 50 km buffer (around presences)
  st_union() |>
  st_make_valid()

DO.SAMPLE <- THINDOTS_EA |> 
  st_buffer(dist = 250000) |>         ### 250 km buffer outer region
  st_union() |>
  st_make_valid()

### Absence region = outer buffer minus inner buffer
ABS.REGION <- st_difference(DO.SAMPLE, NO.SAMPLE) |>
  st_make_valid() |>
  st_union()

### Reproject back to geographic coordinates (WGS84)
ABS.REGION <- st_transform(ABS.REGION, 4326)

## 2) Visualization 

# Ensure valid geometries before plotting
DO.SAMPLE <- st_make_valid(DO.SAMPLE)
NO.SAMPLE <- st_make_valid(NO.SAMPLE)
THINDOTS_EA <- st_make_valid(THINDOTS_EA)

ggplot() +
  geom_sf(data = DO.SAMPLE, fill = "lightgrey", colour = NA) +
  geom_sf(data = NO.SAMPLE, fill = "darkgrey", colour = NA) +
  geom_sf(data = THINDOTS_EA, colour = "red", size = 0.8) +
  theme_bw() +
  labs(title = "Sampling Regions (Presence & Absence Buffers)")

## 3) Preparing Absence Sampling

### Download and prepare world land polygons
world_land <- ne_download(
  scale = "medium",
  type = "land",
  category = "physical",
  returnclass = "sf"
)

### Align CRS between land polygons and absence region
world_land <- st_make_valid(world_land)
ABS.REGION <- st_make_valid(ABS.REGION)
ABS.REGION <- st_transform(ABS.REGION, crs = st_crs(world_land))

### Clip absence region to land only
CLIP.ABS.REGION <- st_intersection(ABS.REGION, world_land)

### Prepare map window zoomed to content
bb <- st_bbox(CLIP.ABS.REGION)
bb_expanded <- bb
bb_expanded["xmin"] <- bb["xmin"] - 5
bb_expanded["xmax"] <- bb["xmax"] + 5
bb_expanded["ymin"] <- bb["ymin"] - 5
bb_expanded["ymax"] <- bb["ymax"] + 5


### Plot clipped region with presences
plot(
  st_geometry(world_land),
  xlim = c(bb_expanded["xmin"], bb_expanded["xmax"]),
  ylim = c(bb_expanded["ymin"], bb_expanded["ymax"]),
  col = "white",
  border = "grey60"
)

plot(st_geometry(CLIP.ABS.REGION), col = "grey85", border = NA, add = TRUE)
plot(st_geometry(THINDOTS), col = "red", pch = 16, cex = 0.5, add = TRUE)


## 4) Absence sampling and combination with presences

SAMPLESIZE <- nrow(THINDOTS)

### Generate random absence points within absence region
ABS <- st_sample(CLIP.ABS.REGION, 
                 size = 3 * SAMPLESIZE, #### HIER EVENTUELL ANPASSEN, WENN ZU WENIG
                 type = "random") |> 
  st_sf() |> 
  st_transform(4326)

### Label and prepare attributes
ABS <- ABS |> 
  mutate(SPEC = 0, 
         STATUS = "ABSENCE")  # corrected STATUS label

### Prepare presence data
PRES <- THINDOTS |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  mutate(SPEC = 1, STATUS = "PRESENCE")

### Add coordinates
PRES <- PRES |> 
  mutate(
    Longitude = st_coordinates(PRES)[, "X"],
    Latitude  = st_coordinates(PRES)[, "Y"]
  )

### Select/keep only the required columns
PRES <- PRES |> 
  dplyr::select(SPEC, STATUS, Longitude, Latitude)

### Prepare absence data similarly
ABS <- ABS |> 
  mutate(
    Longitude = st_coordinates(ABS)[, "X"],
    Latitude  = st_coordinates(ABS)[, "Y"]
  ) |>
  dplyr::select(SPEC, STATUS, Longitude, Latitude)

### Combine presence and absence data
PRESABS <- rbind(
  PRES |> st_drop_geometry(),
  ABS |> st_drop_geometry()
)

### Verify the structure
str(PRESABS)

## 5 Plot all and export to PA_DOTS.csv

plot.new()
plot(st_geometry(CLIP.ABS.REGION), col = "grey85", border = NA)
plot(st_geometry(PRES), col = "red", pch = 16, cex = 0.5, add = TRUE)
plot(st_geometry(ABS), col ="blue", pch = 4, cex = 0.5, add = TRUE)

### Export combined presence–absence data
write.csv(PRESABS, paste(csv_names[7], "_absence_sampled.csv", sep=""), row.names = FALSE)
#### HIER ANPASSEN!!! 