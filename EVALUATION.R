### Liste von Arten

### "Soldanella minima", 440  
### "Phleum paniculatum", 3025  
### "Ruta graveolens", 5231 
### "Linaria arvensis", 7879
### "Lepidium graminifolium", 12122
### "Rumex palustris", 17014
### "Lepidium latifolium" 28802 

# Load required libraries
library(readr)
library(terra)
library(sdm)
library(ggplot2)

# For evaluating the SDM quality you need:
# a) your species PRSABS SpatVector
# b) your respective predictions (*.grd-files)
# c) your respective SDM object (*.sdm)
#--------------------------------------------------------------#

# get PresenceAbsence file as Spatvect

PA_DOTS <- read_csv("Soldanella_minima_absence_sampled.csv", #### Hier anpassen! 
                    col_types = cols(STATUS = col_skip()))

PRSABS <- vect(PA_DOTS,geom=c("Longitude", "Latitude"),
               crs="epsg:4326", keepgeom=FALSE)
#--------------------------------------------------------------#

# Define folder containing your .grd raster files (predictions)
raster_folder <- "./SDM_Modelldateien/Soldanella_minima/"  #### Hier anpassen! 

# List only .grd files *that start with "p"*
grd_files <- list.files(path=raster_folder, pattern = "^p.*\\.grd$", full.names = TRUE)

# Load all found rasters as a SpatRaster list
p_grds <- lapply(grd_files, rast)


# Some output predictions are not in 0-1 scale
# for comparison, we need to rescale them:
# Function to rescale raster values to 0–1 if outside range
rescale_01 <- function(r) {
  r_min <- global(r, "min", na.rm = TRUE)[[1]]
  r_max <- global(r, "max", na.rm = TRUE)[[1]]
  if (r_min < 0 | r_max > 1) {
    message(paste("Rescaling:", names(r)))
    r <- (r - r_min) / (r_max - r_min)
  }
  return(r)
}

# Apply rescaling function to each raster
p_grds_rescaled <- lapply(p_grds, rescale_01)


# Stack all rasters into one SpatRaster (multi-band raster)
pALL_rescaled <- rast(p_grds_rescaled)
#--------------------------------------------------------------#

# Load the previously saved/written SDM model

SDM <- sdm::read.sdm("./SDM_Modelldateien/Soldanella_minima/Soldanella_minima.sdm")   #### Hier anpassen! 

AUC <- getEvaluation(SDM, stat =c('AUC'), wtest = "training")

#--------------------------------------------------------------#
#--------------------------------------------------------------#
#--------------------------------------------------------------#

# Mean of all models
MEANSDM <- ensemble(SDM,
                    pALL_rescaled,
                    filename='MEAN.grd',
                    setting=list(method='unweighted'),
                    overwrite = TRUE)

# Ensemble of all models
ENSSDM <- ensemble(SDM,
                   pALL_rescaled,
                   filename='ENS.grd',
                   setting=list(method='weighted',stat='AUC'),
                   overwrite = TRUE)

## Visualising the outputs of the different ensembles: 
ENS <-c(MEANSDM,ENSSDM) # 2 objects stacked in a single object 
names(ENS) <-c('ENS_MEAN','ENS_AUC')

plot(ENS)

# Where are the differences?
ENS_DIFF <- ENS$ENS_MEAN-ENS$ENS_AUC
col_blue.white.red <- colorRampPalette(c("blue", "white", "red"))

plot(ENS_DIFF,
     col = col_blue.white.red(100),
     main = "Ensemble Difference Mean - AUC")

# Variability between models
VARSDM <- ensemble(SDM, 
                   pALL_rescaled,
                   filename='VAR.grd',
                   setting = list(method=c('stdv','cv'), opt=2),
                   overwrite = TRUE)

plot(VARSDM$ensemble_SPEC_stdev,
     main = "Standard deviation of ENS_MEAN")


# Overall Model Uncertainty
UNCSDM <- ensemble(SDM,pALL_rescaled,
                   filename='UNC.grd',
                   setting = list(method='uncertainty', opt=2),
                   overwrite = TRUE)

plot(UNCSDM$ensemble_entropy,
     main = "Inconsistency among SDM methods")
#--------------------------------------------------------------#
#--------------------------------------------------------------#

#### CALIBRATION PLOTS ####

# Measuring calibration capacity of statistical models #

#### Hier habe ich die Umbenennung von der pALL rückgängig gemacht! -> ist auskommentiert

pALL_renamed <- pALL_rescaled
names(pALL_renamed) <- sub(".*__m_", "", names(pALL_rescaled))
pred_vals <- terra::extract(pALL_rescaled, PRSABS)[, -1]
# names(pALL_rescaled) <- sub(".*__m_", "", names(pALL_rescaled))
# pred_vals <- terra::extract(pALL_rescaled, PRSABS)[, -1]
obs <- PRSABS$SPEC
# caBIO <- calibration(obs, pred_vals$bioclim)
# plot(caBIO)
# caRF <- calibration(obs, pred_vals$rf)
# plot(caRF)

#--------------------------------------------------------------#

cal_df <- do.call(
  rbind,
  lapply(names(pred_vals), function(m) {
    
    ca <- sdm::calibration(
      x = obs,
      p = pred_vals[[m]]
    )
    
    cbind(
      ca@calibration,
      method = m,
      statistic = ca@statistic
    )
  })
)


names(cal_df) <- c(
  "pred",
  "obs",
  "weight",
  "method",
  "statistic"
)

p_calib <-ggplot(cal_df, aes(pred, obs)) +
  geom_line(colour = "steelblue") +
  geom_point(aes(size = weight), colour = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal(xlim = c(0,1), ylim = c(0,1)) +
  scale_size(range = c(1, 4), guide = "none") +
  facet_wrap(~ method, ncol = 5) +
  labs(
    x = "Predicted probability",
    y = "Observed proportion"
  ) +
  theme_minimal()


# ggsave(
#   filename = "CalibrationPlot_Soldanella_minima.png",   #### Hier anpassen! 
#   plot = p_calib,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

CALIBDF <- tapply(cal_df$statistic, cal_df$method, unique)
CALIBDF <- as.data.frame(CALIBDF)
tapply(cal_df$statistic, cal_df$method, unique)

# write.csv(
#   cal_df,
#   file = "CalibrationData_Soldanella_minima.csv",   #### Hier anpassen! 
#   row.names = FALSE
# )

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#

##### AB HIEERRR 

### CREATE RANGE POLYGON BASED ON CUSTOM WEIGHTED ENSEMBLE ###

## 0. Create a new weighting vector for ensemble based on AUC and CALibration
library(sdm)
library(terra)
library(dplyr)
library(rmapshaper)   # optional, for polygon smoothing

CAL <- distinct(cal_df[, c("method", "statistic")])

AUC_CAL <- cbind(CAL,AUC)

CAL_AUC <- AUC_CAL %>%
  rename(CAL = statistic) %>%
  select(method, modelID, CAL, AUC)
View(CAL_AUC)


## 1. Normalize AUC and CAL
#  we want both on the same 0–1 scale before combining:
CAL_AUC$AUC_norm <- CAL_AUC$AUC / max(CAL_AUC$AUC, na.rm = TRUE)
CAL_AUC$CAL_norm <- CAL_AUC$CAL / max(CAL_AUC$CAL, na.rm = TRUE)


## 2. Combine normalized metrics into a single weight
# example for equal weighting
CAL_AUC$custom_weight <- (CAL_AUC$AUC_norm + CAL_AUC$CAL_norm) / 2

# example with more emphasis on CAL (30% AUC, 70% CAL):
CAL_AUC$custom_weight <- 0.3 * CAL_AUC$AUC_norm + 0.7 * CAL_AUC$CAL_norm
weights_vector <- CAL_AUC$custom_weight

# ensure it matches your model order in SDM - #### hier wenn ein Model rauspacken- wenn rauslöschen: 

weights_vector <- CAL_AUC$custom_weight[match(SDM@run.info$modelID, CAL_AUC$modelID)]

#### Hier zum Rausnehmen vom Modell- wenn zu schlecht: 

CAL_AUC <- CAL_AUC[CAL_AUC$modelID != 10, ]


## 3. Run the ensemble with your custom weights
#### FIXME: There is a problem here with weighted means in ens_custom.grd
### Spezifisch für Phleum_paniculatum und Soldanella_minima

## DEBUG: %%%%%%%%%%%%%%%%%
# Prüfe, ob Gewichte NA, 0 oder negativ sind
print(weights_vector)  # Zeigt die aktuellen Gewichte an

# Ersetze NA/0/negative Werte durch einen kleinen positiven Wert (z. B. 0.01)
weights_vector[is.na(weights_vector) | weights_vector <= 0] <- 0.01

# Normalisiere die Gewichte, sodass sie wieder auf 1 summieren
weights_vector <- weights_vector / sum(weights_vector)
# %%%%%%%%%%%%%%%%%%%%

ENS_CUST <- ensemble(
  SDM,
  newdata = pALL_rescaled,
  filename = "ens_custom.grd",
  overwrite = TRUE,
  setting = list(
    method = "weighted",
    weights = weights_vector
  )
)



#--------------------------------------------------------------#

## 5. Extract the evaluation results from your ensemble
EVA <- getEvaluation(SDM, 
                     stat = c('threshold', 
                              'sensitivity', 
                              'specificity'),
                     opt = 8,  # NMI
                     wtest = 'training')

### Wenn irgendwelche Werte nicht passen: 
# Entferne Model ID x
EVA <- EVA[EVA$modelID != 10, ]

threshold_NMI <- mean(EVA$threshold, na.rm = TRUE)



## 6. Apply the threshold (binarize the ensemble)
#  Convert probabilistic suitability values to binary 0/1 presence-absence:

# Binarize ensemble
ENS_BIN <- classify(ENS_CUST,
                    rcl = matrix(c(-Inf, threshold_NMI, 0, 
                                   threshold_NMI, Inf, 1),
                                 ncol = 3, byrow = TRUE),
                    include.lowest = TRUE)

names(ENS_BIN) <- "presence_binary"
plot(ENS_BIN)

# SMOOTHING
# Apply a simple 5x5 mean filter
kernel <- matrix(1, 5, 5)
ens_smooth <- focal(ENS_BIN, w = kernel, fun = mean, na.policy = "omit")
# Re-binarize again: any cell >0.5 becomes presence
ens_smooth_bin <- ens_smooth > 0.5
names(ens_smooth_bin) <- "presence_smooth"
plot(ens_smooth_bin)

# Convert raster to polygons (terra format)
range_poly <- as.polygons(ens_smooth_bin, dissolve = TRUE)
# Drop cells coded 0
range_poly <- range_poly[range_poly$presence_smooth == 1, ]

library(sf)
library(terra)
library(rmapshaper)
# Convert terra polygons to sf before processing
range_poly_sf <- st_as_sf(range_poly)
# Simplify/smooth boundaries
range_poly_smooth <- rmapshaper::ms_simplify(range_poly_sf, 
                                             keep = 0.25)
# Optional: drop tiny islands (if desired)
range_poly_smooth <- ms_filter_islands(range_poly_smooth, 
                                       min_area = 50e6)  # 50 km²

plot(ENS_CUST, main = "Continuous Ensemble Prediction")
contour(ENS_CUST, add = TRUE)

plot(ens_smooth_bin, main = "Smoothed Binary Presence", 
     col = c("lightgrey","darkgreen"))
plot(range_poly_smooth, border = "black", lwd = 1.2, add = TRUE)

# optional export
poly_smooth <- vect(range_poly_smooth)
writeVector(poly_smooth, "range_polygon.shp", overwrite = TRUE)


#### ZUSÄTZLICH FÜRS SPEICHERN

# Plot der glätteten Binärkarte speichern
png(filename = "Smoothed_Binary_Presence_Soldanella_minima.png",  # Anpassen!
    width = 1200, height = 800)
plot(ens_smooth_bin, main = "Smoothed Binary Presence",
     col = c("lightgrey", "darkgreen"))
plot(range_poly_smooth, border = "black", lwd = 1.2, add = TRUE)
dev.off()

# Polygon als Shapefile exportieren (bereits in deinem Code)
writeVector(poly_smooth, "range_polygon_Soldanella_minima.shp", overwrite = TRUE)  # Anpassen!


write.csv(CAL_AUC, "Model_performance_evaluation_Soldanella_minima.csv", row.names = FALSE)

