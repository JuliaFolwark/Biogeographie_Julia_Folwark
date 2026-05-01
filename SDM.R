# SDM

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
library(sf)          # For spatial vector handling
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(rnaturalearth)  # For natural earth data   
library(lwgeom)
library(readr)
library(terra)

PA_DOTS <- read_csv("Lepidium_latifolium_absence_sampled.csv", 
                    col_types = cols(STATUS = col_skip()))

PRSABS <- vect(PA_DOTS,geom=c("Longitude", "Latitude"),
               crs="epsg:4326", keepgeom=FALSE)

# Adapt path 2 raster !!
env_files <- list.files("./ENVIREM",
                        pattern = "\\.tif$",
                        full.names = TRUE)

PREDS <- rast(env_files)
names(PREDS)

names(PREDS) <- sub("^\\d+_env_current_", "", names(PREDS)) # was passiert hier?
names(PREDS)

plot(PREDS[[6]],main= 'growingDegDays0')

#--------------------------------------------------------------#
library(sdm)
D <- sdmData(SPEC ~ . 
             + scale(., method="center") 
             + select(., method ='vifstep'),
             train = PRSABS, 
             predictors = PREDS, 
             parallelSetting = list(ncore = 6, 
                                    method = 'parallel')
)
D
#--------------------------------------------------------------#

SDM <- sdm(SPEC~.,data=D,
           methods=c(
             'bioclim','brt','cart','domain.dismo','fda','glm',
             'glmnet','mars','maxlike','mda',
             'glmpoly','rf','rpart','svm'))
#--------------------------------------------------------------#

SDM

write.sdm(SDM,'Lepidium_latifolium', overwrite=TRUE)  ### hier anpassen! 
# SDM <- read.sdm("./SDM.sdm")    ### wenn man wieder einlesen will 


gui(SDM)

#--------------------------------------------------------------#

#### Project/predict models to region of species range ####

# crop the PREDS raster to the PRSABS vector extent
RANGE_EXT <- crop(PREDS, PRSABS)

# the following writing steps can take long time. Plan accordingly!

# memmax - the maximum amount of RAM (in GB) that terra can use 
# when processing a raster dataset. Should be less than what is 
# detected (see mem_info, and higher values are ignored. 
# Set it to a negative number or NA to ignore this value).


pBIO <- predict(SDM,    
                RANGE_EXT,
                filename="pBIO.grd", 
                species=NULL,
                method="bioclim",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE)

pBRT <- predict(SDM, 
                RANGE_EXT, 
                filename="pBRT.grd", 
                species=NULL,
                method="brt",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

pCRT <- predict(SDM, 
                RANGE_EXT, 
                filename="pCRT.grd", 
                species=NULL,
                method="cart",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

pDOM <- predict(SDM, 
                RANGE_EXT, 
                filename="pDOM.grd", 
                species=NULL,
                method="domain.dismo",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

pFDA <- predict(SDM, 
                RANGE_EXT, 
                filename="pFDA.grd", 
                species=NULL,
                method="fda",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

# pGAM <- predict(SDM, 
#                 RANGE_EXT, 
#                 filename="pGAM.grd", 
#                 species=NULL,
#                 method="gam",
#                 replication=NULL,
#                 run=NULL,
#                 mean=FALSE,
#                 overwrite=TRUE,
#                 memmax=3)

pGLM <- predict(SDM, 
                RANGE_EXT, 
                filename="pGLM.grd", 
                species=NULL,
                method="glm",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

pGLMN <- predict(SDM, 
                 RANGE_EXT, 
                 filename="pGLMN.grd", 
                 species=NULL,
                 method="glmnet",
                 replication=NULL,
                 run=NULL,
                 mean=FALSE,
                 overwrite=TRUE,
                 memmax=3)

pMARS <- predict(SDM, 
                 RANGE_EXT, 
                 filename="pMARS.grd", 
                 species=NULL,
                 method="mars",
                 replication=NULL,
                 run=NULL,
                 mean=FALSE,
                 overwrite=TRUE,
                 memmax=3)

# pMXT <- predict(SDM, 
#                 RANGE_EXT, 
#                 filename="pMXT.grd", 
#                 species=NULL,
#                 method="maxent",
#                 replication=NULL,
#                 run=NULL,
#                 mean=FALSE,
#                 overwrite=TRUE,
#                 memmax=3)         ### Funktioniert bei mir nicht! 

pMXL <- predict(SDM, 
                RANGE_EXT, 
                filename="pMXL.grd", 
                species=NULL,
                method="maxlike",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

pMDA <- predict(SDM, 
                RANGE_EXT, 
                filename="pMDA.grd", 
                species=NULL,
                method="mda",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

pGLMP <- predict(SDM,
                 RANGE_EXT, 
                 filename="pRNGR.grd", 
                 species=NULL,
                 method="glmpoly",
                 replication=NULL,
                 run=NULL,
                 mean=FALSE,
                 overwrite=TRUE,
                 memmax=3)

pRF <- predict(SDM, 
               RANGE_EXT, 
               filename="pRF.grd", 
               species=NULL,
               method="rf",
               replication=NULL,
               run=NULL,
               mean=FALSE,
               overwrite=TRUE,
               memmax=3)

pRFP <- predict(SDM, 
                RANGE_EXT, 
                filename="pRFP.grd", 
                species=NULL,
                method="rpart",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

pSVM <- predict(SDM, 
                RANGE_EXT, 
                filename="pSVM.grd", 
                species=NULL,
                method="svm",
                replication=NULL,
                run=NULL,
                mean=FALSE,
                overwrite=TRUE,
                memmax=3)

#--------------------------------------------------------------#

pALL<-c(
  pBIO,
  pBRT,
  pCRT,
  pFDA,
  pDOM,
  pGLM,
  pGLMN,
  pMARS,
  pMXL,
  pMDA,
  pRF,
  pGLMP,
  pRFP,
  pSVM
)

pALL <- pALL[[order(names(pALL))]]