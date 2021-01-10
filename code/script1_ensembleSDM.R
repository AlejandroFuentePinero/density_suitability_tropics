# Scrip 1: Ensemble species distribution modelling using BIOMOD2
# Alejandro de la Fuente Pinero
# contact: alejandro.delafuentepinero1@my.jcu.edu.au



# Load libraries ----------------------------------------------------------

-----------------------
library(biomod2)
library(raster)
library(tidyverse)
library(usdm)
library(sf)
-----------------------

  
  
setwd("/abundance_vs_suitability/data")

# Load occurrences --------------------------------------------------------

spp_presence <- read.csv("MTHORN_presence_thin.csv")
spp_presence <- spp_presence %>% rename(MTHORN = species_id)
spp_presence$MTHORN <- 1

spp_absences <- read.csv("MTHORN_absence_thin.csv")
spp_absences <- spp_absences %>% rename(MTHORN = species_id)
spp_absences$MTHORN <- 0

pa <- rbind(spp_absences, spp_presence)

# Load predictors ---------------------------------------------------------

predictors_names <- list.files(path="/abundance_vs_suitability/data/predictors", 
                          pattern='tif$', 
                          full.names=TRUE)

predictor_loaded <- lapply(predictors_names, raster)

myExpl <- stack(predictor_loaded)

# SDMs --------------------------------------------------------------------

#format names
myRespName <- "MTHORN"
myResp <- as.numeric(pa[,myRespName])
myRespXY <- pa[,c("longdecimal", "latdecimal")]


#Data formating
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.nb.rep = 1,
                                     PA.nb.absences = 7000,
                                     PA.strategy = "disk",
                                     PA.dist.min = 1000,
                                     na.rm = T)

myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c("GBM","MAXENT.Phillips","MARS", "GAM", "CTA", "ANN", "SRE", "FDA","RF"),
  NbRunEval=3,
  DataSplit=80,
  Prevalence=0.5,
  VarImport=5,
  models.eval.meth = 'TSS',
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"_model",sep=""))


# get all models evaluation
myBiomodModelEval <- as.data.frame(get_evaluations(myBiomodModelOut))

# print the dimnames of this object
dimnames(myBiomodModelEval)
# let's print the TSS scores 
myBiomodModelEval["TSS","Testing.data",,,]
# print variable importances
var.imp <- as.data.frame(get_variables_importance(myBiomodModelOut))

# Ensemble modeling 

myBiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = 'TSS',
  eval.metric.quality.threshold = 0.7,
  prob.mean = T,
  prob.cv = T,
  prob.ci = T,
  prob.ci.alpha = 0.05,
  prob.median = T,
  committee.averaging = T,
  prob.mean.weight = T,
  prob.mean.weight.decay = 'proportional' )

# get evaluation scores
eval_ensemble <- as.data.frame(get_evaluations(myBiomodEM))

# Projection
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  proj.name = 'projected_model',
  selected.models = 'all',
  binary.meth = c('TSS'),
  compress = 'xz',
  clamping.mask = F,
  output.format = '.grd')

myCurrentProj <- get_predictions(myBiomodProj)

# Ensemble forcasting

myBiomodEF <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM,
  projection.output = myBiomodProj)

#################
##End of script##
#################