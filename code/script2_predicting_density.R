# Scrip 2: Relation between abundance and habitat suitability
# Alejandro de la Fuente Pinero
# contact: alejandro.delafuentepinero1@my.jcu.edu.au

-----------------------
library(raster)
library(tidyverse)
library(sf)
-----------------------
  

setwd("/abundance_vs_suitability/data")
# Step 1 ------------------------------------------------------------------

# Using the habitat suitability predicted from ensemble species distribution modeling, we explore
# its correlation with observed density.

# load ensemble model
em <- raster("MTHORN_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")

plot(em)
# load population density

abundance <- read.csv("MTHORN_density.csv")
hl_abundance <- abundance %>% rename(density = MTHORN)

#find relation between pop size and habitat suitability

coordinates(hl_abundance) <-  ~ longdecimal + latdecimal
plot(hl_abundance, add = T)

# for each pixel extract habitat suitability
rasValue <- raster::extract(em, hl_abundance) 

#create a data frame containing density and habitat suitability
hl_predictors <- cbind(hl_abundance,rasValue)

hl_predictors <- as.data.frame(hl_predictors)

hl_predictors <- hl_predictors %>% dplyr::select(-c(latdecimal,longdecimal)) 

hl_predictors <- hl_predictors %>% rename("hs" = 
                                            "c.302..55..171..342..948..899..73..350..293..147..344..671..527..")


#correlation hs~density(observed)
df_cor <- tibble(x = hl_predictors$hs,
                 y = hl_predictors$density) 

cor.test(df_cor$x, df_cor$y, method = "spearman")


library(mgcv)
library(gratia)
library(performance)


fit <- gam(density ~ s(hs), method = "REML", 
           family = tw(link = "log"),
           data = hl_predictors)

performance::check_distribution(fit)

gratia::appraise(fit)

gratia::draw(fit)

summary(fit)


# once we have made sure that the model performed well, we proceed to predict over the whole area


em_points_df <- as.data.frame(rasterToPoints(em))

new_data <- as.data.frame(em_points_df$layer)

new_data <- new_data %>% dplyr::rename(hs = `em_points_df$layer`)

pred <- as.data.frame(predict(fit, newdata = new_data, type = 'response'))

# prepare dataframe to rasterize
pred_newdata <- cbind(new_data, pred)

raster_input <- cbind(em_points_df, pred_newdata) %>% rename(pred = `predict(fit, newdata = new_data, type = "response")`)

raster_input <- raster_input %>% dplyr::select(x,y,pred)

# create raster with density values

coordinates(raster_input) <- ~x+y

em_pred_raster <- rasterize(raster_input, em)

em_pred_raster <- em_pred_raster$pred
em_pred_raster[em_pred_raster<0] <- 0

plot(em_pred_raster)


# End of single species example. The link to the full data-set with all species abundance can be found 
# at the end of the paper. Files are already formatted to be inputted to the script so the exploration of
# other species predictions should be straightforward. If any error or problem should be found, please
# contact Alejandro de la Fuente at alejandro.delafuentepinero1@my.jcu.edu.au



# Influence of species traits on the relationship -------------------------

results <- read.csv("results.csv")

results$taxa <- factor(results$taxa, levels = c("mammal", "bird", "reptile", "frog"))

# check correlation among predictors
corr <- results %>% select(log_presence, log_mass, pot_dispersal2, realized_dist)

cor(corr, method = "spearman")

summary(model <- lm(obsDen_HS_spearman ~ log_presence + log_mass + pot_dispersal2 + realized_dist + taxa, data = results))


check_model(model)
car::vif(model)


summary(model_2 <- lm(obsDen_HS_spearman ~ log_presence + log_mass + pot_dispersal2 + realized_dist, data = results))

check_model(model_2)
car::vif(model_2)


step <- stepAIC(model_2, direction = "backward")

step$anova

summary(model_3 <- lm(obsDen_HS_spearman ~ log_presence + pot_dispersal2, data = results))


#################
##End of script##
#################