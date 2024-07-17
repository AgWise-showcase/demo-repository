
##############################################################################################
## 1. source packages and functions needed for the analytics
##############################################################################################
rm(list=ls())
packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel","MuMIn","ggpmisc","sf","cluster","h2o",
                       "limSolve", "lpSolve", "Rquefts", "terra", "Metrics", "factoextra", "raster", "rgdal", "rstudioapi", "git2r")

installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


#################################################################################################################
# 2.Get the current script's directory
#################################################################################################################

# Initialize the repository: yo need to adjust "D:/OneDrive - CGIAR/AgWise/Dahsboard/AgWise_Demo/" to your file structure
repo <- repository("D:/OneDrive - CGIAR/AgWise/Dahsboard/AgWise_Demo/demo-repository")

# Get the working directory of the repository
repo_root <- workdir(repo)

# Specify the absolute path of your files
input_path <- "Data/Fertilizer_recom/"
geoSpatialData_path <- "Data/geospatial/"
output_path <- "Data/Fertilizer_recom/Intermediate/"

data_realtive_path <- sub(repo_root, "", input_path)
data_full_path <- file.path(repo_root, data_realtive_path)

gs_realtive_path <- sub(repo_root, "", geoSpatialData_path)
gs_full_path <- file.path(repo_root, gs_realtive_path)


result_realtive_path <- sub(repo_root, "", output_path)
result_full_path <- file.path(repo_root, result_realtive_path)

# create a folder for result
if (!dir.exists(result_full_path)){
  dir.create(file.path(result_full_path), recursive = TRUE)
}



#################################################################################################################
# 3. read input data and define global variables 
#################################################################################################################
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"
ds <- readRDS(paste0(data_full_path, "/modelReady.RDS"))
INS <- readRDS(paste0(data_full_path, "/modelReady_INS.RDS"))
Soil_PointData_AOI <- readRDS(paste0(data_full_path, "/Soil_PointData_AOI.RDS"))
topo <-  readRDS(paste0(data_full_path, "/topoData_AOI.RDS"))
AEZ <- readOGR(dsn=gs_full_path,  layer="AEZ_DEM_Dissolve")


#################################################################################################################
# 4.  train ML model using h2o
#################################################################################################################
### for each model test with and without district and refY (or any other variable you wish for your specifc case) 

INS2 <- INS %>% 
  dplyr::select(-c(N_base_supply,P_base_supply,K_base_supply)) %>% 
  unique()
ds2 <- ds %>% dplyr::select(c("TLID", "N","P","K","blup"))
dssr2 <- ds2 %>% inner_join(INS2) %>% unique()

response <- "blup"
predictors <- dssr2 |> names()
predictors <- predictors[!predictors %in% c("TLID", "blup","Ya" ,"season","season_AB", "lat", "lon", "Experiment", "AltClass","AEZs_no","AltClass2", "expCode")]
predictors2 <- predictors[!predictors %in% c("district", "refY")]

h2o.init()
ML_data.h2o <- as.h2o(dssr2)

#create a random training-test split of our data ## should be possible to do it by missing one
ML_inputData_split <- h2o.splitFrame(data = ML_data.h2o, ratios = 0.7, seed = 444)
training_data <- ML_inputData_split[[1]]
test_data <- ML_inputData_split[[2]]


#  grid search to tune hyper parameters 
hyperparams_gbm <- list(
  ntrees = seq(500, 1000, 100),
  max_depth = seq(4, 8, 2)
)

hyperparams_RF <- list(
  ntrees = seq(500, 1000, 100),
  max_depth = seq(4, 8, 2), 
  mtries = c(3, 4, 5) 
)


grid_gbm_CJ <- h2o.grid(
  algorithm = "gbm",
  x = predictors,
  y = response,
  grid_id = "hyperparams_gbm",
  hyper_params = hyperparams_gbm,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)

grid_gbm_CJ_fvar <- h2o.grid(
  algorithm = "gbm",
  x = predictors2,
  y = response,
  grid_id = "hyperparams_gbm",
  hyper_params = hyperparams_gbm,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


grid_RF_CJ <- h2o.grid(
  algorithm = "randomForest",
  x = predictors,
  y = response,
  grid_id = "rf_grid",
  hyper_params = hyperparams_RF,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


grid_RF_CJ_fvar <- h2o.grid(
  algorithm = "randomForest",
  x = predictors2,
  y = response,
  grid_id = "rf_grid",
  hyper_params = hyperparams_RF,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


# Get the best hyper parameters
# Write data to the file
saveRDS(grid_gbm_CJ, paste0(result_full_path, "grid_gbm_CJ.RDS"))
best_hyperParm_GB <- h2o.getModel(grid_gbm_CJ@model_ids[[1]])

saveRDS(grid_gbm_CJ_fvar, paste(result_full_path, "grid_gbm_CJ_fvar.RDS", sep="/"))
best_hyperParm_GB_fvar <- h2o.getModel(grid_gbm_CJ_fvar@model_ids[[1]])

saveRDS(grid_RF_CJ, paste(result_full_path, "grid_RF_CJ.RDS", sep="/"))
best_hyperParm_RF <- h2o.getModel(grid_RF_CJ@model_ids[[1]])

saveRDS(grid_RF_CJ_fvar, paste(result_full_path, "grid_RF_CJ_fvar.RDS", sep="/"))
best_hyperParm_RF_fvar <- h2o.getModel(grid_RF_CJ_fvar@model_ids[[1]])



### fit the model with the tuned hyper parameters: 
ML_gbm_CJ <- h2o.gbm(x = predictors,
                     y = response,
                     ntrees = best_hyperParm_GB@parameters$ntrees,
                     max_depth = best_hyperParm_GB@parameters$max_depth,
                     training_frame = training_data,
                     validation_frame = test_data,
                     keep_cross_validation_predictions = TRUE,
                     nfolds = 5,
                     seed = 444)

model_path_gbm_CJ <- h2o.saveModel(object = ML_gbm_CJ, path = result_full_path, force = TRUE)
print(model_path_gbm_CJ)
# model_path_gbm_CJ <- "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\GBM_model_R_1721043314285_1"
model_GB_CJ <- h2o.loadModel(model_path_gbm_CJ)
local_model_GB_CJ <- h2o.download_model(model_GB_CJ, path = result_full_path)
ML_gbm_CJ <- h2o.upload_model(local_model_GB_CJ)



ML_gbm_CJ_fvar <- h2o.gbm(x = predictors2,
                     y = response,
                     ntrees = best_hyperParm_GB_fvar@parameters$ntrees,
                     max_depth = best_hyperParm_GB_fvar@parameters$max_depth,
                     training_frame = training_data,
                     validation_frame = test_data,
                     keep_cross_validation_predictions = TRUE,
                     nfolds = 5,
                     seed = 444)

model_path_gbm_CJ_fvar <- h2o.saveModel(object = ML_gbm_CJ_fvar, path = result_full_path, force = TRUE)
print(model_path_gbm_CJ_fvar)
# model_path_gbm_CJ_fvar <- "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\GBM_model_R_1721043314285_2"
model_GB_CJ_fvar <- h2o.loadModel(model_path_gbm_CJ_fvar)
local_model_GB_CJ_fvar <- h2o.download_model(model_GB_CJ_fvar, path = result_full_path)
ML_gbm_CJ_fvar <- h2o.upload_model(local_model_GB_CJ_fvar)


ML_RF_CJ <- h2o.randomForest(x = predictors,
                                     y = response,
                                     ntrees =  best_hyperParm_RF@parameters$ntrees,
                                     max_depth = best_hyperParm_RF@parameters$max_depth,
                                     mtries =   best_hyperParm_RF@parameters$mtries,
                                     training_frame = training_data,
                                     validation_frame = test_data,
                                     keep_cross_validation_predictions = TRUE,
                                     nfolds = 5,
                                     seed = 444)
 
model_path_rf_CJ <- h2o.saveModel(object = ML_RF_CJ, path = result_full_path, force = TRUE)
print(model_path_rf_CJ)
# model_path_rf_CJ <- "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\DRF_model_R_1721206198954_1"
model_RF_CJ <- h2o.loadModel(model_path_rf_CJ)
local_model_RF_CJ <- h2o.download_model(model_RF_CJ, path = result_full_path)
model_path_rf_CJ <- h2o.upload_model(local_model_RF_CJ)



ML_RF_CJ_fvar <- h2o.randomForest(x = predictors2,
                             y = response,
                             ntrees =  best_hyperParm_RF_fvar@parameters$ntrees,
                             max_depth = best_hyperParm_RF_fvar@parameters$max_depth,
                             mtries =   best_hyperParm_RF_fvar@parameters$mtries,
                             training_frame = training_data,
                             validation_frame = test_data,
                             keep_cross_validation_predictions = TRUE,
                             nfolds = 5,
                             seed = 444)

model_path_rf_CJ_fvar <- h2o.saveModel(object = ML_RF_CJ_fvar, path = result_full_path, force = TRUE)
print(model_path_rf_CJ_fvar)
# model_path_rf_CJ_fvar <-  "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\DRF_model_R_1721206198954_2"
model_RF_CJ_fvar <- h2o.loadModel(model_path_rf_CJ_fvar)
local_model_RF_CJ_fvar <- h2o.download_model(model_RF_CJ_fvar, path = result_full_path)
ML_RF_CJ_fvar <- h2o.upload_model(local_model_RF_CJ_fvar)




### model diagnostics
h2o.levels(test_data["refY"])
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Very high")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "High")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Medium")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Low")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Very low")



h2o.residual_analysis_plot(ML_gbm_CJ,test_data)
h2o.varimp_plot(ML_gbm_CJ, num_of_features = 12)
h2o.partialPlot(object = ML_gbm_CJ, test_data, cols = c("refY", "N", "P", "K"))
h2o.shap_summary_plot(ML_gbm_CJ, test_data)

h2o.residual_analysis_plot(ML_gbm_CJ_fvar,test_data)
h2o.varimp_plot(ML_gbm_CJ_fvar, num_of_features = 12)
h2o.partialPlot(object = ML_gbm_CJ_fvar, test_data, cols = c("N", "P", "K"))
h2o.shap_summary_plot(ML_gbm_CJ_fvar, test_data)

h2o.residual_analysis_plot(ML_RF_CJ,test_data)
h2o.varimp_plot(ML_RF_CJ, num_of_features = 12)
h2o.partialPlot(object = ML_RF_CJ, test_data, cols = c("refY", "N", "P", "K"))
h2o.shap_summary_plot(ML_RF_CJ, test_data)

h2o.residual_analysis_plot(ML_RF_CJ_fvar,test_data)
h2o.varimp_plot(ML_RF_CJ_fvar, num_of_features = 12)
h2o.partialPlot(object = ML_RF_CJ_fvar, test_data, cols = c("N", "P", "K"))
h2o.shap_summary_plot(ML_RF_CJ_fvar, test_data)

## leave one out cross validation
Leave1_GB_RF <- NULL 
for(k in unique(dssr2$TLID)){
  print(k)
  tdata <- dssr2[dssr2$TLID == k, ]
  tdata.h2o <- as.h2o(tdata)
  predResponseGB <- as.data.frame(h2o.predict(object = ML_gbm_CJ, newdata = tdata.h2o))
  predResponseGBfv <- as.data.frame(h2o.predict(object = ML_gbm_CJ_fvar, newdata = tdata.h2o))
  predResponseRF <- as.data.frame(h2o.predict(object = ML_RF_CJ, newdata = tdata.h2o))
  predResponseRFfv <- as.data.frame(h2o.predict(object = ML_RF_CJ_fvar, newdata = tdata.h2o))
  tdata$Y_pred_GB <- predResponseGB$predict
  tdata$Y_pred_GBfv <- predResponseGBfv$predict
  tdata$Y_pred_RF <- predResponseRF$predict
  tdata$Y_pred_RFfv <- predResponseRFfv$predict
  Leave1_GB_blupY <- rbind(Leave1_GB_blupY, tdata )
}


saveRDS(Leave1_GB_blupY, paste(result_full_path, "Leave1_GB_RF.RDS", sep="/"))
Leave1_GB_RF <- readRDS(paste(result_full_path, "Leave1_GB_RF.RDS", sep="/"))


Leave1_GB_RF <- Leave1_GB_RF %>% 
  dplyr::select(TLID, N, P, K, province, district, AEZ, Y_pred_GB ,Y_pred_GBfv, Y_pred_RF, Y_pred_RFfv, blup) %>% 
  dplyr::rename(Observed=blup,
                GB_Predicted = Y_pred_GB,
                GB_Predicted_fVar = Y_pred_GBfv, 
                RF_Predicted = Y_pred_RF, 
                RF_Predicted_fvar = Y_pred_RFfv )

 gg_GB <- ggplot(Leave1_GB_RF, aes(x = Observed, y = GB_Predicted)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(size = 3) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 5,
                        label.y = 0.95) +
  ggpmisc::stat_poly_eq(use_label(c("R2")),
                        formula = y ~ x, size = 5,
                        label.y = 0.88) +
  geom_text(data = Leave1_GB_RF %>% 
              dplyr::summarise(rmse = sqrt(sum((GB_Predicted - Observed)**2)/n()),
                               Observed = 0) %>%
              mutate(GB_Predicted = c(47)),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 5, hjust = 0.0)  + 
  xlab("Observed potato yield [t/ha]") +
  ylab("Predicted potato yield [t/ha], with GB model")+
  labs(title = ~ atop(paste('Observed yield versus Predicted yield with ML model'),
                      'Yield ~ NPK fertilizer + refY + season + district + soil + AEZ + topography')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12), axis.title = element_text(size=14))


# ggsave("D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\plots\\ML_GB_CJ_yield.pdf", 
#        gg_GB, width=10, height = 7)

ggsave(paste0(result_full_path, "/ML_GB_CJ_yield.pdf"), gg_GB, width=10, height = 7)



gg_GB_fvar <- ggplot(Leave1_GB_RF, aes(x = Observed, y = GB_Predicted_fVar)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(size = 3) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 5,
                        label.y = 0.95) +
  ggpmisc::stat_poly_eq(use_label(c("R2")),
                        formula = y ~ x, size = 5,
                        label.y = 0.88) +
  geom_text(data = Leave1_GB_RF %>% 
              dplyr::summarise(rmse = sqrt(sum((GB_Predicted_fVar - Observed)**2)/n()),
                               Observed = 0) %>%
              mutate(GB_Predicted_fVar = c(47)),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 5, hjust = 0.0)  + 
  xlab("Observed potato yield [t/ha]") +
  ylab("Predicted potato yield [t/ha], with GB model")+
  labs(title = ~ atop(paste('Observed yield versus Predicted yield with ML model'),
                      'Yield ~ NPK fertilizer + soil + AEZ + topography')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12), axis.title = element_text(size=14))


# ggsave("D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\plots\\ML_GB_CJ_fVar_yield.pdf", 
#        gg_GB_fvar, width=10, height = 7)
ggsave(paste0(result_full_path, "/ML_GB_CJ_fVar_yield.pdf"), gg_GB_fvar, width=10, height = 7)





gg_RF <- ggplot(Leave1_GB_RF, aes(x = Observed, y = RF_Predicted)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(size = 3) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 5,
                        label.y = 0.95) +
  ggpmisc::stat_poly_eq(use_label(c("R2")),
                        formula = y ~ x, size = 5,
                        label.y = 0.88) +
  geom_text(data = Leave1_GB_RF %>% 
              dplyr::summarise(rmse = sqrt(sum((RF_Predicted - Observed)**2)/n()),
                               Observed = 0) %>%
              mutate(RF_Predicted = c(40)),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 5, hjust = 0.0)  + 
  xlab("Observed potato yield [t/ha]") +
  ylab("Predicted potato yield [t/ha], with RF model")+
  labs(title = ~ atop(paste('Observed yield versus Predicted yield with ML RF model'),
                      'Yield ~ NPK fertilizer + refY + season + district + soil + AEZ + topography')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12), axis.title = element_text(size=14))


# ggsave("D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\plots\\ML_RF_CJ_yield.pdf", 
#        gg_RF, width=10, height = 7)

ggsave(paste0(result_full_path, "/ML_RF_CJ_yield.pdf"), gg_RF, width=10, height = 7)





gg_RF_fvar <- ggplot(Leave1_GB_RF, aes(x = Observed, y = RF_Predicted_fvar)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(size = 3) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 5,
                        label.y = 0.95) +
  ggpmisc::stat_poly_eq(use_label(c("R2")),
                        formula = y ~ x, size = 5,
                        label.y = 0.88) +
  geom_text(data = Leave1_GB_RF %>% 
              dplyr::summarise(rmse = sqrt(sum((RF_Predicted_fvar - Observed)**2)/n()),
                               Observed = 0) %>%
              mutate(RF_Predicted_fvar = c(37)),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 5, hjust = 0.0)  + 
  xlab("Observed potato yield [t/ha]") +
  ylab("Predicted potato yield [t/ha], with RF model")+
  labs(title = ~ atop(paste('Observed yield versus Predicted yield with ML RF model'),
                      'Yield ~ NPK fertilizer + soil + AEZ + topography')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12), axis.title = element_text(size=14))


# ggsave("D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\plots\\ML_RF_CJ_fVar_yield.pdf", 
#        gg_RF_fvar, width=10, height = 7)
ggsave(paste0(result_full_path, "/ML_RF_CJ_fVar_yield.pdf"), gg_RF_fvar, width=10, height = 7)




head(Leave1_GB_RF)
Leave1_GB_RF <- Leave1_GB_RF[, c("TLID","N",  "P",  "K","province", "district", "AEZ", "Observed", "GB_Predicted", "GB_Predicted_fVar", "RF_Predicted", "RF_Predicted_fvar" )]

pred_Ydiff_ms <- Leave1_GB_RF %>%
  tidyr::gather(variable, value, Observed:RF_Predicted_fvar ) %>%
  dplyr::group_by(TLID, variable) %>%
  mutate(refY = ifelse(N > 75 & P > 30 & K > 50, value, NA),
         refY = mean(refY, na.rm=TRUE),
         dY = refY - value) %>%
  filter(!(N > 75 & P > 30 & K > 50)) %>%
  dplyr::select(-c(refY, value)) %>%
  spread(variable, dY) %>%
  dplyr::rename(dY = Observed , dYGB = GB_Predicted , dYGB_fv=GB_Predicted_fVar, dYRF = RF_Predicted , dYRF_fv=RF_Predicted_fvar) 

pred_Ydiff_ms <- pred_Ydiff_ms[, c("TLID","N","P","K", "province", "district", "AEZ","dY", "dYGB", "dYGB_fv","dYRF", "dYRF_fv")]

pred_Ydiff_ms <- pred_Ydiff_ms %>% 
  tidyr::gather(variable, value, dYGB:dYRF_fv) %>%
  dplyr::mutate(variable = mapvalues(variable, from = c("dYGB", "dYGB_fv", "dYRF", "dYRF_fv"), 
                              to = c("GB with district, refY and season", "GB without district, refY and season","RF with district, refY and season", "RF without district, refY and season")))
head(pred_Ydiff_ms)


gg_GB_RF_Yeffect <- ggplot(data = pred_Ydiff_ms, aes(x = dY, y = value,)) +
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = pred_Ydiff_ms %>% 
              group_by(variable) %>% 
              dplyr::summarise(rmse = sqrt(sum((value - dY)**2)/n()),
                               value = -1,
                               dY = -4,
                               value = 17),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("Observed yield effect relative to reference treatment [t/ha]\n") +
  ylab("ML predicted yield effect relative to reference treatment [t/ha]") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

# ggsave("D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\plots\\ML_GB_RF_Yieldeffect.pdf", 
#        gg_GB_RF_Yeffect, width=10, height = 7)

ggsave(paste0(result_full_path, "/ML_GB_RF_Yieldeffect.pdf"), gg_GB_RF_Yeffect, width=10, height = 7)




#############################################################################
## create yield response curve for N application to compare the response curve with the QUEFTS + ML method
#############################################################################


fr <- data.frame("N" = seq(0, 180, 1),
                 "P" = 22,
                 "K" = 42)
### selecting the gradient boosting method with district, refY and season, the NPK rates can be computed 
## given ML works within the range of observed values, create a combination of NPK rates withing the range
## prepare the geostationary variables at the desired resolution on regular grid or by AEZ or ...
## use the model you selected to predict, for the sake of demonstration we will create 220 virtual treatments if NPK 



## AEZ and altitude info for AOI
## soil data for AOI
# Soil_PointData_AOI <- readRDS("D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Soil_PointData_AOI.RDS")
# AEZ <- readOGR(dsn="D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\geospatial\\",  layer="AEZ_DEM_Dissolve")

Soil_PointData_AOI <- Soil_PointData_AOI %>% dplyr::rename(province = NAME_1, district = NAME_2) %>% unique()
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
gpsPoints <- Soil_PointData_AOI[, c("longitude", "latitude")]
gpsPoints$longitude <- as.numeric(gpsPoints$longitude)
gpsPoints$latitude <- as.numeric(gpsPoints$latitude)
RAW_AEZ_trial <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))

gpsPoints <- Soil_PointData_AOI[, c("longitude", "latitude")]
gpsPoints$longitude <- as.numeric(gpsPoints$longitude)
gpsPoints$latitude <- as.numeric(gpsPoints$latitude)
RAW_AEZ_trial <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))

RAW_AEZ_trial <- RAW_AEZ_trial %>%
  dplyr::select(c(Names_AEZs)) %>%
  dplyr::rename(AEZ = Names_AEZs) %>% 
  cbind(Soil_PointData_AOI[, c("longitude", "latitude")])


Soil_AEZ_AOI <- Soil_PointData_AOI %>%
  left_join(RAW_AEZ_trial)



pred_df <- Soil_AEZ_AOI %>%
  dplyr::mutate_if(is.character, as.factor) %>% 
  left_join(topo) 


## create virtual treat
N <- seq(0, 100, by=10)
P <- seq(0, 40, by=10)
K <- seq(0, 40, by=10)
NPK <- expand.grid(N = N, P = P, K = K) 
dim(NPK)


## call trained model
h2o.init()

model_path_gbm_CJ <- paste0(result_full_path, "/GBM_model_R_1721043314285_1")
# model_path_gbm_CJ <- "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\GBM_model_R_1721043314285_1"
model_GB_CJ <- h2o.loadModel(model_path_gbm_CJ)
local_model_GB_CJ <- h2o.download_model(model_GB_CJ, path = pathOut)
ML_gbm_CJ <- h2o.upload_model(local_model_GB_CJ)


## for demo we will work only with 100 GPS points
yieldSimulation <- c()
for(i in 1:100){#nrow(pred_df)){
  print(i)
  rowPred <- pred_df[i,] %>% 
    dplyr::mutate(N = NPK$N[i], P = NPK$P[i], K = NPK$K[i]) 
  
  refClass_yield <- NULL
  for(referenceY in c("Very low", "Low", "Medium", "High", "Very high")){
    rowPred$refY <- as.factor(referenceY) 
    rowPred[, c("N","P","K","c_tot_top","c_tot_bottom", "ca_top","ca_bottom","clay_tot_psa_top",
                "clay_tot_psa_bottom", "db_od_top", "db_od_bottom","ecec_f_top","ecec_f_bottom",
                "k_top","k_bottom", "mg_top","mg_bottom","n_tot_ncs_top","n_tot_ncs_bottom","oc_top",
                "oc_bottom","p_top","p_bottom","ph_h2o_top","ph_h2o_bottom", "s_top","s_bottom",
                "sand_tot_psa_top","sand_tot_psa_bottom", "silt_tot_psa_top","silt_tot_psa_bottom",
                "texture_class_top","texture_class_bottom", "SOM_top","SOM_bottom", "PWP_top",
                "PWP_bottom","FC_top","FC_bottom","SWS_top", "SWS_bottom","K_0_30","N_0_30",
                "P_0_30","Ptot_0_30", "province","district","refY","AEZ","alt")]
    
    tdata.h2o <- as.h2o(rowPred)
    predYield <- as.data.frame(h2o.predict(object = ML_gbm_CJ, newdata = tdata.h2o))
    rowPred$Yield_pred <- predYield$predict
    refClass_yield <- rbind(refClass_yield, rowPred )
   }
  yieldSimulation <- rbind(yieldSimulation, refClass_yield)
}

## Now that we have for every location yield as a response to NPK, the determination for the 
## yield at current practice, NPK requirement for a target yield can be read from the curve 
## optimizing for nutrient use efficiency, profit, etc can also be done using this data. 

write.csv(yieldSimulation, paste0(result_full_path,"/ML_NPKrates.csv"), row.names = FALSE)








