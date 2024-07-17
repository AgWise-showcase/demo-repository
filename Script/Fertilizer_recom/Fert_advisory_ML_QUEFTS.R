

##############################################################################################
## 1. source packages and functions needed for the analytics
##############################################################################################
rm(list=ls())
# install.packages("rgdal", repos="http://R-Forge.R-project.org")

packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel","MuMIn","ggpmisc","sf","cluster","h2o",
                       "limSolve", "lpSolve", "Rquefts", "terra", "Metrics", "factoextra", "raster", "rgdal")

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
generic_scriptpath <- "Script/Fertilizer_recom/"

data_realtive_path <- sub(repo_root, "", input_path)
data_full_path <- file.path(repo_root, data_realtive_path)

gs_realtive_path <- sub(repo_root, "", geoSpatialData_path)
gs_full_path <- file.path(repo_root, gs_realtive_path)

result_realtive_path <- sub(repo_root, "", output_path)
result_full_path <- file.path(repo_root, result_realtive_path)

script_realtive_path <- sub(repo_root, "", generic_scriptpath)
script_full_path <- file.path(repo_root, script_realtive_path)

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
supply <- readRDS(paste0(data_full_path, "/soilINS_revQUEFTS.RDS"))
Soil_PointData_AOI <- readRDS(paste0(data_full_path, "/Soil_PointData_AOI.RDS"))
topo <-  readRDS(paste0(data_full_path, "/topoData_AOI.RDS"))
AEZ <- readOGR(dsn=gs_full_path,  layer="AEZ_DEM_Dissolve")
rwlake <- st_read(paste0(gs_full_path, "/RWA_Lakes_NISR.shp"))

##############################################################################
### Machine learning model: to relate the soil nutrient supply from reverse QUEFTS to geo-spatial variables 
## ds is the field trial data with GPS, NPK rate and yield. BLUP is added after running LME and reference yield is indicated   
## supply is the apparent soil nutrient supply (ANS) NPK a estimated using reverse QUEFTS. See Fert_advisory_QUEFTS for more detail. 
## INS is the trial id, ANS, GPS, soil data,  AEZ and altitude info put together. For the geospatial data sourtcing see the AgWise routine "source data" 
##############################################################################

source(paste0(script_full_path, "/generic/QUEFTS_functions.R"))

ins <- INS %>%
  dplyr::select(-c(lon, lat,Experiment, expCode, season, Ya,AltClass2, AltClass,AEZs_no)) %>% unique()


INS2 <- ins %>% 
  dplyr::select(-c(N_base_supply,P_base_supply,K_base_supply)) %>% 
  unique()
ds2 <- ds %>% dplyr::select(c("TLID", "N","P","K","blup"))
dssr2 <- ds2 %>% inner_join(INS2) %>% unique() 


ggplot(ins, aes(refY, N_base_supply))+
  geom_boxplot()
ggplot(ins, aes(refY, P_base_supply))+
  geom_boxplot()
ggplot(ins, aes(refY, K_base_supply))+
  geom_boxplot()

## defining the response and predictors for ML
response_N <- "N_base_supply"
response_P <- "P_base_supply"
response_K <- "K_base_supply"

predictors <- ins |> names()
predictors <- predictors[!predictors %in% c("N_base_supply","P_base_supply","K_base_supply", "TLID", "blup", "season_AB")]

h2o.init()
ML_inputData.h2o <- as.h2o(ins)

#create a random training-test split of our data ## should be possible to do it by missing one
ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.7, seed = 444)
training_data <- ML_inputData_split[[1]]
test_data <- ML_inputData_split[[2]]

hyperparams_gbm <- list(
  ntrees = seq(500, 1000, 100), ### is tested for diff nrtrees with seq(20, 200, 20), seq(200, 500, 50) and seq(500, 1000, 100)
  max_depth = seq(4, 8, 2)
)

grid_gbm_P <- h2o.grid(
  algorithm = "gbm",
  x = predictors,
  y = response_P,
  grid_id = "hyperparams_gbm",
  hyper_params = hyperparams_gbm,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


grid_gbm_N <- h2o.grid(
  algorithm = "gbm",
  x = predictors,
  y = response_N,
  grid_id = "hyperparams_gbm",
  hyper_params = hyperparams_gbm,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


grid_gbm_K <- h2o.grid(
  algorithm = "gbm",
  x = predictors,
  y = response_K,
  grid_id = "hyperparams_gbm",
  hyper_params = hyperparams_gbm,
  training_frame = training_data,
  validation_frame = test_data,
  seed = 444
)


# Get the best hyper parameters
best_hyperParm_P <- h2o.getModel(grid_gbm_P@model_ids[[1]])
print(best_hyperParm_P@parameters) 

best_hyperParm_N <- h2o.getModel(grid_gbm_N@model_ids[[1]])
print(best_hyperParm_N@parameters) 

best_hyperParm_K <- h2o.getModel(grid_gbm_K@model_ids[[1]])
print(best_hyperParm_K@parameters) 


### fit the model with the tuned hyper parameters: 
## soil P
ML_gbm_P <- h2o.gbm(x = predictors,
                    y = response_P,
                    ntrees = best_hyperParm_P@parameters$ntrees,
                    max_depth = best_hyperParm_P@parameters$max_depth,
                    training_frame = training_data,
                    validation_frame = test_data,
                    keep_cross_validation_predictions = TRUE,
                    nfolds = 5,
                    seed = 444)

model_path <- h2o.saveModel(object = ML_gbm_P, path = result_full_path, force = TRUE)
print(model_path)
# P_model_path <- "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\GBM_model_R_1720435759070_4"
P_saved_model <- h2o.loadModel(P_model_path)
P_local_model <- h2o.download_model(P_saved_model, path = result_full_path)
ML_gbm_P <- h2o.upload_model(P_local_model)

## soil N
ML_gbm_N <- h2o.gbm(x = predictors,
                    y = response_N,
                    ntrees = best_hyperParm_N@parameters$ntrees,
                    max_depth = best_hyperParm_N@parameters$max_depth,
                    training_frame = training_data,
                    validation_frame = test_data,
                    keep_cross_validation_predictions = TRUE,
                    nfolds = 5,
                    seed = 444)
model_path_N <- h2o.saveModel(object = ML_gbm_N, path = result_full_path, force = TRUE)
print(model_path_N)
# model_path_N <- "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\GBM_model_R_1720435759070_5"
model_N <- h2o.loadModel(model_path_N)
my_local_model_N <- h2o.download_model(model_N, path = result_full_path)
ML_gbm_N <- h2o.upload_model(my_local_model_N)


## soil K
ML_gbm_K <- h2o.gbm(x = predictors,
                    y = response_K,
                    ntrees = best_hyperParm_K@parameters$ntrees,
                    max_depth = best_hyperParm_K@parameters$max_depth,
                    training_frame = training_data,
                    validation_frame = test_data,
                    keep_cross_validation_predictions = TRUE,
                    nfolds = 5,
                    seed = 444)
model_path_K <- h2o.saveModel(object = ML_gbm_K, path = result_full_path, force = TRUE)
print(model_path_K)
# model_path_K <- "D:\\OneDrive - CGIAR\\AgWise\\Dahsboard\\AgWise_Demo\\demo-repository\\Data\\Fertilizer_recom\\Intermediate\\GBM_model_R_1720435759070_6"
model_K <- h2o.loadModel(model_path_K)
local_model_K <- h2o.download_model(model_K, path = result_full_path)
ML_gbm_K <- h2o.upload_model(local_model_K)

###################################################################
### model diagnostics
###################################################################
### model diagnostics
h2o.levels(test_data["refY"])
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Very high")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "High")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Medium")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Low")
test_data["refY"] <- h2o.relevel(x = test_data["refY"], y = "Very low")

# residual plot
h2o.residual_analysis_plot(ML_gbm_P,test_data)
h2o.residual_analysis_plot(ML_gbm_N,test_data)## removing three data on the high end will make the residuals being distributed around zero
h2o.residual_analysis_plot(ML_gbm_K,test_data) ## there is uner and ovre estiamtion of K

#the variable importance plot
h2o.varimp_plot(ML_gbm_P, num_of_features = 12)
h2o.varimp_plot(ML_gbm_N, num_of_features = 12)
h2o.varimp_plot(ML_gbm_K, num_of_features = 12)

# partial dependency plot
h2o.partialPlot(object = ML_gbm_P, test_data, cols = c("refY"))
h2o.partialPlot(object = ML_gbm_N, test_data, cols = c("refY"))
h2o.partialPlot(object = ML_gbm_K, test_data, cols = c("refY"))


# shap values = the direction of the relationship between our features and target
h2o.shap_summary_plot(ML_gbm_P, test_data)
h2o.shap_summary_plot(ML_gbm_N, test_data)
h2o.shap_summary_plot(ML_gbm_K, test_data)


## validate the ANS NPK: leave one out cross validation: suing the best model (GB)
missing_one_valid <- NULL 
for(k in unique(ins$TLID)){
  print(k)
  tdata <- ins[ins$TLID == k, ]
  tdata.h2o <- as.h2o(tdata)
  predResponse_N <- as.data.frame(h2o.predict(object = ML_gbm_N, newdata = tdata.h2o))
  predResponse_P <- as.data.frame(h2o.predict(object = ML_gbm_P, newdata = tdata.h2o))
  predResponse_K <- as.data.frame(h2o.predict(object = ML_gbm_K, newdata = tdata.h2o))
  tdata$N_pred <- predResponse_N$predict
  tdata$P_pred <- predResponse_P$predict
  tdata$K_pred <- predResponse_K$predict
  missing_one_valid <- rbind(missing_one_valid, tdata )
}


saveRDS(missing_one_valid, paste0(result_full_path, "/GB_missing_one_valid.RDS"))
missing_one_valid <- readRDS(paste0(result_full_path, "/GB_missing_one_valid.RDS"))

missing_one_valid$rmse_N = round(sqrt(sum((missing_one_valid$N_base_supply - missing_one_valid$N_pred)**2)/nrow(missing_one_valid)), digits=0)
missing_one_valid$rmse_P = round(sqrt(sum((missing_one_valid$P_base_supply - missing_one_valid$P_pred)**2)/nrow(missing_one_valid)), digits=0)
missing_one_valid$rmse_K = round(sqrt(sum((missing_one_valid$K_base_supply - missing_one_valid$K_pred)**2)/nrow(missing_one_valid)), digits=0)

Leave1_GBN <- missing_one_valid %>% 
  dplyr::select(TLID,N_base_supply,province, district,AEZ,N_pred,rmse_N) %>% 
  dplyr::rename(GB_Predicted= N_pred, RMSE=rmse_N, RevQUEFTS=N_base_supply)%>% 
  dplyr::mutate(ANS = "N")

Leave1_GBP <- missing_one_valid %>% 
  dplyr::select(TLID,P_base_supply,province, district,AEZ,P_pred,rmse_P) %>% 
  dplyr::rename(GB_Predicted= P_pred, RMSE=rmse_P, RevQUEFTS=P_base_supply) %>% 
  dplyr::mutate(ANS = "P")

Leave1_GBK <- missing_one_valid %>% 
  dplyr::select(TLID, K_base_supply, province, district, AEZ, K_pred,rmse_K) %>% 
  dplyr::rename(GB_Predicted= K_pred, RMSE=rmse_K, RevQUEFTS=K_base_supply)%>% 
  dplyr::mutate(ANS = "K")

Leave1_GB <- rbind(Leave1_GBN, Leave1_GBP, Leave1_GBK)
Leave1_GB$ANS = factor(Leave1_GB$ANS, levels=c("N","P","K")) 

gg_pred <-  ggplot(Leave1_GB, aes(x = RevQUEFTS, y = GB_Predicted)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(size = 3) + 
  facet_wrap(~ANS, nrow=1, scales="free") +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 4,
                        label.y = 0.975) +
  ggpmisc::stat_poly_eq(use_label(c("R2")),
                        formula = y ~ x, size = 4,
                        label.y = 0.90) +
  geom_text(data = Leave1_GB %>% 
              group_by(ANS) %>% 
              dplyr::summarise(rmse = sqrt(sum((RevQUEFTS - GB_Predicted)**2)/n()),
                               GB_Predicted = 0) %>%
              mutate(RevQUEFTS = c(200, 200, 200)*0.95),
            aes(label = paste0("rmse = ", round(rmse, digits=0))), size = 4, hjust = 1, vjust=-0.9) + 
  xlab("Reverse QUEFTS predicted soil nutrinet supply") +
  ylab("Gradient Boosting predicted soil nutrinet supply") +
  ggtitle("Gradient Boosting: ANS from QUEFTTS versus from ML using geospatial data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(result_full_path, "/GB_mod_valid.pdf"),  gg_pred, width=9, height = 4)

#################################################
## get the yield prediction using the NPK obtained from reverse QUEFTS and GB ML model
## compare the effect of NPK obtained using the different approaches on yield prediction 
#################################################
py <- NULL
ni <- 0
for(i in unique(missing_one_valid$TLID)){
  
  print(paste0(round(ni/length(unique(missing_one_valid$TLID))*100), "% complete"))
  ni <- ni+1
  
  dsi <- ds[ds$TLID == i,]
  fri <- data.frame("N" = dsi$N,
                    "P" = dsi$P,
                    "K" = dsi$K)
  
  #different supply estimates:
  sqi <- unique(missing_one_valid[missing_one_valid$TLID == i, c("N_base_supply", "P_base_supply", "K_base_supply")]) ## reverse QUEFTS NPK
  spi <- unique(missing_one_valid[missing_one_valid$TLID == i, c("N_pred", "P_pred", "K_pred")])## ML NPK
  sni <- unique(missing_one_valid %>% dplyr::select(N_base_supply, P_base_supply, K_base_supply) %>% 
    dplyr::summarise(across(everything(), list(median)))) ## median of reverse QUEFTS NPK all location 
  
  ayi <- unique(as.numeric(ins[ins$TLID == i,]$refY) * 10 * 1000 * 0.21) ## dry weight
  
  #yield predicted using reverse QUEFTS calculated supply
  pyj <- NULL
  for(j in 1:nrow(fri)){
    yqi <- runQUEFTS(nut_rates = fri[j,],
                     supply = as.numeric(sqi),
                     crop = "Potato",
                     Ya = ayi,
                     SeasonLength = 120)
    
    #yield predicted using supply obtained from predictions by RF
    ypi <- runQUEFTS(nut_rates = fri[j,],
                     supply = as.numeric(spi),
                     crop = "Potato",
                     Ya = ayi,
                     SeasonLength = 120)
    
    #yield predicted by a naive model using median values of NPK supply across all TLIDs:
    yni <- runQUEFTS(nut_rates = fri[j,],
                     supply = as.numeric(sni),
                     crop = "Potato",
                     Ya = ayi,
                     SeasonLength = 120)
    
    pyj <- rbind(pyj, data.frame(TLID = i,
                               N = fri$N[j],
                               P = fri$P[j],
                               K = fri$K[j],
                               Yq = yqi / 1000 / 0.21, #yield predicted using revQUEFTS supply
                               Yp = ypi / 1000 / 0.21, #yield predicted using RF predicted supply
                               Yn = yni / 1000 / 0.21, #yield predicted using median nutrient supply
                               Yb = dsi$blup[j],          #yield blup
                               Yo = dsi$TY[j]))           #yield observed
  }
  py <- rbind(py, pyj)

}


saveRDS(py, paste0(result_full_path, "/predictedYield_diffANS.RDS"))
py <- readRDS(paste0(result_full_path, "/predictedYield_diffANS.RDS"))


ppyy <-  py %>% left_join(ds %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
  mutate(refY = ifelse(N > 75 & P > 30 & K > 50, "Reference treatment", "Other treatments"))


ggRQ_BLUP <- ggplot(ppyy, aes(x = Yb, y = Yq)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  xlab("Observed potato yield (t/ha)")+
  ylab("Predicted potato yield (t/ha), Reverse QUEFTS ANS")+
  xlim(0, 62.5)+
  ylim(0, 62.5)+
  labs(title = ~ atop(paste('Observed vs Predicted potato tuber yield'),
                      'Yield predicted using rerverse QUEFTS estimated ANS')) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 13, face="bold"))

ggsave(paste0(result_full_path, "/RQ_BLUP.pdf"), ggRQ_BLUP)




ggRQ_GB <-ggplot(ppyy, aes(x = Yb, y = Yp)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  xlab("Observed potato yield (t/ha)")+
  ylab("Predicted potato yield (t/ha), Gradient Boosting ANS")+
  xlim(0, 62.5)+
  ylim(0, 62.5)+
  labs(title = ~ atop(paste('Observed vs Predicted potato tuber'),
                      'Gradient Boosting estimated ANS are used to predict yield')) +
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 13, face="bold"))

ggsave(paste0(result_full_path, "/RQ_GB_yield.pdf"), ggRQ_GB)



pyr <- py %>%
  gather(variable, value, Yq:Yo) %>%
  group_by(TLID, N, P, K, variable) %>%
  dplyr::summarise(value = mean(value)) %>%
  mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) %>%
  group_by(TLID, variable) %>%
  mutate(refY = mean(ifelse(treat == "ref", value, NA), na.rm=TRUE),
         dY = refY - value) %>%
  filter(treat != "ref") %>%
  dplyr::select(-treat, -value, -refY) %>%
  spread(variable, dY) %>%
  gather(variable, value, c(Yq, Yp, Yo, Yn)) %>%
  mutate(variable = mapvalues(variable, 
                              from = c("Yq", "Yp", "Yn"),
                              to = c("supply from reverse QUEFTS", "supply by ML prediction", "median for supply"))) %>%
  ungroup()




pyrr <-  pyr %>%
  filter(variable != "Yo") %>%
  mutate(variable = as.character(variable))


ggdiffANS <- ggplot(pyrr, aes(x = Yb, y = value)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = pyr %>% 
              filter(variable != "Yo") %>% 
              group_by(variable) %>% 
              dplyr::summarise(rmse = sqrt(sum((value - Yb)**2)/n()),
                               Yb = -4,
                               value = 15),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("Observed potato yield difference to reference treatment [t/ha]\n") +
  ylab("\nPredicted potato yield difference to reference treatment [t/ha]") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 6) +
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave(paste0(result_full_path, "/RQ_GB_Median_yield.pdf"), ggdiffANS, width=12, height = 6)



##################################################################################
## Use the trained GB models to predict the soil INS at scale
###################################################################################

model_path_P <- paste0(result_full_path,"/GBM_model_R_1720435759070_4") ## if you train a new model "GBM_model_R_1720435759070_4" should be changed
saved_model_P <- h2o.loadModel(model_path_P)
P_local_model <- h2o.download_model(saved_model_P, path = pathOut)
ML_gbm_P <- h2o.upload_model(P_local_model)


model_path_N <- paste0(result_full_path,"/GBM_model_R_1720435759070_5")## if you train a new model "GBM_model_R_1720435759070_5" should be changed
saved_model_N <- h2o.loadModel(model_path_N)
N_local_mode <- h2o.download_model(saved_model_N, path = pathOut)
ML_gbm_N <- h2o.upload_model(N_local_mode)


model_path_K <- paste0(result_full_path,"/GBM_model_R_1720435759070_6")## if you train a new model "GBM_model_R_1720435759070_6" should be changed
saved_model_K <- h2o.loadModel(model_path_K)
K_local_model <- h2o.download_model(saved_model_K, path = pathOut)
ML_gbm_K <- h2o.upload_model(K_local_model)

## soil data for AOI
Soil_PointData_AOI <- Soil_PointData_AOI %>% dplyr::rename(province = NAME_1, district = NAME_2)

## AEZ and altitude info for AOI
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

Soil_AEZ_AOI_topo <- Soil_AEZ_AOI %>%
  dplyr::mutate_if(is.character, as.factor) %>% 
  left_join(topo) 



## Assuming the five reference classes, estimate the INS for every point
INS_AOI <- NULL
for(referenceY in c("Very low", "Low", "Medium", "High", "Very high")){
  print(referenceY)
  tdata <- Soil_AEZ_AOI_topo[Soil_AEZ_AOI_topo$AEZ %in% c("Birunga", "Buberuka highlands", "Congo-Nile watershed divide"), ]
  tdata$refY <- referenceY
  tdata.h2o <- as.h2o(tdata)
  predResponse_N <- as.data.frame(h2o.predict(object = ML_gbm_N, newdata = tdata.h2o))
  predResponse_P <- as.data.frame(h2o.predict(object = ML_gbm_P, newdata = tdata.h2o))
  predResponse_K <- as.data.frame(h2o.predict(object = ML_gbm_K, newdata = tdata.h2o))
  tdata$N_pred <- predResponse_N$predict
  tdata$P_pred <- predResponse_P$predict
  tdata$K_pred <- predResponse_K$predict
  tdata[tdata$N_pred<0, "N_pred"] <- 0
  tdata[tdata$P_pred<0, "P_pred"] <- 0
  tdata[tdata$K_pred<0, "K_pred"] <- 0
  INS_AOI <- rbind(INS_AOI, tdata )
}

summary(tdata$N_pred)
summary(tdata$P_pred)
summary(tdata$K_pred)

saveRDS(INS_AOI, paste0(result_full_path,"/soil_NPK_supply_AOI.RDS"))

############# plot the soil INS

#map with trial locations:
rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
rwAEZ <- AEZ[AEZ$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]
RW_aez <- spTransform(rwAEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
rwAEZ <- st_as_sf(RW_aez)
rwAEZ$Names_AEZs

INS_AOI$refY <- factor(INS_AOI$refY, levels=c("Very low","Low", "Medium", "High", "Very high"))

ggN <- ggplot()+
  geom_tile(data = INS_AOI[INS_AOI$refY %in% c("Low", "Medium", "High", "Very high"),], aes(x=longitude, y=latitude, fill = N_pred))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  facet_wrap(~refY, nrow = 1) +
  scale_fill_gradient(low="palegoldenrod", high="darkgreen",
                      limits = quantile(INS_AOI[INS_AOI$refY %in% c("Low", "Medium", "High", "Very high"),]$N_pred, c(0.05, 0.95)), 
                      oob = scales::squish) +
  guides(fill=guide_legend(title="Soil N supply\n[kg N/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/soilN_AOI.pdf"), ggN, width=12, height = 6)



ggP <- ggplot()+
  geom_tile(data = INS_AOI[INS_AOI$refY %in% c("Low", "Medium", "High", "Very high"),], aes(x=longitude, y=latitude, fill = P_pred))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  facet_wrap(~refY, nrow = 1) +
  scale_fill_gradient(low="mistyrose2", high="darkred",
                      limits = quantile(INS_AOI[INS_AOI$refY %in% c("Low", "Medium", "High", "Very high"),]$P_pred, c(0.05, 0.95)), 
                      oob = scales::squish) +
  guides(fill=guide_legend(title="Soil P supply\n[kg P/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/soilP_AOI.pdf"),
       ggP, width=12, height = 6)


ggK <- ggplot()+
  geom_tile(data = INS_AOI[INS_AOI$refY %in% c("Low", "Medium", "High", "Very high"),], aes(x=longitude, y=latitude, fill = K_pred))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  facet_wrap(~refY, nrow = 1) +
  scale_fill_gradient(low="lightblue1", high="darkblue", 
                      limits = quantile(INS_AOI[INS_AOI$refY %in% c("Low", "Medium", "High", "Very high"),]$K_pred, c(0.05, 0.95)), 
                      oob = scales::squish) +
  guides(fill=guide_legend(title="Soil K supply\n[kg K/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/soilK_AOI.pdf"),
       ggK, width=12, height = 6)



INS_AOI %>%
  gather(nutrient, supply, N_pred:K_pred) %>%
  filter(refY != "Very low") %>%
  group_by(nutrient, refY, AEZ) %>%
  summarise(supply = round(median(supply))) %>%
  print(n = 36)

ggINS_Summary <- INS_AOI %>%
    gather(nutrient, supply, N_pred:K_pred) %>%
    filter(refY != "Very low") %>%
    ggplot(aes(x = AEZ, y = supply)) +
    geom_boxplot() +  
    coord_flip() +
    scale_y_log10() +
    facet_grid(refY ~ nutrient, scales = "free_x")

ggsave(paste0(result_full_path,"/soilNPK_AOI_summary.pdf"),  ggINS_Summary, width=12, height = 6)




# preparing data for K-means clustering
aoi <- INS_AOI
aoi$ID <- 1:nrow(aoi)
## evaluatiung the proprotion of variance explained by AEZM dsitrict, refY
summary(lm(N_pred ~ AEZ + district + refY, data=aoi))$r.squared
summary(lm(N_pred ~ district + refY, data=aoi))$r.squared
summary(lm(N_pred ~ AEZ + refY, data=aoi))$r.squared
summary(lm(N_pred ~ refY, data=aoi))$r.squared
summary(lm(N_pred ~ AEZ, data=aoi))$r.squared

summary(lm(P_pred ~ AEZ + district + refY, data=aoi))$r.squared
summary(lm(P_pred ~ district + refY, data=aoi))$r.squared
summary(lm(P_pred ~ AEZ + refY, data=aoi))$r.squared
summary(lm(P_pred ~ refY, data=aoi))$r.squared
summary(lm(P_pred ~ AEZ, data=aoi))$r.squared

summary(lm(K_pred ~ AEZ + district + refY, data=aoi))$r.squared
summary(lm(K_pred ~ district + refY, data=aoi))$r.squared
summary(lm(K_pred ~ AEZ + refY, data=aoi))$r.squared
summary(lm(K_pred ~ refY, data=aoi))$r.squared
summary(lm(K_pred ~ AEZ, data=aoi))$r.squared


cls <- NULL
nclus <- 100
for(i in unique(aoi$refY)){
  for(j in unique(aoi$AEZ)){
    tmp <- subset(aoi[aoi$refY == i & aoi$AEZ == j,], select = c(ID, AEZ, refY, N_pred, P_pred, K_pred))
    fit <- kmeans(subset(tmp, select = -c(ID, AEZ, refY)), nclus)
    print(paste0("between_SS / total_SS = ", round(fit$betweenss/fit$totss*100),"%"))
    tmp <- data.frame(tmp, fit$cluster)
    cls <- rbind(cls, tmp)
  }
}

cls <- cls %>% left_join(aoi %>% dplyr::select(ID, district, latitude, longitude))

clss <- cls %>%
  group_by(AEZ, refY, fit.cluster) %>%
  summarise(clusN = median(N_pred),
            clusP = median(P_pred),
            clusK = median(K_pred))

my_ferts <- rbind(fertilizers()[c(8,12),], data.frame(group = "synthetic",
                                                      name = "NPK 17-17-17",
                                                      N = 17,
                                                      P = 17*2*31 / (2*31 + 5*16),
                                                      K = 17*2*39.1 / (2*39.1 + 16),
                                                      Ca = 0,
                                                      Mg = 0,
                                                      S = 0,
                                                      Mb = 0,
                                                      Zn = 0, 
                                                      Bo = 0,
                                                      Cu = 0))
fert_rates_blanket <- c(0, 0, 300)
yts <- c(0, 0.1, 0.2) ## desired percent yield increase on top of the yield obtained from blanket recommendation 


## running the following to get yield at zero NPK input and with blanket recommendations takes quite a bit of time, for testing you can just read the saved output 
pyaoi <- NULL
frecs <- NULL

for(i in 1:nrow(clss)){
  
  print(i)
  
  si <- as.numeric(clss[i, c("clusN", "clusP", "clusK")])
  yai <- as.numeric(clss[i, ]$refY) * 10 * 1000 * 0.21 * 1.2 #set to 20% above ceiling of yield class
  
  TY0 <- runQUEFTS(nut_rates = data.frame("N" = 0, "P" = 0, "K" = 0),
                   supply = si,
                   crop = "Potato",
                   Ya = yai)
  
  TYb <- runQUEFTS(nut_rates = as.data.frame(as.list(nutrientRates(my_ferts, fert_rates_blanket))),
                   supply = si,
                   crop = "Potato",
                   Ya = yai)


  FRS <- NULL
  for(j in yts){
    FRS <- rbind(FRS, data.frame(id = i,
                                 target = j,
                                 TYt = TYb * (1 + j) / 1000 / 0.21,
                                 name = my_ferts$name,
                                 rate = rec_targetdY(my_ferts = my_ferts,
                                                     dY = TYb * (1 + j) - TY0,
                                                     target = "absolute",
                                                     supply = si,
                                                     crop = "Potato", #crop to be defined by QUEFTS
                                                     Ya = yai)))
    
    
  }

  pyaoi <- rbind(pyaoi, data.frame(id = i,
                                   TY0 = TY0 / 1000 / 0.21,
                                   TYb = TYb / 1000 / 0.21))
  frecs <- rbind(frecs, FRS)
  
}

saveRDS(pyaoi, paste0(result_full_path,"/pyaoi.RDS"))
saveRDS(frecs, paste0(result_full_path,"/frecs.RDS"))


pyaoi <- readRDS(paste0(result_full_path,"/pyaoi.RDS"))
frecs <- readRDS(paste0(result_full_path,"/frecs.RDS"))

rec <- clss %>%
  bind_cols(pyaoi) %>%
  left_join(frecs) %>%
  mutate(rate = ifelse(rate<0, 0, rate)) %>%
  dplyr::select(-id) %>%
  mutate(name = mapvalues(name, 
                          from = c("Urea (U-46)", "Diammonium phosphate", "NPK 17-17-17"),
                          to = c("Urea", "DAP", "NPK171717"))) %>%
  spread(name, rate) %>%
  ungroup() %>%
  left_join(cls, relationship = "many-to-many")

saveRDS(rec, paste0(result_full_path,"/Fertilizer_recommendations_potato_QUEFTS_ML.RDS"))


gg0 <- ggplot()+
  geom_tile(data = rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,], aes(x=longitude, y=latitude, fill = TY0))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
  facet_wrap(~refY, nrow = 1) +
  scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(10, 45, 5),
                       #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                       limits = c(5, 45),
                       oob = scales::squish) +
  guides(fill=guide_legend(title="Yield without\nfertilizer [t/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/Yield_zero_Fertilizer.pdf"), gg0, width=12, height = 6)




rec %>%
  filter(refY != "Very low",
         rec$target == 0) %>%
  group_by(refY, AEZ) %>%
  summarise(TY0 = median(TY0),
            TYb = median(TYb))


ggBR <- ggplot()+
  geom_tile(data = rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target == 0,], aes(x=longitude, y=latitude, fill = TYb))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
  facet_wrap(~refY, nrow = 1) +
  scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(10, 40, 5),
                       limits = c(5, 40),
                       oob = scales::squish) +
  guides(fill=guide_legend(title="Yield with\ncurrent blanket\nrecommendation\n[t/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/Yield_Blanket_Rate.pdf"), ggBR, width=12, height = 6)




################################################################################################
## RAB choose to provide advisory assuming medium soil NPK for one AEZ and high NPK for the other two
## if there is possibility to provide the advisory interactively with farmers indicating their farm fertility level and 
## desired yield increase the work stops here.
################################################################################################

selrec <- rec %>%
  filter((refY == "Medium" & AEZ == "Congo-Nile watershed divide") | (refY == "High" & AEZ != "Congo-Nile watershed divide"))

## replace NA with the median of the AEZ
ddply(selrec, .(AEZ), summarize, mU=median(Urea, na.rm=TRUE),mDAP=median(DAP, na.rm=TRUE),mNPK=median(NPK171717, na.rm=TRUE))
unique(selrec[is.na(selrec$Urea), ]$AEZ)
unique(selrec[is.na(selrec$DAP), ]$AEZ)
unique(selrec[is.na(selrec$Urea), ]$AEZ)

selrec[is.na(selrec$Urea) & selrec$AEZ == "Birunga", ]$Urea <- 118
# selrec[is.na(selrec$Urea) & selrec$AEZ == "Buberuka highlands", ]$Urea <- 122

selrec[is.na(selrec$DAP) & selrec$AEZ == "Birunga", ]$DAP <- 60
# selrec[is.na(selrec$DAP) & selrec$AEZ == "Buberuka highlands", ]$DAP <- 62

selrec[is.na(selrec$NPK171717) & selrec$AEZ == "Birunga", ]$NPK171717 <- 83
# selrec[is.na(selrec$NPK171717) & selrec$AEZ == "Buberuka highlands", ]$NPK171717 <- 85


quantile(selrec$Urea, probs=seq(0,1,0.01))
quantile(selrec$DAP, probs=seq(0,1,0.01))
quantile(selrec$NPK171717, probs=seq(0,1,0.01))

selrec[selrec$Urea > 300, ]$Urea <- 300
selrec[selrec$DAP > 250, ]$DAP <- 250
selrec[selrec$NPK171717 > 350, ]$NPK171717 <- 350


## maping the Urea requirement by yield percent increase over the yield obtained with blanket recommendation
ggUrea <- ggplot()+
  geom_tile(data = selrec, aes(x=longitude, y=latitude, fill = Urea))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  facet_wrap(~target, nrow = 1) +
  scale_fill_viridis_c(direction = -1, option = "viridis",
                       breaks = seq(0, 300, 50),
                       limits = c(0, 300), 
                       oob = scales::squish) +
  guides(fill=guide_legend(title="Urea\nrequirement\n[kg/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/UreaMap.pdf"), ggUrea, width=12, height = 6)



## maping the DAP requirement by yield percent increase over the yield obtained with blanket recommendation
ggDAP <- ggplot()+
  geom_tile(data = selrec, aes(x=longitude, y=latitude, fill = DAP))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  facet_wrap(~target, nrow = 1) +
  scale_fill_viridis_c(direction = -1, option = "rocket",
                       breaks = seq(0, 200, 50),
                       limits = c(0, 200), 
                       oob = scales::squish) +
  guides(fill=guide_legend(title="DAP\nrequirement\n[kg/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/DAPMap.pdf"), ggDAP, width=12, height = 6)



## Mapping the NPK171717 requirement by yield percent increase over the yield obtained with blanket recommendation
ggNPK <- ggplot()+
  geom_tile(data = selrec, aes(x=longitude, y=latitude, fill = NPK171717))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  facet_wrap(~target, nrow = 1) +
  scale_fill_viridis_c(direction = -1, option = "mako",
                       breaks = seq(0, 400, 50),
                       limits = c(0, 400), 
                       oob = scales::squish) +
  guides(fill=guide_legend(title="NPK 17:17:17\nrequirement\n[kg/ha]")) +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())

ggsave(paste0(result_full_path,"/NP171717Map.pdf"), ggNPK, width=12, height = 6)


##
rec %>%
  gather(name, rate, DAP:Urea) %>%
  filter(refY %in% c("Low", "Medium", "High"),
         target < 0.3) %>%
  ggplot(aes(y = rate, x = AEZ, fill = as.factor(refY))) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  facet_grid(target ~ name, scales="free")

rec %>%
  gather(name, rate, DAP:Urea) %>%
  filter(refY %in% c("Low", "Medium", "High"),
         target < 0.3) %>%
  ggplot(aes(y = rate, x = AEZ)) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  facet_grid(name ~ refY + target, scales="free")

rec %>%
  gather(name, rate, DAP:Urea) %>%
  filter(refY %in% c("Low", "Medium", "High", "Very high"),
         target < 0.3) %>%
  group_by(AEZ, refY, target, name) %>%
  summarise(rate = median(rate, na.rm = TRUE)) %>%
  unite(tmp, target, name) %>%
  spread(tmp, rate) %>%
  print(n = 12)



ggUrea_20 <- ggplot()+
  geom_tile(data = selrec[selrec$target == 0.2,], aes(x=longitude, y=latitude, fill = Urea))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  scale_fill_viridis_c(direction = -1, option = "viridis",
                       breaks = seq(0, 250, 50),
                       limits = c(0, 250), 
                       oob = scales::squish) +
  ggtitle("Urea [kg/ha]") +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"))

ggDAP_20 <- ggplot()+
  geom_tile(data = selrec[selrec$target == 0.2,], aes(x=longitude, y=latitude, fill = DAP))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  scale_fill_viridis_c(direction = -1, option = "rocket",
                       breaks = seq(0, 250, 50),
                       limits = c(0, 250), 
                       oob = scales::squish) +
  ggtitle("DAP [kg/ha]") +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"))


ggNPK_20 <- ggplot()+
  geom_tile(data = selrec[selrec$target == 0.2,], aes(x=longitude, y=latitude, fill = NPK171717))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  scale_fill_viridis_c(direction = -1, option = "mako",
                       breaks = seq(0, 250, 50),
                       limits = c(0, 250), 
                       oob = scales::squish) +
  ggtitle("NPK 17:17:17 [kg/ha]") +
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"))

ggt_20 <- gridExtra::grid.arrange(ggDAP_20, ggNPK_20, ggUrea_20, nrow=1)
ggt_0 <- gridExtra::grid.arrange(ggDAP_0, ggNPK_0, ggUrea_0, nrow=1)



summary(lm(Urea ~ district, data = selrec[selrec$target == 0.2,]))


selrec %>% 
  # filter((refY == "Medium"),
  filter((refY == "Medium" & AEZ == "Congo-Nile watershed divide") | (refY == "High" & AEZ != "Congo-Nile watershed divide"),
         target %in% c(0, 0.2)) %>%
  group_by(target, district) %>%
  summarise(DAP = median(DAP),
            NPK171717 = median(NPK171717),
            Urea = median(Urea)) %>%
  print(n = 34)


###########################################################
# GENERATING FERTILIZER PACKAGES FOR VALIDATION EXERCISES #
###########################################################

#subset scenario with 20% yield increase and median refY level:


ssrec <- selrec %>% 
  # filter((refY == "Medium"),
  filter((refY == "Medium" & AEZ == "Congo-Nile watershed divide") | (refY == "High" & AEZ != "Congo-Nile watershed divide"),
         target == 0.2)

#using k-medoid clustering (more robust than k-means)
tmp <- subset(ssrec, select=c(DAP, NPK171717, Urea))
fviz_nbclust(tmp, pam, method = "wss")
nclus <- 6
pkg <- pam(tmp, nclus, stand = FALSE)
ssrec$pkg.cluster <- pkg$cluster

pkgs <- pkg$medoids %>%
  as.data.frame() %>%
  mutate(pkg.cluster = row_number(),
         pkg.col = rainbow(nclus),
         pkg.code = c("red", "yellow", "green", "cyan", "blue", "purple")) %>%
  rename(DAP.cluster = DAP,
         NPK171717.cluster = NPK171717,
         Urea.cluster = Urea)

ssrec <- merge(ssrec, pkgs)

#calculating variance captured by clusters:
ssrec %>%
  mutate(ts = (Urea - mean(Urea))**2 + (DAP - mean(DAP))**2 + (NPK171717 - mean(NPK171717))**2,
         ms = (Urea - Urea.cluster)**2 + (DAP - DAP.cluster)**2 + (NPK171717 - NPK171717.cluster)**2) %>%
  summarise(var = 1 - sum(ms)/sum(ts))

pkgs %>%
  gather(fertilizer, rate, DAP.cluster:Urea.cluster) %>%
  mutate(fertilizer = gsub(".cluster", "", fertilizer)) %>%
  ggplot(aes(x = fertilizer, y = as.factor(pkg.cluster), fill = rate)) + 
  geom_tile() + 
  scale_fill_viridis_c(direction = -1, option = "magma", guide = "none") +
  geom_text(aes(label = round(rate), colour = rate > 180), size = 9) +
  scale_color_manual(guide = "none", values = c("black", "white")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), limits = as.factor(6:1)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

ggpkg <- ggplot()+
  geom_tile(data = ssrec, aes(x=longitude, y=latitude, fill = as.factor(pkg.cluster)))+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  scale_fill_manual(values = rainbow(nclus))+
  scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        plot.title = element_text(size=18, face="bold"))



ssrec_odk <- ssrec %>%
  mutate(latr = round(latitude * 100)/100,
         lonr = round(longitude * 100)/100,
         lookup_key = paste0("E", lonr, "N", latr),
         DAP.cluster = round(DAP.cluster),
         NPK171717.cluster = round(NPK171717.cluster),
         Urea.cluster = round(Urea.cluster)) %>%
  dplyr::select(lookup_key, pkg.cluster, pkg.code, DAP.cluster, NPK171717.cluster, Urea.cluster) %>%
  rename(pkgNr = pkg.cluster,
         pkgCode = pkg.code,
         DAP = DAP.cluster,
         NPK = NPK171717.cluster,
         Urea = Urea.cluster)

write.csv(ssrec_odk, paste0(result_full_path,"/RwaSIS_fertilizer_packages.csv"), row.names = FALSE)



