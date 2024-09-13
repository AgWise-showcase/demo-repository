##############################################################################################
## 1. source packages and functions needed for the analytics
##############################################################################################
rm(list=ls())
packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel","MuMIn","ggpmisc","sf","cluster","h2o",
                       "limSolve", "lpSolve", "Rquefts", "rgdal", "randomForest","ranger","Metrics", "factoextra", "git2r", "geodata")
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
# 3. read data 
#################################################################################################################
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"
ds <- readRDS(paste0(data_full_path, "/modelReady.RDS"))
Soil_PointData_AOI <- readRDS(paste0(data_full_path, "/Soil_PointData_AOI.RDS"))
Topo <-  readRDS(paste0(data_full_path, "/topoData_AOI.RDS"))
AEZ <- readOGR(dsn=gs_full_path,  layer="AEZ_DEM_Dissolve")
rwlake <- st_read(paste0(gs_full_path, "/RWA_Lakes_NISR.shp"))
source(paste0(script_full_path, "/generic/QUEFTS_functions.R"))



#################################################################################################################
# 4. data wrangling and visualizing 
#################################################################################################################
ds <- ds %>% 
  dplyr::mutate(Experiment = if_else(expCode == "IFDC", "Exp-1", 
                                     if_else(expCode == "SA-VAP-1", "Exp-2", "Exp-3"))) %>% 
  dplyr::mutate(trt = paste0("N",N, "P", P, "K",K)) %>% 
  dplyr::mutate(trt =  ifelse(treat %in% c("NPK11","NPK_increased","NPK", "NPK_all"), "Reference", 
                              ifelse(treat == "N0P0K0", "Control" ,trt))) %>% 
  dplyr::mutate(trt2 =  ifelse(trt == "N0P0K0", "Control", trt)) %>%  
  dplyr::mutate(Experiment = if_else(expCode == "IFDC", "Exp-1", 
                                     if_else(expCode == "SA-VAP-1", "Exp-2", "Exp-3")))




#plot showing variation in yield as affected by NPK rate by experiment and season:
ds %>%
  gather(nutrient, rate, N:K) %>%
  mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
  ggplot(aes(rate, TY)) + 
  geom_point(alpha=.33, shape=16) +
  facet_grid(nutrient ~ Experiment+season) + 
  xlab("\nFertilizer nutrient application rate [kg/ha]") +
  ylab("Observed tuber yield [kg/ha]\n") +
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))



ds %>% 
  gather(nutrient, rate, N:K) %>%
  mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
  ggplot(aes(season, TY)) + 
  geom_boxplot() +
  facet_wrap(~Experiment, scales = "free_x") + 
  xlab(" Season \n (b)") +
  ylab("Potato tuber yield [t/ha]\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle=90, hjust=1, vjust=0.5),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")

ds %>% 
  gather(nutrient, rate, N:K) %>%
  mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
  ggplot(aes( trt2, TY, fill=season)) + 
  geom_boxplot() +
  facet_wrap(~Experiment+season, scales = "free_x") + 
  xlab("\n (b) Fertilizer nutrient application rate [kg/ha]") +
  ylab("Observed tuber yield [kg/ha]\n") +
  theme_bw() +
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle=90, hjust=1, vjust=0.5),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")


## density plot
ds %>% 
  ggplot(aes(x = TY, colour=season, fill=season)) +
  geom_density(alpha=.2, linewidth=1) +
  facet_wrap(~Experiment, scales="free_y", ncol=1) +
  theme_gray()+
  xlab("Potato tuber yield [t/ha] \n (a) ")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 15, face="bold", hjust=0))



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



ggplot()+
  geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  # geom_sf(data = rwshp3[rwshp3$NAME_1 %in% c("Amajyaruguru","Amajyepfo","Iburengerazuba"),], linewidth = 0.2, color = "white", fill=NA) + 
  geom_sf(data = rwshp2[rwshp2$NAME_1 %in% c("Amajyaruguru","Amajyepfo","Iburengerazuba"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) + 
  geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Amajyaruguru","Amajyepfo","Iburengerazuba"),], aes(label = NAME_2), size=3) +
  geom_point(data = ds, aes(x=as.numeric(lon), y=as.numeric(lat), shape = Experiment, colour = Experiment, size = Experiment))+
  scale_shape_manual(values = c(15, 16, 18))+
  scale_size_manual(values = c(2,2,3))+
  scale_colour_manual(values = c("cornflowerblue", "blue", "blue4"))+
  scale_fill_manual(values = c("darkgoldenrod1", "darkgoldenrod", "burlywood"), name= "Agrocology")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=12))


RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
gpsPoints <- Soil_PointData_AOI[, c("longitude", "latitude")]
gpsPoints$longitude <- as.numeric(gpsPoints$longitude)
gpsPoints$latitude <- as.numeric(gpsPoints$latitude)
RAW_AEZ_trial <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))

dsoil_topo <-  RAW_AEZ_trial %>%
  dplyr::select(c(Names_AEZs)) %>%
  cbind(Soil_PointData_AOI) %>% 
  left_join(Topo)

dsoil_topoL <- dsoil_topo %>% 
  gather(variable, value, c_tot_top:TRI) 
dsoil_topoL <- droplevels(dsoil_topoL[!dsoil_topoL$variable %in% c("texture_class_bottom", "texture_class_top", "NAME_1", "NAME_2"),])
dsoil_topoL$value = as.numeric(dsoil_topoL$value)




################################################################
# 4. Running reverse QUEFTS  to get apparent soil nutrient supply
################################################################

## get soil INS
supply <- NULL
for(i in unique(ds$TLID)){
  #subsetting and preparing data for revQUEFTS:
  dsi <- ds[ds$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y" #Aim is to explain variation in the BLUP yields
  dsi$Y <- dsi$Y * 1000 * 0.21 #converting to kg DM/ha, assuming 79% moisture content
  
  yy <- dsi[dsi$refTreat == "TRUE", ]
  Yai <- mean(yy$Y) * 1.2 ## correcting for yield loss due to poor management and because fert.rates are not high enough to remove nutrient deficiency 
  
  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 2 & !is.na(Yai)){
    
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = "Potato")
    print(si)
    supply <- rbind(supply, data.frame(TLID = i,
                                       Ya = Yai,
                                       N_base_supply = si[1],
                                       P_base_supply = si[2],
                                       K_base_supply = si[3]))
  }
}

saveRDS(supply, paste0(result_full_path, "/soilINS_revQUEFTS.RDS"))

supply <- readRDS(paste0(result_full_path, "/soilINS_revQUEFTS.RDS"))

################################################################
# 5. investigate the estimated soil NPK supply 
###############################################################
## merge supply with the data
INS <- supply %>%
  left_join(ds %>% dplyr::select(TLID, lat, lon, Experiment, season) %>% unique()) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
         P_base_supply = ifelse(P_base_supply > 1000, 1000, P_base_supply),
  ) %>%
  mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
  na.omit()


#Create plot to demonstrate ranges in supply by expCode and season combinations:
INS %>%
  gather(variable, value, N_base_supply:K_base_supply) %>%
  mutate(variable = factor(variable, levels = c("N_base_supply", "P_base_supply", "K_base_supply")),
         variable = revalue(variable, c("N_base_supply" = "N",
                                        "P_base_supply" = "P",
                                        "K_base_supply" = "K")),
         season = ifelse(grepl("A", season), "A", "B")) %>%
  ggplot(aes(x = Experiment, y = value, fill = season)) + 
  geom_boxplot()+
  scale_fill_manual(values = c("grey90", "grey50"))+
  facet_wrap(~variable, nrow=1) +
  #ylim(0,500) +
  theme_bw() +
  ylab("Apparent soil nutrient supply [Mg ha-1]\n") +
  theme(axis.title.y = element_text(size = 15, face="bold"),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.08, 0.85),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

## set maximal N to 250,  and K supply to 525 and maximal P supply to 150 kg ha-1. 
## this is done based on expert knowledge of the soils in the target area and data evaluation.  
INS$N_base_supply <- ifelse(INS$N_base_supply > 250, 250,  INS$N_base_supply)
INS$K_base_supply <- ifelse(INS$K_base_supply > 525, 525,  INS$K_base_supply)
INS$P_base_supply <- ifelse(INS$P_base_supply > 150, 150,  INS$P_base_supply)


###################################################
## use the supply and estimate yield estimate to validate QUEFTS
## at the same time estimate yield with blanket recommendation 
###################################################
ds_validate <- unique(ds[, c("treat", "N",  "P","K", "blup", "TLID", "expCode", "refTreat")])
ds_validate$index <- c(1:nrow(ds_validate))

supply_Qy <- NULL
for (i in unique(ds_validate$index)){
  print(i)
  tdata1 <- ds_validate[ds_validate$index == i, ]
  tdata2 <- supply[supply$TLID==tdata1$TLID, ]
  
  ## attainable yield in t/ha and dry wt.
  yya <- tdata2$Ya
  
  
  if(nrow(tdata2) > 0){
    tdata1$yieldQUEFTS <- runQUEFTS(nut_rates = data.frame(N=tdata1$N, P=tdata1$P, K=tdata1$K),
                                    supply = c(tdata2$N_base_supply, tdata2$P_base_supply, tdata2$K_base_supply),
                                    crop = Crop,
                                    Ya = yya,
                                    SeasonLength = SeasonLength)
    
    ## 300 kg NPK171717 will add 51 kg N, 22.2 kg P and 42 kg K. how much yield can be obtained according to QUEFTS with blanket advice
    tdata1$yieldBR <- runQUEFTS(nut_rates = data.frame(N=51, P=22.2, K=42),
                                supply = c(tdata2$N_base_supply, tdata2$P_base_supply, tdata2$K_base_supply),
                                crop = Crop,
                                Ya = yya,
                                SeasonLength = SeasonLength)
    
    ## yield at zero NPK input 
    tdata1$yieldZero <- runQUEFTS(nut_rates = data.frame(N=0, P=0, K=0),
                                  supply = c(tdata2$N_base_supply, tdata2$P_base_supply, tdata2$K_base_supply),
                                  crop = Crop,
                                  Ya = yya,
                                  SeasonLength = SeasonLength)
    
    
    
    # from dry matter to fresh weight
    tdata1$yieldQUEFTS <- (tdata1$yieldQUEFTS / 0.21)
    tdata1$yieldBR <- (tdata1$yieldBR / 0.21)
    tdata1$yieldZero <- (tdata1$yieldZero / 0.21)
    
    supply_Qy <- rbind(supply_Qy, tdata1)
  }
}

saveRDS(supply_Qy, paste(result_full_path, "supply_Qy.RDS", sep=""))
supply_Qy <- readRDS(paste(result_full_path, "supply_Qy.RDS", sep=""))

ds_sdt <- ds %>%
  left_join(supply_Qy[, c("treat","TLID", "expCode", "yieldQUEFTS", "yieldBR", "yieldZero")]) 

ggC <- ggplot(ds_sdt, aes(blup, yieldQUEFTS/1000)) +
  geom_point() +
  geom_abline() +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  xlab("Observed yield [t/ha]") + ylab("Predicted yield using reverse QUEFTS [t/ha]") +
  theme_bw() +
  theme(axis.text= element_text(size=14), axis.title = element_text(size=15, face ="bold"))


################################################################
# 6. aggregate the soil supply at AEZ zones and predict yield  
###############################################################
gpsPoints <- unique(INS[, c("lon", "lat")])
gpsPoints$longitude <- as.numeric(gpsPoints$lon)
gpsPoints$latitude <- as.numeric(gpsPoints$lat)
ANS_AEZ <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))

ANS_AEZ <- ANS_AEZ %>%
  dplyr::select(c(Names_AEZs)) %>%
  dplyr::rename(AEZ = Names_AEZs) %>% 
  cbind(gpsPoints[, c("lon", "lat")])


INS <- INS %>%
  left_join(ANS_AEZ) %>% 
  dplyr::filter(AEZ %in% c("Birunga", "Buberuka highlands", "Congo-Nile watershed divide"))

INS <- INS %>%
  left_join(supply_Qy[, c("TLID",  "yieldBR", "yieldZero")]) %>% 
  unique()


summary(INS$Ya )## is still in dry wt and in kg/ha so no need to chance it 
median_ANS_AEZ <- ddply(INS, .(AEZ), summarize, 
                        Ya = median(Ya),
                        N_med_ANS = median(N_base_supply), 
                        P_med_ANS = median(P_base_supply), 
                        K_med_ANS = median(K_base_supply),
                        Y_BR = median(yieldBR),
                        Y0 = median(yieldZero))

## setting all yield to dry weight 
median_ANS_AEZ$Y_BR <- median_ANS_AEZ$Y_BR*0.21
median_ANS_AEZ$Y0 <- median_ANS_AEZ$Y0*0.21

###########################################################
## Calculate what yield the current blanket recommendation (6 bags (300 kg) of NPK 171717/ha) 
###########################################################
# data.frame(type = c("DAP", "Urea", "NPK"), Ncont =c(18, 46, 17), Pcont=c(20, 0, 7.4), Kcont=c(0,0,14), price=c(722, 640,654))

my_ferts <- data.frame(group = "synthetic", name = c("DAP", "Urea", "NPK"), 
                       N = c(18, 46, 17), P = c(20, 0, 7.4), K = c(0,0,14))

fertrecom_aez <- NULL
for(j in 1:nrow(median_ANS_AEZ)){
  fertperc <- NULL
  for(perc in seq(0, 0.5, 0.1)){
    fertrecom1 <-   rec_targetdY_pot(my_ferts=my_ferts, 
                                     dY = perc, 
                                     target = "relative", 
                                     start = rep(0, nrow(my_ferts)), 
                                     supply = c(median_ANS_AEZ$N_med_ANS[j], median_ANS_AEZ$P_med_ANS[j], median_ANS_AEZ$K_med_ANS[j]),
                                     att_GY = median_ANS_AEZ$Ya[j], 
                                     GY_br = median_ANS_AEZ$Y_BR[j],
                                     crop = "Potato",
                                     SeasonLength = 120,
                                     isBlanketRef = TRUE, 
                                     df_link  = median_ANS_AEZ[j, ])
    fertperc <- rbind(fertperc, fertrecom1)
  }
  fertrecom_aez <- rbind(fertrecom_aez, fertperc)
}


fertrecom_aez$Ya <- fertrecom_aez$Ya/0.21
fertrecom_aez$Y_BR <- fertrecom_aez$Y_BR/0.21
fertrecom_aez$Y0 <- fertrecom_aez$Y0/0.21

dd_AEZ_0 <- fertrecom_aez[fertrecom_aez$yieldPercinc == "0 %", ]
dd_AEZ_10 <- fertrecom_aez[fertrecom_aez$yieldPercinc == "10 %", ]
dd_AEZ_20 <- fertrecom_aez[fertrecom_aez$yieldPercinc == "20 %", ]




