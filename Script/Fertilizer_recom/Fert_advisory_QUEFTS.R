##############################################################################################
## 1. source paclkages and fucntions needed for teh analytics
##############################################################################################
rm(list=ls())
packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel","MuMIn","ggpmisc","sf","cluster","h2o",
                       "limSolve", "lpSolve", "Rquefts", "rgdal", "randomForest","ranger","Metrics", "factoextra")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")


#################################################################################################################
# 2. give path for input data and where to write results and define use case essentials 
#################################################################################################################
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"


pathInFieldData <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", sep="")
pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")
if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}




#################################################################################################################
# 3. read data 
#################################################################################################################
## field data
ds <- unique(readRDS(paste(pathInFieldData, "compiled_fieldData.RDS", sep="")))
soil <- unique(readRDS(paste(pathInFieldData, "Soil_PointData_trial.RDS", sep="")))
Topo <- unique(readRDS(paste(pathInFieldData, "Topography_AEZ_trial.RDS", sep="")))
AEZ <- readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")


#################################################################################################################
# 3. data wrangling and visualizing 
#################################################################################################################

ds <- subset(ds, select=-c(yieldEffectraw, yieldEffectBlup,refY, refYBLUP))
#plot showing yield ranges by experiment and season:
ds %>%
  ggplot(aes(x = season, y = TY)) +
  geom_boxplot() +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  coord_flip()+
  theme_gray()+
  ylab("\nPotato tuber yield [t/ha]")+
  theme(axis.title.x = element_text(size = 15, face="bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))

ds <- ds %>% 
  dplyr::mutate(Experiment = if_else(expCode == "IFDC", "Exp-1", 
                                     if_else(expCode == "SA-VAP-1", "Exp-2", "Exp-3"))) %>% 
  dplyr::mutate(trt = paste0("N",N, "P", P, "K",K)) %>% 
  dplyr::mutate(trt =  ifelse(treat %in% c("NPK11","NPK_increased","NPK", "NPK_all"), "Reference", 
                              ifelse(treat == "N0P0K0", "Control" ,trt))) %>% 
  dplyr::mutate(trt2 =  ifelse(trt == "N0P0K0", "Control", trt)) %>%  
  dplyr::mutate(Experiment = if_else(expCode == "IFDC", "Exp-1 (2014 B)", 
                                     if_else(expCode == "SA-VAP-1", "Exp-2 (2021 A & B)", "Exp-3 (2022 A & B)")))




#plot showing variation in yield as affected by NPK rate by experiment and season:
ds %>%
  gather(nutrient, rate, N:K) %>%
  mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
  ggplot(aes(rate, TY)) + 
  geom_point(alpha=.33, shape=16) +
  facet_grid(nutrient ~ expCode+season) + 
  xlab("\nFertilizer nutrient application rate [kg/ha]") +
  ylab("Observed tuber yield [kg/ha]\n") +
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))



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
  xlab("\n (a) Potato tuber yield [t/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 15, face="bold", hjust=0))



#map with trial locations:
country <- "Rwanda"
rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
rwlake <- st_read("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/Lakes/RWA_Lakes_NISR.shp")
rwAEZ <- readOGR(dsn="~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/AEZ",  layer="AEZ_DEM_Dissolve")
rwAEZ <- rwAEZ[rwAEZ$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]
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

## soil, totpo and AEZ data
soil[soil$ID == "IFDC_3", ]$NAME_1 <- "Amajyaruguru"
soil[soil$ID == "IFDC_3", ]$NAME_2 <- "Burera"
Topo[Topo$ID == "IFDC_3", ]$NAME_1 <- "Amajyaruguru"
Topo[Topo$ID == "IFDC_3", ]$NAME_2 <- "Burera"
Topo[Topo$ID == "SATLRW475382409484", ]$NAME_1 <- "Iburengerazuba"
Topo[Topo$ID == "SATLRW475382409484", ]$NAME_2 <- "Rubavu"
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
gpsPoints <- soil[, c("longitude", "latitude")]
gpsPoints$longitude <- as.numeric(gpsPoints$longitude)
gpsPoints$latitude <- as.numeric(gpsPoints$latitude)
RAW_AEZ_trial <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))

RAW_AEZ_trial <- RAW_AEZ_trial %>%
  dplyr::select(c(Names_AEZs)) %>%
  cbind(soil[, c("ID", "longitude", "latitude")])



AEZ_Topo <- RAW_AEZ_trial %>%
  left_join(Topo)

ds_topo <- merge(ds, AEZ_Topo, by.x=c( "TLID",  "lon","lat") ,by.y=c("ID", "longitude", "latitude"))


### characterizing the soil properties within AEZ
RAW_AEZ_trial <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))
dsoil_topo <-  RAW_AEZ_trial %>%
  dplyr::select(c(Names_AEZs)) %>%
  cbind(soil) %>% 
  left_join(Topo[, c("ID", "altitude", "slope", "TPI", "TRI", "AltClass")])
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

saveRDS(supply, paste(pathOut1, "soilINS_revQUEFTS.RDS", sep=""))

################################################################
# 5. investigat  the estimated soil NPK supply 
###############################################################
## merge supply wih the data
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
## this is done based on expert knowl;edge of the soils in the target area and data evaluation.  
INS$N_base_supply <- ifelse(INS$N_base_supply > 250, 250,  INS$N_base_supply)
INS$K_base_supply <- ifelse(INS$K_base_supply > 525, 525,  INS$K_base_supply)
INS$P_base_supply <- ifelse(INS$P_base_supply > 150, 150,  INS$P_base_supply)



################################################################
# 6. aggregate the soil supply at AEZ zones and predict yield  
###############################################################




