#################################################################################################################
## sourcing required packages 
#################################################################################################################
# "geosphere",
packages_required <- c("terra", "sf", "rgl",  "sp", "geodata", "plyr","tidyverse",  "countrycode", "lubridate",  
                       "parallel", "foreach")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))

#####################################################################################################################################
#' @description a function to be used to define path, input data (GPS files), geospatial layers
#'
#' @param dataPath  Main path where the input data are saved
#' @param country Country name for which the data will be extracted
#' @param useCaseName Name of the use case/research project
#' @param Crop Name of the crop
#' @param inputData data frame with the locations to extract the data. If NULL a default file in the raw folder inside the dataPath will be used. Must have the c(lat, lon, plantingDate, harvestDate). For field observations, plantingDate  harvestDate should be given in yyyy-mm-dd format.
#' @param soilProfile true or false based on if the user need to have soil data by different depths (whole profile)
#' @param AOI TRUE if data for multiple years is required. FALSE if data is required for field trials, for which the actual interval between the planting and harvest dates will be used. 
#' @param pathOut the path to write out the sourced data
#' @param varsbasePath Path with all the weather spatial layers, if NULL a default path is assumed  
#' @return
#' @export
#'
#' @examples Paths_Vars_soil(dataPath = "D:/demo-repository/Data/data_sourcing",country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
#'                                    inputData=NULL, soilProfile =TRUE, AOI=TRUE, pathOut=NULL, varsbasePath=NULL) 
#'                             
Paths_Vars_soil <- function(dataPath,country, useCaseName, Crop, inputData = NULL, soilProfile =TRUE, AOI = TRUE, pathOut=NULL, varsbasePath=NULL){
  
  
  readLayers_soil_isric <- NULL
  shapefileHC <- NULL
  
  if(is.null(inputData)){
    if(AOI == TRUE){
      inputData <- readRDS(paste(dataPath,"/useCase_",country, "_", useCaseName,"/", Crop, "/raw/AOI_GPS.RDS", sep=""))
    }else{
      inputData <- readRDS(paste(dataPath,"/useCase_",country, "_", useCaseName,"/", Crop, "/raw/compiled_fieldData.RDS", sep=""))
    }
  }
  
  if(is.null(varsbasePath)){
    varsbasePath <- paste0(dataPath,"/useCase_",country, "_", useCaseName,"/", Crop, "/Landing/")
  }
  
  

      if(soilProfile == TRUE){
    listRaster_soil <-list.files(path=paste0(varsbasePath, "Soil/soilGrids/profile"), pattern=".tif$")
    readLayers_soil <- terra::rast(paste(paste0(varsbasePath, "Soil/soilGrids/profile"), listRaster_soil, sep="/"))
    shapefileHC <- st_read(paste0(varsbasePath, "Soil/HC27/HC27 CLASSES.shp"), quiet= TRUE)%>%
      st_make_valid()
    if(is.null(pathOut)){
      pathOut <- paste(dataPath,"/useCase_", country, "_", useCaseName,"/", Crop, "/result/geo_4cropModel/", sep="")
    }
  }else{
    listRaster_soil <-list.files(path=paste0(varsbasePath, "Soil/iSDA"), pattern=".tif$")
    readLayers_soil <- terra::rast(paste(paste0(varsbasePath, "Soil/iSDA"), listRaster_soil, sep="/"))
    listRaster_soil_isric <-list.files(path=paste0(varsbasePath, "Soil/soilGrids"), pattern=".tif$")
    readLayers_soil_isric <- terra::rast(paste(paste0(varsbasePath, "Soil/soilGrids"), listRaster_soil_isric, sep="/"))
    if(is.null(pathOut)){
      pathOut <- paste(dataPath,"/useCase_", country, "_", useCaseName,"/", Crop, "/result/geo_4ML/", sep="")
    }

  }
  return(list(inputData, readLayers_soil, readLayers_soil_isric, shapefileHC, pathOut))
}



################################################################################
# https://rdrr.io/cran/geodata/man/soil_grids.html
# https://rdrr.io/cran/geodata/man/soil_af.html
# https://rdrr.io/cran/geodata/man/soil_af_isda.html
# https://rdrr.io/cran/geodata/man/elevation.html DEm data from SRTM
#' @description Is a helper function for extract_geoSpatialPointData. Extract geo-spatial data with no temporal dimension, i,e,. soil properties and topography variables
#' 
#' @param country country name to be sued to extract the first two level of administrative units to attach to the data. 
#' @param inputData is a data frame and must have the c(lat, lon) 
#' @param soilProfile is true/false, if true ISRIC data for the six soil profiles will be processed. This is required for DSSAT and other crop models. 
#' @param pathOut is path used to download the DEM layers temporarily and these layers can be removed after obtaining the data from this function. 
#' @param Layers_soil Raster layers with soil variables of ISRIC by different depths if soilProfile =TRUE. List of rasters from iSDA with two depths (20, 50 cm) if soilProfile=FALSE.   
#' @param Layers_soil_isric Raster layers with soil chemical variables of ISRIC up to 30 cm depth.
#' @param shapefileHC shapefile with the soil classification from harvest choice in 27 main soil profiles used to create the soil file in DSSAT format
#' 
#' @return a data frame with lon, lat,teh top two admistrnative zones, soil properties with columns named with variable names attached with depth,  
#' elevations variables attached for every GPS location 
#' @examples:  inputData = data.frame(lon=c(29.35667, 29.36788), lat=c(-1.534350, -1.538792)
#' get_soil_DEM_pointData(country = "Rwanda",inputData=inputData, soilProfile = FALSE, pathOut = getwd(),
#'Layers_soil = Layers_soil, Layers_soil_isric= Layers_soil_isric, shapefileHC = shapefileHC)) #See Paths_Vars_soil function that reads the files Layers_soil, Layers_soil_isric, and shapefileHC

get_soil_DEM_pointData <- function(country, inputData, soilProfile = FALSE, pathOut, Layers_soil = Layers_soil, Layers_soil_isric= Layers_soil_isric, shapefileHC = shapefileHC){
  
  
  ## 2. read the shape file of the country and crop the global data
  countryShp <- geodata::gadm(country, level = 2, path='.')
  inputData$country = country
  
  dd2 <- raster::extract(countryShp, inputData[, c("lon", "lat")])[, c("NAME_1", "NAME_2")]
  inputData$NAME_1 <- dd2$NAME_1
  inputData$NAME_2 <- dd2$NAME_2
  
  inputData2 <- unique(inputData[, c("lon", "lat", "NAME_1", "NAME_2", "country")])
  inputData2 <- inputData2[complete.cases(inputData2), ]
  inputData2$ID <- c(1:nrow(inputData2))
  gpsPoints <- unique(inputData2[, c("lon", "lat")])
  gpsPoints$lon <- as.numeric(gpsPoints$lon)
  gpsPoints$lat <- as.numeric(gpsPoints$lat)
  # gpsPoints <- gpsPoints[, c("x", "y")]
  areasCovered <- unique(c(raster::extract(countryShp, gpsPoints)$NAME_2))
  areasCovered <- areasCovered[!is.na(areasCovered)]
  print(areasCovered)
  
 # crop_extent <- ext(min(inputData2$lon)-1, max(inputData2$lon)+1, min(inputData2$lat)-1, max(inputData2$lat)+1)
 # croppedLayer_soil <- crop(Layers_soil, crop_extent)

  
  for(aC in areasCovered){
    print(aC)
    countryShpA <- countryShp[countryShp$NAME_2 == aC]
    croppedLayer_soil <- terra::crop(Layers_soil, countryShpA)
    ## 3. apply pedo-transfer functions to get soil organic matter and soil hydraulics variables 
    if (soilProfile == TRUE){
      
      depths <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm")  
      ## get soil organic matter as a function of organic carbon
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("soc_",depths[i])]] * 2)/10
      }
      
      
      ##### permanent wilting point (cm3/cm3) ####
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100) + 0.487 *
          croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.005*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.013*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
          0.068*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100 ) + 0.031
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + 
                                                            (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
      }
      
      ##### FC (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100 + 0.195 * 
          croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.006*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.027*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
          0.452*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100) + 0.299
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
        
      }
      
      
      ##### soil water at saturation (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.034*
          (croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
          0.018*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
          (croppedLayer_soil[[paste0("clay_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
          0.584 * (croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.078
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.043)
        
      }
      
      ##### saturated conductivity (mm/h) ######
      for(i in 1:length(depths)) {
        b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
        lambda <- 1/b
        croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
      }
      
      soilData <- croppedLayer_soil
      
    }else{
      
      depths <- c("0-20cm","20-50cm")  
      
      ## get soil organic matter as a function of organic carbon
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("oc_",depths[i])]] * 2)/10
      }
      
      ##### permanent wilting point (cm3/cm3) ####
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100) + 0.487 *
          croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.005*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.013*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
          0.068*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 ) + 0.031
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
      }
      
      
      
      ##### FC (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 + 0.195 * 
          croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.006*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.027*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
          0.452*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100) + 0.299
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
        
      }
      
      
      ##### soil water at saturation (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.034*
          (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
          0.018*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
          (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
          0.584 * (croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.078
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.043)
        
      }
      
      ##### saturated conductivity (mm/h) ######
      for(i in 1:length(depths)) {
        b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
        lambda <- 1/b
        croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
      }
      
      names(croppedLayer_soil) <- gsub("0-20cm", "top", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("20-50cm", "bottom", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("_0-200cm", "", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("\\.", "_",  names(croppedLayer_soil)) 
      croppedLayer_isric <- terra::crop(Layers_soil_isric, countryShpA)
      names(croppedLayer_isric) <- gsub("0-30cm", "0_30", names(croppedLayer_isric))
      soilData <- c(croppedLayer_soil, croppedLayer_isric)
    }
    if(aC == areasCovered[1]){
      soilData_allregion <- soilData
    }else{
      soilData_allregion <- merge(soilData_allregion, soilData)
    }
  }
  
  
  ## 4. Extract point soil data 
  pointDataSoil <- as.data.frame(raster::extract(soilData_allregion, gpsPoints))
  pointDataSoil <- subset(pointDataSoil, select=-c(ID))
  pointDataSoil <- cbind(unique(inputData2[, c("country", "NAME_1", "NAME_2", "lon", "lat")]), pointDataSoil)

  ## 5. Extract DEM data: at lon and lat at steps of 5 degree
  # countryExt <- terra::ext(countryShp)
                           
    # countryExt <- terra::ext(countryShp[countryShp$NAME_2 %in% unique(inputData2$NAME_2)])
    # 
    # lons <- seq(countryExt[1]-1, countryExt[2]+1, 5)
    # lats <- seq(countryExt[3]-1, countryExt[4]+1, 5)
    # griddem <- expand_grid(lons, lats)
    # 
    # # ## if the extent is not fully within a distince of 5 degrees this does not work, otherwise this would have been better script
    # listRaster_dem1 <-geodata::elevation_3s(lon=countryExt[1], lat=countryExt[3], path=getwd()) #xmin - ymin
    # listRaster_dem2 <-geodata::elevation_3s(lon=countryExt[1], lat=countryExt[4], path=getwd()) #xmin - ymax
    # listRaster_dem3 <-geodata::elevation_3s(lon=countryExt[2], lat=countryExt[3], path=getwd()) #xmax - ymin
    # listRaster_dem4 <-geodata::elevation_3s(lon=countryExt[2], lat=countryExt[4], path=getwd()) #xmax - ymax
    # listRaster_dem <- terra::mosaic(listRaster_dem1, listRaster_dem2, listRaster_dem3, listRaster_dem4, fun='mean')
    
    # dems <- c()
    # listRaster_demx <- NULL
    # for(g in 1:nrow(griddem)){
    #   listRaster_demx <- tryCatch(geodata::elevation_3s(lon=griddem$lons[g], lat=griddem$lats[g], path=pathOut),error=function(e){})
    #   dems <- c(dems, listRaster_demx)
    # }
    # 
    # if(!is.null(dems)){
    #   ## if mosaic works with list as dems is here, the next step is not necessary
    #   if(length(dems) == 1){
    #     listRaster_dem <- dems[[1]]
    #   }else if(length(dems) > 1 & length(dems) < 12){
    #     for (k in c((length(dems)+1):12)){
    #       dems[[k]] <- dems[[1]] 
    #     }
    #     ## as it is not possible to define the number of tiles before hand, as assumption is made to have 12 tiles and when that is not the case the first time will be 
    #     ## duplicated and the mean of it will be take which should not affect the result
    #     listRaster_dem <- terra::mosaic(dems[[1]], dems[[2]],dems[[3]],dems[[4]],
    #                                     dems[[5]], dems[[6]], dems[[7]],dems[[8]],
    #                                     dems[[9]], dems[[10]], dems[[11]],dems[[12]],
    #                                     fun='mean')
    #   }
    #   
    #   dem <- terra::crop(listRaster_dem, countryShp[countryShp$NAME_2 %in% unique(inputData2$NAME_2)])
    #   slope <- terra::terrain(dem, v = 'slope', unit = 'degrees')
    #   tpi <- terra::terrain(dem, v = 'TPI')
    #   tri <- terra::terrain(dem, v = 'TRI')
    #   
    #   ### ideally these four dem layers will be made to a list so that point extraction will be done at once, bu CG Labs capacity does not allow that ...
    #   topoLayer <- terra::rast(list(dem, slope, tpi, tri))
    #   datatopo <- terra::extract(topoLayer, gpsPoints, method='simple', cells=FALSE)
    #   datatopo <- subset(datatopo, select=-c(ID))
    #   topoData <- cbind(inputData2[, c("lon", "lat")], datatopo)
    #   names(topoData) <- c("lon", "lat" ,"altitude", "slope", "TPI", "TRI")
    #   
    #   
    #   pointDataSoil <- unique(merge(pointDataSoil, topoData, by=c("lon", "lat")))
    #   
    #   
    # }else{
    #   pointDataSoil = pointDataSoil
    # }
  
    
  ## 6. Extract harvest choice soil class and drainage rate (just for profile =TRUE)
  if(soilProfile == TRUE){
    coordinates_df <- data.frame(lat=pointDataSoil$lat, lon=pointDataSoil$lon)
    coordinates_sf <- st_as_sf(coordinates_df, coords = c("lon", "lat"), crs = 4326)
    intersecting_polygons <-st_join(coordinates_sf, shapefileHC)
    # Extract the geometry (latitude and longitude) from the 'joined_data' object
    intersecting_polygons  <-intersecting_polygons  %>%
      mutate(lon = st_coordinates(intersecting_polygons)[, "X"], 
             lat = st_coordinates(intersecting_polygons)[, "Y"]) 
    intersecting_polygons  <-as.data.frame(intersecting_polygons)
    intersecting_polygons$geometry <- NULL
    intersecting_polygons$ID <- NULL
  
  
     # Join the LDR (drainage rate) values to the intersecting_polygons data
    LDR_data <- data.frame(LDR = c(rep(0.2, 9), rep(0.5, 9), rep(0.75, 9)),
                           GRIDCODE = seq(1:27))
    
    LDR_data <- merge(intersecting_polygons,LDR_data)
    LDR_data$GRIDCODE <- NULL
    pointDataSoil <- unique(merge(pointDataSoil, LDR_data, by=c("lon", "lat")))
    }
  
  
  return(pointDataSoil)
}

################################################################################
#' Title Extract soil data
#' @description This function extracts soil data based on the input data for GPS and dates for specific country-use Case-crop combination 
#' for the trial sites named as "compiled_fieldData.RDS" and for target areas as AOI_GPS.RDS. The input data should have lon, lat, ID, planting and harvest dates
#'
#' @param country country name to be used for cropping, extracting the top two administrative region names and to define input and output paths
#' @param useCaseName use case name or a project name, and this is used to define input and output paths
#' @param Crop is crop name and is used to define input and output paths
#' @param AOI is TRUE is the input data has defined planting and harvest dates otherwise FALSE
#' @param Planting_month_date planting month and date in mm-dd format and must be provided if AOI is TRUE. It is the earliest possible planting date in the target area. 
#' @param Harvest_month_date harvest month and date in mm-dd format and must be provided if AOI is TRUE 
#' @param plantingWindow is given when several planting dates are to be tested to determine optimal planting date and it should be given in number of weeks starting from Planting_month_date 
#' @param weatherData is TRUE is weather data is required otherwise FALSE
#' @param soilData is TRUE if soil data is required otherwise FALSE
#' @param soilProfile is TRUE if soil data from the six profile of ISRIC is required, otherwise set to FALSE
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param jobs number of cores used to parallel weather data extraction
#'
#' @return If weatherData is TRUE, list of data frames with daily data for c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed") is returned. 
#' If soilData is set TRUE, soil properties at different depth plus elevation and derivatives of DEM are returned. These results are written out in paths defined by country, useCaseName, and crop 
#' and either raw or result of the different AgWise modules space in CG Labs. If AOI is set TRUE, the weather data between the Planting_month_date and Harvest_month_date and for 1979 - 2022 data will be returned. 

#' @examples extract_geoSpatialSoilPointData(dataPath = "D:/demo-repository/Data/data_sourcing",country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI=FALSE,
#' inputData = NULL,soilProfile = FALSE, season = 1, pathOut = NULL,jobs =10)
extract_geoSpatialSoilPointData <- function(dataPath,country, useCaseName, Crop,  AOI=FALSE, inputData = NULL,
                                        soilProfile = FALSE, season = 1, pathOut = NULL, jobs=10){
  

  ARD <- Paths_Vars_soil(dataPath,country=country, useCaseName=useCaseName, Crop=Crop, inputData = inputData, 
                     soilProfile =soilProfile, AOI = AOI,  pathOut = pathOut, varsbasePath=NULL)

  
  inputData <- ARD[[1]]
  Layers_soil <- ARD[[2]]
  Layers_soil_isric <- ARD[[3]]
  shapefileHC <- ARD[[4]]
  pathOut <- ARD[[5]]
  
  if (!dir.exists(pathOut)){dir.create(file.path(pathOut), recursive = TRUE)}


  if(season == 1){
    sData <- get_soil_DEM_pointData(country = country, soilProfile = soilProfile, pathOut=pathOut,
                                    inputData = inputData, Layers_soil = Layers_soil, Layers_soil_isric=Layers_soil_isric, shapefileHC = shapefileHC)
    
    if(AOI == TRUE){
      if (soilProfile == TRUE){
        s_name <- "SoilDEM_PointData_AOI_profile.RDS"
      }else{
        s_name <- "SoilDEM_PointData_AOI.RDS"
      }
    }else{
      if (soilProfile == TRUE){
        s_name <- "SoilDEM_PointData_trial_profile.RDS"
      }else{
        s_name <- "SoilDEM_PointData_trial.RDS"
      }
    }
    saveRDS(sData, paste(pathOut, s_name, sep="/"))
  }
    
  return(sData)

  
}






