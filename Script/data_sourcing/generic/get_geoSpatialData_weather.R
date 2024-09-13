#################################################################################################################
## sourcing required packages 
#################################################################################################################
# "geosphere",
packages_required <- c("terra", "sf", "rgl", "sp", "geodata", "countrycode", "lubridate",  "tidyverse", 
                       "parallel", "foreach","furrr")

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
#' @param Planting_month_date mm-dd of crop planting 
#' @param Harvest_month_date mm-dd of first crop harvesting
#' @param AOI TRUE if data for multiple years is required. FALSE if data is required for field trials, for which the actual interval between the planting and harvest dates will be used. 
#' @param processModel TRUE if the output data will be used for process based models.When pathOut=NULL, if TRUE the data will be saved in the folder result/geo_4cropModel if FALSE in result/geo_4ML 
#' @param pathOut the path to write out the sourced data 
#' @param varsbasePath Path with all the weather spatial layers, if NULL a default path is assumed  
#' @return
#' @export
#'
#' @examples Paths_Vars_weather(dataPath = "D:/demo-repository/Data/data_sourcing",country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
#'                                    inputData=NULL, Planting_month_date=NULL, Harvest_month_date=NULL, AOI=FALSE, ,processModel = TRUE, pathOut = NULL, varsbasePath=NULL) 
#'                                    
Paths_Vars_weather <- function(dataPath, country, useCaseName, Crop, inputData = NULL, Planting_month_date, Harvest_month_date,AOI = TRUE, processModel =TRUE, pathOut=NULL, varsbasePath=NULL){
  
  # dataPath <- "~/agwise-datacuration/dataops/datacuration/Data/useCase_"
  # OutputPath <- "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_"
  
  
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
  
  
  listRasterRF <- list.files(path=paste0(varsbasePath, "chirps-precipitation"), pattern=".nc$", full.names = TRUE)
    listRasterTmax <-list.files(path=paste0(varsbasePath, "TemperatureMax/AgEra"), pattern=".nc$", full.names = TRUE)
    listRasterTMin <-list.files(path=paste0(varsbasePath, "TemperatureMin/AgEra"), pattern=".nc$", full.names = TRUE)
    listRasterRH <-list.files(path=paste0(varsbasePath, "agera5-relativehumidity"), pattern=".nc$", full.names = TRUE)
    listRasterSR <-list.files(path=paste0(varsbasePath, "SolarRadiation/AgEra"), pattern=".nc$", full.names = TRUE)
    listRasterWS <-list.files(path=paste0(varsbasePath, "agera5-windSpeed"), pattern=".nc$", full.names = TRUE)

    if(processModel == TRUE){
      if(is.null(pathOut)){
        pathOut <- paste(dataPath,"/useCase_", country, "_", useCaseName,"/", Crop, "/result/geo_4cropModel/", sep="")
      }
    }else{
      if(is.null(pathOut)){
        pathOut <- paste(dataPath,"/useCase_", country, "_", useCaseName,"/", Crop, "/result/geo_4ML/", sep="")
      }
    }
   

  return(list(inputData, listRasterRF, listRasterTmax, listRasterTMin, listRasterRH,listRasterSR, listRasterWS, pathOut))
}

#####################################################################################################################################
#' @description a function to get weather data for a specific point in datacube
#'
#' @param datacube_path Path with the script in python getDataFromCubePoint.py
#' @param downloadpath Main path where the weather data will be stored
#' @param country Country name for which the data will be extracted (if bbox=NULL)
#' @param Planting_month_date mm-dd of crop planting
#' @param Harvest_month_date mm-dd of first crop harvesting
#' @param weather_variable Name of the weather variable to be extracted
#' @param useCaseName Name of the use case/research project
#' @param Crop Name of the crop
#' @param numberofyears Number of years that will be extracted
#' @param lastYear Last year of the weather data
#' @param x longitude location to extract the data
#' @param y latitude location to extract the data
#' @return spatial weather data from datacube 
#' @return
#' @export
#'
#' @examples
get_dataCube_point <-function(datacube_path,downloadpath,country,Planting_month_date,Harvest_month_date,weather_variable,useCaseName,Crop,numberofyears, lastYear, x,y){
  # Get bounding-box of the country of interest
  
  
  
  downloadpathfinal <- paste0(downloadpath,"/useCase_", country, "_" ,useCaseName,"/",Crop,"/Landing/",weather_variable,"/")
  
  if (!dir.exists(downloadpathfinal)) {
    dir.create(downloadpathfinal)
  }
  
  
  startDate <- Planting_month_date
  endDate <- Harvest_month_date
  
  
  setwd(datacube_path)
  # 1. Chirps-Precipitation & input manually parameters for startDate, endDate, bounding box and downloadpath
  getDataCommand <- paste0("python getDataFromCubePoint.py --startDate=",startDate," --endDate=",endDate,
                           " --x=",x," --y=",y,
                           "  --variable=",weather_variable," --downloadpath=",downloadpathfinal," --lastYear ",
                           lastYear," --numberofyears=",numberofyears)
  system(getDataCommand, intern = TRUE)
  
  
}


#################################################################################################################
# DATA SOURCE https://data.chc.ucsb.edu/products/CHIRPS-2.0/ for rainfall
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=form fr AgEra 5 data
# Is a helper function for extract_geoSpatialPointData. Extract geo-spatial data with time dimension 
#' @description this functions loops through all .nc files (~30 - 40 years) for rain. temperature, solar radiation, wind speed and relative humidity. 
#' Planting_month_date should be set to one month prior to the earliest possible planting month and date so that data is available to-set initial conditions while running crop model. 
#' 
#' @param datacube_path Path with the script in python getDataFromCubePoint.py
#' @param downloadpath Main path where the weather data will be stored
#' @param country country name to be used to extract the first two level of administrative units to attach to the data. 
#' @param inputData data frame with the locations to extract the data. If NULL a default file in the raw folder inside the dataPath will be used. Must have the c(lat, lon, plantingDate, harvestDate). For field observations, plantingDate  harvestDate should be given in yyyy-mm-dd format.
#' @param AOI TRUE if data for multiple years is required. FALSE if data is required for field trials, for which the actual interval between the planting and harvest dates will be used. 
#' @param Planting_month_date if AOI is TRUE, Planting_month_date should be provided in mm-dd format. weather data across years between Planting_month_date and Harvest_month_date will be provided. 
#' @param Harvest_month_date if AOI is TRUE, Harvest_month_date should be provided in mm-dd format.  weather data across years between Planting_month_date and Harvest_month_date will be provided
#' @param varName is the name of the variable for which data is required and it is one of c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed")
#' @param listRaster list with the raster files of the varName
#' @param plantingWindow number of weeks starting considering the Planting_month_date as earliest planting week. It is given when several planting dates are to be tested to determine optimal planting date and it should be given in  
#' @param jobs defines how many cores to use for parallel data sourcing
#' 
#' @return based on the provided variable name, this function returns, daily data for every GPS together with longitude, latitude, planting Date, harvest Date, NAME_1, NAME_2 and 
#' daily data with columns labelled with the concatenation of variable name and Julian day of data.  When AOI is set to FALSE, every GPS location is allowed to have 
#' its own unique planting and harvest dates and in this case, because the different GPS location can have non-overlapping dates, NA values are filled for dates prior to
#' planting and later than harvest dates. When AOI is true, the user defined Planting_month_date and Harvest_month_date is considered for all locations and data is provided across the years. 
#' The weather data is extracted 1 months before the actual planting month 
#' @examples: inputData <- data.frame(lon=c(29.3679, 29.3941,  29.390), lat=c(-1.539, -1.716, -1.716), 
#' plantingDate  = c("2020-08-27", "2020-09-04", "2020-09-04"),
#' harvestDate = c("2020-12-29", "2020-12-29", "2020-12-29"))
#' get_weather_pointData(datacube_path= "D:/demo-repository/Script/data_sourcing/generic/data_cube",downloadpath="D:/demo-repository/Data/data_sourcing",country = "Rwanda",inputData = inputData,  AOI=FALSE, 
#'                     Planting_month_date=NULL, Harvest_month_date=NULL, varName="temperatureMin", listRaster=list("D:/demo-repository/Data/data_sourcing/useCase_Rwanda_RAB/Maize/Landing/TemperatureMin/AgEra/2000.nc","D:/demo-repository/Data/data_sourcing/useCase_Rwanda_RAB/Maize/Landing/TemperatureMin/AgEra/2001.nc"), 
#'                     plantingWindow=1, jobs=10)
#'                
get_weather_pointData <- function(datacube_path,downloadpath, country, inputData,  AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, varName, listRaster, plantingWindow=1, jobs){
  if(AOI == TRUE){
    if(is.null(Planting_month_date) | is.null(Harvest_month_date)){
      print("with AOI=TRUE, Planting_month_date, Harvest_month_date can not be null, please refer to the documentation and provide mm-dd for both parameters")
      return(NULL)
    }
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    Harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    
    ## py and hy are used only as place holder for formatting purposes
    if(Planting_month < Harvest_month){
      planting_harvest_sameYear <- TRUE
      py <- 2000
      hy <- 2000
    }else{
      planting_harvest_sameYear <- FALSE
      py <- 2000
      hy <- 2001
    }
  
    
    ## set planting date one moth prior to the given Planting_month_date so that initial condition for the crop model could be set correctly
    Planting_month_date <- as.Date(paste0(py, "-",Planting_month_date)) ## the year is only a place holder to set planting month 1 month earlier
    Planting_month_date <- Planting_month_date %m-% months(1)
    
    ## set harvest date one month later to the make sure there is enough weather data until maturity 
    Harvest_month_date <- as.Date(paste0(hy, "-",Harvest_month_date)) ## the year is only a place holder 
  
   
    ## if multiple planting dates are to be tested, adjust the Harvest_month_date to extract weather data for the later planting dates.  
    if(plantingWindow > 1 & plantingWindow < 5){
      Harvest_month_date <- Harvest_month_date %m+% months(1)
    }else if(plantingWindow >= 5 & plantingWindow <=8){
      Harvest_month_date <- Harvest_month_date %m+% months(2)
    }
   }

  # 1. read all the raster files

   # if(AOI == TRUE & varName == "Rainfall"){
   #   listRaster <- listRaster[20:42]
   # }else if (AOI == TRUE){
   #   listRaster <- listRaster[22:44]
   # }
   # 

  
  
  ## 2. format the input data with GPS, dates and ID and add administrative unit info
  if(AOI == TRUE){
    countryCoord <- unique(inputData[, c("lon", "lat")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    ## After checking if planting and harvest happens in the same year, get the date of the year 
    countryCoord$startingDate <- Planting_month_date
    countryCoord$endDate <- Harvest_month_date
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "startingDate", "endDate")
    countryCoord$ID <- c(1:nrow(countryCoord))
    ground <- countryCoord[, c("longitude", "latitude", "startingDate", "endDate", "ID")]
    
  }else{
    inputData <- unique(inputData[, c("lon", "lat", "plantingDate", "harvestDate")])
    inputData$plantingDate <- as.Date(inputData$plantingDate)
    inputData$harvestDate <- as.Date(inputData$harvestDate)
    inputData$plantingDate <- inputData$plantingDate %m-% months(1)
    inputData <- inputData[complete.cases(inputData), ]
    inputData$ID <- c(1:nrow(inputData))
    names(inputData) <- c("longitude", "latitude", "startingDate", "endDate", "ID")
    ground <- inputData
  }
  
  # ground$harvestDate <- as.Date(ground$harvestDate, "%Y-%m-%d")
  countryShp <- geodata::gadm(country, level = 2, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  ## 3.get the seasonal rainfall parameters for AOI
  
  if(AOI == TRUE){
      if(varName %in% c("Rainfall", "relativeHumidity","windSpeed")) {
        
        cls <- makeCluster(jobs)
        doParallel::registerDoParallel(cls)
        
        rf_result <- foreach(i=1:length(listRaster), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
          rasti <- listRaster[i]
          PlHvD <- terra::rast(rasti)
          xy <- ground[, c("longitude", "latitude")]
          raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
          raini <- raini[,-1]
          if(varName %in% c("temperatureMax","temperatureMin")){
            raini <- raini-274
          }else if (varName == "solarRadiation"){
            raini <- raini/1000000
          }
          ground_adj <- ground
          names(raini) <- paste(varName, time(PlHvD), sep="_")
          lubridate::year(ground_adj$startingDate) <- min(lubridate::year(time(PlHvD)))
          lubridate::year(ground_adj$endDate) <- max(lubridate::year(time(PlHvD)))
          # names(raini) <- paste(varName, sub("^[^_]+", "", names(raini)), sep="")
          ground_adj$startingDate <- as.character(ground_adj$startingDate)
          ground_adj$endDate <- as.character(ground_adj$endDate)
          ground2 <- cbind(ground_adj, raini)
        }
        
        data_points <- dplyr::bind_rows(rf_result)
        stopCluster(cls)
      }else{
        if (planting_harvest_sameYear ==  TRUE) {
          
          cls <- makeCluster(jobs)
          doParallel::registerDoParallel(cls)
          
          rf_result <- foreach(i=1:length(listRaster), .packages = c('terra', 'plyr', 'stringr','tidyr','tidyverse')) %dopar% {
            rasti <- listRaster[i]
            pl_j <-as.POSIXlt(unique(ground$startingDate))$yday
            hv_j <-as.POSIXlt(unique(ground$endDate))$yday + 2
            PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
            xy <- ground[, c("longitude", "latitude")]
            raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
            raini <- raini[,-1]
            if(varName %in% c("temperatureMax","temperatureMin")){
              raini <- raini-274
            }else if (varName == "solarRadiation"){
              raini <- raini/1000000
            }
            ground_adj <- ground
            lubridate::year(ground_adj$startingDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
            lubridate::year(ground_adj$endDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
            start <- as.Date(unique(ground_adj$startingDate))
            maxDaysDiff <- abs(max(min(pl_j) - max(hv_j)))
            end <- start + as.difftime(maxDaysDiff, units="days")
            ddates <- seq(from=start, to=end, by=1)
            names(raini) <- paste(varName, ddates[1:length(names(raini))], sep="_")
            # names(raini) <- paste(varName, sub("^[^_]+", "", names(raini)), sep="")
            ground_adj$startingDate <- as.character(ground_adj$startingDate)
            ground_adj$endDate <- as.character(ground_adj$endDate)
            ground2 <- cbind(ground_adj, raini)
          }
          
          data_points <- dplyr::bind_rows(rf_result)
          stopCluster(cls)
        }else{
          cls <- makeCluster(jobs)
          doParallel::registerDoParallel(cls)
          ## Rainfall
          rf_result2 <- foreach(i = 1:(length(listRaster)-1), .packages = c('terra', 'plyr', 'stringr','tidyr','tidyverse')) %dopar% {
            listRaster <- listRaster[order(listRaster)]
            rast1 <- listRaster[i]
            rast2 <- listRaster[i+1]
            ground_adj <- ground
            lubridate::year(ground_adj$startingDate) <- as.numeric(str_extract(rast1, "[[:digit:]]+"))
            lubridate::year(ground_adj$endDate) <- as.numeric(str_extract(rast2, "[[:digit:]]+"))
            start <- as.Date(unique(ground_adj$startingDate))
            maxDaysDiff <- as.numeric(max(ground_adj$endDate) - min(ground_adj$startingDate))
            end <- start + as.difftime(maxDaysDiff, units="days")
            ddates <- seq(from=start, to=end, by=1)
            # Convert planting Date and harvesting in Julian Day 
            pl_j <-as.POSIXlt(unique(ground_adj$startingDate))$yday
            hv_j <-as.POSIXlt(unique(ground_adj$endDate))$yday
            rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
            rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
            PlHvD <- c(rasti1, rasti2)
            xy <- ground[, c("longitude", "latitude")]
            raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
            raini <- raini[,-1]
            if(varName %in% c("temperatureMax","temperatureMin")){
              raini <- raini-274
            }else if (varName == "solarRadiation"){
              raini <- raini/1000000
            }
            names(raini) <- paste(varName, ddates, sep="_")
            ground_adj$startingDate <- as.character(ground_adj$startingDate)
            ground_adj$endDate <- as.character(ground_adj$endDate)
            ground2 <- cbind(ground_adj, raini)
          }
          
          data_points <- dplyr::bind_rows(rf_result2)
          stopCluster(cls)
        }
      }
    
  } else {
    
    
    # Get the Year
    ground$yearPi <- as.numeric(format(as.POSIXlt(ground$startingDate), "%Y"))
    ground$yearHi <- as.numeric(format(as.POSIXlt(ground$endDate), "%Y"))
    

    
    ## drop data with planting dates before 2000, as there is no downloaded data for AgeEra files
    ground <- droplevels(ground[ground$yearPi >= 2000 & ground$yearHi >= 2000, ])
    
    # Convert planting date and harvesting date in Julian Day
    ground$ pl_j <-as.POSIXlt(ground$startingDate)$yday
    ground$hv_j <-as.POSIXlt(ground$endDate)$yday
    
    # get the max number of days on the field to be used as column names. 
    start <- as.Date(min(ground$startingDate))
    maxDaysDiff <- abs(max(min(ground$pl_j) - max(ground$hv_j)))
    end <- maxDaysDiff +  as.Date(max(ground$endDate)) # start + as.difftime(maxDaysDiff, units="days")
    ddates <- seq(from=start, to=end, by=1)
   
    # create list of all possible column names to be able to row bind data from different sites with different planting and harvest dates ranges
    # rf_names <- c(paste0(varName, "_",  c(min(ground$pl_j):max(ground$hv_j))))
    rf_names <- c(paste0(varName, "_",  ddates))
    rf_names2 <-  as.data.frame(matrix(nrow=length(rf_names), ncol=1))
    colnames(rf_names2) <- "dataDate"
    rf_names2[,1] <- rf_names
    rf_names2$ID <- c(1:nrow(rf_names2))
 
    
    data_points <- NULL
    
    
    if(varName %in% c("Rainfall", "relativeHumidity","windSpeed")) {
      plan(multisession)  # Or use multiprocess or multicore based on your system
      
      process_groundi <- function(i) {
        print(i)
        groundi <- ground[i, c("longitude", "latitude", "startingDate", "endDate", "ID", "NAME_1", "NAME_2", "yearPi", "yearHi", "pl_j", "hv_j")]
        yearPi <- as.numeric(groundi$yearPi)
        yearHi <- as.numeric(groundi$yearHi)
        pl_j <- groundi$pl_j
        hv_j <- groundi$hv_j
        
        # Get the month and day of planting and harvesting
        monthdayPi <- format(as.POSIXlt(groundi$startingDate), "%m-%d")
        monthdayHi <- format(as.POSIXlt(groundi$endDate), "%m-%d")
        
        weather_variable <- ifelse(varName == "Rainfall", 'chirps-precipitation',
                                   ifelse(varName == "relativeHumidity", 'agera5-relativehumidity',
                                          'agera5-windSpeed'))
        
        get_dataCube_point(datacube_path, downloadpath, country, Planting_month_date = monthdayPi,
                           Harvest_month_date = monthdayHi, weather_variable = weather_variable,
                           useCaseName, Crop, numberofyears = 1, lastYear = yearPi, 
                           x = groundi$longitude, y = groundi$latitude)
        
        downloadpathfinal <- paste0(downloadpath, "/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", weather_variable, "/")
        setwd(downloadpathfinal)
        csv_filename <- paste0(weather_variable, "_", groundi$longitude, "_", groundi$latitude, "_",
                               groundi$startingDate, "_", groundi$endDate, ".csv")
        raini <- read_csv(csv_filename, show_col_types = FALSE)
        
        file.remove(csv_filename)
        
        if (varName %in% c("temperatureMax", "temperatureMin")) {
          raini <- raini - 274
        } else if (varName == "solarRadiation") {
          raini <- raini / 1000000
        }
        
        start <- as.Date(unique(groundi$startingDate))
        maxDaysDiff <- as.numeric(groundi$endDate - groundi$startingDate)
        end <- start + as.difftime(maxDaysDiff, units = "days")
        ddates <- seq(from = start, to = end, by = 1)
        raini$dataDate <- paste(varName, ddates, sep = "_")
        rownames(raini) <- NULL
        
        # Merging data for different trials with differing growing periods
        raini <- merge(raini, rf_names2, by = "dataDate", all.y = TRUE)
        raini <- raini[order(raini$ID), ]
        rownames(raini) <- raini$dataDate
        raini <- raini %>% dplyr::select(-c(ID, dataDate, lat, lon))
        raini2 <- as.data.frame(t(raini))
        rownames(raini2) <- NULL
        raini2 <- cbind(groundi, raini2)
        
        return(raini2)
      }
      
      # Apply the function in parallel using future_map
      data_points <- future_map_dfr(1:nrow(ground), process_groundi,.options = furrr_options(stdout = TRUE))
    
      
      }else{
      
    for(i in 1:nrow(ground)){
      print(i)
      groundi <- ground[i, c("longitude", "latitude", "startingDate", "endDate", "ID", "NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j")]
      yearPi <- as.numeric(groundi$yearPi)
      yearHi <- as.numeric(groundi$yearHi)
      pl_j <- groundi$pl_j
      hv_j <- groundi$hv_j

      # Case planting and harvesting dates span the same year
      if (yearPi == yearHi) {
        rasti<-listRaster[which(grepl(yearPi, listRaster, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      # Case planting and harvesting dates span two different years
      if (yearPi < yearHi) {
        rasti1<-listRaster[which(grepl(yearPi, listRaster, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster[which(grepl(yearHi, listRaster, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
        
      }
      
      ### Extract the information for the i-th row 
      
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      raini <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      raini <- raini[,-1]
      if(varName %in% c("temperatureMax","temperatureMin")){
        raini <- raini-274
      }else if (varName == "solarRadiation"){
        raini <- raini/1000000
      }
      
      start <- as.Date(unique(groundi$startingDate))
      maxDaysDiff <- as.numeric(groundi$endDate - groundi$startingDate)
      end <- start + as.difftime(maxDaysDiff, units="days")
      ddates <- seq(from=start, to=end, by=1)
      names(raini) <- paste(varName, ddates, sep="_")
      raini <- as.data.frame(t(raini))
      raini$dataDate <- rownames(raini)
      rownames(raini) <- NULL
      
      ## merging data for different trials with differing growing period requires having data for the whole period of time
      raini <- merge(raini, rf_names2, by="dataDate", all.y=TRUE)
      raini <- raini[order(raini$ID),]
      rownames(raini) <- raini$dataDate
      raini <- raini %>% dplyr::select(-c(ID,dataDate))
      raini2 <- as.data.frame(t(raini))
      rownames(raini2) <- NULL
      raini2 <- cbind(groundi, raini2)
      data_points <- dplyr::bind_rows(data_points, raini2)
    }
   }
  }
  
  data_points <- data_points %>% 
    dplyr::select_if(~sum(!is.na(.)) > 0)
  
  return(data_points)
}





################################################################################
#' Title Extract daily weather data
#' @description This function extracts weather data based on the input data for GPS and dates for specific country-use Case-crop combination
#' for the trial sites named as "compiled_fieldData.RDS" and for target areas as AOI_GPS.RDS. The input data should have lon, lat, ID, planting and harvest dates
#'
#' @param datacube_path Path with the python source code to download data from datacube
#' @param dataPath Main path where the input data are saved
#' @param country country name to be used for cropping, extracting the top two administrative region names and to define input and output paths
#' @param useCaseName use case name or a project name, and this is used to define input and output paths
#' @param Crop is crop name and is used to define input and output paths
#' @param inputData data frame with the locations to extract the data. Must have the c(lat, lon, plantingDate, harvestDate). For field observations, plantingDate  harvestDate should be given in yyyy-mm-dd format. 
#' @param AOI is TRUE is the input data has defined planting and harvest dates otherwise FALSE
#' @param Planting_month_date planting month and date in mm-dd format and must be provided if AOI is TRUE. It is the earliest possible planting date in the target area. 
#' @param Harvest_month_date harvest month and date in mm-dd format and must be provided if AOI is TRUE 
#' @param plantingWindow is given when several planting dates are to be tested to determine optimal planting date and it should be given in number of weeks starting from Planting_month_date 
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param processModel TRUE if the output data will be used for process based models.When pathOut=NULL, if TRUE the data will be saved in the folder result/geo_4cropModel if FALSE in result/geo_4ML 
#' @param pathOut Path where the outputs will be saved, if NULL the function will save the files in a default folder (result)
#' @param jobs number of cores used to parallel weather data extraction
#'
#' @return List of data frames with daily data for c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed") is returned. 
#' These results are written out in paths defined by country, useCaseName, and crop 
#' If AOI is set TRUE, the weather data between the Planting_month_date and Harvest_month_date and for 2000 - 2022 data will be returned. 

#' @examples inputData <- data.frame(lon=c(29.3679, 29.3941,  29.390), lat=c(-1.539, -1.716, -1.716), 
#' plantingDate  = c("2020-08-27", "2020-09-04", "2020-09-04"),
#' harvestDate = c("2020-12-29", "2020-12-29", "2020-12-29"))
#' extract_geoSpatialWeatherPointData(datacube_path="D:/demo-repository/Script/data_sourcing/generic/data_cube",dataPath = "D:/demo-repository/Data/data_sourcing",country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
#'                                    inputData=inputData, AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, plantingWindow=1, 
#'                                    season = 1,processModel = TRUE, pathOut = NULL, jobs=10)
extract_geoSpatialWeatherPointData <- function(datacube_path,dataPath,country, useCaseName, Crop,  inputData = NULL,
                                        AOI=FALSE,Planting_month_date=NULL, Harvest_month_date=NULL, plantingWindow=1, 
                                       season = 1,processModel = TRUE, pathOut = NULL, jobs=10){
  

  ARD <- Paths_Vars_weather(dataPath=dataPath,country=country, useCaseName=useCaseName, Crop=Crop, inputData = inputData, 
                     Planting_month_date=Planting_month_date, Harvest_month_date=Harvest_month_date,
                     AOI = AOI,  processModel = processModel, varsbasePath=NULL, pathOut = pathOut)
  
  inputData <- ARD[[1]]
  listRasterRF <- ARD[[2]]
  listRasterTmax <- ARD[[3]]
  listRasterTMin <- ARD[[4]]
  listRasterRH <- ARD[[5]]
  listRasterSR <- ARD[[6]]
  listRasterWS <- ARD[[7]]
  pathOut <- ARD[[8]]
  
  if (!dir.exists(pathOut)){dir.create(file.path(pathOut), recursive = TRUE)}
    i=1
    wData <- list()
    for(varName in c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed")){
      
      if(varName == "Rainfall"){
        listRaster <- listRasterRF
      }else if (varName == "temperatureMax"){
        listRaster <- listRasterTmax
      }else if (varName == "temperatureMin"){
        listRaster <- listRasterTMin
      }else if(varName == "relativeHumidity"){
        listRaster <- listRasterRH
      }else if(varName == "solarRadiation"){
        listRaster <- listRasterSR
      }else if(varName == "windSpeed"){
        listRaster <- listRasterWS
      }

      #listRaster <- listRaster[grep("2000", listRaster):grep("2022", listRaster)]
      
      
      vData <- get_weather_pointData(datacube_path,downloadpath=dataPath,inputData = inputData, 
                                     country = country, AOI=AOI, Planting_month_date=Planting_month_date, plantingWindow, 
                                     Harvest_month_date=Harvest_month_date, varName=varName, listRaster= listRaster, jobs=jobs)
      
      w_name <- ifelse(AOI == TRUE, paste(varName, "_Season_", season, "_PointData_AOI.RDS", sep=""), paste(varName, "_PointData_trial.RDS", sep=""))
      saveRDS(vData, paste(pathOut, w_name, sep="/"))
      print(paste("Data sourcing for ", varName, " is done", sep=""))
      # wData[[i]] <-  vData
      rm(vData)
      i=i+1
    }

    
  
  # if(weatherData == TRUE & soilData == TRUE & season == 1){
  #   wData[[7]] <- sData
  return(wData)
  # }else if (weatherData == TRUE & soilData == FALSE){
  #   return(wData)
  # }else if(weatherData == FALSE & soilData == TRUE & season == 1) {
  #   return(sData)
  # }
  
}



# 3. Is a helper function for get_WeatherSummarydata to get seasonal weather parameters for point data over the cropping season  -------------------------------------------
#' @description is a function to get total rainfall, number of rainy days,monthly rainfall and monthly mean Tmax, Tmin, solar radiation and wind speed. 
#' @param rastLayer[var]_1 The .nc file for the planting year for the variable var (RF: rainfall;Tmax: maximum temperature; Tmin: minimum temperature; RH: relative humidity; SR: solar radiation; WS: wind speed)
#' @param rastLayer[var]_2 the .nc file for the harvest year
#' @param gpsdata a data frame with longitude and latitude 
#' @param pl_j the planting date as the date of the year
#' @param hv_j the harvest date as the date of the year
#' @param planting_harvest_sameYear TRUE if the planting and harvesting occurs the same year 
#' @param start_time Planting date in date format
#' @param end_time Harvesting date in date format
#' 
#' @return  a data frame with total rainfall, number of rainy days, monthly rainfall and monthly mean Tmax, Tmin, solar radiation and wind speed
#' @example summary_pointdata_rainfall(rastLayerRF_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1981.nc",
# rastLayerRF_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1982.nc",
# gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)),  
# pl_j=35, hv_j=150, planting_harvest_sameYear = TRUE,start_time=2000-02-04,end_time=2000-05-29)
summarize_pointdata <- function(rastLayerRF_1=NULL, rastLayerRF_2=NULL, 
                                rastLayerTmax_1=NULL, rastLayerTmax_2=NULL,
                                rastLayerTmin_1=NULL, rastLayerTmin_2=NULL,
                                rastLayerRH_1=NULL, rastLayerRH_2=NULL,
                                rastLayerSR_1=NULL, rastLayerSR_2=NULL,
                                rastLayerWS_1=NULL, rastLayerWS_2=NULL,
                                gpsdata, pl_j, hv_j, planting_harvest_sameYear,
                                start_time,end_time){
  
  # 3.1. Read the rainfall data and shape the ground data ####
  if(planting_harvest_sameYear == TRUE){
    PlHvD_RF <- terra::rast(rastLayerRF_1)
    time_values <- time(PlHvD_RF)
    # Find the indices of the layers that fall within the time range
    layer_indices <- which(time_values >= start_time & time_values <= end_time)
    PlHvD_RF<- PlHvD_RF[[layer_indices]]

    PlHvD_Tmax <- terra::rast(rastLayerTmax_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_Tmin <- terra::rast(rastLayerTmin_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_RH <- terra::rast(rastLayerRH_1)
    time_values <- time(PlHvD_RH)
    layer_indices <- which(time_values >= start_time & time_values <= end_time)
    PlHvD_RH<- PlHvD_RH[[layer_indices]]
    PlHvD_SR <- terra::rast(rastLayerSR_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_WS <- terra::rast(rastLayerWS_1)
    time_values <- time(PlHvD_WS)
    layer_indices <- which(time_values >= start_time & time_values <= end_time)
    PlHvD_WS<- PlHvD_WS[[layer_indices]]
    
    
  }else{

    PlHvD_RF <- terra::rast(rastLayerRF_1)
    time_values <- time(PlHvD_RF)
    # Find the indices of the layers that fall within the time range
    layer_indices <- which(time_values >= start_time & time_values <= end_time)
    PlHvD_RF<- PlHvD_RF[[layer_indices]]
    
    rastTmax_i1 <- if(class(terra::rast(rastLayerTmax_1))[1]=='SpatRaster'){terra::rast(rastLayerTmax_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerTmax_1))))}
    rastTmax_i2 <- if(class(terra::rast(rastLayerTmax_2))[1]=='SpatRaster'){terra::rast(rastLayerTmax_2, lyrs=c(1:hv_j))}
    PlHvD_Tmax <- c(rastTmax_i1, rastTmax_i2)

    rastTmin_i1 <- if(class(terra::rast(rastLayerTmin_1))[1]=='SpatRaster'){terra::rast(rastLayerTmin_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerTmin_1))))}
    rastTmin_i2 <- if(class(terra::rast(rastLayerTmin_2))[1]=='SpatRaster'){terra::rast(rastLayerTmin_2, lyrs=c(1:hv_j))}
    PlHvD_Tmin <- c(rastTmin_i1, rastTmin_i2)
    
    PlHvD_RH <- terra::rast(rastLayerRH_1)
    time_values <- time(PlHvD_RH)
    # Find the indices of the layers that fall within the time range
    layer_indices <- which(time_values >= start_time & time_values <= end_time)
    PlHvD_RH<- PlHvD_RH[[layer_indices]]
    
    rastSR_i1 <- if(class(terra::rast(rastLayerSR_1))[1]=='SpatRaster'){terra::rast(rastLayerSR_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerSR_1))))}
    rastSR_i2 <- if(class(terra::rast(rastLayerSR_2))[1]=='SpatRaster'){terra::rast(rastLayerSR_2, lyrs=c(1:hv_j))}
    PlHvD_SR <- c(rastSR_i1, rastSR_i2)
    
    PlHvD_WS <- terra::rast(rastLayerWS_1)
    time_values <- time(PlHvD_WS)
    # Find the indices of the layers that fall within the time range
    layer_indices <- which(time_values >= start_time & time_values <= end_time)
    PlHvD_WS<- PlHvD_WS[[layer_indices]]
    

  }
  
  xy <- gpsdata[, c("longitude", "latitude")]
  
  RFi <- if(class(PlHvD_RF) == "SpatRaster"){terra::extract(PlHvD_RF, xy, method='simple', cells=FALSE)}
  RFi <- RFi[,-1]
  
  Tmaxi <- if(class(PlHvD_Tmax) == "SpatRaster"){terra::extract(PlHvD_Tmax, xy, method='simple', cells=FALSE)}
  Tmaxi <- Tmaxi[,-1]
  Tmaxi <- Tmaxi-274
  
  Tmini <- if(class(PlHvD_Tmin) == "SpatRaster"){terra::extract(PlHvD_Tmin, xy, method='simple', cells=FALSE)}
  Tmini <- Tmini[,-1]
  Tmini <- Tmini-274
  
  RHi <- if(class(PlHvD_RH) == "SpatRaster"){terra::extract(PlHvD_RH, xy, method='simple', cells=FALSE)}
  RHi <- RHi[,-1]
  
  SRi <- if(class(PlHvD_SR) == "SpatRaster"){terra::extract(PlHvD_SR, xy, method='simple', cells=FALSE)}
  SRi <- SRi[,-1]
  SRi <- SRi/1000000
  
  WSi <- if(class(PlHvD_WS) == "SpatRaster"){terra::extract(PlHvD_WS, xy, method='simple', cells=FALSE)}
  WSi <- WSi[,-1]
  
  
  

  
  # 3.2. Get the rainfall seasonal parameters at a location ####
  ## The total rainfall over the growing period
  
  rainiq <- t(RFi)
  gpsdata$totalRF <- colSums(rainiq, na.rm=T)
  
  ## The number of rainy days (thr >= 2 mm) over the growing period 
  gpsdata$nrRainyDays <- NULL
  for (m in 1:nrow(RFi)){
    # print(m)
    mdata <- RFi[m, ]
    mdata[mdata < 2] <- 0
    mdata[mdata >= 2] <- 1
    gpsdata$nrRainyDays[m] <- sum(mdata, na.rm=T)
    
    ## The monthly rainfall, at 31 days interval and the remaining  days at the end, over the growing period
    mrdi <- RFi[m, ]
    mtmaxi <- Tmaxi[m,]
    mtmini <- Tmini[m,]
    mrhi <- RHi[m,]
    msri <- SRi[m,]
    mwsi <- WSi[m,]
    
    mdiv <- unique(c(seq(1, length(mrdi), 30), length(mrdi)))
    
    mdivq <- length(mrdi)%/%31
    mdivr <- length(mrdi)%%31
    
    ##################
    mrf <- NULL
    for (q in 1:mdivq){
      mrf <- c(mrf, sum(mrdi[((q*31)-30):(q*31)],na.rm=T))	
    }
    # Then add the remainder
    mrf <- c(mrf, sum(mrdi[(q*31):((q*31)+mdivr)],na.rm=T))
    
    mtmax <- NULL
    for (q in 1:mdivq){
      mtmax <- c(mtmax, mean(as.numeric(mtmaxi[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mtmax <- c(mtmax, mean(as.numeric(mtmaxi[(q*31):((q*31)+mdivr)])))
    
    mtmin <- NULL
    for (q in 1:mdivq){
      mtmin <- c(mtmin, mean(as.numeric(mtmini[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mtmin <- c(mtmin, mean(as.numeric(mtmini[(q*31):((q*31)+mdivr)])))
    
    mrh <- NULL
    for (q in 1:mdivq){
      mrh <- c(mrh, mean(as.numeric(mrhi[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mrh <- c(mrh, mean(as.numeric(mrhi[(q*31):((q*31)+mdivr)])))
    
    msr <- NULL
    for (q in 1:mdivq){
      msr <- c(msr, mean(as.numeric(msri[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    msr <- c(msr, mean(as.numeric(msri[(q*31):((q*31)+mdivr)])))
    
    mws <- NULL
    for (q in 1:mdivq){
      mws <- c(mws, mean(as.numeric(mwsi[((q*31)-30):(q*31)])))	
    }
    # Then add the remainder
    mws <- c(mws, mean(as.numeric(mwsi[(q*31):((q*31)+mdivr)])))
  
    ###################################
    
    # for (q in 1:length(mdivq)){
    #   mrf <- c(mrf, sum(mdiv[q:31*q]))
    # }
    # 
    # mrf <- c()
    # mtmax <- c()
    # mtmin <- c()
    # mrh <- c()
    # msr <- c()
    # mws <- c()
    
    # for (k in 1:(length(mdiv)-1)) {
    #   # print(k)
    #   if(k == 1){
    #     mrf <- c(mrf, sum(mrdi[c(mdiv[k]:mdiv[k+1])]))
    #     mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c(mdiv[k]:mdiv[k+1])])))
    #     mtmin <- c(mtmin, mean(as.numeric(mtmini[c(mdiv[k]:mdiv[k+1])])))
    #     mrh <- c(mrh, mean(as.numeric(mrhi[c(mdiv[k]:mdiv[k+1])])))
    #     msr <- c(msr, mean(as.numeric(msri[c(mdiv[k]:mdiv[k+1])])))
    #     mws <- c(mws, mean(as.numeric(mwsi[c(mdiv[k]:mdiv[k+1])])))
    #   }else{
    #     mrf <- c(mrf, sum(mrdi[c((mdiv[k]+1):(mdiv[k+1]))]))
    #     mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     mtmin <- c(mtmin, mean(as.numeric(mtmini[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     mrh <- c(mrh, mean(as.numeric(mrhi[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     msr <- c(msr, mean(as.numeric(msri[c((mdiv[k]+1):(mdiv[k+1]))])))
    #     mws <- c(mws, mean(as.numeric(mwsi[c((mdiv[k]+1):(mdiv[k+1]))])))
    #   }
    # }}
    # 
  
    if(length(mrf) > 15){## if the crop is > 15 months on the field ( to account for cassava as well)
      mrf <- c(mrf, rep("NA", 15 -length(mrf)))
      mtmax <- c(mtmax, rep("NA", 15 - length(mtmax)))
      mtmin <- c(mtmin, rep("NA", 15 -length(mtmin)))
      mrh <- c(mrh, rep("NA", 15 -length(mrh)))
      msr <- c(msr, rep("NA", 15 -length(msr)))
      mws <- c(mws, rep("NA", 15 -length(mws)))
    }
    
    mrf_names <- c(paste0("Rain_month", c(1:15)))
    mtmax_names <- c(paste0("Tmax_month", c(1:15)))
    mtmin_names <- c(paste0("Tmin_month", c(1:15)))
    mrh_names <- c(paste0("relativeHumid_month", c(1:15)))
    msr_names <- c(paste0("solarRad_month", c(1:15)))
    mws_names <- c(paste0("windSpeed_month", c(1:15)))
    
    
    for (h in 1:length(mrf_names)) {
      colname <- mrf_names[h]
      gpsdata[[colname]][m] <- mrf[h]
      
      colname <- mtmax_names[h]
      gpsdata[[colname]] <- mtmax[h]
      
      colname <- mtmin_names[h]
      gpsdata[[colname]] <- mtmin[h]
      
      colname <- mrh_names[h]
      gpsdata[[colname]] <- mrh[h]
      
      colname <- msr_names[h]
      gpsdata[[colname]] <- msr[h]
      
      colname <- mws_names[h]
      gpsdata[[colname]] <- mws[h]
    }

  

    gpsdata$plantingYear <- min(lubridate::year(time(PlHvD_RF)))
    gpsdata$harvestYear <- max(lubridate::year(time(PlHvD_RF)))

  
  gpsdata <- gpsdata %>% 
    dplyr::select_if(~sum(!is.na(.)) > 0)
  
  return(gpsdata)
}

}

# 5. Extract the season weather parameters for point based data -------------------------------------------
#' @description this functions loops through all .nc files (~30 -40 years)  to provide point based seasonal weather parameters.
#' @details for AOI it requires a "AOI_GPS.RDS" data frame with c("longitude","latitude") columns 
#'          for trial sites it requires a "compiled_fieldData.RDS" data frame with c("lon", "lat", "plantingDate", "harvestDate")
#'
#' @param dataPath Main path where the input data are saved
#' @param datacube_path Path with the python source code to download data from datacube
#' @param country country name
#' @param useCaseName use case name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param inputData data frame with the locations to extract the data. If NULL a default file in the raw folder inside the dataPath will be used. Must have the c(lat, lon, plantingDate, harvestDate). For field observations, plantingDate  harvestDate should be given in yyyy-mm-dd format.
#' @param varsbasePath Path with all the weather spatial layers, if NULL a default path is assumed
#' @param Planting_month_date is provided as mm-dd, and for AOI, it is the intended planting date defined using expert knowledge, crop models, remote sensing analysis, etc, preferably it should also make use of climate forecast info
#' @param Harvest_month_date is provided as mm-dd, defined in similar way as Planting_month_date
#' @param zone administrative level 2 name 
#' @param season number of the crop growing season  
#' @param pathOut Path where the outputs will be saved, if NULL the function will save the files in a default folder (result)
#' @param jobs defines how many cores to use for parallel data sourcing
#' @param processModel TRUE if the output data will be used for process based models.When pathOut=NULL, if TRUE the data will be saved in the folder result/geo_4cropModel if FALSE in result/geo_4ML 
#' 
#' @return a data frame containing the col information & columns corresponding to the weather parameters#' 
#'        totalRF : Total rainfall between pl_Date and hv_Date (mm)
#'        nrRainyDays : Number of rainy days between pl_Date and hv_Date (days)
#'        di : Average daily rainfall between pl_Date and hv_Date (mm/day)
#'        Rainfall_monthx: total monthly rainfall
#'        Tmax_month_monthx: Mean monthly maximum temperature
#'        Tmin_month: Mean monthly minimum temperature
#'        relativeHumid_month: Mean monthly relative humidity
#'        solarRad_month: Mean monthly solar radiation
#'        windSpeed_month: Mean monthly wind speed 
#' @examples: get_WeatherSummarydata(dataPath = "D:/demo-repository/Data/data_sourcing",datacube_path = "D:/demo-repository/Script/data_sourcing/generic/data_cube",country = "Rwanda",  useCaseName = "RAB",
#'                                  Crop = "Potato", AOI = FALSE, inputData = NULL,varsbasePath=NULL,Planting_month_date = "07-01",  Harvest_month_date = "11-30", zone=NULL,season=1, pathOut = NULL, jobs = 10,processModel =TRUE)
#' 
get_WeatherSummarydata <- function(dataPath,datacube_path, country, useCaseName, Crop, AOI = FALSE, inputData = NULL,varsbasePath=NULL,
                                    Planting_month_date = NULL, Harvest_month_date = NULL, zone=NULL,
                                    season=1, pathOut = NULL, jobs = 10,processModel =TRUE){
  
  ARD <- Paths_Vars_weather(dataPath=dataPath,country=country, useCaseName=useCaseName, Crop=Crop, inputData = inputData, varsbasePath=varsbasePath, 
                    Planting_month_date=Planting_month_date, Harvest_month_date=Harvest_month_date,
                     AOI = AOI,  pathOut = pathOut)
  
 
  
  inputData <- ARD[[1]]
  listRaster_RF <- ARD[[2]]
  listRaster_Tmax <- ARD[[3]]
  listRaster_Tmin <- ARD[[4]]
  listRaster_RH <- ARD[[5]]
  listRaster_SR <- ARD[[6]]
  listRaster_WS <- ARD[[7]]
  pathOut <- ARD[[8]]
  

  # Creation of the output dir
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
 
  # Input point data AOI / Trial
  if(AOI == TRUE){

    
    countryCoord <- inputData
    countryCoord$ID <- c(1:nrow(inputData))
    countryCoord <- unique(countryCoord[, c("lon", "lat", "ID")])
   
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    if(Planting_month < harvest_month){
      planting_harvest_sameYear <- TRUE
      }else{
      planting_harvest_sameYear <- FALSE
    }
    
  # add a place holder for the year to get the julian date  
    if(planting_harvest_sameYear ==TRUE){
      countryCoord$plantingDate <- paste(2000, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2000, Harvest_month_date, sep="-")
    }else{
      countryCoord$plantingDate <- paste(2000, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2001, Harvest_month_date, sep="-")
    }
    countryCoord <- countryCoord[complete.cases(countryCoord),]
    names(countryCoord) <- c("longitude", "latitude", "ID" ,"plantingDate", "harvestDate")
    ground <- countryCoord
  }else{
    countryCoord <- unique(inputData[, c("lon", "lat", "plantingDate", "harvestDate")])
    countryCoord$ID <- c(1:nrow(countryCoord))
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", "ID")
    ground <- countryCoord
  
  }
  
  ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d")
  ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") 
  countryShp <- geodata::gadm(country, level = 2, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  ground$pyear <- as.numeric(format(as.POSIXlt(ground$plantingDate), "%Y"))
  ground <- ground[ground$pyear >= 2000, ]
  
  # Clean the raster files
  # trf_files <- list.files(dirname(listRaster_RF)[1])
  # tmax_files <- list.files(dirname(listRaster_Tmax)[1])
  # tmin_files <- list.files(dirname(listRaster_Tmin)[1])
  # trh_files <- list.files(dirname(listRaster_RH)[1])
  # tsr_files <- list.files(dirname(listRaster_SR)[1])
  # tws_files <- list.files(dirname(listRaster_WS)[1])
  # 
  # tmax <- which(tmax_files %in% trf_files)
  # tmin <- which(tmin_files %in% trf_files)
  # rh <- which(trh_files %in% trf_files)
  # sr <- which(tsr_files %in% trf_files)
  # ws <- which(tws_files %in% trf_files)
  # 
  # length(ws)
  # tmax_files <- tmax_files[tmax]
  # tmin_files<- tmin_files[tmin]
  # trh_files <- trh_files[rh]
  # tsr_files <- tsr_files[sr]
  # tws_files <- tws_files[ws]
  # listRaster_Tmax <- listRaster_Tmax[tmax]
  # listRaster_Tmin <- listRaster_Tmin[tmin]
  # listRaster_RH <- listRaster_RH[rh]
  # listRaster_SR <- listRaster_SR[sr]
  # listRaster_WS <- listRaster_WS[ws]
  

 
  # Compute the seasonal rainfall parameters
  if(AOI == TRUE){
    # Convert planting Date and harvesting in Julian Day 
    pl_j <-as.POSIXlt(unique(ground$Planting))$yday
    hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
    
    start_time <- unique(ground$Planting) 
    end_time <- unique(ground$Harvesting)
    
    if (planting_harvest_sameYear ==  TRUE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      # Loop over all the years 
      rf_result <- foreach(i=1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr','lubridate','tidyverse'),
                           .export = c('summarize_pointdata')) %dopar% {
        start_time <- unique(ground$Planting) 
        end_time <- unique(ground$Harvesting)
        yearStart<-lubridate::year(start_time  %m+% years(i-1))
        start_time <- make_date(yearStart,1,1)+ days(pl_j)
        yearEnd <- lubridate::year(end_time %m+% years(i-1))
        end_time  <- make_date(yearEnd,1,1)+ days(hv_j)
        rastRF_1 <- listRaster_RF[i]
        rastTmax_1 <- listRaster_Tmax[i]
        rastTmin_1 <- listRaster_Tmin[i]
        rastRH_1 <- listRaster_RH[i]
        rastSR_1 <- listRaster_SR[i]
        rastWS_1 <- listRaster_WS[i]
        #source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_V2.R", local = TRUE)
        
        summarize_pointdata(rastLayerRF_1=rastRF_1, rastLayerRF_2 = NULL,
                            rastLayerTmax_1=rastTmax_1, rastLayerTmax_2 = NULL,
                            rastLayerTmin_1=rastTmin_1, rastLayerTmin_2 = NULL,
                            rastLayerRH_1=rastRH_1, rastLayerRH_2 = NULL,
                            rastLayerSR_1=rastSR_1, rastLayerSR_2 = NULL,
                            rastLayerWS_1=rastWS_1, rastLayerWS_2 = NULL,
                            gpsdata = ground, pl_j=pl_j, hv_j=hv_j, 
                            planting_harvest_sameYear = planting_harvest_sameYear,
                            start_time=start_time,end_time=end_time)
      }
      rainfall_points <- do.call(rbind, rf_result)
      
    }
    

    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      #days <- (365 - pl_j) + hv_j
      
      #rasters <- days%/%31
      
      rf_result2 <- foreach(i = 1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr','lubridate','tidyverse'),
                            .export = c('summarize_pointdata')) %dopar% {
        start_time <- unique(ground$Planting) 
        end_time <- unique(ground$Harvesting)
        yearStart<-lubridate::year(start_time  %m+% years(i-1))
        start_time <- make_date(yearStart,1,1)+ days(pl_j)
        yearEnd <- lubridate::year(end_time %m+% years(i-1))
        end_time  <- make_date(yearEnd,1,1)+ days(hv_j)
        #for ( i in 1:(length(listRaster_RF)-1)){
         rastRF_1 <- listRaster_RF[i]
        rastRF_2 <- NULL
        
        rastTmax_1 <- listRaster_Tmax[i]
        rastTmax_2 <- listRaster_Tmax[i+1]
        
        rastTmin_1 <- listRaster_Tmin[i]
        rastTmin_2 <- listRaster_Tmin[i+1]
        
        rastRH_1 <- listRaster_RH[i]
        rastRH_2 <- NULL
        
        rastSR_1 <- listRaster_SR[i]
        rastSR_2 <- listRaster_SR[i+1]
        
        rastWS_1 <- listRaster_WS[i]
        rastWS_2 <- NULL
        
        print(i)
        
        #source(sourceFnc, local=TRUE)
        
       
        summarize_pointdata(rastLayerRF_1=rastRF_1, rastLayerRF_2 = rastRF_2, 
                             rastLayerTmax_1=rastTmax_1, rastLayerTmax_2 = rastTmax_2,
                             rastLayerTmin_1=rastTmin_1, rastLayerTmin_2 = rastTmin_2,
                             rastLayerRH_1=rastRH_1, rastLayerRH_2 = rastRH_2,
                             rastLayerSR_1=rastSR_1, rastLayerSR_2 = rastSR_2,
                             rastLayerWS_1=rastWS_1, rastLayerWS_2 = rastWS_2,
                             gpsdata = ground, pl_j=pl_j, hv_j=hv_j,
                             planting_harvest_sameYear = planting_harvest_sameYear,
                            start_time=start_time,end_time=end_time)

      }   
      rainfall_points <- do.call(rbind, rf_result2)
      
    
    
    stopCluster(cls)}
    # Compute the seasonal rainfall parameters for trial data: having varying planting and harvest dates
   }else{
     
    rainfall_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      groundi <- ground[i,]
      yearPi <- format(as.POSIXlt(groundi$Planting), "%Y")
      yearHi <- format(as.POSIXlt(groundi$Harvesting), "%Y")
      pl_j <-as.POSIXlt(groundi$Planting)$yday
      hv_j <-as.POSIXlt(groundi$Harvesting)$yday
      
      
      # Get the month and day of planting and harvesting
      monthdayPi <- format(as.POSIXlt(groundi$Planting), "%m-%d")
      monthdayHi <- format(as.POSIXlt(groundi$Harvesting), "%m-%d")
      
      downloadpath <- dataPath
      #Get rain data from datacube
      get_dataCube_point(datacube_path, downloadpath=dataPath, country, Planting_month_date = monthdayPi,
                         Harvest_month_date = monthdayHi, weather_variable = 'chirps-precipitation',
                         useCaseName, Crop, numberofyears = 1, lastYear = yearPi, 
                         x = groundi$longitude, y = groundi$latitude)
      
      downloadpathfinal <- paste0(downloadpath, "/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/chirps-precipitation/")
      setwd(downloadpathfinal)
      csv_filename <- paste0("chirps-precipitation_", groundi$longitude, "_", groundi$latitude, "_",
                             groundi$Planting, "_", groundi$Harvesting, ".csv")
      raini <- read_csv(csv_filename, show_col_types = FALSE)
      file.remove(csv_filename)
      
      #Get relative humidity data from datacube
      get_dataCube_point(datacube_path, downloadpath=dataPath, country, Planting_month_date = monthdayPi,
                         Harvest_month_date = monthdayHi, weather_variable = 'agera5-relativehumidity',
                         useCaseName, Crop, numberofyears = 1, lastYear = yearPi, 
                         x = groundi$longitude, y = groundi$latitude)
      
      downloadpathfinal <- paste0(downloadpath, "/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/agera5-relativehumidity/")
      setwd(downloadpathfinal)
      csv_filename <- paste0("agera5-relativehumidity_", groundi$longitude, "_", groundi$latitude, "_",
                             groundi$Planting, "_", groundi$Harvesting, ".csv")
      RH_i <- read_csv(csv_filename, show_col_types = FALSE)
      file.remove(csv_filename)
      
      #Get wind speed data from datacube
      get_dataCube_point(datacube_path, downloadpath=dataPath, country, Planting_month_date = monthdayPi,
                         Harvest_month_date = monthdayHi, weather_variable = 'agera5-windSpeed',
                         useCaseName, Crop, numberofyears = 1, lastYear = yearPi, 
                         x = groundi$longitude, y = groundi$latitude)
      
      downloadpathfinal <- paste0(downloadpath, "/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/agera5-windSpeed/")
      setwd(downloadpathfinal)
      csv_filename <- paste0("agera5-windSpeed_", groundi$longitude, "_", groundi$latitude, "_",
                             groundi$Planting, "_", groundi$Harvesting, ".csv")
      WS_i <- read_csv(csv_filename, show_col_types = FALSE)
      file.remove(csv_filename)
      
      # one layer per trial when pla ting and harvest year are the same, two otherwise
      if (yearPi == yearHi) {
        # rastRF_i <-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        # rastRF_i <- terra::rast(rastRF_i, lyrs=c(pl_j:hv_j))
        
        rastTmax_i <-listRaster_Tmax[which(grepl(yearPi, listRaster_Tmax, fixed=TRUE) == T)]
        rastTmax_i <- terra::rast(rastTmax_i, lyrs=c(pl_j:hv_j))
        
        rastTmin_i <-listRaster_Tmin[which(grepl(yearPi, listRaster_Tmin, fixed=TRUE) == T)]
        rastTmin_i <- terra::rast(rastTmin_i, lyrs=c(pl_j:hv_j))
        
        # rastRH_i <-listRaster_RH[which(grepl(yearPi, listRaster_RH, fixed=TRUE) == T)]
        # rastRH_i <- terra::rast(rastRH_i, lyrs=c(pl_j:hv_j))
        
        rastSR_i <-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        rastSR_i <- terra::rast(rastSR_i, lyrs=c(pl_j:hv_j))
        
        # rastWS_i <-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        # rastWS_i <- terra::rast(rastWS_i, lyrs=c(pl_j:hv_j))
        
      }else{
        # rastRF_i1 <-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
        # rastRF_i1 <- terra::rast(rastRF_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastRF_i1))))
        # rastRF_i2 <-listRaster_RF[which(grepl(yearHi, listRaster_RF, fixed=TRUE) == T)]
        # rastRF_i2 <- terra::rast(rastRF_i2, lyrs=c(1:hv_j))
        # rastRF_i <- c(rastRF_i1, rastRF_i2)
        
        rastTmax_i1 <-listRaster_Tmax[which(grepl(yearPi, listRaster_Tmax, fixed=TRUE) == T)]
        rastTmax_i1 <- terra::rast(rastTmax_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastTmax_i1))))
        rastTmax_i2 <-listRaster_Tmax[which(grepl(yearHi, listRaster_Tmax, fixed=TRUE) == T)]
        rastTmax_i2 <- terra::rast(rastTmax_i2, lyrs=c(1:hv_j))
        rastTmax_i <- c(rastTmax_i1, rastTmax_i2)
        
        rastTmin_i1 <-listRaster_Tmin[which(grepl(yearPi, listRaster_Tmin, fixed=TRUE) == T)]
        rastTmin_i1 <- terra::rast(rastTmin_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastTmin_i1))))
        rastTmin_i2 <-listRaster_Tmin[which(grepl(yearHi, listRaster_Tmin, fixed=TRUE) == T)]
        rastTmin_i2 <- terra::rast(rastTmin_i2, lyrs=c(1:hv_j))
        rastTmin_i <- c(rastTmin_i1, rastTmin_i2)
        
        # rastRH_i1 <-listRaster_RH[which(grepl(yearPi, listRaster_RH, fixed=TRUE) == T)]
        # rastRH_i1 <- terra::rast(rastRH_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastRH_i1))))
        # rastRH_i2 <-listRaster_RH[which(grepl(yearHi, listRaster_RH, fixed=TRUE) == T)]
        # rastRH_i2 <- terra::rast(rastRH_i2, lyrs=c(1:hv_j))
        # rastRH_i <- c(rastRH_i1, rastRH_i2)
        
        rastSR_i1 <-listRaster_SR[which(grepl(yearPi, listRaster_SR, fixed=TRUE) == T)]
        rastSR_i1 <- terra::rast(rastSR_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastSR_i1))))
        rastSR_i2 <-listRaster_SR[which(grepl(yearHi, listRaster_SR, fixed=TRUE) == T)]
        rastSR_i2 <- terra::rast(rastSR_i2, lyrs=c(1:hv_j))
        rastSR_i <- c(rastSR_i1, rastSR_i2)
        
        # rastWS_i1 <-listRaster_WS[which(grepl(yearPi, listRaster_WS, fixed=TRUE) == T)]
        # rastWS_i1 <- terra::rast(rastWS_i1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastWS_i1))))
        # rastWS_i2 <-listRaster_WS[which(grepl(yearHi, listRaster_WS, fixed=TRUE) == T)]
        # rastWS_i2 <- terra::rast(rastWS_i2, lyrs=c(1:hv_j))
        # rastWS_i <- c(rastWS_i1, rastWS_i2)
        
        
      }
      
      ##Extract the information for the i-th row ####
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      rainfall_points_i <- t(raini$precipitation)
      Tmax_points_i <- terra::extract(rastTmax_i, xy,method='simple', cells=FALSE)
      Tmin_points_i <- terra::extract(rastTmin_i, xy,method='simple', cells=FALSE)
      RH_points_i <- t(RH_i$Relative_Humidity_2m_12h)
      SR_points_i <- terra::extract(rastSR_i, xy,method='simple', cells=FALSE)
      WS_points_i <- t(WS_i$Wind_Speed_10m_Mean)
      
      
      
      Tmax_points_i <- Tmax_points_i-274
      Tmin_points_i <- Tmin_points_i-274
      SR_points_i <- SR_points_i/1000000
      
      
      ## get year
      groundi$Year <- yearPi
      
      # Compute the total amount of rainfall
      groundi$totalRF <- sum(rainfall_points_i[c(1:length(rainfall_points_i))])
      
      # Compute the Number of rainy day
      nrdi <- rainfall_points_i[c(1:length(rainfall_points_i))]
      nrdi[nrdi < 2] <- 0
      nrdi[nrdi >= 2] <- 1
      groundi$nrRainyDays <- sum(nrdi)
      
      # Compute monthly total, at 31 days interval and the remaining  days at the end
      mrdi <- rainfall_points_i[c(1:length(rainfall_points_i))]
      mtmaxi <- Tmax_points_i[c(2:length(Tmax_points_i))]
      mtmini <- Tmin_points_i[c(2:length(Tmin_points_i))]
      mrhi <- RH_points_i[c(1:length(RH_points_i))]
      msri <- SR_points_i[c(2:length(SR_points_i))]
      mwsi <- WS_points_i[c(1:length(WS_points_i))]
      
      
      
      mdiv <- unique(c(seq(1, length(mrdi), 30), length(mrdi)))
      length(mtmini)
      
      mdivq <- length(mrdi)%/%31
      mdivr <- length(mrdi)%%31
      
      ##################
      mrf <- NULL
      for (q in 1:mdivq){
        mrf <- c(mrf, sum(mrdi[((q*31)-30):(q*31)]))	
      }
      # Then add the remainder
      mrf <- c(mrf, sum(mrdi[(q*31):((q*31)+mdivr)]))
      
      mtmax <- NULL
      for (q in 1:mdivq){
        mtmax <- c(mtmax, mean(as.numeric(mtmaxi[((q*31)-30):(q*31)])))	
      }
      # Then add the remainder
      mtmax <- c(mtmax, mean(as.numeric(mtmaxi[(q*31):((q*31)+mdivr)])))
      
      mtmin <- NULL
      for (q in 1:mdivq){
        mtmin <- c(mtmin, mean(as.numeric(mtmini[((q*31)-30):(q*31)])))	
      }
      # Then add the remainder
      mtmin <- c(mtmin, mean(as.numeric(mtmini[(q*31):((q*31)+mdivr)])))
      
      mrh <- NULL
      for (q in 1:mdivq){
        mrh <- c(mrh, mean(as.numeric(mrhi[((q*31)-30):(q*31)])))	
      }
      # Then add the remainder
      mrh <- c(mrh, mean(as.numeric(mrhi[(q*31):((q*31)+mdivr)])))
      
      msr <- NULL
      for (q in 1:mdivq){
        msr <- c(msr, mean(as.numeric(msri[((q*31)-30):(q*31)])))	
      }
      # Then add the remainder
      msr <- c(msr, mean(as.numeric(msri[(q*31):((q*31)+mdivr)])))
      
      mws <- NULL
      for (q in 1:mdivq){
        mws <- c(mws, mean(as.numeric(mwsi[((q*31)-30):(q*31)])))	
      }
      # Then add the remainder
      mws <- c(mws, mean(as.numeric(mwsi[(q*31):((q*31)+mdivr)])))
      
      ###########
      
      # mrf <- c()
      # mtmax <- c()
      # mtmin <- c()
      # mrh <- c()
      # msr <- c()
      # mws <- c()
      # for (k in 1:(length(mdiv)-1)) {
      #   print(k)
      #   if(k == 1){
      #     mrf <- c(mrf, sum(mrdi[c(mdiv[k]:mdiv[k+1])]))
      #     mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c(mdiv[k]:mdiv[k+1])])))
      #     mtmin <- c(mtmin, mean(as.numeric(mtmini[c(mdiv[k]:mdiv[k+1])])))
      #     mrh <- c(mrh, mean(as.numeric(mrhi[c(mdiv[k]:mdiv[k+1])])))
      #     msr <- c(msr, mean(as.numeric(msri[c(mdiv[k]:mdiv[k+1])])))
      #     mws <- c(mws, mean(as.numeric(mwsi[c(mdiv[k]:mdiv[k+1])])))
      #   }else{
      #     mrf <- c(mrf, sum(mrdi[c((mdiv[k]+1):(mdiv[k+1]))]))
      #     mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c((mdiv[k]+1):(mdiv[k+1]))])))
      #     mtmin <- c(mtmin, mean(as.numeric(mtmini[c((mdiv[k]+1):(mdiv[k+1]))])))
      #     mrh <- c(mrh, mean(as.numeric(mrhi[c((mdiv[k]+1):(mdiv[k+1]))])))
      #     msr <- c(msr, mean(as.numeric(msri[c((mdiv[k]+1):(mdiv[k+1]))])))
      #     mws <- c(mws, mean(as.numeric(mwsi[c((mdiv[k]+1):(mdiv[k+1]))])))
      #   }
      # }
      
      ## if the crop is > 15 months on the field (to make it work for cassava, hatcan have 15 months growing period)
      if(length(mrf) > 15){
        mrf <- c(mrf, rep("NA", 15 - length(mrf)))
        mtmax <- c(mtmax, rep("NA", 15 - length(mtmax)))
        mtmin <- c(mtmin, rep("NA", 15 -length(mtmin)))
        mrh <- c(mrh, rep("NA", 15 -length(mrh)))
        msr <- c(msr, rep("NA", 15 -length(msr)))
        mws <- c(mws, rep("NA", 15 -length(mws)))
      }
      
      mrf_names <- c(paste0("Rainfall_month", c(1:15)))
      mtmax_names <- c(paste0("Tmax_month", c(1:15)))
      mtmin_names <- c(paste0("Tmin_month", c(1:15)))
      mrh_names <- c(paste0("relativeHumid_month", c(1:15)))
      msr_names <- c(paste0("solarRad_month", c(1:15)))
      mws_names <- c(paste0("windSpeed_month", c(1:15)))
      
      
      for (h in 1:length(mrf_names)) {
        colname <- mrf_names[h]
        groundi[[colname]] <- mrf[h]
        
        colname <- mtmax_names[h]
        groundi[[colname]] <- mtmax[h]
        
        colname <- mtmin_names[h]
        groundi[[colname]] <- mtmin[h]
        
        colname <- mrh_names[h]
        groundi[[colname]] <- mrh[h]
        
        colname <- msr_names[h]
        groundi[[colname]] <- msr[h]
        
        colname <- mws_names[h]
        groundi[[colname]] <- mws[h]
      }
      
      groundi <- subset(groundi, select=-c(plantingDate, harvestDate, Year))
      
      rainfall_points <- bind_rows(rainfall_points, groundi)
    }
  }
  
  
  ## drop NA columns and save the result
  rainfall_points <- rainfall_points %>% 
    dplyr::select_if(~sum(!is.na(.)) > 0)
  
  
  if(AOI == TRUE){
    if(!is.null(zone)){
      fname <- paste("weatherSummaries_Season_", season, zone, "_AOI.RDS",sep="")
    }else{
      fname <- paste("weatherSummaries_Season_", season, "_AOI.RDS",sep="")
    }
    
  }else{
    fname <- "weatherSummaries_trial.RDS"
  }

  saveRDS(object = rainfall_points, file=paste(pathOut, fname, sep="/"))
  
  return(rainfall_points)
}
###############################################################################################
###############################################################################################
###############################################################################################
#' @description Get daily weather data, as ( longitude, latitude, NAME_1, Rainfall, location, date, pl_year)

#' @param dataPath Main path where the input data are saved
#' @param country country name to be used for cropping, extracting the top two administrative region names and to define input and output paths
#' @param useCaseName use case name or a project name, and this is used to define input and output paths
#' @param Crop is crop name and is used to define input and output paths
#' @param inputData data frame with the locations to extract the data. Must have the c(lat, lon, plantingDate, harvestDate). 
#' @param Planting_month_date planting month and date in mm-dd format and must be provided if AOI is TRUE. It is the earliest possible planting date in the target area. 
#' @param Harvest_month_date harvest month and date in mm-dd format and must be provided if AOI is TRUE 
#' @param varname weather variable name to be analyzed ("Rainfall","temperatureMax","temperatureMin","solarRadiation","windSpeed")
#' @param plantingWindow is given when several planting dates are to be tested to determine optimal planting date and it should be given in number of weeks starting from Planting_month_date 
#' @param jobs number of cores used to parallel weather data extraction

#' 
#' @return a data frame containing the daily weather data  
#' @examples: get_weather_seasonality(dataPath = "D:/demo-repository/Data/data_sourcing",country = "Rwanda",  useCaseName = "RAB",Crop = "Potato", inputData = NULL, 
#' Planting_month_date = "07-01",  Harvest_month_date = "11-30",varName="Rainfall", plantingWindow=1,jobs=10)



get_weather_seasonality <- function(dataPath,country, useCaseName, Crop , inputData = NULL, Planting_month_date=NULL, Harvest_month_date=NULL, varName, plantingWindow=1, jobs){
  
  ARD <- Paths_Vars_weather(dataPath=dataPath,country=country, useCaseName=useCaseName, Crop=Crop, inputData = inputData, varsbasePath=NULL, 
                            Planting_month_date=Planting_month_date, Harvest_month_date=Harvest_month_date,
                            AOI = TRUE,  pathOut = NULL,processModel=FALSE)
  
  
  
  inputData <- ARD[[1]]
  listRaster_RF <- ARD[[2]]
  listRaster_Tmax <- ARD[[3]]
  listRaster_Tmin <- ARD[[4]]
  listRaster_RH <- ARD[[5]]
  listRaster_SR <- ARD[[6]]
  listRaster_WS <- ARD[[7]]
  pathOut <- ARD[[8]]

  
  
  
  pathOut1 <- paste(dataPath,"/useCase_", country, "_", useCaseName,"/", Crop, "/result/", sep="")
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if(is.null(Planting_month_date) | is.null(Harvest_month_date)){
      print("with AOI=TRUE, Planting_month_date, Harvest_month_date can not be null, please refer to the documentation and provide mm-dd for both parameters")
      return(NULL)
    }
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    Harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    
    ## py and hy are used only as place holder for formatting purposes
    if(Planting_month < Harvest_month){
      planting_harvest_sameYear <- TRUE
      py <- 2000
      hy <- 2000
    }else{
      planting_harvest_sameYear <- FALSE
      py <- 2000
      hy <- 2001
    }
    
    ## set planting date one moth prior to the given Planting_month_date so that initial condition for the crop model could be set correctly
    Planting_month_date <-as.Date(paste0(py, "-",Planting_month_date)) ## the year is only a place holder to set planting month 1 month earlier
    Planting_month_date <- Planting_month_date %m-% months(1)
    
    ## if multiple planting dates are to be tested, adjust the Harvest_month_date to extract weather data for the later planting dates.  
    Harvest_month_date <- as.Date(paste0(hy, "-",Harvest_month_date)) ## the year is only a place holder to set planting month 1 month earlier
    if(plantingWindow > 1 & plantingWindow <= 5){
      Harvest_month_date <- Harvest_month_date %m+% months(1)
    }else if(plantingWindow > 5 & plantingWindow <=8){
      Harvest_month_date <- Harvest_month_date %m+% months(2)
    }else if(plantingWindow > 8 & plantingWindow <=12){
      Harvest_month_date <- Harvest_month_date %m+% months(3)
    }
  
    if(lubridate::year(Planting_month_date) < lubridate::year(Harvest_month_date)){
      planting_harvest_sameYear <- FALSE
    }
  
  ## 1. read all the raster files 
    if(varName == "Rainfall"){
      listRaster <- listRaster_RF
    }else if (varName == "temperatureMax"){
      listRaster <- listRaster_Tmax
    }else if (varName == "temperatureMin"){
      listRaster <- listRaster_TMin
    }else if(varName == "relativeHumidity"){
      listRaster <- listRaster_RH
    }else if(varName == "solarRadiation"){
      listRaster <- listRaster_SR
    }else if(varName == "windSpeed"){
      listRaster <- listRaster_WS
    }
  
  
  # if(varName == "Rainfall"){
  #   listRaster <- listRaster[10:42]
  # }else {
  #   listRaster <- listRaster[12:44]
  # }
  
  
  ## 2. format the input data with GPS, dates and ID and add administrative unit info
    countryCoord <- unique(inputData[, c("lon", "lat")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    ## After checking if planting and harvest happens in the same year, get the date of the year 
    countryCoord$startingDate <- Planting_month_date
    countryCoord$endDate <- Harvest_month_date
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "startingDate", "endDate")
    countryCoord$ID <- c(1:nrow(countryCoord))
    ground <- countryCoord[, c("longitude", "latitude", "startingDate", "endDate", "ID")]
    
  
  
  # ground$harvestDate <- as.Date(ground$harvestDate, "%Y-%m-%d")
  countryShp <- geodata::gadm(country, level = 2, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  ## 3.get the seasonal rainfall parameters for AOI
  if(varName %in% c("Rainfall", "relativeHumidity","windSpeed")) {
    
    cls <- makeCluster(jobs)
    doParallel::registerDoParallel(cls)
    
    rf_result <- foreach(i=1:length(listRaster), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
      rasti <- listRaster[i]
      PlHvD <- terra::rast(rasti)
      pl_j <-as.POSIXlt(unique(ground$startingDate))$yday
      hv_j <-as.POSIXlt(unique(ground$endDate))$yday
      xy <- ground[, c("longitude", "latitude")]
      raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
      raini <- raini[,-1]
      if(varName %in% c("temperatureMax","temperatureMin")){
        raini <- raini-274
      }else if (varName == "solarRadiation"){
        raini <- raini/1000000
      }
      ground_adj <- ground
      names(raini) <- paste(varName, time(PlHvD), sep="_")
      lubridate::year(ground_adj$startingDate) <- min(lubridate::year(time(PlHvD)))
      lubridate::year(ground_adj$endDate) <- max(lubridate::year(time(PlHvD)))
      start <- as.Date(unique(ground_adj$startingDate))
      maxDaysDiff <- as.numeric(max(ground_adj$endDate) - min(ground_adj$startingDate))
      end <- start + as.difftime(maxDaysDiff, units="days")
      ddates <- seq(from=start, to=end, by=1)
      ground_adj$startingDate <- as.character(ground_adj$startingDate)
      ground_adj$endDate <- as.character(ground_adj$endDate)
      ground2 <- cbind(ground_adj, raini)
      ground3 <- gather(ground2, year, varName, names(raini)[1]:names(raini)[length(raini)])
      ground3$location <- paste(ground3$longitude, ground3$latitude, sep="_")
      ground3 <- ground3 %>%
        dplyr::group_by(location ) %>%
        dplyr::mutate(date = c(ddates)) %>%
        as.data.frame()
      ground3$pl_year <- as.numeric(min(lubridate::year(time(PlHvD))))
      names(ground3)[names(ground3) == "varName"] <- varName
      ground3 <- subset(ground3, select=-c(startingDate, endDate, ID, year, NAME_2))
    }
    
    data_points <- dplyr::bind_rows(rf_result)
    stopCluster(cls)
  }else{
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      rf_result <- foreach(i=1:length(listRaster), .packages = c('terra', 'plyr', 'stringr','tidyr','tidyverse')) %dopar% {
        rasti <- listRaster[i]
        pl_j <-as.POSIXlt(unique(ground$startingDate))$yday
        hv_j <-as.POSIXlt(unique(ground$endDate))$yday
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        if(varName %in% c("temperatureMax","temperatureMin")){
          raini <- raini-274
        }else if (varName == "solarRadiation"){
          raini <- raini/1000000
        }
        ground_adj <- ground
        lubridate::year(ground_adj$startingDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
        lubridate::year(ground_adj$endDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
        start <- as.Date(unique(ground_adj$startingDate))
        maxDaysDiff <- abs(max(min(pl_j) - max(hv_j)))
        end <- start + as.difftime(maxDaysDiff, units="days")
        ddates <- seq(from=start, to=end, by=1)
        ground_adj$startingDate <- as.character(ground_adj$startingDate)
        ground_adj$endDate <- as.character(ground_adj$endDate)
        ground2 <- cbind(ground_adj, raini)
        ground3 <- gather(ground2, year, varName, names(raini)[1]:names(raini)[length(raini)])
        ground3$location <- paste(ground3$longitude, ground3$latitude, sep="_")
        ground3 <- ground3 %>%
          dplyr::group_by(location ) %>%
          dplyr::mutate(date = c(ddates)) %>%
          as.data.frame()
        ground3$pl_year <- as.numeric(str_extract(rast1, "[[:digit:]]+"))
        names(ground3)[names(ground3) == "varName"] <- varName
        ground3 <- subset(ground3, select=-c(startingDate, endDate, ID, year, NAME_2))
      }
      
      data_points <- dplyr::bind_rows(rf_result)
      stopCluster(cls)
    }else{
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      ## Rainfall
      rf_result2 <- foreach(i = 1:(length(listRaster)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster <- listRaster[order(listRaster)]
        rast1 <- listRaster[i]
        rast2 <- listRaster[i+1]
        ground_adj <- ground
        lubridate::year(ground_adj$startingDate) <- as.numeric(str_extract(rast1, "[[:digit:]]+"))
        lubridate::year(ground_adj$endDate) <- as.numeric(str_extract(rast2, "[[:digit:]]+"))
        start <- as.Date(unique(ground_adj$startingDate))
        maxDaysDiff <- as.numeric(max(ground_adj$endDate) - min(ground_adj$startingDate))
        end <- start + as.difftime(maxDaysDiff, units="days")
        ddates <- seq(from=start, to=end, by=1)
        # Convert planting Date and harvesting in Julian Day 
        pl_j <-as.POSIXlt(unique(ground_adj$startingDate))$yday
        hv_j <-as.POSIXlt(unique(ground_adj$endDate))$yday
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        if(varName %in% c("temperatureMax","temperatureMin")){
          raini <- raini-274
        }else if (varName == "solarRadiation"){
          raini <- raini/1000000
        }
        
        
        ground2 <- cbind(ground_adj, raini)
        ground3 <- gather(ground2, year, varName, names(raini)[1]:names(raini)[length(raini)])
        ground3$location <- paste(ground3$longitude, ground3$latitude, sep="_")
        ground3 <- ground3 %>%
          dplyr::group_by(location ) %>%
          dplyr::mutate(date = c(ddates)) %>%
          as.data.frame()
        ground3$pl_year <- as.numeric(str_extract(rast1, "[[:digit:]]+"))
        names(ground3)[names(ground3) == "varName"] <- varName
        ground3 <- subset(ground3, select=-c(startingDate, endDate, ID, year, NAME_2))
      }
      data_points <- dplyr::bind_rows(rf_result2)
      stopCluster(cls)
      
    } 
  }
  
  
  
  data_points <- data_points %>% 
    dplyr::select_if(~sum(!is.na(.)) > 0)
  
  return(data_points)
}








