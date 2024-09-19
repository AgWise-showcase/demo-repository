source("D:/demo-repository/Script/data_sourcing/generic/get_geoSpatialData_weather.R")

#### geoSpatialWeather AOI =TRUE ####
datacube_path <- "D:/demo-repository/Script/data_sourcing/generic/data_cube"
country <- "Rwanda"
Planting_month_date <- "12-11"
Harvest_month_date <- "05-30"
plantingWindow <- 8
useCaseName<- "RAB"
Crop <- "Maize"
AOI <- TRUE
season = 1
dataPath <- "D:/demo-repository/Data/data_sourcing"
inputData = NULL
pathOut = NULL
jobs=5
processModel = TRUE

extract_geoSpatialWeatherPointData(datacube_path=datacube_path,dataPath=dataPath,country=country, useCaseName=useCaseName, 
                                   Crop=Crop,inputData=inputData, AOI=AOI,Planting_month_date=Planting_month_date, 
                                   Harvest_month_date=Harvest_month_date, plantingWindow=plantingWindow, season=season, 
                                   processModel=processModel, pathOut=pathOut, jobs=jobs)


#### geoSpatialWeather AOI =FALSE ####

datacube_path <- "D:/demo-repository/Script/data_sourcing/generic/data_cube"
country <- "Rwanda"
Planting_month_date <- NULL
Harvest_month_date <- NULL
plantingWindow <- NULL
useCaseName<- "RAB"
Crop <- "Maize"
AOI <- FALSE
season = NULL
dataPath <- "D:/demo-repository/Data/data_sourcing"
inputData = NULL
pathOut = NULL
jobs=5
processModel = FALSE

extract_geoSpatialWeatherPointData(datacube_path=datacube_path,dataPath=dataPath,country=country, useCaseName=useCaseName, 
                                   Crop=Crop,inputData=inputData, AOI=AOI,Planting_month_date=Planting_month_date, 
                                   Harvest_month_date=Harvest_month_date, plantingWindow=plantingWindow, season=season, 
                                   processModel=processModel, pathOut=pathOut, jobs=jobs)

#### WeatherSummary AOI =TRUE ####

dataPath <- "D:/demo-repository/Data/data_sourcing"
datacube_path <- "D:/demo-repository/Script/data_sourcing/generic/data_cube"
country <- "Rwanda"
useCaseName<- "RAB"
Crop <- "Maize"
AOI <- TRUE
inputData = NULL
varsbasePath=NULL
Planting_month_date <- "12-11"
Harvest_month_date <- "05-30"
zone=NULL
season = 1
pathOut = NULL
jobs=5
processModel =FALSE

get_WeatherSummarydata(dataPath=dataPath,datacube_path=datacube_path, country=country, useCaseName=useCaseName, 
                       Crop=Crop, AOI=AOI, inputData = inputData,varsbasePath=varsbasePath,
                      Planting_month_date = Planting_month_date, Harvest_month_date = Harvest_month_date, zone=zone,
                                   season=season, pathOut = pathOut, jobs = jobs,processModel = processModel)


#### WeatherSummary AOI =FALSE ####

dataPath <- "D:/demo-repository/Data/data_sourcing"
datacube_path <- "D:/demo-repository/Script/data_sourcing/generic/data_cube"
country <- "Rwanda"
useCaseName<- "RAB"
Crop <- "Maize"
AOI <- FALSE
inputData = NULL
varsbasePath=NULL
Planting_month_date <- NULL
Harvest_month_date <- NULL
zone=NULL
season = 1
pathOut = NULL
jobs=5
processModel =FALSE

get_WeatherSummarydata(dataPath=dataPath,datacube_path=datacube_path, country=country, useCaseName=useCaseName, 
                       Crop=Crop, AOI=AOI, inputData = inputData,varsbasePath=varsbasePath,
                       Planting_month_date = Planting_month_date, Harvest_month_date = Harvest_month_date, zone=zone,
                       season=season, pathOut = pathOut, jobs = jobs,processModel = processModel)

#### Weather seasonality by default is for AOI =TRUE ####

dataPath <- "D:/demo-repository/Data/data_sourcing"
country <- "Rwanda"
useCaseName<- "RAB"
Crop <- "Maize"
Planting_month_date <- "12-11"
Harvest_month_date <- "05-30"
varName <- "Rainfall" #"temperatureMax","temperatureMin","relativeHumidity","solarRadiation", "windSpeed"
plantingWindow <- 8
jobs <- 5
processModel = FALSE
inputData = NULL

weather_daily <- get_weather_seasonality(dataPath=dataPath,country=country, useCaseName=useCaseName, 
                                         Crop=Crop,inputData = inputData, Planting_month_date=Planting_month_date, Harvest_month_date=Harvest_month_date, 
                                         varName=varName, plantingWindow=plantingWindow, jobs=jobs)
  

