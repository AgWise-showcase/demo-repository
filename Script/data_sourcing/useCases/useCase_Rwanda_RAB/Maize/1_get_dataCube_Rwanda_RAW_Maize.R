#' Function that extract spatial weather data from datacube using a python script
#'
#' @param main_downloadpath Main path where the weather data will be stored
#' @param datacube_path Path with the script in python getDataFromCubeZone.py
#' @param country Country name for which the data will be extracted (if bbox=NULL)
#' @param Planting_month_date mm-dd of crop planting
#' @param Harvest_month_date mm-dd of first crop harvesting
#' @param plantingWindow Number of weeks that the planting can be delayed
#' @param weather_variable Name of the weather variable to be extracted
#' @param useCaseName Name of the use case/research project
#' @param Crop Name of the crop
#' @param numberofyears Number of years that will be extracted
#' @param lastYear Last year of the weather data
#' @param bbox dataframe with xmin, xmax, ymin and ymax to define the area of data extraction
#'        if NULL the limits of the country are used
#' @return spatial weather data from datacube 



main_downloadpath <- "D:/demo-repository/Data/data_sourcing"
datacube_path <- "D:/demo-repository/Script/data_sourcing/generic/data_cube"
country <- "Rwanda"
Planting_month_date <- "12-11"
Harvest_month_date <- "05-30"
plantingWindow <- 8
weather_variables <-c('chirps-precipitation','agera5-relativehumidity','agera5-windSpeed') 
useCaseName<- "RAB"
Crop <- "Maize"
numberofyears <- 23
lastYear <- 2022
bbox=NULL

source("D:/demo-repository/Script/data_sourcing/generic/get_dataCube.R")
for(i in 1: length(weather_variables)){
get_dataCube(main_downloadpath, datacube_path, country,Planting_month_date,
             Harvest_month_date,plantingWindow,weather_variable=weather_variables[i],useCaseName,
             Crop,numberofyears, lastYear, bbox=NULL)
}
  