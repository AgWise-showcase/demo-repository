# Get weather data from datacube

# Introduction: 
# This script let us obtain spatial layers of weather data from datacube
# Authors : P.Moreno, A. Sila 
# Credentials : EiA, 2024
# Last modified August 29, 2024 


#################################################################################################################
## sourcing required packages 
#################################################################################################################

packages_required <- c("sf", "rnaturalearth", "rnaturalearthdata","tidyverse")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


#' Function that defines the latitude and longitude limits of a country
#'
#' @param country_name country name
#' @return xmin, xmax,ymin and ymax of a country
#' @export
#'
#' @examples stbbox("Rwanda")
stbbox <- function(country_name = country_name){
  # Retrieve country data
  country_name <- country_name
  country <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(name == country_name)
  
  # Check if country data is correctly loaded
  # str(country$admin)
  
  #plot(country)
  # Create bounding box
  
  bbox <- st_bbox(country)
  
  # Print bounding box
  print(bbox)
  return(bbox)
}

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
#' @export
#'
#' @examples stbbox("Rwanda")
get_dataCube <-function(main_downloadpath, datacube_path, country,Planting_month_date,Harvest_month_date,plantingWindow,weather_variable,useCaseName,Crop,numberofyears, lastYear, bbox=NULL){
  # Get bounding-box of the country of interest
  if (is.null(bbox)) {
    bbox <- stbbox(country)
  }
  

  downloadpath <- paste0(main_downloadpath,"/useCase_", country, "_" ,useCaseName,"/",Crop,"/Landing/",weather_variable,"/")
  
  if (!dir.exists(downloadpath)) {
    dir.create(downloadpath)
  }
  
  
  ## check if both planting and harvest dates are in the same year
  Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
  planting_day <- as.numeric(str_extract(Planting_month_date, "(?<=-)[^-]+"))
  Harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
  harvest_day <- as.numeric(str_extract(Harvest_month_date, "(?<=-)[^-]+"))
  Planting_month <- Planting_month -1
  

  ## if multiple planting dates are to be tested, adjust the Harvest_month_date to extract weather data for the later planting dates.  
  if(plantingWindow > 1 & plantingWindow < 5){
    Harvest_month <- Harvest_month + 1
  }else if(plantingWindow >= 5 & plantingWindow <=8){
    Harvest_month <- Harvest_month + 2
  }
  if (Harvest_month > 12){Harvest_month <- Harvest_month -12}
  if (Planting_month < 1){Planting_month <- Planting_month +12}
  
  startDate <- paste0(Planting_month,"-",planting_day)
  endDate <- paste0(Harvest_month,"-",harvest_day)

  
  
  setwd(datacube_path)

  getDataCommand <- paste0("python getDataFromCubeZone.py --startDate=",startDate," --endDate=",endDate,
                           " --xmin=",as.numeric(bbox$xmin)," --xmax=",as.numeric(bbox$xmax)," --ymin=",as.numeric(bbox$ymin)," --ymax=",as.numeric(bbox$ymax),
                           "  --variable=",weather_variable," --downloadpath=",downloadpath," --lastYear ",lastYear," --numberofyears=",numberofyears)
  system(getDataCommand, intern = TRUE)
  


}






