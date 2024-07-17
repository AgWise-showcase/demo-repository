library(geodata)

# Reading the weather and soil data for crop model and transforming it to DSSAT format
#'
#' @param pathInput Path with all the weather and soil input data in R (RDS) format (e.g. D:/datasourcing/Data/useCase_Rwanda_RAB/Potato/result/geo_4cropModel)
#' @param pathOutput Main path where the weather and soil data in DSSAT format will be created
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param zone administrative level 1 for which the weather and soil data will be created and how the input data are organized
#' @param level2
#' @param varietyid ID of  the variety in DSSAT format which the data are being created for
#' @param pathIn_zone TRUE if the input data (in geo_4cropModel) are organized by zone or province and false if it is just one file 

#' @return weather and soil data in DSSAT format

#You need to modify the path that contains the source code in the generic folder (/Script/Optimal_Planting/generic)
source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/readGeo_CM_zone.R")

#The following parameters of the function can be modified based on your use case
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
season=1
level2 <- NA
varietyid <- "890011"
pathIn_zone = TRUE

#Change the path with the weather and soil data in R format (RDS, e.g.: Rainfall_Season_1_PointData_AOI.RDS) (pathInput) 
#Similarly modify the path where you want to save your outputs in DSSAT format (pathOutput) the outputs will be stored in  /useCase_country_useCaseName/Crop/transform/
pathInput <- paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel", sep="")
pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"


#Identify the regions (zones, NAME_1) that you want to obtain the weather and soil data 
countryShp <- geodata::gadm(country, level = 1, path='.')
zones <- unique(countryShp$NAME_1)

for (i in 1:length(zones)){
readGeo_CM_zone(pathInput= pathInput,pathOutput = pathOutput, country = country, useCaseName = useCaseName, Crop = Crop, AOI = AOI, 
                season= season, zone =zones[i],level2=level2,varietyid =varietyid,pathIn_zone = pathIn_zone)
}

