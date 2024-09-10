#' Create multiple experimental files
#'
#' @param country country name
#' @param useCaseName use case name 
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI "Area of interest" AOI=TRUE when we want to explore crop simulations with historical data.
#'        AOI= FALSE when there is information for actual trial sites (with observed yield data).
#' @param filex_temp Name of the template experimental file in DSSAT format (FILEX)
#' @param Planting_month_date it is needed only when AOI=TRUE and it should be provided as mm-dd format 
#' @param Harvest_month_date if AOI =TRUE, Harvest_month_date is the initial month for harvesting and it should be provided in mm-dd format.
#'        The parameter is no needed it when AOI=FALSE because the actual harvesting date from the trials would be provided.   
#' @param ID trial ID
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param plantingWindow number of weeks starting considering the Planting_month_date as earliest planting week. It is given when several 
#'        planting dates are to be tested to determine optimal planting date
#' @param varietyid ID of the variety based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone Name of administrative level 1 for the specific location the experimental file is created
#' @param level2 Name of administrative level 2 (part of the administrative level 1 or "zone") for the specific location the experimental file is created
#' @param fertilizer if TRUE the parameter modifies the fertilizer date to be at planting






#You need to modify the path that contains the source code in the generic folder (/Script/Optimal_Planting/generic)
source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/2_dssat_expfile_zone.R")

#The following parameters of the function can be modified based on your use case
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
season=1
level2 <- NA
varietyids <- c("890011","890012","890013")
pathIn_zone = TRUE
filex_temp <- "KEAG8104.MZX"
Planting_month_date="03-01"
Harvest_month_date="11-30"
ID="TLID"
plantingWindow=1
fertilizer=F
geneticfiles = "MZCER048"
index_soilwat=1

#Modify the path with the coordinates (path_coord) that are used to obtain the weather and soil data in RDS format (stored in pathInput)
#Also, change the path where you want to save your outputs (pathOutput) which should be the same that in the example code 1_get_Weather_Soil_data_RAB_Maize.R
path_coord <- paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw", sep="")
pathInput <- paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel", sep="")
pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"


zones <- list.files(paste(pathOutput,"/useCase_", country, "_", useCaseName,"/", Crop, '/transform/DSSAT/AOI/',varietyids[1], sep=""))
zones <- zones[file.info(paste(pathOutput, "/useCase_", country, "_", useCaseName, "/", Crop, '/transform/DSSAT/AOI/', varietyids[1], "/", zones, sep=""))$isdir]

# Exclude the "gadm" folder in case the shapefiles of the country has been downloaded it 
zones <- zones[zones != "gadm"]

for (i in 1:length(varietyids)){ 
  for (j in 1:length(zones)){
    expdata_AOI <- dssat.expfile(path_coord=path_coord,pathInput=pathInput, pathOutput=pathOutput,
                                 country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI,
                                 filex_temp=filex_temp, Planting_month_date=Planting_month_date,Harvest_month_date=Harvest_month_date, 
                                 ID=ID,season =season, plantingWindow=plantingWindow,varietyid=varietyids[i], zone =zones[j], level2=level2, 
                                 fertilizer=fertilizer,geneticfiles=geneticfiles,index_soilwat=index_soilwat,pathIn_zone =pathIn_zone)
  }
}
