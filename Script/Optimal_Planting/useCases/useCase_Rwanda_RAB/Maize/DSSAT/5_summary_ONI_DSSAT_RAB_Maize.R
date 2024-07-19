#  ONI classification of aggregated DSSAT output
#' @description Function that will classify each DSSAT simulation based on ONI index
#''@param pathOutput Main path where the merged DSSAT outputs are
#''@param path_coord Path that contains the coordinates of the points simulated
#' @param country country name
#' @param useCaseName use case name
#' @param Crop targeted crop
#' @param AOI TRUE if the data is required for target area, and FALSE if it is for trial sites, default =TRUE
#' @param season integer, cropping season concerned, default = 1 
#' @param Plot, provide somes plots of the output, default = TRUE
#' @param short_variety variety ID with short growing period duration
#' @param medium_variety variety ID with medium growing period duration 
#' @param long_variety variety ID with long growing period duration

#You need to modify the path that contains the source code in the generic folder (/Script/Optimal_Planting/generic)
source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/5_dssat_summary_ONI.R")


#Change the path where you want to save your outputs (pathOutput) which should be the same that in the example code 1_get_Weather_Soil_data_RAB_Maize.R
#Modify the path with the coordinates (path_coord) that are used to obtain the weather and soil data in RDS format (stored in pathInput)
pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"
path_coord <-  paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw", sep="")

#The following parameters of the function can be modified based on your use case
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"  
AOI = TRUE
season <- 1
Plot=TRUE
short_variety <- "890011"
medium_variety <- "890012"
long_variety <- "890013"


get_ONI(pathOutput=pathOutput,path_coord=path_coord,country=country, useCaseName=useCaseName, Crop=Crop, AOI=AOI, 
        season =season, Plot=Plot, short_variety=short_variety, medium_variety =medium_variety, long_variety =long_variety)
