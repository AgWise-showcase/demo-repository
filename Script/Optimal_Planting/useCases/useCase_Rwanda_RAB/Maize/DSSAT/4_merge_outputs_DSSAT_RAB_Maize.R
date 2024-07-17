# Merge outputs of the DSSAT simulations
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param season when data is needed for more than one season, this needs to be provided to be used in the file name
#' @param varietyids ids of the varieties based on the cultivar file of DSSAT (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
#' @param zone_folder When TRUE the output folders are organized by administrative level 1.
#' @param level2_foler When TRUE the output folders are organized by administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created


#You need to modify the path that contains the source code in the generic folder (/Script/Optimal_Planting/generic)
source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/merge_DSSAT_output.R")


#Also, change the path where you want to save your outputs (pathOutput) which should be the same that in the example code 1_get_Weather_Soil_data_RAB_Maize.R
pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"

#The following parameters of the function can be modified based on your use case
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
season <- 1
varietyids <- "890011"
zone_folder = TRUE
level2_folder = FALSE


merge_DSSAT_output(pathOutput=pathOutput,country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI, season = season, varietyids=varietyids, zone_folder = zone_folder, level2_folder = level2_folder)
