#' Main function that simulate the DSSAT model
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param TRT is the number of treatments to be run from the experimental file
#' @param varietyid identification or variety ID in the cultivar file of DSSAT
#' @param zone Name of the administrative level 1 for the specific location the experimental file is created.
#' @param level2 Name of the administrative level 2 (has to be part of the administrative level 1 or "zone" of the country) 
#'        for the specific location the experimental file is created
#'        
#'        

#You need to modify the path that contains the source code in the generic folder (/Script/Optimal_Planting/generic)
source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/3_dssat_exec.R")


#Also, change the path where you want to save your outputs (pathOutput) which should be the same that in the example code 1_get_Weather_Soil_data_RAB_Maize.R
pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"

#The following parameters of the function can be modified based on your use case
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
TRT <-1:2
varietyids <- c("890011","890012","890013")
level2 <- NA
#Specify the path where the executable/binary file of DSSAT is located (you neeed to install DSSAT before running the simulations)
path_DSSAT <-"C:/DSSAT48/DSCSM048.EXE"


zones <- list.files(paste(pathOutput,"/useCase_", country, "_", useCaseName,"/", Crop, '/transform/DSSAT/AOI/',varietyids[1], sep=""))
zones <- zones[file.info(paste(pathOutput, "/useCase_", country, "_", useCaseName, "/", Crop, '/transform/DSSAT/AOI/', varietyids[1], "/", zones, sep=""))$isdir]

# Exclude the "gadm" folder in case the shapefiles of the country has been downloaded it 
zones <- zones[zones != "gadm"]

for (i in 1:length(varietyids)){
  for (j in 1:length(zones)){
    execmodel_AOI <-dssat.exec(pathOutput=pathOutput,country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI,
                               TRT=TRT,varietyid=varietyids[i], zone=zones[j], level2=level2,path_DSSAT=path_DSSAT)
  }
}
