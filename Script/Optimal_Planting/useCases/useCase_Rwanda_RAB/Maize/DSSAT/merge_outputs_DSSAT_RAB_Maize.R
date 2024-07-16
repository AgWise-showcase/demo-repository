source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/merge_DSSAT_output.R")



pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
season <- 1
varietyids <- "890011"
zone_folder = TRUE
level2_folder = FALSE


merge_DSSAT_output(pathOutput=pathOutput,country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI, season = season, varietyids=varietyids, zone_folder = zone_folder, level2_folder = level2_folder)
