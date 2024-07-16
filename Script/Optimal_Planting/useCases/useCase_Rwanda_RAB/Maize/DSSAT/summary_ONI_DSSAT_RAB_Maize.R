
source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/dssat_summary_ONI.R")


pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"
path_coord <-  paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw", sep="")
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"  
AOI = TRUE
season <- 1
Plot=TRUE
short_variety <- "890011"
medium_variety <- NA
long_variety <- NA


get_ONI(pathOutput=pathOutput,path_coord=path_coord,country=country, useCaseName=useCaseName, Crop=Crop, AOI=AOI, 
        season =season, Plot=Plot, short_variety=short_variety, medium_variety =medium_variety, long_variety =long_variety)
