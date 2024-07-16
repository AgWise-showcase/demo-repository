library(geodata)
source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/readGeo_CM_zone.R")

country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
season=1
level2 <- NA
varietyid <- "890011"
pathIn_zone = TRUE
pathInput <- paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel", sep="")
pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"



countryShp <- geodata::gadm(country, level = 1, path='.')
zones <- unique(countryShp$NAME_1)

for (i in 1:length(zones)){
readGeo_CM_zone(pathInput= pathInput,pathOutput = pathOutput, country = country, useCaseName = useCaseName, Crop = Crop, AOI = AOI, 
                season= season, zone =zones[i],level2=level2,varietyid =varietyid,pathIn_zone = pathIn_zone)
}

