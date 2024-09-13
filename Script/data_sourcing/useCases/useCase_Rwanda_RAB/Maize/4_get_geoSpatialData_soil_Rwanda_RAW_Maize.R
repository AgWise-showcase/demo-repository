source("D:/demo-repository/Script/data_sourcing/generic/get_geoSpatialData_soil.R")

dataPath <- "D:/demo-repository/Data/data_sourcing"
country <- "Rwanda"
useCaseName<- "RAB"
Crop <- "Maize"
AOI=TRUE
inputData = NULL
soilProfile = TRUE
season = 1
pathOut = NULL
jobs=4

extract_geoSpatialSoilPointData(dataPath=dataPath,country=country, useCaseName=useCaseName, Crop=Crop, AOI=AOI,inputData = inputData,
                                            soilProfile = soilProfile,  season = season, pathOut = pathOut, jobs=jobs)
