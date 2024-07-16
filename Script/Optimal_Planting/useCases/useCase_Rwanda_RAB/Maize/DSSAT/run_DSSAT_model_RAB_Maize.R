source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/dssat_exec.R")



pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"
country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
TRT <-1:2
varietyid <- "890011"
level2 <- NA
path_DSSAT <-"C:/DSSAT48/DSCSM048.EXE"


zones <- list.files(paste(pathOutput,"/useCase_", country, "_", useCaseName,"/", Crop, '/transform/DSSAT/AOI/',varietyid, sep=""))

for (i in 1:length(zones)){
    execmodel_AOI <-dssat.exec(pathOutput=pathOutput,country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI,
                               TRT=TRT,varietyid=varietyid, zone=zones[i], level2=level2,path_DSSAT=path_DSSAT)
}