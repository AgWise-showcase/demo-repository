source("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Script/Optimal_Planting/generic/dssat_expfile_zone.R")

country <- "Rwanda"
useCaseName <- "RAB"
Crop <-  "Maize"
AOI = TRUE
season=1
level2 <- NA
varietyid <- "890011"
pathIn_zone = TRUE
path_coord <- paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw", sep="")
pathInput <- paste("D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel", sep="")
pathOutput <- "D:/OneDrive - CGIAR/agwise/DSSAT/demo-repository/Data/Optimal_Planting"
filex_temp <- "KEAG8104.MZX"
Planting_month_date="03-01"
Harvest_month_date="11-30"
ID="TLID"
plantingWindow=1
fertilizer=F
geneticfiles = "MZCER048"
index_soilwat=1

zones <- list.files(paste(pathOutput,"/useCase_", country, "_", useCaseName,"/", Crop, '/transform/DSSAT/AOI/',varietyid, sep=""))


for (i in 1:length(zones)){
    expdata_AOI <- dssat.expfile(path_coord=path_coord,pathInput=pathInput, pathOutput=pathOutput,
                                 country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI,
                                 filex_temp=filex_temp, Planting_month_date=Planting_month_date,Harvest_month_date=Harvest_month_date, 
                                 ID=ID,season =season, plantingWindow=plantingWindow,varietyid=varietyid, zone =zones[i], level2=level2, 
                                 fertilizer=fertilizer,geneticfiles=geneticfiles,index_soilwat=index_soilwat,pathIn_zone =pathIn_zone)
  }
