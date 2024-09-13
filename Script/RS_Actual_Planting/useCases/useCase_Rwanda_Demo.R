#### Actual planting date based on Remote Sensing workflow

# Set the location where the generic scripts are stored (eg. ~\Demo\Scripts\generic\)
script.loc <- utils::choose.dir(caption="Please choose the folder where the generic scripts are stored")

#################################################################################################################
## 1 - Download MODIS data ####
## source "get_MODISdata_SA.R" function and execute it for Rwanda Demo use case
#################################################################################################################
## NOTA user are required to register on Earth Data to get their credentials (https://urs.earthdata.nasa.gov/)

source(paste(script.loc,"\\get_MODISdata_SA.R", sep=""))

usr = "*" # Earth Data user name
pswrd = "*" # Earth Data password
country = "Rwanda"
useCaseName = "Demo"
level = 0
admin_unit_name = NULL
Start_year = "2019"
End_year = "2020"
overwrite = TRUE

download_MODIS(usr, pswrd, country , useCaseName ,level , admin_unit_name , Start_year, End_year, overwrite = TRUE)

#################################################################################################################
## 1bis - Optional Download the crop mask data from Google Earth Engine ####
## Please follow the get_ESACropland_fromGEE.html guidelines
# ~Demo\Scripts\generic\et_ESACropland_fromGEE.html
#################################################################################################################


#################################################################################################################
## 2 - Preprocessed the MODIS data ####
## source "get_MODISts_PreProc_SA.R" function and execute it for Rwanda Demo use case
#################################################################################################################
source(paste(script.loc,"\\get_MODISts_PreProc_SA.R", sep=""))

country = "Rwanda"
useCaseName = "Demo"
Planting_year = 2021
Harvesting_year = 2022
overwrite = TRUE
CropMask=TRUE

smooth_rasterTS(country, useCaseName, Planting_year, Harvesting_year, overwrite, CropMask)

#################################################################################################################
## 3 - Optional - Derive the crop type map from MODIS data ####
## source "get_RS_Croptype_SA.R" function and execute it for Rwanda Demo use case
#################################################################################################################
source(paste(script.loc,"\\get_RS_Croptype_SA.R", sep=""))


country = "Rwanda"
useCaseName = "Demo"
level = 1
admin_unit_name = NULL
Planting_year = 2021
Harvesting_year = 2022
Planting_month = "September"
Harvesting_month = "March"
overwrite = TRUE
crop = c("Maize")
coord = c("lon", "lat")
CropMask = TRUE

CropType (country, useCaseName, level, admin_unit_name, Planting_year, Harvesting_year, Planting_month, Harvesting_month, crop, coord, overwrite, CropMask)

#################################################################################################################
## 4 - Compute the Planting dates from MODIS data ####
## source "get_RS_Phenology_SA.R" function and execute it for Rwanda Demo use case
#################################################################################################################
source(paste(script.loc,"\\get_RS_Phenology_SA.R", sep=""))

country = "Rwanda"
useCaseName = "Demo"
level = 1
admin_unit_name = NULL
crop= "Maize"
Planting_year = 2021
Harvesting_year = 2022
Planting_month = "September"
Harvesting_month = "March"
emergence = 5
overwrite = TRUE
CropMask = TRUE
CropType = FALSE
thr= c(0.50, 0.30)
validation = TRUE
coord = c('lon', 'lat')

Phenology_rasterTS(country, useCaseName, crop, level, admin_unit_name, Planting_year, Harvesting_year, Planting_month, Harvesting_month, 
                   emergence, CropMask=T, CropType=F, coord, thr=c(0.50,0.30), validation=T, overwrite)

