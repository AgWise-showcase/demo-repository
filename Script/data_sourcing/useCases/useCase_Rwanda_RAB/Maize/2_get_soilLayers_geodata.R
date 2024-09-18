#' Function to get soil layers from soilGrids using the library geodata
#'
#' @param country Country name for which the data will be extracted (if bbox=NULL)
#' @param useCaseName Name of the use case/research project
#' @param Crop Name of the crop
#' @param dataPath Main path where the data will be saved
#' @param depths Soil depths for which the soil data will be downloaded 
#' @param depths_complete Soil depth threshold for which the soil data will be downloaded.
#'        It needs to coincide with the the parameter depths

library(geodata)
library(furrr)

plan(multisession)


country <- "Rwanda"
useCaseName<- "RAB"
Crop <- "Maize"
dataPath <- "D:/demo-repository/Data/data_sourcing"


#### Download soilGrids variables #####
variables <- c("bdod","cfvo", "clay", "nitrogen","phh2o", "sand", "silt", "soc") 
depths <- c(5,15,30,60)
depths_complete <- c("0-5","5-15","15-30","30-60")
downloadpath <- paste0(dataPath,"/useCase_", country, "_" ,useCaseName,"/",Crop,"/Landing/Soil/soilGrids/profile/")

if (!dir.exists(downloadpath)) {
  dir.create(downloadpath,recursive = TRUE)
}

# Create a list of all variable-depth combinations
var_depth_combinations <- expand.grid(variable = variables, depth = depths, stringsAsFactors = FALSE)

# Download, crop, and save rasters in parallel
future_map2(var_depth_combinations$variable, var_depth_combinations$depth, function(var, depth) {
  # Download raster
  capa <- soil_world(var = var, depth = depth, path = downloadpath)
  
  # Load the shapefile for the country
  countryShp <- geodata::gadm(country, level = 2, path = '.')
  
  # Crop the raster
  cropped_raster <- crop(capa, countryShp)
  
  # Construct the new file path
  depth_index <- which(depths == depth)
  oldfile <- paste0(downloadpath, "soil_world/", var, "_", depths_complete[depth_index], "cm_mean_30s.tif")
  file.remove(oldfile)
  newfile <- paste0(downloadpath, var, "_", depths_complete[depth_index], "cm_mean_30s.tif")
  
  # Save the cropped raster
  writeRaster(cropped_raster, newfile, filetype = "GTiff", overwrite = TRUE)
})

#Get CEC from soil_af
for (i in 1:length(depths)){
  var="cec"
  capa <- soil_af(var="CEC",depth=depths[i],path = downloadpath)
  # Load the shapefile for the country
  countryShp <- geodata::gadm(country, level = 2, path = '.')
  
  # Crop the raster
  cropped_raster <- crop(capa, countryShp)
  
  # Construct the new file path
  depth_index <- which(depths == depths[i])
  # oldfile <- paste0(downloadpath, "soil_af/af_", var, "_", depths_complete[depth_index], "cm_30s.tif")
  # file.remove(oldfile)
  newfile <- paste0(downloadpath, var, "_", depths_complete[depth_index], "cm_mean_30s.tif")
  
  # Save the cropped raster
  writeRaster(cropped_raster, newfile, filetype = "GTiff", overwrite = TRUE)
}


#Remove additional original files with different name than the variable
unlink(paste0(downloadpath, "soil_af"), recursive = TRUE)
unlink(paste0(downloadpath, "soil_world"), recursive = TRUE)

#### Download soil elements concentration #####
downloadpath2 <- paste0(dataPath,"/useCase_", country, "_" ,useCaseName,"/",Crop,"/Landing/Soil/soilGrids/")
variables2 <- c("B","Cu","K","Mn", "N","P", "Ptot")


# Download, crop, and save rasters in parallel
future_map(variables2, function(var) {
  # Download raster
  capa <- soil_af_elements(var = var, path = downloadpath2)
  
  # Load the shapefile for the country
  countryShp <- geodata::gadm(country, level = 2, path = '.')
  
  # Crop the raster
  cropped_raster <- crop(capa, countryShp)
  
  # Construct the new file path
  vart<- tolower(var)
  # oldfile <- paste0(downloadpath2, "soil_af/af_", vart, "_0-30cm_30s.tif")
  # file.remove(oldfile)
  newfile <- paste0(downloadpath2, "af_",vart, "_0-30cm_30s.tif")
  
  # Save the cropped raster
  writeRaster(cropped_raster, newfile, filetype = "GTiff", overwrite = TRUE)
})
#Remove additional original files with different name than the variable
unlink(paste0(downloadpath2, "soil_af"), recursive = TRUE)

#### Download variables from iSDA #####
downloadpath3 <- paste0(dataPath,"/useCase_", country, "_" ,useCaseName,"/",Crop,"/Landing/Soil/iSDA/")
if (!dir.exists(downloadpath3)) {
  dir.create(downloadpath3,recursive = TRUE)
}
variables3 <- c("clay","C.tot","Ca","db.od", "eCEC.f", "Fe", "K", "Mg", "N.tot", "oc", "P", "pH.H2O", 
                "sand", "silt", "S", "texture")
depths3 <- c(20,50)
depths_complete3 <- c("0-20", "20-50")
# Create a list of all variable-depth combinations
var_depth_combinations3 <- expand.grid(variable = variables3, depth = depths3, stringsAsFactors = FALSE)

# Download, crop, and save rasters in parallel
future_map2(var_depth_combinations3$variable, var_depth_combinations3$depth, function(var, depth) {
  # Download raster
  capa <- soil_af_isda(var = var, depth = depth, path = downloadpath3)
  # Load the shapefile for the country
  countryShp <- geodata::gadm(country, level = 2, path = '.')
  # Crop the raster
  cropped_raster <- crop(capa, countryShp)
  # Construct the new file path
  depth_index <- which(depths3 == depth)
  vart<- tolower(var)
  # oldfile <- paste0(downloadpath3, "soil_af_isda/isda_", vart, "_", depths_complete3[depth_index], "cm_v0.13_30s.tif")
  # file.remove(oldfile)
  newfile <- paste0(downloadpath3,"isda_", vart, "_", depths_complete[depth_index], "cm_v0.13_30s.tif")
  
  # Save the cropped raster
  writeRaster(cropped_raster, newfile, filetype = "GTiff", overwrite = TRUE)
})
#Remove additional original files with different name than the variable
unlink(paste0(downloadpath3, "soil_af_isda"), recursive = TRUE)
