# AgWise
AgWise is a collaborative and adaptive data analytics framework designed to generate tailored agronomic advice. It offers scalable, generic workflows for delivering agronomy advisories across a wide range of crops and target regions. The framework ensures broad accessibility and applicability to support diverse agricultural needs. It specializes in providing site- and context-specific fertilizer recommendations based on the unique conditions of each target area. AgWise is currently providing agronomic advisories in multiple countries for various crops, promoting scientifically robust recommendations that contribute to improved agricultural productivity, enhanced climate risk management and environmental sustainability in the global South.
# AgWise analytical workflow
This repository contains data and scripts for demonstrating AgWise analytical workflows. It includes tools to develop site-specific fertilizer advice, determine optimal planting dates and cultivars using process-based crop models, and map crop types and planting dates using remote sensing techniques.


![Auto Assign](https://github.com/AgWise-showcase/demo-repository/actions/workflows/auto-assign.yml/badge.svg)

![Proof HTML](https://github.com/AgWise-showcase/demo-repository/actions/workflows/proof-html.yml/badge.svg)


## AgWise fertilizer recommendation workflows 
The tool utilizes advanced data analytical techniques to model the variation in yield response to nutrient use observed in field trials data. This approach ensures that recommendations are finely tuned to the specific needs of each field by integrating multiple factors into its models, including, soil conditions, local weather patterns, crop-specific requirements and socio-economic conditions of the target farmers. The ultimate objective of AgWise is helping farmers to maximize nutrient use efficiency, enhance crop productivity and increase farm profitability by recommending optimal fertilizer rates tailored to their unique conditions.
In AgWise we have four different approaches that we use based on data availability and quality and objective of the advisory.In this repository, data and scripts are provided to demonstrate the AgWise workflows. 

#### The first approach 
Using QUEFTS crop model to determine the soil nutrient supply and the yield gap. Using yield measurements from field trials under different nutrient applications, the soil nutrient supply is determined. Scripts for this approach can be found Script/Fertilizer_recom/fert_advisory_QUEFTS.R

#### The second approach
Using machine learning algorithms yield is modeled as response to nutrients and biophysical factors such as soil and weather. The script for this workflow can be found in Script/Fertilizer_recom/fert_advisory_ML.R

#### The third approach
Integrates QUEFTS and Machine learning. Several machine learning algorithms are used to model the soil nutrient supply estimated using QUEFTS as a response to geospatial variables sourced at scale. The script for this workflow can be found in Script/Fertilizer_recom/fert_advisory_ML_QUEFTS.R

#### The fourth approach 
Refining the third approach by incorporating the yield ceiling for rainfed farming. This yield ceiling (water limited yield) is simulated at scale for several climate scenarios using crop models such as DSSAT and Oryza. Having the spatial and temporal variation in yield ceiling makes the fertilizer advice more contextualized. 

#### How to choose which approach to use
Data Requirements: The complexity and accuracy of the models depend on the availability and quality of data. While the first two approaches rely mainly on field trials data, the third and fourth approaches incorporate extensive geo-spatial and climate data.
Complexity: The complexity increases from the first approach to the fourth, with the incorporation of advanced modeling techniques and data requirements.
Accuracy: Generally, as the complexity increases, the accuracy and precision of fertilizer advice tend to improve, especially in estimating soil nutrient supply and yield potential under varying conditions.
Applicability: The choice of approach depends on the availability of data, computational resources, and the specific requirements of the target audience. While simpler models may suffice for initial fertilizer advice, more complex models offer better precision and adaptability to diverse conditions.

## Optimal planting dates and cultivars
The AgWise planting dates and cultivars optimization workflow couples diverse crop models with spatially varying soil and weather data to provide accurate, site-specific recommendations. It plays a critical role in enhancing climate resilience in farming by evaluating the impact of various climate scenarios on crop variety selection for specific target areas. AgWise assesses the effects of different climate scenarios, helping farmers choose the most suitable crop varieties for their regions. It generates varieties suitability maps, detailing the performance of long, medium, and short-duration crop varieties under different climate scenarios. 
We estimated the optimal planting dates and cultivars per location based on water-limited simulations using the crop model  Decision Support System for Agrotechnology Transfer <a href="http://dssat.net/about">(DSSAT)</a>. The model used weather data from CHIRPS and AgERA5, while ISRIC was considered for the soil data. The simulations considered 22-year historical data (2000-2022). We defined 3 generic cultivars based on their crop duration (short, medium, and long). To define the optimal planting date, we obtained an initial planting date from expert opinion in each country, and we included 9 additional weekly planting dates after the initial recommendation. The simulation outputs were aggregated across different planting dates, varieties, and seasons. Therefore, we identified the optimum planting dates across varieties and season types. We classify the season types by  each growing season during the historical simulations based on the 3 phases of El Niño-Southern Oscillation (ENSO). The determination of ENSO phases was undertaken through the use of the Oceanic Niño Index (ONI). ONI values greater and less than 0.5 are defined as El Niño and La Niña, respectively. In contrast, ONI values between -0.5 and 0.5 signify a neutral ENSO phase. The planting date with the highest median yield across the years was the optimum planting date. The optimal planting date was defined in a pixel base and estimated by cultivar type. In addition, the optimal cultivar by the ENSO phase was the one with the highest median yield across planting dates. The following text will explain the workflow for optimal planting and cultivars.

#### Create weather and soil files in DSSAT format
The function `readGeo_CM_zone` available in [Script/Optimal_Planting/generic/1_readGeo_CM_zone.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/generic/1_readGeo_CM_zone.R) reads input data in R format (RDS) for each location that we want to simulate. For details on how to organize the weather and soil data, see the example data in the following folder [Data/Optimal_Planting/useCase_Rwanda_RAB/Maize/raw/geo_4cropModel/Amajyaruguru](https://github.com/AgWise-showcase/demo-repository/tree/main/Data/Optimal_Planting/useCase_Rwanda_RAB/Maize/raw/geo_4cropModel/Amajyaruguru). We use the [DSSAT package](https://cran.r-project.org/web/packages/DSSAT/index.html) available in R to convert the information from RDS to DSSAT format. 
The following is an example of obtaining information for Rwanda in DSSAT format. We also include a brief explanation of the function's parameters. See the whole example to run the country at [/Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/1_get_Weather_Soil_data_RAB_Maize.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/1_get_Weather_Soil_data_RAB_Maize.R):

      #' @param pathInput: Path with all the weather and soil input data in R (RDS) format (e.g. D:/datasourcing/Data/useCase_Rwanda_RAB/Potato/result/geo_4cropModel)
      #' @param pathOutput: Main path where the weather and soil data in DSSAT format will be created.
      #' @param country: Country where the simulations are created for
      #' @param useCaseName: Name of the use case or project
      #' @param Crop: Name of the crop that will be simulated.
      #' @param AOI: TRUE if the data is required for a target area, and FALSE for trial sites with observed yield data.
      #' @param season: Refers to possible different seasons for the same year where 1 is the main season. 
      #' @param zone: Administrative level 1 for which the weather and soil data will be created.
      #' @param level2: Administrative level 2 for which the weather and soil data will be created.
      #' @param varietyid: ID of the variety in DSSAT format that will be used for the simulations (column @VAR# in the cultivar file and parameter INGENO in the experimental file *.**X)
      #' @param pathIn_zone: TRUE if the input data (in geo_4cropModel) are organized by zone or province and FALSE if it is just one file for the whole country

      source(~/Script/Optimal_Planting/generic/1_readGeo_CM_zone.R)
      country <- "Rwanda"
      useCaseName <- "RAB"
      Crop <-  "Maize"
      AOI = TRUE
      season=1
      level2 <- NA
      varietyid <- "890011"
      pathIn_zone = TRUE
      pathInput <- paste("~/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel", sep="")
      pathOutput <- "~/Data/Optimal_Planting"
      countryShp <- geodata::gadm(country, level = 1, path='.')
      zones <- unique(countryShp$NAME_1)

      readGeo_CM_zone(pathInput= pathInput,pathOutput = pathOutput, country = country, useCaseName = useCaseName, Crop = Crop, AOI = AOI, 
                      season= season, zone =zones[1],level2=level2,varietyid =varietyid,pathIn_zone = pathIn_zone)

You must have a soil.sol file as a template for creating the different soil data by location. The template soil data is located in the [Data and Landing folder of the example simulation](https://github.com/AgWise-showcase/demo-repository/blob/main/Data/Optimal_Planting/useCase_Rwanda_RAB/Maize/Landing/DSSAT/soil.sol)

#### Create the experimental files in DSSAT format
The function `dssat.expfile` available in [Script/Optimal_Planting/generic/2_dssat_expfile_zone.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/generic/2_dssat_expfile_zone.R) creates an experimental file based on a [template experimental file](https://github.com/AgWise-showcase/demo-repository/blob/main/Data/Optimal_Planting/useCase_Rwanda_RAB/Maize/Landing/DSSAT/KEAG8104.MZX). This function also copies all the genetic parameters of DSSAT in each of the folders with weather and soil data so they can be used in the simulations. The function allows you to have different weekly planting dates (_plantingWindow_) based on an initial planting date (_Planting_month_date_). You can add fertilizer applications for each planting as long as it is initially defined in the template file (_fertilizer=F_). The function starts the simulations and field capacity one month before planting; however, if you want to modify the initial soil water content, you can change the parameter _index_soilwat_ with 1 representing field capacity and 0 wilting point. Finally, you can change the variety ID (_varietyid_) you want to simulate. The ID of the variety should be available in the [template cultivar file of DSSAT](https://github.com/AgWise-showcase/demo-repository/blob/main/Data/Optimal_Planting/useCase_Rwanda_RAB/Maize/Landing/DSSAT/MZCER048.CUL). Below there is an example of how to obtain a experimental file for a given zone and variety. The example also includes the additional parameters that were not defined for the creation of weather and soil files. A whole example for the country is available at [/Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/2_create_Experimental_File_RAB_Maize.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/2_create_Experimental_File_RAB_Maize.R)   
      
      #' @param path_coord: Path with the coordinates for each point that will be simulated.
      #' @param filex_temp: Name of the template experimental file in DSSAT format (FILEX).
      #' @param geneticfiles: Name of  the genetic files in DSSAT (e.g., for maize, MZCER048)
      #' @param Planting_month_date: It is needed only when AOI=TRUE and defines the initial planting date in mm-dd format 
      #' @param Harvest_month_date: If AOI =TRUE, it is the initial harvesting date in mm-dd format.
      #'        The parameter is not needed when AOI=FALSE because the actual harvesting date from the trials would be provided.   
      #' @param ID: Trial ID or column name used as the ID of the different locations simulated.
      #' @param plantingWindow: Number of weeks from the initial Planting_month_date that will defined as additional planting dates.
      #' @param fertilizer: if TRUE the parameter modifies the fertilizer date to be at planting for each of the planting dates defined.
      #' @param index_soilwat: Index that defines the initial soil water content with 1 representing field capacity and 0 wilting point.
      
      source("~/Script/Optimal_Planting/generic/2_dssat_expfile_zone.R")

      country <- "Rwanda"
      useCaseName <- "RAB"
      Crop <-  "Maize"
      AOI = TRUE
      season=1
      level2 <- NA
      varietyids <- c("890011","890012","890013")
      pathIn_zone = TRUE
      filex_temp <- "KEAG8104.MZX"
      Planting_month_date="03-01"
      Harvest_month_date="11-30"
      ID="TLID"
      plantingWindow=1
      fertilizer=F
      geneticfiles = "MZCER048"
      index_soilwat=1
      
      path_coord <- paste("~/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw", sep="")
      pathInput <- paste("~/Data/Optimal_Planting/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel", sep="")
      pathOutput <- "~/Data/Optimal_Planting"
      
      
      zones <- list.files(paste(pathOutput,"/useCase_", country, "_", useCaseName,"/", Crop, '/transform/DSSAT/AOI/',varietyids[1], sep=""))
      zones <- zones[file.info(paste(pathOutput, "/useCase_", country, "_", useCaseName, "/", Crop, '/transform/DSSAT/AOI/', varietyids[1], "/", zones, sep=""))$isdir]
      zones <- zones[zones != "gadm"]
      
      expdata_AOI <- dssat.expfile(path_coord=path_coord,pathInput=pathInput, pathOutput=pathOutput,
                                       country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI,
                                       filex_temp=filex_temp, Planting_month_date=Planting_month_date,Harvest_month_date=Harvest_month_date, 
                                       ID=ID,season =season, plantingWindow=plantingWindow,varietyid=varietyids[1], zone =zones[1], level2=level2, 
                                       fertilizer=fertilizer,geneticfiles=geneticfiles,index_soilwat=index_soilwat,pathIn_zone =pathIn_zone)

#### Run the simulations in DSSAT
The function `dssat.exec` available in [Script/Optimal_Planting/generic/3_dssat_exec.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/generic/3_dssat_exec.R) runs the model based on the weather, soil and experimental files created on the previous steps. An example of running the model in Rwanda is presented below. A whole example for the country is available at [Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/3_run_DSSAT_model_RAB_Maize.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/3_run_DSSAT_model_RAB_Maize.R)

      #' @param path_DSSAT: Path where the executable/binary file of DSSAT is located (you need to install DSSAT before running the simulations).
      #' @param TRT is the number of treatments to be run from the experimental file

      pathOutput <- "~/Data/Optimal_Planting"
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
      zones <- zones[file.info(paste(pathOutput, "/useCase_", country, "_", useCaseName, "/", Crop, '/transform/DSSAT/AOI/', varietyid[1], "/", zones, sep=""))$isdir]
      zones <- zones[zones != "gadm"]

      execmodel_AOI <-dssat.exec(pathOutput=pathOutput,country=country, useCaseName=useCaseName, Crop=Crop, AOI = AOI,
                                     TRT=TRT,varietyid=varietyids[1], zone=zones[1], level2=level2,path_DSSAT=path_DSSAT)

#### Merge the DSSAT simulations
The function `merge_DSSAT_output` available in [Script/Optimal_Planting/generic/4_merge_DSSAT_output.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/generic/4_merge_DSSAT_output.R), merges all the outputs of the simulations in one file in RDS format. The example to merge the results in Rwanda is available in [Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/4_merge_outputs_DSSAT_RAB_Maize.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/4_merge_outputs_DSSAT_RAB_Maize.R)

#### Summary of the DSSAT simulations
The function `get_ONI` is available in [Script/Optimal_Planting/generic/5_dssat_summary_ONI.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/generic/5_dssat_summary_ONI.R). This function creates summary plots based on the yield results for the different ENSO phases and varieties. The example code is available in [Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/5_summary_ONI_DSSAT_RAB_Maize.R](https://github.com/AgWise-showcase/demo-repository/blob/main/Script/Optimal_Planting/useCases/useCase_Rwanda_RAB/Maize/DSSAT/5_summary_ONI_DSSAT_RAB_Maize.R). 

## Scripts for use of remote sensing for crop type mapping will be added soon
