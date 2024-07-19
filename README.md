
# AgWise analytical workflow
This repository contains data and scripts for demonstrating AgWise analytical workflows. It includes tools to develop site-specific fertilizer advice, determine optimal planting dates and cultivars using process-based crop models, and map crop types and planting dates using remote sensing techniques.

## AgWise fertilizer recommendation workflows 
Developing fertilizer advice requires knowledge of the yield gap and the yield response to nutrient applications. In AgWise we have four different approaches that we use based on data availability and quality and objective of the advisory.In this repository, data and scripts are provided to demonstrate the AgWise workflows. 

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
We estimated the optimal planting dates and cultivars per location based on water-limited simulations using the crop model  Decision Support System for Agrotechnology Transfer <a href="http://dssat.net/about">(DSSAT)</a>. The model used weather data from CHIRPS and AgERA5, while ISRIC was considered for the soil data. The simulations considered 22-year historical data (2000-2022). We defined 3 generic cultivars based on their crop duration (short, medium, and long). To define the optimal planting date, we obtained an initial planting date from expert opinion in each country, and we included 9 additional weekly planting dates after the initial recommendation. The simulation outputs were aggregated across different planting dates, varieties, and seasons. Therefore, we identified the optimum planting dates across varieties and season types. We classify the season types by  each growing season during the historical simulations based on the 3 phases of El Niño-Southern Oscillation (ENSO). The determination of ENSO phases was undertaken through the use of the Oceanic Niño Index (ONI). ONI values greater and less than 0.5 are defined as El Niño and La Niña, respectively. In contrast, ONI values between -0.5 and 0.5 signify a neutral ENSO phase. The planting date with the highest median yield across the years was the optimum planting date. The optimal planting date was defined in a pixel base and estimated by cultivar type. In addition, the optimal cultivar by the ENSO phase was the one with the highest median yield across planting dates. The following text will explain the workflow for optimal planting and cultivars.

#### Create weather and soil files in DSSAT format
The function `readGeo_CM_zone` available in [Script/Optimal_Planting/generic/1_readGeo_CM_zone.R](https://github.com/AgWise-showcase/demo-repository/blob/Optimal_Planting/Script/Optimal_Planting/generic/1_readGeo_CM_zone.R) reads input data in R format (RDS) for each location that we want to simulate. For details on how to organize the weather and soil data, see the example data in the following folder [Data/Optimal_Planting/useCase_Rwanda_RAB/Maize/raw/geo_4cropModel/Amajyaruguru](https://github.com/AgWise-showcase/demo-repository/tree/Optimal_Planting/Data/Optimal_Planting/useCase_Rwanda_RAB/Maize/raw/geo_4cropModel/Amajyaruguru). We use the [DSSAT package](https://cran.r-project.org/web/packages/DSSAT/index.html) available in R to convert the information in R format to DSSAT format. 
The following is an example of how to obtain information in DSSAT format for Rwanda:

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



## Scripts for use of remote sensing for crop type mapping will be added soon
