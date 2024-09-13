#CUSTOM QUEFTS FUNCTION to calculate yield for a data frame with fertilizer nutrient application rates
#Requires estimates of soil N, P and K supply, and the attainable yield.
#nut_rates = data frame with headers N, P and K, and the nutrient rates applied in kg/ha. 
#supply = vector with indigenous soil N, P and K supply in kg/ha
#crop = crop for which to run Rquefts, by default "Maize"
#att_GY = attainable yield (without nutrient limitations) in kg/ha
#SeasonLength = required parameter to estimate the attainable yield, default = 120 days
#TODO: Other QUEFTS parameters can be added...

runQUEFTS <- function(nut_rates, #df with columns N, P and K with fertilizer nutrient rates in kg/ha
                      supply, #vector with N, P and K supply
                      crop = c("Maize", "Potato", 'Rice'), #crop to be defined by QUEFTS
                      Ya, #attainable yield
                      SeasonLength = 120){ #parameter important for the attainable yield estimation
  
  require("Rquefts")
  

  
  qCrop <- quefts_crop(crop) #crop params for QUEFTS
  
  qSoil <- quefts_soil() #soil params for QUEFTS
  #replacing the default by the values for N, P and K supply:
  qSoil$N_base_supply <- supply[1]
  qSoil$P_base_supply <- supply[2]
  qSoil$K_base_supply <- supply[3]
  
  #attainable yield, assuming ds contains a treatment which eliminates nutrient deficiency
  ## assuming the leaf ratio is leaf biomass estimation knowing the yield (leaf / yield)
  
  hi <- data.frame(crop       = c("Maize", "Potato", "Rice"),
                   leaf_ratio = c(0.46, 0.17, 0.18),
                   stem_ratio = c(0.56, 0.14, 0.67))
  

  
  qYa <- list(leaf_att  = Ya * hi[hi$crop == crop,]$leaf_ratio, #assumed ratio based on mean HI
              stem_att  = Ya * hi[hi$crop == crop,]$stem_ratio, #assumed ratio based on mean HI
              store_att = Ya, 
              SeasonLength=120)  
  
  Yq <- NULL
  for(i in 1:nrow(nut_rates)){
    qFertilizer <- list(N=nut_rates$N[i], 
                        P=nut_rates$P[i], 
                        K=nut_rates$K[i])
    q <- quefts(qSoil, qCrop, qFertilizer, qYa)
    Yq <- c(Yq, run(q)["store_lim"])
  }
  
  return(as.vector(Yq))
  
}

#REVERSE QUEFTS FUNCTION USING THE runQUEFTS function
#Estimates the apparent soil nutrient supply based on a set of observed crop responses in dataset ds
#start = vector with starting values for soil N, P and K supply in kg/ha, by default c(10, 60, 60). 
#ds = dataset with columns N, P and K as the fertiliser nutrient application rates in kg/ha, and
#     Y as the storage organ yield observed for the different fertiliser treatments.
#     This ds must include at least 3 (but 4-5 is better) treatments with orthogonal N, P and K rates,
#     of which one treatment has rates that can be assumed to eliminate nutrient deficiency.
#crop = crop for which to run Rquefts, by default "Maize"
#SeasonLength = required parameter to estimate the attainable yield, default = 120 days
#TODO: Other QUEFTS parameters can be added...
#TODO: add boundaries to avoid that supply values exceed 400 kg/ha (higher values are unlikely).

revQUEFTS <- function(ds,
                      Ya,
                      start = c(60, 10, 60), #starting values for the optimization algorithm.
                      crop = c("Maize", "Potato", "Rice"), #crop to be defined by QUEFTS
                      SeasonLength = 120){ #parameter important for the attainable yield estimation
  
  require("limSolve")
  require("lpSolve")
  

  
  
  #build the function to optimise
  qmo <- function(supply, 
                  ds, 
                  Ya,
                  crop=crop, 
                  SeasonLength=SeasonLength
  ){
    
    #obtain QUEFTS yield estimates for each of the rows in ds
    Yq <- runQUEFTS(nut_rates = subset(ds, select=c(N, P, K)),
                    supply = supply,
                    crop = crop,
                    Ya = Ya,
                    SeasonLength = SeasonLength)
    
    #calculate the total sum of squared errors (SSE) against the observed yields:
    SSE <- sum((Yq - ds$Y)**2)
    
    return(SSE)
    
  }
  
  #now performing the optimisation that minimises SSE:
  result <- optim(start,
                  #method = "L-BFGS-B",
                  #lower = c(0, 0, 0),
                  #upper = c(400, 400, 400),
                  qmo,
                  ds = ds,
                  Ya = Ya,
                  crop = crop,
                  SeasonLength = SeasonLength)
  
  #return the vector with the optimal solution for soil N, P and K supply:
  return(result$par)
  
}



#Alternative: run reverse QUEFTS in parallel on multiple CPUs
#create a function to run revQUEFTS optimisation:
## TODO this is tested only for potato, need some work to be generic
calculate_supply <- function(TLID, Crop){

  print(TLID)

  #subsetting and preparing data for revQUEFTS:
  dsi <- ds[ds$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y"
  dsi$Y <- dsi$Y * 1000 * 0.21 #converting to kg DM/ha, assuming 79% moisture content

  #attainable yield is set to 20% above yield obtained with high NPK rate:
  Yai <- mean(dsi[dsi$N > 75 & dsi$P > 30 & dsi$K > 50,]$Y) * 1.2

  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 2 & !is.na(Yai)){

    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = Crop)
    print(si)

    supply <- data.frame(TLID = i,
                         Ya = Yai,
                         N_base_supply = si[1],
                         P_base_supply = si[2],
                         K_base_supply = si[3])
  }

  return(supply)

}





#OPTIMISATION FUNCTION to calculate fertiliser rates to achieve a specified yield increase
#Requires a set of fertilisers and the target yield increase specified in absolute or relative terms.
#my_ferts = dataframe with fertiliser details similar to Rquefts::fertilizers()
#dY = target yield increase in absolute terms or relative terms
#     e.g., dY = 2000 and target = "absolute": calculates fertiliser rates required to increase yield by 2000 kg/ha;
#     or, e.g., dY = 0.4 and target = "relative": calculates fertiliser rates required to increase yield by 40%.
#start = start values for optimisation algorithm, set by default to 50 kg/ha for each fertiliser in my_ferts.
##supply = vector with indigenous soil N, P and K supply in kg/ha
#crop = crop for which to run Rquefts, by default "Maize"
#Ya = attainable yield (without nutrient limitations) in kg/ha
#SeasonLength = required parameter to estimate the attainable yield, default = 120 days
#TODO: Other QUEFTS parameters can be added...
#TODO: Requires improvement to improve stability across range of start values... 
#      Currently attempts to minimise the total quantity of fertiliser
#      May require constraints to avoid negative values or values below 25 kg/ha.


rec_targetdY_pot <- function(my_ferts,
                             dY,
                             target = c("relative", "absolute"), #is dY relative or absolute?
                             start = rep(1, nrow(my_ferts)), #starting values for the optimisation algorithm.
                             supply, #indigenous nutrient supply
                             crop = crop, #crop to be defined by QUEFTS
                             att_GY, #attainable yield
                             GY_br, #blnaket recom yield
                             SeasonLength = 120,
                             isBlanketRef = FALSE,
                             df_link){ #parameter important for the attainable yield estimation
  
  
  # df_link$Y_att <- att_GY * 1.2
  df_link$yieldPercinc <- paste(dY*100, " %", sep="")
  
  #calculate the control yield $for Rice. this is the yield estimated by QUEFTS for soil INS + blanket recommendation
  if(isBlanketRef == TRUE){
    GY0  <- GY_br
  }else{
    GY0 <- runQUEFTS(nut_rates = data.frame("N" = 0, "P" = 0, "K" = 0),
                     supply = supply,
                     crop = crop,
                     Ya = att_GY,
                     SeasonLength = SeasonLength)
  }
  
  
  #calculate the target yield
  if(dY == 0){
    GYt <- GY0
    df_link$targetYield <- GY_br
  }else{
    GYt <- ifelse(target == "absolute", GY0 + dY, GY0 * (1 + dY))
    df_link$targetYield <- round(GY_br + (GY_br * dY), 2)
  }
  
  if(GYt > att_GY){
    # print("No solution available: yield target exceeds attainable yield.")
    fert_rates <- rep(NA, nrow(my_ferts))
    
  }else{
    
    #build the function to optimise
    qfo <- function(fert_rates,
                    my_ferts = my_ferts,
                    GYt = GYt,
                    supply = supply,
                    crop = crop,
                    Ya = att_GY,
                    SeasonLength=SeasonLength){
      
      GYq <- runQUEFTS(nut_rates = as.data.frame(as.list(nutrientRates(my_ferts, fert_rates))),
                       supply = supply,
                       crop = crop,
                       Ya = att_GY,
                       SeasonLength = SeasonLength)
      
      #set a value that minimises the yield difference with the control, and in addition...
      #minimises the total amount of fertiliser to apply (to ensure more stable results).
      return((GYq - GYt)**2 + sqrt(sum(fert_rates**2)))
      
    }
    
    #now performing the optimisation that minimises the difference to the target yield:
    result <- optim(start,
                    qfo,
                    my_ferts = my_ferts,
                    GYt = GYt,
                    supply = supply,
                    crop = crop,
                    Ya = att_GY,
                    SeasonLength = SeasonLength)
    
    #return the vector with the fertilizer rates to achieve the target:
    fert_rates <- result$par
    
  }
  
  frn <- data.frame(fert = my_ferts$name, rates=fert_rates)
  dw <- spread(frn, fert, rates)
  df_link <- cbind(df_link, dw)
  
  # df_link$DAP <- round(fert_rates[1], 0)
  # df_link$Urea <- round(fert_rates[2], 0)
  # df_link$NPK171717 <- round(fert_rates[3], 0)
  
  return(df_link)
  
}


