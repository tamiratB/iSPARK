rm(list=ls()) # cleaning the environment
gc() # clearing memory

library(dplyr)
#library(mgcv)
#library(e1071)
library(leaflet)
#library(tidyr)
library(randomForest)
library(mice)
library(reshape2)
library(stringr)

#----------- define working directory and environmental variable --------------

scriptDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptDir)

# download weather and elevation dataset
# if you already downloaded the dataset, make sure you need to re-download? This is expensive
# operation and time consuming.
dataDownload = TRUE

# -------------------------- main script starts here ---------------------------

if(dataDownload == TRUE){
    # --------- reading raw data and subset for Nigeria and maize crop -------------
    
    carob <- read.csv("../data/carob_fertilizer-cc.csv", stringsAsFactors = FALSE)
    nigeria <- subset(carob, country == "Nigeria" & crop == "maize")
    
    # ------------------------ cleaning carob data ---------------------------------
    
    # drop columns with NA values for the whole row (columns with no data)
    tmp <- apply(nigeria, 2, function(col) all(is.na(col)))
    carobCleaned <- nigeria[, !tmp]
    
    # filter out rows where any of the 'yield' columns have NA values (rows having no yield data)
    carobCleaned <- carobCleaned[!apply(carobCleaned["yield"], 1, function(row) any(is.na(row))), ]
    
    # drop "dataset_id","country","reference", and "crop" columns (not needed for the assessments)
    carobCleaned <- carobCleaned[, !(names(carobCleaned) %in% c("dataset_id","country","reference","crop"))]
    
    # clean 'planting_date' formats to YYYY to make it consistent with the rest of sites dataset 
    carobCleaned$planting_date <- substr(carobCleaned$planting_date, 1, 4)
    # rename column 'planting_date' to 'year' and cast the new 'year' column to numeric
    carobCleaned <- carobCleaned %>%
      rename(year = planting_date)
    carobCleaned$year <- as.numeric(carobCleaned$year)
    
    # replace NA values in 'year' by 9999, so 'year' won't be dropped during filtering (for trying to impute missing years later)
    carobCleaned["year"][is.na(carobCleaned["year"])] <- 9999
    
    # ------------ generate unique names for 'sites' having no names ---------------
    
    # first, format latitude and longitude into two decimal places
    carobCleaned$latitude <- as.numeric(formatC(carobCleaned$latitude, format = "f", digits = 2))
    carobCleaned$longitude <- as.numeric(formatC(carobCleaned$longitude, format = "f", digits = 2))
    
    # generate unique site id based on sites latitude and longitude data
    carobCleaned <- carobCleaned %>%
      mutate(siteId = str_c("site_", latitude, "_", longitude)) %>% # assign ID based on the latlon
      group_by(siteId) %>%
      mutate(uniqueId = str_c(siteId, "_", row_number())) %>%  # to check how many of them are there in total
      ungroup()
    
    # identify distinct siteId values
    uniqueSiteIds <- unique(carobCleaned$siteId)
    
    # generate unique site names
    uniqueSiteNames <- paste0("unknownSite", seq_along(uniqueSiteIds))
    
    # mapping site IDs to unique site names
    siteNameMap <- setNames(uniqueSiteNames, uniqueSiteIds)
    
    # assign unique site names to a new column 'siteName' based on siteId
    carobCleaned <- carobCleaned %>%
      mutate(siteName = siteNameMap[as.character(siteId)])
    
    # assign values from 'siteName' column to 'site' column where 'site' has empty strings
    # and keep labeled sites name untouched
    carobCleaned <- carobCleaned %>%
      mutate(site = ifelse(site == "", siteName, site))
    
    # drop the 'siteName' column, not needed any longer
    carobCleaned <- carobCleaned %>%
      select(-siteName)
    
    # --- filtering out columns having missing values larger than the threshold ----
    
    # count missing values (NA and empty character "") in each column and drop columns having
    # large number of missing values
    threshold <- 310 # threshold for missing values more than %5 of the total data (i.e., 6200)
    
    # filter columns (drop NAs and empty character (i.e., "") counts more than the threshold) 
    carobCleaned <- carobCleaned %>%
      select(where(~ sum(is.na(.)) <= threshold)) %>%  # NAs
      select(where(~ sum(as.character(.) == "", na.rm=TRUE) <= threshold)) # empty character
    
    # replace back missing 'year' values with NA and try to impute missing years (not the best way, but better than dropping records)
    carobCleaned$year[carobCleaned$year == 9999] <- NA
    
    # show summary of missing values (NA and empty character "") in each column
    print(carobCleaned %>% summarise(across(everything(), ~ sum(is.na(.)))))
    print(carobCleaned %>% summarise(across(everything(), ~ sum(. == ""))))
    
    # impute missing N, P, K, and year values using "mice imputation" method
    tmp <- mice(carobCleaned, m = 15)  # create mice object with 15 imputations
    carobCleaned <- complete(tmp)  # complete imputed dataset
    
    # extract a unique site's 'latitude' and 'longitude' for elevation and soil type extraction
    tmp <- c("site", "siteId", "latitude", "longitude")
    
    # save single record per site
    siteData <- carobCleaned %>%
      select(all_of(tmp)) %>%
      distinct(siteId, .keep_all = TRUE)
    
}

# integrate weather, elevation, and soil type data into Carob dataset
source("dataDownloader.R")

# impute missing rainfall and temperature values using "mice imputation" method (we missed some years of weather data)
tmp <- mice(carobCleaned, m = 15)  # create mice object with 15 imputations
carobCleaned <- complete(tmp)      # complete imputed dataset

# ------------------ check the resulting integrated dataset --------------------
tmp <- c("site", "N_fertilizer", "P_fertilizer", "K_fertilizer", "year", "yield", "yield_part", "t2m", "prec", "elevation", "soil_type")

# print out site data only once
siteData <- carobCleaned %>%
  select(all_of(tmp)) %>%
  distinct(site, .keep_all = TRUE)

# print the resulting dataframe
print(siteData)
# ----- the carobCleaned data should be complete and clean at this point -------

# ==================== building a predictive model =============================

# cast required data columns as factor to allow the model to learn region-specific effects (soil variations such as 'soil_type')
carobCleaned$site <- as.factor(carobCleaned$site)
carobCleaned$soil_type <- as.factor(carobCleaned$soil_type)

# build a 'random forest' model using the whole dataset
model <- randomForest(yield ~ site + elevation + N_fertilizer + P_fertilizer + K_fertilizer + soil_type + t2m + prec, data = carobCleaned, importance = TRUE)
#print(model)

# ------------------ model validation (cross-validation) -----------------------
# split data into training and test dataset
set.seed(123)
trainIndex <- sample(1:nrow(carobCleaned), 0.7 * nrow(carobCleaned))
trainData <- carobCleaned[trainIndex, ]
testData <- carobCleaned[-trainIndex, ]

# fit model on training data
valModel <- randomForest(yield ~ site + elevation + N_fertilizer + P_fertilizer + K_fertilizer + soil_type + t2m + prec, data = trainData)

# predict on test data
predictions <- predict(valModel, newdata = testData)

# check RMSE
rmse <- sqrt(mean((testData$yield - predictions)^2))
# print(rmse)

# ==================== site specific recommendations ===========================

# a list to collect 'site' predicted yields for later use
sitesPredictedYeild <- list()

# collect site names from 'Carob' dataset
siteNames <- c("University Farm, Ibadan","FOC University of Nigeria, Nsukka","IITA, Ibadan","University Farm, Nsukka",
               "Akure","Ado-Ekiti","Owerri","Abeokuta","Iburu"#,"Ibadan","Umudike","Unknown","Zaria"
               )
# calculate site specific NPK rates by varying one fertilizer while keeping the other two their mean values, and 
# find the optimal value for each fertilizer at specific site
for (i in 1:length(siteNames)){
    # filter data for a specific region
    specificRegionData <- subset(carobCleaned, site == siteNames[i])
    
    # calculate optimal N for the specific region
    simulatedData <- data.frame(
      N_fertilizer = seq(0, 200, by = 5),
      P_fertilizer = mean(specificRegionData$P_fertilizer),
      K_fertilizer = mean(specificRegionData$K_fertilizer),
      rainfall = mean(specificRegionData$rainfall),
      temperature = mean(specificRegionData$temperature),
      soil_type = unique(specificRegionData$soil_type),
      elevation = unique(specificRegionData$elevation),
      site = factor(siteNames[i], levels = levels(carobCleaned$site))
    )
    # predict yield for specified region with 'simulatedData' that created by varying N levels
    simulatedData$predictedYield <- predict(model, newdata = simulatedData)
    # find optimal N rates for the specified region
    optimalN <- simulatedData$N_fertilizer[which.max(simulatedData$predictedYield)]
    
    # calculate optimal P for the specific region
    simulatedData <- data.frame(
      P_fertilizer = seq(0, 200, by = 5),
      N_fertilizer = mean(specificRegionData$N_fertilizer),
      K_fertilizer = mean(specificRegionData$K_fertilizer),
      rainfall = mean(specificRegionData$rainfall),
      temperature = mean(specificRegionData$temperature),
      soil_type = unique(specificRegionData$soil_type),
      elevation = unique(specificRegionData$elevation),
      site = factor(siteNames[i], levels = levels(carobCleaned$site))
    )
    # predict yield for specified region with 'simulatedData' that created by varying P levels
    simulatedData$predictedYield <- predict(model, newdata = simulatedData)
    # find optimal P rates for the specified region
    optimalP <- simulatedData$P_fertilizer[which.max(simulatedData$predictedYield)]
    
    # calculate optimal K for the specific region
    simulatedData <- data.frame(
      K_fertilizer = seq(0, 200, by = 5),
      P_fertilizer = mean(specificRegionData$P_fertilizer),
      N_fertilizer = mean(specificRegionData$N_fertilizer),
      rainfall = mean(specificRegionData$rainfall),
      temperature = mean(specificRegionData$temperature),
      soil_type = unique(specificRegionData$soil_type),
      elevation = unique(specificRegionData$elevation),
      site = factor(siteNames[i], levels = levels(carobCleaned$site))
    )
    # predict yield for specified region with 'simulatedData' that created by varying K levels
    simulatedData$predictedYield <- predict(model, newdata = simulatedData)
    # find optimal K rates for the specified region
    optimalK <- simulatedData$K_fertilizer[which.max(simulatedData$predictedYield)]
    
    # print out optimal NPK for the specified site
    cat(sprintf("Optimal NPK rates for %33s is: N = %3d, P = %3d, K = %3d", siteNames[i], optimalN, optimalP, optimalK), "\n")
  }