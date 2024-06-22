rm(list=ls()) # cleaning the environment
gc() # clearing memory

library(dplyr)
#library(mgcv)
#library(e1071)
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
dataDownload = FALSE

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
    tmp <- c("site", "year", "siteId", "latitude", "longitude")
   
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

# clean any record remains with NAs or empty strings 
# some records have wrong latitude and longitude like lat=3.43, lon=6.45, it is in the Atlantic ocean
carobCleaned <- na.omit(carobCleaned)

# ------------------ check the resulting integrated dataset --------------------
tmp <- c("site", "N_fertilizer", "P_fertilizer", "K_fertilizer", "year", "yield", "yield_part", "t2m", "prec", "elevation", "soil_type")

# print out site data only once
siteData <- carobCleaned %>%
  select(all_of(tmp)) %>%
  distinct(site, .keep_all = TRUE)

# printout the integrated data
print(siteData)

# ----- the carobCleaned data should be complete and clean at this point -------

# ==================== building a predictive model =============================

# reduce the number of categories in 'site' column, random forest does not handle more than 53 categories
# count each category
numCounts <- table(carobCleaned$site)
# keep categories with more than 30 occurrences
threshold <- 25
# create a new variable where infrequent categories are labeled as 'Others'
carobCleaned$reducedSites <- ifelse(numCounts[carobCleaned$site] > threshold, as.character(carobCleaned$site), "Others")
# cast required data columns as factor to allow the model to learn region-specific effects (soil variations such as 'soil_type')
# and use 'reducedSites'
carobCleaned$reducedSites <- factor(carobCleaned$reducedSites)
carobCleaned$soil_type <- as.factor(carobCleaned$soil_type)

# build a 'random forest' model using the whole dataset
model <- randomForest(yield ~ reducedSites + elevation + N_fertilizer + P_fertilizer + K_fertilizer + soil_type + t2m + prec, data = carobCleaned, importance = TRUE)
#print(model)

# ------------------ model validation (cross-validation) -----------------------
# split data into training and test dataset
set.seed(123)
trainIndex <- sample(1:nrow(carobCleaned), 0.7 * nrow(carobCleaned))
trainData <- carobCleaned[trainIndex, ]
testData <- carobCleaned[-trainIndex, ]

# fit model on training data
valModel <- randomForest(yield ~ reducedSites + elevation + N_fertilizer + P_fertilizer + K_fertilizer + soil_type + t2m + prec, data = trainData)

# predict on test data
predictions <- predict(valModel, newdata = testData)

# check RMSE
rmse <- sqrt(mean((testData$yield - predictions)^2))
# print(rmse)

# ==================== site specific recommendations ===========================

# select unique names from 'reducedSites' and remove 'Others' since it belongs to many locations
siteNames <- carobCleaned %>% distinct(reducedSites) %>% filter((reducedSites != "Others"))
siteNames <- as.character(siteNames$reducedSites)

# calculate site specific NPK rates by varying one fertilizer while keeping the other two their mean values, and 
# find the optimal value for each fertilizer at specific site
fertilizers <- c("N_fertilizer","P_fertilizer","K_fertilizer")
iterLength <- seq(0, 200, by = 5)

for (i in 1:length(siteNames)){
    # filter data for a specific region
    specificRegionData <- subset(carobCleaned, reducedSites == siteNames[i])
    for (vars in fertilizers){ 
        if (vars == "N_fertilizer"){
            N = iterLength
            P = rep(mean(specificRegionData$P_fertilizer),length(iterLength))
            K = rep(mean(specificRegionData$K_fertilizer),length(iterLength))
        }else if(vars == "P_fertilizer"){
            P = iterLength
            N = rep(mean(specificRegionData$N_fertilizer),length(iterLength))
            K = rep(mean(specificRegionData$K_fertilizer),length(iterLength))
        } else{
            K = iterLength
            P = rep(mean(specificRegionData$P_fertilizer),length(iterLength))
            N = rep(mean(specificRegionData$N_fertilizer),length(iterLength))
        }
        # calculate optimal N, P, and K for the specific region
        simulatedData <- data.frame(
          N_fertilizer = N,
          P_fertilizer = P,
          K_fertilizer = K,
          prec = rep(mean(specificRegionData$prec),length(iterLength)),
          t2m = rep(mean(specificRegionData$t2m),length(iterLength)),
          soil_type = rep(unique(specificRegionData$soil_type),length(iterLength)),
          elevation = rep(unique(specificRegionData$elevation),length(iterLength)),
          reducedSites = rep(factor(siteNames[i], levels = levels(carobCleaned$reducedSites)),length(iterLength))
        )
        # predict yield for specified region with 'simulatedData' 
        simulatedData$predictedYield <- predict(model, newdata = simulatedData)
        # find optimal N, P, and K rates for the specified region
        if (vars == "N_fertilizer"){
            optimalN <- simulatedData$N_fertilizer[which.max(simulatedData$predictedYield)]
        }else if(vars == "P_fertilizer"){
            optimalP <- simulatedData$P_fertilizer[which.max(simulatedData$predictedYield)]
        } else{
            optimalK <- simulatedData$K_fertilizer[which.max(simulatedData$predictedYield)]
        }
    }
    # print out optimal NPK for the specified site
    cat(sprintf("Optimal NPK rates for %33s is: N = %3d, P = %3d, K = %3d", siteNames[i], optimalN, optimalP, optimalK), "\n")
}
