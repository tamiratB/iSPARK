rm(list=ls()) # cleaning the environment
gc() #clearing memory

library(ggplot2)
library(dplyr)
#library(mgcv)
#library(e1071)
library(leaflet)
library(tidyr)
library(randomForest)
library(mice)
library(reshape2)
library(kableExtra)

#-----------------define working directory--------------------------------------

scriptDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(scriptDir)

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
# cast 'planting_date' as numeric, and assign '9999' for missing 'planting_date'
carobCleaned$planting_date <- substr(carobCleaned$planting_date, 1, 4)
carobCleaned$planting_date <- as.numeric(carobCleaned$planting_date)
carobCleaned["planting_date"][is.na(carobCleaned["planting_date"])] <- 9999

# replace missing 'site', and 'soil_type' with 'Unknown' value
carobCleaned$site[carobCleaned$site == ""] <- "Unknown"
carobCleaned$soil_type[carobCleaned$soil_type == ""] <- "Unknown"

# add missing 'elevation' data from NASA POWER dataset
carobCleaned$elevation[carobCleaned$site == "Unknown"] <- 519.33 
carobCleaned$elevation[carobCleaned$site == "Iburu"] <- 623.54
carobCleaned$elevation[carobCleaned$site == "Zaria"] <- 646.9
carobCleaned$elevation[carobCleaned$site == "Umudike"] <- 49.45

# count missing values (NA and empty character "") in each column and drop columns having
# large number of missing values
threshold <- 310 #threshold for missing values more than %5 of the total data (i.e., 6200)

# filter columns (drop NAs and empty character (i.e., "") counts more than the threshold) 
carobCleaned <- carobCleaned %>%
  select(where(~ sum(is.na(.)) <= threshold)) %>%  # NAs
  select(where(~ sum(as.character(.) == "", na.rm=TRUE) <= threshold)) # empty character

# ----------------------- read weather dataset ---------------------------------

# weather datasets from NASA POWER (https://power.larc.nasa.gov/data-access-viewer/)
weatherData <- c("Abeokuta", "Ado-Ekiti", "Akure", "FOC-University-of-Nigeria-Nsukka", "Ibadan", "Iburu", "Owerri", "Umudike", "University-Farm-Nsukka", "Unknown", "Zaria")

# set a growing season (in this case, MAY to OCTOBER )
season <- c("MAY", "JUN", "JUL", "AUG", "SEP", "OCT")
# number of days in each month (used to convert precipitation data into 'mm' of the season, it is given in 'mm/day' from NASA POWER)
numDays <- c(MAY=31, JUN=30, JUL=31, AUG=31, SEP=30, OCT=31)

for (weather in weatherData)
{
  siteWeather <- read.csv(paste0("../data/", weather, "_weather.csv"), skip = 12) # read weather data one-by-one
  # select rows by variable from weather data
  t2m <- siteWeather %>% filter(PARAMETER == "T2M")
  prec <- siteWeather %>% filter(PARAMETER == "PRECTOTCORR")
  
  # calculate the row-wise average and add it as a new column (i.e., 't2mAverage') for temperature data
  t2m <- t2m %>%
    mutate(t2mAverage = rowMeans(select(., all_of(season)), na.rm = TRUE))
  
  # calculate the row-wise weighted sum and add it as a new column (i.e., weightedSumPrec) for precipitation data
  prec <- prec %>%
    rowwise() %>%
    mutate(weightedSumPrec = sum(c_across(MAY:OCT) * c(numDays['MAY'], numDays['JUN'], numDays['JUL'], 
                                                       numDays['AUG'], numDays['SEP'], numDays['OCT'])))
  
  # select 'weightedSumPrec' from 'prec' and join with its corresponding 't2mAverage' in 't2m' for same site
  if (weather == "Abeokuta"){ # needed for the first site and the rest will be join to the left of this dataframe
    # keep calculated t2mAverage in temporary dataframe
    tmp <- t2m %>%
      select(YEAR, t2mAverage)
    # join the first 't2m' and 'prec' data in one dataframe by 'year'
    allSitesWeatherData <- tmp %>%
      left_join(prec %>% select(YEAR, weightedSumPrec), by = 'YEAR')
  }else{
    # join the rest of the other site's 't2m' and 'prec' data in one dataframe (i.e., 'allSitesWeatherData')
    allSitesWeatherData <- allSitesWeatherData %>%
      left_join(t2m %>% select(YEAR, t2mAverage), by = 'YEAR')
    allSitesWeatherData <- allSitesWeatherData %>%
      left_join(prec %>% select(YEAR, weightedSumPrec), by = 'YEAR') 
  }
}
# rename column names for the big dataframe 'allSitesWeatherData' that collected all sites weather data
colNames <- c("year", "Abeokuta_t2m", "Abeokuta_prec", "Ado-Ekiti_t2m", "Ado-Ekiti_prec", "Akure_t2m", "Akure_prec", 
              "FOC-University-of-Nigeria-Nsukka_t2m", "FOC-University-of-Nigeria-Nsukka_prec", "Ibadan_t2m", "Ibadan_prec",
              "Iburu_t2m", "Iburu_prec", "Owerri_t2m", "Owerri_prec", "Umudike_t2m", "Umudike_prec", "University-Farm-Nsukka_t2m", 
              "University-Farm-Nsukka_prec", "Unknown_t2m", "Unknown_prec", "Zaria_t2m", "Zaria_prec")
allSitesWeatherData <- allSitesWeatherData %>% 
  rename_with(~ colNames)

# ------------------ integrate weather data into Carob dataset -----------------

# first rename column 'planting_date' to 'year' for 'carobCleaned' dataframe to join it with 'allSitesWeatherData' dataframe by 'year'
# and cast the new 'year' column to numeric
carobCleaned <- carobCleaned %>%
  rename(year = planting_date)
carobCleaned$year <- as.numeric(carobCleaned$year)

# group 'carobCleaned' data by site and split each group (i.e., into individual site)
carobCleanedGrouped <- carobCleaned %>%
  group_by(site) %>%
  group_split()

# check point
#print(head(carobCleaned))
#print(str(carobCleaned))

# prepare empty list (i.e., 'integratedSitesData') to collect all sites integrated data 
weatherDataNames <- c("Abeokuta", "Ado-Ekiti", "Akure", "FOC-University-of-Nigeria-Nsukka", "Ibadan", "Ibadan", "Iburu", "Owerri", "Umudike", "Ibadan", "University-Farm-Nsukka", "Unknown", "Zaria")
integratedSitesData <- list()

# iterate through 'carobCleaned' and 'allSitesWeatherData', and integrate them together and collect output to 'integratedSitesData' list
for (i in 1:13){
# select 'site', 'year', 'N_fertilizer',... from 'carobCleaned' and join with 'allSitesWeatherData' based on 'year'
integratedSitesData[[i]] <- carobCleanedGrouped[[i]] %>%
  select(year,site,elevation,yield,N_fertilizer,P_fertilizer,K_fertilizer,soil_type) %>% # select required columns for further analysis
  left_join(allSitesWeatherData[,c('year', paste0(weatherDataNames[i],'_t2m'),paste0(weatherDataNames[i], '_prec'))], by = "year") %>% # join with 'allSitesWeatherData' based on year
  rename(rainfall = paste0(weatherDataNames[i],'_prec')) %>%
  rename(temperature = paste0(weatherDataNames[i],'_t2m'))
}

# convert 'integratedSitesData' list into dataframe (conversion 'list' to 'dataframe' by appending)
wholeIntegratedSitesData <- bind_rows(integratedSitesData)

# impute missing N, P, K, rainfall and temperature data using "mice imputation" method
tmp <- mice(wholeIntegratedSitesData, m = 15)  # create mice object with 15 imputations
wholeIntegratedSitesData <- complete(tmp)  # complete imputed dataset

# ================================== EDA =======================================

# ---------- visualize sites information and spatial distribution --------------

# printout 'site', 'latitude', and 'longitude' data to check their location
tmp <- c("site", "latitude", "longitude")

# print out site data only once
siteData <- carobCleaned %>%
  select(all_of(tmp)) %>%
  distinct(latitude, .keep_all = TRUE)

# print the resulting dataframe
print(siteData)

# ----- display the spatial distributions of these sites using leaflet map -----

# create a leaflet map
map <- leaflet(siteData) %>%
  addTiles() %>%
  setView(lng = mean(siteData$longitude), lat = mean(siteData$latitude), zoom = 5)

# add markers for each location with popup labels (popup site names)
map <- map %>%
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~site)

# display the map
map

# ---------- historical rainfall and temperature (1981-2022) plots ------------- 

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8")

# select temperature data from 'allSitesWeatherData' dataframe and plot
png(file = paste0("../plots/historicalTemp.png"), width = 300, height = 200, units="mm", res=300, pointsize = 20)
  temperature <- allSitesWeatherData %>%
    pivot_longer(cols = c('Abeokuta_t2m', 'Ado-Ekiti_t2m', 'Akure_t2m', 'FOC-University-of-Nigeria-Nsukka_t2m', 'Ibadan_t2m', 'Iburu_t2m', 'Owerri_t2m',
                          'Umudike_t2m', 'University-Farm-Nsukka_t2m', 'Unknown_t2m', 'Zaria_t2m'), names_to = "variable", values_to = "value")
  ggplot(temperature, aes(x=year, y=value, color = variable)) + geom_line(linewidth=1) +
    labs(title = "Temperature (1981-2022)", x = "Year", y = "Temeperature (degC)") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Sites") +
    scale_color_manual(values = colors)
dev.off()

# select rainfall data from 'allSitesWeatherData' dataframe and plot
png(file = paste0("../plots/historicalPrecip.png"), width = 300, height = 200, units="mm", res=300, pointsize = 20)
  precipitation <- allSitesWeatherData %>%
    pivot_longer(cols = c('Abeokuta_prec', 'Ado-Ekiti_prec', 'Akure_prec', 'FOC-University-of-Nigeria-Nsukka_prec', 'Ibadan_prec', 'Iburu_prec', 'Owerri_prec',
                          'Umudike_prec', 'University-Farm-Nsukka_prec', 'Unknown_prec', 'Zaria_prec'), names_to = "variable", values_to = "value")
  ggplot(precipitation, aes(x=year, y=value, color = variable)) + geom_line(linewidth=1) +
    labs(title = "Precipitation (1981-2022)", x = "Year", y = "Precipitation (mm)") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + labs(color = "Sites") +
    scale_color_manual(values = colors)
dev.off()
# -------------------- yield variability across sites --------------------------

# boxplot to check for yield variability and outliers
png(file = paste0("../plots/yieldBoxplots.png"), width = 300, height = 250, units="mm", res=300, pointsize = 20)
par(mar = c(10, 4, 4, 2) + 0.1)
boxplot(wholeIntegratedSitesData$yield ~ wholeIntegratedSitesData$site, xlab="", ylab = "Yield",
        main = "Yield by Site", xaxt = "n")
text(x = 1:length(unique(wholeIntegratedSitesData$site)), 
     y = par("usr")[3] - 0.5, 
     srt = 45, 
     adj = 1, 
     labels = unique(wholeIntegratedSitesData$site), 
     xpd = TRUE)
dev.off()

# --- correlation, heatmaps, pairewise analysis among fertilizers and yield ----

# select 'N_fertilizer', 'P_fertilizer', 'K_fertilizer', and 'yield'  columns to do correlation analysis
selectedColumns <- wholeIntegratedSitesData %>%
  select(N_fertilizer, P_fertilizer, K_fertilizer, yield, rainfall, temperature, elevation)
  print("Correlation among variables:\n")
  cor(selectedColumns)

# heatmaps to visualize correlations
png(file = paste0("../plots/corrHeatmaps.png"), width = 300, height = 200, units="mm", res=300, pointsize = 20)
corr <- melt(cor(selectedColumns))
p <- ggplot(data = corr, aes(x = Var1, y = Var2, fill = value)) + 
      geom_tile() + labs(x = "", y = "", fill = "Correlation") +
      scale_fill_gradient2()
print(p)
dev.off()

# pairwise plots to examine relationships across variables
png(file = paste0("../plots/pairesPlot.png"), width = 300, height = 200, units="mm", res=300, pointsize = 20)
  pairs(~ yield + N_fertilizer + P_fertilizer + K_fertilizer + rainfall + temperature, data = wholeIntegratedSitesData)
dev.off()

# display yield against each factor
# ggplot(wholeIntegratedSitesData, aes(x = N_fertilizer, y = yield)) + geom_point() + geom_smooth(method = "lm")
# ggplot(wholeIntegratedSitesData, aes(x = P_fertilizer, y = yield)) + geom_point() + geom_smooth(method = "lm")
# ggplot(wholeIntegratedSitesData, aes(x = K_fertilizer, y = yield)) + geom_point() + geom_smooth(method = "lm")
# ggplot(wholeIntegratedSitesData, aes(x = rainfall, y = yield)) + geom_point() + geom_smooth(method = "lm")
# ggplot(wholeIntegratedSitesData, aes(x = temperature, y = yield)) + geom_point() + geom_smooth(method = "lm")


# ==================== building predictive a model =============================

# cast required data columns as factor to allow the model to learn region-specific effects (soil variations such as 'soil_type')
wholeIntegratedSitesData$site <- as.factor(wholeIntegratedSitesData$site)
wholeIntegratedSitesData$soil_type <- as.factor(wholeIntegratedSitesData$soil_type)

# build a 'random forest' model using the whole dataset
model <- randomForest(yield ~ site + elevation + N_fertilizer + P_fertilizer + K_fertilizer + soil_type + temperature + rainfall, data = wholeIntegratedSitesData, importance = TRUE)
#print(model)

# display variable importance
png(file = "../plots/variablesImportance.png", width = 300, height = 200, units="mm", res=300, pointsize = 20)
  varImpPlot(model)
dev.off()

# plot partial dependence for a specific predictor

png(file = paste0("../plots/N_fertilizer_PDP.png"), width = 300, height = 200, units="mm", res=300, pointsize = 20)
  partialPlot(model, wholeIntegratedSitesData, "N_fertilizer")
dev.off()


# --------------------- model validation (cross-validation)-----------------------
# split data into training and test datasets
set.seed(123)
trainIndex <- sample(1:nrow(wholeIntegratedSitesData), 0.7 * nrow(wholeIntegratedSitesData))
trainData <- wholeIntegratedSitesData[trainIndex, ]
testData <- wholeIntegratedSitesData[-trainIndex, ]

# fit model on training data
valModel <- randomForest(yield ~ site + elevation + N_fertilizer + P_fertilizer + K_fertilizer + soil_type + temperature + rainfall, data = trainData)

# predict on test data
predictions <- predict(valModel, newdata = testData)

# check RMSE
rmse <- sqrt(mean((testData$yield - predictions)^2))
# print(rmse)



# ==================== site specific recommendations ===========================

# a list to collect 'site' predicted yields for later use
sitesPredictedYeild <- list()

# collect site names from 'carob' dataset
siteNames <- c("University Farm, Ibadan","FOC University of Nigeria, Nsukka","IITA, Ibadan","University Farm, Nsukka",
               "Akure","Ado-Ekiti","Owerri","Abeokuta","Iburu"#,"Ibadan","Umudike","Unknown","Zaria"
               )
# calculate site specific NPK rates by varying one fertilizer while keeping the other two their mean values, and 
# find the optimal value for each fertilizer at specific site
for (i in 1:length(siteNames)){
    # filter data for a specific region
    specificRegionData <- subset(wholeIntegratedSitesData, site == siteNames[i])
    
    # calculate optimal N for the specific region
    simulatedData <- data.frame(
      N_fertilizer = seq(0, 200, by = 5),
      P_fertilizer = mean(specificRegionData$P_fertilizer),
      K_fertilizer = mean(specificRegionData$K_fertilizer),
      rainfall = mean(specificRegionData$rainfall),
      temperature = mean(specificRegionData$temperature),
      soil_type = unique(specificRegionData$soil_type),
      elevation = unique(specificRegionData$elevation),
      site = factor(siteNames[i], levels = levels(wholeIntegratedSitesData$site))
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
      site = factor(siteNames[i], levels = levels(wholeIntegratedSitesData$site))
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
      site = factor(siteNames[i], levels = levels(wholeIntegratedSitesData$site))
    )
    # predict yield for specified region with 'simulatedData' that created by varying K levels
    simulatedData$predictedYield <- predict(model, newdata = simulatedData)
    # find optimal K rates for the specified region
    optimalK <- simulatedData$K_fertilizer[which.max(simulatedData$predictedYield)]
    
    # print out optimal NPK for specified site
    cat(sprintf("Optimal NPK rates for %33s is: N = %3d, P = %3d, K = %3d", siteNames[i], optimalN, optimalP, optimalK), "\n")
  }