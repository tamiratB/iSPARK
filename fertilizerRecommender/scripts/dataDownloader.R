# load needed packages
library(nasapower)
library(elevatr)
library(ncdf4)
library(dplyr)
library(httr)

if (dataDownload == TRUE){
    # set timeout period for longer to slow connections
    httr::set_config(config(timeout = 120)) # timeout to 120 seconds
    
    print("Sit back and relax; this may take some time depending on the size of your data.")
    
    # initialize columns for 't2m', 'prec', 'elevation', and 'soil_type'
    carobCleaned$t2m <- NA_real_
    carobCleaned$prec <- NA_real_
    carobCleaned$elevation <- NA_real_
    carobCleaned$soil_type <- NA_character_

    # column values needed to extract weather, elevation, and soil type data
    colValues <- c("year", "latitude","longitude")
    
    # read soil texture data
    soilTextureData <- nc_open("../data/GLDASp5_soiltexture_025d.nc4")
    # read soil texture code from the GLDAS dataset
    soilTexture <- ncvar_get(soilTextureData, "GLDAS_soiltex")
    
    # read latitude and longitude variables
    latitudes <- ncvar_get(soilTextureData, "lat")
    longitudes <- ncvar_get(soilTextureData, "lon")
    
    # prepare a soil type look up table for soil_type's based on GLDAS soil texture data (https://ldas.gsfc.nasa.gov/gldas/soils) 
    soilTypesMap <- data.frame(
      id = 1:16,
      soilTypes = c("Sand","Loamy Sand","Sandy Loam","Silt Loam","Silt","Loam","Sandy Clay Loam","Silty Clay Loam",
                    "Clay Loam","Sandy Clay","Silty Clay","Clay","Organic Materials","Water","Bedrock","Other")
    )
    
    # retrieve elevation and soil type data
    print("Retrieving elevation data...")
    for (i in 1:nrow(siteData)) {
        # pick the current row latitude and longitude values
        rowValues <- siteData[i, colValues]
        lat <- rowValues[[2]]
        lon <- rowValues[[3]]
        
        # ---------------retrieve elevation data from SRTM -------------------------
        coords <- data.frame(x = lat, y = lon)
        srtmElev <- get_elev_point(locations = coords, prj = "+proj=longlat +datum=WGS84", src = "aws")

        # assign the elevation to all records at this latitude and longitude
        carobCleaned$elevation[carobCleaned$latitude == lat & carobCleaned$longitude == lon] <- srtmElev$elevation

        # ---------------- extract soil texture data -------------------------------
        # search the closest grid point in the soil texture data
        latIdx <- which.min(abs(latitudes - lat))
        lonIdx <- which.min(abs(longitudes - lon))
        
        # extract the soil texture data for the closest grid point
        soilType <- soilTexture[lonIdx, latIdx]
        # look up soil type from 'soilTypesMap'
        soilType <- soilTypesMap$soilTypes[soilTypesMap$id == as.integer(soilType)]
        # insert soil type data into carobCleaned
        carobCleaned$soil_type[carobCleaned$latitude == lat & carobCleaned$longitude == lon] <- soilType
    }
    print("Elevation data downloading completed!")
    
    #------------------ function to retrieve weather data-------------------------
    getClimData <- function(year, latitude, longitude) {
      # define the date range for data
      startDate <- paste0(year)
      endDate <- paste0(year+1)
      
      # get climate data from NASA POWER for the specified point location and year
      climateData <- get_power(
        community = "AG",
        lonlat = c(longitude, latitude),
        pars = c("T2M", "PRECTOTCORR"),
        dates = c(startDate, endDate),
        temporal_api = "monthly"
      )
      
      return(climateData)
    }
    # ----------------------------------------------------------------------------
    
    # select a growing season (in this case, MAY to OCTOBER )
    season <- c("MAY", "JUN", "JUL", "AUG", "SEP", "OCT")
    # number of days in each month (used to convert precipitation data into 'mm' of the season, it is given in 'mm/day' from NASA POWER)
    numDays <- c(MAY=31, JUN=30, JUL=31, AUG=31, SEP=30, OCT=31)
    print("Retrieving weather data...")

    # retrieve weather data
    for (i in 1:nrow(carobCleaned)) {
        cat(sprintf("Currently at row: %d of %d", i, nrow(carobCleaned)))

        # pick the current row year, latitude, longitude values
        rowValues <- carobCleaned[i, colValues]
        year <- rowValues[[1]]
        lat <- rowValues[[2]]
        lon <- rowValues[[3]]
        
        # ------------- retrieve weather data from NASA POWER ----------------------
        if(year < 1981){next} # no weather data before 1981 from NASA POWER, skip them 
        
        climate <- getClimData(year, lat, lon)
        
        # select rows by variable from weather data
        t2m <- climate %>% filter(PARAMETER == "T2M")
        prec <- climate %>% filter(PARAMETER == "PRECTOTCORR")
        
        # calculate the row-wise average and add it as a new column (i.e., 'mean_t2m') for temperature data
        t2m$mean_t2m <- rowMeans(t2m[, season], na.rm = TRUE) 

        # calculate the row-wise weighted sum and add it as a new column (i.e., weightedSumPrec) for precipitation
        prec <- prec %>%
          rowwise() %>%
          mutate(weightedSumPrec = sum(c_across(MAY:OCT) * c(numDays['MAY'], numDays['JUN'], numDays['JUL'], 
                                                             numDays['AUG'], numDays['SEP'], numDays['OCT'])))  
        
        # insert t2m and prec data into carobCleaned dataframe
        carobCleaned$t2m[carobCleaned$year == year & carobCleaned$latitude == lat & carobCleaned$longitude == lon] <- 
          t2m$mean_t2m[t2m$YEAR == year & t2m$PARAMETER == 'T2M']
        carobCleaned$prec[carobCleaned$year == year & carobCleaned$latitude == lat & carobCleaned$longitude == lon] <- 
          prec$weightedSumPrec[prec$YEAR == year & prec$PARAMETER == 'PRECTOTCORR']
    }
    print("Weather data downloading completed!")
    # this is expensive operation, save data for later use
    saveRDS(carobCleaned, file="../data/carobCleaned.rds")
  
} else{
    print("Reading data from data/carobCleaned.rds")
    carobCleaned <- readRDS("../data/carobCleaned.rds")
}
