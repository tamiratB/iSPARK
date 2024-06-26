## Climate-smart fertilizer recommendation
Climate-smart fertilizer recommendations tailored to specific sites have become a fundamental concern for achieving optimal productivity and environmental protection. This work aims to promote a balanced optimal fertilizer application, maximum yield achievement, and minimal environmental disruption. By integrating advanced data analytics and machine learning algorithms, this reusable script provides precise, site-specific fertilizer advice that enhances crop productivity while promoting sustainable agricultural practices. 

The provided script enables us to generate comprehensive climate-smart fertilizer recommendations tailored to specific sites. This script is built using the Random Forest machine learning algorithm and has been tested with Carob's standardized agricultural research data and weather datasets from NASA POWER.

The climate-smart fertilizer recommender script is developed using R programming and its extensive libraries. While it utilizes R base packages, the `dplyr` package is extensively employed for data manipulation. The following R packages are essential for the script, and it depends entirely on them:

- `randomForest`: for implementing the Random Forest algorithm
- `dplyr`: for efficient data manipulation and transformation
- `ggplot2`: for data visualization
- `mice`: for multivariate imputation by chained equations
- `leaflet`: for spatial data visualization
- `nasapower`: to download rainfall and temperature data from NASA POWER
- `elevatr`: to retrieve elevation data from SRTM
- `ncdf4`: for reading and extracting soil texture data from netCDF file 

Using these packages, the script can process large datasets, handle missing data, and generate precise fertilizer recommendations that consider site-specific environmental and geographic conditions. 

The script can be executed within RStudio, providing an interactive development environment for users who prefer a graphical interface. Alternatively, it can be run as a standalone script from the command line, offering flexibility for automation and integration into larger workflows. This dual compatibility enables that the script is accessible to a wide range of users, from those who favor a traditional IDE to those who need to incorporate it into automated processes or server-side applications.

## How to run the script
This R script is tested for maize crops in Nigeria based on the Carob dataset (https://carob-data.org/download.html). However, it can be customized for other countries and crops. The main script is `fertilizerRecommender.R`, which calls two other scripts: `dataDownloader.R` and `eda.R`, as needed.

The `dataDownloader.R` script retrieves rainfall, temperature, and elevation datasets, and extracts soil texture data from the 'GLDASp5_soiltexture_025d.nc4' dataset available at https://ldas.gsfc.nasa.gov/gldas/soils. The GLDAS soil texture dataset is also provided in the 'data' directory for offline use.

The `eda.R` script performs exploratory data analysis and generates plots. It should be run after the main `fertilizerRecommender.R` script completes. The `dataDownloader.R` script is called by the main script if the environmental variable `dataDownload` is set to `TRUE`. Otherwise, previously downloaded and saved data will be loaded from 'data/carobCleaned.rds'.

To test this script, download the Carob dataset, extract it into the 'data' directory along with the soil texture data, and run the main script.

# Sample output
The following is the sample fertilizer recommendation output for few locations from Carob dataset.
| Sites                                 | N   | P  | K  |
|-------------------------|-----|----|----|
| Zaria                                 | 155 | 60 | 25 |
| IITA, Ibadan                      |  55 | 20 | 55 |
| University Farm, Nsukka | 130 | 20 |  0 |
| Ibadan                              | 170 | 25 |  0 |
| Iburu                                 | 155 | 65 | 25 |
| unknownSite14           | 140 | 65 | 80 |
| unknownSite22           | 130 | 20 | 80 |
| unknownSite38           | 130 | 20 | 80 |
| unknownSite42           | 130 | 20 | 70 |
| unknownSite278          |  70 | 15 | 20 |
| unknownSite287          | 185 | 65 | 25 |
| unknownSite288          |  10 | 15 | 25 |
| unknownSite289          |  85 | 55 | 25 |
| unknownSite290          |  70 | 65 | 25 |
| unknownSite317          |  60 | 25 |  0 |
| unknownSite322          | 155 | 65 | 30 |
| unknownSite325          | 170 | 15 | 80 |
| unknownSite326          | 170 | 50 | 80 |
| unknownSite327          | 165 | 65 | 25 |
| unknownSite328          | 170 | 15 | 80 |
| unknownSite330          | 155 | 15 | 80 |
| unknownSite359          | 140 | 25 | 55 |
| unknownSite361          | 170 | 25 | 20 |
| unknownSite379          |  80 | 65 | 80 |
| unknownSite388          |  75 | 15 | 25 |
| unknownSite389          | 170 | 15 | 25 |
| unknownSite390          |  80 | 15 | 80 |
| unknownSite408          |  55 | 65 |  0 |
| unknownSite427          |  75 | 15 |  0 |
| unknownSite428          |  75 | 15 | 25 |
| unknownSite431          |  75 | 15 | 20 |
| unknownSite450          |  80 | 65 | 80 |
| unknownSite456          | 170 | 15 | 25 |
| unknownSite481          | 100 | 65 | 80 |
| Umudike                 | 155 |  5 | 55 |
