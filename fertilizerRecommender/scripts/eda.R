library(ggplot2)
library(dplyr)

# ---------- visualize sites information and spatial distribution --------------

# printout 'site', 'latitude', and 'longitude' data to check their location
tmp <- c("site", "latitude", "longitude")

# print out site location data
siteData <- carobCleaned %>%
  select(all_of(tmp)) %>%
  distinct(site, .keep_all = TRUE)

# print the resulting dataframe
print(siteData)

# create a leaflet map
map <- leaflet(siteData) %>%
  addTiles() %>%
  setView(lng = mean(siteData$longitude), lat = mean(siteData$latitude), zoom = 5)

# add markers for each location with popup labels (popup site names)
map <- map %>%
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~site)

# display the map
map

# check rainfall variability
png(file = paste0("../plots/rainfall_variability.png"), width = 300, height = 250, units="mm", res=300, pointsize = 20)
par(mar = c(8, 4, 4, 2) + 0.1)
boxplot(carobCleaned$prec ~ carobCleaned$reducedSites, xlab="", ylab = "Rainfall (mm)",
        main = "Rainfall by Site", xaxt = "n")
text(x = 1:length(unique(carobCleaned$reducedSites)), 
     y = par("usr")[3] - 0.5, 
     srt = 45, 
     adj = 1, 
     labels = unique(carobCleaned$reducedSites), 
     xpd = TRUE,
     cex = 0.6)
dev.off()

# check temperature variability
png(file = paste0("../plots/temperature_variability.png"), width = 300, height = 250, units="mm", res=300, pointsize = 20)
par(mar = c(10, 4, 4, 2) + 0.1)
boxplot(carobCleaned$t2m ~ carobCleaned$reducedSites, xlab="", ylab = expression(Temperature~(degree*C)),
        main = "Temperature by Site", xaxt = "n")
text(x = 1:length(unique(carobCleaned$reducedSites)), 
     y = par("usr")[3] - 0.5, 
     srt = 45, 
     adj = 1, 
     labels = unique(carobCleaned$reducedSites), 
     xpd = TRUE,
     cex = 0.6)
dev.off()

# check yield variability 
png(file = paste0("../plots/yieldBoxplots.png"), width = 300, height = 250, units="mm", res=300, pointsize = 20)
    par(mar = c(10, 4, 4, 2) + 0.1)
    boxplot(carobCleaned$yield ~ carobCleaned$reducedSites, xlab="", ylab = "Yield",
            main = "Yield by Site", xaxt = "n")
    text(x = 1:length(unique(carobCleaned$reducedSites)), 
         y = par("usr")[3] - 0.5, 
         srt = 45, 
         adj = 1, 
         labels = unique(carobCleaned$reducedSites), 
         xpd = TRUE,
         cex = 0.6)
dev.off()

# --- correlation, heatmaps, pairewise analysis among fertilizers and yield ----

# do correlation analysis
selectedColumns <- carobCleaned %>%
  select(N_fertilizer, P_fertilizer, K_fertilizer, yield, prec, t2m, elevation)
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
png(file = paste0("../plots/pairesPlot.png"), width = 350, height = 250, units="mm", res=300, pointsize = 20)
    pairs(~ yield + N_fertilizer + P_fertilizer + K_fertilizer + prec + t2m + elevation, data = carobCleaned)
dev.off()

# plot variable importance
png(file = "../plots/variablesImportance.png", width = 300, height = 200, units="mm", res=300, pointsize = 20)
    varImpPlot(model)
dev.off()

# -------------------------- Fertilizers Vs yield --------------------------------
variables <- c("N_fertilizer","P_fertilizer","K_fertilizer")

for (vars in colnames(carobCleaned)){ 
  if (vars %in% variables){
    # yield against each factor
    png(file = paste0("../plots/", vars, "_Vs_Yield.png"), width = 300, height = 200, units="mm", res=300, pointsize = 20)
    p <- ggplot(carobCleaned, aes(x = carobCleaned[[vars]], y = yield)) + geom_point() + geom_smooth(method = "lm") +
      labs(title = "Fertilizer usage and corresponding yield", x = vars, y = "Yield") + theme_minimal() 
    print(p)
    dev.off()
    
    # partial dependence for a specific predictor
    png(file = paste0("../plots/", vars, "_PDP.png"), width = 300, height = 200, units="mm", res=300, pointsize = 20)
    # partialPlot(model, carobCleaned, as.name(vars))
    do.call(partialPlot, list(model, carobCleaned, vars))
    dev.off()
  }
}
