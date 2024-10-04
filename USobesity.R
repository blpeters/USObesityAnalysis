library(prism)
library(raster)
library(sf)
library(dplyr)

# Using PRISM package, download the max temps in July for 30 years (1981-2010 per docs)
prism_set_dl_dir('C:/Users/brett/OneDrive/Documents/Course_Materials/MSU_GDAV_Certificate/SPST/Activity14/data')
get_prism_normals(type = 'tmax', resolution = '4km', mon = 7, keepZip = TRUE)

# Open July temperature data from PRISM
tmaxasc <- pd_to_file(prism_archive_ls()) # asc is for Esri ASCII format of raster
tmax <- raster(tmaxasc) # convert format to raster
plot(tmax)

# Get counties shapefile previously downloaded in activity 10
counties <- st_read('C:/Users/brett/OneDrive/Documents/Course_Materials/MSU_GDAV_Certificate/SPST/Activity10/tl_2022_us_county')
plot(counties$geometry)

# clean up counties to include full FIPS and geometry
counties$FIPS <- paste0(counties$STATEFP, counties$COUNTYFP)
countiestidy <- counties %>% select(FIPS, geometry)

USdem <- raster('data/NAdem.tif') # DEM elevation raster
plot(USdem)
USdata <- st_read('data/inc_obes.gpkg') # Obesity and income data

# Need to add raster data to the USdata set - aggregate (average) the raster 
# values by the polygons defined in USdata using the raster extract() fun.
USdata$tmax <- extract(tmax, USdata, fun = mean, na.rm = TRUE)
USdata$elev <- extract(USdem, USdata, fun = mean, na.rm = TRUE)
