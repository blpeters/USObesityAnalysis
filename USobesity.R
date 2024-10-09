library(prism)
library(raster)
library(sf)
library(sfdep)
library(spdep)
library(spatialreg)
library(dplyr)
library(ggplot2)
library(viridis)

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

ggplot() + 
  geom_sf(data = USdata,
          aes(geometry = geom, fill = elev)) +
  coord_sf(crs = 4269, xlim = c(-125,-66), ylim = c(24,50)) +
  scale_fill_viridis(direction = 1, option = "turbo")

# Rename complicated variables
USdata <- USdata %>% rename(medinc = estimate, ob_pct = usobesity_Obesitypct,
                            inac_pct = usobesity_Inactivepct, fid = usobesity_fid,
                            state = usobesity_State, county  = usobesity_County)

# Exploratory Data Analysis
# Hypotheses - Obesity increases as temperature increases. Obesity increases as 
# elevation decreases. Obesity increases as median income decreases

# linear model of obesity as function of elev, temp, income
USlm <- lm(ob_pct ~ tmax + medinc + elev, data = USdata)
plot(USlm)

# notes on linear model:
# residuals in this case are (actual obesity pct) - (predicted obesity pct)
# Residual vs. fitted graph useful to see if we are using the appropriate type 
# of model (linear) for the dataset. Our model appears to be linear and exhibits
# homoscedasticity with consistent variance at most observed values. However, the
# model is tending to overpredict obesity at the low ends and underpredict at
# the higher end.
#
# Q-Q Plot - Quantile-Quantile
# Tells us if residuals are normally distributed
# There's a decent amount of points beyond 2 standard deviations, but it's hard
# to tell how many out of the whole set. possibly more than 5% though. The
# overpredicting of low values and underpredicting of high values still holds
#
# Scale - Location Plot
# shows if the residuals are spread equally among our predictions in order to 
# check homoscedasticity (equal variance of residuals)
#
# The regression summary:
# median is close to zero, so this is good to indicate the distribution is 
# normal and the model isn't skewed.
# min and max and 1st and 3rd quartile are very close as well. normal!
# Estimate (coefficients) tmax - for a 1 degree increase in max temp in July,
# we can expect a .21% increase in obesity (and it's significant ***)
# t value is the number of standard deviations between the estimate and 0 -
# zero being no effect. In general, if t is high, coefficient is statistically
# significant.
# p-value for the t-test (0.05 as benchmark). All three are highly significant.
# close to 0% chance this is due to chance.
# Reject the null hypothesis that temp, elev, and median income have no effect
# on predicted obesity rates.
# Adjusted R-squared is only 0.52, so these three variables only explain roughly
# half of the obesity percentage in the US
# F-statistic - General indicator if there is an actual relationship between
# predictor variables and the response variable. Higher is more significant


################# PART B - LOOK FOR CLUSTERS ######################

# Perform Moran's I test and Getis-Ord Gi* test

# Global Moran's I to determine overall spatial clustering
# LISA - Local Indicators of Spatial Association to identify hot spots or
# clusters of high/ low values and spatial outliers (Local Moran's I/Getis-Ord)

USdata_clean <- na.omit(USdata)
USdata_clean2 <- USdata_clean[!st_is_empty(USdata_clean),]

# Global Moran's I - Need neighbors, weighted distance and the data (NWX)
neigh <- st_contiguity(USdata_clean2)
wts <- st_weights(neigh, allow_zero = TRUE)
USgm <- global_moran(USdata_clean2$ob_pct, neigh, wts)

# Local Moran's I
USlocalm <- local_moran(USdata_clean2$ob_pct, neigh, wts)
USdata_clean2$pysal <- USlocalm$pysal

ggplot() +
  geom_sf(data = USdata_clean2, aes(geometry = geom, fill = pysal)) +
  scale_fill_manual(values = c("#0004f9", "#84e802", "#ce84ff", "#fc0206")) +
  labs(title = "Local Moran's I - US Obesity Rates") +
  theme_void()  

# Getis-Ord Gi*
gi <- local_gstar_perm(USdata_clean2$ob_pct, neigh, wts)

# z score map
USdata_clean2 %>% ggplot(aes(fill = gi$gi_star))  +
  geom_sf() +
  scale_fill_gradient2(low = 'blue', high = 'red') +
  labs(title = "Getis-Ord Gi-star - US Obesity", fill = 'Z-Score') +
  theme_void()

# p-value map
USdata_clean2 %>% ggplot(aes(fill = gi$p_value))  +
  geom_sf() +
  scale_fill_gradient2() +
  labs(title = "Getis-Ord Gi-star: p-values - US Obesity", fill = 'p-value') +
  theme_void()

# Spatial Lag Model

nb <- poly2nb(USdata_clean2, queen = TRUE)
weights <- nb2listw(nb, style = "W",zero.policy = TRUE)

model <- lm(USdata_clean2$ob_pct ~ USdata_clean2$tmax + USdata_clean2$medinc + USdata_clean2$elev, data = USdata_clean2)
obSLM <- lagsarlm(model, listw = weights, zero.policy=TRUE)

# plot SLM and raw obesity data to compare
fitted_slm <- as.vector(fitted(obSLM))
ggplot(USdata_clean2) +
       geom_sf(aes(fill = fitted_slm)) +
       scale_fill_viridis_c(limits = range(USdata_clean2$ob_pct)) +
       labs(title = 'Spatial Lag Model') +
       theme_void()
ggplot(USdata_clean2) +
  geom_sf(aes(fill = ob_pct)) +
  scale_fill_viridis_c(limits = range(USdata_clean2$ob_pct)) +
  labs(title = "Raw Obesity %") + 
  theme_void()

# Spatial Error Model

modelSEM <- errorsarlm(model, listw = weights, zero.policy = TRUE)
fitted_sem <- as.vector(fitted(modelSEM))

# plot fitted SEM values
ggplot(USdata_clean2) +
  geom_sf(aes(fill = fitted_sem)) +
  scale_fill_viridis_c(limits = range(USdata_clean2$ob_pct)) +
  labs(title = 'Spatial Error Model - Fitted Values') +
  theme_void()

# Also plot residuals
residuals <- as.vector(residuals(modelSEM))
ggplot(USdata_clean2) + 
  geom_sf(aes(fill = residuals)) + 
  labs(title = 'Residuals of Fitted Values of Spatial Error Model') + 
  scale_fill_viridis_c() +
  theme_void()
