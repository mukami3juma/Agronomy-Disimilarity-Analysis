# showing carob's fertilizer and variety trials group data

path <- getwd()
df0 <- read.csv("D:/OneDrive - CGIAR/Documents/carob1//data/compiled/carob_fertilizer.csv") # 115210
df1 <- read.csv("D:/OneDrive - CGIAR/Documents/carob1//data/compiled/carob_variety_trials.csv") # 432

# Data transformation/cleaning
# checking variables that appear in both fertilizer and variety trials datasets

f <- which(names(df0) %in% names(df1))
names(df0)[f]

# merging fertilizer and variety trials based on common variables in the two datasets

df <- merge(df0,df1,by = names(df0)[f],all = TRUE) #115642

# dropping duplicates

df <- df[!duplicated(df),] # remaining 100930

# minimize to EAC countries i.e DRC, Tanzania, Kenya, Burundi, Rwanda, South Sudan(not in dataset of interest), 
# and Uganda.

# subsetting to remain with EAC countries
dd <- df[df$country %in% c("Democratic Republic of the Congo","Tanzania","Kenya", "Burundi", "Rwanda","Uganda"),]  #35302, 

# filling NA coords to avoid losing data

# based on adm2
g <- data.frame(country = c("Tanzania", "Tanzania", "Tanzania","Tanzania", "Tanzania", "Tanzania", "Tanzania"), 
                adm2 = c("Lindi","Newala", "Biharamulo", "Tandahimba", "Nachingwea", "Serengeti","Masasi"),
                lat = c(-9.5, -10.72487, -2.63194, -10.76232,-10.35959, -2.33333, -10.733), 
                lon = c(38.5, 39.27979,31.30889, 39.63077, 38.78148, 34.83333, 38.767))

r <- merge(dd,g,by = c("country","adm2"),all.x = TRUE)
r$latitude <- ifelse(is.na(r$latitude),r$lat,r$latitude)
r$longitude <- ifelse(is.na(r$longitude),r$lon,r$longitude)

dd <- r[,1:85]

# based on adm1
g <- data.frame(country = c("Rwanda", "Rwanda", "Rwanda", "Tanzania", 
                            "Tanzania", "Tanzania", "Tanzania", "Uganda", "Uganda", "Uganda", 
                            "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", "Uganda", 
                            "Uganda"),
                adm1 = c("Iburasirazuba", "Amajyaruguru", "Amajyepfo", 
                         "Gairo", "Moshi", "Mvomero", "Kongwa", "Bukedea", "Kumi", "Kabale", 
                         "Oyam", "Bulambuli", "Kanungu", "Manafwa", "Kapchorwa", "Kisoro", 
                         "Sironko", "Lira"),
                lat = c(-1.75, -1.5763, -2.59667, -6.13841,
                        -3.35, -6.3, -6.2, 1.36667, 1.5, -1.24857, 2.38129, 1.32055, 
                        -0.75, 0.88333, 1.3333, -1.28538, 1.157, 2.2499), 
                lon = c(30.5,30.0675, 29.73944, 36.88079, 37.33333, 37.45, 36.417, 34.13333,
                        33.95, 29.98993, 32.50071, 34.28062, 29.73, 34.33333, 34.42,
                        29.68497, 34.314, 32.89985))

r <- merge(dd,g,by = c("country","adm1"),all.x = TRUE)
r$latitude <- ifelse(is.na(r$latitude),r$lat,r$latitude)
r$longitude <- ifelse(is.na(r$longitude),r$lon,r$longitude)

dd <- r[,1:85]

# based on country
g <- data.frame(country = c("Kenya", "Tanzania"), 
                lat = c(1, -6), 
                lon = c(38, 35))

r <- merge(dd,g,by = c("country"),all.x = TRUE)
r$latitude <- ifelse(is.na(r$latitude),r$lat,r$latitude)
r$longitude <- ifelse(is.na(r$longitude),r$lon,r$longitude)

# v <- dd
# p <- complete.cases(v[,c("country")])
# vv <- v[p,]
# v1 <- unique(vv[is.na(vv$latitude) | is.na(vv$longitude), c("country", "latitude", "longitude")])
# # v1 <- v1[v1$adm1 != "",]
# 
# for (i in 1:nrow(v1)) {
#   ll <- carobiner::geocode(country = v1$country[i], adm1 = v1$country[i], location = v1$country[i], service = "geonames", username = "efyrouwa")
#   ii <- unlist(jsonlite::fromJSON(ll))
#   c <- as.integer(ii["totalResultsCount"][[1]])
#   v1$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
#   v1$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
# }
# 
# v1 <- v1[!is.na(v1$latitude) | !is.na(v1$longitude),]
# sss <- dput(v1)

# 
# t <- unique(dr[,c("longitude","latitude")])
# library(leaflet)
# places <- data.frame(latitude = t$latitude, longitude = t$longitude)
# map <- leaflet(places) %>% addTiles() # adds default map tiles
# map <- map %>% addMarkers(lng = ~longitude, lat = ~latitude)
# map


dr <- r #35302

# subsetting to remain with variables of interest

dr <- dr[,c("dataset_id","country","longitude","latitude","crop","N_fertilizer","P_fertilizer",
            "K_fertilizer")]

# calculating NAs 

missing <- sapply(dr, function(x) sum(is.na(x)) / length(x) * 100)
miss <- data.frame(Variables = names(missing), Missing_Percentage = missing)
row.names(miss) <- NULL

# Data transformation/cleaning# dealing with NAs

# distribution: removing  outlier amounts of NPK application and dropping amounts with above respective maximums

rcrds <- read.csv("D:/OneDrive - CGIAR/Documents/carob1/terms/records.csv")

for (i in names(dr)[5:length(dr)]) {
  col_index <- which(unique(rcrds$name) == i)  # Find the index of the column in records to extract it's valid_max
  valid_max <- rcrds$valid_max[col_index]  # Get the valid_max for the column
  
  # Check if valid_max is not NA and dr[i] > valid_max
  if (!is.na(valid_max) && any(dr[[i]] > valid_max)) {
    dr[[i]][dr[[i]] > valid_max] <- NA
  }
}

# Removing NAs from NPK fertilizer inputs(lost 2967 entries remaining with 32335)

dr <- dr[!is.na(dr$N_fertilizer),]
dr <- dr[!is.na(dr$P_fertilizer),]
dr <- dr[!is.na(dr$K_fertilizer),]

# 
# fixing datatypes
# # EGB: Not necessary
# dr$longitude <- as.numeric(dr$longitude)
# dr$K_fertilizer <- as.numeric(dr$K_fertilizer)

# EDA
# Create a frequency table of the countries

level <- table(dr$country)

# Create a sorted ggplot2 bar plot

library(ggplot2)
ggplot(dr, aes(x = reorder(country, -level[country]), fill = country)) +
  geom_bar() +
  labs(x = "Country", y = "Frequency",title = "Frequency distribution of Country") +
  guides(fill = "none")

# Create a frequency table of the crops

level <- table(dr$crop)

# Create a sorted ggplot2 bar plot

library(ggplot2)
ggplot(dr, aes(x = reorder(crop, -level[crop]), fill = crop)) +
  geom_bar() +
  labs(x = "Crop", y = "Frequency",title = "Frequency distribution of Crop") +
  guides(fill = "none")


# # N,P,K fertilizer boxplots
fert <- dr[,c("N_fertilizer","P_fertilizer","K_fertilizer")]
boxplot(fert, main = "Boxplot of N,P,K Fertilizers")

# calculating quantiles to determine where they should fall dependent on variable distribution
# 
# summary_stats <- c(0.00, 0.00, 15.00, 15.67, 30.00, 201.00) # P fertilizer
# summary_stats <- c(0.00, 0.00, 0.00, 36.94, 60.00, 200.00) # N
# summary_stats <- c(0.00, 0.00, 0.00, 20.15, 30.00, 180.00 ) # K
# 
# # calculating quantiles
# quantiles <- quantile(summary_stats,probs = c(0,0.5,1))
# # defining the threshold based on the median
# threshold <- quantiles[2]
# # creating quantile percentages
# quantile_percentages <- round(quantiles / max(summary_stats) * 100, 2)


# create groups of fertilier inputs based on their individual distributions and quantiles

dr$N_levels <- cut(dr$N_fertilizer, breaks = quantile(dr$N_fertilizer, probs = c(0.5,0.75,1)), 
                   labels = c("Low","High"), 
                   include.lowest = TRUE, right = FALSE)

dr$P_levels <- cut(dr$P_fertilizer, breaks = quantile(dr$P_fertilizer, probs = c(0,0.5,1)), 
                   labels = c("Low","High"), 
                   include.lowest = TRUE, right = FALSE)
# EGB: Changing to manual cuts
dr$K_levels <- cut(dr$K_fertilizer, breaks = quantile(dr$K_fertilizer, probs = c(0.5,0.75,1)), 
                   labels = c("Low","High"),
                   include.lowest = TRUE, right = FALSE)


library(terra)
# EGB: Remove points outside AOI
KEN <- terra::vect("C:/Users/User/Downloads/gadm41_KEN.gpkg", layer = "ADM_ADM_0")
TZA <- terra::vect("C:/Users/User/Downloads/gadm41_TZA.gpkg", layer = "ADM_ADM_0")
RWA <- terra::vect("C:/Users/User/Downloads/gadm41_RWA.gpkg", layer = "ADM_ADM_0")
UGA <- terra::vect("C:/Users/User/Downloads/gadm41_UGA.gpkg", layer = "ADM_ADM_0")
BDI <- terra::vect("C:/Users/User/Downloads/gadm41_BDI.gpkg", layer = "ADM_ADM_0")
COD <- terra::vect("C:/Users/User/Downloads/gadm41_COD.gpkg", layer = "ADM_ADM_0")
EAC <- terra::union(terra::union(terra::union(terra::union(terra::union(KEN,TZA),RWA),UGA),BDI),COD)
dr <- terra::as.data.frame(terra::intersect(terra::vect(dr, geom=c("longitude", "latitude")), EAC), geom	 = "XY")


# dr <- unique(dr[,c(lati])
# EGB: Nice! But also subset for crop!

# subsetting per fertilizer level for maize which has the highest inputs and is of our interest
# N_levels
# First, low amounts
# de <- dr[dr$N_levels == "Low",]
d.M.N.L <- unique(dr[dr$N_levels == "Low" & dr$crop == "maize",c("y","x")]) # 4566 with low N input and crop maize but 298 points are unique
d.M.N.L<- na.omit(d.M.N.L)

d.M.N.H <- unique(dr[dr$N_levels == "High" & dr$crop == "maize",c("y","x")]) # 7397 with high N input and crop maize but 288 points are unique
d.M.N.H <- na.omit(d.M.N.H)

# d.S.N.L <- unique(dr[dr$N_levels == "Low" & dr$crop == "soybean",c("y","x")])
# d.S.N.H <- unique(dr[dr$N_levels == "High" & dr$crop == "soybean",c("y","x")])
# d.B.N.L <- unique(dr[dr$N_levels == "Low" & dr$crop == "common bean",c("y","x")])
# d.B.N.H <- unique(dr[dr$N_levels == "High" & dr$crop == "common bean",c("y","x")])
# # subsetting unique for lon and lat
# dt <- unique(de[,c("latitude","longitude")])

# extracting bio climatic conditions per coordinate


# source("https://raw.githubusercontent.com/egbendito/AgWISE-generic/develop/00_dataProcessing/worldclim.R")
source("C:/Users/User/Documents/AgWISE-generic/00_dataProcessing/worldclim.R")

# install.packages("raster")
library(raster)

# downloading worlclim variables

f <- worldclim(var=c("tavg", "tmin", "tmax", "prec","elev","bio"),10,raster = TRUE,coords = data.frame(X = c(terra::ext(EAC)[1][[1]], terra::ext(EAC)[2][[1]]),
                                                                                                       Y = c(terra::ext(EAC)[3][[1]], terra::ext(EAC)[4][[1]])))


# 
# ff <- worldclim(var=c("tavg", "tmin", "tmax", "prec","elev","bio"),10,raster = FALSE,coords = data.frame(X = c(terra::ext(f1)[1][[1]], terra::ext(f1)[2][[1]]),
#                                                                                                          Y = c(terra::ext(f1)[3][[1]], terra::ext(f1)[4][[1]])))

# masking to exact boundary of AOI
f <- terra::crop(f,EAC)
f <- terra::mask(f,EAC)
plot(f[[1]])
plot(EAC,add=T)
points(x=d.M.N.L$x,y=d.M.N.L$y)

# downloading iSDA variables soil info, depth 0-20cm and geodata package for whole of AFRICA

pH <- geodata::soil_af_isda("pH.H2O", depth = 20, path='data/', quiet=TRUE)
SOC <- geodata::soil_af_isda("C.tot", depth = 20, path='data/', quiet=TRUE)
sand <- geodata::soil_af_isda("sand", depth = 20, path='data/', quiet=TRUE)
clay <- geodata::soil_af_isda("clay", depth = 20, path='data/', quiet=TRUE)
silt <- geodata::soil_af_isda("silt", depth = 20, path='data/', quiet=TRUE)
N <- geodata::soil_af_isda("N.tot", depth = 20, path='data/', quiet=TRUE)
K <- geodata::soil_af_isda("K", depth = 20, path='data/', quiet=TRUE)
P <- geodata::soil_af_isda("P", depth = 20, path='data/', quiet=TRUE)

# cropping and masking soil details to only needed AOI
pH <- resample(terra::mask(terra::crop(pH,EAC),EAC),f)
SOC <- resample(terra::mask(terra::crop(SOC,EAC),EAC),f)
sand <- resample(terra::mask(terra::crop(sand,EAC),EAC),f)
clay <- resample(terra::mask(terra::crop(clay,EAC),EAC),f)
silt <- resample(terra::mask(terra::crop(silt,EAC),EAC),f)
N <- resample(terra::mask(terra::crop(N,EAC),EAC),f)
K <- resample(terra::mask(terra::crop(K,EAC),EAC),f)
P <- resample(terra::mask(terra::crop(P,EAC),EAC),f)

plot(pH) 
plot(EAC,add=T)
points(x=d.M.N.L$x,y=d.M.N.L$y)
plot(SOC)
plot(EAC,add=T)
plot(sand)
plot(EAC,add=T)
plot(clay)
plot(EAC,add=T)
plot(silt)
plot(EAC,add=T)
plot(N)
plot(EAC,add=T)
plot(K)
plot(EAC,add=T)
plot(P)
plot(EAC,add=T)



# putting environmental and soil properties in one
# spatial raster(they must have similar extents in order to be combined,so we follow f's extent)

spat_raster<- c(f,pH, SOC, sand, clay, silt, N, K, P)

# Stack the SpatRaster objects into a single SpatRaster brick
stacked_raster <- raster::stack(spat_raster)

# Print information about the stacked raster
print(stacked_raster)


install.packages("dismo")
library("dismo")
install.packages("rgeos")
library("rgeos")
library("terra")
install.packages("rJava")
library("rJava")


ncell(stacked_raster) # high ncell shows more higher spatial resolution and more detailed data
set.seed(23) 
bg <- sampleRandom(x=stacked_raster,
                   size=ncell(stacked_raster), # size doesnt change variables of interest. also,remaining 11868 after dropping NAs
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 

# viewing first env condition on plot
plot(stacked_raster[[68]])

# add the background points to the plotted raster
terra::plot(bg,add=T)

# add the occurrence data to the plotted raster
points(x = d.M.N.L$x, y = d.M.N.L$y, col ="blue", pch = "+")
terra::plot(EAC, add = T)

# bio & soil features for low N level in maize
ott <- terra::extract(spat_raster,d.M.N.L[,c("x","y")]) 
v <- usdm::vifstep(ott[,2:ncol(ott)]) #threshold is 10

# variables of choice

vars <- c("wc2.1_10m_tmax_08","wc2.1_10m_prec_02","wc2.1_10m_prec_04","wc2.1_10m_prec_07","wc2.1_10m_prec_11",
          "wc2.1_10m_elev","wc2.1_10m_bio_7","wc2.1_10m_bio_15","wc2.1_10m_bio_18","wc2.1_10m_bio_19",
          "c.tot.0-20cm","clay.tot.psa_0-20cm","n.tot.ncs_0-20cm","k_0-20cm","p_0-20cm")

# checking correlation in chosen variables
# drop_vars <- c("wc2.1_10m_bio_15","k_0-20cm","wc2.1_10m_prec_07")

# "wc2.1_10m_bio_15" correlated at 0.7 and above with "c.tot.0-20cm" & "wc2.1_10m_bio_19"
# "k_0-20cm" correlated at 0.7 and above with "p_0-20cm"
# "wc2.1_10m_prec_07" correlated at 0.7 and above with "wc2.1_10m_bio_19"


# randomly select 70% for training
set.seed(23)
selected <- sample(1:nrow(d.M.N.L), nrow(d.M.N.L) * 0.7)

prc_train <- d.M.N.L[selected,]  # this is the selection to be used for model training
prc_test <- d.M.N.L[-selected, ]  # this is the opposite of the selection

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p <- terra::extract(spat_raster, prc_train[,c("x", "y")])
# p_df <- data.frame(x = prc_train$x, y = prc_train$y, values = p)
# library(sf)
# sf_obj <- st_as_sf(p_df, coords = c("x", "y"))

# omiting NAs form p df
p <- na.omit(p) # 3 NAs remianing 12073

# env conditions for testing occ
p_test <- terra::extract(spat_raster, prc_test[,c("x", "y")])

# extracting env conditions for background
a <- terra::extract(spat_raster, terra::vect(bg))

# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))

# (rep(1,nrow(p)) creating the number of rows as the p data
# set to have the number '1' as the indicator for presence;
# rep(0,nrow(a)) creating the number of rows as the a data
# set to have the number '0' as the indicator for absence;
# the c combines these ones and zeros into a new vector that
# can be added to the Maxent table data frame with the
# environmental attributes of the presence and absence
# locations
pder <- as.data.frame(rbind(p, a))

# train Maxent with tabular data
mod <- maxent(x=pder[,vars], ## env conditions
              p=pa,   ## 1:presence or 0:absence
              path=paste0("D:/OneDrive - CGIAR/Documents/Dissimilarity-Analysis-in-Agronomy/data/output"), ## folder for maxent output; 
              # if we do not specify a folder R will put the results in a temp file, 
              # and it gets messy to read those. . .
              args=c("responsecurves") ## parameter specification
)
# the maxent functions runs a model in the default settings. To change these parameters,
# you have to tell it what you want...i.e. response curves or the type of features

# view the maxent model in a html brower
mod

# example 1, project to study area [raster]
ped1 <- predict(mod, spat_raster)  # studyArea is the clipped rasters 
head(ped1)
plot(ped1)  # plot the continuous prediction
points(d.M.N.L$x,d.M.N.L$y)
terra::plot(EAC, add = T)

# example 2, project with training occurrences [dataframes]
ped2 <- predict(mod, p)
head(ped2)
hist(ped2)

# example 3, project with testing occurrences [dataframes]
ped3 <- predict(mod, p_test)
head(ped3)
hist(ped3)

mod_eval_train <- dismo::evaluate(p = p, a = a, model = mod)
print(mod_eval_train)

mod_eval_test <- dismo::evaluate(p = p_test, a = a, model = mod)
print(mod_eval_test)

plot(mod_eval_train, 'ROC')
plot(mod_eval_train, 'TPR')
boxplot(mod_eval_train)
density(mod_eval_test)

# calculate thresholds of models

thd1 <- threshold(mod_eval_train, "no_omission")  # 0% omission rate 
thd2 <- threshold(mod_eval_train, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# threshold value

plot(ped1 >= thd1)
terra::plot(EAC, add = T)

# ##4 Maxent parameters ###4.1 Select features
# 
# #####Thread 21
# # load the function that prepares parameters for maxent
# source("https://raw.githubusercontent.com/shandongfx/workshop_maxent_R/master/code/Appendix2_prepPara.R")
# 
# mod1_autofeature <- maxent(x=pder[,selected_vars], 
#                            ## env conditions, here we selected only 13 predictors from prior feature selection
#                            p=pa,
#                            ## 1:presence or 0:absence
#                            path="C:/Users/User/Documents/carob1/output/maxent_outputs1_auto",
#                            ## this is the folder you will find maxent output
#                            args=prepPara(userfeatures=NULL) ) 
# ## default is autofeature
# 
# # or select Linear& Quadratic features
# mod1_lq <- maxent(x=pder[,selected_vars],
#                   p=pa,
#                   path=paste0("C:/Users/User/Documents/carob1/output/maxent_outputs1_lq"),
#                   args=prepPara(userfeatures="LQ") ) 
# ## default is autofeature, here LQ represents Linear& Quadratic
# ## (L-linear, Q-Quadratic, H-Hinge, P-Product, T-Threshold)
# 
# ###4.2 Change beta-multiplier
# 
# #####Thread 22
# 
# #change betamultiplier for all features
# mod2 <- maxent(x=pder[,selected_vars], 
#                p=pa, 
#                path=paste0("C:/Users/User/Documents/carob1/output/maxent_outputs2_0.5"), 
#                args=prepPara(userfeatures="LQ",
#                              betamultiplier=0.5) ) 
# 
# mod2 <- maxent(x=pder[,selected_vars], 
#                p=pa, 
#                path=paste0("C:/Users/User/Documents/carob1/output/maxent_outputs2_complex"), 
#                args=prepPara(userfeatures="LQH",
#                              ## include L, Q, H features
#                              beta_lqp=1.5, 
#                              ## use different betamultiplier for different features
#                              beta_hinge=0.5 ) ) 
# 
# ###4.3 Specify projection layers
# 
# #####Thread 23
# 
# # note: (1) the projection layers must exist in the hard disk (as relative to computer RAM); 
# # (2) the names of the layers (excluding the name extension) must match the names 
# # of the predictor variables; 
# mod3 <- maxent(x=pder[,selected_vars], 
#                p=pa, 
#                path=paste0("C:/Users/User/Documents/carob1/output/maxent_outputs3_prj1"), 
#                args=prepPara(userfeatures="LQ",
#                              betamultiplier=1,
#                              projectionlayers="/Users/iel82user/Google Drive/1_osu_lab/projects/2017_7_workshop_enm_R/data/studyarea") ) 
# 
# # load the projected map
# ped <- raster(paste0("C:/Users/User/Documents/carob1/output/maxent_outputs3_prj1/species_studyarea.asc"))
# plot(ped)
# 
# # we can also project on a broader map, but please 
# # caustion about the inaccuracy associated with model extrapolation.
# mod3 <- maxent(x=pder[,selected_vars], 
#                p=pa, 
#                path=paste0("../output/maxent_outputs3_prj2"), 
#                args=prepPara(userfeatures="LQ",
#                              betamultiplier=1,
#                              projectionlayers="/Users/iel82user/Google Drive/1_osu_lab/projects/2017_7_workshop_enm_R/data/bioclim") ) 
# # plot the map
# ped <- raster(paste0("../output/maxent_outputs3_prj2/species_bioclim.asc"))
# plot(ped)
# 
# # simply check the difference if we used a different betamultiplier
# mod3_beta1 <- maxent(x=pder[,selected_vars], 
#                      p=pa, 
#                      path=paste0("C:/Users/User/Documents/carob1/output/maxent_outputs3_prj3"), 
#                      args=prepPara(userfeatures="LQ",
#                                    betamultiplier=100, 
#                                    ## for an extreme example, set beta as 100
#                                    projectionlayers="/Users/iel82user/Google Drive/1_osu_lab/projects/2017_7_workshop_enm_R/data/bioclim") ) 
# ped3 <- raster(paste0("../output/maxent_outputs3_prj3/species_bioclim.asc"))
# plot(ped-ped3) ## quickly check the difference between the two predictions
# 
# ###4.4 Clamping function
# 
# #####Thread 24
# 
# # enable or disable clamping function; note that clamping
# # function is involved when projecting
# mod4_clamp <- maxent(x = pder[,selected_vars], p = pa, path = paste0("C:/Users/User/Documents/carob1/output/maxent_outputs4_clamp"), 
#                      args = prepPara(userfeatures = "LQ", betamultiplier = 1, 
#                                      doclamp = TRUE, projectionlayers = "/Users/iel82user/Google Drive/1_osu_lab/projects/2017_7_workshop_enm_R/data/bioclim"))
# 
# mod4_noclamp <- maxent(x = pder[c(,selected_vars)], p = pa, 
#                        path = paste0("C:/Users/User/Documents/carob1/output/maxent_outputs4_noclamp"), 
#                        args = prepPara(userfeatures = "LQ",
#                                        betamultiplier = 1, doclamp = FALSE, projectionlayers = "/Users/iel82user/Google Drive/1_osu_lab/projects/2017_7_workshop_enm_R/data/bioclim"))
# 
# ped_clamp <- raster(paste0("C:/Users/User/Documents/carob1/output/maxent_outputs4_clamp/species_bioclim.asc"))
# ped_noclamp <- raster(paste0("C:/Users/User/Documents/carob1/output/maxent_outputs4_noclamp/species_bioclim.asc"))
# plot(stack(ped_clamp, ped_noclamp))
# 
# ## we may notice small differences, especially clamp shows
# ## higher predictions in most areas.
# ###4.5 Cross validation
# 
# #####Thread 25
# 
# mod4_cross <- maxent(x=pder[,selected_vars], p=pa, 
#                      path=paste0("C:/Users/User/Documents/carob1/output/maxent_outputs4_cross"), 
#                      args=prepPara(userfeatures="LQ",
#                                    betamultiplier=1,
#                                    doclamp = TRUE,
#                                    projectionlayers="/Users/iel82user/Google Drive/1_osu_lab/projects/2017_7_workshop_enm_R/data/bioclim",
#                                    replicates=5, ## 5 replicates
#                                    replicatetype="crossvalidate") )
# ##possible values are: crossvalidate,bootstrap,subsample