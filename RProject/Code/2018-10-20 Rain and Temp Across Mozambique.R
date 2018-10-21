##############################################
### Author: Piper Williams 
### BIOS 6640, Project 1
### Purpose: Initial Data Exploration
### Date: October 9, 2018
##############################################

# load libraries
library(dplyr)
library(tibble)
library(ggplot2)
library(tseries) # for ccf() function
library(RColorBrewer)
library(sp)
library(maptools) 
library(knitr)
library(kableExtra)

# read in data set
data <- read.csv("/Users/piper/Piper Documents/R and Python/Project 1/DataRaw/MozSyntheticMalaria.csv")

# create variable "malaria incidence in cases per 1,000 in children under 5"
data <- mutate(data,
               cptu5 = (malaria / (Population_UN*u5weight))*1000)

# exclude data from 2017
data <- data %>% 
  filter(!(Epiyear == 2017))

################ Rainfall and Temp across Mozambique Provinces ####################
# prep data for shape file
data2 <- data
data2$Province <- as.character(data2$Province)
data2$Province[data2$Province %in% c("MAPUTO", "MAPUTO CIDADE")] <- "MAPUTO"

# check that levels were combined correctly
data2$Province <- as.factor(data2$Province)
levels(data2$Province) 

# average rain total and tavg for each province
rain.province <- as.data.frame(tapply(data2$rainTot, list(data2$Province), mean))
colnames(rain.province) <- c("avg.rainTot")
tavg.province <- as.data.frame(tapply(data2$tavg, list(data2$Province), mean))
colnames(tavg.province) <- c("avg.tavg")
all.stats.province <- cbind(rain.province, tavg.province)

# average rain total and tavg for each district
rain.district <- as.data.frame(tapply(data2$rainTot, list(data2$District), mean))
colnames(rain.district) <- c("avg.rainTot")
tavg.district <- as.data.frame(tapply(data2$tavg, list(data2$District), mean))
colnames(tavg.district) <- c("avg.tavg")
all.stats.district <- cbind(rain.district, tavg.district)

# import shape file for provinces
poly.province <- readShapePoly("/Users/piper/Piper Documents/R and Python/Project 1/DataRaw/mozambique_admin1.shp")
plot(poly.province)

# import shape file for districts
poly.district <- readShapePoly("/Users/piper/Piper Documents/R and Python/Project 1/DataRaw/Moz_admin2.shp")
plot(poly.district)

# rename rows to match shape files
rownames(all.stats.province) <- poly.province$admin_1
rownames(all.stats.district) <- poly.district$DISTCODE

# combine 'all.stats' data frame with the shape file
polydata.province <- SpatialPolygonsDataFrame(poly.province, all.stats.province, match.ID = F)
polydata.district <- SpatialPolygonsDataFrame(poly.district, all.stats.district, match.ID = F)

#plot avg. total rainfall for each province aggregated over all time
spplot(polydata.province, c("avg.rainTot"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Total Weekly Rainfall (mm)", 
       as.table = TRUE, col="transparent")

#plot avg. total rainfall for each district aggregated over all time
spplot(polydata.district, c("avg.rainTot"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Total Weekly Rainfall (mm)", 
       as.table = TRUE,  col="transparent")

#plot avg. temperature for each province aggregated over all time
spplot(polydata.province, c("avg.tavg"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Temperature (Celcius)", 
       as.table = TRUE, col="transparent")

#plot avg. total rainfall for each district aggregated over all time
spplot(polydata.district, c("avg.tavg"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Temperature (Celcius)", 
       as.table = TRUE,  col="transparent")

####################### ANNUAL QUARTER PLOTS ######################################
data2 <- data2 %>%
  mutate(Quarter = cut(Epiweek, breaks = c(0, 13, 26, 39, Inf), 
                       labels = c("Quarter 1" , "Quarter 2", "Quarter 3", "Quarter 4")))

# average rain total and tavg for each province per quarter 
rain.province2 <- as.data.frame(tapply(data2$rainTot, list(data2$Province, data2$Quarter), mean))
tavg.province2 <- as.data.frame(tapply(data2$tavg, list(data2$Province, data2$Quarter), mean))
all.stats.province2 <- cbind(rain.province2, tavg.province2)
colnames(all.stats.province2) <- c('rain.Quarter1', 'rain.Quarter2', 'rain.Quarter3',
                                   'rain.Quarter4', 'tavg.Quarter1', 'tavg.Quarter2',
                                   'tavg.Quarter3', 'tavg.Quarter4')

# average rain total and tavg for each district per quarter
rain.district2 <- as.data.frame(tapply(data2$rainTot, list(data2$District, data2$Quarter), mean))
tavg.district2 <- as.data.frame(tapply(data2$tavg, list(data2$District, data2$Quarter), mean))
all.stats.district2 <- cbind(rain.district2, tavg.district2)
colnames(all.stats.district2) <- c('rain.Quarter1', 'rain.Quarter2', 'rain.Quarter3',
                                   'rain.Quarter4', 'tavg.Quarter1', 'tavg.Quarter2',
                                   'tavg.Quarter3', 'tavg.Quarter4')

# rename rows to match shape files
rownames(all.stats.province2) <- poly.province$admin_1
rownames(all.stats.district2) <- poly.district$DISTCODE

# combine 'all.stats' data frame with the shape file
polydata.province2 <- SpatialPolygonsDataFrame(poly.province, all.stats.province2, match.ID = F)
polydata.district2 <- SpatialPolygonsDataFrame(poly.district, all.stats.district2, match.ID = F)

#plot avg. total rainfall for each province aggregated over annual quarters
spplot(polydata.province2, c('rain.Quarter1', 'rain.Quarter2', 'rain.Quarter3','rain.Quarter4'), 
       names.attr = c('Quarter 1', 'Quarter 2', 'Quarter 3', 'Quarter 4'),
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Total Weekly Rainfall (mm)", 
       as.table = TRUE, col="transparent")

#plot avg. total rainfall for each district aggregated over annual quarters
spplot(polydata.district2, c('rain.Quarter1', 'rain.Quarter2', 'rain.Quarter3','rain.Quarter4'), 
       names.attr = c('Quarter 1', 'Quarter 2', 'Quarter 3', 'Quarter 4'), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Total Weekly Rainfall (mm)", 
       as.table = TRUE,  col="transparent")

#plot avg. temperature for each province aggregated over annual quarters
spplot(polydata.province2, c('tavg.Quarter1', 'tavg.Quarter2', 'tavg.Quarter3','tavg.Quarter4'), 
       names.attr = c('Quarter 1', 'Quarter 2', 'Quarter 3', 'Quarter 4'), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Temperature (Celcius)", 
       as.table = TRUE, col="transparent")

#plot avg. total rainfall for each district aggregated over annual quarters
spplot(polydata.district2, c('tavg.Quarter1', 'tavg.Quarter2', 'tavg.Quarter3','tavg.Quarter4'), 
       names.attr = c('Quarter 1', 'Quarter 2', 'Quarter 3', 'Quarter 4'), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Temperature (Celcius)", 
       as.table = TRUE,  col="transparent")

