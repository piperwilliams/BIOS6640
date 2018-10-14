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

# read in data set
data <- read.csv("/Users/piper/Piper Documents/R and Python/Project 1/DataRaw/MozSyntheticMalaria.csv")

# create variable "malaria incidence in cases per 1,000 population in children under 5"
data <- mutate(data,
               cptu5 = (malaria / (Population_UN*u5weight))*1000)
data <- data %>% 
  filter(!(Epiyear == 2017))

####################### Exploratory Data Analysis ################################
# summary of data
summary(data) # no missing data

# distribution of the outcome 
hist(data$cptu5) # seems positively skewed
hist(log(data$cptu5)) # does not make better, skewed negatively
hist(sqrt(data$cptu5)) # seems very normally distributed
# unsure if I want to transform outcome, since we are not modeling data

############################## Optimal Lag Length #################################
# ccf plots to determine optimal lead length
ccf(data$rainTot, data$cptu5, main = "CCF Plot: Total Rainfall vs. CPT-U5") # optimal lead ~ 4
ccf(data$tavg, data$cptu5, main = "CCF Plot: Avg Temperature vs. CPT-U5") # optimal lead ~ 15
ccf(data$rainTot, data$cptu5, 8, main = "CCF Plot: Total Rainfall vs. CPT-U5")
ccf(data$tavg, data$cptu5, 20, main = "CCF Plot: Avg. Temperature vs. CPT-U5")

# numerical estimates of max ACF and corresponding lag/lead 
rain.ccf <- ccf(data$rainTot, data$cptu5)
max.acf1 <- max(rain.ccf$acf) # 0.1197 = max ACF
rain.lag <- rain.ccf$lag[which(rain.ccf$acf > max.acf1-0.0001 & rain.ccf$acf < max.acf1+0.0001)] # -4

temp.ccf <- ccf(data$tavg, data$cptu5)
max.acf2 <- max(temp.ccf$acf) # 0.2101 = max ACF
temp.lag <- temp.ccf$lag[which(temp.ccf$acf > max.acf2-0.0001 & temp.ccf$acf < max.acf2+0.0001)] # -16

########################## Region with highest malaria ############################
# region with most cases/1000 under 5
avg.cptu5.by.region <- data %>% 
  group_by(Region) %>%
  summarise(mean.cptu5 = mean(cptu5)) # Northern region = highest mean cptu5

mal.by.region <- data %>% 
  group_by(Region) %>%
  summarise(mean.malaria = mean(malaria)) # Northern region = highest mean malaria cases u5

# prep data for shape file
data2 <- data
data2$Province <- as.character(data2$Province)
data2$Province[data2$Province %in% c("MAPUTO", "MAPUTO CIDADE")] <- "MAPUTO"

# check that levels were combined correctly
data2$Province <- as.factor(data2$Province)
levels(data2$Province) 

# rain total for each province
rain.province <- as.data.frame(tapply(data2$rainTot, list(data2$District), mean))
tavg.province <- as.data.frame(tapply(data2$tavg, list(data2$District), mean))

all.stats <- cbind(rain.province, tavg.province)
colnames(all.stats) <- c("avg.rainTot", "avg.tavg")

# import shape file for provinces
poly <- readShapePoly("/Users/piper/Piper Documents/R and Python/Week 5/Moz_admin2.shp")
plot(poly)

# rename rows to match shape file
all.stats2 <- all.stats
rownames(all.stats2) <- poly$admin_2

# combine 'all.stats' data frame with the shape file
polydata <- SpatialPolygonsDataFrame(poly, all.stats2, match.ID = F)

# create color palettes for maps
rain.pal <- brewer.pal(n = 9, name = "YlGnBu")
tavg.pal <- brewer.pal(n = 9, name = "RdPu")

#plot avg. total rainfall for each province
spplot(polydata, c("avg.rainTot"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Total Weekly Rainfall (mm)", 
       as.table = TRUE, col.regions = rain.pal, col="transparent", cuts = 8)

#plot avg. tavg for each province
spplot(polydata, c("avg.tavg"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Average Weekly Temperature Average", 
       as.table = TRUE, col.regions = tavg.pal, col="transparent", cuts = 8)
