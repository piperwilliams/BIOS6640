##############################################
### Author: Piper Williams 
### BIOS 6640, Project 1
### Purpose: Malaria in Mozambique
### Date: October 20, 2018
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

# avg. cptu5 for each province
cptu5.province <- as.data.frame(tapply(data2$cptu5, list(data2$Province), mean))
colnames(cptu5.province) <- c("avg.cptu5")

# avg. cptu5 for each district
cptu5.district <- as.data.frame(tapply(data2$cptu5, list(data2$District), mean))
colnames(cptu5.district) <- c("avg.cptu5")

# import shape file for provinces
poly.province <- readShapePoly("/Users/piper/Piper Documents/R and Python/Project 1/DataRaw/mozambique_admin1.shp")
plot(poly.province)

# import shape file for districts
poly.district <- readShapePoly("/Users/piper/Piper Documents/R and Python/Project 1/DataRaw/Moz_admin2.shp")
plot(poly.district)

# rename rows to match shape files
rownames(cptu5.province) <- poly.province$admin_1
rownames(cptu5.district) <- poly.district$DISTCODE

# combine data frame with the shape file
polydata.cptu5.province <- SpatialPolygonsDataFrame(poly.province, cptu5.province, match.ID = F)
polydata.cptu5.district <- SpatialPolygonsDataFrame(poly.district, cptu5.district, match.ID = F)

# plot avg. cptu5 for each province aggregated over all time
spplot(polydata.cptu5.province, c("avg.cptu5"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Malaria Cases per 1000 Under 5", 
       as.table = TRUE, col="transparent")

# plot avg. cptu5 for each district aggregated over all time
spplot(polydata.cptu5.district, c("avg.cptu5"), 
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Malaria Cases per 1000 Under 5", 
       as.table = TRUE,  col="transparent")

####################### ANNUAL QUARTER PLOTS ######################################
data2 <- data2 %>%
  mutate(Quarter = cut(Epiweek, breaks = c(0, 13, 26, 39, Inf), 
                       labels = c("Quarter 1" , "Quarter 2", "Quarter 3", "Quarter 4")))

# average cptu5 for each province per quarter 
cptu5.province2 <- as.data.frame(tapply(data2$cptu5, list(data2$Province, data2$Quarter), mean))
colnames(cptu5.province2) <- c('cptu5.Quarter1', 'cptu5.Quarter2', 
                                   'cptu5.Quarter3', 'cptu5.Quarter4')

# average cptu5 for each district per quarter
cptu5.district2 <- as.data.frame(tapply(data2$cptu5, list(data2$District, data2$Quarter), mean))
colnames(cptu5.district2) <- c('cptu5.Quarter1', 'cptu5.Quarter2', 
                               'cptu5.Quarter3', 'cptu5.Quarter4')

# rename rows to match shape files
rownames(cptu5.province2) <- poly.province$admin_1
rownames(cptu5.district2) <- poly.district$DISTCODE

# combine data frame with the shape file
polydata.cptu5.province2 <- SpatialPolygonsDataFrame(poly.province, cptu5.province2, match.ID = F)
polydata.cptu5.district2 <- SpatialPolygonsDataFrame(poly.district, cptu5.district2, match.ID = F)

#plot avg. cptu5 for each province aggregated over annual quarters
spplot(polydata.cptu5.province2, c('cptu5.Quarter1', 'cptu5.Quarter2', 'cptu5.Quarter3','cptu5.Quarter4'), 
       names.attr = c('Quarter 1', 'Quarter 2', 'Quarter 3', 'Quarter 4'),
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Malaria Cases per 1000 Under 5", 
       as.table = TRUE, col="transparent")

#plot avg. cptu5 for each district aggregated over annual quarters
spplot(polydata.cptu5.district2, c('cptu5.Quarter1', 'cptu5.Quarter2', 'cptu5.Quarter3','cptu5.Quarter4'), 
       names.attr = c('Quarter 1', 'Quarter 2', 'Quarter 3', 'Quarter 4'),
       colorkey=list(space="right"), scales = list(draw = TRUE), 
       main = "Malaria Cases per 1000 Under 5", 
       as.table = TRUE, col="transparent")

######################### Malaria Incidence Over Time ############################
data2.red <- data2 %>% 
  select(Epiyear, Epiweek, rainTot, tavg, cptu5, Province, District, DISTCODE, Region) %>%
  mutate(hypoth.date = as.Date(paste(Epiyear, Epiweek, "1", sep = "-"), format="%Y-%U-%u"))

ts.data.region <- data2.red %>%
  group_by(Region, hypoth.date) %>%
  summarise(mean.cptu5 = mean(cptu5),
            mean.rainTot = mean(rainTot),
            mean.tavg = mean(tavg)) 

ggplot(data=ts.data.region, aes(x=hypoth.date, y=mean.cptu5)) +
  geom_line(aes(color=Region)) + 
  labs(y="Malaria Cases per 1000 Under 5", x="Date") + 
  scale_color_manual(values = c("purple1", "darkblue", "orange1", "deeppink1"))

# also plot rainfall and temperature
ggplot(data=ts.data.region, aes(x=hypoth.date, y=mean.rainTot)) +
  geom_line(aes(color=Region)) + 
  labs(y="Average Total Weekly Rainfall (mm)", x="Date") + 
  scale_color_manual(values = c("purple1", "darkblue", "orange1", "deeppink1"))

ggplot(data=ts.data.region, aes(x=hypoth.date, y=mean.tavg)) +
  geom_line(aes(color=Region)) + 
  labs(y="Average Temperature (Celcius)", x="Date") + 
  scale_color_manual(values = c("purple1", "darkblue", "orange1", "deeppink1"))

# province plot
ts.data.province <- data2.red %>%
  group_by(Province, hypoth.date) %>%
  summarise(mean.cptu5 = mean(cptu5),
            mean.rainTot = mean(rainTot),
            mean.tavg = mean(tavg)) 

ggplot(data=ts.data.province, aes(x=hypoth.date, y=mean.cptu5)) +
  geom_line(aes(color=Province)) + 
  labs(y="Malaria Cases per 1000 Under 5", x="Date") + 
  scale_color_manual(values = c("purple4", "purple1", "darkblue", "blue", "deepskyblue1", 
                                  "cyan2", "yellow1", "orange1", "deeppink1", "deeppink4"))

