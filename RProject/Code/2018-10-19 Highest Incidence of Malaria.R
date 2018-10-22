##############################################
### Author: Piper Williams 
### BIOS 6640, Project 1
### Purpose: Highest Incidence of Malaria
      # Plots and Tables
### Date: October 19, 2018
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

# make Maputo one province 
data$Province <- as.character(data2$Province)
data$Province[data$Province %in% c("MAPUTO", "MAPUTO CIDADE")] <- "MAPUTO"

# check that levels were combined correctly
data$Province <- as.factor(data$Province)
levels(data$Province)

########################## Region with highest malaria ############################
# region with most cases/1000 under 5 (aggregated over all time)
avg.cptu5.by.region <- data %>% 
  group_by(Region) %>%
  summarise(mean.cptu5 = mean(cptu5)) # Northern region = highest mean cptu5

mal.by.region <- data %>% 
  group_by(Region) %>%
  summarise(mean.malaria = mean(malaria)) # Northern region = highest mean malaria cases u5

# see if certain parts of year have higher incidence compared to others
data <- data %>%
  mutate(Quarter = cut(Epiweek, breaks = c(0, 13, 26, 39, Inf), 
         labels = c("Quarter 1" , "Quarter 2", "Quarter 3", "Quarter 4")))

avg.quarter.cptu5.by.region <- data %>% 
  group_by(Region, Quarter) %>%
  summarise(mean.cptu5 = mean(cptu5)) # q1 and q2 seem to have higher incidence

avg.quarter.cptu5.by.province <- data %>% 
  group_by(Province, Quarter) %>%
  summarise(mean.cptu5 = mean(cptu5))

# plot this data
ggplot(data=avg.quarter.cptu5.by.region) + 
  geom_bar(stat='identity', aes(x=Quarter, y=mean.cptu5, fill=Region)) +
  facet_wrap( ~Region, ncol = 2) + 
  labs(y="Average Cases per 1000 Under 5", x="Annual Quarter") + 
  scale_fill_manual(values = c("purple1", "darkblue", "orange1", "deeppink1"))

ggplot(data=avg.quarter.cptu5.by.province) + 
  geom_bar(stat='identity', aes(x=Quarter, y=mean.cptu5, fill=Province)) +
  facet_wrap( ~Province, ncol = 2) + 
  labs(y="Average Cases per 1000 Under 5", x="Annual Quarter") + 
  scale_fill_manual(values = c("purple4", "purple1", "darkblue", "blue", "deepskyblue1", 
                               "cyan2", "yellow1", "orange1", "deeppink1", "deeppink4"))

