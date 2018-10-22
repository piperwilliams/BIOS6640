##############################################
### Author: Piper Williams 
### BIOS 6640, Project 1
### Purpose: Extra Exploration
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

########################### Seasonal Boxplots #####################################
data <- data %>%
  mutate(Quarter = cut(Epiweek, breaks = c(0, 13, 26, 39, Inf), 
                       labels = c("Quarter 1" , "Quarter 2", "Quarter 3", "Quarter 4")))

ggplot(data=data) +
  geom_boxplot(aes(x=Quarter, y=cptu5)) # not interesting
