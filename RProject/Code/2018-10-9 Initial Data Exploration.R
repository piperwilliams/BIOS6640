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

####################### Exploratory Data Analysis ################################
# summary of data
summary(data) # no missing data

# distribution of the outcome 
ggplot(data=data) +
  geom_histogram(alpha=0.6, fill='purple', aes(x=cptu5)) + 
  labs(x='Incidence in Cases per 1000 Under 5', y='Frequency') 
                                              # seems positively skewed
ggplot(data=data) +
  geom_histogram(alpha=0.6, fill='turquoise2', aes(x=log(cptu5))) + 
  labs(x='log(Incidence in Cases per 1000 Under 5)', y='Frequency') 
                                          # does not make better, skewed negatively
ggplot(data=data) +
  geom_histogram(alpha=0.6, fill='deeppink2', aes(x=sqrt(cptu5))) + 
  labs(x=expression(sqrt('Incidence in Cases per 1000 Under 5')), y='Frequency') 
                                                    # seems normally distributed
# If I were to model the data, I would likely square-root transform the outcome

# smoothing splines to understand overall relationships between outcome and 
# total rainfall and average temperature 
g1 <- ggplot(data=data, aes(color=Region)) + 
  geom_point(aes(x=rainTot, y=cptu5)) +
  geom_smooth(aes(x=rainTot, y=cptu5)) +
  labs(x="Weekly Total Rainfall", y="Incidence in Cases per 1000 Under 5")
g2 <- ggplot(data=data, aes(color=Region)) +
  geom_smooth(aes(x=rainTot, y=cptu5)) +
  labs(x="Weekly Total Rainfall", y="Incidence in Cases per 1000 Under 5")
grid.arrange(g1, g2, g3, nrow = 1)

g4 <- ggplot(data=data, aes(color=Region)) + 
  geom_point(aes(x=tavg, y=cptu5)) +
  geom_smooth(aes(x=tavg, y=cptu5)) +
  labs(x="Average Weekly Temperature", y="Incidence in Cases per 1000 Under 5")
g5 <- ggplot(data=data, aes(color=Region)) +
  geom_smooth(aes(x=tavg, y=cptu5)) +
  labs(x="Average Weekly Temperature", y="Incidence in Cases per 1000 Under 5")
grid.arrange(g4, g5, nrow = 1) # potential quadratic/quartic relationship

# correlation plot between rainTot and tavg
ggplot(data=data) + 
  stat_bin2d(aes(x=tavg, y=rainTot)) +
  labs(x="Average Weekly Temperature", y="Weekly Total Rainfall")
                                                  # difficult plot to interpret

cor(data$tavg, data$rainTot) # Pearson correlation = 0.1534
cor(data$tavg, data$rainTot, method = 'spearman') # Spearman correlation = 0.3180
# remember: these two variables may be correlated, 
# but not necessarily linearly correlated

