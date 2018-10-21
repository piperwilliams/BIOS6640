##############################################
### Author: Piper Williams 
### BIOS 6640, Project 1
### Purpose: Lags of Temp and Rain
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

# create variable "malaria incidence in cases per 1,000 population in children under 5"
data <- mutate(data,
               cptu5 = (malaria / (Population_UN*u5weight))*1000)
data <- data %>% 
  filter(!(Epiyear == 2017))

############################## Optimal Lag Length #################################
# ccf plots to determine optimal lead length
par(mfrow = c(1,2))
ccf(data$rainTot, data$cptu5, main = "CCF Plot: Total Rainfall vs. CPT-U5") # optimal lead ~ 4
ccf(data$rainTot, data$cptu5, 8, main = "CCF Plot: Total Rainfall vs. CPT-U5")

par(mfrow = c(1,2))
ccf(data$tavg, data$cptu5, main = "CCF Plot: Avg Temperature vs. CPT-U5") # optimal lead ~ 15
ccf(data$tavg, data$cptu5, 20, main = "CCF Plot: Avg. Temperature vs. CPT-U5")

# numerical estimates of max ACF and corresponding lag/lead 
rain.ccf <- ccf(data$rainTot, data$cptu5)
max.acf1 <- max(rain.ccf$acf) # 0.1197 = max ACF
rain.lag <- rain.ccf$lag[which(rain.ccf$acf > max.acf1-0.0001 & rain.ccf$acf < max.acf1+0.0001)] # -4

temp.ccf <- ccf(data$tavg, data$cptu5)
max.acf2 <- max(temp.ccf$acf) # 0.2101 = max ACF
temp.lag <- temp.ccf$lag[which(temp.ccf$acf > max.acf2-0.0001 & temp.ccf$acf < max.acf2+0.0001)] # -16
# I realize that one limitation of this method is the default 
# utilizes Pearson correlation


# create a table summarizing these results
rain.ccf.results <- c(rain.lag, max.acf1)
temp.ccf.results <- c(temp.lag, max.acf2)
labels <- c('Weekly Total Rainfall', 'Average Weekly Temperature')
ccf.results <- rbind(rain.ccf.results, temp.ccf.results)
ccf.results <- as.data.frame(cbind(labels, ccf.results))
rownames(ccf.results) <- NULL
colnames(ccf.results) <- c("", "Lag", "Maximum ACF")

knitr::kable(ccf.results, format = "latex", booktabs = TRUE, 
             align=rep('c')) %>%
  kable_styling(position = "center") %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "8em") %>%
  column_spec(3, width = "8em")

