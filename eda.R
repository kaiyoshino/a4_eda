library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(vioplot)


# load data
cities <- read.csv('data/data.csv')


# change variables to numerics
cities$population <- as.numeric(as.character(cities$population))
cities$violent_crime <- as.numeric(as.character(cities$violent_crime))
cities$murder_and_non_negligent_man_slaughter <- as.numeric(as.character(cities$murder_and_non_negligent_man_slaughter))
cities$forcible_rape <- as.numeric(as.character(cities$forcible_rape))
cities$robbery <- as.numeric(as.character(cities$robbery))
cities$aggravated_assault <- as.numeric(as.character(cities$aggravated_assault))
cities$property_crime <- as.numeric(as.character(cities$property_crime))
cities$burglary <- as.numeric(as.character(cities$burglary))
cities$larceny_theft <- as.numeric(as.character(cities$larceny_theft))
cities$motor_vehicle_theft <- as.numeric(as.character(cities$motor_vehicle_theft))
cities$arson<- as.numeric(as.character(cities$arson))
cities$crime_index_total <- as.numeric(as.character(cities$crime_index_total))
cities$modified_crime_index_total <- as.numeric(as.character(cities$modified_crime_index_total))

# change NA variables to 0
cities[is.na(cities)] <- 0

# remove cities with zero population - bad data rows
cities <- cities[apply(cities["population"],1,function(z) !any(z==0)),] 

# view data set
View(cities)

# determine shape
dim(cities)

# view column names
colnames(cities)

# data types of columns
sapply(cities, class)

# Summary of each column
summary(cities)

# view unique cities
unique(cities$city)


######################## Univariate analysis

hist(cities$year)
hist(cities$population)
hist(cities$violent_crime)
hist(cities$murder_and_non_negligent_man_slaughter)
hist(cities$forcible_rape)
hist(cities$robbery)
hist(cities$aggravated_assault)
hist(cities$property_crime)
hist(cities$burglary)
hist(cities$larceny_theft)
hist(cities$motor_vehicle_theft)
hist(cities$arson)

# Violin plot for each variable
vioplot(
  cities$violent_crime,
  cities$murder_and_non_negligent_man_slaughter,
  cities$forcible_rape, 
  cities$robbery,
  cities$aggravated_assault,
  cities$property_crime,
  cities$burglary,
  cities$larceny_theft,
  cities$motor_vehicle_theft,
  cities$arson,
  names=c('Violent Crime', 'Murder', 'Rape', 'Robbery', 'Aggravated Assault', 'Property Crime', 'Burglary', 'Larceny/Theft', 'Vehicle Theft', 'Arson')
)

# Boxplots for each variable (need to reshape first)
long.data <- gather(cities, count, value, violent_crime, murder_and_non_negligent_man_slaughter, forcible_rape, robbery, aggravated_assault, property_crime, burglary, larceny_theft, motor_vehicle_theft, arson)
boxplot(value ~ count, data = long.data, las=2)





########################### Univariate Analysis by Population

small.pop <- cities %>% filter(population <= 5000)
med.small.pop <- cities %>% filter(population >= 5001 & population <= 15000)
med.large.pop <- cities %>% filter(population >= 15001 & population <= 75000)
large.pop <- cities %>% filter(population >= 75001)

# aggravated assault by population size
vioplot(
  small.pop$aggravated_assault, 
  med.small.pop$aggravated_assault, 
  med.large.pop$aggravated_assault, 
  large.pop$aggravated_assault,
  names=c('small','med-small', 'med-large','large'))

# number of people in a given city per aggravated assault charge
vioplot(
  small.pop$aggravated_assault / small.pop$population, 
  med.small.pop$aggravated_assault / med.small.pop$population, 
  med.large.pop$aggravated_assault / med.large.pop$population, 
  large.pop$aggravated_assault / large.pop$population,
names=c('small','med-small', 'med-large','large'))

vioplot(
  small.pop$murder_and_non_negligent_man_slaughter, 
  med.small.pop$murder_and_non_negligent_man_slaughter, 
  med.large.pop$murder_and_non_negligent_man_slaughter, 
  large.pop$murder_and_non_negligent_man_slaughter,
  names=c('small','med-small', 'med-large','large'))

vioplot(
  small.pop$motor_vehicle_theft, 
  med.small.pop$motor_vehicle_theft, 
  med.large.pop$motor_vehicle_theft, 
  large.pop$motor_vehicle_theft,
  names=c('small','med-small', 'med-large','large'))

vioplot(
  small.pop$forcible_rape, 
  med.small.pop$forcible_rape, 
  med.large.pop$forcible_rape, 
  large.pop$forcible_rape,
  names=c('small','med-small', 'med-large','large'))




########### Univariate Analysis by Year

p <- qplot(aggravated_assault, data = cities, geom = "histogram")
p + facet_wrap(~ year)

p <- qplot(murder_and_non_negligent_man_slaughter, data = cities, geom = "histogram")
p + facet_wrap(~ year)

p <- qplot(forcible_rape, data = cities, geom = "histogram")
p + facet_wrap(~ year)

p <- qplot(robbery, data = cities, geom = "histogram")
p + facet_wrap(~ year)

p <- qplot(larceny_theft, data = cities, geom = "histogram")
p + facet_wrap(~ year)

p <- qplot(arson, data = cities, geom = "histogram")
p + facet_wrap(~ year)

p <- qplot(burglary, data = cities, geom = "histogram")
p + facet_wrap(~ year)



################ Bivariate Analysis
pairs(cities[,8:ncol(cities)])




