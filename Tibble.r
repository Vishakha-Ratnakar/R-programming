
#Step (1)
#Load the following libraries
library(aimsir17)
library(tibble)

observations

#vector for date column
dates <- unique(observations$date)


#vector for temperature of station = MACE HEAD column
temp_MH <- observations[observations$station=="MACE HEAD",]$temp

#vector for temperature of station = DUBLIN AIRPORT column
temp_DA <- observations[observations$station=="DUBLIN AIRPORT",]$temp

#vector for temperature of station = SherkinIsland column
temp_SI <- observations[observations$station=="SherkinIsland",]$temp

str(dates)
str(temp_MH)
str(temp_DA)
str(temp_SI)

#Using the Base R function plot() followed by points(), 
#create the following graph. Use col=1 for Mace Head
#(Black), col=2 for Dublin Airport (Red) and col=3 for Sherkin Island (Green).


plot(dates,temp_MH)
points(dates,temp_DA,col= 2)
points(dates,temp_SI,col=3)

#From these four atomic vectors, create a tibble.
#Note there are 24 x 365 = 8760 rows.
data_set <- tibble(Date = dates,
             MaceHead = temp_MH,
             DublinAirport = temp_DA,
             SherkinIsland = temp_SI)
data_set

#Make a copy of this tibble in order to have a record of 
#the input data set. Note that this tibble excludes the date,
#in order to simplify the remaining steps
input_data <- tibble(MaceHead = temp_MH,
                     DublinAirport = temp_DA,
                     SherkinIsland = temp_SI)
input_data

#Minimum temperature for each observation is recorded using which.min() function
data_set$MinTemp <- as.factor(names(input_data)[apply(input_data,1,FUN = which.min)])
data_set

#Minimum temperature for each observation is recorded using which.max() function
data_set$MaxTemp <- as.factor(names(input_data)[apply(input_data,1,FUN = which.max)])
data_set

# percentage of times each station was either the minimum or maximum observation.
min_stations <- round((table(data_set$MinTemp)/length(data_set$MinTemp))*100,digit=2)

max_stations <- round((table(data_set$MaxTemp)/length(data_set$MaxTemp))*100,digit=2)

min_stations
max_stations

#summary function
summary(data_set)



