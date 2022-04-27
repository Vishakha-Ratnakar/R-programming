library(dplyr)
library(ggplot2)
library(aimsir17)

#1. Gather the following summary annual information for each weather station,
#and store in the tibble annual.

annual <- observations %>% group_by(station) %>%
  summarise(TotalRain = sum(rain,na.rm = T),
            AvrWind = mean(wdsp,na.rm = T),
            AvrTemp = mean(temp, na.rm = T))
print(annual,n=25)

#2. . For each observation, add the ranking (highest to lowest), 
#using the R function rank().
annual <- annual %>% mutate(Rank_Rain = rank(-TotalRain),
                  Rank_Wind = rank(-AvrWind),
                  Rank_Temp = rank(-AvrTemp)) 
annual <- annual %>%
  mutate(Avr_Rank = rowMeans(select(.,Rank_Rain,Rank_Wind,Rank_Temp),na.rm = T))

print(annual,n=25)


#3. Find the values observations with the highest and lowest rank. 

target <- annual %>% filter(Avr_Rank == max(Avr_Rank)| Avr_Rank == min(Avr_Rank))

target


#4. Extract the station names from the tibble, make use of the function pull()

target <- pull(target,station)
target

#5. Based on the variable target filter the observations tibble so that it
#contains only those records for the highest and lowest ranked stations.

my_obs <- observations %>% filter(station %in% target)
my_obs

#6. Plot the following graph showing the distribution of temperature

ggplot(my_obs) +
  geom_freqpoly(mapping = aes(x = temp, color = station),bins = 30)

#7. Plot the following graph showing the distribution of windspeed
ggplot(my_obs,aes(x=station,y=wdsp,colour=station)) +
   geom_boxplot(na.rm = T) 

#8. Plot the following graph showing the distribution of rainfall

my_obs %>% group_by(station,day) %>%
  summarise(Rain = sum(rain, na.rm = T)) %>%
  ggplot(aes(x=station,y=Rain,colour=station)) +
  geom_boxplot()

#9. Generate a summary tibble of monthly statistics for each station.

montly <- my_obs %>% group_by(station,month) %>%
  summarise(Rain = sum(rain,na.rm = T),
            AvrWind = mean(wdsp,na.rm = T),
            AvrTemp = mean(temp, na.rm = T))
print(montly,n=24)

#10. Generate the following plot of rainfall for each month.
ggplot(montly,mapping = aes(x=month,y=Rain,colour=station))+
  geom_point() +
  geom_path() +
  scale_x_continuous(breaks = 1:12) #scale_x_continuous is used to mark default scales


#11. Generate the following plot of temperature for each month.

ggplot(montly,mapping = aes(x=month,y=AvrTemp,colour=station))+
  geom_point() +
  geom_path() +
  scale_x_continuous(breaks = 1:12)

#12. Generate the following plot of windspeed for each month.

ggplot(montly,mapping = aes(x=month,y=AvrWind,colour=station))+
  geom_point() +
  geom_path() +
  scale_x_continuous(breaks = 1:12)
             
 


