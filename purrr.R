library(aimsir17)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)



#1. Calculate the average daily windspeed for all stations.

daily_wind <- observations %>% 
  group_by(station,month,day) %>%
  summarise(AvrWind = mean(wdsp, na.rm = T))

daily_wind

#2. Calculate the average daily wind power generated nationally.

daily_gen <- eirgrid17 %>%
  group_by(month,day) %>%
  summarise(AvrGen = mean(IEWindGeneration,na.rm = T) )

daily_gen

#3. Join the two tibbles, making sure that the resulting tibble is ungrouped.

ds <- full_join(daily_wind,daily_gen) %>% ungroup()
ds

#4. Remove any incomplete cases from the dataset.

summary(ds)

ds_clean <- ds[complete.cases(ds),]
ds_clean

#5. Plot the following x-y values, along with a linear model for each station

ds_clean %>% ggplot(data=ds_clean,mapping = aes(x = AvrWind, y= AvrGen)) +
  geom_point() +
  geom_smooth(formula = y ~ x,method = "lm") +
  facet_wrap(~station)

#6. Create a nested tibble for each station. Note each data 
#value is a tibble of 365 x 4, which is as expected.

ds_n <- ds_clean %>%
  group_by(station) %>%
  nest()
ds_n

#7. . Add four new columns to the dataset, via a succession of mutate 
#calls, and the use of map_* functions from purrr. These new columns
#will include: the linear model for each station (independent variable is
#AvrWind, dependent variable AvrGen), the intercept coefficient
#(beta_0), the slope coefficient (beta_1),and the RSquared value. 
#Sort the tibble so that the best RSquared values are shown.

ds_n<- ds_n %>% mutate(EnerMod = map(data,~lm(AvrGen ~ AvrWind,data=.))) %>%
  mutate(Beta_0 = map_dbl(EnerMod,~.$coefficients[1])) %>%
  mutate(Beta_1 = map_dbl(EnerMod,~.$coefficients[2])) %>%
  mutate(R_Squared = map_dbl(EnerMod,~summary(.)$r.squared)) %>%
  arrange(desc(R_Squared))
ds_n



