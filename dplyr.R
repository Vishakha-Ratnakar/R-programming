library(dplyr)
library(ggplot2)
library(nycflights13)
library(lubridate)

#1.Select the following columns from the flights tibble

my_flights <- flights %>% select(time_hour,origin,carrier,dep_delay)
my_flights

#2. Using the lubridate package, process the time_hour
#column to add the following additional columns

my_flights <- my_flights %>%
  mutate(month = month(time_hour),
         month_name = month(time_hour,label = TRUE),
         day = mday(time_hour),
         hour = hour(time_hour))
my_flights

#3. Perform a join operation with the airlines
#tibble to get the following tibble.

my_data <- inner_join(my_flights,airlines,by="carrier")
my_data

#4. Remove any incomplete records (hint, using complete.cases())

my_data <- my_data %>% filter(complete.cases(.))
my_data

#5. Get the top six airlines in terms of flight frequency from the 
#cleaned dataset

top_6 <- my_data %>%
  group_by(name) %>%
  summarise(TotalFlights = n()) %>%
  arrange(desc(TotalFlights)) %>%
  top_n(6)
  
top_6

#6. Use a filtering join to filter these airlines from the cleaned data set.

final_data <- semi_join(my_data,top_6,by="name")
final_data

#7. Using the quantile function, Calculate the 95% intervals for
#departure times for each month of the year.

quant_month_95 <- final_data %>%
  group_by(month_name) %>%
  summarise(Q2.5 = quantile(dep_delay, prob = 0.025),
            Q97.5 = quantile(dep_delay, prob = 0.975))

quant_month_95

#8. Calculate the 95% intervals for departure times for each carrier

quant_carrier_95 <- final_data %>%
  group_by(name) %>%
  summarise(Q2.5 = quantile(dep_delay, prob = 0.025),
            Q97.5 = quantile(dep_delay, prob = 0.975))

quant_carrier_95

#9. Calculate the 95% intervals for departure delays for each airport

quant_airport_95 <- final_data %>%
  group_by(origin) %>%
  summarise(Q2.5 = quantile(dep_delay, prob = 0.025),
            Q97.5 = quantile(dep_delay, prob = 0.975))

quant_airport_95

#10. Create your own plot from final_data that shows an insightful relationship.
#We have plotted a graph that shows the 
#average departure delay of each airline for every month.
graph <- final_data %>% group_by(month_name,name) %>%
  summarise(Avr_Dep_delay = mean(dep_delay,na.rm = T)) %>%
  ggplot(aes(x=month_name,y=Avr_Dep_delay,color = name, group = name)) +
  geom_point() + geom_path() + facet_wrap(~name)
                            
graph



