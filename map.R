library(purrr)
library(dplyr)
library(ggplot2)


#2. Create the tibble d1, which is mpg grouped by class.
d1 <- mpg %>% group_by(class)
d1
  
#3. Create, and then call, the function my_mpg_lms(). The output is copied 
#to the variable mods1, and this contains a list of 7 elements, where 
#each element contains the results of the linear model for each class
#of car. Note that the name of each list element must be the class of the car.
#The function group_keys() provides useful information on the keys for
#a grouped data tibble.


my_mpg_lms<-function(x){
  
  linear_model <- d1 %>% 
    group_split() %>%
    map(~ lm(cty ~ displ, data = .)) 
  
  names(linear_model) <- d1 %>% group_keys() %>% unlist()

  structure( linear_model ,class="my_mpg_lms") 
  
}

mods1 <- my_mpg_lms(d1)

length(mods1)
class(mods1)
mods1

#3.Write a summary function that generates the following output

summary.my_mpg_lms <- function(x){
  
  cat("The following are the model groups\n",
    names(x),
    "\n\nHere are the results...\n") 

  i <- 0
  walk2(x,names(x), ~{
    cat("Model#", i+1, "Group", .y, "Obs =", nrow(.x$model),"\n")
    print(summary(.x))
    i<<-i+1
    cat("==================================================================\n\n")
  })
}
summary(mods1)





