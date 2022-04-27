


library(tidyverse)

timer <- function()
{ 
  name = ""
  start_time <- 0.0   #The start time of timer
  finish_time <- 0.0  #The finish time of timer
  name <- NULL      
  duration <- 0.0     #The actual time taken
  
  #A four column tibble that stores each result
  all_times <- tibble(        
    Name = character(),
    StartTime = character(),
    FinishTime = character(),
    Duration = character()
  )
  
  #Stores the result
  archive <- function(){
    all_times <<- all_times %>% 
      add_row(Name = name,StartTime = as.character(start_time), FinishTime = 
                as.character(finish_time), Duration = as.character(duration))
  }
  
  #start the timer for a person name
  start <- function(name = "Unknown"){
    name <<- name
    start_time <<- Sys.time()

  }
  
  #Completes the timer
  finish <- function(){
    Sys.sleep(5)     #used to model the passage of time
    finish_time <<- Sys.time()    
    duration <<- finish_time - start_time    #Calculates the time taken
    archive()    #Call to archive function to store the result
  }
  
  get_time <- function(){
    duration      #returns the duration of current timing
  }
  
  get_all_times <- function(){
    all_times      # returns all the results contained in the tibble
  }
  
  #List of functions inside the closure
  list(
    start = start,
    finish = finish,
    get_time = get_time,
    get_all_times = get_all_times
  )
  
}


t <- timer()
str(t)
environment(t$start)$all_times

t$start("Person1")
t$finish()
t$get_time()
t$get_all_times()

t$start("Person2")
t$finish()
t$get_time()
t$get_all_times()
