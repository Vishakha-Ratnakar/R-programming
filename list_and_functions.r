
# Lists and functions

library(repurrrsive)

my_list <- sw_people

## This function takes in the list and then returns a
##modified list that contains (1) the name and (2) the
##height. Where the height is missing, it should
##convert that to NA
r <- list() ## temporary list

get_sw_people_heights <- function(my_list) 
{
  for(i in my_list)
  {
    suppressWarnings(i$height <- as.numeric(i$height)) #as.numeric is used to change the datatype of variable height from character to integer
    el_height <- replace(i$height,i$height=="unknown",is.na<- NA)#replace unknown by NA
    new_list <- list(name = i$name, height = el_height)
    r <- append(r, list(new_list))
  }
  return(r)
}

data_set <- get_sw_people_heights(my_list)
str(data_set[1:2])


##This function takes in the list with just the names
##and heights, and removes any NA value. A logical
##vector (where TRUE is a valid value, and FALSE is
##a missing value) should be created which can
##then be applied to the list (inside the function)

r <- list() #temporary list
logical_v <- vector(mode="logical")
clear_heights <- function(data_set)
{
  for(i in data_set)
  {
    logical_v <- append(logical_v,!is.na(i$height))
  }
  r <- data_set[logical_v==TRUE]
  return(r)
}

clean_data_set <- clear_heights(data_set)

length(data_set)
length(clean_data_set)


##The function extracts those values whose
##heights are within the range.

r <- vector(mode = "list") # temparory list
get_height_range <- function(clean_data_set, min_height,max_height)
{
  for(i in clean_data_set)
  {
     if(i$height >= min_height & i$height <= max_height)
     {
       r <- append(r,list(i))
     }
  }
  return(r)
}

range <- get_height_range(clean_data_set,174,175)
range

##Tidies the output from the previous function. Note
##that the result is a vector of character strings.

r <- vector() #temporary vector
tidy_output <- function(range)
{
  for( i in range)
  {
    r <- append(r,paste0("Name = <",i$name,"> Height = ",i$height))
  }
  return(r)
}

result <- tidy_output(range)
result










