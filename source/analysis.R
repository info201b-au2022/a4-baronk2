library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")





## Load data frame ---- 


incarceration_trends <- read.csv(file = "C:/Users/Owner/Documents/Kevin/School/University_Of_Washington/Informatics/2022-2023/Autumn_2022/Info_201/Assignments/A04/data/incarceration_trends.csv")
# View(incarceration_trends)




## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Functions to calculate values referenced in Summary Paragraph of `index.Rmd`
#----------------------------------------------------------------------------#

get_total_US_prison_population_2017 <- function() {
  
  total_US_prison_population_2017 <- ""
  
  return(total_US_prison_population_2017)
  
}


get_max_2017_prison_population_state <- function() {
  
  max_2017_prison_population_state <- ""
  
  return(max_2017_prison_population_state)
  
}


get_max_state_2017_prison_population <- function() {
  
  max_state_2017_prison_population <- ""
  
  return(max_state_2017_prison_population)
  
}


get_average_state_2017_prison_population <- function() {
  
  average_state_2017_prison_population <- ""
  
  return(average_state_2017_prison_population)
  
}


get_black_incarceration_disproportionality_US_2017 <- function() {
  
  black_incarceration_disproportionality_US_2017 <- ""
  
  return(black_incarceration_disproportionality_US_2017)
  
}


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#


# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
return()   
}


# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 








## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#










## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#







## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


