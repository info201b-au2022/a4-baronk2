library(tidyverse)
library("dplyr")
library("ggplot2")
library("usmap")


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
# test_query1 <- function() {
#   return ("Hello world")
# }
# 
# # Return a vector of numbers
# test_query2 <- function(num=6) {
#   v <- seq(1:num)
#   return(v)
# }

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Functions to calculate values referenced in Summary Paragraph of `index.Rmd`
#----------------------------------------------------------------------------#

get_total_US_prison_population_2018 <- function() {
  
  total_US_prison_population_2018 <-
    incarceration_trends %>%
    filter(year == 2018) %>% 
    summarize(us_total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    pull(us_total_jail_pop)
    
  return(total_US_prison_population_2018)
  
}


get_max_2018_prison_population_state <- function() {
  
  max_2018_prison_population_state <- 
    incarceration_trends %>%
    filter(year == 2018) %>% 
    group_by(state) %>% 
    summarize(state_total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    filter(state_total_jail_pop == max(state_total_jail_pop, na.rm = TRUE)) %>% 
    pull(state)
  
  return(max_2018_prison_population_state)
  
}


get_max_state_2018_prison_population <- function() {
  
  max_state_2018_prison_population <-  
    incarceration_trends %>%
    filter(year == 2018) %>% 
    group_by(state) %>% 
    summarize(state_total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    filter(state_total_jail_pop == max(state_total_jail_pop, na.rm = TRUE)) %>% 
    pull(state_total_jail_pop)
  
  return(max_state_2018_prison_population)
  
}


get_average_state_2018_prison_population <- function() {
  
  average_state_2018_prison_population <-   
    incarceration_trends %>%
    filter(year == 2018) %>% 
    group_by(state) %>% 
    summarize(state_total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    summarize(state_average_jail_pop = mean(state_total_jail_pop, na.rm = TRUE)) %>% 
    pull(state_average_jail_pop)
  
  return(average_state_2018_prison_population)
  
}


get_black_incarceration_disproportionality_US_2018 <- function() {
  
  black_incarcerated_population <- 
    incarceration_trends %>%
    filter(year == 2018) %>% 
    summarize(us_black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>% 
    pull(us_black_jail_pop)
  
  total_incarcerated_population <- 
    incarceration_trends %>%
    filter(year == 2018) %>% 
    summarize(us_total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
    pull(us_total_jail_pop)
  
  black_incarcerated_representation <- 
    black_incarcerated_population / total_incarcerated_population
  
  black_general_population_15to64 <- 
    incarceration_trends %>%
    filter(year == 2018) %>% 
    summarize(us_black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE)) %>% 
    pull(us_black_pop_15to64)
  
  total_general_population_15to64 <- 
    incarceration_trends %>%
    filter(year == 2018) %>% 
    summarize(us_total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE)) %>% 
    pull(us_total_pop_15to64)
  
  black_general_representation_15to64 <- 
    black_general_population_15to64 / total_general_population_15to64
  
  
  black_incarceration_disproportionality_US_2018 <- 
    black_incarcerated_representation / black_general_representation_15to64
  
  return(black_incarceration_disproportionality_US_2018)
  
}


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#


# This function returns a dataframe with years 1970 to 2018 and the total 
# incarcerated population for each year

get_year_jail_pop <-
  function() {
  
    year_jail_pop_df <-
      incarceration_trends %>%
      group_by(year) %>% 
      summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
      
    return(year_jail_pop_df)
    
  }


# This function returns a bar chart of the data found in `get_year_jail_pop`

plot_jail_pop_for_us <-
  function()  {
  
    plot <-
      ggplot(
        get_year_jail_pop(),
        aes(
          x = year,
          y = total_jail_pop
        )
      ) +
      
      geom_bar(stat = "identity") +
      
      labs(
        title = "Incarcerated Population",
        subtitle = "US (1970-2018)",
        caption = "The generally increasing trend in the incarcerated population in the United States can be seen here."
      ) +
      
      xlab("Year") +
      ylab("Incarcerated Population") +
      
      scale_y_continuous(labels = scales::comma)
    
    return(plot)
    
  } 


# This function returns a summary paragraph about the data displayed in
# `plot_jail_pop_for_us`

get_us_plot_summary <- 
  function() {
    
    return("This plot shows that the total incarcerated population in the United States was fairly static through the 1970s. It steadily increased from circa 1980 to circa 2005, and has plateaued with a slightly declining trend from circa 2005 to 2018. The increasing trend could reflect a few decades of prison construction, crackdowns on crime, and general population growth, while the plateau could signify having reached near maximum capacity in prisons with a reduction in planning for overflow or new prison construction.")
    
  }


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# This function returns a dataframe with years 1970 to 2018 and the total 
# incarcerated population for each year

get_year_jail_by_states <-
  function(states) {
  
    states_jail_pop_df <-
      incarceration_trends %>%
      filter(state %in% states) %>% 
      mutate(
        state_year = 
          paste0(
            state,
            "_",
            year
        )
      ) %>% 
      group_by(state_year) %>%
      summarize(
        total_state_jail_pop = sum(total_jail_pop, na.rm = TRUE)
      ) %>% 
      mutate(state = substr(state_year, 1,2),
             year = substr(state_year, 4, 7)
             ) %>%
      select(-state_year) %>% 
      mutate(total_state_jail_pop_temp = total_state_jail_pop) %>% 
      select(-total_state_jail_pop) %>% 
      mutate(total_state_jail_pop = total_state_jail_pop_temp) %>% 
      select(-total_state_jail_pop_temp) %>% 
      group_by(state)
    
    return(states_jail_pop_df)
  
  }


############

# Test state df

# View(
#   get_year_jail_by_states(
#     c(
#       "CA",
#       "NY",
#       "WA",
#       "WY"
#     )
#   )
# )

############


# This function returns a line plot of the data found in `get_year_jail_by_states`

plot_jail_pop_by_states <-
  function(states)  {
    
    plot <-
      ggplot(
        get_year_jail_by_states(states),
        aes(
          x = year,
          y = total_state_jail_pop,
          color = state,
          group = state
        )
      ) +
      
      geom_path() +
      geom_point(stat = "identity") +
      
      
      labs(
        title = "Incarcerated Population by State",
        subtitle = "(1970-2018)",
        caption = "A comparison of incarcerated populations for California, New York, Washington, and Wyoming\nfrom 1970 to 2018 can be seen here.",
        color = "State"
      ) +
      
      xlab("Year") +
      ylab("Incarcerated Population") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
      
      scale_y_continuous(labels = scales::comma)
    
    return(plot)
    
  } 


############

# Test state plot

# plot_jail_pop_by_states(c("CA", "NY", "WA", "WY"))

############


# This function returns a summary paragraph about the data displayed in
# `plot_jail_pop_by_states`

get_states_plot_summary <- 
  function() {
    
    return("This plot shows the trends of total incarcerated populations for multiple states from 1970 to 2018. The states included are California, New York, Washington, and Wyoming. California is included because it had the largest incarcerated population in 2018. New York is included because it has the largest urban popluation in the country in New York City. Washington is included to give local perspective. Wyoming is included because it has the smallest general population, and this could provide some added perspective to the states with higher incarcerated populations counts. It can be seen that both California and New York both show a distinct turn from downward trends to upward trends, bouncing on the year 1978, while Washington shows more steady growth throughout the period.")
    
  }


## Section 5  ---- 
#----------------------------------------------------------------------------#
# Disproportionality of Black Incarceration
#----------------------------------------------------------------------------#

# This function returns a dataframe with the disproportionality of Black
# incarceration within counties and the populations of those counties for 2018.

get_black_incarceration_disproportionality_counties_2018_df <-
  function() {
    
    black_incarceration_disproportionality_counties_2018_df <-
      incarceration_trends %>%
      filter(year == 2018) %>%
      select(
        state,
        county_name,
        total_pop,
        total_pop_15to64,
        black_pop_15to64,
        total_jail_pop,
        black_jail_pop
      ) %>% 
      mutate(
        black_jail_rep =
          black_jail_pop / total_jail_pop
        ,
        black_general_15to64_rep =
          black_pop_15to64 / total_pop_15to64,
        black_inc_disp = 
          black_jail_rep / black_general_15to64_rep
      ) %>% 
      select(
        state,
        county_name,
        total_pop,
        black_inc_disp
      ) %>% 
      filter(
        total_pop > 0,
        black_inc_disp > 0
      )
    
    return(black_incarceration_disproportionality_counties_2018_df)
    
  }


############

# Test get_black_incarceration_disproportionality_counties_2018_df()

# View(
#   get_black_incarceration_disproportionality_counties_2018_df()
# )

average_disp <-
  mean(
    get_black_incarceration_disproportionality_counties_2018_df()$black_inc_disp
  )


############


# This function returns a scatter plot of the data found in
# `get_black_incarceration_disproportionality_counties_2018_df`

get_black_incarceration_disproportionality_counties_2018_plot <-
  function()  {
    
    plot <-
      ggplot(
        get_black_incarceration_disproportionality_counties_2018_df(),
        aes(
          x = total_pop,
          y = black_inc_disp
        )
      ) +
      
      geom_point() +
      
      
      labs(
        title = "Disproportionality of Black Incarceration in US Counties",
        subtitle = "2018",
        caption = "A comparison of county population to disproportionality of Black incarceration.\nY Values above 1.0 represent a larger percentage of Black incarcerated individuals\nthan are present in the general population (for general population ages 15 to 64)."
      ) +
      
      xlab("County Population\n(log10 display)") +
      ylab("Disproportionality of Black Incarceration\n(log10 display)") +

      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +

      scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::comma) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::comma) +

      coord_trans(x="log10", y="log10") +
      
      geom_line(mapping = aes(y = 1, color = "red")) +
      
      geom_line(mapping = aes(y = average_disp, color = "blue")) +
      
      scale_color_discrete(
        name = 
          "Reference",
        labels =
          c(
            paste0(
              "Counties Average: ",
              round(
                average_disp,
                2
              )
            ),
            "Proportional: 1.0"
          )
      )
      
    return(plot)
    
  } 


############

# Test `get_black_incarceration_disproportionality_counties_2018_plot()`

# get_black_incarceration_disproportionality_counties_2018_plot()

############


# This function returns a summary paragraph about the data displayed in `get_black_incarceration_disproportionality_counties_2018_plot`

get_disproportionality_of_black_incarceration_summary_paragraph <- 
  function() {
    
    return(paste0("Question #3) How does disproportionality of incarceration of Black people vary with county population for all US counties?\n\nIt can be seen from this scatter plot that most counties have Black incarcerated populations that are larger relative to the entire incarcerated population than the Black populations aged 15 to 64 compared to the general populations of the same age. (While data for total population of all ages for each county was available, data for Black populations of all ages was not, so the 15 to 64 age category, where both data sets were available, was used. This is a wide enough age range to capture meaningful data trends). Counties with populations in the middle two quartiles are more likely to have a wide range of Black incarceration disproportionality, with some counties showing less representation of Black people in prisons than in the general population (Y values less than 1.0), and some showing over 100 times more Black representation in prisons than in the general population. As county population increases, the range of Y values narrows and converges, but not on the proportional value of 1.0. Instead, they converge on a value between 1.0 and the average disproportionality of ", round(average_disp, 2), "."))
    
  }


## Section 6  ---- 
#----------------------------------------------------------------------------#
# Disproportionality of Black Incarceration by US County (2018)
#----------------------------------------------------------------------------#

# This function returns a dataframe with the disproportionality of Black
# incarceration within counties other data about the counties for mapping.

get_county_disproportionality_map_df <-
  function() {
    
    county_disproportionality_map_df <-
      incarceration_trends %>%
      filter(year == 2018) %>%
      select(
        fips,
        state,
        county_name,
        total_pop,
        total_pop_15to64,
        black_pop_15to64,
        total_jail_pop,
        black_jail_pop
      ) %>% 
      mutate(
        black_jail_rep =
          black_jail_pop / total_jail_pop
        ,
        black_general_15to64_rep =
          black_pop_15to64 / total_pop_15to64,
        black_inc_disp = 
          black_jail_rep / black_general_15to64_rep
      ) %>% 
      select(
        fips,
        state,
        county_name,
        total_pop,
        black_inc_disp
      ) %>% 
      filter(
        total_pop > 0,
        black_inc_disp > 0
      ) %>% 
      mutate(
        log10_black_inc_disp = 
          log10(black_inc_disp)
      )
    
    return(county_disproportionality_map_df)
    
  }


############

# Test `get_county_disproportionality_map_df()`

# View(
#   get_county_disproportionality_map_df()
# )


############


# This function returns a map of the data found in
# `get_county_disproportionality_map_df()`

get_county_disproportionality_map <-
  function()  {
    
    map <- 
      plot_usmap(
        regions = "counties",
        data = get_county_disproportionality_map_df(),
        values = "log10_black_inc_disp"
      ) +
      labs(
        title = "Disproportionality of Black Incarceration by US County",
        subtitle = "(2018)",
        caption = "Darker shades represent counties where Black incarcerated population representation is higher than\nBlack representation in the general population."
      ) + 
        
      theme(
        panel.background =
          element_rect(
            color =
              "black"
          )
      ) +
      
        
      scale_fill_continuous(
        name = "Black Incarceration\nDisproportionality\n(log10(x) display)",
        label = scales::comma,
        low = "white",
        high = "black"
      ) +
      theme(legend.position = "right")
    
    return(map)
    
  } 


############

# Test `get_county_disproportionality_map()`

# get_county_disproportionality_map()

############


# This function returns a summary paragraph about the data displayed in `get_county_disproportionality_map()`

get_county_disproportionality_map_summary_paragraph <- 
  function() {
    
    return("Question #4) Where are the counties with the most disproportional Black incarceration rates located across the country?\n\nLight gray (corresponding with 0 value) represents 10^0 = 1.0 disproportionality, or perfect perfect proportionality. Dark gray and black shades, near values of 2.0, represents disproportionality of 10^2 = 100, which is severely disproportionate. This is the case in various single counties throughout the country, with no particularly visible pattern as to where they are concentrated.")
    
  }
