
test_data <- readRDS("data/generated_test_25_male_190.rds")


# gather, grepl
# The point of this code is to reshape the data in a wide format, 
# where each row corresponds to a different exercise, 
# and each column corresponds to a different variable related to that exercise.

library(dplyr)
library(tidyr)
library(lubridate)

pivot_user_data <- function(test_data){
  # Create an empty list to store the 7 dataframes
  df_list <- list()
  
  # Define the columns that each of the 7 dataframes will have
  columns <- c("date", "exercise_name", "exercise_number", "exercise_sets", "exercise_reps", 
               "exercise_weight", "exercise_volume", "exercise_rank_by_sex", "exercise_percent_by_sex", 
               "exercise_body_total_level", "exercise_age_total_level", "weekday", "month", "week", "day")
  
  # Loop through the 7 exercises
  for (i in 1:7) {
    
    # Define the name of each exercise
    exercise_name <- paste0("exercise_", i, "_name")
    # Define the sets of each exercise
    exercise_sets <- paste0("exercise_", i, "_sets")
    # Define the reps of each exercise
    exercise_reps <- paste0("exercise_", i, "_reps")
    # Define the weight of each exercise
    exercise_weight <- paste0("exercise_", i, "_weight")
    # Define the volume of each exercise
    exercise_volume <- paste0("exercise_", i, "_volume")
    # Define the rank of each exercise
    exercise_rank <- paste0("exercise_", i, "_rank_by_sex")
    # Define the percent of each exercise
    exercise_percent <- paste0("exercise_", i, "_percent_by_sex")
    # Define the body level of each exercise
    exercise_body_level <- paste0("exercise_", i, "_body_total_level")
    # Define the age level of each exercise
    exercise_age_level <- paste0("exercise_", i, "_age_total_level")
    
    # Create a dataframe with the selected columns
    df <- tibble(date = test_data$date, 
                 exercise_name = c(test_data[, exercise_name]) %>% unname() %>% `[[`(1), 
                 exercise_number = i, 
                 exercise_sets = c(test_data[, exercise_sets]) %>% unname() %>% `[[`(1), 
                 exercise_reps = c(test_data[, exercise_reps]) %>% unname() %>% `[[`(1), 
                 exercise_weight = c(test_data[, exercise_weight]) %>% unname() %>% `[[`(1), 
                 exercise_volume = c(test_data[, exercise_volume]) %>% unname() %>% `[[`(1), 
                 exercise_rank_by_sex = c(test_data[, exercise_rank]) %>% unname() %>% `[[`(1), 
                 exercise_percent_by_sex = c(test_data[, exercise_percent]) %>% unname() %>% `[[`(1), 
                 exercise_body_total_level = c(test_data[, exercise_body_level]) %>% unname() %>% `[[`(1), 
                 exercise_age_total_level = c(test_data[, exercise_age_level]) %>% unname() %>% `[[`(1), 
                 weekday = weekdays(as.Date(test_data$date)),
                 month = months(as.Date(test_data$date)),
                 week = week(as.Date(test_data$date)),
                 day = as.numeric(format(as.Date(test_data$date), "%d")))  %>% filter(!is.na(exercise_name))
    
    # Add the dataframe to the list
    df_list[[i]] <- df
  }
  
  pivoted_data <- bind_rows(df_list) %>% arrange(date,exercise_number)
  
  return(pivoted_data)
  
}

