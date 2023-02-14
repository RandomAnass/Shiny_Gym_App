# Simulation Data:
library(tidyverse)

data_list <- readRDS("data/harvested_data.rds")


  # Load the data
  strength_total <- data_list$strength_total
  levels_by_body_total <- data_list$levels_by_body_total
  levels_by_age_total <- data_list$levels_by_age_total
  sets_and_reps_total <- data_list$sets_and_reps_total
  strength_total <- data_list$strength_total
  definition_of_levels <- data_list$definition_of_levels
    
# Create a function to get the strength level based on weight and bodyweight ratio
get_strength_level <- function(exercise_name, sex, weight=NA, age=NA) {
  
  if (!is.na(weight)) {
    strength_level <- levels_by_body_total[
      which(levels_by_body_total$bw == weight & 
              levels_by_body_total$sex == sex &
              levels_by_body_total$exercise_name == exercise_name), 
      c("beg", "nov", "int", "adv", "elite")
    ]
  } else if (!is.na(age)) {
    strength_level <- levels_by_age_total[
      which(levels_by_age_total$age == age & 
              levels_by_age_total$sex == sex &
              levels_by_age_total$exercise_name == exercise_name), 
      c("beg", "nov", "int", "adv", "elite")
    ]
  } else {
    stop("Please provide either weight or age.")
  }
  
  return(strength_level)
}



get_exercise_popularity <- function(sets, reps, user_sex, current_exercise_name) {
  # Filter the data to match the input exercise, sex, sets and reps
  filtered_data <- sets_and_reps_total %>%
    filter(sets_by_sex == sets & reps_by_sex == reps & sex == user_sex & exercise_name == current_exercise_name)
  
  # Extract the rank and percent from the filtered data
  rank_by_sex <- filtered_data$rank_by_sex[1]
  percent_by_sex <- filtered_data$percent_by_sex[1]
  
  # Return the rank and percent as a list
  return(list(rank_by_sex = rank_by_sex, percent_by_sex = percent_by_sex))
}



generate_user_dataframe <- function(user_code = "test", age, sex, weight) {
  
  # Generate the date range for a year
  date_range <- seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-31"), by = "days")
  
  # Create a dataframe to store the training data for the user
  training_data <- data.frame(date = date_range)
  
  # Loop through the date range
  for (i in 1:length(date_range)) {
    
    # Check if there was training on the day
    if (runif(1) > 0.5) {
      
      # Generate the number of exercises for the day
      num_exercises <- sample(4:7, 1)
      
      # Loop through the number of exercises
      for (j in 1:num_exercises) {
        
        # Generate the exercise name
        exercise_name <- sample(strength_total[1:900, ]$exercise_name, 1)
        
        # Generate the number of sets and reps
        sets <- sample(3:5, 1)
        reps <- sample(6:12, 1)
        
        # Call the function get_exercise_popularity to get the popularity data
        popularity_data <- get_exercise_popularity(sets, reps, sex, exercise_name)
     
        
        # Generate the weight used
        weight_used <- ceiling(sample(40:120, 1) + i*0.5)
        
        # Calculate the volume
        volume <- sets * reps * weight_used
        
        # Get the strength level based on weight and bodyweight ratio
        bodyweight_ratio <- weight_used / weight

        body_total_level <- get_strength_level( exercise_name, sex, weight= weight)
        # Get the strength level based on age
        age_total_level <- get_strength_level(exercise_name, sex, age= age)
        
        # Add the exercise data to the training data dataframe

        training_data[i, paste0("exercise_", j, "_name")] <- exercise_name
        training_data[i, paste0("exercise_", j, "_sets")] <- sets
        training_data[i, paste0("exercise_", j, "_reps")] <- reps
        training_data[i, paste0("exercise_", j, "_weight")] <- weight_used
        training_data[i, paste0("exercise_", j, "_volume")] <- volume
        training_data[i, paste0("exercise_", j, "_rank_by_sex")] <- popularity_data$rank_by_sex
        training_data[i, paste0("exercise_", j, "_percent_by_sex")] <- popularity_data$percent_by_sex
        training_data[i, paste0("exercise_", j, "_body_total_level")] <- colnames(body_total_level[which.min(abs(body_total_level - weight_used))])
        training_data[i, paste0("exercise_", j, "_age_total_level")] <-  colnames(age_total_level[which.min(abs(age_total_level - weight_used))])
        # bodyweight_ratio
        # age
        # sex
        # user_code
      }
    }
  }
  return(training_data)
}

# Test - example 
test_data <- generate_user_dataframe(user_code = "test", 25, "male", 190)  

test_data <- test_data %>% rowwise() %>%  mutate(day_volume = sum(exercise_1_volume, exercise_2_volume, exercise_3_volume,exercise_4_volume,exercise_5_volume,exercise_6_volume,exercise_7_volume, na.rm = TRUE))

view(test_data)
saveRDS( test_data, file = "data/generated_test_25_male_190.rds")