# Load libraries
library(rvest)
library(tidyverse)
library(stringr)
library(janitor)

# Define the URL of the main page
url <- "https://strengthlevel.com/strength-standards"

# Read the HTML content of the main page
html_content <- read_html(url)

# Extract the URLs of the exercise pages
exercise_icons <- html_content %>%
  html_nodes("[class='column is-half-mobile is-one-third-tablet is-one-quarter-desktop exerciseitem']")  %>%
  html_nodes("figure")  %>%
  html_nodes("img") %>%
  html_attr("src") 

exercise_names <- html_content %>%
  html_nodes("[class='column is-half-mobile is-one-third-tablet is-one-quarter-desktop exerciseitem']")  %>%
  html_nodes("figure")  %>%
  html_nodes("img") %>%
  html_attr("alt") 

exercise_images <- exercise_icons %>% 
  str_replace( "256x256", "1000x1000") %>%
  str_replace( "silhouettes", "illustrations")  %>%
  str_replace( "png", "jpg") 

exercise_urls <- html_content %>%
  html_nodes("[class='column is-half-mobile is-one-third-tablet is-one-quarter-desktop exerciseitem']")  %>%
  html_nodes("a") %>% 
  html_attr("href") 

exercise_data <- data.frame(exercise_names, exercise_urls, exercise_icons, exercise_images)

# Initialize an empty list to store the data
levels_by_body_total <- NULL
levels_by_age_total  <- NULL
sets_and_reps_total  <- NULL
strength_total  <- NULL
error_causing_urls <- c()
# Loop over the exercise URLs
for ( exercise_url in exercise_data$exercise_urls) {
  
  tryCatch(
    {
      exercise_name <- sub(".*standards/", "", exercise_url)  
      
      #https://strengthlevel.com/strength-standards/bench-press
      # Read the HTML content of the exercise page
      tables <- read_html(paste0("https://strengthlevel.com", exercise_url))  %>%  
        html_table(fill = TRUE) 
      # exercise_images <- append(x, -)
      # read_html(paste0("https://strengthlevel.com", exercise_url))  %>%
      #   html_nodes("figure")  %>%
      #   html_nodes("img") %>%
      #   html_attr("src") %>% head(2)  %>% tail(1)
      strength_weight <- tables[[1]] %>% clean_names()
      strength_ratio <- tables[[2]] %>% clean_names()
      strength_ratio$sex <- "male"
      strength_male <-  merge(strength_weight, strength_ratio, 
                              by = "strength_level")
      
      levels_by_body_w_m <- tables[[3]] %>% clean_names()
      levels_by_body_w_m$sex <- "male"
      levels_by_age_m <- tables[[4]] %>% clean_names()
      levels_by_age_m$sex <- "male"
      
      sets_and_reps_m <- tables[[5]] %>% clean_names()
      colnames(sets_and_reps_m) <- paste0(colnames(sets_and_reps_m),"_by_sex") 
      sets_and_reps_m$sex <- "male"
      
      strength_weight <- tables[[6]] %>% clean_names()
      strength_ratio <- tables[[7]] %>% clean_names()
      strength_ratio$sex <- "female"
      strength_female <-  merge(strength_weight, strength_ratio, 
                                by = "strength_level")
      
      levels_by_body_w_f <- tables[[8]] %>% clean_names()
      levels_by_body_w_f$sex <- "female"
      levels_by_age_f <- tables[[9]] %>% clean_names()
      levels_by_age_f$sex <- "female"
      
      sets_and_reps_f <- tables[[10]] %>% clean_names()
      colnames(sets_and_reps_f) <- paste0(colnames(sets_and_reps_f),"_by_sex") 
      sets_and_reps_f$sex <- "female"
      
      
      levels_by_body <- rbind(levels_by_body_w_m,levels_by_body_w_f)
      levels_by_body$exercise_name <- exercise_name
      levels_by_age <- rbind(levels_by_age_m,levels_by_age_f)
      levels_by_age$exercise_name <- exercise_name
      sets_and_reps <- rbind(sets_and_reps_m,sets_and_reps_f)
      sets_and_reps$exercise_name <- exercise_name
      strength <- rbind(strength_male,strength_female)
      strength$exercise_name <- exercise_name
      
      levels_by_body_total <- rbind(levels_by_body_total,levels_by_body)
      levels_by_age_total  <-  rbind(levels_by_age_total,levels_by_age)
      sets_and_reps_total  <-  rbind(sets_and_reps_total,sets_and_reps)
      strength_total  <-  rbind(strength_total,strength)  

    },
    error=function(cond) {
      message(paste("URL causing error:", paste0("https://strengthlevel.com", exercise_url)))
      error_causing_urls <- append(exercise_url, error_causing_urls)
      message("Here's the original error message:")
      message(cond)
      #return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      #return(NULL)
    }

  )    
  
  

}

definition_of_levels <- tables[[11]]
strength_total
sets_and_reps_total
levels_by_body_total
levels_by_age_total


