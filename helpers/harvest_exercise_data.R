library(rvest)
library(tidyverse)
library(stringr)
library(janitor)
library(RSelenium)


# Define the URL of the main page
rD <- rsDriver(port = sample(7600)[1], browser = c("firefox"), chromever = NULL)
remDr <- rD[["client"]]
remDr$navigate("https://strengthlevel.com/strength-standards")
remDr$maxWindowSize()

# Locate the pop-up for accepting cookies
popup_element <- remDr$findElement('xpath', "/html/body/div[1]/div/div/div/div[2]/div/button[2]")

# If the pop-up is found, click on it
if (!is.null(popup_element)) {
  popup_element$clickElement()
}



# Check if the button is disabled
checkButton <- function() {
  webElem1 <- remDr$findElement('xpath', "/html/body/section/div/div[6]/section[2]/div[2]/button")
  attr <- tryCatch(webElem1$getElementAttribute("disabled"), error = function(e) "")
  if (attr == "" || length(attr)==0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


# Click the Load More button
while (!checkButton()) {

  webElem1 <- remDr$findElement('xpath', "/html/body/section/div/div[6]/section[2]/div[2]/button")
  remDr$executeScript("arguments[0].scrollIntoView(true);", list(webElem1))
  Sys.sleep(1)
  webElem1$clickElement()
  Sys.sleep(1)
  
}


html_content <- remDr$getPageSource()[[1]]
html_content <- read_html(html_content)


# url <- "https://strengthlevel.com/strength-standards"
# 
# # Read the HTML content of the main page
# html_content <- read_html(url)

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

# Initialize an empty tables to store the data
levels_by_body_total <- NULL
levels_by_age_total  <- NULL
sets_and_reps_total  <- NULL
strength_total  <- NULL
error_causing_urls <- c()
# Loop over the exercise URLs
for ( exercise_url in exercise_data$exercise_urls) {
  Sys.sleep(0.25)
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
      error_causing_urls <<- append(exercise_url, error_causing_urls)
      message(paste("/n URL causing error:", paste0("https://strengthlevel.com", exercise_url)))
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

remDr$close()
remDr$quit()

definition_of_levels <- tables[[9]]

# definition_of_levels
# error_causing_urls
# strength_total
# sets_and_reps_total
# levels_by_body_total
# levels_by_age_total


saveRDS(list(definition_of_levels = definition_of_levels, error_causing_urls = error_causing_urls,strength_total = strength_total, sets_and_reps_total = sets_and_reps_total,levels_by_body_total = levels_by_body_total, levels_by_age_total = levels_by_age_total) , file = "harvested_data.rds")
# Restore the object
#list_data <- readRDS(file = "harvested_data.rds")


# Making the extraction as a function:

harvested_data <- function(save = F) {

  
  # Define the URL of the main page
  rD <- rsDriver(port = sample(7600)[1], browser = c("firefox"), chromever = NULL)
  remDr <- rD[["client"]]
  remDr$navigate("https://strengthlevel.com/strength-standards")
  remDr$maxWindowSize()
  
  # Locate the pop-up for accepting cookies
  popup_element <- remDr$findElement('xpath', "/html/body/div[1]/div/div/div/div[2]/div/button[2]")
  
  # If the pop-up is found, click on it
  if (!is.null(popup_element)) {
    popup_element$clickElement()
  }
  
  
  
  # Check if the button is disabled
  checkButton <- function() {
    webElem1 <- remDr$findElement('xpath', "/html/body/section/div/div[6]/section[2]/div[2]/button")
    attr <- tryCatch(webElem1$getElementAttribute("disabled"), error = function(e) "")
    if (attr == "" || length(attr)==0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
  
  # Click the Load More button
  while (!checkButton()) {
    
    webElem1 <- remDr$findElement('xpath', "/html/body/section/div/div[6]/section[2]/div[2]/button")
    remDr$executeScript("arguments[0].scrollIntoView(true);", list(webElem1))
    Sys.sleep(1)
    webElem1$clickElement()
    Sys.sleep(1)
    
  }
  
  
  html_content <- remDr$getPageSource()[[1]]
  html_content <- read_html(html_content)
  
  
  # url <- "https://strengthlevel.com/strength-standards"
  # 
  # # Read the HTML content of the main page
  # html_content <- read_html(url)
  
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
  
  # Initialize an empty tables to store the data
  levels_by_body_total <- NULL
  levels_by_age_total  <- NULL
  sets_and_reps_total  <- NULL
  strength_total  <- NULL
  error_causing_urls <- c()
  # Loop over the exercise URLs
  for ( exercise_url in exercise_data$exercise_urls) {
    Sys.sleep(0.25)
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
        error_causing_urls <<- append(exercise_url, error_causing_urls)
        message(paste("/n URL causing error:", paste0("https://strengthlevel.com", exercise_url)))
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
  
  remDr$close()
  remDr$quit()
  
  definition_of_levels <- tables[[9]]

  
  data_list <- list(definition_of_levels = definition_of_levels, error_causing_urls = error_causing_urls,strength_total = strength_total, sets_and_reps_total = sets_and_reps_total,levels_by_body_total = levels_by_body_total, levels_by_age_total = levels_by_age_total)
  
  if(save){
    saveRDS( data_list, file = "harvested_data.rds")
    
  }
  
  return(data_list)


  
  
}