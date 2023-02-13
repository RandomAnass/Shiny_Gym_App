# year simulation

# paquetes
library(tidyverse)
library(lubridate)
library(ragg)

test_data

test_data <- test_data %>% 
  mutate(weekday = wday(date, label = T, week_start = 1), 
         month = month(date, label = T, abbr = F),
         week = isoweek(date),
         day = day(date)) %>%
  mutate(week = case_when(month == "December" & week == 1 ~ 53,
                                  month == "January" & week %in% 52:53 ~ 0,
                                  TRUE ~ week),
                          pcat = cut(day_volume, 50)) 

test_data <- test_data %>% mutate(  pcat = cut(day_volume, 14))
                  

library(calendR)

scale_data <- function(x) {                              # Create user-defined function
 365* ((x - min(x)) / (max(x) - min(x)))
}
calendR(year = 22,
        special.days = scale_data(test_data$day_volume), # Vector of the same length as the number of days of the year
        gradient = TRUE,      # Set gradient = TRUE to create the heatmap
        special.col = rgb(0, 0, 1, alpha = 1), # Color of the gradient for the highest value
        low.col = "white")


#test_data$month

calendR(year = 22,
        special.days = scale_data(test_data$day_volume), # Vector of the same length as the number of days of the year
        gradient = TRUE,      # Set gradient = TRUE to create the heatmap
        special.col = rgb(0, 0, 1, alpha = 1), # Color of the gradient for the highest value
        low.col = "white")


gg <- ggcal(test_data$date, test_data$day_volume) +
  scale_fill_gradient2(low="#ffffff", mid="#4d4dff", high="#000066", 
                       midpoint=mean(test_data$day_volume))

print(gg)

