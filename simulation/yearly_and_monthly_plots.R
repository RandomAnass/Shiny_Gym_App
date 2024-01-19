# TO DO: Finish style/design and make general functions:
# year simulation

library(tidyverse)
library(lubridate)
library(ggplot2)
library(calendR)
library(ragg)
Sys.setlocale("LC_ALL", "English")
test_data <- readRDS("data/generated_test_25_male_190.rds")

plot_yearly_calendar <- function(test_data) {
dfPlot <- test_data %>% 
  mutate(weekday = wday(date, label = T, week_start = 7), # can put week_start = 1 to start week on Monday
         month = month(date, label = T),
         week = as.numeric(strftime(date, format = "%U")) + 1)

dfPlot <- dfPlot %>% 
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week))

ggplot(dfPlot, aes(weekday,-week, fill = day_volume)) +
  geom_tile(colour = "#ecf0f5")  + 
  geom_text(aes(label = day(date)), size = 3) +
  theme(aspect.ratio = 1/2,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.background = element_rect(fill = "#ecf0f5")) +
  scale_fill_gradient2(midpoint = 0.5) +
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Calendar heatmap 2022") 
}

plot_yearly_calendar_ <- function(data) {
  scale_data <- function(x) {
    365 * ((x - min(x)) / (max(x) - min(x)))
  }
  
  data2 <- data %>% 
    mutate(weekday = wday(date, label = T, week_start = 1), 
           month = month(date, label = T, abbr = F),
           week = isoweek(date),
           day = day(date)) %>%
    mutate(week = case_when(month == "December" & week == 1 ~ 53,
                            month == "January" & week %in% 52:53 ~ 0,
                            TRUE ~ week)) 
  print(length(data2$day_volume))
  calendR(
    year = 22,
    special.days = scale_data(data2$day_volume),
    gradient = TRUE,
    special.col = rgb(0, 0, 1, alpha = 1),
    bg.col = "#ecf0f5",
    text.size = 4
  )
}

plot_monthly_calendar <- function(data, month_) {
  scale_data_month <- function(x) {
    31 * ((x - min(x)) / (max(x) - min(x)))
  }
  
  data3 <- data %>% 
    mutate(weekday = wday(date, label = T, week_start = 1), 
           month = month(date, label = T, abbr = F),
           week = isoweek(date),
           day = day(date)) %>%
    mutate(week = case_when(month == "December" & week == 1 ~ 53,
                            month == "January" & week %in% 52:53 ~ 0,
                            TRUE ~ week))
  

  month_data <- data3 %>% filter(month == month_)
  print(length(month_data$day_volume))
  calendR(
    year = 22,
    month =  which(month.name == month_),
    special.days = scale_data_month(month_data$day_volume),
    gradient = TRUE,
    special.col = rgb(0, 1, 0, alpha = 1),
    bg.col = "#ecf0f5"
  )
}

plot_waterfall <- function(filtered_data) {
  highchart() %>%
    hc_chart(type = "waterfall") %>%
    hc_xAxis(categories = filtered_data$exercise_name) %>%
    hc_add_series(
      name = "Exercise Volume",
      data = filtered_data$exercise_volume,
      colorByPoint = TRUE
    )
}




# 
# library(ggcal)
# 
# gg <- ggcal(test_data$date, test_data$day_volume) +
#   scale_fill_gradient2(low="#ffffff", mid="#4d4dff", high="#000066", 
#                        midpoint=mean(test_data$day_volume))
# 
# print(gg)

# 
# test_data <- test_data %>% 
#   mutate(weekday = wday(date, label = T, week_start = 1), 
#          month = month(date, label = T, abbr = F),
#          week = isoweek(date),
#          day = day(date)) %>%
#   mutate(week = case_when(month == "December" & week == 1 ~ 53,
#                           month == "January" & week %in% 52:53 ~ 0,
#                           TRUE ~ week)) 
# 
# test_data %>% select(date,day_volume)
# #view(test_data)
# 
# 
# 
# scale_data <- function(x) {                            
#   365* ((x - min(x)) / (max(x) - min(x)))
# }
# calendR(year = 22,
#         special.days = scale_data(test_data$day_volume), # Vector of the same length as the number of days of the year
#         gradient = TRUE,      # Set gradient = TRUE to create the heatmap
#         special.col = rgb(0, 0, 1, alpha = 1), # Color of the gradient for the highest value
#         low.col = "white")
# 
# #test_data$month
# calendR(year = 22, 
#         special.days = scale_data(test_data$day_volume), # Vector of the same length as the number of days of the year
#         gradient = TRUE,      # Set gradient = TRUE to create the heatmap
#         special.col = rgb(0, 0, 1, alpha = 1), # Color of the gradient for the highest value
#         low.col = "white",
#         legend.pos = "bottom",
#         orientation = "portrait")
# 
# scale_data_month <- function(x) {                            
#   31* ((x - min(x)) / (max(x) - min(x)))
# }
# month_data <- test_data %>% filter(month == "janvier")
# calendR(year = 22,month = 1,
#         special.days = scale_data_month(month_data$day_volume), # Vector of the same length as the number of days of the year
#         gradient = TRUE,      # Set gradient = TRUE to create the heatmap
#         special.col = rgb(0, 1, 0, alpha = 1), # Color of the gradient for the highest value
#         low.col = "white")
# 
# 
# 
# dfPlot <- test_data %>% 
#   mutate(weekday = wday(date, label = T, week_start = 7), # can put week_start = 1 to start week on Monday
#          month = month(date, label = T),
#          week = as.numeric(strftime(date, format = "%U")) + 1)
# 
# dfPlot <- dfPlot %>% 
#   group_by(month) %>% 
#   mutate(monthweek = 1 + week - min(week))
# 
# ggplot(dfPlot, aes(weekday,-week, fill = day_volume)) +
#   geom_tile(colour = "white")  + 
#   geom_text(aes(label = day(date)), size = 3) +
#   theme(aspect.ratio = 1/2,
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         panel.grid = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_text(face = "bold", size = 15),
#         panel.border = element_rect(colour = "black", fill=NA, size=1)) +
#   scale_fill_gradient2(midpoint = 0.5) +
#   facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
#   labs(title = "Calendar heatmap 2022")
