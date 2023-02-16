library(shiny)
library(highcharter)
library(dplyr)

source("helpers/pivot_user_data.R")

test_data <- readRDS("data/generated_test_25_male_190.rds")
pivoted_data <- pivot_user_data(test_data)
# Load data
data <- pivoted_data

# Define UI
ui <- fluidPage(
  titlePanel("Waterfall Plot"),
  sidebarLayout(
    sidebarPanel(
      dateInput(inputId = "date", label = "Select Date", value = min(data$date), min = min(data$date), max = max(data$date), format = "yyyy-mm-dd")
    ),
    mainPanel(
      highchartOutput(outputId = "waterfall")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data based on selected date
  filtered_data <- reactive({
    data %>% filter(date == input$date)
  })
  
  # Generate waterfall plot
  output$waterfall <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(categories = filtered_data()$exercise_name) %>%
      hc_add_series(name = "Exercise Volume", data = filtered_data()$exercise_volume, colorByPoint = TRUE)
  })
}

# Run app
shinyApp(ui = ui, server = server)

# A more complex possibility is giving to each exercise a unique color
# library(grDevices)
# library("hash") # debug hash functoin first
# # in this case:
# output$waterfall <- renderHighchart({
#   
#   # Define a unique color for each exercise based on the exercise name
#   exercise_colors <- lapply(unique(filtered_data()$exercise_name), function(name) {
#     hcl(h = hash(name) %% 360, c = 100, l = 50)
#   })
#   
#   highchart() %>%
#     hc_chart(type = "waterfall") %>%
#     hc_xAxis(categories = filtered_data()$exercise_name) %>%
#     hc_add_series(name = "Exercise Volume", data = filtered_data()$exercise_volume, colorByPoint = TRUE, colors = exercise_colors)
# })