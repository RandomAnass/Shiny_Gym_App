library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(DT)
library(shinyWidgets)

source("helpers/pivot_user_data.R")
# Source helper functions 
source("helpers/progress_graphs.R")
source("simulation/yearly_and_monthly_plots.R")

# Read the data
test_data_ <- readRDS("data/generated_test_25_male_190.rds")
test_data <- pivot_user_data(test_data_)

# Read the exercise data
exercise_data <- readRDS("data/exercise_data.rds") 
exercise_data$exercise_names <- sub(".*standards/", "", exercise_data$exercise_urls)  

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Gym App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calendar", tabName = "calendar"),
      menuItem("Progress", tabName = "progress"),
      menuItem("Training of the Day", tabName = "training"),
      menuItem("Waterfall Plot", tabName = "waterfall")
    )
  ),
  dashboardBody(
    tabItems(
      # Calendar tab
      tabItem(tabName = "calendar",
              fluidRow(
                column(width = 6, plotOutput("yearly_calendar")),
                column(width = 6,
                       selectInput("month", "Select Month", choices = month.name),
                       plotOutput("monthly_calendar")
                )
              )
      ),
      
      # Progress tab
      tabItem(tabName = "progress",
              fluidRow(
                column(width = 12,
                       selectInput("exercise", "Select Exercise", choices = unique(test_data$exercise_name), multiple = TRUE)
                ),
                column(width = 12, highchartOutput("progress_graph"))
              )
      ),
      
      # Training of the Day tab
      tabItem(tabName = "training",
              fluidRow(
                column(width = 12, dateInput("training_date", "Select Date", value = min(test_data$date), min = min(test_data$date), max = max(test_data$date))),
                column(width = 12, tableOutput("exercise_table"))  # Changed to tableOutput
              )
      ),
      
      # Waterfall Plot tab
      tabItem(tabName = "waterfall",
              fluidRow(
                column(width = 12, dateInput("date", "Select Date", value = min(test_data$date), min = min(test_data$date), max = max(test_data$date))),
                column(width = 12, highchartOutput("waterfall"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Render yearly calendar
  output$yearly_calendar <- renderPlot({
    plot_yearly_calendar(test_data_)
  })
  
  # Render monthly calendar
  output$monthly_calendar <- renderPlot({
    plot_monthly_calendar(test_data_, month = input$month)
  })
  
  
  # Render progress graph
  output$progress_graph <- renderHighchart({
    filtered_data <- test_data %>% filter(exercise_name %in% input$exercise)
    plot_prgress(filtered_data, exercise_type = "exercise_volume", curvature = "line")
  })
  
  # Filter test data based on selected date
  filtered_data <- reactive({
    test_data %>% 
      filter(date == input$training_date) %>%
      mutate( exercise_images= paste('<img src="','https://static.strengthlevel.com/images/illustrations/',exercise_name,'-1000x1000.jpg" height="150"></img>', sep = "")) %>%
      select(exercise_name, exercise_sets, exercise_reps, exercise_weight, exercise_volume, exercise_images)
  })
  
  
  
  output$exercise_table <- renderTable({
    print(filtered_data)
    filtered_data()  # Use parentheses to access the reactive value
  }, sanitize.text.function = function(x) x)  # Prevent HTML escaping
  
  # Render waterfall plot
  output$waterfall <- renderHighchart({
    filtered_data <- test_data %>% filter(date == input$date)
    plot_waterfall(filtered_data)
  })
}

# Run the app
shinyApp(ui = ui, server = server)




