library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinythemes)

source("helpers/pivot_user_data.R")
# Source helper functions 
source("helpers/progress_graphs.R")
source("simulation/yearly_and_monthly_plots.R")

print("getting data")
# Read the data
print(exists("data"))
test_data_ <- readRDS("data/generated_test_25_male_190.rds")
test_data <- pivot_user_data(test_data_)

# Read the exercise data
exercise_data <- readRDS("data/exercise_data.rds") 
exercise_data$exercise_names <- sub(".*standards/", "", exercise_data$exercise_urls)  
harvested_data <- readRDS("data/harvested_data.rds")
print("got data")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Gym App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
      menuItem("Progress", tabName = "progress", icon = icon("chart-line")),
      menuItem("Training of the Day", tabName = "training", icon = icon("dumbbell")),
      menuItem("Waterfall Plot", tabName = "waterfall", icon = icon("chart-bar")),
      menuItem("Exercise info", tabName = "exercise_info", icon = icon("info-circle")),
      menuItem("Input", tabName = "input", icon = icon("edit")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"))
    )
  )
  ,
  dashboardBody(
    tabItems(
      # Calendar tab
      tabItem(tabName = "calendar",
              fluidRow(

                column(width = 12,
                       materialSwitch(
                         inputId = "calendar_style",
                         label = "Calendar Style",
                         value = FALSE
                       ),
                       plotOutput("yearly_calendar"),
                       selectInput("month_", "Select Month", choices = month.name),
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
      ),
      # Exercise Info tab
      tabItem(
        tabName = "exercise_info",
        fluidRow(
          column(
            width = 12,
            selectInput(
              "exercise_info",
              "Select Exercise",
              choices = unique(test_data$exercise_name),
              selected = NULL
            ),
            textOutput("exercise_title"),
            uiOutput("exercise_image"),
            tabsetPanel(
              id = "exercise_info_tabs",
              tabPanel("Strength Levels", dataTableOutput("strength_levels")),
              tabPanel("Sets and Reps", dataTableOutput("sets_reps")),
              tabPanel("Workouts", dataTableOutput("workouts")),
              tabPanel("Bodyweight Ratio", dataTableOutput("bodyweight_ratio")),
              tabPanel("Age", dataTableOutput("age"))
            )
          )
        )
      ),
      # Input tab
      tabItem(tabName = "input",
              fluidRow(
                column(width = 6, dateInput("input_date", "Select Date", value = Sys.Date())),
                column(width = 6,
                       selectInput("input_exercise", "Select Exercise", choices = unique(test_data$exercise_name))
                )
              ),
              fluidRow(
                #column(width = 2, numericInput("input_sets", "Sets", value = 1, min = 1)),
                #column(width = 2, numericInput("input_reps", "Reps", value = 1, min = 1)),
                column(width = 2, sliderInput("input_sets", "Sets", min = 1, max = 30, value = 1)),
                column(width = 2, sliderInput("input_reps", "Reps", min = 1, max = 30, value = 1)),
                column(width = 2, numericInput("input_weight", "Weight", value = 0, min = 0))
              ),
              fluidRow(
                column(width = 12,
                       actionButton("add_entry", "Add Entry"),
                       dataTableOutput("input_table"),
                       actionButton("finish_input", "Finish Input")
                )
              )
      ),
      # Settings tab
      tabItem(tabName = "settings",
              fluidRow(
                column(width = 12,
                       shinythemes::themeSelector()
                       )
                )
              )
      
      
      
    )
  )
)

# Server
server <- function(input, output) {
  
  
  # Create an empty data frame to store the input data
  input_data <- reactiveVal(data.frame(
    Date = character(),
    Exercise = character(),
    Sets = numeric(),
    Reps = numeric(),
    Weight = numeric(),
    Volume = numeric(),
    stringsAsFactors = FALSE
  ))

  
  # Update the input table when a new entry is added
  observeEvent(input$add_entry, {
    new_entry <- data.frame(
      Date = input$input_date,
      Exercise = input$input_exercise,
      Sets = input$input_sets,
      Reps = input$input_reps,
      Weight = input$input_weight,
      Volume = input$input_sets*input$input_reps*input$input_weight,
      stringsAsFactors = FALSE
    )

    input_data(rbind(input_data(), new_entry))
    })
  
  # Render the input table
  output$input_table <- renderDataTable({
    input_data()
  })
  file_name <- "data/input_data.rds"  
  # Save the input data and reset the table when "Finish Input" button is clicked
  observeEvent(input$finish_input, {
    if (nrow(input_data()) > 0) {
  

      # input_data(data.frame(
      #   Date = character(),
      #   Exercise = character(),
      #   Sets = numeric(),
      #   Reps = numeric(),
      #   Weight = numeric(),
      #   stringsAsFactors = FALSE
      # ))

      # Check if the file exists
      if (file.exists(file_name)) {
        # Read the existing data from file
        existing_data <- readRDS(file_name)  # Use read.csv for CSV format
        print(typeof(existing_data))   
        print(typeof(input_data))

        # Append the new data to the existing data
        combined_data <- rbind(existing_data,  as.data.frame(input_data))
      } else {
        # If the file doesn't exist, use the new data as the starting point
        combined_data <- as.data.frame(input_data)
      }
      
      # Save the combined data to the file
      saveRDS(combined_data, file = file_name)
      
      showModal(modalDialog(
        title = "Input Saved",
        "Your input has been saved.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  
  ##########
  # Render yearly calendar
  output$yearly_calendar <- renderPlot({
    if (input$calendar_style) {
      plot_yearly_calendar(test_data_)
    } else {
      plot_yearly_calendar_(test_data_)
    }
  })
  
  # Render monthly calendar
  output$monthly_calendar <- renderPlot({
    plot_monthly_calendar(test_data_, month = input$month_)
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
    filtered_data()  # Use parentheses to access the reactive value
  }, sanitize.text.function = function(x) x)  # Prevent HTML escaping
  
  # Render waterfall plot
  output$waterfall <- renderHighchart({
    filtered_data <- test_data %>% filter(date == input$date)
    plot_waterfall(filtered_data)
  })
  
  
  # Exercise title
  output$exercise_title <- renderText({
    exercise_name <- input$exercise_info
    exercise_name
  })
  
  # Exercise image
  output$exercise_image <- renderUI({
    exercise_name <- input$exercise_info
    exercise_image_path <- paste0(
      "https://static.strengthlevel.com/images/illustrations/",
      exercise_name,
      "-1000x1000.jpg"
    )
    img <- tags$img(src = exercise_image_path, height = "300px", width = "300px")
    img
  })
  
  # Strength levels table
  output$strength_levels <- renderDataTable({
    filtered_data <- harvested_data$definition_of_levels %>%
      rename(Level = X1, Definition = X2)
    datatable(filtered_data, options = list(pageLength = 5))
  })
  
  # Sets and reps table
  output$sets_reps <- renderDataTable({
    filtered_data <- harvested_data$sets_and_reps_total %>%
      filter(exercise_name == input$exercise_info)
    datatable(filtered_data, options = list(pageLength = 5))
  })
  
  # Workouts table
  output$workouts <- renderDataTable({
    filtered_data <- harvested_data$strength_total %>%
      filter(exercise_name == input$exercise_info)
    datatable(filtered_data, options = list(pageLength = 5))
  })
  
  # Bodyweight ratio table
  output$bodyweight_ratio <- renderDataTable({
    filtered_data <- harvested_data$levels_by_body_total %>%
      filter(exercise_name == input$exercise_info)
    datatable(filtered_data, options = list(pageLength = 5))
  })
  
  # Age table
  output$age <- renderDataTable({
    filtered_data <- harvested_data$levels_by_age_total %>%
      filter(exercise_name == input$exercise_info)
    datatable(filtered_data, options = list(pageLength = 5))
  })
  


}

# Run the app
shinyApp(ui = ui, server = server)




