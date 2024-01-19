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