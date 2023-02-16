############## Working simple version :

library(shiny)
library(highcharter)
library(tidyverse)
source("helpers/pivot_user_data.R")

test_data <- readRDS("data/generated_test_25_male_190.rds")
pivoted_data <- pivot_user_data(test_data)

# Load data
data <- pivoted_data

# Get a list of unique exercise names
exercise_names <- unique(data$exercise_name)

# Create select inputs for the exercise names and variables to display
exercise_select <- selectInput("exercise", "Choose an exercise", choices = exercise_names)
var_select <- selectInput("var", "Choose a variable to display", choices = c("exercise_weight", "exercise_volume", "exercise_frequency"))

# Define the server function
server <- function(input, output) {
  
  # Create a reactive data frame for the selected exercise and variable
  selected_data <- reactive({
    data %>%
      filter(exercise_name == input$exercise)
  })
  
  # Create the weight progression chart
  output$weight_chart <- renderHighchart({
    selected_data() %>%
      arrange(date) %>%
      hchart("line", hcaes(x = date, y = !!sym(input$var)), name = input$exercise)
  })
}

# Define the UI
ui <- fluidPage(
  titlePanel("Exercise Data"),
  sidebarLayout(
    sidebarPanel(
      exercise_select,
      var_select
    ),
    mainPanel(
      highchartOutput("weight_chart")
    )
  )
)

# Run the app
shinyApp(ui = ui, server = server)


#TO DO:
#more developped version: We need to add the option to chose the variables too (exercise weight, volume, frequency ...)
library(shiny)
library(highcharter)
library(tidyverse)
source("helpers/pivot_user_data.R")

test_data <- readRDS("data/generated_test_25_male_190.rds")
pivoted_data <- pivot_user_data(test_data)
# Load data
data <- pivoted_data

# Get a list of unique exercise names
exercise_names <- unique(data$exercise_name)

# Create a select input for the exercise names
exercise_select <- selectInput("exercise", "Choose an exercise", choices = exercise_names)

# Create a select input for the exercise variables
variable_select <- selectInput("variable", "Choose a variable", choices = c("exercise_weight", "exercise_volume", "exercise_frequency"))

# Define the server function
server <- function(input, output) {
  
  # Create a reactive data frame for the selected exercise
  selected_exercise <- reactive({
    data %>%
      filter(exercise_name == input$exercise) %>%
      mutate(exercise_frequency = 1/(date - lag(date, default = first(date))) %>% as.numeric())
  })
  
  # Create the weight progression chart
  output$weight_chart <- renderHighchart({
    selected_exercise() %>%
      arrange(date) %>%
      hchart("line", hcaes(x = date, y = !!sym(input$variable)), name = input$variable)
  })
  
}


# Define the UI
ui <- fluidPage(
  titlePanel("Exercise Data"),
  sidebarLayout(
    sidebarPanel(exercise_select, variable_select),
    mainPanel(
      highchartOutput("weight_chart"),
      highchartOutput("freq_chart")
    )
  )
)

# Run the app
shinyApp(ui = ui, server = server)