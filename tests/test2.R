library(shiny)
library(highcharter)


# Define UI for the fitness planner app
ui <- fluidPage(
  
  titlePanel("Fitness Planner"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("exercise", "Choose an exercise:",
                  c("Running", "Weight lifting", "Yoga", "Pilates", "Swimming")),
      sliderInput("intensity", "Intensity:",
                  min = 1, max = 10, value = 5),
      dateInput("date", "Date:", value = Sys.Date()),
      actionButton("add", "Add Activity")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Calendar", highchartOutput("calendar_plot")),
        tabPanel("Progress", highchartOutput("progress_plot"))
      )
    )
  )
)

# Define server logic for the fitness planner app
server <- function(input, output) {
  
  # Create a reactive values object to store the user's input
  user_data <- reactiveValues(data = data.frame(Activity = character(),
                                                Intensity = numeric(),
                                                Date = character()))
  
  observeEvent(input$add, {
    # Add the user's input to the reactive values object
    user_data$data <- rbind(user_data$data,
                            data.frame(Activity = input$exercise,
                                       Intensity = input$intensity,
                                       Date = as.character(input$date)))
  })
  
  # Plot the calendar view
  output$calendar_plot <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_series(
        name = "Activity",
        data = user_data$data,
        type = "column",
        colorByPoint = TRUE
      )
  })
  
  # Plot the progress view
  output$progress_plot <- renderHighchart({
    highchart() %>% 
      hc_add_series(
        name = "Intensity",
        data = user_data$data,
        type = "line",
        hcaes(x = as.Date(Date), y = Intensity)
      ) %>% 
      hc_xAxis(type = "datetime")
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

