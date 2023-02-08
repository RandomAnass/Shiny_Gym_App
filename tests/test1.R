library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Training Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
      menuItem("Graphs", tabName = "graphs", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "calendar",
              fluidRow(
                box(width = 12, 
                    title = "Calendar", status = "primary",
                    highchartOutput("calendar_plot", height = "500px")
                ),
                box(width = 12,
                    title = "Training Details", status = "primary",
                    uiOutput("training_details")
                )
              )
      ),
      tabItem(tabName = "graphs",
              fluidRow(
                box(width = 12,
                    title = "Graphs", status = "primary",
                    highchartOutput("progress_graphs")
                )
              )
      )
    )
  )
)



server <- function(input, output) {
  #User inputs
  
  user_input <- reactive({
    data.frame(
      Age = as.numeric(input$age),
      Sex = input$sex,
      Weight = as.numeric(input$weight),
      Lat_Pulldown = as.numeric(input$lat_pulldown),
      Bench_Press = as.numeric(input$bench_press),
      Squat = as.numeric(input$squat),
      Deadlift = as.numeric(input$deadlift),
      Activity_Type = input$activity_type
    )
  })
  #Filter outliers
  
  filtered_data <- reactive({
    user_input() %>%
      filter(Lat_Pulldown <= 1000 &
               Bench_Press <= 1000 &
               Squat <= 1000 &
               Deadlift <= 1000)
  })
  #Calculate total volume of training
  
  total_volume <- reactive({
    sum(filtered_data()[,4:7])
  })
  #Create calendar plot
  
  output$calendar_plot <- renderHighchart({
    data <- data.frame(x = as.Date("2022-01-01"), y = 0, value = total_volume())
    
    wasm
    
    hc <- highchart() %>% 
      hc_add_series(type = "heatmap", 
                    colorScale = c("#e0f3f3", "#2171b5"),
                    data = data)
    
    hc
    
  })
  
  training_details <- data.frame(Activity = character(), Lat_Pulldown = numeric(), Bench_Press = numeric(), Squat = numeric(), Deadlift = numeric())
  #Show training details when a day is clicked on the calendar plot
  
  observeEvent(input$calendar_plot_click, {
    day_selected <- input$calendar_plot_click$x
    if (is.null(day_selected)) return()
    
    scss
    
    data <- filtered_data()
    if (nrow(data) == 0) {
      training_details <- data.frame(Activity = character(), Lat_Pulldown = numeric(), Bench_Press = numeric(), Squat = numeric(), Deadlift = numeric())
    } else {
      training_details <- data.frame(
        Activity = data$Activity_Type,
        Lat_Pulldown = data$Lat_Pulldown,
        Bench_Press = data$Bench_Press,
        Squat = data$Squat,
        Deadlift = data$Deadlift
      )
    }
    
    output$training_details <- renderUI({
      fluidRow(
        box(width = 12, dataTableOutput("training_table"))
      )
    })
    
    output$training_table <- renderDataTable({
      training_details
    })
    
  })
}



shiny::runApp(list(ui = ui, server = server))
  
  