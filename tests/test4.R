library(shiny)
library(flexdashboard)
library(shinyjs)

calendarPage <- function(data) {
  
  # Create the calendar tab
  ui <- fluidPage(
    fluidRow(
      column(width = 12,
             tags$head(
               tags$style(HTML(".day-cell {
                                 height: 100px;
                                 width: 100px;
                                 display: inline-block;
                                 text-align: center;
                                 vertical-align: middle;
                                 font-weight: bold;
                                 border: 1px solid gray;
                                 padding: 10px;
                                 margin: 5px;
                                 cursor: pointer;
                               }
                               .selected {
                                 background-color: lightgray;
                               }"))
             ),
             uiOutput("calendar"))
    )
  )
  
  # Create the server logic to generate the calendar
  server <- function(input, output, session) {
    selected_date <- reactiveVal()
    
    output$calendar <- renderUI({
      dates <- as.Date(colnames(data), format = "%A")
      days_list <- lapply(dates, function(day) {
        day_str <- format(day, "%d")
        if (is.null(selected_date()) || selected_date() != day) {
          actionButton(day_str, day_str,
                       class = "day-cell",
                       style = paste0("background-color:", data[1, day]),
                       onclick = paste0("shinyjs.set_selected_date(", as.numeric(day), ")"))
        } else {
          actionButton(day_str, day_str,
                       class = "day-cell selected",
                       onclick = NULL)
        }
      })
      do.call(tagList, days_list)
    })
    
    observeEvent(input$day, {
      selected_date(as.Date(input$day))
    })
    
    shinyjs::useShinyjs()
    shinyjs::runjs("
      shinyjs.set_selected_date = function(date) {
        Shiny.setInputValue('day', date, {priority: 'event'});
      }
    ")
  }
  
  # Return the shiny app object
  shinyApp(ui, server)
}

data <- matrix(nrow = 31, ncol = 7)
colnames(data) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
rownames(data) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                    "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")

data[1,1] <- "Strength"
data[2,2] <- "Cardio"
data[3,3] <- "Yoga"
data[4,4] <- "Strength"
data[5,5] <- "Cardio"
data[6,6] <- "Yoga"
calendarPage(data)
