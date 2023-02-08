library(shiny)
library(lubridate)

calendar <- function(year, month, selectable = TRUE, dateFormat = "dd", 
                     dayNames = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                     dayLabels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                     cellLoc = "center", header = NULL, showNA = FALSE, weekstart = 1, 
                     css = list(".calendar-day {width: 100px; height: 100px; display: inline-block; text-align: center; vertical-align: middle; font-weight: bold; border: 1px solid gray; padding: 10px; margin: 5px;} .selected {background-color: lightgray;}")) {
  
  # Get the dates of the month
  dates <- as.Date(paste(year, month, 1, sep="-"), format = "%Y-%m-%d")
  last_day <- as.integer(format(ceiling_date(as.Date(paste(year, month, 1, sep="-") , "month") - days(1), format="%d"))
  dates <- as.Date(paste(year, month, 1:last_day, sep="-"), format = "%Y-%m-%d")
  
  # Create a reactive matrix of the dates of the month
  dates_matrix <- reactive({
    matrix(dates, nrow=7, byrow=TRUE)
  })
  
  # Create the Shiny UI
  ui <- fluidPage(
    tags$style(css),
    titlePanel("Interactive Calendar"),
    fluidRow(
      column(width=12,
             actionButton("refresh", "Refresh"),
             tags$hr(),
             uiOutput("calendar")
      )
    )
  )
  
  # Create the Shiny Server
  server <- function(input, output) {
    output$calendar <- renderUI({
      div(
        lapply(1:nrow(dates_matrix()), function(i) {
          div(
            lapply(dates_matrix()[i, ], function(date) {
              if (is.na(date)) {
                div("", class = "calendar-day")
              } else {
                day_num <- as.integer(format(date, format=dateFormat))
                day_name <- dayLabels[weekdays(date, abbreviate = FALSE) == dayNames[1]]
                div(
                  tags$p(day_num),
                  tags$p(day_name),
                  class = ifelse(selectable, "calendar-day", ""),
                  id = as.character(date),
                  onclick = ifelse(selectable, 
                                   paste0("window.open('#text_", as.numeric(format(date, format="%Y%m%d")),
                                          "')"), "")
                )
              }
            })
          )
        })
      )
    })
  }
  
  shinyApp(ui, server)
}

calendar(year = 2022, month = 2, selectable = TRUE) 
        