library(shiny)
library(shinydashboard)
library(calendar)

ui <- dashboardPage(
  dashboardHeader(title = "Calendar with buttons"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 12, calendarOutput("calendar"))
    )
  )
)

server <- function(input, output) {
  output$calendar <- renderCalendar({
    cal <- calendar(year = 2020, month = 5)
    for (i in 1:nrow(cal)) {
      for (j in 1:ncol(cal)) {
        day <- cal[i, j]
        if (!is.na(day)) {
          cal[i, j] <- as.html(
            tags$a(day, href = paste0("https://www.google.com/#q=day_", day),
                   style = "text-decoration:none;color:inherit;display:block;position:relative;background:transparent;",
                   class = "btn"
            )
          )
        }
      }
    }
    cal
  })
}

shinyApp(ui, server)
