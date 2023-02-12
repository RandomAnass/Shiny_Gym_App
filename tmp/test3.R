library(shiny)
library(highcharter)

hc_calendar_custom <- function(df, date_var, value_var, title = "Calendar Plot") {
  df <- as.data.frame(df)
  df$date <- as.Date(df[[date_var]])
  df$value <- df[[value_var]]
  min_date <- min(df$date)
  max_date <- max(df$date)
  df$yearmonth <- format(df$date, format = "%Y-%m")
  df$weekday <- weekdays(df$date)
  df$weekday_num <- as.numeric(df$weekday)
  
  hc <- highchart() %>% 
    hc_xAxis(type = "datetime",
             min = as.numeric(min_date),
             max = as.numeric(max_date),
             dateTimeLabelFormats = list(
               day = "%e. %b",
               week = "%e. %b",
               month = "%b '%y",
               year = "%Y"
             )) %>% 
    hc_yAxis(type = "datetime",
             dateTimeLabelFormats = list(
               day = "%A",
             )) %>% 
    hc_colorAxis(min = 1,
                 max = 7,
                 stops = list(
                   c(1, "#f7a35c"),
                   c(2, "#f15c80"),
                   c(3, "#e4d354"),
                   c(4, "#8085e9"),
                   c(5, "#8d4653"),
                   c(6, "#91e8e1"),
                   c(7, "#7cb5ec")
                 )) %>% 
    hc_add_series(type = "heatmap",
                  data = list(date = df[[date_var]], value = df[[value_var]]),
                  nullColor = "rgba(200, 200, 200, 0.3)",
                  colsize = 24 * 36e5,
                  rowsize = 24 * 36e5,
                  tooltip = list(
                    headerFormat = "{point.key}",
                    pointFormat = "Value: {point.value}"
                  ),
                  dataLabels = list(
                    enabled = FALSE
                  )) %>% 
    hc_title(text = title) %>% 
    hc_legend(enabled = FALSE)
  
  hc
}

# UI
ui <- fluidPage(
  titlePanel("Gym Planner"),
  sidebarLayout(
    sidebarPanel(
      selectInput("exercise_type", "Exercise Type:",
                  c("Strength" = "strength",
                    "Cardio" = "cardio",
                    "Yoga" = "yoga")),
      dateInput("training_date", "Training Date:")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Calendar", 
                 highchartOutput("calendar_plot")),
        tabPanel("Progress",
                 h3("Coming soon!"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Calendar Plot
  output$calendar_plot <- renderHighchart({
    hc_calendar_custom(df, "date", "value")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
