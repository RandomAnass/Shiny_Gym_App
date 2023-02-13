install.packages("toastui")
library(toastui)
calendar()

calendar(cal_demo_data(), navigation = TRUE, defaultDate = Sys.Date()) %>%
  cal_month_options(
    startDayOfWeek  = 1, 
    narrowWeekend = TRUE
  ) %>% 
  cal_props(cal_demo_props())

calendar(cal_demo_data("week"), view = "week", defaultDate = Sys.Date()) %>% 
  cal_week_options(
    startDayOfWeek = 1,
    workweek = TRUE
  ) %>% 
  cal_props(cal_demo_props())

# Define directly schedules properties
calendar() %>%
  cal_schedules(
    title = "My schedule",
    start = format(Sys.Date(), "%Y-%m-03 00:00:00"),
    end = format(Sys.Date(), "%Y-%m-17 00:00:00")
  )

# Or you can use a data.frame
my_calendar_data <- data.frame(
  title = "My schedule",
  start = format(Sys.Date(), "%Y-%m-03 00:00:00"),
  end = format(Sys.Date(), "%Y-%m-17 00:00:00")
)
calendar() %>%
  cal_schedules(
    my_calendar_data
  )


calendar() %>%
  cal_schedules(
    title = "My schedule",
    calendarId = 123, # <-- set a calendarId
    start = format(Sys.Date(), "%Y-%m-03 00:00:00"),
    end = format(Sys.Date(), "%Y-%m-17 00:00:00")
  ) %>% 
  cal_props(
    id = 123, # <-- reference to schedules' calendarId
    name = "My Calendar",
    color = "white",
    backgroundColor = "darkorange",
    borderColor = "darkorange"
  )



library(fullcalendar)
my.vals <- c(112,87,45,66,10,6,19,412,8,99,100,74,12,106,
             51,58,91,199,14,22,80,18,3,39,2,1,17,596,4,1003,392)
palette <- colorRampPalette(colors=c("#ff0000", "#0000ff"))
cols <- palette(31)
seqtime <- gsub( "[[:alpha:]]+$", "",
                 seq(strptime("2018-07-01", format="%Y-%m-%d"),
                     strptime("2018-07-31", format="%Y-%m-%d"), 
                     by="1440 mins")  )
data <- data.frame(title = my.vals,
                   start = seqtime,
                   end = seqtime,
                   color = cols)
fullcalendar(data)
##############

library(shiny)
library(toastui)

ui <- fluidPage(
  titlePanel("Interactive Calendar"),
  calendarOutput("calendar")
)

server <- function(input, output) {
  output$calendar <- renderCalendar({
    calendar(
      date = Sys.Date(),
      onClick = function(date, el) {
        message("Clicked on date: ", date)
      },
      customData = list(
        lapply(1:31, function(i) {
          list(date = as.Date("2022-01-01") + i,
               className = c("custom-day",
                             ifelse(i %% 2 == 0, "even", "odd")))
        })
      )
    )
  })
}

shinyApp(ui, server)
