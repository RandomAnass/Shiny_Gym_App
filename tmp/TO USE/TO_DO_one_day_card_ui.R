
# TO DO: Modify according to the harvested Data
library(shiny)
library(DT)
# https://stackoverflow.com/questions/25106481/add-an-image-to-a-table-like-output-in-r

# generate random training data
set.seed(123)
exercise_names <- c("Squat", "Deadlift", "Bench Press", "Bicep Curl")
n_exercises <- length(exercise_names)
n_rows <- 20
exercise_data <- data.frame(
  Exercise = sample(exercise_names, n_rows, replace = TRUE),
  Sets = sample(3:5, n_rows, replace = TRUE),
  Reps = sample(5:12, n_rows, replace = TRUE),
  Weight = round(rnorm(n_rows, mean = 50, sd = 20), 1),
  Image = paste0("https://picsum.photos/100/100?image=", sample(1084:1088, n_rows, replace = TRUE))
)

# create Shiny UI
ui <- fluidPage(
  DTOutput("training_table")
)

# create Shiny server
server <- function(input, output) {
  output$training_table <- renderDT({
    datatable(exercise_data,
              rownames = FALSE,
              options = list(
                columnDefs = list(
                  list(
                    targets = 4,
                    createdCell = JS(
                      "function(cell) {",
                      "cell.innerHTML = '<img src=' + cell.innerHTML + ' width=100 height=100>';",
                      "}"
                    )
                  )
                )
              ),
              escape = FALSE
    )
  })
}

# run Shiny app
shinyApp(ui, server)

