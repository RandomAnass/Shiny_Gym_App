library(htmltools)

calendR_button <- function(year, month, special.days, gradient = TRUE, special.col = "lightblue", low.col = "white") {
  n <- length(special.days)
  
  calendar <- "<table>"
  
  for (i in 1:n) {
    day <- i + 0.5
    url <- paste0("https://www.google.com/#q=day_", i)
    calendar <- paste0(calendar, "<td><a href='", url, "'><button>", i, "</button></a></td>")
  }
  
  calendar <- paste0(calendar, "</table>")
  
  return(HTML(calendar))
}

calendR_button(year = 2020,
               month = 5,
               special.days = rnorm(31),  # Vector of the same length as the number of days of the month
               gradient = TRUE,           # Set gradient = TRUE to create the heatmap
               special.col = "lightblue", # Color of the gradient for the highest value
               low.col = "white")         # Color of the gradient for the lowest value
