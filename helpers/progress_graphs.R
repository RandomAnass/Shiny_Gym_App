library(highcharter)
library(tidyverse)
source("helpers/pivot_user_data.R")

test_data <- readRDS("data/generated_test_25_male_190.rds")
pivoted_data <- pivot_user_data(test_data)



plot_prgress <- function(plot_data = pivoted_data,exercise_type="exercise_volume", curvature ="line" ) {
  # exercise_type = exercise_volume ,exercise_weight
  # curve = line, spline, scatter, stock
  n <- 20
  plot_data_top_n <- pivoted_data %>% filter(exercise_name %in% names(sort(table(pivoted_data$exercise_name),decreasing=TRUE)[1:n]))
  # TO DO: decide when to use plot_data_top_n vs plot_data
  # TO DO: 
  

    hc <- hchart(
      plot_data, type =  curvature ,
      hcaes(x = date, y = !!exercise_type, group = exercise_name,
            enableMouseTracking = FALSE,
            showInLegend = FALSE)  
    ) %>%   hc_exporting(
      enabled = TRUE, # always enabled
      filename = "progress_graphs"
    )
  
    
    
      #hc
  
  
  
  return(hc)
}


plot_waterfall <- function(filtered_data) {
  highchart() %>%
    hc_chart(type = "waterfall") %>%
    hc_xAxis(categories = filtered_data$exercise_name) %>%
    hc_add_series(
      name = "Exercise Volume",
      data = filtered_data$exercise_volume,
      colorByPoint = TRUE
    )
}


# Other graphs
# To DO: explore other possibilities 
#plot_data <- plot_prgress(plot_data = pivoted_data,exercise_type="exercise_volume", curvature ="line" ) 
#textcld <- as.data.frame(table(plot_data$exercise_name))
#names(textcld) <- c( "word", "n")

#hchart(textcld, "wordcloud", hcaes(name = word, weight = log(n)))
#hchart(textcld, "wordcloud", hcaes(name = word, weight = n))
