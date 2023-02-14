library(highcharter)
# to change, add dplyr, make it in functions

pivoted_data
data_plot <- pivoted_data %>% filter(exercise_name %in% names(sort(table(pivoted_data$exercise_name),decreasing=TRUE)[1:20]))
hc <- hchart(
  data_plot, "line", 
  hcaes(x = date, y = exercise_volume, group = exercise_name)
) %>%   hc_exporting(
  enabled = TRUE, # always enabled
  filename = "progress_graphs"
)

hc