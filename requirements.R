        


#insalling packages:
list.of.packages <-c("tidyverse","rvest","stringr", "janitor", "RSelenium","DT","shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages ,repos ="http://cran.us.r-project.org")
#lapply(libraries, library, character.only = TRUE)