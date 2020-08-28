WD <- Sys.getenv("WORK_DIR")
LP <- Sys.getenv("LIB_PATH")
if (WD != "") {
  setwd(WD)
}
if (LP != "") {
  .libPaths(LP)
}
list.of.packages <- c("shiny", "shinyFiles", "data.table", "rgdal", "sf", "dplyr", "stringi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

library(shiny)
runApp(launch.browser = TRUE)
