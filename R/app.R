#' @name launchApp
#'
#' @title Launch the MDMAPR app.
#'
#' @description This function runs the MDMAPR Shiny web application. Please update the
#'  dbInstance function first to specify if you are running the application with or
#'  without a database connection. If you are running the application with a database,
#'  then after updating the dbInstance function, run the dbVariables function to input
#'  the variables needed to establish a connection to your MDMAPR MySQL database
#'  instance. Then run launchApp() to launch app.
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @usage launchApp()
#'

#Open terminal with r command
# load packages
load.lib<-c("dplyr","ggplot2","knitr","kableExtra","RColorBrewer","shiny", "shinydashboard", "shinyjs","shinyWidgets","leaflet","DBI",
            "leaflet.extras","data.table","readxl","plotly","reactable","writexl",
            "xfun","berryFunctions","RDML","htmltools","tidyverse", "methods","utils","stats", "MDMAPR",
            "bslib","htmlwidgets","DT","logging", "shinycssloaders", "maps", "shinythemes", "datadigest", "rio", "stargazer", "corrplot", "plyr")

install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)


library(shiny)

source("./shinyAppServer.R")

source("./shinyAppUI.R")

shinyApp(ui = shinyAppUI, server = shinyAppServer)

unclass(as.Date("1971-01-01"))
