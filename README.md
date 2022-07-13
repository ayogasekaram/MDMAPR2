# MDMAPR 2.0 (2021)

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

The MDMAPR 2.0 is an open-source and extensible Shiny web application that is able to merge raw qPCR fluorescence data and metadata together to facilitate the spatial visualization of species presence/absence detections. The application also has the ability to visualize qPCR fluorescence curves and standard curves to evaluate data quality. MDMAPR 2.0 aims to centralize varied qPCR data, which includes data from pathogen and environmental qPCR species detection studies, gene expression studies, and quantification studies used in identifying pathogen-associated health threats.

The MDMAPR 2.0 was developed by Alka Benawra and Abinaya Yogasekaram at the University of Guelph. 

## Installation

You can install the released version of MDMAPR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MDMAPR")
```

## Example on how to run MDMAPR without database connection: 
``` r
library(MDMAPR)

#Set dbInstance to no
dbInstance("No")

#Open terminal with r command
# load packages
load.lib<-c("shiny", "shinydashboard", "shinyjs","shinyWidgets","leaflet","DBI",
            "leaflet.extras","data.table","ggplot2","dplyr","readxl","plotly","reactable","writexl",
            "xfun","berryFunctions","RDML","htmltools","methods","utils","stats",
            "bslib","htmlwidgets","DT","logging")
install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

# Run the app
source("./R/shinyAppServer.R")
source("./R/shinyAppUI.R")
launchApp()
```

or more easily

``` r
R -e "shiny::runApp('./R')"
# Only do the below with new changes
touch app.R # when you want to reload new changes. Then reload the browser
```
