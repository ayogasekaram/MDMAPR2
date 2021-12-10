
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MDMAPR

<!-- badges: start -->

<!-- badges: end -->

<img src="images/mdmaprlogo.png" width=400>

The MDMAPR 2.0 is an open-source and extensible Shiny web application that is able to merge raw qPCR fluorescence data and metadata together to facilitate the spatial visualization of species presence/absence detections. The application also has the ability to visualize qPCR fluorescence curves and standard curves to evaluate data quality. MDMAPR 2.0 aims to centralize varied qPCR data, which includes data from pathogen and environmental qPCR species detection studies, gene expression studies, and quantification studies used in identifying pathogen-associated health threats.

The MDMAPR 2.0 shiny application has the option to be connected to a custom developed MySQL database in order to populate the applications interface with data. Data can also be uploaded directly on the application for analysis.

To learn how to set up a MDMAPR 2.0 MySQL database to run with the MDMAPR 2.0 Shiny application please refer to the [wiki](https://github.com/AlkaBenawra/MDMAPR/wiki).

The MDMAPR 2.0 was developed by Alka Benawra at the University of Guelph. 

## Installation

You can install the released version of MDMAPR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MDMAPR")
```

## Open terminal with r command
``` r
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


## How to debug 
``` r
# Open the file you want to debug. This will open a debug console at that point.
browser()
# to exit brower, type c then enter
```
## Data Import page

<kbd><img src="images/data_import.png" width=600></kbd>

The Data Import page is used to format raw qPCR fluorescence data
and associated metadata into a format that is acceptable to be added to
the MDMAPR 2.0 database for storage. On this page, a raw qPCR
experimental fluorescence file, a raw standard curve fluorescence file,
and the filled in MDMAPR 2.0 metadata file are required. The Data
Submission tool will parse the data into 13 CSV files. A preview of the
tables is viewable on the page. A zipped file of the CSVs can be
downloaded by pressing the ‘Download Data Submission Files’ button.
NOTE: Before you can upload the generated CSV data files into their
respective tables in the MDMAPR 2.0 database, the ID columns (projectID,
geographicRegionID, siteID, stationID, replicateID, extractID, assayID,
runID, pcrChemistryID, resultID, standardCurveID, and SCresultID) must
be manually changed from alphabetical characters to unique numeric IDs.

## Standard Curve Analysis page

<kbd><img src="images/standard_curve.png" width=600></kbd>

The Data Submission page is used to format raw qPCR fluorescence data
and associated metadata into a format that is acceptable to be added to
the MDMAPR 2.0 database for storage. On this page, a raw qPCR
experimental fluorescence file, a raw standard curve fluorescence file,
and the filled in MDMAPR 2.0 metadata file are required. The Data
Submission tool will parse the data into 13 CSV files. A preview of the
tables is viewable on the page. A zipped file of the CSVs can be
downloaded by pressing the ‘Download Data Submission Files’ button.
NOTE: Before you can upload the generated CSV data files into their
respective tables in the MDMAPR 2.0 database, the ID columns (projectID,
geographicRegionID, siteID, stationID, replicateID, extractID, assayID,
runID, pcrChemistryID, resultID, standardCurveID, and SCresultID) must
be manually changed from alphabetical characters to unique numeric IDs.

## Mapping Dashboard

<kbd><img src="images/mapping_dashboard.png" width=600></kbd>

The Mapping Dashboard is used to perform geospatial analysis on qPCR run
samples. On this page, an interactive map displays location markers for
qPCR run sample collection locations and allows for filtering of markers
by Cq intensity, date, location, taxon details, machine type, project,
and assay. Hovering above a certain marker will display a pop-up menu
that shows information about the specific marker. As well, the page
contains a data table which shows detailed information about the markers
on the map. The data table will update based on the used filters. A copy
of the data table can be downloaded as a CSV by pressing the ‘Download
Mapped Markers Metadata’ button.

## Data Overview Page

<kbd><img src="images/data_overview.png" width=600></kbd>

The Data Overview page is used to analyze individual tube/well samples
and facilitates the quality control inspection of data. The page has
four tabs, which include the ‘Presence/Absence Samples’, ‘Amplification
Plot’, ‘Standard Curve Data Overview’, and ‘Standard Curve Plot’. The
‘Presence/Absence Samples’ tab displays a table which indicates if a
target sequence was detected in a tube/well based on its Cq Value. The
‘Amplification Plot’ shows the amplification curve associated with a
specific well sample from the ‘Presence/Absence Samples’ tab. The
‘Standard Curve Data Overview’ tab displays a table with information
related to the standard curve used for the samples in the
‘Presence/Absence Samples’ tab. Lastly, the ‘Standard Curve Plot’ tab
shows the plotted standard curve.

