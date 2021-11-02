library(leaflet)
library(leaflet.extras)

mappingTab <- function() {

  tabItem(tabName = "dashboard",

          h1(strong("Mapping Dashboard")),

          p("Perform geospatial analysis based on collection locations for run qPCR samples.",
            style = "font-size:16px;"),

          # fluidRow(
          #
          #   #Adding static valueboxes
          #   valueBoxOutput("sampleBox",  width = 3),
          #   valueBoxOutput("platformBox", width = 3),
          #   valueBoxOutput("taxonBox", width = 3),
          #   valueBoxOutput("assayBox", width = 3)),

            fluidRow(

              column(width = 12,
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("mymap", height = 800)))),
                         br(),

                         #Second Row on page for filter functions
                         fluidRow(

                           div(style='height:300px; overflow-y: scroll',
                               box( width = 12,
                                    title = "Filter Options",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    collapsible = T,
                                    column(2,

                                           #Radio button to select what type of Cq value you want to based on threshold vaue (User provided or system calculated)
                                           radioButtons("thresholdValueButton",
                                                        "Select Threshold Value to view associated Cq Value:",
                                                        c("User Provided Threshold" = 10,
                                                          "System Calculated Threshold" = 12)),

                                           #Slider for Cq intensity
                                           sliderInput("range", "Select the Cq Intensity",
                                                       min = 0,
                                                       max = 40,
                                                       step = 0.10, value=c(0,40)),

                                           #Slider for date
                                           sliderInput("date_input",
                                                       "Select Event Date Range",
                                                       min =as.Date("2010-01-01", "%Y-%m-%d"),
                                                       max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                                       value = range(c(as.Date("2010-01-01", "%Y-%m-%d"), as.Date(Sys.Date(), "%Y-%m-%d"))),
                                                       timeFormat = "%Y-%m-%d",
                                                       step = 1),

                                           #Dropdown menu for Continent
                                           pickerInput(inputId = "continent_input",
                                                       "Continent",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for countrys
                                           pickerInput(inputId = "country_input", "Country",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for state or province
                                           pickerInput(inputId = "stateProvince_input",
                                                       "State or Province",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for locality
                                           pickerInput(inputId = "locality_input",
                                                       "Locality",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE)


                                    ),

                                    column(4, offset = 1,


                                           #Dropdown menu for family type
                                           pickerInput(inputId = "family_input",
                                                       "Family",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for genus type
                                           pickerInput(inputId = "genus_input",
                                                       "Genus",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for species type
                                           pickerInput(inputId = "species_input",
                                                       "Species",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for ct intensity
                                           pickerInput(inputId = "CqIntensity_input",
                                                       "Cq Intensity",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for Machine type
                                           pickerInput(inputId = "machine_input",
                                                       "Machine Type",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE)

                                    ),

                                    column(4, offset = 1,

                                           #Dropdown menu for Target gene
                                           pickerInput(inputId = "targetGene_input",
                                                       "Target Gene",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for project ID
                                           pickerInput(inputId = "projectID_input", "Project",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),


                                           #Dropdown menu for assay
                                           pickerInput(inputId = "assay_input", "Assay",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE),

                                           #Dropdown menu for establishment means
                                           pickerInput(inputId = "establishmentMeans_input",
                                                       "Establishment Means",
                                                       choices = "None",
                                                       selected = "None",
                                                       options = list('actions-box' = TRUE),
                                                       multiple = TRUE))

                               ))
                          ),

          #Download Mapped data
          fluidRow(p(strong("Mapped Markers Metadata"),
                     style = "font-size:25px"),
                   downloadLink("downloadFilteredData",
                                p("Download Mapped Markers Metadata",
                                  style = "font-size:16px;
                                      color:#F4412E;
                                       text-decoration: underline;" ))
                   ),
                   br(),

          #Data table for mapped data.
          fluidRow(DT::dataTableOutput("mapping_data"))
          )
}
