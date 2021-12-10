dataModelling <- function() {
  tabItem(tabName = "dataModelling",

          h1(strong("Data Modelling")),
          fluidRow(
          tabBox(
            id = "model_preprocess",
            width = 3,
            height = "800px",
            # tab 1 for uploading the files
            tabPanel("Upload Files",
                     br(),
                     fileInput("model_data", "Upload Data for Model Building",
                               multiple = FALSE,
                               accept = c(".csv"), width="500px"),
                     fluidRow(
                       column(6, actionButton("submit_modelling",
                                              "Submit  Files")))
                     ), # end of tab panel
            # tab 2 for selecting variables.
            tabPanel("Create model",
                     br(),
                     column(12,
                              uiOutput("Xvarselection"),
                            br(),
                            uiOutput("SelectY"),
                            br(),
                            box(
                              sliderInput(
                                "Slider1",
                                label = h3("Train/Test Split %"),
                                min = 0,
                                max = 100,
                                value = 75),
                              width="300px"),
                            textOutput("cntTrain"),
                            textOutput("cntTest")
          )) # end of tab panel 2
          ), # end of tab box

          tabBox(
            id = "tabset_model",
            height = "800px",
            width = 9,

            tabPanel("Data",
                     box(withSpinner(DTOutput(
                       "Data"
                     )), width = 12)),
            tabPanel(
              "Data Summary",
              box(withSpinner(verbatimTextOutput("Summ")), width = 6),
              box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
            ),

            #
            tabPanel("Data Strucure",
                     # box(
                     #   withSpinner(verbatimTextOutput("structure")), width = "100%"
                     # ),
                     explorerOutput("digest")
                     ),
            tabPanel("Plots",
                     box(withSpinner(plotOutput(
                       "Corr"
                     )), width = 12),
            box(withSpinner(verbatimTextOutput("CorrMatrix")), width = 12)),
            tabPanel(
              "Model",
              box(
                withSpinner(verbatimTextOutput("Model")),
                width = 6,
                title = "Model Summary"
              ),
              box(
                withSpinner(verbatimTextOutput("Model_new")),
                width = 6,
                title = "Model Summary"
              ),
              #
              box(
                withSpinner(verbatimTextOutput("ImpVar")),
                width = 5,
                title = "Variable Importance"
              )
            ),
            textOutput("correlation_accuracy"),
            tabPanel(
              "Prediction",
              box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
              box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
            )
          ))# end of tabox 2 and fluid row
          )# end of tab item
  }
