stdCurve <- function() {
  tabItem(tabName = "stdCurve",
          h1(strong("Standard Curve Analysis")),
          fluidRow(
            tabBox(id = "tabset1",
                   width = 3,
                   height = "600px",

                   #Tab1 containing the standard curve files
                   tabPanel("Upload Files",

                            column(12,

                                   br(),

                                   p("To analyze standard curve data, upload a qPCR standard curve fluorescence file, and a filled in metadata file.",  style = "font-size:16px;"),


                                   fileInput("SC_fluorescence_file",
                                             "Upload Standard Curve Fluorescence File (csv/xlsx/xls)",
                                             multiple = FALSE,
                                             accept = c(".xlsx", ".xls", ".csv")),

                                   fileInput("SC_metadata_file",
                                             "Upload Metadata File (xlsx/xls)",
                                             multiple = FALSE,
                                             accept = c(".xlsx", ".xls")),

                                   #Select qPCR run platform
                                   selectInput(inputId = "SC_platform",
                                               label = "qPCR Platform",
                                               choices = c("None",
                                                           "StepOnePlus",
                                                           "Biomeme two3/Franklin",
                                                           "MIC/BioRad"),
                                               multiple = FALSE),


                                   #Uploaded files submit and reset button

                                   fluidRow(column(6, actionButton("Uploaded_SC_submit",
                                                                   "Submit  Files")),
                                            column(6, actionButton("Uploaded_SC_reset",
                                                                   "Reset Files"))),
                            )),
                   tabPanel("Design Std Curve",

                            column(12,

                                   br(),
                                   p("Remove wells that do not have amplification."),
                                   #uiOutput("selectComp"),
                                   selectInput(inputId = "SC_wells",
                                               label = "Wells to Include",
                                               choices = "None",
                                               selected = "None",
                                               size=12,
                                               multiple = T,
                                               selectize=F),

                                   fluidRow(column(4,
                                                   actionButton("std_recalib",
                                                                "Recalibrate Curve!")))

                            ))),
            #Standard curve plot
            tabBox(

              title = strong("Standard Curve Analysis"),
              id = "data_analysis_box",
              height = "800px",
              width = 9,

              tabPanel("Standard Curve Data Overview",

                       strong("Data from the 'Standard Curve Data Overview' table is visualized in the 'Standard Curve Plot' tab."),

                       DT::dataTableOutput('SC_overview_table')),

              tabPanel("Standard Curve Plot",
                       strong("The residual gradient colour scheme depicts how well the standard curve fits the data points. Yellow points are best fit by the curve, dark purple points are least fit, and orange points are in between."), p("LOD = Limit of Detection, LOQ = Limit of Quantification"),

                       plotlyOutput("standardCurve_plot"))
            )# end of tab box
          ) #end of fluid row
  )# end of tab item

}
