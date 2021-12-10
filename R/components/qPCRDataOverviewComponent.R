#qPCR Data Overview page ---------------------------

qPCRDataOverviewComponent <- function() {

print("Inside qPCRDataOverviewComponent")

      tabItem(tabName = "qPCRDataOverviewPage",

              h1(strong("qPCR Data Overview")),

              p("Analyze individual well samples for a qPCR run.",
                style = "font-size:16px;"),

              br(),

              #Dropdown menu to select standard curve to view
              fluidRow(

                tabBox(id = "tabset1",
                       width = 3,

                       #Tab1
                       tabPanel("Data",
                                column(12,

                                       br(),


                                       p("To analyze qPCR experimental data and standard curve data associated with a specific project please select Assay, then Machine Type, then Project Name, and then Standard Curve. Then Press the 'Submit' button.",  style = "font-size:16px;"),

                                       #Dropdown menu for assay name
                                       pickerInput(inputId = "DA_assay_input",
                                                   "Assay",
                                                   choices = "None",
                                                   selected = "None",
                                                   multiple = FALSE),


                                       #Dropdown menu for machine type
                                       pickerInput(inputId = "DA_machine_input",
                                                   "Machine Type",
                                                   choices = "None",
                                                   selected = "None",
                                                   multiple = FALSE),


                                       #Dropdown menu for project
                                       pickerInput(inputId = "DA_project_input",
                                                   "Project Name",
                                                   choices = "None",
                                                   selected = "None",
                                                   multiple = FALSE),


                                       # #Dropdown menu for standard curve
                                       # pickerInput(inputId = "SC_input",
                                       #             "Standard Curve",
                                       #             choices = "None",
                                       #             selected = "None",
                                       #             multiple = FALSE),
                                       #
                                       # br(),


                                       #Submit button and reset button
                                       fluidRow(column(4,
                                                       actionButton("DA_submit",
                                                                    "Submit  Files")),
                                                column(4,
                                                       offset = 3,
                                                       actionButton("DA_reset",
                                                                    "Reset Files"))))),

                       tabPanel("Upload Files",

                                column(12,

                                       br(),

                                       p("To analyze external qPCR files, upload a qPCR experimental fluorescence file, a qPCR standard curve fluorescence file, and a filled in metadata file. Press the 'Submit' button and then navigate back to the 'Data' panel to view the updated dropdown menu options.",  style = "font-size:16px;"),

                                       #User uploaded fluorescence and metadata files
                                       fileInput("qPCR_fluorescence_file",
                                                 "Upload qPCR  Experimental Fluorescence File (csv/xlsx/xls)",
                                                 multiple = FALSE,
                                                 accept = c(".xlsx", ".xls", ".csv")),
#
#                                        fileInput("SC_fluorescence_file",
#                                                  "Upload Standard Curve Fluorescence File (csv/xlsx/xls)",
#                                                  multiple = FALSE,
#                                                  accept = c(".xlsx", ".xls", ".csv")),

                                       fileInput("qPCR_metadata_file",
                                                 "Upload Metadata File (xlsx/xls)",
                                                 multiple = FALSE,
                                                 accept = c(".xlsx", ".xls")),


                                       #Select qPCR run platform
                                       selectInput(inputId = "DA_platform",
                                                   label = "qPCR Platform",
                                                   choices = c("None",
                                                               "StepOnePlus",
                                                               "Biomeme two3/Franklin",
                                                               "MIC"),
                                                   multiple = FALSE),

                                       #Uploaded files submit and reset button
                                       fluidRow(column(4,
                                                       actionButton("Uploaded_DA_submit",
                                                                    "Submit  Files")),

                                                column(4, actionButton("Uploaded_DA_reset",
                                                                       "Reset Files")))))),

                #Standard curve plot
                tabBox(

                  title = strong("Data Analysis"),
                  id = "data_analysis_box",
                  height = 1000,
                  width = 9,

                  tabPanel("Presence/Absense Samples",

                           strong("Select a radio button to view associated amplification curve on 'Amplification Plot' tab."), p("Cq Value cells are coloured based on the Cq Cutoff value. Cells coloured in orange refer to positive target sequence detections and cells coloured in blue refer to negative detections."),

                           numericInput(inputId = "cqValueCutoff",
                                        label = h3("Enter Cq Cutoff Value",
                                                   style = "font-size:15px;"),
                                        value = 40,
                                        min = 1,
                                        max = 40),

                           reactableOutput("presence_absence_table")),

                  tabPanel("Amplification Plot",  plotlyOutput("selected"))

                  # tabPanel("Standard Curve Data Overview",
                  #
                  #          strong("Data from the 'Standard Curve Data Overview' table is visualized in the 'Standard Curve Plot' tab."),
                  #
                  #          DT::dataTableOutput('SC_overview_table')),

                  # tabPanel("Standard Curve Plot",
                  #          strong("The residual gradient colour scheme depicts how well the standard curve fits the data points. Yellow points are best fit by the curve, dark purple points are least fit, and orange points are in between."), p("LOD = Limit of Detection, LOQ = Limit of Quantification"),
                  #
                  #          plotlyOutput("standardCurve_plot"))
                  )))
}
