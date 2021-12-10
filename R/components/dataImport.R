dataImport <- function() {
  tabItem(tabName = "dataImport",
          h1(strong("Data Import")),
          fluidRow(
            column(width = 6,
                   box(title = span( icon("upload"), "Upload Data"), status = "info",
                       width = 12, solidHeader = TRUE,collapsible = T, collapsed = F,
                         #qPCR Data file upload
                                p("Upload files here if you are uploading data for the first time in MDMAPR."),
                                #Upload qPCR experimental fluorescence file
                                fileInput("qpcr_file",
                                          "Upload qPCR Experimental Fluorescence File (RDML)",
                                          multiple = FALSE,
                                          accept = c(".rdml"), width="500px"),
                       fileInput("SCI_fluorescence_file",
                                 "Upload Standard Curve Fluorescence File (csv/xlsx/xls)",
                                 multiple = FALSE,
                                 accept = c(".xlsx", ".xls", ".csv"), width = "500px"),

                       #Upload metadata file
                       fileInput("metadata_file", "Upload Filled MDMAPR Metadata File (xlsx)", width="500px",
                                                 multiple = FALSE,
                                                 accept = c(".xlsx", ".xls")),

                       #Select qPCR run platform
                       selectInput(inputId = "platform",
                                                      label = "qPCR Platform",
                                                      choices = c("None", "StepOnePlus", "Biomeme two3/Franklin", "MIC/BioRad"),
                                                      selected = "None",
                                                      multiple = FALSE, width="200px"),
                                textInput(inputId = "upload_data_name", label = "Upload Data Name", width="450px"),

                       fluidRow(column(12,textOutput("error_msg"))),
                       fluidRow(
                         column(6, actionButton("submit",
                                                "Submit  Files")),
                         column(6, actionButton("reset",
                                                "Reset Files")))
                   )),# end of upload data box and column
            column(width = 6,
                   box(title = span( icon("plus-square"), "Add Another Dataset"), collapsible = TRUE, collapsed = TRUE,
                       status = "info", width = 12, solidHeader = TRUE,
                         #qPCR Data file upload
                                p("Upload files here if you would like to add to your dataset or progress file. Only submit files here if you've already uploaded data."),
                                #Upload qPCR experimental fluorescence file
                                fileInput("qpcr_file_add",
                                          "Upload qPCR Experimental Fluorescence File (RDML)",
                                          multiple = FALSE,
                                          accept = c(".rdml"), width="500px"),
                       # add standard curve
                       fileInput("SCI_fluorescence_file_add",
                                 "Upload Standard Curve Fluorescence File (csv/xlsx/xls)",
                                 multiple = FALSE,
                                 accept = c(".xlsx", ".xls", ".csv"), width = "500px"),

                         #Upload metadata file
                                fileInput("metadata_file_add", "Upload Filled MDMAPR Metadata File (xlsx)",
                                          multiple = FALSE,
                                          accept = c(".xlsx", ".xls"), width="500px"),

                         #Select qPCR run platform
                       selectInput(inputId = "platform_add",
                                               label = "qPCR Platform",
                                               choices = c("None", "StepOnePlus", "Biomeme two3/Franklin", "MIC"),
                                               selected = "None",
                                               multiple = FALSE,
                                   width="200px"),
                        textInput(inputId = "add_data_name", label = "Add Data Name", width="450px"),
                         fluidRow(column(10,textOutput("error_msg_add"))),
                         fluidRow(column(4, actionButton("submit_add",
                                                "Submit  Files")),
                         column(4, actionButton("reset_add",
                                                "Reset Files")))
                   ), # end of add dataset box
                   box(title=span( icon("save"), "Load Saved Data"), status="warning", solidHeader=TRUE, width=12,collapsible = T, collapsed = T,
                       fluidRow(
                         column(12, h4(icon("database"), "Continue Previous Work"))),
                       fluidRow(column(12,
                                       p("Restore progress by adding an MDMAPR data file."),
                                       fileInput("load_saved_data", label = "Restore Progress (.zip file)",
                                                 buttonLabel = "Browse",
                                                 placeholder = "Previously saved MDMAPR zip file", multiple=F,
                                                 accept=c(".zip"),
                                                 width="500px"),
                                       column(4, actionButton("submit_zip",
                                                              "Submit  File"))))), # end of load data box

                   box(title = span( icon("play-circle"), "Import Sample Data from MDMAPR package"),
                       collapsible = TRUE, collapsed = TRUE,
                       status = "primary", solidHeader = TRUE, width = 12,
                       fluidRow(
                         column(12, DT::DTOutput("data_set_table")),
                         column(2, offset = 0, actionButton("load_MDMAPR_data", "Load",
                                                            icon = icon("bolt")))
                       )) #end of import sample data box
            )), # end of right hand column
            #adding another section for the Data Exporting Features
            h1(strong("Data Export")),
            fluidRow(
              box(width=12, status="warning",title=span( icon("file-export"), "Preview and Export Data"), solidHeader = T,
                  p("View your sample data and metadata below. Click the \"Export Data\" button to download this MDMAPR file. You can upload this file in the \"Load Saved Data\" panel above to reanalyze with MDMAPR."),
                  column(6, downloadButton(outputId="downloadZipData",label="Download MDMAPR Data File")),
                  br(),
                  DT::dataTableOutput("dataexport")))

          )

}
