# ui <- fluidPage(
#   # ======== Modules ========
#   # exampleModuleUI is defined in R/example-module.R
#   wellPanel(
#     h2("Modules example"),
#     exampleModuleUI("examplemodule1", "Click counter #1"),
#     exampleModuleUI("examplemodule2", "Click counter #2")
#   ),
#   # =========================

#   wellPanel(
#     h2("Sorting example"),
#     sliderInput("size", "Data size", min = 5, max = 20, value = 10),
#     div("Lexically sorted sequence:"),
#     verbatimTextOutput("sequence")
#   ),

#   homeUI()
# )

source('./R/ui/app-ui-module.R')
source('./R/server/app-server-module.R')

shinyApp(appUI, appServer)
