welcomeTab <- function() {
  tabItem(tabName = "welcomepage",
          includeHTML("R/ui/html/home.html"),
          tags$script(src = "plugins/scripts.js"),
          tags$head(
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
            tags$link(rel = "icon",
                      type = "image/png",
                      href = "images/logo_icon.png")
          ),
          br(),
          box(title = "Vignettes",
              status = "primary", solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(11,fluidPage(includeMarkdown("R/ui/rmd/MDMAPR_Vignettes.Rmd")))
              )),
          br(),

          h1(strong("FAQs")),

          br(),

          fluidRow(box(
            title = p("How are target copy numbers calculated?",
                      style = "font-size:16px;"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            p("If standard curve fluorescence values and metadata are provided, the system will calculate the second derivative threshold and Cq Value. These are related to the DNA copy number value by a linear model and used to estimate copy number in the experimental samples.")
          )),

          fluidRow(box(
            title = p("How are the System Calculated Threshold values determined?",
                      style = "font-size:16px;"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            p("The Second Derivative Method is used to calculate the System Calculated Threshold values.", style = "font-size:16px;"))),

          fluidRow(
            box(title = p("What formula is used to calculate the Cq values?",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("The function th.cyc() from the R package chipPCR is used to calculate the Cq values.", style = "font-size:16px;")))
  ) # end of tab item
}