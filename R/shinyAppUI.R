source('./dashboardBodyComponent.R')
# Define UI for application ---------------------------
shinyAppUI <- dashboardPage(

  #Skin color of app
  skin = "blue",

  ##Title content
  dashboardHeader(title ="MDMAPR 2.0"),

  ##Sidebar content
  dashboardSidebar(width=230,
                   sidebarMenu(
                     img(src="mdmaprlogo2.png",height=230,width=230),
      #To allow app to use functions from shinyjs package
      useShinyjs(),

      #ID values for sidebar menu items
      id = "tab_being_displayed",

      #Icons for sidebar were obtained from https://fontawesome.com/icons?from=io
      menuItem("Welcome", tabName = "welcomepage", icon = icon("door-open")),
      menuItem("Data Import/Export", tabName = "dataImport", icon = icon("database")),
      menuItem("Standard Curve Analysis", tabName = "stdCurve", icon = icon("calculator")),
      menuItem("Mapping Dashboard", tabName = "dashboard", icon = icon("map")),
      menuItem("Data Overview", tabName = "qPCRDataOverviewPage", icon = icon("chart-bar")),
      menuItem("Statistical Modelling", tabName = "dataModelling", icon = icon("chart-line"))
    )
  ),
  ##Body content
  dashBoardBodyComponent()
)



