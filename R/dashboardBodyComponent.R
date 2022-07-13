source('./components/dataImport.R')
source('./components/mappingDashboard.R')
source('./components/stdCurveComponent.R')
source('./components/qPCRDataOverviewComponent.R')
source('./components/welcomeComponent.R')
source('./components/dataModelling.R')
#source('./components/eLowQuant-Functions-V20210407.R')

dashBoardBodyComponent <- function() {

dashboardBody(

    #Style tag for website name, located in left top corner of page
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: Verdana, Geneva, sans-serif;
        font-size: 24px;
      }
    '))),

    tabItems(

      #Mapping Dashboard ---------------------------
      mappingDashboard(),

      # #Data Import ---------------------------
      dataImport(),

      #qPCR Data Overview page ---------------------------
      qPCRDataOverviewComponent(),

      #Data Submission page  ---------------------------
      #dataSubmissionComponent(),

      #Welcome page ---------------------------
      welcomeComponent(),

      #Standard Curve Page ---------------------------
      stdCurve(),

      #Get Started page ---------------------------
      dataModelling()

      #Std Curve Componenet ---------------------------
      #stdCurve()
    )
  )
}
