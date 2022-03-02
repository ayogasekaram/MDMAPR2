#' @import RMySQL
#' @import shinydashboard
#' @importFrom DBI dbGetQuery
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @import leaflet
#' @import leaflet.extras
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shinyjs reset
#' @importFrom shinyjs alert
#' @importFrom shinyjs useShinyjs
#' @import ggplot2
#' @import dplyr
#' @import readxl
#' @import reactable
#' @import writexl
#' @importFrom xfun file_ext
#' @importFrom berryFunctions is.error
#' @importFrom plotly plotlyOutput
#' @importFrom plotly style
#' @importFrom plotly layout
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @import htmltools
#' @importFrom shiny div
#' @importFrom shiny downloadHandler
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny need
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveVal
#' @importFrom shiny renderText
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSliderInput
#' @importFrom shiny validate
#' @importFrom shiny a
#' @importFrom shiny actionButton
#' @importFrom shiny br
#' @importFrom shiny column
#' @importFrom shiny downloadLink
#' @importFrom shiny em
#' @importFrom shiny fileInput
#' @importFrom shiny fluidRow
#' @importFrom shiny h1
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny HTML
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny p
#' @importFrom shiny radioButtons
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny strong
#' @importFrom shiny tabPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny shinyApp
#' @importFrom shiny textOutput
#' @import methods
#' @importFrom utils read.csv
#' @importFrom utils str
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils zip
#' @importFrom stats family
#' @importFrom stats lm
#' @importFrom stats mad
#' @importFrom stats na.exclude
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats residuals
#' @importFrom bslib is_bs_theme

source('./server/table_creation_helpers.R')
source('./server/standard_curve_helpers.R')
source('./server/data_processing_helpers.R')
source('./server/file_validation_helpers.R')
source('./server/threshold_calc_helpers.R')
source('./server/qPCR_overview_helpers.R')

shinyAppServer <- function(input, output, session) {


  #Function that is opposite on 'is in' function
  '%ni%' <- Negate('%in%')

  ####### MAPPING DASHBOARD/DATA IMPORT ---------------------------

  #Icons from https://fontawesome.com/icons?from=io

  ############ Value Boxes Above the Map ##########
  sample_number <- reactive({

    if (!is.null(uploaded_data())) {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$extractID)))}

    else { return (0)}
  })

  output$sampleBox <- renderValueBox({

    val <- as.numeric(sample_number())

    valueBox(
      paste0(sample_number()),
      "Samples",
      icon = tags$i(class = "fas fa-vials", style="font-size: 15px"),
      color = "yellow"
    )})


  platform_number <- reactive({
    if (!is.null(uploaded_data())) {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$runPlatform)))}

    else { return (0)}
  })


  output$platformBox <- renderValueBox({

    valueBox(
      paste0(platform_number()),
      "Unique qPCR Platforms",
      icon = tags$i(class = "fas fa-hdd", style="font-size: 15px"),
      color = "yellow"
    )})




  taxon_number <- reactive({
    if (!is.null(uploaded_data())) {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$taxonID)))}

    else { return (0)}
  })



  output$taxonBox <- renderValueBox({

    valueBox(
      paste0(taxon_number()),
      "Taxon",
      icon = tags$i(class = "fas fa-frog", style="font-size: 15px"),
      color = "yellow"
    )})



  assay_number <- reactive({
    if (!is.null(uploaded_data())) {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$assayID)))}

    else { return (0)}
  })



  output$assayBox <- renderValueBox({

    valueBox(
      value=tags$p(paste(assay_number()," Unique Assays"), style = "font-size: 70%;"),
      subtitle = "",
      icon = tags$i(class = "fas fa-flask", style="font-size: 30px"),
      color = "yellow"
    )})


  ############ File Validation Upon Upload ##########

  #File validate errors messages.
  error_message <- reactive({
    validate(need(input$qpcr_file, 'No fluorescence file uploaded'),
             need(input$upload_data_name, 'No name given to data set'),
             need(input$metadata_file, 'No metadata file uploaded'),
             need(try(file_ext(input$qpcr_file) == "rdml") ,
                  "fluorescence file must be an RDML"),
             need(try(file_ext(input$metadata_file) == "xlsx" | file_ext(input$metadata_file) == "xls") , "Metadata file must be xlsx/xls"))
  })


  output$error_msg <- renderText({
    error_message()})

  #Pop-up validation messages for uploaded fluorescence file.
  observeEvent(input$qpcr_file,fluorescence_file_validation_msgs(input$qpcr_file))

  #Pop-up validation messages for uploaded std_curve file.
  observeEvent(input$SCI_fluorescence_file, std_fluorescence_file_validation_msgs(input$SCI_fluorescence_file))

  #Pop-up validation messages for uploaded metadata file,
  observeEvent(input$metadata_file, metadata_file_validation_msgs(input$metadata_file))

  observeEvent(req(input$qpcr_file, input$metadata_file),
               selected_platform_validation_msgs(input$SCI_fluorescence_file,
                                                 input$metadata_file,
                                                 input$platform))


  ############ File Validation Upon Add Data Upload ##########

  #File validate errors messages.
  error_message_add <- reactive({
    validate(need(input$qpcr_file_add, 'No fluorescence file uploaded'),
             need(input$dataset_name_add, 'No name given to data set'),
             need(input$metadata_file_add, 'No metadata file uploaded'),
             need(try(file_ext(input$qpcr_file_add) == "rdml") ,
                  "Fluorescence file must be an RDML"),
             need(try(file_ext(input$metadata_file_add) == "xlsx" | file_ext(input$metadata_file_add) == "xls") , "Metadata file must be xlsx/xls"))
  })


  output$error_msg <- renderText({
    error_message()})

  #Pop-up validation messages for uploaded fluorescence file.
  observeEvent(input$qpcr_file_add,fluorescence_file_validation_msgs(input$qpcr_file_add))

  #Pop-up validation messages for uploaded std_curve file.
  observeEvent(input$SCI_fluorescence_file_add, std_fluorescence_file_validation_msgs(input$SCI_fluorescence_file_add))

  #Pop-up validation messages for uploaded metadata file,
  observeEvent(input$metadata_file_add, metadata_file_validation_msgs(input$metadata_file_add))

  observeEvent(req(input$qpcr_file_add, input$metadata_file_add),
               selected_platform_validation_msgs(input$SCI_fluorescence_file_add,
                                                 input$metadata_file_add,
                                                 input$platform_add))





  ############ Data Processing Upon Hitting Submit ##########

  #initalize uploaded_data reactive variable
  uploaded_data <- reactiveVal(value = NULL)

  observeEvent(input$submit, {
    output$dataexport <- renderDataTable({
      export_data <- as.data.frame(uploaded_data())
      datatable(export_data,
                options = list(
                  scrollX = TRUE
                ))
    })
  })

  observeEvent(input$submit, {

    updateTabItems(session, "tab_being_displayed", "dashboard")

    withProgress(message = 'Creating data export', value = 0, {
      output$dataexport <- renderDataTable({
        export_data <- as.data.frame(uploaded_data())
        datatable(export_data,
                  options = list(
                    scrollX = TRUE
                  ))
      })

      # Increment the progress bar, and update the detail text.
      incProgress(1, detail = paste("Doing part", 1))
    })
  })

  observeEvent(input$submit, {

    print("Coming here 2?")

    valid_files <- TRUE

    withProgress(message = 'Validating files', value = 0, {

      valid_files <- user_uploaded_file_validate(input$qpcr_file, input$metadata_file, input$platform, input$SCI_fluorescence_file, input$dataset_name)

      incProgress(1, detail = paste("Doing part", 1))
    })

    # validate user uploaded files
    if (valid_files == TRUE)
    {return(uploaded_data(NULL))}


    else{
      updateTabItems(session, "tab_being_displayed", "dashboard")


      withProgress(message = 'Processing data', value = 0, {

        # 1. Read in the RDML file
        raw_multiplex_data_list <- process_Multiplexed_RDML(input$qpcr_file$datapath)

        # Increment the progress bar, and update the detail text.
        incProgress(1/6, detail = paste("Processing RDML", 1))

        # 2. Read in and format the metadata
        formatted_metadata <- format_qPCR_metadata(input$metadata_file$datapath)

        # Increment the progress bar, and update the detail text.
        incProgress(1/6, detail = paste("Formatting Metadata", 2))

        # 3. remove the control records
        controls_removed <- remove_null_records(formatted_metadata, raw_multiplex_data_list)
        # 3b. Let's separate the controls list object
        formatted_metadata <- controls_removed[[2]]
        raw_multiplex_data_list <-controls_removed[[1]]

        incProgress(1/6, detail = paste("Handling Control Records", 3))

        # 4. Calculate the second derivative threshold
        # convert all columns into numeric values
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x) {sapply(x[,c(2:ncol(x))], as.numeric);x})
        
        # confirm WellLocation is the row name
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x){rownames(x)<- as.character(x$wellLocation);x})
        
        # remove the column that contains the wellLocation information
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x){x <- x[,-1]})

        # calculate threshold
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list,calculate_second_deriv_threshold)

        # Add the userprovided threshold value
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x){merge(x, formatted_metadata[ , c("userProvidedThresholdValue", "wellLocation")], by="wellLocation")})

        incProgress(1/6, detail = paste("Calculating Threshold", 4))

        # 5. Calculate the Cq value using the threshold
        raw_data_with_Cq <- lapply(raw_multiplex_data_list, function(x){add_Cq(x, "systemCalculatedThresholdValue", "systemCalculatedCqValue")})

        incProgress(1/6, detail = paste("Calculating Cq", 5))

        # 6. If the user has threshold values provided, calculate the Cq value with that threshold
        for(target in 1:length(raw_data_with_Cq)){
          copy_numbers <- list()
          if(any(is.na(raw_data_with_Cq[[target]]$userProvidedThresholdValue))){
            raw_data_with_Cq[[target]] <- cbind(raw_data_with_Cq[[target]],CqvaluewithUserThreshold="No Threshold Value Provided by User")
          } else{
            raw_data_with_Cq[[target]] <- add_Cq(raw_data_with_Cq[[target]],"userProvidedThresholdValue", "CqvaluewithUserThreshold")}
          copy_numbers[[length(copy_numbers)+1]] <- data.frame(wellLocation=formatted_metadata$wellLocation, logDNACopyNumber=rep("NA", nrow(raw_data_with_Cq[[target]])), rSquared=rep("NA", nrow(raw_data_with_Cq[[target]])))
        }
        # Assess if standard curve file has been provided, if so, process the fluorescence and metadata
        if (!is.null(input$SCI_fluorescence_file)){
          print("is there a problem")
          print(!is.null(input$SCI_fluorescence_file))

          # if the standard curve file is provided, the platform specific processing is required
          if (input$platform == "Biomeme two3/Franklin") {
            std_fluorescence <- process_biomeme_raw_data(read.csv(input$SCI_fluorescence_file$datapath))}
          else if (input$platform == "MIC/BioRad") {
            std_fluorescence <- process_MIC_raw_data(read.csv(input$SCI_fluorescence_file$datapath))}
          else if (input$platform == "StepOnePlus") {
            #Read in raw qPCR data
            std_fluorescence <- process_SOP_uploaded_file(read_excel(input$SCI_fluorescence_file$datapath, 4))}

          # all metadata files are processed with the same function
          std_meta <- format_std_curve_metadata(input$metadata_file$datapath)

          # 7. Calculate the threshold for the Standard curve data if provided
          rownames(std_fluorescence) <- std_meta$wellLocation
          std_fluorescence <- std_fluorescence[,-1]
          std_w_threshold <- calculate_second_deriv_threshold(std_fluorescence)


          # 8. Calculate Cq values for the Standard curve data if provided
          std_w_threshold <- add_Cq(std_w_threshold, "systemCalculatedThresholdValue", "systemCalculatedCqValue")


          # 9. Merge with copy number information
          std_w_threshold <- merge(std_w_threshold, std_meta[, c("wellLocation", "standardConc")])

          # 10. Calculate the copy number
          for (target in (1:length(raw_data_with_Cq))){
            copy_numbers[[target]] <- calculate_copy_number(std_w_threshold, raw_data_with_Cq[[target]])
          }
          #copy_numbers <- calculate_copy_number(std_w_threshold, raw_data_with_Cq[[1]])

        }
        # 11. Merge all the data
        #all_merged_data <- merge(raw_data_with_Cq[[1]], formatted_metadata, by="wellLocation")
        all_merged_data <- lapply(raw_data_with_Cq, function(x){merge(x,formatted_metadata, by="wellLocation")})
        
        # add the copy number
        for (target in 1:length(all_merged_data)){
          all_merged_data[[target]] <- merge(all_merged_data[[target]], copy_numbers[[target]], by="wellLocation")
        }
        #all_merged_data <- lapply(all_merged_data, function(x){merge(x,copy_numbers, by="wellLocation")})
        
        # combine all dataframes in the list as a single dataframe
        
        #test <- do.call(rbind, all_merged_data)
        all_merged_data <- do.call(rbind, all_merged_data)
        
        # processing the data like cq column intervals
        all_merged_data <- merged_file_processing(all_merged_data, input$upload_data_name)
        

        incProgress(1/6, detail = paste("Merging Files", 6))
      })

      return(uploaded_data(all_merged_data))

      #   }
    }
  })


  ############ Data Processing Upon Hitting Add Data Submit ##########

  # This will repeat the above processing but for new datasets

  # for input$addDataSubmit
  added_data <- reactiveVal(value = NULL)

  observeEvent(input$submit_add, {
    output$dataexport <- renderDataTable({
      export_data <- as.data.frame(uploaded_data())
      datatable(export_data,
                options = list(
                  scrollX = TRUE
                ))
    })
  })

  observeEvent(input$submit_add, {

    updateTabItems(session, "tab_being_displayed", "dashboard")

    withProgress(message = 'Creating data export', value = 0, {
      output$dataexport <- renderDataTable({
        export_data <- as.data.frame(uploaded_data())
        datatable(export_data,
                  options = list(
                    scrollX = TRUE
                  ))
      })

      # Increment the progress bar, and update the detail text.
      incProgress(1, detail = paste("Doing part", 1))
    })
  })

  observeEvent(input$submit_add, {

    print("Coming here 2?")

    valid_files <- TRUE

    withProgress(message = 'Validating files', value = 0, {

      valid_files <- user_uploaded_file_validate(input$qpcr_file_add, input$metadata_file_add, input$platform_add, input$SCI_fluorescence_file_add, input$add_data_name)

      incProgress(1, detail = paste("Doing part", 1))
    })

    # validate user uploaded files
    if (valid_files == TRUE)
    {return(uploaded_data(NULL))}


    else{
      updateTabItems(session, "tab_being_displayed", "dashboard")


      withProgress(message = 'Processing data', value = 0, {

        # 1. Read in the RDML file
        raw_multiplex_data_list <- process_Multiplexed_RDML(input$qpcr_file_add$datapath)

        # Increment the progress bar, and update the detail text.
        incProgress(1/6, detail = paste("Processing RDML", 1))

        # 2. Read in and format the metadata
        formatted_metadata <- format_qPCR_metadata(input$metadata_file_add$datapath)

        # Increment the progress bar, and update the detail text.
        incProgress(1/6, detail = paste("Formatting Metadata", 2))

        # 3. remove the control records
        controls_removed <- remove_null_records(formatted_metadata, raw_multiplex_data_list)
        # 3b. Let's separate the controls list object
        formatted_metadata <- controls_removed[[2]]
        raw_multiplex_data_list <-controls_removed[[1]]

        incProgress(1/6, detail = paste("Handling Controls", 3))

        # 4. Calculate the second derivative threshold
        # use lapply to set the wellLocations as the rownames
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, "rownames<-", raw_multiplex_data_list[[1]]$wellLocation)
        well_names <- raw_multiplex_data_list[[1]]$wellLocation

        # remove the column that contains that information
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x){x <- x[,-1]})

        # convert all columns into numeric values
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x) {sapply(x, as.numeric)})
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, "rownames<-", well_names)

        # calculate threshold
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list,calculate_second_deriv_threshold)

        # Add the userprovided threshold value
        raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x){merge(x, formatted_metadata[ , c("userProvidedThresholdValue", "wellLocation")], by="wellLocation")})

        incProgress(1/6, detail = paste("Calculating Threshold", 4))

        # 5. Calculate the Cq value using the threshold
        raw_data_with_Cq <- lapply(raw_multiplex_data_list, function(x){add_Cq(x, "systemCalculatedThresholdValue", "systemCalculatedCqValue")})

        incProgress(1/6, detail = paste("Calculating Cq", 5))

        # 6. If the user has threshold values provided, calculate the Cq value with that threshold
        if(any(is.na(raw_data_with_Cq[[1]]$userProvidedThresholdValue))){
          raw_data_with_Cq<- lapply(raw_data_with_Cq, function(x)cbind(x,CqvaluewithUserThreshold="No Threshold Value Provided by User"))
        } else{
          raw_data_with_Cq <- lapply(raw_data_with_Cq, function(x){add_Cq(x,"userProvidedThresholdValue", "CqvaluewithUserThreshold")})}

        copy_numbers <- data.frame(wellLocation=formatted_metadata$wellLocation, logDNACopyNumber=rep("NA", nrow(raw_data_with_Cq[[1]])), rSquared=rep("NA", nrow(raw_data_with_Cq[[1]])))
        # Assess if standard curve file has been provided, if so, process the fluorescence and metadata

        # Assess if standard curve file has been provided, if so, process the fluorescence and metadata
        if (!is.null(input$SCI_fluorescence_file_add)){

          # if the standard curve file is provided, the platform specific processing is required
          if (input$platform == "Biomeme two3/Franklin") {
            std_fluorescence <- process_biomeme_raw_data(read.csv(input$SCI_fluorescence_file_add$datapath))}
          else if (input$platform == "MIC/BioRad") {
            std_fluorescence <- process_MIC_raw_data(read.csv(input$SCI_fluorescence_file_add$datapath))}
          else if (input$platform == "StepOnePlus") {
            #Read in raw qPCR data
            std_fluorescence <- process_SOP_uploaded_file(read_excel(input$SCI_fluorescence_file_add$datapath, 4))}

          # all metadata files are processed with the same function
          std_meta <- format_std_curve_metadata(input$metadata_file_add$datapath)

          # 7. Calculate the threshold for the Standard curve data if provided
          rownames(std_fluorescence) <- std_meta$wellLocation
          std_fluorescence <- std_fluorescence[,-1]
          std_w_threshold <- calculate_second_deriv_threshold(std_fluorescence)


          # 8. Calculate Cq values for the Standard curve data if provided
          std_w_threshold <- add_Cq(std_w_threshold, "systemCalculatedThresholdValue", "systemCalculatedCqValue")


          # 9. Merge with copy number information
          std_w_threshold <- merge(std_w_threshold, std_meta[, c("wellLocation", "standardConc")])

          # 10. Calculate the copy number
          copy_numbers <- calculate_copy_number(std_w_threshold, raw_data_with_Cq[[1]])


        }
        # 11. Merge all the data
        all_merged_data <- merge(raw_data_with_Cq[[1]], formatted_metadata, by="wellLocation")
        # add the copy number
        all_merged_data <- merge(all_merged_data, copy_numbers, by="wellLocation")

        # processing the data like cq column intervals
        all_merged_data <- merged_file_processing(all_merged_data, input$upload_data_name)


        new_merged_data <- rbind.fill(all_merged_data, uploaded_data())




        incProgress(1/6, detail = paste("Merging Data", 6))
      })

      return(uploaded_data(new_merged_data))

      #   }
    }
  })


  ############ Zipfile Upload ##########


  all <- reactive({
    inFile <- req(input$load_saved_data)
    filelist <- unzip(inFile$datapath, list = T)
    lapply(filelist$Name, read_csv)
  })

  # upload the fluorescence table for mapping
  observeEvent(input$submit_zip, {
    updateTabItems(session, "tab_being_displayed", "dashboard")
    return(uploaded_data(all()[[1]]))
  })

  # upload the standard curve table for analysis
  observeEvent(input$submit_zip, {
    return(standard_curve_tab_data(all()[[2]]))
  })

  ############ Data Export Download Button ##########

  output$downloadZipData <- downloadHandler(
    filename = 'MDMAPR_Data.zip',
    content = function(fname) {
      fs <- c(
        "mergedTable.csv",
        "curve.csv")

      write.csv(
        as.data.frame(uploaded_data()),
        file = "mergedTable.csv",
        sep = ",",
        row.names = FALSE,
        na = "NULL")

      curve_df <- as.data.frame(standard_curve_tab_data())

      write.csv(
        curve_df,
        file = "curve.csv",
        sep = ",",
        row.names = FALSE,
        na = "NULL")

      zip(zipfile=fname, files=fs)
    },

    contentType = "application/zip")

  ############ Mapping Dash: Initialize and Populate Filters ##########

  #Updated Family list
  family_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      family_data <- as.character(unique(data$family))
      return(family_data)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "family_input",
                      choices = family_list(),
                      selected = family_list())
  })



  #Updated Genus list
  genus_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      genus_data <- as.character(unique(data$genus))
      return(genus_data)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "genus_input",
                      choices = genus_list(),
                      selected = genus_list())
  })



  #Updated Species list
  species_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      species_data <- as.character(unique(data$species))
      return(species_data)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "species_input",
                      choices = species_list(),
                      selected = species_list())
  })




  #Update qPCR machine list
  machine_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      run_platform_add <- as.character(unique(data$runPlatform))
      return(run_platform_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "machine_input",
                      choices = machine_list(),
                      selected = machine_list() )
  })




  #Update target gene list
  targetGene_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      targetGene_add <- as.character(unique(data$geneSymbol))
      return(targetGene_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "targetGene_input",
                      choices = targetGene_list(),
                      selected = targetGene_list() )
  })




  #Update project list
  project_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      project_add <- as.character(unique(data$projectName))
      return(project_add)} else {
        return(NULL)}
  })


  observe({
    updatePickerInput(session, "projectID_input",
                      choices = project_list(),
                      selected = project_list() )
  })



  #Update assay list
  assay_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      assay_add <- as.character(unique(data$assayName))
      return(assay_add)} else {
        return(NULL)}
  })


  observe({
    updatePickerInput(session, "assay_input",
                      choices = assay_list(),
                      selected = assay_list())
  })




  #Update continent list
  continent_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      continent_add <- as.character(unique(data$continent))
      return(continent_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "continent_input",
                      choices = continent_list(),
                      selected = continent_list() )
  })


  #Update country list
  country_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      country_add <- as.character(unique(data$country))
      return(country_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "country_input",
                      choices = country_list(),
                      selected = country_list() )
  })




  #Update state/province list
  stateProvince_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      stateProvince_add <- as.character(unique(data$stateProvince))
      return(stateProvince_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "stateProvince_input",
                      choices = stateProvince_list(),
                      selected = stateProvince_list() )
  })



  #Update locality list
  locality_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      locality_add <- as.character(unique(data$locality))
      return(locality_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "locality_input",
                      choices = locality_list(),
                      selected = locality_list())
  })




  #Update establishment means list
  establishmentMeans_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      establishmentMeans_add <- as.character(unique(data$establishmentMeans))
      return(establishmentMeans_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "establishmentMeans_input",
                      choices = establishmentMeans_list(),
                      selected = establishmentMeans_list() )
  })


  #Update Cq intensity list
  ctIntensity_list <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      ctintensity_updated <- as.character(unique(c(as.character(unique(data$CqIntensity)),
                                                   as.character(unique(data$CqIntensitySystemCalculated)))))
      return(ctintensity_updated)}


    else {
      return(NULL)}
  })


  observe({
    updatePickerInput(session, "CqIntensity_input",
                      choices = ctIntensity_list(),
                      selected = ctIntensity_list() )
  })



  #Update minimum date on date range slider
  #Update Cq intensity list
  min_date <- reactive({
    if (!is.null(uploaded_data())) {
      data <- as.data.frame(mergedData())
      min_date_updated <- min(as.Date(data$collectionDate, "%Y-%m-%d"))
      return(as.character(min_date_updated))}

    else {
      return(NULL)}
  })



  observe({

    updateSliderInput(session, "date_input",
                      min = as.Date(min_date(),"%Y-%m-%d"),
                      max = as.Date(Sys.Date(), "%Y-%m-%d"),
                      value = range(c(as.Date(min_date(),"%Y-%m-%d"),
                                      as.Date(Sys.Date(), "%Y-%m-%d"))),
                      step=1)
  })




  #merge uploaded data with data from MySQL connection
  mergedData <- reactive({

    if(is.null(uploaded_data())) {

      return("No_Data")}


    else if (!is.null(uploaded_data())) {

      uploaded <- as.data.frame(uploaded_data())

      return (uploaded)
    }

  })



  #Mapped markers filtered by widgets on dashboard page.
  filtered <- reactive({

    data_final <- mergedData()

    if (!is.null(uploaded_data())) {

      data_final <- data_final[data_final[ , cq_column()] >= input$range[1] &
                                 data_final[ , cq_column()] <= input$range[2] &
                                 data_final$collectionDate >= input$date_input[1] &
                                 data_final$collectionDate <= input$date_input[2], ]%>%
      filter(family %in% input$family_input)  %>%
      filter(species %in% input$species_input)  %>%
      filter(genus %in% input$genus_input)  %>%
      filter(runPlatform %in% input$machine_input)%>%
      filter(geneSymbol %in% input$targetGene_input) %>%
      filter(projectName%in% input$projectID_input) %>%
      filter(assayName %in% input$assay_input) %>%
      filter(country %in% input$country_input) %>%
      filter(continent %in% input$continent_input) %>%
      filter(stateProvince %in% input$stateProvince_input) %>%
      filter(locality %in% input$locality_input) %>%
      filter(establishmentMeans %in% input$establishmentMeans_input)%>%
      filter(CqIntensitySystemCalculated %in% input$CqIntensity_input)

      return(data_final)}
  })



  #Download button for downloading CSV of filtered mapping data
  output$downloadFilteredData <- downloadHandler(

    filename = 'MDMAP_Mapping_data.csv',

    content = function(file) {

      write.csv(filtered(), file,  row.names=FALSE)
    })


  #Dynamic data table with Mapping marker information
  output$mapping_data <- renderDataTable({


    mapping_data <- as.data.frame(filtered()[,c(
      "dataset_name", "projectName","projectDescription",	"InstitutionID", "sampleName", "wellLocation", "copyNumber", "rSquared",	"control","runRecordedBy", "runDate",	"runTime",	"runPlatform", "machineID",	"reactionConditions",	"reactionVolume", "templateAmount",	"forwardPrimerBatch",	"reversePrimerBatch", "dNTPConcentration",	"primerConcentration",	"probeConcentration","Mg2+Concentration",	"polymeraseBatch",	"polymeraseConcentrations", "thermocyclerParameters",	"pcrDataNotes",	"taxonID","establishmentMeans",	"assayName",	"assayOwnership","assayDescription",	"assayCitation", "assayDate",	"geneTarget", "geneSymbol",	"dilutions",	"replicates","primerR",	"primerF",	"probe","ampliconLength (bp)",	"probeFluorescentTag",	"dye(s)","quencher",	"probeModification", "kingdom",	"phylum",	"class","order",	"family",	"genus","subgenus",	"species",	"vernacularName","organismScope",	"replicateID",	"extractName", "analyst",	"extractionDate",	"extractionTime","location",	"extractionMethod",	"methodCitation","extractionNotes",	"tubePlateID",	"frozen","fixed",	"dnaStorageLocation",	"extractMethodOfStorage", "dnaVolume",	"quantificationMethod",	"concentration(ng/ul)","stationID",	"collectorName",	"replicateName", "collectionDate",	"collectionTime",	"storageID","DateOfStorage",	"methodOfStorage",	"minimumElevationInMeters","maximumElevationInMeters",	"verbatimElevation",	"minimumDepthInMeters","maximumDepthInMeters",	"verbatimDepth", "flowRate(m/s)",	"filterType",	"filtrationDuration(mins)", "volumeFiltered",	"processLocation",	"replicationNumber","riparianVegetationPercentageCover",	"dissolvedOxygen(mg/L)",	"waterTemperature(C)","pH",	"TSS(mg/L)",	"EC(uS/cm)", "turbidity(NTU)",	"discharge",	"tide", "chlorophyl",	"salinity(ppt)",	"contaminants(ng/g)","traceMetals(mg/kg)",	"organicContent(%)",	"microbialActivity", "grainSize",	"replicateDataNotes",	"siteID", "stationName",	"decimalLongitude",	"decimalLatitude", "geographicRegionID",	"locality",	"estimatedPerimeter","estimatedSurfaceArea(m2)",	"siteType",	"siteLength(m2)","projectID",	"continent",	"country","stateProvince",	"municipality",	"standardCurveID",	"standardCurveName","SCdate",	"SCrecordedBy",	"SCdataNotes",	"CqIntensitySystemCalculated", "userProvidedThresholdValue",	"userProvidedCqValue",	"systemCalculatedThresholdValue","systemCalculatedCqValue", "Cycle_Number1",	"Cycle_Number2", "Cycle_Number3",	"Cycle_Number4",	"Cycle_Number5", "Cycle_Number6",	"Cycle_Number7",	"Cycle_Number8", "Cycle_Number9",	"Cycle_Number10",	"Cycle_Number11","Cycle_Number12",	"Cycle_Number13",	"Cycle_Number14","Cycle_Number15",	"Cycle_Number16",	"Cycle_Number17","Cycle_Number18",	"Cycle_Number19",	"Cycle_Number20","Cycle_Number21",	"Cycle_Number22",	"Cycle_Number23","Cycle_Number24",	"Cycle_Number25",	"Cycle_Number26","Cycle_Number27",	"Cycle_Number28",	"Cycle_Number29", "Cycle_Number30",	"Cycle_Number31",	"Cycle_Number32","Cycle_Number33",	"Cycle_Number34",	"Cycle_Number35", "Cycle_Number36", "Cycle_Number37","Cycle_Number38","Cycle_Number39","Cycle_Number40","Cycle_Number41", "Cycle_Number42", "Cycle_Number43", "Cycle_Number44", "Cycle_Number45", "Cycle_Number46", "Cycle_Number47", "Cycle_Number48", "Cycle_Number49", "Cycle_Number50", "Cycle_Number51", "Cycle_Number52", "Cycle_Number53", "Cycle_Number54", "Cycle_Number55", "Cycle_Number56", "Cycle_Number57", "Cycle_Number58", "Cycle_Number59", "Cycle_Number60", "Cycle_Number61", "Cycle_Number62", "Cycle_Number63", "Cycle_Number64", "Cycle_Number65", "Cycle_Number66", "Cycle_Number67", "Cycle_Number68", "Cycle_Number69", "Cycle_Number70")])

    datatable(mapping_data,
              options = list(
                scrollX = TRUE
              ))
  })



  #What Cq value to use based on user select threshold value on Mapping Dashboard.
  cq_column <- reactive({
  #   if (input$thresholdValueButton == 10)
  # {return(11)}
  # else
    return(14)
  })


  #Data that will appear in each popup window for the mapped markers.
  popup_data <- reactive({

    data <- as.data.frame(filtered())


    content.output <- paste("<strong><h5>Species:", data$species, "</strong>",
                            "<strong><h5> NCBI Taxon ID:", "<a href='https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
                            data$taxonID, "'>", data$taxonID, "</a>", "</strong>",
                            "<strong><h5>Cq Value:", data[ , cq_column()], "</strong>",
                            "<strong><h5>Copy Number Value:", exp(data$copyNumber), "</strong>",
                            "<strong><h5>Threshold Value:", as.numeric(data$systemCalculatedThresholdValue), "</strong>",
                            "<br><h6>Sample Name:", data$extractName,
                            "<br><h6>Well Location:", data$wellLocation,
                            "<br><h6>qPCR Device:", data$runPlatform,
                            "<br><h6>Common Name:", data$vernacularName,
                            "<br><h6>Event Date:", data$collectionDate,
                            "<h6>Event Coordinate(Lat, Lon):", data$decimalLatitude,",", data$decimalLongitude,
                            "<h6>Event Identifer(s):", data$analyst)
    return(content.output)
  })


  cq_intensity_column <- reactive({ if (input$thresholdValueButton == 10)
  {return(as.data.frame(filtered())$CqIntensity)}
    else  {return(as.data.frame(filtered())$CqIntensitySystemCalculated)}
  })



  #Define the colour Palette for leaflet map
  palette_map_ct <- colorFactor( palette = c("#fbd300",
                                             "#ff8600",
                                             "#ea5f94",
                                             "#9d02d7",
                                             "#0000ff"),
                                 levels = c("0 < Very strong < 10",
                                            "10 <= Strong < 20",
                                            "20 <= Moderate < 30",
                                            "30 <= Weak < 40",
                                            "None > 40"))

  #Default static leaflet map before filtering parameters are applied.
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%

      addLegend(position = "bottomright",
                pal = palette_map_ct,
                values =  c("0 < Very strong < 10",
                            "10 <= Strong < 20",
                            "20 <= Moderate < 30",
                            "30 <= Weak < 40",
                            "None > 40"),
                title = 'qPCR Cq Signal Intensity',
                opacity = 0.6) %>%

      #View map full screen (note: only works in web browser)
      addFullscreenControl() %>%


      #Default map view --> Change to Guelph
      setView(lng = -80.2262, lat = 43.5327, zoom = 3) %>%

      #Change leaflet map tiles
      addProviderTiles(providers$Esri.WorldStreetMap)

  })


  # Observe will track if any filtering widgets are used. If any filters are implemented the   # Leaflet map will clear the existing markers and update the markers to relflect the
  # current filtering conditions.
  observe({

    #when leaflet map is not on first page you need to use req in order for marker points to appear on map
    req(input$tab_being_displayed == "dashboard")

    if (!is.null(uploaded_data())) {
      print(as.data.frame(filtered()))

      leafletProxy("mymap", data = as.data.frame(filtered())) %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups() %>%
        #Adding labels to markers on map
        addCircleMarkers(lng = ~decimalLongitude,
                         lat = ~decimalLatitude,
                         color = ~palette_map_ct(cq_intensity_column()),
                         clusterOptions = markerClusterOptions(),
                         popup= popup_data()) }


  })



  #Reset uploaded file input and fitler widget selections to return map to its default view.
  observeEvent(input$reset, {
    shinyjs::reset("platform")
      updateSelectInput(session,
                        inputId = "platform",
                        label = "qPCR Platform",
                        choices = c("None",
                                    "StepOnePlus",
                                    "Biomeme two3/Franklin",
                                    "MIC/BioRad"),
                        selected = "None")

      shinyjs::reset("qpcr_file")
      shinyjs::reset("metadata_file")
      shinyjs::reset("SCI_fluorescence_file")
      shinyjs::reset("upload_data_name")
      shinyjs::reset("range") #ct range slider
      shinyjs::reset("family_input") #family dropdown box
      shinyjs::reset("genus_input") #genus dropdown box
      shinyjs::reset("species_input") #species dropdown box
      shinyjs::reset("CqIntensity_input") #ct intensity dropdown box
      shinyjs::reset("machine_input") #machine input
      shinyjs::reset("targetGene_input") #target gene (gene symbol) input
      shinyjs::reset("projectID_input") #project ID input
      shinyjs::reset("assay_input") #assay input
      shinyjs::reset("continent_input") #continent input
      shinyjs::reset("country_input") #country input
      shinyjs::reset("stateProvince_input") #state/province input
      shinyjs::reset("locality_input") #locality input
      shinyjs::reset("establishmentMeans_input") #establishment means input

      #Reset date range slider
      updateSliderInput(session, "date_input",
                        value = range(c(min(as.Date("2010-01-01", "%Y-%m-%d")), as.Date(Sys.Date(), "%Y-%m-%d"))))

      #Clear markers from leaflet map
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups()

  })


  # reset for add data box
  observeEvent(input$reset_add, {
    shinyjs::reset("platform_add")
    updateSelectInput(session,
                      inputId = "platform_add",
                      label = "qPCR Platform",
                      choices = c("None",
                                  "StepOnePlus",
                                  "Biomeme two3/Franklin",
                                  "MIC/BioRad"),
                      selected = "None")

    shinyjs::reset("qpcr_file_add")
    shinyjs::reset("metadata_file_add")
    shinyjs::reset("SCI_fluorescence_file_add")
    shinyjs::reset("add_data_name")
  })

  # Standard Curve Design page ---------------------------


  ############ Standard Curve File Validation ##########

  #Return popup message regarding uploaded standard curve fluorescence file.
  observeEvent(input$SC_fluorescence_file,
               std_fluorescence_file_validation_msgs(input$SC_fluorescence_file))

  #Return popup messaged regarding uploaded metadata file.
  observeEvent(input$SC_metadata_file,
               std_metadata_file_validation_msgs(input$SC_metadata_file))


  standard_curve_tab_data <- reactiveVal(value = NULL)

  observeEvent(input$Uploaded_SC_submit, {
    isolate(

      #Validate content of user uploaded files
      if (user_uploaded_standard_curve_file_validation(input$SC_fluorescence_file,
                                                       input$SC_metadata_file,
                                                       input$SC_platform) == TRUE)

      {return(NULL)}


      ############ Standard Curve Data Processing ##########
      else{

        if (input$SC_platform == "Biomeme two3/Franklin") {

          #Read in raw qPCR data
          qpcr_biomem23_raw <- process_biomeme23_uploaded_file(read.csv(input$SC_fluorescence_file$datapath))

          #Read in metadata
          metadata_biomem23 <-  format_standardCurve_metadata(read_xlsx(input$SC_metadata_file$datapath, sheet = 5))

          #Function to process and merge files
          merged_biomem23_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_biomem23_raw, metadata_biomem23)
          
          #Function to calculate LOD and LOQ
          merged_biomem23_file <- calculate_SC_LOD_LOQ(merged_biomem23_file, input$LOQthres)


          #This merged datatable that will be used to populate map
          return(standard_curve_tab_data(merged_biomem23_file))

        }


        ### Machine Option 2: MIC ###
        else if (input$SC_platform == "MIC/BioRad") {

          #Read in raw qPCR data
          qpcr_MIC_raw  <- process_MIC_uploaded_file(read.csv(input$SC_fluorescence_file$datapath))
          print(qpcr_MIC_raw)

          #Read in metadata
          metadata_MIC <- format_standardCurve_metadata(read_xlsx(input$SC_metadata_file$datapath, sheet = 5))
          print(metadata_MIC)

          #Function to process and merge files
          merged_mic_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_MIC_raw,metadata_MIC)
          print(merged_mic_file)
          
          #Function to calculate LOD and LOQ
          merged_mic_file <- calculate_SC_LOD_LOQ(merged_mic_file, input$LOQthres)
          print(merged_mic_file)

          #This merged datatable that will be used to populate map
          return(standard_curve_tab_data(merged_mic_file))

        }

        ### Machine Option 3: StepOnePlus ###
        else if (input$SC_platform == "StepOnePlus") {

          #Read in standard curve fluorescence file
          standardCurve_StepOnePlus_raw <- process_SOP_uploaded_file(read_excel(input$SC_fluorescence_file$datapath, sheet = 4))

          #Read in standard curve metadata
          standardCurve_metadata_StepOnePlus <- format_standardCurve_metadata(read_xlsx(input$SC_metadata_file$datapath, sheet = 5))

          #Function to process and merge files
          merged_StepOnePlus_file <- merge_standardCurve_metadata_fluorescence_file(standardCurve_StepOnePlus_raw, standardCurve_metadata_StepOnePlus)
          
          #Function to calculate LOD and LOQ
          merged_StepOnePlus_file <- calculate_SC_LOD_LOQ(merged_StepOnePlus_file, input$LOQthres)
          print(merged_StepOnePlus_file)

          return(standard_curve_tab_data(merged_StepOnePlus_file))
        }
      })
  })

  # show the LOD warning Message
  output$text <- renderPrint({
    req(standard_curve_tab_data())
    noquote(standard_curve_tab_data()$LODWarning[1])})

  SC_well_list <- reactive({

    if (!is.null(standard_curve_tab_data())) {

      data <- as.data.frame(standard_curve_tab_data())

      well_data <- append('None', as.character(unique(data$wellLocation)))

      return(well_data)}

    else {return(NULL)}
  })
  #
  observe({updateSelectInput(session,
                             "SC_wells",
                             choices = SC_well_list(),
                             selected = 'None')})

  SC_assay_list <- reactive({

    if (!is.null(uploaded_data())) {

      data <- as.data.frame(uploaded_data())

      assay_data <- append('None', as.character(unique(data$assayName)))

      return(assay_data)}

    else {return(NULL)}
  })
  #
  observe({updatePickerInput(session,
                             "DA_assay_input",
                             choices = SC_assay_list(),
                             selected = 'None')})


  ############ Standard Curve Table and Plot Creation ##########

  #Standard curve plot only appears when submit button is pressed
  observeEvent(input$Uploaded_SC_submit | input$submit_zip, isolate({

    output$standardCurve_plot <- renderPlotly({
      req(standard_curve_tab_data())

      #Add data to plot
      SC_plot_data <- standard_curve_tab_data()

      #Remove control records
      SC_plot_data <- control_records_to_remove(SC_plot_data)

      #Change standard concentration value to numeric and then take log value
      SC_plot_data$standardConc <- as.numeric(SC_plot_data$standardConc)
      SC_plot_data$standardConc <- log(SC_plot_data$standardConc)

      #Change user provided Cq to numeric value to numeric
      SC_plot_data$systemCalculatedCqValue <- as.numeric(SC_plot_data$systemCalculatedCqValue)
      SC_plot_data$systemCalculatedLOQ<- as.numeric(SC_plot_data$systemCalculatedLOQ)
      SC_plot_data$systemCalculatedLOD<- as.numeric(SC_plot_data$systemCalculatedLOD)

      #Add column with residual values to data set
      regression_line <- lm(as.numeric(systemCalculatedCqValue) ~ as.numeric(standardConc), SC_plot_data)
      SC_plot_data$Residual <- abs(residuals(regression_line))

      #Code to get R squared
      #Adapted from: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
      # Code to get equation of the line and R-squared
      # Adapted from: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

      lm_eq <- function(df){

        model1 <- lm(systemCalculatedCqValue ~ standardConc, df, na.action=na.exclude)

        b = format(unname(coef(model1)[1]), digits = 2)

        mx = paste("(", format(unname(coef(model1)[2]), digits = 2), "x", ")",  sep = "")

        r2 = format(summary(model1)$r.squared, digits = 3)

        equation_of_line <- paste("y", " = ", mx, " + ", b, ",   ", "R-squared", " = ", r2,  sep = "")

        return (equation_of_line)

      }



      print(

        ggplotly(height = 700,

                 ggplot(data = SC_plot_data, aes(x = standardConc,
                                                 y = systemCalculatedCqValue,
                                                 color = Residual)) +

                   geom_point(size = 10, alpha = .3) +

                   geom_smooth(method = "lm",
                               se = FALSE,
                               alpha = .15,
                               color = 'black',
                               formula = y ~ x) +

                   geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOD),
                              color = "#ea5f94",
                              linetype="dashed") +
                   #
                   geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOQ),
                              color = "#0178A1",
                              linetype="dotted") +

                   geom_text(aes(x= log(systemCalculatedLOD),
                                 label="LOD",
                                 y= mean(systemCalculatedCqValue)),
                             colour="#ea5f94",
                             angle=90,
                             vjust = 1.2,
                             size=5) +

                   geom_text(aes(x= log(systemCalculatedLOQ),
                                 label="LOQ",
                                 y= mean(systemCalculatedCqValue)*1.2),
                             colour="#0178A1",
                             angle=90,
                             vjust = 1.2,
                             size=5) +

                   geom_text(aes(label=wellLocation)) +


                   xlab("log DNA Copy Number") +

                   ylab("System Calculated Cq") +

                   theme_minimal() +

                   scale_color_gradient(low = "#fbd300", high = "#0000ff") +

                   theme( axis.title.x = element_text( size=13),
                          axis.title.y = element_text( size=13),
                          axis.text.x = element_text(size=12),
                          axis.text.y = element_text(size=12),
                          plot.title = element_text(hjust = 0.5, size=18)) +

                   # ggtitle(paste0("Standard Curve for ", input$SC_project_input,  "\n",
                   #                lm_eq(SC_plot_data)))
                   ggtitle(paste0("Standard Curve ",
                                  lm_eq(SC_plot_data)))

        ) %>%

          layout(margin = list(l=50, r=50, b=100, t=100, pad=4),
                 annotations = list(x = 1.1, y = -0.17,
                                    text = "Source: MDMAPR-CC-BY",
                                    showarrow = F,
                                    xref='paper',
                                    yref='paper',
                                    font=list(size=12, color="darkblue"))))
    })
  }))

  observeEvent(input$Uploaded_SC_reset, {
    shinyjs::reset("SC_platform")
    updateSelectInput(session,
                      inputId = "SC_platform",
                      label = "qPCR Platform",
                      choices = c("None",
                                  "StepOnePlus",
                                  "Biomeme two3/Franklin",
                                  "MIC/BioRad"),
                      selected = "None")

    shinyjs::reset("SC_fluorescence_file")
    shinyjs::reset("SC_metadata_file")
    shinyjs::reset("SC_overview_table")
  })

  ############ Standard Curve Re-calibration based on wells selected ##########
  observeEvent(input$std_recalib, isolate({

    output$standardCurve_plot <- renderPlotly({
      req(standard_curve_tab_data())
      #filter data based on well selected
      SC_plot_data <- standard_curve_tab_data()
      SC_plot_data <- SC_plot_data[SC_plot_data$wellLocation==input$SC_wells, ]

      #Remove control records
      SC_plot_data <- control_records_to_remove(SC_plot_data)

      #Change standard concentration value to numeric and then take log value
      SC_plot_data$standardConc <- as.numeric(SC_plot_data$standardConc)
      SC_plot_data$standardConc <- log(SC_plot_data$standardConc)

      #Change user provided Cq to numeric value to numeric
      SC_plot_data$systemCalculatedCqValue <- as.numeric(SC_plot_data$systemCalculatedCqValue)
      SC_plot_data$systemCalculatedLOQ<- as.numeric(SC_plot_data$systemCalculatedLOQ)
      SC_plot_data$systemCalculatedLOD<- as.numeric(SC_plot_data$systemCalculatedLOD)

      #Add column with residual values to data set
      regression_line <- lm(as.numeric(systemCalculatedCqValue) ~ as.numeric(standardConc), SC_plot_data)
      SC_plot_data$Residual <- abs(residuals(regression_line))

      #Code to get R squared
      #Adapted from: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
      # Code to get equation of the line and R-squared
      # Adapted from: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

      lm_eq <- function(df){

        model1 <- lm(systemCalculatedCqValue ~ standardConc, df, na.action=na.exclude)

        b = format(unname(coef(model1)[1]), digits = 2)

        mx = paste("(", format(unname(coef(model1)[2]), digits = 2), "x", ")",  sep = "")

        r2 = format(summary(model1)$r.squared, digits = 3)

        equation_of_line <- paste("y", " = ", mx, " + ", b, ",   ", "R-squared", " = ", r2,  sep = "")

        return (equation_of_line)

      }



      print(

        ggplotly(height = 700,

                 ggplot(data = SC_plot_data, aes(x = standardConc,
                                                 y = systemCalculatedCqValue,
                                                 color = Residual)) +

                   geom_point(size = 10, alpha = .3) +

                   geom_smooth(method = "lm",
                               se = FALSE,
                               alpha = .15,
                               color = 'black',
                               formula = y ~ x) +

                   geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOD),
                              color = "#ea5f94",
                              linetype="dashed") +
                   #
                   geom_vline(xintercept = log(SC_plot_data$systemCalculatedLOQ),
                              color = "#0178A1",
                              linetype="dotted") +

                   geom_text(aes(x= log(LOD),
                                 label="LOD",
                                 y= mean(systemCalculatedCqValue)),
                             colour="#ea5f94",
                             angle=90,
                             vjust = 1.2,
                             size=5) +

                   geom_text(aes(x= log(LOQ),
                                 label="LOQ",
                                 y= mean(systemCalculatedCqValue)*1.2),
                             colour="#0178A1",
                             angle=90,
                             vjust = 1.2,
                             size=5) +

                   geom_text(aes(label=wellLocation)) +


                   xlab("log DNA Copy Number") +

                   ylab("System Calculated Cq") +

                   theme_minimal() +

                   scale_color_gradient(low = "#fbd300", high = "#0000ff") +

                   theme( axis.title.x = element_text( size=13),
                          axis.title.y = element_text( size=13),
                          axis.text.x = element_text(size=12),
                          axis.text.y = element_text(size=12),
                          plot.title = element_text(hjust = 0.5, size=18)) +

                   # ggtitle(paste0("Standard Curve for ", input$SC_project_input,  "\n",
                   #                lm_eq(SC_plot_data)))
                   ggtitle(paste0("Standard Curve ",
                                  lm_eq(SC_plot_data)))

        ) %>%

          layout(margin = list(l=50, r=50, b=100, t=100, pad=4),
                 annotations = list(x = 1.1, y = -0.17,
                                    text = "Source: MDMAPR-CC-BY",
                                    showarrow = F,
                                    xref='paper',
                                    yref='paper',
                                    font=list(size=12, color="darkblue"))))
    })
  }))

  ############ Standard Curve Table Output ##########
  output$SC_overview_table  <- renderDataTable({

    data <- standard_curve_tab_data()[ , -c(1, 2, 3, 4, 102)]

    datatable(data,
              options = list(scrollX = TRUE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '500px', targets = c(84)))))})

  
  
  
  ########### Low Quant eDNA LOD Method ###########
output$lowquant_standardCurve_plot <- renderPlot({
  req(standard_curve_tab_data())
  
  DAT <- standard_curve_tab_data()[, c("standardCurveName", "runRecordedBy", "systemCalculatedCqValue", "standardConc")]
  # rename the columns to match the calculation
  colnames(DAT) <- c("Target", "Lab", "Cq", "SQ")
  
  ## Ensure data is in the proper format:
  DAT$Target <- as.factor(DAT$Target)
  DAT$Lab <- as.factor(DAT$Lab)  #ML
  DAT$Cq <- suppressWarnings(as.numeric(as.character(DAT$Cq))) #Non-numerical values (i.e. negative wells) will be converted to NAs
  DAT$SQ <- suppressWarnings(as.numeric(as.character(DAT$SQ))) #Non-numerical values (i.e. NTC) will be converted to NAs
  # by MDMAPR Definitions, if a Cq value is 40, there is no amplification and should be converted to NA
  DAT$Cq[which(DAT$Cq==40)] <- NA
  
  # setting all negative controls to 0
  DAT$SQ[is.na(DAT$SQ)] <- 0
  DAT.df <- data.frame(DAT)
  
  
  # compute poisson estimates
  DAT.Tar.SQ <-  DAT.df %>%
    group_by(Target, SQ) %>%
    dplyr::summarise(detect=sum(!is.na(Cq)), n=n(),  Cqmean=mean(Cq, na.rm=TRUE), 
                     Lab=Lab[1])
  DAT.Tar.SQ <- droplevels(data.frame(DAT.Tar.SQ))
  uLabs <- unique(DAT.Tar.SQ$Lab) #unique labs 
  
  DAT.Tar.SQ <- arrange(DAT.Tar.SQ, Lab, Target, SQ) #sort data by SQ in Target in Lab

  ## Add variables to data set:  L10.SQ, phat, ... 
  DAT.Tar.SQ <- within(DAT.Tar.SQ, {
    L10.SQ <- log10(SQ)  
    phat <- detect/n           #sample proportion detect
    vphat <- phat*(1-phat)/n   #var of phat
    lamhat <- -log(1-phat) 
    vlamhat <- phat/n/(1-phat)  #var of lamhat using the delta method
    sdlamhat <- sqrt(vlamhat)   #sd of lamhat using the delta method
    MElamhat <- 1.96*sdlamhat  #margin of error for lambda hat using delta method
  }
  )
  print("got here")
  ## All Targets and Labs **DO NOT DUPLICATE Target names over Labs!!
  uLabs <- unique(DAT.Tar.SQ$Lab)
  uTargets <- unique(DAT.Tar.SQ$Target)
  nTargets <- length(uTargets)
  uLabsTargets <- unique(DAT.Tar.SQ[,c('Lab','Target')])
  uLabsTargets$Lab <- as.character(uLabsTargets$Lab)
  
  #ensure ulabsTargets in same order as uTargets
  uLabsTargets <- uLabsTargets[match(uLabsTargets$Target, uTargets),]  
  uLabsTargets.names <- apply(uLabsTargets, 1, paste, collapse=', ')
  
  DAT.Tar.SQ <- within(DAT.Tar.SQ, {
    CIexphat.lower <-  1 - qbeta(.975, n-detect+1, detect)  #exact phat bounds
    CIexphat.upper <-  qbeta(.975, detect+1, n-detect)
    
    ## Use transformed exact phat bounds
    Lamhatex.Lower <- -log(1 - CIexphat.lower)
    Lamhatex.Upper <- -log(1 - CIexphat.upper)
  }
  )
  print("got here2")
  nndetect <- vector("list", nTargets) 
  nrowTarget <- rep(0, length=nTargets)
  
  for(i in 1:nTargets) {
    
    print(paste0(nTargets, "is there a mistake"))
    
    Target.dat <- subset(DAT.Tar.SQ, Target==uTargets[i])
    print(Target.dat)
    bSQ <- !is.na(Target.dat$phat)  
    lastSQ <- as.logical(cumprod(Target.dat$phat!=1 & bSQ)) 
    ## removes first observations with SQ with phat=1 and larger SQs
    Target.dat <- Target.dat[lastSQ,]
    print(Target.dat)
    print("with last sq")
    nndetect[i] <- list(Target.dat )
    nrowTarget[i] <- nrow(Target.dat)
    
    print(nrow(nndetect[[i]]))
    
    if(nrow(nndetect[[i]]) < 2) {print("we got here");next}
    
    maxSQ <- max(Target.dat$SQ)
    maxlamhat <- max(Target.dat$lamhat)
    
    ######################### show this plot on the standard curve tab
    print("got here 3")
    plot(Target.dat$SQ, Target.dat$lamhat, xlog=TRUE, ylab='Lambda hat',
         xlab='Starting copy number',
         ylim=c(0, maxlamhat), xlim=c(0, maxSQ), main=uLabsTargets.names[i])
    ## Transformed Exact CI
    arrows(Target.dat$SQ, Target.dat$Lamhatex.Lower, Target.dat$SQ, 
           Target.dat$Lamhatex.Upper,
           length=0.05, angle=90, code=3)
    ## overlay simple regression line and R-squared
    jlm <- lm(lamhat ~ SQ, data=Target.dat)
    abline(jlm, col=2)
    legend("topleft", paste('lm Rsq=',round(summary(jlm)$r.squared, 2)), bty="n")
    print("got here 4")

  }
})  
  
  
  
  #qPCR Data Overview page ---------------------------

  ##Presence Absence table

  # Filter options on data overview page
  #Update assay list on page when additional file is uploaded
  DA_assay_list <- reactive({

    if (!is.null(uploaded_data())) {

      data <- as.data.frame(uploaded_data())

      assay_data <- append('None', as.character(unique(data$assayName)))

      return(assay_data)}

    else {return(NULL)}
  })

  observe({updatePickerInput(session,
                             "DA_assay_input",
                             choices = DA_assay_list(),
                             selected = 'None')})


  #Update machine based on assay selection
  DA_runPlatform_list <- reactive({

    if (input$DA_assay_input != 'None') {

      data <- as.data.frame(uploaded_data())

      updated_list <- data[data$assayName == input$DA_assay_input, ]

      runPlatform_list <-  as.character(unique(updated_list$runPlatform))

      return(runPlatform_list)}

    else {return(NULL)}
  })

  observe({updatePickerInput(session,
                             "DA_machine_input",
                             choices = append('None', DA_runPlatform_list()),
                             selected = 'None')})


  #Update project list based on assay selection
  DA_project_list <- reactive({

    if (input$DA_machine_input != 'None') {

      data <- as.data.frame(uploaded_data())

      updated_list <- data[data$assayName == input$DA_assay_input, ]
      updated_list <- updated_list[updated_list$runPlatform == input$DA_machine_input, ]
      project_list <-  as.character(unique(updated_list$projectName))

      return(project_list)}

    else {return(NULL)}
  })


  observe({updatePickerInput(session,
                             "DA_project_input",
                             choices = append('None', DA_project_list()),
                             selected = 'None') })


  #Transform dataframe for presence/absence table based on filtered
  presence_absence_table_data <- reactive({

    if (!is.null(uploaded_data())) {

      data <- uploaded_data()
      PA_filtered_data <- data[data$assayName == input$DA_assay_input, ]
      PA_filtered_data <- PA_filtered_data[PA_filtered_data$runPlatform == input$DA_machine_input, ]
      PA_filtered_data <- PA_filtered_data[PA_filtered_data$projectName == input$DA_project_input, ]

    }
  })

  what_clr <- function(value) {
    if (value >= input$cqValueCutoff)
    {return ("#8FBACB")}

    else
    {return("#ffb14e")}
  }


  available_threshold <- function(value) {
    if (value == "Unable to Determine Threshold")
    {return ("#ffd700")}
  }

  # #Presence/absence table
  observeEvent(input$submit, isolate ({

    output$presence_absence_table <- renderReactable({
      data <- as.data.frame(presence_absence_table_data())

      prescence_abscence_table <- data[ , c("projectName", "runID", "extractName", "control", "geneSymbol", "runPlatform", "wellLocation", "userProvidedThresholdValue", "userProvidedCqValue", "systemCalculatedThresholdValue", "systemCalculatedCqValue" )]

      reactable(prescence_abscence_table,

                #Table columns
                columns = list(

                  projectName = colDef(name = "Project Name",align = "center", width = 300),
                  runID = colDef(name = "Plate ID", align = "center"),
                  extractName = colDef(name = "Sample Name", align = "center", width = 200),
                  control = colDef(name = "Control", align = "center"),
                  geneSymbol = colDef(name = "Gene", align = "center"),
                  runPlatform = colDef(name = "Machine", align = "center"),
                  wellLocation = colDef(name = "Well Location", align = "center", width = 200),

                  userProvidedThresholdValue = colDef(name = "User Provided Threshold",
                                                      align = "center",
                                                      width = 300),

                  # userProvidedCqValue = colDef(name = "User Provided Cq Value",
                  #                              width = 250,
                  #                              style = function(value) {
                  #                                color  <- what_clr(value)
                  #                                list(background = color)}),

                  systemCalculatedThresholdValue = colDef(name = "System Calculated Threshold",
                                                          width = 300,
                                                          align = "center",
                                                          style = function(value) {
                                                            color  <- available_threshold(value)
                                                            list(background = color)}),

                  systemCalculatedCqValue = colDef(name = "System Calculated Cq Value",
                                                   width = 250,
                                                   style = function(value) {
                                                     color  <- what_clr(value)
                                                     list(background = color)})),

                #Filter each column by text
                filterable = TRUE,

                #Type in page number to jump to a page
                paginationType = "jump",

                #Minimum rows shown on page
                minRows = 20,

                #Number of rows to show
                defaultPageSize = 20,

                #Adding outline around cells
                outlined = TRUE,

                #Color every other row
                striped = TRUE,

                #Hover above row to highlight it
                highlight = TRUE,

                #Default record selected from table
                defaultSelected = 1,

                #Check box
                selection = "single",

                #Wrap text in column
                wrap = FALSE,

                theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee",
                                                               boxShadow = "inset 2px 0 0 0 #ffa62d"))
      )
    })
  }))
  #
  #
  # ## Amplification plot
  #
  # #Get selected row for amplification plot
  selected <- reactive(getReactableState("presence_absence_table", "selected"))
  #
  # #Created amplifcation plot based on selected well sample
  #
  observeEvent(input$submit, isolate({

    output$selected <-  renderPlotly({

      #Created dataframe of filtered presence/absence data
      data <- as.data.frame(presence_absence_table_data())[selected(), ]


      #Create data frame for amplification curve
      amp_curve_data <- na.omit(as.data.frame(t(data[ , c(14:83)])))
      colnames(amp_curve_data) <- "Fluorescence"
      amp_curve_data$cycles <- c(1:nrow(amp_curve_data))


      #Created plot
      print(
        ggplotly(height = 700,

                 ggplot(amp_curve_data, aes(x = cycles, y = as.numeric(Fluorescence))) +

                   geom_point(aes(colour = "Absorbances") , size = 2) +

                   geom_hline(aes(yintercept = as.numeric(data$userProvidedThresholdValue),
                                  color = "User Provided Threshold"),
                              linetype="dashed", size = 1) +

                   geom_hline(aes(yintercept = as.numeric(data$systemCalculatedThresholdValue),
                                  color = "System Calculated Threshold"),
                              linetype="dotted", size = 1) +


                   ggtitle(paste0( "Well ", data$wellLocation, " Amplification Curve")) +

                   labs(x = " Cycle", y = "Absorbance") +

                   theme_gray() +

                   scale_colour_manual("",
                                       breaks = c("Absorbances",
                                                  "User Provided Threshold",
                                                  "System Calculated Threshold"),
                                       values = c("User Provided Threshold"="#ea5f94",
                                                  "Absorbances"="#0000ff",
                                                  "System Calculated Threshold"="#ff8600")) +

                   theme(plot.title = element_text(hjust = 0.5, size=18),
                         axis.title.x = element_text( size=13),
                         axis.title.y = element_text( size=13),
                         axis.text.x = element_text(size=12),
                         axis.text.y = element_text(size=12),
                         legend.text = element_text(size = 10),
                         legend.background = element_rect(fill="lightblue")))  %>%

          layout(legend = list(orientation = "h", x = 0.02, y = -0.16), #legend position
                 margin = list(l=50, r=60, b=140, t=100, pad=4),
                 annotations = list(x = 1, y = -0.31,
                                    text = "Source: MDMAPR-CC-BY",
                                    showarrow = F,
                                    xref='paper',
                                    yref='paper',
                                    font=list(size=12,
                                              color="darkblue"))))})
  }))


  ## welcome page (Create downloadable metadata template) -----

  #Created  metadata template for users to download
  project_sheet <- data.frame(matrix(ncol = 24, nrow = 1))

  colnames(project_sheet) <- c("projectID", "projectCreationDate","projectName","projectRecordedBy","projectOwner","projectContactEmail","projectDescription","InstitutionID","projectDataNotes","geographicRegionID", "continent","country", "stateProvince","municipality","siteID", "locality","estimatedPerimeter","estimatedSurfaceArea(m2)","siteType","siteLength(m2)", "stationID","stationName", "decimalLongitude", "decimalLatitude")


  replicate_sheet <- data.frame(matrix(ncol = 55, nrow = 1))
  colnames(replicate_sheet) <- c("replicateID", "stationID", "collectorName","replicateName","collectionDate","collectionTime","storageID","DateOfStorage","methodOfStorage","minimumElevationInMeters","maximumElevationInMeters","verbatimElevation","minimumDepthInMeters","maximumDepthInMeters","verbatimDepth","flowRate(m/s)", "filterType","filtrationDuration(mins)","volumeFiltered","processLocation","replicationNumber","riparianVegetationPercentageCover","dissolvedOxygen(mg/L)","waterTemperature(C)","pH","TSS(mg/L)","EC(uS/cm)","turbidity(NTU)","discharge","tide","chlorophyl","salinity(ppt)","contaminants(ng/g)","traceMetals(mg/kg)","organicContent(%)","microbialActivity","grainSize","replicateDataNotes", "extractID", "extractName","analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes","tubePlateID","frozen", "fixed","dnaStorageLocation","extractMethodOfStorage","dnaVolume","quantificationMethod", "concentration(ng/ul)")


  assay_sheet <- data.frame(matrix(ncol = 30, nrow = 1))
  colnames(assay_sheet) <- c( "assayID", "establishmentMeans","assayName","assayOwnership","assayDescription", "assayCitation", "assayDate", "geneTarget", "geneSymbol","dilutions", "replicates", "primerR", "primerF", "probe","ampliconLength (bp)", "probeFluorescentTag", "dye(s)","quencher","probeModification", "taxonID", "kingdom","phylum","class","order","family", "genus", "subgenus", "species", "vernacularName","organismScope")

  results_sheet <- data.frame(matrix(ncol = 29, nrow = 1))
  colnames(results_sheet) <- c("resultID","assayID", "extractID", "wellLocation","sampleName", "copyNumber", "control", "userProvidedThresholdValue", "userProvidedCqValue", "runID", "runRecordedBy", "runDate", "runTime","runPlatform","machineID", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")

  standardCurveResults_sheet <- data.frame(matrix(ncol = 36, nrow = 1))
  colnames(standardCurveResults_sheet ) <- c("SCresultID","wellLocation","sampleName", "copyNumber", "control","standardConc",   "userProvidedThresholdValue", "userProvidedCqValue","runID", "runRecordedBy", "runDate", "runTime", "runPlatform","machineID", "standardCurveID","assayID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes", "LOD","LOQ", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")


  data_list <- (list( project_Table = project_sheet,
                      replicate_Table = replicate_sheet,
                      assay_Table = assay_sheet,
                      results_Table = results_sheet,
                      standardCurveResults_Table = standardCurveResults_sheet))

  output$downloadTemplate <- downloadHandler(
    filename = 'MDMAPR_metadata_template.xlsx',
    content = function(file) {write_xlsx(data_list, file)})




# Data Modelling page ---------------------------

  dd <- mtcars
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("report", sep = ".", "html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(n = input$selectData)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,

                        envir = new.env(parent = globalenv()))
    }
  )

  # initialize the reactive value
  InputDataset <- reactiveVal(NULL)

# read in the uploaded csv file, also removing all columns that are all NA
  observeEvent(input$submit_modelling,{
    modelling_data <- as.data.frame(read.csv(input$model_data$datapath))
    # need to convert all NULL to NA
    modelling_data[modelling_data=="NULL"] <- NA
    modelling_data <- modelling_data[colSums(!is.na(modelling_data)) > 0]

    # convert all character variables into factor
    modelling_data[sapply(modelling_data, is.character)] <- lapply(modelling_data[sapply(modelling_data, is.character)],
                                                                   as.factor)
    # remove the columns that have only one value throughout.
    modelling_data <- modelling_data[vapply(modelling_data, function(x) length(unique(x)) > 1, logical(1L))]
    return(InputDataset(modelling_data))

  })


  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- as.data.frame(InputDataset())
    }
    else{
      dt <- as.data.frame(InputDataset()[, c(input$SelectX)])
    }

  })


  output$Xvarselection <- renderUI({
    box(
    pickerInput("SelectX",
                "Select variables:",
                multiple = TRUE,
                choices = as.character(names(InputDataset_model())),
                selected=as.character(names(InputDataset_model())),
                width="200px"),
    status = "primary",
    width = "300px",
    height = "200px",
    solidHeader = TRUE,
    title = "X variables")
  })

  #
  output$SelectY <-  renderUI({
    box(
      selectizeInput('SelectY',
                     "Select variable to predict:",
                  multiple = F,
                  choices = as.character(names(InputDataset())),
                  selected=1,
                  width="200px"),
      status = "primary",
      width = "300px",
      height = "200px",
      solidHeader = TRUE,
      title = "Y Variable")
  })


  # observe({
  #   lstname <- names(InputDataset())
  #   updateSelectInput(session = session,
  #                     inputId = "SelectY",
  #                     choices = lstname)
  # })

  splitSlider <- reactive({
    input$Slider1 / 100
  })
  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(InputDataset()))
  output$structure <- renderPrint(str(InputDataset()))

  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data

  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })

  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })



  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))

  output$Data <- renderDT(InputDataset())

  # identify variables that are numeric for correlation matrix
  numeric_model_input <- reactive({
    nums <- unlist(lapply(InputDataset(), is.numeric))
    return(InputDataset()[ , nums])
  })

  cormat <- reactive({
    round(cor(numeric_model_input()), 1)
  })
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))

  correlationMatrix <- reactive({
    cor(numeric_model_input())
    print(cor(numeric_model_input()))
  })

   # it's this line that causes problems
  output$CorrMatrix <-
    renderPrint(round(as.data.frame(correlationMatrix()), 4))
  #

  #
  # #Code section for Linear Regression-----------------------------------------------------------------------------
  #
  # f <- reactive({
  #   as.formula(paste(input$SelectY, "~."))
  # })
  #
  # #observe({print(f())})
  #
  # # tmp_model <- lm(mpg ~., data = mtcars)
  # # vardt <- as.data.frame(varImp(tmp_model, scale = FALSE))
  # # varnames <- as.data.frame(rownames(vardt))
  # # impvalue <- as.data.frame(vardt$Overall)
  # # FinalVal <- cbind(varnames,impvalue)
  #
  #
  #
  #
  #
  # Linear_Model <- reactive({
  #   lm(f(), data = trainingData())
  # })
  #
  # output$Model <- renderPrint(summary(Linear_Model()))
  # output$Model_new <-
  #   renderPrint(
  #     stargazer(
  #       Linear_Model(),
  #       type = "text",
  #       title = "Model Results",
  #       digits = 1,
  #       out = "table1.txt"
  #     )
  #   )
  #
  # Importance <- reactive({
  #   varImp(Linear_Model(), scale = FALSE)
  # })
  #
  # tmpImp <- reactive({
  #   #varImp(Linear_Model())
  #   imp <- as.data.frame(varImp(Linear_Model()))
  #   imp <- data.frame(overall = imp$Overall,
  #                     names   = rownames(imp))
  #   imp[order(imp$overall, decreasing = T),]
  #
  # })
  #
  # output$ImpVar <- renderPrint(tmpImp())
  #
  # price_predict <- reactive({
  #   predict(Linear_Model(), testData())
  # })
  #
  # tmp <- reactive({
  #   tmp1 <- testData()
  #   tmp1[, c(input$SelectY)]
  # })
  #
  #
  # actuals_preds <-
  #   reactive({
  #     data.frame(cbind(actuals = tmp(), predicted = price_predict()))
  #   })
  #
  # Fit <-
  #   reactive({
  #     (
  #       plot(
  #         actuals_preds()$actuals,
  #         actuals_preds()$predicted,
  #         pch = 16,
  #         cex = 1.3,
  #         col = "blue",
  #         main = "Best Fit Line",
  #         xlab = "Actual",
  #         ylab = "Predicted"
  #       )
  #     )
  #   })
  #
  # output$Prediction <- renderPlot(Fit())
  #
  # output$residualPlots <- renderPlot({
  #   par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
  #   plot(Linear_Model())
  #   par(mfrow = c(1, 1)) # Change back to 1 x 1
  #
  # })
  #
  # output$digest <- renderExplorer({
  #
  #   explorer(data = dd$data, demo = F)
  #   #codebook(mtcars)
  # })


}



