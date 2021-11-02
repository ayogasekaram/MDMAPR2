source('./R/example-module.R')
source('./R/example.R')

library(htmlwidgets)

  process_Multiplexed_RDML <- function (rdml_file) {
    # read in rdml file
    raw_data <- RDML$new(filename = rdml_file)
    #pull all the fluorescence data
    fdata <- as.data.frame(raw_data$GetFData(long.table = T))
    # subset the data by the target
    list_of_fdata = split(fdata, f = fdata$target)
    # apply the dataframe formatting function to all dataframes in the list
    list_to_return <- lapply(list_of_fdata, df_formatting_by_target)
    return (list_to_return)
  }

    format_qPCR_metadata <- function(metadataFile) {

    #Read in sheets
    project_Table <- read_xlsx(metadataFile,sheet = 1)
    replicate_Table <- read_xlsx(metadataFile,sheet = 2)
    assay_Table <-  read_xlsx(metadataFile, sheet = 3)
    results_Table <- read_xlsx(metadataFile, sheet = 4)
    standardCurve_Table <- read_xlsx(metadataFile, sheet = 5)


    #Creating metadata table
    metadata_table <-
      distinct(left_join(assay_Table, standardCurve_Table[, c("standardCurveID", "assayID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes")], by = "assayID"))

    metadata_table <-  left_join(results_Table, metadata_table, by = "assayID")

    metadata_table <- left_join(metadata_table, replicate_Table, by = "extractID")

    metadata_table <- left_join(metadata_table, project_Table, by = "stationID")


    #Organize columns
    metadata_table <- as.data.frame(metadata_table[, c(
      "resultID","runID", "assayID","pcrChemistryID","extractID","wellLocation","sampleName", "copyNumber","control","userProvidedThresholdValue", "userProvidedCqValue","runRecordedBy", "runDate","runTime","runPlatform","machineID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch","reversePrimerBatch","dNTPConcentration", "primerConcentration","probeConcentration","Mg2+Concentration","polymeraseBatch","polymeraseConcentrations","thermocyclerParameters","pcrDataNotes","taxonID","establishmentMeans","assayName","assayOwnership","assayDescription","assayCitation","assayDate","geneTarget","geneSymbol","dilutions","replicates","primerR","primerF","probe","ampliconLength (bp)","probeFluorescentTag","dye(s)","quencher","probeModification","kingdom","phylum","class","order","family","genus","subgenus","species","vernacularName","organismScope","replicateID","extractName","analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes","tubePlateID","frozen", "fixed","dnaStorageLocation","extractMethodOfStorage","dnaVolume","quantificationMethod", "concentration(ng/ul)","stationID","collectorName","replicateName","collectionDate","collectionTime","storageID","DateOfStorage","methodOfStorage","minimumElevationInMeters","maximumElevationInMeters","verbatimElevation","minimumDepthInMeters","maximumDepthInMeters","verbatimDepth", "flowRate(m/s)", "filterType","filtrationDuration(mins)","volumeFiltered","processLocation","replicationNumber","riparianVegetationPercentageCover","dissolvedOxygen(mg/L)","waterTemperature(C)","pH","TSS(mg/L)","EC(uS/cm)","turbidity(NTU)","discharge","tide","chlorophyl","salinity(ppt)","contaminants(ng/g)","traceMetals(mg/kg)","organicContent(%)","microbialActivity","grainSize","replicateDataNotes","siteID","stationName","decimalLongitude","decimalLatitude","geographicRegionID","locality","estimatedPerimeter","estimatedSurfaceArea(m2)","siteType","siteLength(m2)","projectID","continent","country","stateProvince","municipality","projectCreationDate","projectName", "projectRecordedBy","projectOwner","projectContactEmail","projectDescription","InstitutionID","projectDataNotes","standardCurveID","standardCurveName","SCdate","SCrecordedBy","SCdataNotes")])
    return(metadata_table)

  }

  remove_null_records <- function(meta_data, fluor_file_list){
    # This function will change the list and metadata files in place
    meta_data_name = deparse(substitute(meta_data))
    fluor_file_list_name = deparse(substitute(fluor_file_list))
    if(length(which(grepl("null", tolower(meta_data$sampleName))))!=0){
      # get the position of the records that have null in the sample names
      control_well_locations <- as.character(meta_data$wellLocation[which(tolower(meta_data$sampleName)=="null")])
      # remove these records from the fluorescence files (if present)
      fluor_file_list <- lapply(fluor_file_list, function(x) x[-c(which(as.character(x$wellLocation) %in%
                                                                          control_well_locations)),])
      # remove these records from the metadata
      meta_data <- meta_data[-c(which(grepl("null", tolower(meta_data$sampleName)))), ]}
      # create a list to store these elements (they will be unlisted and brought back into the global environment)
      cleaned_data <- list(fluor_file_list, meta_data)
      return(cleaned_data)
  }

  ############### Calculating the Threshold Value ##################
  calculate_second_deriv_threshold <- function(fluorescence_values_df) {

        fluorescence_values <- as.data.frame(t(fluorescence_values_df))

    # ensure all values are NUMERIC
    fluorescence_values[] <- lapply(fluorescence_values, as.numeric)

    #New table with threshold data
    thresholdData <- as.data.frame(matrix(nrow = ncol(fluorescence_values) , ncol = 1))
    colnames(thresholdData) <- c("systemCalculatedThresholdValue")
    rownames(thresholdData) <- colnames(fluorescence_values)

    for (runSample in 1:ncol(fluorescence_values)) {

      #Get the total number of thremocycler cycles for this sample
      number_of_cycles <- length(fluorescence_values [,runSample])

      #Set the initial reference absorbance to the minimum absorbance for the sample
      reference_absorbance_value <- as.numeric(min(fluorescence_values[, runSample]))

      #Set the initial absorbance cycle to the cycle with the minimum absorbance value
      place_holder_cycle = which.min(fluorescence_values[, runSample])

      #Calculate the absorbance range for the sample to be able to assess the percent change from cycle to cycle
      absorb_range = as.numeric(max(fluorescence_values[, runSample])) - as.numeric(min(fluorescence_values[, runSample]))


      #Step through the data points for the cycles from the minimum value to the end of the cycles to see if the data is inceasing within a certain percentage as compared to the total absorbance range for the sample
      for (cycle_number_counter in which.min(fluorescence_values[, runSample]):length(fluorescence_values[, runSample])) {

        #Calculate the difference between the reference (first, initially set to the minimum value for the dataset) value and the test value (current value in the data series for this loop)
        difference = as.numeric(reference_absorbance_value - as.numeric(fluorescence_values[cycle_number_counter, runSample]))

        #Check to see if the difference between the reference and the test divided by the total range is greater than 0.01.
        #NOTE: could have the minimum variation between successive data points a user defined value here we use less than 1%
        #If yes then making the place holder cycle equal to this cycle number as I will then only use data after this cycle to calculate the threshold.
        if((difference/absorb_range) >= 0.01) {

          #Update place holder value cycle number
          place_holder_cycle <- cycle_number_counter
        }

        #Setting the reference equal to the value at this loop to represent the reference for the next loop where the test value will be incremented to the absorbance value for the next cycle for the sample
        reference_absorbance_value <- as.numeric(fluorescence_values[cycle_number_counter, runSample])

      } # Closing loop

      ########################### Obtaining the threshold value ###########################

      #NOTE: could have the minimum number of data points to calculate the threshold as a user defined value
      #Finally, checking to see if more than 75% of the data points passed these quality checks for use in calculating the threshold. if not then bad data no threshold calculation is conducted.
      if ((place_holder_cycle/number_of_cycles) < 0.75) {

        #Subset dataframe to plot it
        data_to_plot <- as.data.frame(as.numeric(t(fluorescence_values[, runSample])))

        data_to_plot <- cbind(as.data.frame(c(1:number_of_cycles)), data_to_plot)

        data_to_plot <- data_to_plot[-c(1:place_holder_cycle), ]

        # This is the section where I get the second derivative of the curve and determine the value at which we will set the threshold

        deriv <- function(x, y)
          diff(y) / diff(x)

        middle_pts <- function(x)
          x[-1] - diff(x) / 2

        second_d <- deriv(middle_pts(data_to_plot[, 1]), deriv(data_to_plot[, 1], data_to_plot[, 2]))

        #Getting the max value of the second derivative data set. This will represent the points between the values on the curve between the end of the noise data and the end of the data set.
        #So we need to add the max value index to the placeholder and then add one to round up to the next value to get the index (or cycle number) on the original data set.
        max_deriv_value_index <- which.max(second_d)

        #The theshold value at the cycle determined by the double derivitave is...
        #we need to add one
        threshold_value <- fluorescence_values[(place_holder_cycle + max_deriv_value_index +1),runSample]


        #Add threshold value to dataframe
        thresholdData$systemCalculatedThresholdValue[runSample] <- threshold_value

      }

      #If no threshold value can be computed
      else {thresholdData$systemCalculatedThresholdValue[runSample] <- "Unable to Determine Threshold" }
    }
    thresholdData$wellLocation <- rownames(thresholdData)
    fluorescence_values_df <- as.data.frame(fluorescence_values_df)
    fluorescence_values_df$wellLocation <- rownames(as.data.frame(fluorescence_values_df))
    fluorescence_values_df <- merge(fluorescence_values_df, thresholdData, by="wellLocation")
    rownames(fluorescence_values_df) <- fluorescence_values_df$wellLocation
    return(fluorescence_values_df)
  }

appServer <- function(input, output, session) {
  # ======== Modules ========
  # exampleModuleServer is defined in R/example-module.R
  exampleModuleServer("examplemodule1")
  exampleModuleServer("examplemodule2")
  # =========================

  data <- reactive({
    # lexical_sort from R/example.R
    lexical_sort(seq_len(input$size))
  })
  output$sequence <- renderText({
    paste(data(), collapse = " ")
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

      #View map full screen (note: only works in web broswer)
      addFullscreenControl() %>%


      #Default map view --> Change to Guelph
      setView(lng = -80.2262, lat = 43.5327, zoom = 3) %>%

      #Change leaflet map tiles
      addProviderTiles(providers$Esri.WorldStreetMap)
  })

  observeEvent(input$submit, {

      updateTabItems(session, "tab_being_displayed", "dashboard")

      # 1. Read in the RDML file
      # browser()
      print("HELLO")
      # print(input$qpcr_file$datapath)

      # raw_multiplex_data_list <- process_Multiplexed_RDML(input$qpcr_file$datapath)

      # # 2. Read in and format the metadata

      # formatted_metadata <- format_qPCR_metadata(input$metadata_file$datapath)

      # # 3. remove the control records

      # controls_removed <- remove_null_records(formatted_metadata, raw_multiplex_data_list)
      # # 3b. Let's separate the controls list object
      # formatted_metadata <- controls_removed[[2]]
      # raw_multiplex_data_list <-controls_removed[[1]]

      # # 4. Calculate the second derivative threshold

      # # use lapply to set the wellLocations as the rownames
      # raw_multiplex_data_list <- lapply(raw_multiplex_data_list, "rownames<-", raw_multiplex_data_list[[1]]$wellLocation)
      # well_names <- raw_multiplex_data_list[[1]]$wellLocation

      # # remove the column that contains that information
      # raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x){x <- x[,-1]})

      # # convert all columns into numeric values
      # raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x) {sapply(x, as.numeric)})
      # raw_multiplex_data_list <- lapply(raw_multiplex_data_list, "rownames<-", well_names)

      # # calculate threshold
      # raw_multiplex_data_list <- lapply(raw_multiplex_data_list,calculate_second_deriv_threshold)

      # # Add the userprovided threshold value
      # raw_multiplex_data_list <- lapply(raw_multiplex_data_list, function(x){merge(x, formatted_metadata[ , c("userProvidedThresholdValue", "wellLocation")], by="wellLocation")})

      # # 5. Calculate the Cq value using the threshold
      # raw_data_with_Cq <- lapply(raw_multiplex_data_list, function(x){add_Cq(x, "systemCalculatedThresholdValue", "systemCalculatedCqValue")})

      # # 6. If the user has threshold values provided, calculate the Cq value with that threshold
      # if(any(is.na(raw_data_with_Cq[[1]]$userProvidedThresholdValue))){
      #   raw_data_with_Cq<- lapply(raw_data_with_Cq, function(x)cbind(x,CqvaluewithUserThreshold="No Threshold Value Provided by User"))
      # } else{
      #   raw_data_with_Cq <- lapply(raw_data_with_Cq, function(x){add_Cq(x,"userProvidedThresholdValue", "CqvaluewithUserThreshold")})}

      # copy_numbers <- data.frame(wellLocation=formatted_metadata$wellLocation, logDNACopyNumber=rep("NA", nrow(raw_data_with_Cq[[1]])))

      # # Assess if standard curve file has been provided, if so, process the fluorescence and metadata
      # if (is.null(input$SCI_fluorescence_file)==FALSE){

      #   # if the standard curve file is provided, the platform specific processing is required
      #   if (input$platform == "Biomeme two3/Franklin") {
      #     std_fluorescence <- process_biomeme_raw_data(read.csv(input$SCI_fluorescence_file$datapath))}
      #   else if (input$platform == "MIC") {
      #     std_fluorescence <- process_MIC_raw_data(read.csv(input$SCI_fluorescence_file$datapath))}
      #   else if (input$platform == "StepOnePlus") {
      #     #Read in raw qPCR data
      #     std_fluorescence <- process_SOP_uploaded_file(read_excel(input$qpcr_file$datapath, 4))}

      #   # all metadata files are processed with the same function
      #   std_meta <- format_std_curve_metadata(input$metadata_file$datapath)

      #   # 7. Calculate the threshold for the Standard curve data if provided
      #   rownames(std_fluorescence) <- std_meta$wellLocation
      #   std_fluorescence <- std_fluorescence[,-1]
      #   std_w_threshold <- calculate_second_deriv_threshold(std_fluorescence)


      #   # 8. Calculate Cq values for the Standard curve data if provided
      #   std_w_threshold <- add_Cq(std_w_threshold, "systemCalculatedThresholdValue", "systemCalculatedCqValue")


      #   # 9. Merge with copy number information
      #   std_w_threshold <- merge(std_w_threshold, std_meta[, c("wellLocation", "standardConc")])

      #   # 10. Calculate the copy number
      #   copy_numbers <- calculate_copy_number(std_w_threshold, raw_data_with_Cq[[1]])

      # }

      # # 11. Merge all the data
      # all_merged_data <- merge(raw_data_with_Cq[[1]], formatted_metadata, by="wellLocation")
      # # add the copy number
      # all_merged_data <- merge(all_merged_data, copy_numbers, by="wellLocation")

      # # processing the data like cq column intervals
      # all_merged_data <- merged_file_processing(all_merged_data, input$upload_data_name)

      # return(uploaded_data(all_merged_data))

 #   }
})

  #   observeEvent(input$submit, {
  #   output$dataexport <- renderDataTable({
  #     export_data <- as.data.frame(uploaded_data())
  #     datatable(export_data,
  #               options = list(
  #                 scrollX = TRUE
  #               ))
  #   })
  # })

  # observeEvent(input$submit, isolate ({

  #   output$presence_absence_table <- renderReactable({
  #     data <- as.data.frame(presence_absence_table_data())

  #     prescence_abscence_table <- data[ , c("projectName", "runID", "extractName", "control", "geneSymbol", "runPlatform", "wellLocation", "userProvidedThresholdValue", "userProvidedCqValue", "systemCalculatedThresholdValue", "systemCalculatedCqValue" )]

  #     reactable(prescence_abscence_table,

  #               #Table columns
  #               columns = list(

  #                 projectName = colDef(name = "Project Name",align = "center", width = 300),
  #                 runID = colDef(name = "Plate ID", align = "center"),
  #                 extractName = colDef(name = "Sample Name", align = "center", width = 200),
  #                 control = colDef(name = "Control", align = "center"),
  #                 geneSymbol = colDef(name = "Gene", align = "center"),
  #                 runPlatform = colDef(name = "Machine", align = "center"),
  #                 wellLocation = colDef(name = "Well Location", align = "center", width = 200),

  #                 userProvidedThresholdValue = colDef(name = "User Provided Threshold",
  #                                                     align = "center",
  #                                                     width = 300),

  #                 userProvidedCqValue = colDef(name = "User Provided Cq Value",
  #                                              width = 250,
  #                                              style = function(value) {
  #                                                color  <- what_clr(value)
  #                                                list(background = color)}),

  #                 systemCalculatedThresholdValue = colDef(name = "System Calculated Threshold",
  #                                                         width = 300,
  #                                                         align = "center",
  #                                                         style = function(value) {
  #                                                           color  <- available_threshold(value)
  #                                                           list(background = color)}),

  #                 systemCalculatedCqValue = colDef(name = "System Calculated Cq Value",
  #                                                  width = 250,
  #                                                  style = function(value) {
  #                                                    color  <- what_clr(value)
  #                                                    list(background = color)})),

  #               #Filter each column by text
  #               filterable = TRUE,

  #               #Type in page number to jump to a page
  #               paginationType = "jump",

  #               #Minimum rows shown on page
  #               minRows = 20,

  #               #Number of rows to show
  #               defaultPageSize = 20,

  #               #Adding outline around cells
  #               outlined = TRUE,

  #               #Color every other row
  #               striped = TRUE,

  #               #Hover above row to highlight it
  #               highlight = TRUE,

  #               #Default record selected from table
  #               defaultSelected = 1,

  #               #Check box
  #               selection = "single",

  #               #Wrap text in column
  #               wrap = FALSE,

  #               theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee",
  #                                                              boxShadow = "inset 2px 0 0 0 #ffa62d"))
  #     )
  #   })
  # }))

  # observeEvent(input$submit, isolate({

  #   output$selected <-  renderPlotly({

  #     #Created dataframe of filtered presence/absence data
  #     data <- as.data.frame(presence_absence_table_data())[selected(), ]


  #     #Create data frame for amplification curve
  #     amp_curve_data <- na.omit(as.data.frame(t(data[ , c(14:83)])))
  #     colnames(amp_curve_data) <- "Fluorescence"
  #     amp_curve_data$cycles <- c(1:nrow(amp_curve_data))


  #     #Created plot
  #     print(
  #       ggplotly(height = 700,

  #                ggplot(amp_curve_data, aes(x = cycles, y = as.numeric(Fluorescence))) +

  #                  geom_point(aes(colour = "Absorbances") , size = 2) +

  #                  geom_hline(aes(yintercept = as.numeric(data$userProvidedThresholdValue),
  #                                 color = "User Provided Threshold"),
  #                             linetype="dashed", size = 1) +

  #                  geom_hline(aes(yintercept = as.numeric(data$systemCalculatedThresholdValue),
  #                                 color = "System Calculated Threshold"),
  #                             linetype="dotted", size = 1) +


  #                  ggtitle(paste0( "Well ", data$wellLocation, " Amplification Curve")) +

  #                  labs(x = " Cycle", y = "Absorbance") +

  #                  theme_gray() +

  #                  scale_colour_manual("",
  #                                      breaks = c("Absorbances",
  #                                                 "User Provided Threshold",
  #                                                 "System Calculated Threshold"),
  #                                      values = c("User Provided Threshold"="#ea5f94",
  #                                                 "Absorbances"="#0000ff",
  #                                                 "System Calculated Threshold"="#ff8600")) +

  #                  theme(plot.title = element_text(hjust = 0.5, size=18),
  #                        axis.title.x = element_text( size=13),
  #                        axis.title.y = element_text( size=13),
  #                        axis.text.x = element_text(size=12),
  #                        axis.text.y = element_text(size=12),
  #                        legend.text = element_text(size = 10),
  #                        legend.background = element_rect(fill="lightblue")))  %>%

  #         layout(legend = list(orientation = "h", x = 0.02, y = -0.16), #legend position
  #                margin = list(l=50, r=60, b=140, t=100, pad=4),
  #                annotations = list(x = 1, y = -0.31,
  #                                   text = "Source: MDMAPR-CC-BY",
  #                                   showarrow = F,
  #                                   xref='paper',
  #                                   yref='paper',
  #                                   font=list(size=12,
  #                                             color="darkblue"))))})
  # }))

  # Do we need file validations for the data import tab?
}
