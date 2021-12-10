df_formatting_by_target <- function(fluorescence_df, process_type){

  # subset the data with the columns we need for the raw fluorescence
  fluorescence_df <- fluorescence_df[, c( "position", "fluor", "react.id")]
  # split by position
  formatted_df <- do.call('data.frame', split(fluorescence_df, fluorescence_df$position))
  # remove columns with just the well position
  formatted_df <- Filter(function(x)(length(unique(x))>1), formatted_df)
  # rename column with just the delimiter
  colnames(formatted_df) <- sub("\\..*", "", colnames(formatted_df))
  # ensure the well numbers and cycle are captured as the first row in the data frame
  formatted_df <- rbind(colnames(formatted_df), formatted_df)
  # rownames should match the appropriate data (wellLocation then cycle numbers)
  rownames(formatted_df) <- c("wellLocation", paste0("Cycle_Number", 1:(nrow(formatted_df)-1)))
  # transpose the df such that each well is a row
  formatted_df <- as.data.frame(t(formatted_df))
  return(formatted_df)
}

#################### MAIN FUNCTION ####################
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

##############Formatting the metadata File#################
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

############### Format the Standard Curve Metadata ##################
format_std_curve_metadata <- function (standardCurve_metadata) {
  standardCurve_Table <- read_excel(standardCurve_metadata, sheet = 5)
  standardCurve_Table <- as.data.frame(standardCurve_Table[, c("SCresultID",
                                                               "runID",
                                                               "pcrChemistryID",
                                                               "standardCurveID",
                                                               "wellLocation",
                                                               "sampleName",
                                                               "copyNumber",
                                                               "control",
                                                               "standardConc",
                                                               "userProvidedThresholdValue",
                                                               "userProvidedCqValue",
                                                               "runRecordedBy",
                                                               "runDate",
                                                               "runTime",
                                                               "runPlatform",
                                                               "machineID",
                                                               "reactionConditions",
                                                               "reactionVolume",
                                                               "templateAmount",
                                                               "forwardPrimerBatch",
                                                               "reversePrimerBatch",
                                                               "dNTPConcentration",
                                                               "primerConcentration",
                                                               "probeConcentration",
                                                               "Mg2+Concentration",
                                                               "polymeraseBatch",
                                                               "polymeraseConcentrations",
                                                               "thermocyclerParameters",
                                                               "pcrDataNotes",
                                                               "assayID",
                                                               "standardCurveName",
                                                               "SCdate",
                                                               "SCrecordedBy",
                                                               "SCdataNotes",
                                                               "LOD",
                                                               "LOQ")])
  return (standardCurve_Table)
}

############### Removing the control records##################
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

############### Calculating the Cq Value ##################
add_Cq <- function(fluorescence_df, threshold_val_column, Cq_val_col){
  #fluorescence_df contains: fluorescence values (each row is one sample), the System Calculated Threshold and the user Provided Threshold. The user must specify which column to use for the threshold calculation.
  # need to handle, if the user does not prove threshold values.

  # subset the data with only the fluorescence values
  flu_data_subset <- subset(fluorescence_df, select=c(grep("Cycle_Number", colnames(fluorescence_df))))

  cycle_number <- seq(ncol(flu_data_subset))

  #Number of samples
  number_of_rows <- nrow(fluorescence_df)
  for (i in 1:number_of_rows){
    # for each sample assess if it is unable to determine the threshold. if so, Cq value is 40. If there is a value, calculate the Cq.
    if (fluorescence_df[[threshold_val_column]][i] == "Unable to Determine Threshold")
    {fluorescence_df[[Cq_val_col]][i] <- 40}
    else{
      fluorescence_df[[Cq_val_col]][i] <- as.numeric(as.character(th.cyc(cycle_number,as.numeric(flu_data_subset[i, ]),r = round(as.numeric(fluorescence_df[[threshold_val_column]])[i], 3),linear = TRUE)[1]))
    }}

  fluorescence_df[[Cq_val_col]] <- as.numeric(fluorescence_df[[Cq_val_col]])
  return(fluorescence_df)}

############### Calculating the Copy Number ##################
calculate_copy_number <- function(standard_curve_flu, experimental_flu){
  regression_line <- lm(log(standardConc) ~ systemCalculatedCqValue, standard_curve_flu, na.action=na.exclude)
  testing_cq <- as.data.frame(experimental_flu$systemCalculatedCqValue)
  colnames(testing_cq) <- "systemCalculatedCqValue"
  predictions <- as.data.frame(predict(regression_line, testing_cq))
  colnames(predictions) <- "logDNACopyNumber"
  predictions$wellLocation <- experimental_flu$wellLocation
  predictions$rSquared <- rep(as.numeric(format(summary(regression_line)$r.squared, digits=3)),length(predictions$wellLocation))
  return(predictions)
}

############### Functions for processing the standard curve flu ##################

# For MIC
process_MIC_raw_data <- function(raw_fluorescence){
  raw_fluorescence <- as.data.frame(t(raw_fluorescence[, 2:ncol(raw_fluorescence)]))
  # add consistent column names
  #Changing column names to consistent with naming convention in database
  total_runs <- ncol(raw_fluorescence)
  colnames(raw_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))
  # add a column for merging with metadata
  raw_fluorescence$wellLocation <- as.numeric(gsub("[^0-9]", "", rownames(raw_fluorescence)))
  # move wellLocation to the front
  raw_fluorescence <- as.data.frame(raw_fluorescence[,c(ncol(raw_fluorescence),1:ncol(raw_fluorescence)-1)])
  print(paste0("number of wells: ", nrow(raw_fluorescence), ", number of cycles: ", ncol(raw_fluorescence)-1))
  return(raw_fluorescence)
}

# For Biomeme
process_biomeme_raw_data <- function(raw_fluorescence){
  #Creating dataframe with fluorescence values from biomem raw qPCR file
  end_row <-(which(grepl('Raw Fluorescence', raw_fluorescence$Run.Name))) - 2
  raw_fluorescence <- raw_fluorescence[11:end_row, 2:41]
  #Changing column names to consistent with naming convention in database
  total_runs <- ncol(raw_fluorescence)
  colnames(raw_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))
  #change all values to numeric
  cols = c(1:total_runs)
  raw_fluorescence[, cols] <- apply(raw_fluorescence[, cols], 2, function(x)  as.numeric(as.character(x)))
  # add wellLocation (key for merging)
  raw_fluorescence$wellLocation <- fluorescence_file[11:end_row, 1]
  raw_fluorescence <- as.data.frame(raw_fluorescence[,c(ncol(raw_fluorescence),1:ncol(raw_fluorescence)-1)])
  print(paste0("number of wells: ", nrow(raw_fluorescence), ", number of cycles: ", ncol(raw_fluorescence)-1))
  return (raw_fluorescence)
}

############### Functions for processing merged file ##################

merged_file_processing <- function(merged_file, dataset_name){
  # handling extreme cq values

  merged_file$systemCalculatedCqValue <- as.numeric(merged_file$systemCalculatedCqValue)
  merged_file$systemCalculatedCqValue[merged_file$systemCalculatedCqValue < 0] <- 40
  merged_file$systemCalculatedCqValue[merged_file$systemCalculatedCqValue > 40] <- 40

  merged_file$CqIntensitySystemCalculated <- cut(merged_file$systemCalculatedCqValue,
                                                 breaks = c(0, 10, 20, 30, 40, 1000),
                                                 right = FALSE,
                                                 labels = c("0 < Very strong < 10",
                                                            "10 <= Strong < 20",
                                                            "20 <= Moderate < 30",
                                                            "30 <= Weak < 40",
                                                            "None > 40"))
  #Changing wellLocation column class to character
  merged_file$wellLocation <- as.character(merged_file$wellLocation)

  # ensuring date values have a date format
  merged_file$projectCreationDate <- as.Date(as.character(merged_file$projectCreationDate), "%Y-%m-%d")
  merged_file$collectionDate <- as.Date(as.character(merged_file$collectionDate ), "%Y-%m-%d")
  merged_file$extractionDate <- as.Date(as.character(merged_file$extractionDate), "%Y-%m-%d")
  merged_file$runDate <- as.Date(as.character(merged_file$runDate), "%Y-%m-%d")
  merged_file$assayDate <- as.Date(as.character(merged_file$assayDate), "%Y-%m-%d")

  # adding dataset name
  merged_file$dataset_name <- dataset_name

  # remove one of the userprovided threshold columns and rename the other
  merged_file$userProvidedThresholdValue.x <- NULL
  colnames(merged_file)[colnames(merged_file) == 'userProvidedThresholdValue.y'] <- 'userProvidedThresholdValue'

  # adding
  merged_file$copyNumber <- merged_file$logDNACopyNumber
  merged_file$logDNACopyNumber <- NULL

  # create a dataframe that will add the cycle number until 70

  number_of_cycles = length(grep(x = colnames(merged_file), pattern = "Cycle_Number"))

  filler_df <- setNames(data.frame(matrix(nrow=nrow(merged_file), ncol=70-number_of_cycles)), c(paste0("Cycle_Number", (number_of_cycles+1):70)))

  # we can cbind this to our dataframe.
  merged_file <- cbind(merged_file, filler_df)

  # now we'll set the order of the values
  merged_file <- merged_file[ , c("dataset_name", "resultID", "runID", "assayID", "pcrChemistryID", "extractID", "wellLocation", "sampleName", "copyNumber", "control", "userProvidedThresholdValue", "CqvaluewithUserThreshold", "systemCalculatedThresholdValue","systemCalculatedCqValue", "userProvidedCqValue","CqIntensitySystemCalculated", "rSquared", "Cycle_Number1",	"Cycle_Number2", "Cycle_Number3",	"Cycle_Number4",	"Cycle_Number5", "Cycle_Number6",	"Cycle_Number7",	"Cycle_Number8", "Cycle_Number9",	"Cycle_Number10",	"Cycle_Number11","Cycle_Number12",	"Cycle_Number13",	"Cycle_Number14","Cycle_Number15",	"Cycle_Number16",	"Cycle_Number17","Cycle_Number18",	"Cycle_Number19",	"Cycle_Number20","Cycle_Number21",	"Cycle_Number22",	"Cycle_Number23","Cycle_Number24",	"Cycle_Number25",	"Cycle_Number26","Cycle_Number27",	"Cycle_Number28",	"Cycle_Number29", "Cycle_Number30",	"Cycle_Number31",	"Cycle_Number32","Cycle_Number33",	"Cycle_Number34",	"Cycle_Number35", "Cycle_Number36", "Cycle_Number37","Cycle_Number38","Cycle_Number39","Cycle_Number40","Cycle_Number41", "Cycle_Number42", "Cycle_Number43", "Cycle_Number44", "Cycle_Number45", "Cycle_Number46", "Cycle_Number47", "Cycle_Number48", "Cycle_Number49", "Cycle_Number50", "Cycle_Number51", "Cycle_Number52", "Cycle_Number53", "Cycle_Number54", "Cycle_Number55", "Cycle_Number56", "Cycle_Number57", "Cycle_Number58", "Cycle_Number59", "Cycle_Number60", "Cycle_Number61", "Cycle_Number62", "Cycle_Number63", "Cycle_Number64", "Cycle_Number65", "Cycle_Number66", "Cycle_Number67", "Cycle_Number68", "Cycle_Number69", "Cycle_Number70", "runRecordedBy","runDate", "runTime", "runPlatform", "machineID", "reactionConditions", "reactionVolume", "templateAmount", "forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration", "probeConcentration", "Mg2+Concentration", "polymeraseBatch", "polymeraseConcentrations", "thermocyclerParameters", "pcrDataNotes", "taxonID", "establishmentMeans", "assayName", "assayOwnership", "assayDescription", "assayCitation", "assayDate", "geneTarget", "geneSymbol", "dilutions", "replicates", "primerR", "primerF", "probe", "ampliconLength (bp)", "probeFluorescentTag", "dye(s)", "quencher", "probeModification", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus", "species", "vernacularName", "organismScope", "replicateID", "extractName", "analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes", "tubePlateID", "frozen", "fixed", "dnaStorageLocation", "extractMethodOfStorage", "dnaVolume", "quantificationMethod", "concentration(ng/ul)", "stationID", "collectorName", "replicateName", "collectionDate", "collectionTime", "storageID", "DateOfStorage", "methodOfStorage", "minimumElevationInMeters", "maximumElevationInMeters", "verbatimElevation", "minimumDepthInMeters", "maximumDepthInMeters", "verbatimDepth", "flowRate(m/s)", "filterType", "filtrationDuration(mins)", "volumeFiltered", "processLocation", "replicationNumber", "riparianVegetationPercentageCover", "dissolvedOxygen(mg/L)", "waterTemperature(C)", "pH", "TSS(mg/L)", "EC(uS/cm)", "turbidity(NTU)", "discharge", "tide", "chlorophyl", "salinity(ppt)", "contaminants(ng/g)", "traceMetals(mg/kg)", "organicContent(%)", "microbialActivity", "grainSize", "replicateDataNotes", "siteID", "stationName", "decimalLongitude", "decimalLatitude", "geographicRegionID", "locality", "estimatedPerimeter", "estimatedSurfaceArea(m2)", "siteType", "siteLength(m2)", "projectID", "continent", "country", "stateProvince", "municipality", "projectCreationDate","projectName", "projectRecordedBy", "projectOwner", "projectContactEmail", "projectDescription", "InstitutionID", "projectDataNotes", "standardCurveID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes")]

  return(merged_file)

}
