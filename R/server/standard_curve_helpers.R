#Function calculates threshold value for each individual well on a qPCR plate based on raw absorbance values. The threshold function is based on the second derivative method to calculate the threshold value for qPCR data.
calculate_threshold_value <- function(fluorescence_values) {

  fluorescence_values <- as.data.frame(t(fluorescence_values))

  #New table with threshold data
  thresholdData <- as.data.frame(matrix(nrow = ncol(fluorescence_values) , ncol = 1))
  colnames(thresholdData) <- c("computedThresholdValue")

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
      if ( (difference / absorb_range) >= 0.01  ) {

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
      thresholdData$computedThresholdValue[runSample] <- threshold_value

    }

    #If no threshold value can be computed
    else {thresholdData$computedThresholdValue[runSample] <- "Unable to Determine Threshold" }
  }

  return(thresholdData)
}
#Function is used to calculated Cq value from a set of fluorescence data
add_CqValue <- function(flur_file, meta_file) {

  #remove one to exclude CqValue column
  cycle_number <- seq(ncol(flur_file) - 1)

  #Number of samples
  number_of_rows <- nrow(flur_file)

  for (i in 1:number_of_rows) {

    if (meta_file$userProvidedThresholdValue[i] == "Unable to Determine Threshold")
    {flur_file$userProvidedCqValue[i] <- 40}

    else{

      flur_file$userProvidedCqValue[i] <-
        th.cyc(
          cycle_number,
          as.numeric(flur_file[i, -c(ncol(flur_file))]),
          r = round(as.numeric(
            meta_file$userProvidedThresholdValue
          )[i], 3),
          linear = TRUE
        )[1]


    }}

  return(flur_file)

}

format_standardCurve_metadata <- function (standardCurve_metadata) {
  standardCurve_Table <- as.data.frame(standardCurve_metadata[, c("SCresultID",
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
#Return popup message regarding uploaded standard curve fluorescence file.
control_records_to_remove <- function(meta_data) {
  if (length(which(grepl("Y", toupper(meta_data$control)))) != 0)
  {return (meta_data[-c(which(grepl("Y", toupper(meta_data$control)))), ])}

  else
  {return(meta_data)}
}

process_MIC_uploaded_file <- function(fluorescence_file) {

  #Transpose fluorescence dataframe
  qpcr_MIC_fluorescence <- as.data.frame(t(fluorescence_file))

  #Change column names in fluorescence dataframe
  total_runs <- ncol(qpcr_MIC_fluorescence)
  colnames(qpcr_MIC_fluorescence) <-
    c(paste0("Cycle_Number", 1:total_runs))

  #Remove first row with cycle numbers
  qpcr_MIC_fluorescence <- qpcr_MIC_fluorescence[-c(1),]


  #change all values to numeric
  cols = c(1:total_runs)
  qpcr_MIC_fluorescence[, cols] <-
    apply(qpcr_MIC_fluorescence[, cols], 2, function(x)
      as.numeric(as.character(x)))
  str(qpcr_MIC_fluorescence)

  return(qpcr_MIC_fluorescence)

}

process_biomeme23_uploaded_file <- function(fluorescence_file) {

  #Creating dataframe with fluorescence values and ct intensity value from biomem raw qPCR file
  end_row <-(which(grepl('Raw Fluorescence', fluorescence_file$Run.Name))) - 2

  qpcr_biomem23_fluorescence <- fluorescence_file[11:end_row, 2:41]


  #Changing column names to consistent with naming covention in database
  total_runs <- ncol(qpcr_biomem23_fluorescence)
  colnames(qpcr_biomem23_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))


  #change all values to numeric
  cols = c(1:total_runs)
  qpcr_biomem23_fluorescence[, cols] <- apply(qpcr_biomem23_fluorescence[, cols], 2, function(x)  as.numeric(as.character(x)))

  return (qpcr_biomem23_fluorescence)
}

merge_standardCurve_metadata_fluorescence_file <- function(fluorescence_File, metadata) {

  #Remove null records (controls or empty runs) and associated fluorescence
  fluorescence_File <- null_records_to_remove_flur(metadata, fluorescence_File)
  metadata <- null_records_to_remove_meta(metadata)


  #Add CqValue row to dataframe
  user_Cq <- function (flur, meta) {

    if (any(is.na(meta$userProvidedCqValue)) == TRUE)

    {flur$userProvidedCqValue <- ""
    flur <- add_CqValue(flur, meta)

    return(flur)}

    else {
      flur$userProvidedCqValue <- meta$userProvidedCqValue
      # flur <- add_CqValue(flur, meta)

      return(flur)
    }
  }


  fluorescence_File <- user_Cq(fluorescence_File, metadata)

  #Handeling extreme values
  fluorescence_File$userProvidedCqValue <- as.numeric(fluorescence_File$userProvidedCqValue)
  fluorescence_File$userProvidedCqValue[fluorescence_File$userProvidedCqValue < 0] <-40
  fluorescence_File$userProvidedCqValue[fluorescence_File$userProvidedCqValue > 40] <- 40


  #Making dataframe with empty fluorescence data
  total_runs <- ncol(fluorescence_File) - 1  #substract one for Cq value column
  flur_col <- as.data.frame(matrix(NA, nrow =  nrow(fluorescence_File),ncol = (70 - total_runs)))
  colnames(flur_col) <- c(paste0("Cycle_Number", c(((total_runs + 1):70))))


  #System calculated threshold and CqValue
  #Calculate threshold
  fluorescence_data_for_threshold <- fluorescence_File[1:(ncol(fluorescence_File) - 1)]
  systemCalculatedThreshold <- calculate_threshold_value(fluorescence_data_for_threshold)
  colnames(systemCalculatedThreshold) <-"userProvidedThresholdValue"

  #Calculated Cq value with calulcated threshold
  calculated_system_cq <- add_CqValue(fluorescence_File, systemCalculatedThreshold)
  names(calculated_system_cq)[ncol(calculated_system_cq)] <- "systemCalculatedCqValue"
  colnames(systemCalculatedThreshold) <-"systemCalculatedThresholdValue"

  #Ensure threshold value and cq value are numeric
  calculated_system_cq$systemCalculatedCqValue <- as.numeric(calculated_system_cq$systemCalculatedCqValue)

  #Change class to date
  metadata$runDate <- as.Date(as.character(metadata$runDate), "%Y-%m-%d")
  metadata$SCdate <- as.Date(as.character(metadata$SCdate), "%Y-%m-%d")


  #Mreging fluorescence and metadata files
  merged_file <-
    cbind(
      metadata[1:10],
      fluorescence_File[ncol(fluorescence_File)],
      systemCalculatedThreshold[1],
      calculated_system_cq[ncol(calculated_system_cq)],
      fluorescence_File[1:(ncol(fluorescence_File) - 1)],
      flur_col,
      metadata[12:36]
    )


  #Changing wellLocation column class to character
  merged_file$wellLocation <- as.character(merged_file$wellLocation)

  #Ensure columns are numeric
  merged_file$userProvidedThresholdValue <- as.numeric(merged_file$userProvidedThresholdValue)
  merged_file$userProvidedCqValue <- as.numeric(merged_file$userProvidedCqValue)
  merged_file$standardConc <- as.numeric(merged_file$standardConc)

  #Return merged dataframe
  return(merged_file)

}

null_records_to_remove_flur <- function(meta_data, fluor_file) {
  if (length(which(grepl("null", tolower(meta_data$sampleName)))) != 0)
  {return (fluor_file[-c(which(grepl("null", tolower(meta_data$sampleName)))), ])}

  else
  {return(fluor_file)}

}


#Function removes null records (controls or empty runs) from metadata file if sample name contains 'null'.
null_records_to_remove_meta <- function(meta_data) {
  if (length(which(grepl("null", tolower(meta_data$sampleName)))) != 0)
  {return (meta_data[-c(which(grepl("null", tolower(meta_data$sampleName)))), ])}

  else
  {return(meta_data)}

}


#Function to add new rows and column names to stepOneplus dataframe
reformat_SOP_Flur_data <- function (flur_data) {
  flur_df <- as.data.frame(matrix(nrow = 0, ncol = 40))
  colnames(flur_df) <-  c(paste0("Cycle_Number", 1:40))

  for (i in seq(from = 1, to = 3840, by = 40))
  {
    start <- i
    end <- i + 39

    one_row <- flur_data[1, start:end]
    colnames(one_row) <- c(paste0("Cycle_Number", 1:40))

    flur_df  <-  rbind(flur_df, one_row)

  }
  return(flur_df)
}


#Function to format raw StepOnePlus fluorescence file in matrix where each row
#represent a plate well and each column has the fluorescence value for a cycle.
process_SOP_uploaded_file <- function(fluorescence_file) {

  #Remove rows above main table
  fluorescence_file <- fluorescence_file[-c(1:6), ]

  #Extract first row with column names
  colnames(fluorescence_file) <- c(fluorescence_file[1,])

  #Remove first row of table since it has column names in it
  fluorescence_file <- fluorescence_file[-c(1),]

  #Only keep first 3 columns (application can only deal with one dye currently)
  fluorescence_file <- fluorescence_file[, 1:3]

  #Changing Cycle class to numeric
  fluorescence_file$Cycle <- as.numeric(fluorescence_file$Cycle)

  #Sort well names then cycles
  fluorescence_file <- fluorescence_file[order(fluorescence_file$Well, fluorescence_file$Cycle),]

  #Creating empty data frame to populate
  run_location <- as.data.frame(matrix(nrow = 96, ncol = 1))

  #Populating Well name column with unique well names (96 names)
  run_location[, 1] <- unique(fluorescence_file$Well)
  colnames(run_location) <- c("run_location")


  #Getting fluorescence values

  #Turning StepOnePlus raw fluorescence data in to 1 by 3840 matrix with just fluorescence data
  stepOnePlus_fluorescence_matrix <-  as.data.frame(t(fluorescence_file[,-c(1:2)]))

  #Creating matrix, each row is a sample with the fluorescene values for each cycle
  SOP_flur <- reformat_SOP_Flur_data(stepOnePlus_fluorescence_matrix)

  return(SOP_flur)

}


#Function to format raw StepOnePlus fluorescence file in matrix where each row
#represent a plate well and each column has the fluorescence value for a cycle, with well names.
process_SOP_uploaded_file_with_well_names <- function(fluorescence_file) {

  #Remove rows above main table
  fluorescence_file <- fluorescence_file[-c(1:6), ]

  #Extract first row with column names
  colnames(fluorescence_file) <- c(fluorescence_file[1,])

  #Remove first row of table since it has column names in it
  fluorescence_file <- fluorescence_file[-c(1),]

  #Only keep first 3 columns (application can only deal with one dye currently)
  fluorescence_file <- fluorescence_file[, 1:3]

  #Changing Cycle class to numeric
  fluorescence_file$Cycle <- as.numeric(fluorescence_file$Cycle)

  #Sort well names then cycles
  fluorescence_file <- fluorescence_file[order(fluorescence_file$Well, fluorescence_file$Cycle),]

  #Creating empty data frame to populate
  run_location <- as.data.frame(matrix(nrow = 96, ncol = 1))

  #Populating Well name column with unique well names (96 names)
  run_location[, 1] <- unique(fluorescence_file$Well)
  colnames(run_location) <- c("run_location")


  #Getting fluorescence values

  #Turning StepOnePlus raw fluorescence data in to 1 by 3840 matrix with just fluorescence data
  stepOnePlus_fluorescence_matrix <-  as.data.frame(t(fluorescence_file[,-c(1:2)]))

  #Creating matrix, each row is a sample with the fluorescene values for each cycle
  SOP_flur <- reformat_SOP_Flur_data(stepOnePlus_fluorescence_matrix)

  SOP_flur$wellLocation <-  run_location$run_location

  SOP_flur <- SOP_flur %>% select(41, 1:40)

  return(SOP_flur)

}
#Function to add new rows and column names to stepOneplus dataframe
reformat_SOP_Flur_data <- function (flur_data) {
  flur_df <- as.data.frame(matrix(nrow = 0, ncol = 40))
  colnames(flur_df) <-  c(paste0("Cycle_Number", 1:40))

  for (i in seq(from = 1, to = 3840, by = 40))
  {
    start <- i
    end <- i + 39

    one_row <- flur_data[1, start:end]
    colnames(one_row) <- c(paste0("Cycle_Number", 1:40))

    flur_df  <-  rbind(flur_df, one_row)

  }
  return(flur_df)
}


#Function to format raw StepOnePlus fluorescence file in matrix where each row
#represent a plate well and each column has the fluorescence value for a cycle.
process_SOP_uploaded_file <- function(fluorescence_file) {

  #Remove rows above main table
  fluorescence_file <- fluorescence_file[-c(1:6), ]

  #Extract first row with column names
  colnames(fluorescence_file) <- c(fluorescence_file[1,])

  #Remove first row of table since it has column names in it
  fluorescence_file <- fluorescence_file[-c(1),]

  #Only keep first 3 columns (application can only deal with one dye currently)
  fluorescence_file <- fluorescence_file[, 1:3]

  #Changing Cycle class to numeric
  fluorescence_file$Cycle <- as.numeric(fluorescence_file$Cycle)

  #Sort well names then cycles
  fluorescence_file <- fluorescence_file[order(fluorescence_file$Well, fluorescence_file$Cycle),]

  #Creating empty data frame to populate
  run_location <- as.data.frame(matrix(nrow = 96, ncol = 1))

  #Populating Well name column with unique well names (96 names)
  run_location[, 1] <- unique(fluorescence_file$Well)
  colnames(run_location) <- c("run_location")


  #Getting fluorescence values

  #Turning StepOnePlus raw fluorescence data in to 1 by 3840 matrix with just fluorescence data
  stepOnePlus_fluorescence_matrix <-  as.data.frame(t(fluorescence_file[,-c(1:2)]))

  #Creating matrix, each row is a sample with the fluorescene values for each cycle
  SOP_flur <- reformat_SOP_Flur_data(stepOnePlus_fluorescence_matrix)

  return(SOP_flur)

}


#Function to format raw StepOnePlus fluorescence file in matrix where each row
#represent a plate well and each column has the fluorescence value for a cycle, with well names.
process_SOP_uploaded_file_with_well_names <- function(fluorescence_file) {

  #Remove rows above main table
  fluorescence_file <- fluorescence_file[-c(1:6), ]

  #Extract first row with column names
  colnames(fluorescence_file) <- c(fluorescence_file[1,])

  #Remove first row of table since it has column names in it
  fluorescence_file <- fluorescence_file[-c(1),]

  #Only keep first 3 columns (application can only deal with one dye currently)
  fluorescence_file <- fluorescence_file[, 1:3]

  #Changing Cycle class to numeric
  fluorescence_file$Cycle <- as.numeric(fluorescence_file$Cycle)

  #Sort well names then cycles
  fluorescence_file <- fluorescence_file[order(fluorescence_file$Well, fluorescence_file$Cycle),]

  #Creating empty data frame to populate
  run_location <- as.data.frame(matrix(nrow = 96, ncol = 1))

  #Populating Well name column with unique well names (96 names)
  run_location[, 1] <- unique(fluorescence_file$Well)
  colnames(run_location) <- c("run_location")


  #Getting fluorescence values

  #Turning StepOnePlus raw fluorescence data in to 1 by 3840 matrix with just fluorescence data
  stepOnePlus_fluorescence_matrix <-  as.data.frame(t(fluorescence_file[,-c(1:2)]))

  #Creating matrix, each row is a sample with the fluorescene values for each cycle
  SOP_flur <- reformat_SOP_Flur_data(stepOnePlus_fluorescence_matrix)

  SOP_flur$wellLocation <-  run_location$run_location

  SOP_flur <- SOP_flur %>% select(41, 1:40)

  return(SOP_flur)

}


#Function to format raw Biomeme two3/Franklin fluorescence file in matrix where each row
# represent a plate well and each column has the fluorescence value for a cycle with well names.
process_biomeme23_uploaded_file_with_well_names <- function(fluorescence_file) {

  #Creating dataframe with fluorescence values from biomem raw qPCR file
  end_row <-(which(grepl('Raw Fluorescence', fluorescence_file$Run.Name))) - 2
  qpcr_biomem23_fluorescence <- fluorescence_file[11:end_row, 2:41]

  #Changing column names to consistent with naming covention in database
  total_runs <- ncol(qpcr_biomem23_fluorescence)
  colnames(qpcr_biomem23_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))


  #change all values to numeric
  cols = c(1:total_runs)
  qpcr_biomem23_fluorescence[, cols] <- apply(qpcr_biomem23_fluorescence[, cols], 2, function(x)  as.numeric(as.character(x)))

  qpcr_biomem23_fluorescence$wellLocation <- fluorescence_file[11:end_row, 1]

  qpcr_biomem23_fluorescence <- qpcr_biomem23_fluorescence %>% select(41, 1:40)

  return (qpcr_biomem23_fluorescence)
}


#Function to format raw StepOnePlus fluorescence file in matrix where each row
#represent a plate well and each column has the fluorescence value for a cycle, with well names.
process_SOP_uploaded_file_with_well_names <- function(fluorescence_file) {

  #Remove rows above main table
  fluorescence_file <- fluorescence_file[-c(1:6), ]

  #Extract first row with column names
  colnames(fluorescence_file) <- c(fluorescence_file[1,])

  #Remove first row of table since it has column names in it
  fluorescence_file <- fluorescence_file[-c(1),]

  #Only keep first 3 columns (application can only deal with one dye currently)
  fluorescence_file <- fluorescence_file[, 1:3]

  #Changing Cycle class to numeric
  fluorescence_file$Cycle <- as.numeric(fluorescence_file$Cycle)

  #Sort well names then cycles
  fluorescence_file <- fluorescence_file[order(fluorescence_file$Well, fluorescence_file$Cycle),]

  #Creating empty data frame to populate
  run_location <- as.data.frame(matrix(nrow = 96, ncol = 1))

  #Populating Well name column with unique well names (96 names)
  run_location[, 1] <- unique(fluorescence_file$Well)
  colnames(run_location) <- c("run_location")


  #Getting fluorescence values

  #Turning StepOnePlus raw fluorescence data in to 1 by 3840 matrix with just fluorescence data
  stepOnePlus_fluorescence_matrix <-  as.data.frame(t(fluorescence_file[,-c(1:2)]))

  #Creating matrix, each row is a sample with the fluorescene values for each cycle
  SOP_flur <- reformat_SOP_Flur_data(stepOnePlus_fluorescence_matrix)

  SOP_flur$wellLocation <-  run_location$run_location

  SOP_flur <- SOP_flur %>% select(41, 1:40)

  return(SOP_flur)

}

# Function that calculates LOD and LOQ for the standard curve data using the generic method
calculate_SC_LOD_LOQ <- function(merged_file, LOQthreshold){
  LOD.Data <- merged_file[, c("standardCurveName", "runRecordedBy", "systemCalculatedCqValue", "standardConc")]
  # rename the columns to match the calculation
  colnames(LOD.Data) <- c("Target", "Lab", "Cq", "SQ")
  
  # now set a condition that will change all Cq of 40s to NA
  LOD.Data$Cq[which(LOD.Data$Cq==40)] <- NA
  
  # Step 2: Determine the dilutions used in the standard curve for the target
  Standards <- unique(LOD.Data$SQ[!is.na(LOD.Data$SQ)])
  
  # Step 3: Calculate the rate of detection for each standard (any well with a Cq value is considered a detection)
  # Calculate other metrics for LOQ calculations (Cq mean, standard deviation, coefficient of variation)
  DAT2 <- data.frame(Standards=Standards,Target=LOD.Data$Target[1],Reps=NA,Detects=NA,Cq.mean=NA,
                     Cq.sd=NA,Cq.CV=NA)
  
  for(i in 1:nrow(DAT2)) {
    DAT2$Reps[i] <- sum(LOD.Data$SQ==DAT2$Standards[i],na.rm=TRUE)
    DAT2$Detects[i] <- sum(!is.na(LOD.Data$Cq)&LOD.Data$SQ==DAT2$Standards[i],na.rm=TRUE)
    DAT2$DetectionRate[i] <- DAT2$Detects[i]/DAT2$Reps[i]
    DAT2$Cq.mean[i] <- mean(LOD.Data$Cq[LOD.Data$SQ==DAT2$Standards[i]],na.rm=TRUE)
    DAT2$Cq.sd[i] <- sd(LOD.Data$Cq[LOD.Data$SQ==DAT2$Standards[i]],na.rm=TRUE)
    DAT2$Cq.CV[i] <- sqrt(2^(DAT2$Cq.sd[i]^2*log(2))-1)
  }
  
  # Step 4: Determine the lowest concentration with at least a 95% detection rate
  # we want to assess whether a concentration above the LOD has greater variation
  
  
  A <- min(DAT2$Standards[DAT2$DetectionRate>=0.95])
  LOD <- A
  if(length(which(DAT2$DetectionRate<0.95))>0) {
    B <- max(DAT2$Standards[DAT2$DetectionRate<0.95])
    if(B>A) {
      warning <- paste0("WARNING: For ",LOD.Data$Target[1],", ",B," copies/reaction standard detected at lower rate than ",A," copies/reaction standard.Please retest.")
    }
  }
  if(length(which(DAT2$DetectionRate<0.95))==0) {
    warning <- paste0("WARNING: LoD cannot be determined for ",LOD.Data$Target[1],", because it is lower than the lowest standard you tested.Report as <",A," copies/reaction, or retest with lower concentrations.")
  }
  
  
  # Step 5: Find the LOQ value (lowest standard concentration that can be quantified with a CV value below the defined threshold). Default CV value will be 35%.
  threshold <- as.numeric(LOQthreshold)
  LOQ <- min(DAT2$Standards[DAT2$Cq.CV<threshold], na.rm = T)
  
  merged_file$systemCalculatedLOD <- LOD
  merged_file$LODWarning <- warning
  merged_file$systemCalculatedLOQ <- LOQ
  
  return(merged_file)
}
