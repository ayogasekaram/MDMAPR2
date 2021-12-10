  #Function takes in user uploaded data and parses it into Project table.
  create_project_table <- function(uploaded_data) {
    project_table <-
      distinct(uploaded_data[, c(
        "projectID",
        "projectCreationDate",
        "projectName",
        "projectRecordedBy",
        "projectOwner",
        "projectContactEmail",
        "projectDescription",
        "InstitutionID",
        "projectDataNotes"
      )])

    return(project_table)
  }



  #Function takes in user uploaded data and parses it into Geographic Region table.
  create_geographicRegion_Table <- function(uploaded_data) {
    geographicRegion_table <-
      distinct(uploaded_data[, c(
        "geographicRegionID",
        "projectID",
        "continent",
        "country",
        "stateProvince",
        "municipality"
      )])

    return(geographicRegion_table)
  }


  #Function takes in user uploaded data and parses it into Site table.
  create_site_Table <- function(uploaded_data) {
    site_table <-
      distinct(uploaded_data[, c(
        "siteID",
        "geographicRegionID",
        "locality",
        "estimatedPerimeter",
        "estimatedSurfaceArea(m2)",
        "siteType",
        "siteLength(m2)"
      )])

    return(site_table)
  }



  #Function takes in user uploaded data and parses it into Station table.
  create_station_Table <- function(uploaded_data) {
    station_Table <-
      distinct(uploaded_data[, c("stationID",
                                 "siteID",
                                 "stationName",
                                 "decimalLongitude",
                                 "decimalLatitude")])

    return(station_Table)
  }



  #Function takes in user uploaded data and parses it into Replicate table.
  create_replicate_Table <- function(uploaded_data) {
    replicate_Table <-
      distinct(uploaded_data[, c(
        "replicateID",
        "stationID",
        "collectorName",
        "replicateName",
        "collectionDate",
        "collectionTime",
        "storageID",
        "DateOfStorage",
        "methodOfStorage",
        "minimumElevationInMeters",
        "maximumElevationInMeters",
        "verbatimElevation",
        "minimumDepthInMeters",
        "maximumDepthInMeters",
        "verbatimDepth",
        "flowRate(m/s)",
        "filterType",
        "filtrationDuration(mins)",
        "volumeFiltered",
        "processLocation",
        "replicationNumber",
        "riparianVegetationPercentageCover",
        "dissolvedOxygen(mg/L)",
        "waterTemperature(C)",
        "pH",
        "TSS(mg/L)",
        "EC(uS/cm)",
        "turbidity(NTU)",
        "discharge",
        "tide",
        "chlorophyl",
        "salinity(ppt)",
        "contaminants(ng/g)",
        "traceMetals(mg/kg)",
        "organicContent(%)",
        "microbialActivity",
        "grainSize",
        "replicateDataNotes"
      )])

    return(replicate_Table)
  }



  #Function takes in user uploaded data and parses it into Extract table.
  create_extract_Table <- function(uploaded_data) {
    extract_Table <-
      distinct(uploaded_data[, c(
        "extractID",
        "replicateID",
        "extractName",
        "analyst",
        "extractionDate",
        "extractionTime",
        "location",
        "extractionMethod",
        "methodCitation",
        "extractionNotes",
        "tubePlateID",
        "frozen",
        "fixed",
        "dnaStorageLocation",
        "extractMethodOfStorage",
        "dnaVolume",
        "quantificationMethod",
        "concentration(ng/ul)"
      )])

    return(extract_Table)
  }


  ##Function takes in user uploaded data and parses it into Result table.
  create_results_Table <- function(uploaded_data) {
    results_Table <-
      uploaded_data[, c(
        "resultID",
        "runID",
        "assayID",
        "pcrChemistryID",
        "extractID",
        "wellLocation",
        "sampleName",
        "copyNumber",
        "control",
        "userProvidedThresholdValue",
        "userProvidedCqValue",
        "systemCalculatedThresholdValue",
        "systemCalculatedCqValue",
        "Cycle_Number1",
        "Cycle_Number2",
        "Cycle_Number3",
        "Cycle_Number4",
        "Cycle_Number5",
        "Cycle_Number6",
        "Cycle_Number7",
        "Cycle_Number8",
        "Cycle_Number9",
        "Cycle_Number10",
        "Cycle_Number11",
        "Cycle_Number12",
        "Cycle_Number13",
        "Cycle_Number14",
        "Cycle_Number15",
        "Cycle_Number16",
        "Cycle_Number17",
        "Cycle_Number18",
        "Cycle_Number19",
        "Cycle_Number20",
        "Cycle_Number21" ,
        "Cycle_Number22",
        "Cycle_Number23",
        "Cycle_Number24",
        "Cycle_Number25",
        "Cycle_Number26",
        "Cycle_Number27",
        "Cycle_Number28",
        "Cycle_Number29",
        "Cycle_Number30",
        "Cycle_Number31",
        "Cycle_Number32",
        "Cycle_Number33",
        "Cycle_Number34",
        "Cycle_Number35",
        "Cycle_Number36",
        "Cycle_Number37",
        "Cycle_Number38",
        "Cycle_Number39",
        "Cycle_Number40",
        "Cycle_Number41",
        "Cycle_Number42",
        "Cycle_Number43",
        "Cycle_Number44",
        "Cycle_Number45",
        "Cycle_Number46",
        "Cycle_Number47",
        "Cycle_Number48",
        "Cycle_Number49",
        "Cycle_Number50",
        "Cycle_Number51",
        "Cycle_Number52",
        "Cycle_Number53",
        "Cycle_Number54",
        "Cycle_Number55",
        "Cycle_Number56",
        "Cycle_Number57",
        "Cycle_Number58",
        "Cycle_Number59",
        "Cycle_Number60",
        "Cycle_Number61",
        "Cycle_Number62",
        "Cycle_Number63",
        "Cycle_Number64",
        "Cycle_Number65",
        "Cycle_Number66",
        "Cycle_Number67",
        "Cycle_Number68",
        "Cycle_Number69",
        "Cycle_Number70"
      )]

    return(results_Table)
  }



  #Function takes in user uploaded data and parses it into Assay table.
  create_assay_Table <- function(uploaded_data) {
    assay_table <-
      distinct(uploaded_data[, c(
        "assayID",
        "taxonID",
        "establishmentMeans",
        "assayName",
        "assayOwnership",
        "assayDescription",
        "assayCitation",
        "assayDate",
        "geneTarget",
        "geneSymbol",
        "dilutions",
        "replicates",
        "primerR",
        "primerF",
        "probe",
        "ampliconLength (bp)",
        "probeFluorescentTag",
        "dye(s)",
        "quencher",
        "probeModification"
      )])

    return(assay_table)

  }



  #Function takes in user uploaded data and parses it into Taxon table.
  create_taxon_Table <- function(uploaded_data) {
    taxon_table <-
      distinct(uploaded_data[, c(
        "taxonID",
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "subgenus",
        "species",
        "vernacularName",
        "organismScope"
      )])

    return(taxon_table)

  }



  ##Function takes in user uploaded data and parses it into PCR Chemsitry table.
  create_pcrChemistry_Table <- function(uploaded_data, SC_uploaded_data) {

    pcrChemistry_table <-
      distinct(uploaded_data[, c(
        "pcrChemistryID",
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
        "pcrDataNotes"
      )])


    SCpcrChemistry_table <-
      distinct(SC_uploaded_data[, c(
        "pcrChemistryID",
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
        "pcrDataNotes"
      )])


    pcrChemistry_table <-
      rbind(pcrChemistry_table, SCpcrChemistry_table)

    return(pcrChemistry_table)

  }



  #Function takes in user uploaded data and parses it into Standard Curve table.
  create_standardCurve_Table <- function(uploaded_data) {
    standardCurve_Table <-
      distinct(uploaded_data[, c(
        "standardCurveID",
        "assayID",
        "standardCurveName",
        "SCdate",
        "SCrecordedBy",
        "SCdataNotes",
        "LOD",
        "LOQ"
      )])

    return(standardCurve_Table)

  }



  #Function takes in user uploaded data and parses it into Standard Curve Result table.
  create_standardCurveResults_Table <- function(uploaded_data) {
    standardCurveResults_Table <-
      uploaded_data[, c(
        "SCresultID",
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
        "systemCalculatedThresholdValue",
        "systemCalculatedCqValue",
        "Cycle_Number1",
        "Cycle_Number2",
        "Cycle_Number3",
        "Cycle_Number4",
        "Cycle_Number5",
        "Cycle_Number6",
        "Cycle_Number7",
        "Cycle_Number8",
        "Cycle_Number9",
        "Cycle_Number10",
        "Cycle_Number11",
        "Cycle_Number12",
        "Cycle_Number13",
        "Cycle_Number14",
        "Cycle_Number15",
        "Cycle_Number16",
        "Cycle_Number17",
        "Cycle_Number18",
        "Cycle_Number19",
        "Cycle_Number20",
        "Cycle_Number21",
        "Cycle_Number22",
        "Cycle_Number23",
        "Cycle_Number24",
        "Cycle_Number25",
        "Cycle_Number26",
        "Cycle_Number27",
        "Cycle_Number28",
        "Cycle_Number29",
        "Cycle_Number30",
        "Cycle_Number31",
        "Cycle_Number32",
        "Cycle_Number33",
        "Cycle_Number34",
        "Cycle_Number35",
        "Cycle_Number36",
        "Cycle_Number37",
        "Cycle_Number38",
        "Cycle_Number39",
        "Cycle_Number40",
        "Cycle_Number41",
        "Cycle_Number42",
        "Cycle_Number43",
        "Cycle_Number44",
        "Cycle_Number45",
        "Cycle_Number46",
        "Cycle_Number47",
        "Cycle_Number48",
        "Cycle_Number49",
        "Cycle_Number50",
        "Cycle_Number51",
        "Cycle_Number52",
        "Cycle_Number53",
        "Cycle_Number54",
        "Cycle_Number55",
        "Cycle_Number56",
        "Cycle_Number57",
        "Cycle_Number58",
        "Cycle_Number59",
        "Cycle_Number60",
        "Cycle_Number61",
        "Cycle_Number62",
        "Cycle_Number63",
        "Cycle_Number64",
        "Cycle_Number65",
        "Cycle_Number66",
        "Cycle_Number67",
        "Cycle_Number68" ,
        "Cycle_Number69",
        "Cycle_Number70"
      )]

    return(standardCurveResults_Table)

  }



  #Function takes in user uploaded data and parses it into Run Information table.
  create_runInformation_Table <-
    function(uploaded_data, SC_uploaded_data) {
      runInformation_Table <-
        distinct(uploaded_data[, c("runID",
                                   "runRecordedBy",
                                   "runDate",
                                   "runTime",
                                   "runPlatform",
                                   "machineID")])

      runInformation_Table2 <-
        distinct(SC_uploaded_data[, c("runID",
                                      "runRecordedBy",
                                      "runDate",
                                      "runTime",
                                      "runPlatform",
                                      "machineID")])


      runInformation_Table <-
        rbind(runInformation_Table, runInformation_Table2)

      return(runInformation_Table)

    }


