#################### File Validation Functions for MDMAPR ##########

#Function that is opposite on 'is in' function
'%ni%' <- Negate('%in%')


########### These validations happen as the user uploads the file#####
#Function to give pop-up validation messages for uploaded fluorescence file., is it the correct file type?
fluorescence_file_validation_msgs <- function(flur_file) {

  if (file_ext(flur_file$datapath) %ni% c("rdml"))
    {shinyjs::alert("ERROR: Fluorescence file is not RDML.")}
  #
  #     else {shinyjs::alert("Successful file upload.")}
  }

#Function to give pop-up validation messages for uploaded metadata file., is format of the metadata good (only applies to the sheets related to the fluorescence file)
metadata_file_validation_msgs <- function(meta_file){

  if (file_ext(meta_file$datapath) %ni% c("xlsx", "xls"))
  {shinyjs::alert("ERROR: Metadata file is not an accepted file type.")}

  #Check how many sheets are in uploaded metadata file
  else if (length(excel_sheets(meta_file$datapath)) < 5)
  {shinyjs::alert("ERROR: Incorrect Metadata template used.")}


  #Checks to see if any of the sheets are empty (i.e: only contain field names)
  else if (nrow(read_excel(meta_file$datapath, sheet = 1)) == 0 |
           nrow(read_excel(meta_file$datapath, sheet = 2)) == 0 |
           nrow(read_excel(meta_file$datapath, sheet = 3)) == 0 |
           nrow(read_excel(meta_file$datapath, sheet = 4)) == 0 |
           nrow(read_excel(meta_file$datapath, sheet = 5)) == 0)
  {shinyjs::alert("ERROR: One or more required sheets on Metadata file is empty.")}


  #Check is metadata parsing function works on file
  else if (is.error(format_qPCR_metadata(meta_file$datapath)) == TRUE)
  {shinyjs::alert("ERROR: Metadata file is missing one or more necessary columns.")}


  # else {shinyjs::alert("Successful file upload.")}
}

# platform selection only applies if they are uploading standard curve file. Execute this function if the standard curve file is uploaded.
selected_platform_validation_msgs <- function(standard_curve, meta_file, platform){

  #If files are MIC
  if (platform == "MIC")

  {if (file_ext(standard_curve$datapath) != "csv")
  {shinyjs::alert("ERROR: MIC fluorescence file must be csv.")}


    #check if fluorescence file if biomeme file
    else if (read.csv(standard_curve$datapath)[1, 1] == 'Date & Time')
    {shinyjs::alert("ERROR: Fluorescence file is not correct file type for MIC platform.")}


    #Check is fluorescence file processing  function work
    else if (is.error(process_MIC_raw_data(read.csv(standard_curve$datapath))) == TRUE)
    {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for MIC platform."))}


    #Check that formatted fluorescence file and formatted metadata file have the same number of rows
    else if (nrow(process_MIC_raw_data(read.csv(standard_curve$datapath))) != nrow(format_std_curve_metadata(meta_file$datapath)))
    {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}

    else {shinyjs::alert("Successful platform selected.")}}



  else if (platform == "StepOnePlus")

  {if (file_ext(standard_curve$datapath) %ni% c("xlsx", "xls"))
  {shinyjs::alert("ERROR: Step One Plus fluorescence file must be xlsx/xls.")}

    #Check is fluorescence file processingfunction work
    else if (is.error(process_SOP_uploaded_file(read_excel(standard_curve$datapath, 4))) == TRUE)
    {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for Step One Plus platform."))}

    #Check that formatted fluorescence file and formatted metadata file have the same number of rows
    else if (nrow(process_SOP_uploaded_file(read_excel(standard_curve$datapath, sheet = 4))) != nrow(format_std_curve_metadata(meta_file$datapath)))
    {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}

    else {shinyjs::alert("Successful platform selected.")}}



  #If files are Biomeme two3/Franklin
  else if (platform == "Biomeme two3/Franklin")

  {if (file_ext(standard_curve$datapath) != "csv")
  {shinyjs::alert("ERROR: Biomeme two3/Franklin fluorescence file must be csv.")}


    #Check is fluorescence file processing  function work
    else if (is.error(process_biomeme_raw_data(read.csv(standard_curve$datapath))) == TRUE)
    {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for Biomeme two3/Franklin platform."))}


    #Check that formatted fluorescence file and formatted metadata file have the same number of rows
    else if (nrow(process_biomeme_raw_data(read.csv(standard_curve$datapath))) != nrow(format_std_curve_metadata(meta_file$datapath)))
    {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}

    else {shinyjs::alert("Successful platform selected.")}}


  #qPCR platform not selected
  else {
    #shinyjs::alert("Please select qPCR platform type.")
  }

}

std_fluorescence_file_validation_msgs <- function(std_file) {
  if (file_ext(std_file$datapath) %ni% c("csv", "xlsx", "xls"))
  {shinyjs::alert("ERROR: Fluorescence file is not an accepted file type.")}
  else {}

}

std_metadata_file_validation_msgs <- function(meta_file){

  if (file_ext(meta_file$datapath) %ni% c("xlsx", "xls"))
  {shinyjs::alert("ERROR: Metadata file is not an accepted file type.")}


  #Checks to see if any of the sheets are empty (i.e: only contain field names)
  else if (nrow(read_excel(meta_file$datapath, sheet = 5)) == 0)
  {shinyjs::alert("ERROR:Required sheet in Metadata file is empty.")}


  #Check is metadata parsing function works on file
  else if (is.error(format_std_curve_metadata(meta_file$datapath)) == TRUE)
  {shinyjs::alert("ERROR: Metadata file is missing one or more necessary columns.")}


  # else {shinyjs::alert("Successful file upload.")}
}
########### These validations happen when the user hits submit#####

# have a function that can be used in the standard curve page (std.curve, platform, meta)

# invoke this function within the user uploaded data on the main data import page
#Function runs validation tests on user uploaded standard curve file.

user_uploaded_standard_curve_file_validation <- function(standard_curve, metadata_file, platform_type){
print("inside std. curve valid from main data import")
  print(platform_type)
  if (is.null(standard_curve) | is.null(metadata_file))
  {print("no files")
    return(TRUE)}

  else if (file_ext(standard_curve$datapath) %ni% c("csv", "xlsx", "xls"))
  {print("flu not the right format")
    return(TRUE)}

  else if (platform_type == 'None')
  {print("no platform selected")
    return(TRUE)}

  else if (file_ext(metadata_file$datapath) %ni% c("xlsx", "xls"))
  {print("metadata file type not working")
    return(TRUE)}

  else if (length(excel_sheets(metadata_file$datapath)) < 5)
  {print("metadata not enough sheets")
    return(TRUE)}

  #Checks to see if any of the sheets are empty (i.e: only contain field names)
  else if (nrow(read_excel(metadata_file$datapath, sheet = 5)) == 0)
  {print("meta stanrd sheet empty")
    return(TRUE)}

  #Checks to see if there is an error with formatting
  else if (is.error(format_std_curve_metadata(metadata_file$datapath)) == TRUE)
  {print("formatted std is wrong")
    return(TRUE)}


  #If files are MIC or BioRad
  else if (platform_type == "MIC/BioRad")
  {if (file_ext(standard_curve$datapath) != "csv")
  {print("file not csv for mic")
    return(TRUE)}


    #check if fluorescence file if biomeme file
    else if (read.csv(standard_curve$datapath)[1, 1] == 'Date & Time')
    {print("bmeme")
      return(TRUE)}


    #Check is fluorescence file processingfunction work
    else if (is.error(process_MIC_raw_data(read.csv(standard_curve$datapath))) == TRUE)
    {print("error in processing mic")
      return(TRUE)}


    #Check that formatted fluorescence file and formatted metadata file have the same number of rows
    if(TRUE){
      print('we came this far')
      print(nrow(process_MIC_raw_data(read.csv(standard_curve$datapath))))
      print(nrow(format_std_curve_metadata(metadata_file$datapath)))
    }
    else if (nrow(process_MIC_raw_data(read.csv(standard_curve$datapath))) != nrow(format_std_curve_metadata(metadata_file$datapath)))
    {return(TRUE)}


    else
    {return(FALSE)}
  }


  #If files are step one plus
  else if (platform_type == "StepOnePlus")
  {if (file_ext(standard_curve$datapath) %ni% c("xlsx", "xls"))
  {return(TRUE)}


    #Check is fluorescence file processing  function work
    else if (is.error(process_SOP_uploaded_file(read_excel(standard_curve$datapath, 4))) == TRUE)
    {return(TRUE)}

    #Check that formatted fluorescence file and formatted metadata file have the same number of rows
    else if (nrow(process_SOP_uploaded_file(read_excel(standard_curve$datapath, 4))) != nrow(format_std_curve_metadata(metadata_file$datapath)))
    {return(TRUE)}


    else
    {return(FALSE)}
  }


  #If files are step one plus Biomeme two3/Franklin
  else if (platform_type ==  "Biomeme two3/Franklin")
  {if (file_ext(standard_curve$datapath) != "csv")
  {return(TRUE)}


    #Check is fluorescence file processing  function work
    else if (is.error(process_biomeme_raw_data(read.csv(standard_curve$datapath))) == TRUE)
    {return(TRUE)}


    #Check that formatted fluorescence file and formatted metadata file have the same number of rows
    else if (nrow(process_biomeme_raw_data(read.csv(standard_curve$datapath))) != nrow(format_std_curve_metadata(metadata_file$datapath)))
    {return(TRUE)}


    else
    {return(FALSE)} }


  else {
    print("validation passed")
    return(FALSE)}

}

#Function runs validation tests on user uploaded files.
user_uploaded_file_validate <- function(fluor_file, metadata_file, platform_type, standard_curve, dataset_name){

 print("inside validation function")
   # return an error if necessary files are missing
  if (is.null(fluor_file) | is.null(metadata_file))
  {print("null files")
    return(TRUE)}

  # return an error if the experimental file is not rdml
  else if (file_ext(fluor_file$datapath) %ni% c("rdml"))
  {print("rdml type not passed")
    return(TRUE)}

  # if there is a standard curve file is provided but not platform selected, return an error
  else if (!is.null(standard_curve$datapath) & platform_type == 'None')
  {print("standard uploaded and no file type")
    return(TRUE)}

  # if the metadata files are not in excel format
  else if (file_ext(metadata_file$datapath) %ni% c("xlsx", "xls"))
  {print("metadata isn't excel")
    return(TRUE)}

  # any of the metadata sheet missing
  else if (length(excel_sheets(metadata_file$datapath)) < 5)
  {print("one of the sheets are missing")
    return(TRUE)}


  #Checks to see if any of the sheets are empty (i.e: only contain field names)
  else if (nrow(read_excel(metadata_file$datapath, sheet = 1)) == 0 |
           nrow(read_excel(metadata_file$datapath, sheet = 2)) == 0 |
           nrow(read_excel(metadata_file$datapath, sheet = 3)) == 0 |
           nrow(read_excel(metadata_file$datapath, sheet = 4)) == 0 |
           nrow(read_excel(metadata_file$datapath, sheet = 5)) == 0)
  {print("one of the sheets are empty")
    return(TRUE)}

  #Check is metadata parsing function works on file
  else if (is.error(format_qPCR_metadata(metadata_file$datapath)) == TRUE)
  {print('error in metadata processing')
    return(TRUE)}

  #Check is rdml parsing function works on file
  else if (is.error(process_Multiplexed_RDML(fluor_file$datapath)) == TRUE)
  {print('error in RDML processing')
    return(TRUE)}
  #
  #     #Check that formatted fluorescence file and formatted metadata file have the same number of rows
  else if (nrow(process_Multiplexed_RDML(fluor_file$datapath)[[1]]) != nrow(format_qPCR_metadata(metadata_file$datapath)))
  {print("something wrong with the number of rows")
    return(TRUE)}


  # If a standard curve file is provided, run a validation on them
  if (!is.null(standard_curve$datapath)){
    print("eval standard curve as true")

    if(user_uploaded_standard_curve_file_validation(standard_curve, metadata_file, platform_type)==TRUE){
      print("something wrong with standard curve processing")
      return(TRUE)}
    else{return(FALSE)}
    } # end of conditionals based on the presence of a standard curve file
  else{print("all validation passed")
    return(FALSE)}
}






