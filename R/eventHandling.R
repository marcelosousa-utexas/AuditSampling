
read_data_file <- function (filepath) {

  # List of common Excel file extensions
  excel_extensions <- c("xls", "xlsx", "xlsm", "xlsb")

  # Example file extension
  file_extension <- tools::file_ext(filepath)
  #print(file_extension)

  # Compare if the file extension is in the list of Excel extensions
  if (file_extension %in% excel_extensions) {
    df <- read_excel(filepath)
  } else if (file_extension %in% c("csv")) {
    df <- read.csv(filepath)
  } else {
    # Code to execute if the file extension is neither Excel nor CSV
    stop("Extension not supported.")
  }

  return(df)

}

read_sampling_file <- function (filepath) {

  # List of common Excel file extensions
  excel_extensions <- c("xls", "xlsx", "xlsm", "xlsb")

  # Example file extension
  file_extension <- tools::file_ext(filepath)
  #print(file_extension)

  # Compare if the file extension is in the list of Excel extensions
  if (file_extension %in% excel_extensions) {
    #df <- read_excel(filepath)
    parameters <- read_excel(filepath, sheet = "Parameters")
    sampleDesign <- read_excel(filepath, sheet = "Sample Design")
    stratifiedData <- read_excel(filepath, sheet = "Stratified Data")
    sampletoAnalyse <- read_excel(filepath, sheet = "Sample to Analyse")

    # Store the data frames in a list

  } else if (file_extension %in% c("csv")) {
    df <- read.csv(filepath)
  } else {
    # Code to execute if the file extension is neither Excel nor CSV
    stop("Extension not supported.")
  }

  return(list(parameters = parameters, sampleDesign = sampleDesign, stratifiedData = stratifiedData, sampletoAnalyse = sampletoAnalyse))

}


nicer_number_view <- function(sample_result) {

  # Function to format numeric columns
  format_column <- function(column) {
    if (all(column == floor(column))) {
      formatC(column, format = "f", big.mark = ".", decimal.mark = ",", digits = 0)
    } else {
      formatC(column, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
  }

  # Apply the format_column function to all numeric columns
  sample_result <- sample_result %>%
    mutate_if(is.numeric, format_column)

  return(sample_result)
}


show_warning_yes_no <- function (achieved_precision, desired_precision) {

  text = paste("Achieved Precision from the sample was ", achieved_precision ,", which is greater than the planned precision of ", desired_precision,". Do you want to increase your sample size to match the specifications?",  sep = "")

  showModal(modalDialog(
    title = "Important message",
    div(id = "textmsg", paste(text)),
    footer = tagList(
      actionButton("no", "No"),
      actionButton("yes", "Yes")
    )
  ))

}


show_warning_msg <- function (old_n, new_n, achieved_precision, desired_precision) {

  #text = paste("Achieved Precision from the sample was ", achieved_precision, ", which is greater than the planned precision of ", desired_precision ,". Do you want to increase your sample size to match the specifications?")
  text = paste("The sample size was increased from ", old_n ," to " , new_n, " to achieve the planned precision of ", desired_precision,". Now the achieved precision is ", achieved_precision, ".", sep = "")

  showModal(modalDialog(
    title = "Important message",
    div(id = "textmsg", paste(text)),
    footer = tagList(
      actionButton("ok", "OK")
    )
  ))

}
