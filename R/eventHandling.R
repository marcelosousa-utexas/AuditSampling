.AuditSampling_env <- new.env()

setClass("columnNames", slots = list(
  primaryKey = "character",
  booked_column_name = "character",
  audit_column_name = "character",
  data_column_name = "character",
  stratum_name = "character",
  higher_values = "character"
))


# Constructor function to create instances of GlobalVariables class
columnNames <- function(primaryKey, booked_column_name, audit_column_name, data_column_name, stratum_name, higher_values) {
  obj <- new(
    "columnNames",
    primaryKey =  primaryKey,
    booked_column_name =  booked_column_name,
    audit_column_name =  audit_column_name,
    data_column_name = data_column_name,
    stratum_name = stratum_name,
    higher_values = higher_values
  )
  return(obj)
}

# Setter method to set multiple attributes at once
setValues <- function(values) {
#setValues <- function(obj, values) {
  # Check if values is a list
  if (!is.list(values)) {
    stop("Input must be a list of values.")
  }

  # Check each value and set corresponding slot
  if ("primaryKey" %in% names(values)) {
    primaryKey <- values[["primaryKey"]]
    if (!is.character(primaryKey)) {
      stop("PrimaryKey must be a non-negative numeric value.")
    }
    #obj@primaryKey <- primaryKey
    .AuditSampling_env$col_data@primaryKey <- primaryKey
  }

  if ("booked_column_name" %in% names(values)) {
    booked_column_name <- values[["booked_column_name"]]
    if (!is.character(booked_column_name)) {
      stop("Booked Values must be a character value.")
    }
    #obj@booked_column_name <- booked_column_name
    .AuditSampling_env$col_data@booked_column_name <- booked_column_name
  }

  if ("audit_column_name" %in% names(values)) {
    audit_column_name <- values[["audit_column_name"]]
    if (!is.character(audit_column_name)) {
      stop("Audited Values must be a character value.")
    }
    #obj@Audited_Values <- Audited_Values
    .AuditSampling_env$col_data@audit_column_name <- audit_column_name
  }

  if ("data_column_name" %in% names(values)) {
    data_column_name <- values[["data_column_name"]]
    if (!is.character(data_column_name)) {
      stop("Data column name of data must be a character value.")
    }
    #obj@Audited_Values <- Audited_Values
    .AuditSampling_env$col_data@data_column_name <- data_column_name
  }

  if ("stratum_name" %in% names(values)) {
    stratum_name <- values[["stratum_name"]]
    if (!is.character(stratum_name)) {
      stop("Stratum name must be a character value.")
    }
    #obj@Audited_Values <- Audited_Values
    .AuditSampling_env$col_data@stratum_name <- stratum_name
  }

  if ("higher_values" %in% names(values)) {
    higher_values <- values[["higher_values"]]
    if (!is.character(higher_values)) {
      stop("Higher values name must be a character value.")
    }
    #obj@Audited_Values <- Audited_Values
    .AuditSampling_env$col_data@higher_values <- higher_values
  }

  #return(obj)
}


getPrimaryKey <- function() {
  return(.AuditSampling_env$col_data@primaryKey)
}

getBookedColumnName <- function() {
  return(.AuditSampling_env$col_data@booked_column_name)
}

getAuditedColumnName <- function() {
  return(.AuditSampling_env$col_data@audit_column_name)
}

getDataColumnName <- function() {
  return(.AuditSampling_env$col_data@data_column_name)
}

getStratumName <- function() {
  return(.AuditSampling_env$col_data@stratum_name)
}

getHigherValues <- function() {
  return(.AuditSampling_env$col_data@higher_values)
}


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
    df <- read_csv(filepath, show_col_types = FALSE)
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
    #uiOutput("warning")
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


show_warning_minimal_ni <- function () {

  #text = paste("Achieved Precision from the sample was ", achieved_precision, ", which is greater than the planned precision of ", desired_precision ,". Do you want to increase your sample size to match the specifications?")
  text = "The minimal allowed value for the minimal sample size of each stratum is 2."

  showModal(modalDialog(
    title = "Warning",
    div(id = "textmsg", paste(text)),
  ))

}


