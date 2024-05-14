getExcelFile <- function() {

  # # Define file types
  # filetypes <- matrix(c("CSV files", "*.csv",
  #                       "Excel 2000 files", "*.xls",
  #                       "Excel files", "*.xlsx",
  #                       "All files", "*.*"), ncol = 2, byrow = TRUE)

  # Define file types
  filetypes <- matrix(c("Excel files", "*.xlsx", "Excel 2000 files", "*.xls"), ncol = 2, byrow = TRUE)

  # Launch file browser with specified file types
  selected_file <- tk_choose.files(filters = filetypes)


  # Check if a file was selected
  if (length(selected_file) > 0) {
    cat("Selected file:", selected_file, "\n")
    return(selected_file)
  } else {
    cat("No file selected.\n")
    selected_file <- NA
  }

}




# Function to create a GUI for selecting the sheet name
selectSheetName <- function(file_path) {
  #library(readxl)
  # Get the names of all sheets in the Excel file
  sheet_names <- readxl::excel_sheets(file_path)

  # Create a Tcl/Tk window
  win <- tktoplevel()
  tkwm.title(win, "Select Sheet Name")

  # Function to handle button click
  okButtonClicked <- function() {
    selected_sheet <- tclvalue(sheet_name)
    tkdestroy(win)  # Close the window
    return(selected_sheet)
  }

  # Add a label
  lbl_message <- tklabel(win, text = "Select the name of the sheet:")
  tkpack(lbl_message, padx = 10, pady = 10)

  # Add a combo box for selecting the sheet name
  sheet_name <- tclVar("")
  cmb_sheet_name <- ttkcombobox(win, textvariable = sheet_name, values = sheet_names)
  tkpack(cmb_sheet_name, padx = 10, pady = 10)

  # Add an OK button
  btn_ok <- tkbutton(win, text = "OK", command = okButtonClicked)
  tkpack(btn_ok, padx = 10, pady = 10)

  # Center the window on the screen
  screen_width <- as.integer(tkwinfo("screenwidth", win))
  screen_height <- as.integer(tkwinfo("screenheight", win))
  window_width <- 300  # Width of the window
  window_height <- 150  # Height of the window
  x <- (screen_width - window_width) / 2
  y <- (screen_height - window_height) / 2
  tkwm.geometry(win, sprintf("%dx%d+%d+%d", window_width, window_height, x, y))

  # Start the event loop
  tkwait.window(win)

  # Return the selected sheet name
  return(tclvalue(sheet_name))
}


# Function to create a GUI for selecting the column name
selectColumnName <- function(df) {
  # Get the names of all columns in the dataframe
  column_names <- colnames(df)

  # Create a Tcl/Tk window
  win <- tktoplevel()
  tkwm.title(win, "Select Column Name")

  # Function to handle button click
  okButtonClicked <- function() {
    selected_column <- tclvalue(column_name)
    tkdestroy(win)  # Close the window
    return(selected_column)
  }

  # Add a label
  lbl_message <- tklabel(win, text = "Select the name of the column:")
  tkpack(lbl_message, padx = 10, pady = 10)

  # Add a combo box for selecting the column name
  column_name <- tclVar("")
  cmb_column_name <- ttkcombobox(win, textvariable = column_name, values = column_names)
  tkpack(cmb_column_name, padx = 10, pady = 10)

  # Add an OK button
  btn_ok <- tkbutton(win, text = "OK", command = okButtonClicked)
  tkpack(btn_ok, padx = 10, pady = 10)

  # Center the window on the screen
  screen_width <- as.integer(tkwinfo("screenwidth", win))
  screen_height <- as.integer(tkwinfo("screenheight", win))
  window_width <- 300  # Width of the window
  window_height <- 150  # Height of the window
  x <- (screen_width - window_width) / 2
  y <- (screen_height - window_height) / 2
  tkwm.geometry(win, sprintf("%dx%d+%d+%d", window_width, window_height, x, y))

  # Start the event loop
  tkwait.window(win)

  # Return the selected column name
  return(tclvalue(column_name))
}

loadFile <- function() {

  fileLocation <- getExcelFile()

  data <- c()
  data_column_name <- c()

  if (!is.na(fileLocation)) {
    sheetName <- selectSheetName(fileLocation)
    if (!sheetName == "") {
      data <- read_excel(fileLocation, sheet = sheetName, col_names = TRUE)
    } else {
      data <- read_excel(fileLocation, col_names = TRUE)
    }
  }

  if (!is.null(nrow(data))) {
    data_column_name <- selectColumnName(data)
  }

  return(list(data = data, data_column_name = data_column_name))

}

