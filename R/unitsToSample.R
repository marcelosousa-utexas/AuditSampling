unitsToSample <- function(my_data, data_column_name, primaryKey, result) {

  sample_planning <- result$sample_planning
  # #best_n_dataframe <- result$best_n_dataframe
  # bins <- result$optimum_result$bins[[1]]
  #
  # my_data <- my_data %>%
  #   mutate(
  #     Stratum = as.character(cut(!!sym(data_column_name), breaks = bins, labels = FALSE, include.lowest = TRUE)),
  #     Stratum = ifelse(is.na(Stratum), "Censo", Stratum)
  #   )

  censo <- my_data %>%
    filter(Stratum == "Censo")  %>%
    arrange(Stratum, !!sym(data_column_name) )

  sampling_data <- my_data %>%
    filter(!(Stratum == "Censo"))

  sample_planning <- sample_planning %>%
    filter(grepl("^\\d+$", Stratum))

  ni <- sample_planning %>%
    {setNames(.$ni, .$Stratum)}

  # Define a custom sampling function
  sample_func <- function(data, n, stratum) {
    # Sample size cannot exceed data size
    sample_size <- min(n[[stratum]], nrow(data))
    # Use slice_sample function for stratified sampling
    return(slice_sample(data, n = sample_size))
  }

  sample_data <- sampling_data %>%
    # Filter for numeric strata (optional)
    filter(grepl("^\\d+$", Stratum)) %>%
    #mutate(Stratum = as.numeric(Stratum)) %>%
    # Group by Stratum
    group_by(Stratum) %>%
    # Sample data from each group
    nest() %>%
    mutate(data = map2(data, Stratum, ~sample_func(.x, ni, .y))) %>%
    ungroup() %>%
    unnest(data) %>%
    arrange(Stratum, !!sym(data_column_name)) %>%
    relocate(!!sym(primaryKey))



  unidades_auditoria <- rbind(sample_data, censo)

  sample_data <- sample_data %>%
    mutate(
      Booked_Values = !!sym(data_column_name)
    )

  unidades_auditoria <- unidades_auditoria %>%
    mutate(
      Booked_Values = !!sym(data_column_name)
    )

}

# df1 <- read.csv("/home/marcelo/Downloads/Invoices.csv") %>%
#   select(Invoice_Number, Invoice_Amount)
# df2 <- read.csv("/home/marcelo/Downloads/sampleUnits-2024-05-17.csv") %>%
#   select(Invoice_Number, Invoice_Amount)
#
# # Create a new column in Dataframe1 to indicate if each row is present in Dataframe2
# df1 <- df1 %>%
#   mutate(unitToSample = ifelse(Invoice_Number %in% df2$Invoice_Number, 1, 0))
#
#
#
check_unitToSample <- function(invoice_amount, Stratum) {
  if (!is.na(invoice_amount)) {
    if (Stratum == "Censo") {
      return(2)
    } else {
      return(1)
    }
  } else {
    return(0)
  }
}

updateDataBaseUnitsToSample <- function(dataframe, data_column_name, primaryKey, unitsToSample) {
  #dataframe <- dataframe %>%
  #  mutate(unitToSample = ifelse(primaryKey %in% unitsToSample$primaryKey, 1, 0))


  unitsToSample <- unitsToSample %>%
    select(!!sym(primaryKey), !!sym(data_column_name))

  # Custom function to determine unitToSample value


  # Apply the custom function to create the new column

  dataframe <- left_join(dataframe, unitsToSample, by = "primaryKey", suffix = c("", ".temp"))

  data_column_name <- paste(data_column_name, ".temp", sep="")

  dataframe <- dataframe %>%
    mutate(unitToSample = mapply(check_unitToSample, !!sym(data_column_name), Stratum)) %>%
    select(-!!sym(data_column_name))

  sum(dataframe$unitToSample)

  return(dataframe)

}

