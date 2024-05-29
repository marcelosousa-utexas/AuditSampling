unitsToSample <- function(my_data, data_column_name,
                          primaryKey,
                          sample_planning,
                          booked_column_name = "Booked_Values",
                          audit_column_name = "Audited_Values") {



  census <- my_data %>%
    filter(!!sym(getStratumName()) == getHigherValues())  %>%
    arrange(!!sym(getStratumName()), !!sym(data_column_name) )

  sampling_data <- my_data %>%
    filter(!(!!sym(getStratumName()) == getHigherValues()))

  sample_planning <- sample_planning %>%
    filter(grepl("^\\d+$", !!sym(getStratumName())))

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
    filter(grepl("^\\d+$", !!sym(getStratumName()))) %>%
    #mutate(Stratum = as.numeric(Stratum)) %>%
    # Group by Stratum
    group_by(!!sym(getStratumName())) %>%
    # Sample data from each group
    nest() %>%
    mutate(data = map2(data, !!sym(getStratumName()), ~sample_func(.x, ni, .y))) %>%
    ungroup() %>%
    unnest(data) %>%
    #arrange(Stratum, !!sym(data_column_name)) %>%
    arrange(!!sym(data_column_name)) %>%
    relocate(!!sym(primaryKey))

  audit_units <- rbind(sample_data, census)


  audit_units <- audit_units %>%
    mutate(
      !!sym(booked_column_name) :=  !!sym(data_column_name),
      #Audited_Values = !!sym(data_column_name)
      #!!sym(audit_column_name) :=  NA
    )

  if (!(audit_column_name %in% names(audit_units))) {
    audit_units <- audit_units %>%
      mutate(!!sym(audit_column_name) := NA)
  }


  return(audit_units)
}


moreUnitsToSample <- function(dataframe, data_column_name, primaryKey, unitsToSample, ni, booked_column_name = "Booked_Values", audit_column_name = "Audited_Values") {

  #print(unitsToSample)
  # Define the column name to check
  # audited_column <- "Audited_Values"
  #
  # if (audited_column %in% names(unitsToSample)) {
  #   unitsToSample <- unitsToSample %>% select(- !!sym(audited_column))
  # }


  #print(unitsToSample)

  sampling_data <- dataframe %>%
    # mutate(
    #   Booked_Values = !!sym(data_column_name),
    #   Audited_Values = !!sym(data_column_name)
    # )  %>%
    filter(unitToSample == 0) %>%
    select(-unitToSample)

  # Define a custom sampling function
  sample_func <- function(data, n, stratum) {
    # Sample size cannot exceed data size
    sample_size <- min(n[[stratum]], nrow(data))
    # Use slice_sample function for stratified sampling
    return(slice_sample(data, n = sample_size))
  }

  sample_data <- sampling_data %>%
    # Filter for numeric strata (optional)
    filter(grepl("^\\d+$", !!sym(getStratumName()))) %>%
    #mutate(Stratum = as.numeric(Stratum)) %>%
    # Group by Stratum
    group_by(!!sym(getStratumName())) %>%
    # Sample data from each group
    nest() %>%
    mutate(data = map2(data, !!sym(getStratumName()), ~sample_func(.x, ni, .y))) %>%
    ungroup() %>%
    unnest(data) %>%
    arrange(!!sym(getStratumName()), !!sym(data_column_name)) %>%
    relocate(!!sym(primaryKey))

  # sample_data <- sample_data %>%
  #   mutate(
  #     Booked_Values = !!sym(data_column_name)
  #   )

  audit_units <- bind_rows(unitsToSample, sample_data)


  audit_units <- audit_units %>%
    mutate(!!sym(booked_column_name) :=  ifelse(is.na(!!sym(booked_column_name)), !!sym(data_column_name), !!sym(booked_column_name))) %>%
    # mutate(
    #   Booked_Values = !!sym(data_column_name)
    #   #Audited_Values = ""
    # )  %>%
    arrange(!!sym(getStratumName()), !!sym(data_column_name))



  # if (!(audited_column %in% names(audit_units))) {
  #   print("not in column2")
  #   audit_units <- audit_units %>%
  #     mutate(!!sym(audited_column) := "")
  # }

  return(audit_units)

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

updateNi <- function(dataframe, confidence, precision, n_min, ni_min) {

  alplha <- 1 - confidence

  eval <- dataframe %>%
    filter(grepl("^\\d+$", !!sym(getStratumName())))

  new_ni <- get_ni(eval$npop, eval$sd, eval$nsample/(sum(eval$nsample)), alplha, precision, n_min, ni_min)

  eval$nsample <- new_ni - eval$nsample

  ni <- eval %>%
    {setNames(.$nsample, .$Stratum)}

  return(list(ni = ni, new_ni = new_ni))
}

updateDataBaseUnitsToSample <- function(dataframe, data_column_name, primaryKey, unitsToSample) {

  check_unitToSample <- function(invoice_amount, Stratum) {
    if (!is.na(invoice_amount)) {
      if (getStratumName() == getHigherValues()) {
        return(2)
      } else {
        return(1)
      }
    } else {
      return(0)
    }
  }

  #dataframe <- dataframe %>%
  #  mutate(unitToSample = ifelse(primaryKey %in% unitsToSample$primaryKey, 1, 0))


  unitsToSample <- unitsToSample %>%
    select(!!sym(primaryKey), !!sym(data_column_name))

  # Custom function to determine unitToSample value


  # Apply the custom function to create the new column

  dataframe <- left_join(dataframe, unitsToSample, by = "primaryKey", suffix = c("", ".temp"))

  data_column_name <- paste(data_column_name, ".temp", sep="")

  dataframe <- dataframe %>%
    mutate(unitToSample = mapply(check_unitToSample, !!sym(data_column_name), !!sym(getStratumName()))) %>%
    select(-!!sym(data_column_name))

  sum(dataframe$unitToSample)

  return(dataframe)

}

