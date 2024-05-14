unitsToSample <- function(my_data, result) {

  sample_planning <- result$sample_planning
  #best_n_dataframe <- result$best_n_dataframe
  bins <- result$optimum_result$bins[[1]]

  my_data <- my_data %>%
    mutate(
      Stratum = as.character(cut(!!sym(data_column_name), breaks = bins, labels = FALSE, include.lowest = TRUE)),
      Stratum = ifelse(is.na(Stratum), "Censo", Stratum)
    )

  censo <- my_data %>%
    filter(Stratum == "Censo")  %>%
    arrange(!!sym(data_column_name) )

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
    unnest(data) %>%
    arrange(!!sym(data_column_name))

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

