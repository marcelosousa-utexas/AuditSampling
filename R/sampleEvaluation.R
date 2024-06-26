evaluate_sample <- function(sample_planning, unitsToExamine,
                            booked_column_name = "Booked_Values",
                            audit_column_name = "Audited_Values",
                            confidence = 0.95,
                            estimation_method = "mean",
                            t_Student = FALSE) {

  stratum_name <- "Stratum"
  higher_values <- "Census"

  columnNamesClass <- columnNames(primaryKey = "primaryKey", booked_column_name = booked_column_name, audit_column_name = audit_column_name, data_column_name = "data_column_name", stratum_name = stratum_name, higher_values = higher_values)

  assign("col_data", columnNamesClass, envir = .AuditSampling_env)
  #print(.AuditSampling_env$col_data)

  #print(getStratumName())
  #t_Student <- FALSE
  alpha <- 1 - confidence

  #booked_column_name = "Booked_Values"
  #audit_column_name = "Audited_Values"

  pop_profile <- sample_planning %>%
    filter(grepl("^\\d+$", !!sym(getStratumName()))) %>%
    select(!!sym(getStratumName()), npop, Total)

  #print(pop_profile)

  if (!(audit_column_name %in% names(unitsToExamine))) {
    unitsToExamine <- unitsToExamine %>%
      mutate(!!sym(audit_column_name) := !!sym(booked_column_name))
  }

  unitsToExamine <- unitsToExamine %>%
    #mutate(!!sym(audit_column_name) := if_else(!!sym(audit_column_name) == "", NA, !!sym(audit_column_name))) %>%  # Convert empty strings to NA
    mutate(!!sym(audit_column_name) := coalesce(!!sym(audit_column_name), !!sym(booked_column_name)))  # Replace NA with values from column2


  unitsToExamine <- unitsToExamine %>%
    #filter(grepl("^\\d+$", Stratum)) %>%
    #mutate (!!sym(audit_column_name) := !!sym(booked_column_name)) %>%
    select(!!sym(getStratumName()), !!sym(booked_column_name), !!sym(audit_column_name))

  #print(unitsToExamine)

  sample_data <- unitsToExamine %>%
    filter(grepl("^\\d+$", !!sym(getStratumName())))

  ### FUNCTION DEFINITION

  error_count_function <- function(Booked_Values, Audit_Values) {
    difference <- Booked_Values - Audit_Values
    non_zero_count <- sum(difference != 0)
    return(non_zero_count)
  }


  calculate_mean <- function(estimation_method, Booked_Values, Audit_Values) {

    if (estimation_method == "mean") {
      mean = mean(Audit_Values)
    } else if (estimation_method == "difference")  {
      mean = mean(Audit_Values - Booked_Values)
    } else {
      print("Unknown estimation_method method")
      mean = NA
    }

    return(mean)
  }


  calculate_sd <- function(estimation_method, Booked_Values, Audit_Values) {

    if (estimation_method == "mean") {
      sd = sd(Audit_Values)
    } else if (estimation_method == "difference")  {
      sd = sd(Audit_Values - Booked_Values)
    } else {
      print("Unknown estimation_method method")
      sd = NA
    }

    return(sd)
  }

  sd_error_function <- function(sd, nsample) {
    sd / sqrt(nsample)
  }

  sd_error_mean <- function(sd, nsample) {
    sd / sqrt(nsample)
  }

  expected_audited_value_func <- function(sum_booked, mean, npop) {


    if (estimation_method == "mean") {
      expected_audited_value <- mean * npop
    } else if (estimation_method == "difference")  {
      expected_audited_value <- sum_booked + (mean * npop)
    } else {
      print("Unknown estimation_method method")
      expected_audited_value = NA
    }

    return(expected_audited_value)

  }

  standard_error_pop_func <- function(sd_error, npop, nsample, sum_booked, exp_audited) {
    npop <- as.numeric(npop)
    nsample <- as.numeric(nsample)
    expected_std_error_pop <- sd_error * sqrt(npop*(npop-nsample))
    return(expected_std_error_pop)
  }


  # Using the approx. of degree of freedom proposed by Satterthwaite, 1946 referenced by Cochran, 1997 (page 96)
  calculate_df <- function(npop, nsample, sd) {
    g <- npop*(npop - nsample)/nsample
    num <- (sum(g * sd ** 2))**2
    den <- sum((g**2 * sd** 4)/(nsample - 1))
    #df <- round(num/den,6)  #IDEA
    df <- floor(num/den) #round it down to be more conservative (see https://online.stat.psu.edu/stat506/book/export/html/655)
    return(df)
  }

  precision_function <- function(exp_sd_error, alpha, nsample, npop, sd, t_Student = FALSE) {

    if (t_Student) {

      df <- calculate_df(npop, nsample, sd)
      #t_student = round(qt(1 - alpha/2, df),6) #IDEA
      t_student = qt(1 - alpha/2, df-1 )
      precision <- exp_sd_error * t_student

    } else {
      z_alpha <- round(qnorm(1 - alpha/2),2)
      precision <- exp_sd_error * z_alpha
    }

    return(precision)
  }


  var_mean_function <- function (sd, nsample, npop) {
    #wh <- npop/sum(npop)
    #var_mean <- sum((wh**2 * sd**2)/nsample) - sum(wh * sd**2)/sum(npop)
    npop <- as.numeric(npop)
    nsample <- as.numeric(nsample)
    var_mean <- sum(npop*(npop - nsample) * sd**2 / nsample)/sum(npop)**2
    return(var_mean)

  }

  ### CALCULATE PRECISION


  sample_result <- sample_data %>%
    mutate(!!sym(getStratumName()) := as.numeric(!!sym(getStratumName()))) %>%
    group_by(!!sym(getStratumName())) %>%
    #summarise(nsample = n(), sum_booked = sum(Booked_Values), sum_audited = sum(Audit_Values),  mean = mean(Audit_Values), sd = sd(Audit_Values), error_count = error_count_function(Booked_Values,Audit_Values))
    summarise(nsample = n(), sum_booked = sum(!!sym(booked_column_name)), sum_audited = sum(!!sym(audit_column_name)),  mean = calculate_mean(estimation_method, !!sym(booked_column_name), !!sym(audit_column_name)), sd = calculate_sd(estimation_method, !!sym(booked_column_name), !!sym(audit_column_name)), error_count = error_count_function(!!sym(booked_column_name), !!sym(audit_column_name)))

  #sample_result
  #sample_result$strata <- as.numeric(sample_result$strata)

  # Combine the results
  all_result <- sample_result %>%
    mutate(npop = pop_profile$npop, sum_pop = pop_profile$Total) %>%
    #mutate(sd_error = sd_error_function(sd, nsample), exp_audited = expected_audited_value_func(sum_pop, mean_diff, npop), exp_sd_error = standard_error_pop_func(sd_error, npop), precision = precision_func(exp_sd_error, nsample)) %>%
    mutate(sd_error = sd_error_function(sd, nsample), exp_audited = expected_audited_value_func(sum_pop, mean, npop), exp_sd_error = standard_error_pop_func(sd_error, npop, nsample, sum_pop, exp_audited), precision = precision_function(exp_sd_error, alpha, nsample, npop, sd, t_Student)) %>%
    #mutate(Stratum = as.numeric(Stratum)) %>%
    relocate(npop, .after = !!sym(getStratumName())) %>%
    relocate(sum_pop, .after = npop) %>%
    relocate(exp_audited, .after = sum_pop) %>%
    relocate(exp_sd_error, .after = exp_audited) %>%
    relocate(sd_error, .after = sd)
    #arrange(Stratum)

  all_result <- as.data.frame(all_result)
  all_result <- all_result %>%
    mutate(!!sym(getStratumName()) := as.character(!!sym(getStratumName())))

  # Function to check if booked_value is within the specified interval
  check_within_interval <- function(materiality, booked_value, expected_audited_value, precision) {
    # Check if booked_value is within the interval
    #print(expected_audited_value - precision)
    #print(booked_value - materiality )

    #print(expected_audited_value + precision)
    #print(booked_value + materiality )

    is_within_interval <- (expected_audited_value - precision) >= booked_value - materiality &
      (expected_audited_value + precision) <= booked_value + materiality

    return(is_within_interval)
  }

  totals <- all_result %>%
    summarise(
      !!sym(getStratumName()) := "Sub Total",
      var_mean = var_mean_function(sd, nsample, npop),
      var_estimate = sum(npop)**2*var_mean,
      exp_sd_error = sqrt(var_estimate),
      #var_mean = var_mean_function2(sample_result$sd, sample_result$nsample, sample_result$npop, sample_result$pi),
      sd_error = sqrt(var_mean),
      precision = precision_function(exp_sd_error, alpha, nsample, npop, sd, t_Student),
      #var_mean_y = var_mean_y_function(npop, nsample, squared_sum, mean, var_mean),
      nsample = sum(nsample),
      npop = sum(npop),
      exp_audited = sum(exp_audited),
      sum_pop = sum(sum_pop),
      sum_booked = sum(sum_booked),
      sum_audited = sum(sum_audited),
      #diff_sum = sum(diff_sum),
      #mean_diff = sum(mean_diff),
      sd = sum(sd),
      error_count = sum(error_count)

    ) %>%
    select(-var_estimate, -var_mean)
  #select(-sd_error_estimate, -var_estimate)

  sub_strata_censu <- unitsToExamine %>%
    filter(!!sym(getStratumName()) == getHigherValues())

  sub_strata_censu <- sub_strata_censu %>%
    group_by(!!sym(getStratumName())) %>%
    summarise(
      !!sym(getStratumName()) := getHigherValues(),
      nsample = n(),
      npop = n(),
      exp_audited = sum(!!sym(audit_column_name)),
      sum_pop = sum(!!sym(booked_column_name)),
      sum_booked = sum(!!sym(booked_column_name)),
      sum_audited = sum(!!sym(audit_column_name)),
      error_count = error_count_function(!!sym(booked_column_name), !!sym(audit_column_name)),
      #exp_sd_error = "-",
      #sd = "-",
      #precision = "-"
    ) %>%
    ungroup()

  if (length(sub_strata_censu$npop) > 0) {
    strata <- bind_rows(all_result, totals, sub_strata_censu)
  } else {
    strata <- bind_rows(all_result)
  }

  #print(strata)
  #print("strata")

  total_line <- strata %>%
    filter(!(!!sym(getStratumName()) == "Sub Total")) %>%
    summarise(!!sym(getStratumName()) := "Total",
              #sd = sqrt(sum(npop*(npop - ni) * sd**2 / ni)/sum(npop)**2),
              #sd = sqrt((sum(npop**2 * sd**2/ni)/(sum(npop)**2)) - (sum(npop * sd**2)/(sum(npop)**2))),
              sd = totals$sd,
              precision = totals$precision,
              exp_sd_error = totals$exp_sd_error,
              nsample = sum(nsample),
              npop = sum(npop),
              exp_audited = sum(exp_audited),
              sum_pop = sum(sum_pop),
              sum_booked = sum(sum_booked),
              sum_audited = sum(sum_audited),
              error_count = sum(error_count)
    )

  strata <- bind_rows(strata, total_line)

  #sample_result <- bind_rows(all_result, totals)
  sample_result <- strata %>%
    select(-sd_error, -mean) %>%
    #relocate(mean_diff, .after = sd) %>%
    #relocate(exp_audited, .after = mean_diff) %>%
    relocate(nsample, .after = sum_pop) %>%
    relocate(sum_booked, .after = nsample) %>%
    relocate(sum_audited, .after = sum_booked) %>%
    #relocate(diff_sum, .after = sum_audited) %>%
    #relocate(exp_audited, .after = diff_sum) %>%
    relocate(error_count, .after = precision)

  sample_result <- data.frame(sample_result)
  #print(sample_result)

  # format_column <- function(dataframe) {
  #   formatC(dataframe, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
  # }
  #
  # # Apply the format_column function to all columns in the dataframe
  # sample_result  <- sample_result %>%
  #   mutate_at(vars(-one_of("Stratum", "npop", "nsample", "error_count")), format_column) %>%
  #   mutate(
  #     npop =  formatC(npop, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
  #     Stratum = ifelse(is.na(Stratum), "Total", Stratum)
  #   )

  #print(sample_result)
  #print(sample_result$nsample[strata[[getStratumName()]] == "Total"])

  return(sample_result)


}

add_ni_strata <- function(strata, max_index) {

  strata$ni[max_index] = strata$ni[max_index] + 1
  strata$ni[strata[[getStratumName()]] == "Total"] <- strata$ni[strata[[getStratumName()]] == "Total"] + 1

  strata_total <- strata %>%
    filter(!grepl("^\\d+$", !!sym(getStratumName())))

  strata <- strata %>%
    filter(grepl("^\\d+$", !!sym(getStratumName()))) %>%
    mutate (
      pi = ni/sum(ni)
    )

  strata <- rbind(strata, strata_total)
  return(strata)

}

take_more_samples <-  function (my_data, data_column_name, booked_column_name, audit_column_name, desired_precision, strata, unitsToExamine, eval_dataframe, confidence = 0.95, estimation_method = "mean", t_Student = FALSE) {

  temp_eval_dataframe <- eval_dataframe

  eval_verification <- eval_dataframe %>%
    filter(grepl("^\\d+$", !!sym(getStratumName())))

  sample_achieved_precision <- max(eval_dataframe$precision, na.rm = TRUE)

  while (sample_achieved_precision > desired_precision) {

    sde_col <- eval_verification$exp_sd_error

    while(length(sde_col) > 1) {
      # Find the maximum value
      max_val <- max(sde_col, na.rm = TRUE)

      # Find the index of the maximum value
      max_index <- which(sde_col == max_val)

      sde_col <- sde_col[-max_index]

      if ( eval_verification$nsample[max_index] < eval_verification$npop[max_index] ) {
        #eval_verification$nsample[max_index] = eval_verification$nsample[max_index] + 1
        #print(max_val)

        # if ("unitToSample" %in% names(my_data)) {
        #   remaining_df <- my_data %>%
        #     filter(unitToSample == 0) %>%
        #     select(-unitToSample)
        # } else {
        #   remaining_df <- anti_join(my_data, unitsToExamine, by = names(my_data))
        #   remaining_df <- remaining_df %>%
        #     filter(Stratum == eval_dataframe$Stratum[max_index])
        # }

        remaining_df <- anti_join(my_data, unitsToExamine, by = names(my_data))
        remaining_df <- remaining_df %>%
          filter(!!sym(getStratumName()) == eval_dataframe[[getStratumName()]][max_index])

        #sample_element <- sample(remaining_df[[data_column_name]], size = 1)
        sample_index <- sample(nrow(remaining_df), size = 1)
        sample_row <- remaining_df[sample_index, ]
        sample_row <- sample_row %>%
          mutate(!!sym(booked_column_name) :=  !!sym(data_column_name))

        if (audit_column_name %in% names(unitsToExamine)) {
          #unitsToExamine <- unitsToExamine %>% select(-Audited_Values)
          sample_row <- sample_row %>%
            mutate(!!sym(audit_column_name) := NA)
        }

        unitsToExamine <- rbind(unitsToExamine, sample_row)

        strata <- add_ni_strata(strata, max_index)

        #eval_dataframe <- evaluate_sample(strata, unitsToExamine)
        eval_dataframe <- evaluate_sample(strata, unitsToExamine, booked_column_name, audit_column_name, confidence, estimation_method)


        eval_verification <- eval_dataframe %>%
          filter(grepl("^\\d+$", !!sym(getStratumName())))


        sample_achieved_precision <- max(eval_dataframe$precision, na.rm = TRUE)



        break
      }
      else {
        next
      }

    }

    if (desired_precision >= sample_achieved_precision) {
      break
    }

  }

  unitsToExamine <- unitsToExamine %>%
    arrange(!!sym(data_column_name))

  return(list(unitsToExamine = unitsToExamine, eval_dataframe = eval_dataframe))

}
