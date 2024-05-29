#stratification_env <- new.env()
#stratification_env$primaryKey <- NULL
#stratification_env$booked_column_name <- NULL
#stratification_env$audit_column_name <- NULL


get_bounds_from_vector <- function(data) {
  if (!is.numeric(data)) {
    stop("Input must be a numeric vector.")
  }

  positive <- data[data >= 0]
  negative <- data[data < 0]

  if (length(positive) > 0) {
    upper_bound <- min(positive)
  } else {
    upper_bound <- Inf
  }

  if (length(negative) > 0) {
    lower_bound <- max(negative)
  } else {
    lower_bound <- -Inf
  }

  return(list(lower_bound = lower_bound, upper_bound = upper_bound))
}


build_bounds <- function(dataframe, data_column_name, user_cutoff = NA, direction = "both", remove_mode = "smooth", cutoff_alpha = NA) {

  data <- dataframe[[data_column_name]]
  if (is.na(user_cutoff)) {


    if (remove_mode == "very_smooth") {
      cutoff_alpha <- 0.01
    } else if (remove_mode == "smooth") {
      cutoff_alpha <- 0.05
    } else if (remove_mode == "moderate") {
      cutoff_alpha <- 0.2
    } else if (remove_mode == "high") {
      cutoff_alpha <- 0.3
    } else if (remove_mode == "very_high") {
      cutoff_alpha <- 0.5
    } else if (remove_mode == "custom") {
      cutoff_alpha <- cutoff_alpha
    } else {
      remove_mode <- "smooth"
      cutoff_alpha <- 0.05
    }

    z_threshold <- round(qnorm(1 - cutoff_alpha/2), 2)

    # Calculate z-scores
    if (direction == "both") {
      z_scores <- abs((data - mean(data)) / sd(data))
    } else if (direction == "upper") {
      z_scores <- (data - mean(data)) / sd(data)
    } else if (direction == "lower") {
      z_scores <- -(data - mean(data)) / sd(data)
    } else {
      direction <- "both"
      z_scores <- abs((data - mean(data)) / sd(data))
    }

    # Find indices of outliers
    outliers_indices <- which(z_scores > z_threshold)
    #print(outliers_indices)

    bounds <- get_bounds_from_vector(data[outliers_indices])
    #print(bounds)

    # Remove outliers
    #cleaned_data <- data[-outliers_indices]

  } else {

    if (length(user_cutoff) == 1 & is.numeric(user_cutoff)) {
      if (direction == "both") {
        direction <- "upper"
      }
    } else if (length(user_cutoff) == 2 & is.numeric(user_cutoff[1]) & is.numeric(user_cutoff[2])) {
      direction <- "both"
    } else {
      print("user_cutoff is not valid. Need to be a single numeric number or a 2 dimension numeric array")
      stop()
    }

    if (direction == "upper") {
      bounds <- c()
      bounds$lower_bound <- -Inf
      bounds$upper_bound <- user_cutoff


    } else if (direction == "lower") {
      bounds <- c()
      bounds$lower_bound <- user_cutoff
      bounds$upper_bound <- Inf
    } else {
      bounds <- get_bounds_from_vector(user_cutoff)

    }

  }

  return(bounds)
}


dataframe_cutoff <- function(dataframe, data_column_name, bounds) {

  lower_bound <- bounds$lower_bound[[1]]
  upper_bound <- bounds$upper_bound[[1]]

  census <- dataframe %>%
    filter( !!sym(data_column_name) <= lower_bound | !!sym(data_column_name)  >= upper_bound)

  dataframe <- dataframe %>%
    filter( !!sym(data_column_name)  > lower_bound & !!sym(data_column_name) < upper_bound)

  return(list(census = census, dataframe = dataframe))
}


round_to_human_interval <- function(x) {
  # Determine the magnitude of the number
  grandeza <- (floor(log10(abs(x))))
  magnitude <- 10 ^ grandeza
  if (grandeza < 6) {
    rounded_value <- floor((x/magnitude)/0.5)*0.5*magnitude
    factors <- c(c(1.5, 2, 5, 10) * magnitude, rounded_value)
    closest_factor <- factors[which.min(abs(factors - x))]
  } else {
    closest_factor <- magnitude
  }
  return(closest_factor)
}

get_bins_param <- function(dataframe, data_column_name) {

  param <- list()
  # Calculate the number of bins using the Freedman-Diaconis rule
  data <- dataframe[[data_column_name]]

  n <- length(data)
  IQR <- IQR(data)
  bin_width <- 2 * IQR * (n^(-1/3))

  rounded_bin_width <- round_to_human_interval(bin_width)

  # Calculate the number of bins using the rounded binwidth
  bins <- ceiling((max(data) - min(data)) / rounded_bin_width)

  param <- list(rounded_bin_width = rounded_bin_width, bins = bins)

  return (param)

}

get_freq_distribution <- function(dataframe, data_column_name, binwidth, number_of_bins) {

  data <- dataframe[[data_column_name]]
  # closed="right" is the default and is compatible with include.lowest = TRUE from cut method

  # Calculate histogram statistics without plotting
  hist_data <- ggplot(dataframe, aes(x = data)) +
    geom_histogram(color="black", fill="white", boundary = 0, binwidth = binwidth, bins = number_of_bins, closed="left") +
    #geom_histogram(color="black", fill="white", boundary = 0, bins = 500, closed="left") +
    #geom_histogram(color="black", fill="white", boundary = 0, binwidth = rounded_bin_width, bins = bins) +
    labs(x = "Column Name", y = "Frequency", title = "Histogram of Column Name")

  # Extracting the bin stratification
  bin_strata <- ggplot_build(hist_data)$data[[1]]
  bin_strata <- bin_strata[, 2:5]
  bin_strata$Interval <-  c(1:nrow(bin_strata))
  return (bin_strata)

}

add_interval_original_data <- function(dataframe, data_column_name, freq_distribution) {

  data <- dataframe[[data_column_name]]
  breaks <- c(freq_distribution$xmin, tail(freq_distribution$xmax, n = 1))  # Extract the breaks from the xmin and xmax columns

  breaks <- breaks - 0.01
  breaks[length(breaks)] <- breaks[length(breaks)]+ 0.01
  labels <- c(1:nrow(freq_distribution))

  dataframe$Interval <- cut(data, breaks = breaks, labels = labels, include.lowest = FALSE)
  dataframe$Interval <- as.numeric(dataframe$Interval)
  return(dataframe$Interval)
}

generate_freq_distribution <- function(dataframe, data_column_name, freq_distribution) {

  df_sum <- dataframe %>%
    group_by(Interval) %>%
    summarise(
      N = n(),
      Total = sum((!!sym(data_column_name))),
      Sum_Squares = sum((!!sym(data_column_name))**2)
    )

  freq_distribution <- inner_join(df_sum, freq_distribution, by = "Interval") %>%
    select(-count, -x) %>%
    relocate(Interval, xmin, xmax, N, Total, Sum_Squares)
  return(freq_distribution)

}

add_square_root_freq <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(
      cell_length = round(xmax - xmin, 0),
      sqrtf =  sqrt(N),
      csqrtf = cumsum(sqrtf * sqrt(cell_length)),
      ctotal = cumsum(Total)
    )
  return(dataframe)
}

linear_interpolation <- function(x1, y1, x2, y2, x) {
  # Calculate the slope (m) of the line
  m <- (y2 - y1) / (x2 - x1)

  # Calculate the y-intercept (b) of the line
  b <- y1 - m * x1

  # Calculate the y-coordinate of the point on the line at x
  y <- m * x + b

  return(y)
}

interpolate_duplicates <- function(vec) {
  # Identify indices of duplicated values
  duplicated_indices <- which(duplicated(vec) | duplicated(vec, fromLast = TRUE))

  # Iterate through each duplicated value
  for (value in unique(vec[duplicated_indices])) {
    indices <- which(vec == value)

    # If there are duplicates, perform interpolation
    if (length(indices) > 1) {
      start_idx <- indices[1] - 1
      end_idx <- indices[length(indices)] + 1

      # Ensure indices are within the bounds of the vector
      if (start_idx < 1) start_idx <- 1
      if (end_idx > length(vec)) end_idx <- length(vec)

      start_value <- vec[start_idx]
      end_value <- vec[end_idx]

      # Interpolate values
      n <- length(indices)
      for (i in seq_along(indices)) {
        vec[indices[i]] <- start_value + (end_value - start_value) * (i / (n + 1))
      }
    }
  }

  return(vec)
}


get_best_cum_freq <- function(allocation_method, dataframe, L) {

  if (allocation_method  == "Neyman") {
    cum_total <- dataframe$csqrtf
    total <- dataframe$sqrtf
  } else if (allocation_method  == "equal_dollar") {
    cum_total <- dataframe$ctotal
    total <- dataframe$Total
  } else {
    print("error")
  }

  T <- max(cum_total)

  number_of_boundaries <- L - 1

  # Initialize an empty vector to store the results
  boundaries <- numeric(number_of_boundaries)

  # For loop from 1 to L-1
  for(i in 1:number_of_boundaries){
    # Calculate the formula
    best_allocation_freq <- i*T/L

    differences <- sqrt((best_allocation_freq - cum_total)**2)
    index <- which.min(differences)

    #if(best_allocation_freq - cum_total[index] > 0) {
    if(index < length(differences) & best_allocation_freq - cum_total[index] > 0) {
      xmax1 <- dataframe$xmax[index]
      xmax2 <- dataframe$xmax[index + 1]
      freq_cum_1 <- cum_total[index]
      freq_cum_2 <- cum_total[index + 1]
      best_bound <- linear_interpolation(freq_cum_1, xmax1, freq_cum_2, xmax2, best_allocation_freq)

    #} else if (best_allocation_freq - cum_total[index] < 0) {
    } else if (index > 1 & best_allocation_freq - cum_total[index] < 0) {

      xmax1 <- dataframe$xmax[index]
      xmax2 <- dataframe$xmax[index - 1]
      freq_cum_1 <- cum_total[index]
      freq_cum_2 <- cum_total[index - 1]
      best_bound <- linear_interpolation(freq_cum_1, xmax1, freq_cum_2, xmax2, best_allocation_freq)


    } else {

      best_bound <- dataframe$xmax[index]
    }
    boundaries[i] <- best_bound

  }

  return(boundaries)

}


calculate_sd <- function(estimation_method, Sum_Squares, npop, mean, p1 = NULL) {

  if (estimation_method == "mean") {
    sd <- sqrt(abs((Sum_Squares - npop*mean**2)/(npop - 1)))
    return(sd)
  } else if (estimation_method == "difference")  {
    sd <- sqrt(abs((Sum_Squares - npop*mean**2)/(npop - 1)))
    sd_diff <- sqrt(p1*sd**2 + p1*(1 - p1)*mean**2)
    return(sd_diff)
  } else {
    print("Unknown estimation_method method")
  }

}

round_ni <- function(ni, n, n_min, ni_min) {

  floor_ni <- floor(ni)
  decimal_ni <- ni - floor(ni)
  round_up <- numeric(length(ni))

  while (sum(floor(ni)) < n) {
    max_position <- which.max(decimal_ni)
    decimal_ni[max_position] <- 0
    round_up[max_position] <- 1
    ni <- floor_ni + round_up
  }

  # Ensure ni is at least 5
  ni[ni < ni_min] <- ni_min

  return(ni)

}

# See S.8 ALLOCATION REQUIRING MORE TIIAN 100 PER CENT SAMPLING Cochran 1977 (fl. 118/pg. 114)
neyman_minimax <- function(npop, sd, ni, alpha, desired_precision, n_min, ni_min)  {

  n <- sum(ni)
  over <- which((npop - ni) <= 0)

  while(length(over) > 0) {

    under <- which((npop - ni) > 0)
    ni[over] <- npop[over]
    n <- n - sum(ni[over])
    ni[under] <- npop[under] * sd[under] / sum(npop[under] * sd[under]) * n

    pi <- ni/sum(ni)
    pi[under] <- ni[under]/sum(ni[under])

    n <- calculate_sample_size_n(npop[under], sd[under],  pi[under], alpha, desired_precision)
    n <- ceiling(n)

    if (is.nan(n) | n < n_min) {
      n <- n_min
    }

    ni[under] <- pi[under]*n
    #ni[under] <- round_ni(ni[under], n, n_min, ni_min)
    #ni[under] <- round_ni(ni[under], n - sum(ni[under]), n_min, ni_min - sum(ni[under]))

    pi <- ni/sum(ni)

    over <- which((npop - ni) < 0)

  }

  return(ni)

}

proportion_function <- function(estimation_method, allocation_method, sd, npop, Total = NULL, nsample = NULL) {

  if (estimation_method == "difference") {
    allocation_method = "Neyman"
  }

  if (allocation_method == "Neyman") {
    # Neyman allocation
    #return(neyman_minimax(npop, sd, n))
    #print(npop*sd/sum(npop*sd))

    return(npop*sd/sum(npop*sd))
    #return(neyman_minimax(npop, sd))
  } else if (allocation_method == "proportional")  {
    return(npop/sum(npop))
  } else if (allocation_method == "equal_dollar")  {
    #print(Total/sum(Total))
    return(Total/sum(Total))
    #return(npop*sd/sum(npop*sd))
  } else if (allocation_method == "custom")  {
    return(nsample/sum(nsample))
  } else {
    print("Unknown allocation method")
  }

}

sum_sample_size_num_function <- function(npop, sd, pi) {
  sum(npop**2*sd**2/pi)
}

sum_sample_size_den_function <- function(npop, sd) {
  sum(npop*sd**2)
}

calculate_sample_size_n <- function(npop, sd, pi, alpha, desired_precision) {

  sum_sample_size_num <- sum_sample_size_num_function(npop, sd, pi)
  sum_sample_size_den <- sum_sample_size_den_function(npop, sd)
  z_alpha <- round(qnorm(1 - alpha/2),2)
  #nsample <- 50
  #t_alpha <- qt(1 - alpha/2, nsample-1)
  n <- z_alpha**2*sum_sample_size_num/(desired_precision**2 + z_alpha**2*sum_sample_size_den)

  return(n)
}

amount_precision_function <- function(exp_sd_error, alpha, nsample, npop, t_Student = FALSE) {

  if (t_Student) {

    df <- calculate_df(npop, nsample, sd)
    #t_student = round(qt(1 - alpha/2, df),6) #IDEA
    t_student = qt(1 - alpha/2, df-1 )
    precision <- exp_sd_error * t_student * npop

  } else {
    z_alpha <- round(qnorm(1 - alpha/2),2)
    precision <- exp_sd_error * z_alpha * npop
  }

  return(precision)
}

get_ni <- function(npop, sd, pi, alpha, desired_precision, n_min, ni_min){

  n <- calculate_sample_size_n(npop, sd, pi, alpha, desired_precision)
  n <- ceiling(n)

  if (is.nan(n) | n < n_min) {
    n <- n_min
  }

  ni <- round_ni(n*pi, n, n_min, ni_min)
  ni <- neyman_minimax(npop, sd, ni, alpha, desired_precision, n_min, ni_min)
  #ni <- round_ni(n*pi, n, n_min, ni_min)

  return(ni)

}

build_strata <- function(my_data_ref, data_column_name, dataframe, boundaries, estimation_method, allocation_method, p1, alpha, desired_precision, L, n_min, ni_min) {

  bins <- c(boundaries$xmin, tail(boundaries$xmax, n = 1))

  strata <- my_data_ref %>%
    mutate(!!sym(getStratumName()) := cut(!!sym(data_column_name), breaks = bins, labels = rownames(boundaries), include.lowest = TRUE)) %>%
    filter(!is.na(!!sym(getStratumName()) )) %>%
    group_by(!!sym(getStratumName()) ) %>%
    summarise(
      mean = sum((!!sym(data_column_name)))/n(),
      Sum_Squares = sum((!!sym(data_column_name))**2),
      npop = n(),
      sd = calculate_sd(estimation_method, Sum_Squares, npop, mean, p1),
      Total = sum((!!sym(data_column_name))),
      #sum_sqrtf = 1
    )

  if (any(strata$npop == 1) | nrow(strata) < L) {

    return(strata)
  }



  strata <- strata %>%
    ungroup() %>%
    mutate(
      xmin = boundaries$xmin,
      xmax = boundaries$xmax,
      pi = proportion_function(estimation_method, allocation_method, sd, npop, Total, nsample = NULL),
      ni = get_ni(npop, sd, pi, alpha, desired_precision, n_min, ni_min),
      pi = ni/sum(ni),
      SDE = npop*sd/sqrt(ni)*sqrt((npop -ni)/npop)
    ) %>%
    relocate(!!sym(getStratumName()) , xmin, xmax)

  #print(strata)
  return(strata)
}

build_best_n_dataframe <- function(strata, my_data_ref, data_column_name, iter, L, binwidth, number_of_bins, cut_off, bins, result, estimation_method, allocation_method, p1, alpha, desired_precision) {

  n <- sum(strata$ni)

  n_census <- my_data_ref %>%
    filter(!!sym(data_column_name) > cut_off) %>%
    summarise(N = n())
  n_census <- sum(n_census$N)

  precision <- strata %>%
  summarise(
    sd = sqrt((sum(npop**2 * sd**2/ni)/(sum(npop)**2)) - (sum(npop * sd**2)/(sum(npop)**2))),
    npop = sum(npop),
    nsample = sum(ni),
    precision = amount_precision_function(sd, alpha, nsample, npop)
  )

  precision <- precision$precision

  result <- rbind(result, data.frame(n_sample = n, n_census = n_census, n = n + n_census, precision = precision, binwidth = binwidth, number_of_bins = number_of_bins, cut_off = cut_off, bins = I(list(bins)), xmin = min(bins), xmax = max(bins), iter = iter, L = L, strata = I(list(strata))))

  return(result)

}


build_sub_strata <- function (my_data_ref, data_column_name, dataframe, boundaries, estimation_method, allocation_method, p1, alpha, desired_precision, L, n_min, ni_min) {

  sub_strata <- build_strata(my_data_ref, data_column_name, dataframe, boundaries, estimation_method, allocation_method, p1, alpha, desired_precision, L, n_min, ni_min)

  return(sub_strata)

}

best_cut_off <- function (my_data_ref, user_cutoff, my_data, data_column_name, strata_L, estimation_method, allocation_method, p1, alpha, desired_precision, best_n_dataframe, n_min, ni_min, break_n) {

  iter <- 0
  max_iter <- 20

  while(TRUE) {

    bins_param <- get_bins_param(my_data, data_column_name)
    binwidth <- bins_param$rounded_bin_width
    number_of_bins <- bins_param$bins


    binwidth <- binwidth
    number_of_bins <- number_of_bins

    #print(binwidth)
    #print(number_of_bins)


    while (number_of_bins > 500000){
      max_iter <- 0
      number_of_bins <- ceiling(number_of_bins/10)
      binwidth <- ceiling(binwidth*10)
      #binwidth <- ceiling(binwidth)
      #binwidth <- sum(my_data[[data_column_name]])/number_of_bins
      #rounded_bin_width <- round_to_human_interval(binwidth)
    }


    #print(binwidth)
    #print(number_of_bins)

    #binwidth <- binwidth/(strata_L/4)
    #number_of_bins <- number_of_bins*(strata_L/4)

    freq_distribution <- get_freq_distribution(my_data, data_column_name, binwidth, number_of_bins)
    my_data$Interval <- add_interval_original_data(my_data, data_column_name, freq_distribution)
    freq_distribution <- generate_freq_distribution(my_data, data_column_name, freq_distribution)
    freq_distribution <- add_square_root_freq(freq_distribution)

    if (iter > 0) {

      if (nrow(freq_distribution) == 0) {
        break
      }

      cut_off <- freq_distribution$xmax[nrow(freq_distribution) - iter]

      if (length(cut_off) < 1) {
        break
      }

      freq_distribution <- freq_distribution %>%
        filter(xmax < max(cut_off))

    }

    best_cum_freq <- get_best_cum_freq(allocation_method, freq_distribution, strata_L)
    #print(best_cum_freq)

    bins <- c(min(freq_distribution$xmin), best_cum_freq, min(max(freq_distribution$xmax), user_cutoff))
    #print(bins)


    if (any(duplicated(bins))) {
      bins <- interpolate_duplicates(bins)
    }

    # Create a dataframe from bins
    boundaries <- data.frame(xmin = bins)

    # Generate df_boundary dataframe
    boundaries <- boundaries %>%
      mutate(xmax = lead(xmin)) %>%
      filter(!is.na(xmax))


    # boundaries <- data.frame(
    #   xmin = c(0.000, 4376.88, 9248.74, 16904.52, 23864.32),
    #   xmax = c(4376.88,  9248.74, 16904.52, 23864.32, 35000.00)
    # )
    #
    # bins <- c(boundaries$xmin, tail(boundaries$xmax, n = 1))

    cut_off <- max(boundaries$xmax)


    my_data <- my_data %>%
      filter(!!sym(data_column_name) <= cut_off)

    sub_strata <- build_sub_strata(my_data, data_column_name, freq_distribution, boundaries, estimation_method, allocation_method, p1, alpha, desired_precision, strata_L, n_min, ni_min)


    if (any(sub_strata$npop == 1) | any(nrow(sub_strata) < strata_L)){
      #stop("Breaking loop: strata$npop contains 1.")
      print("break 2")
      break
    }

    bins <- c(sub_strata$xmin, tail(sub_strata$xmax, n = 1))

    best_n_dataframe <- build_best_n_dataframe(sub_strata, my_data_ref, data_column_name, iter, strata_L, binwidth, number_of_bins, cut_off, bins, best_n_dataframe, estimation_method, allocation_method, p1, alpha, desired_precision)

    iter <- iter + 1

    if (iter > max_iter) {
      break
    }

    check_n_var <- best_n_dataframe %>%
      filter(L == strata_L) %>%
      summarise(max_n = max(n), min_n = min(n))

    if (nrow(best_n_dataframe) > 1 & check_n_var$max_n > break_n*check_n_var$min_n) {
      print("break 3")
      break
    }

    if (sum(sub_strata$ni) < n_min) {
      print("break 4")
      break
    }

  }


  return(best_n_dataframe)

}


build_optimum_result <- function (best_n_dataframe) {

  optimum_result <- best_n_dataframe %>%
    # filter(n == min(n)) %>%
    # slice_min(n = 1, order_by = -n_census)
    filter(n == min(n)) %>%
    arrange(precision, desc(n_census)) %>%
    slice_head(n = 1)

  return(optimum_result)

}

build_final_strata <- function(best_n_dataframe, estimation_method, p1, my_data, data_column_name){

  print(getPrimaryKey())
  print("getPrimaryKey() here")

  cut_off <- best_n_dataframe$cut_off[[1]]
  iter <- best_n_dataframe$iter[[1]]
  sub_strata <- best_n_dataframe$strata[[1]]

  sub_totals <- sub_strata %>%
    summarise(
      !!sym(getStratumName()) := "Sub Total",
      #sd = sqrt(sum(npop*(npop - ni) * sd**2 / ni)/sum(npop)**2),
      sd = sqrt((sum(npop**2 * sd**2/ni)/(sum(npop)**2)) - (sum(npop * sd**2)/(sum(npop)**2))),
      SDE = sqrt(sum(SDE**2)),
      Total = sum(Total),
      xmin = min(xmin),
      xmax = max(xmax),
      Sum_Squares = sum(Sum_Squares),
      npop = sum(npop),
      ni = sum(ni),
      mean = Total/npop,
      pi = sum(pi),
    )

  sub_strata_censu <- my_data %>%
    filter((!!sym(data_column_name)) > cut_off) %>%
    mutate(!!sym(getStratumName()) := getHigherValues())

  #print(sub_strata_censu)

  sub_strata_censu <- sub_strata_censu %>%
    group_by(!!sym(getStratumName())) %>%
    summarise(
      !!sym(getStratumName()) := getHigherValues(),
      Total = sum((!!sym(data_column_name))),
      xmin = max(sub_strata$xmax),
      xmax = max(max(my_data[[data_column_name]])),
      #xmax = max(!!sym(data_column_name)),
      Sum_Squares = sum((!!sym(data_column_name))**2),
      npop = n(),
      ni = npop,
      mean = Total/ni,
      pi = NA
    ) %>%
    ungroup()

  if (length(sub_strata_censu$npop) > 0) {
    strata <- bind_rows(sub_strata, sub_totals, sub_strata_censu)
  } else {
    strata <- bind_rows(sub_strata)
  }

  total_line <- strata %>%
    filter(!(!!sym(getStratumName()) == "Sub Total")) %>%
    summarise(!!sym(getStratumName()) := "Total",
              #sd = sqrt(sum(npop*(npop - ni) * sd**2 / ni)/sum(npop)**2),
              #sd = sqrt((sum(npop**2 * sd**2/ni)/(sum(npop)**2)) - (sum(npop * sd**2)/(sum(npop)**2))),
              sd = sub_totals$sd,
              SDE = sub_totals$SDE,
              xmin = min(xmin),
              xmax = max(xmax),
              Sum_Squares = sum(Sum_Squares),
              npop = sum(npop),
              Total = sum(Total),
              #sum_sqrtf = sum(sum_sqrtf),
              pi = NA,
              ni = sum(ni),
              mean = Total/ni,
              )

  strata <- bind_rows(strata, total_line)

  # strata <- strata %>%
  #   select(-pi)

  return(strata)

}

build_final_strata_update <- function(dataframe, new_ni){

  strata <- dataframe %>%
    filter(grepl("^\\d+$", !!sym(getStratumName()))) %>%
    ungroup() %>%
    mutate(
      ni = new_ni,
      SDE = npop*sd/sqrt(ni)*sqrt((npop -ni)/npop)
    )

  #print(strata)

  sub_totals <- strata %>%
    summarise(
      !!sym(getStratumName()) := "Sub Total",
      #sd = sqrt(sum(npop*(npop - ni) * sd**2 / ni)/sum(npop)**2),
      sd = sqrt((sum(npop**2 * sd**2/ni)/(sum(npop)**2)) - (sum(npop * sd**2)/(sum(npop)**2))),
      SDE = sqrt(sum(SDE**2)),
      ni = sum(ni),
      Total = sum(Total),
      xmin = min(xmin),
      xmax = max(xmax),
      npop = sum(npop),
      mean = Total/npop
    )

  sub_strata_censu <- dataframe %>%
    filter(!!sym(getStratumName()) == getHigherValues())

  if (length(sub_strata_censu$npop) > 0) {
    strata <- bind_rows(strata, sub_totals, sub_strata_censu)
  } else {
    strata <- bind_rows(strata)
  }

  total_line <- strata %>%
    filter(!(!!sym(getStratumName()) == "Sub Total")) %>%
    summarise(!!sym(getStratumName()) := "Total",
              #sd = sqrt(sum(npop*(npop - ni) * sd**2 / ni)/sum(npop)**2),
              #sd = sqrt((sum(npop**2 * sd**2/ni)/(sum(npop)**2)) - (sum(npop * sd**2)/(sum(npop)**2))),
              sd = sub_totals$sd,
              SDE = sub_totals$SDE,
              xmin = min(xmin),
              xmax = max(xmax),
              #Sum_Squares = sum(Sum_Squares),
              npop = sum(npop),
              Total = sum(Total),
              #sum_sqrtf = sum(sum_sqrtf),
              #pi = NA,
              ni = sum(ni),
              mean = Total/ni,
    )

  strata <- bind_rows(strata, total_line)
  return(strata)

}

execute <- function (my_data, data_column_name, user_cutoff = desired_precision/2, estimation_method = "mean", allocation_method = "Neyman", L = seq(10,10), confidence = 0.95,
                     desired_precision = sum(my_data[data_column_name])*0.02 , n_min = 30, ni_min = 5, break_n = 3,
                     primaryKey = "primaryKey", booked_column_name = "Booked_Values", audit_column_name = "Audited_Values") {

  stratum_name <- "Stratum"
  higher_values <- "Census"

  columnNamesClass <- columnNames(primaryKey = primaryKey, booked_column_name = booked_column_name, audit_column_name = audit_column_name, data_column_name = data_column_name, stratum_name = stratum_name, higher_values = higher_values)

  assign("col_data", columnNamesClass, envir = .AuditSampling_env)
  print(.AuditSampling_env$col_data)

  # primaryKey <- getPrimaryKey()
  # print(primaryKey)
  # print("primaryKey in execute")
  # booked_column_name <- getBookedColumnName()
  # audit_column_name <- getAuditedColumnName()
  #

  #stratification_env$primaryKey <- getPrimaryKey()
  #stratification_env$booked_column_name <- getBookedColumnName()
  #stratification_env$audit_column_name <- getAuditedColumnName()


  print(paste("Desired precision:", desired_precision))
  print(paste("Cutoff :", user_cutoff))
  print(paste("Confidence level:", confidence))
  print(paste("Estimated mode:", estimation_method))
  print(paste("Allocation method:", allocation_method))
  print(paste("Global minimal sampling units:", n_min))
  print(paste("Minimal sampling per stratum:", ni_min))

  alpha <- (1 - confidence)

  list_cutoff <- data.frame(remove_mode = numeric(0),
                            user_cutoff = numeric(0))

  #list_cutoff <- rbind(list_cutoff, data.frame(remove_mode = "smooth", user_cutoff = NA))
  #list_cutoff <- rbind(list_cutoff, data.frame(remove_mode = "high", user_cutoff = NA))
  list_cutoff <- rbind(list_cutoff, data.frame(remove_mode = NA, user_cutoff = user_cutoff))
  #list_cutoff <- rbind(list_cutoff, data.frame(remove_mode = NA, user_cutoff = desired_precision/2))


  best_n_dataframe <- data.frame(n_sample = numeric(0),
                                 n_census = numeric(0),
                                 n = numeric(0),
                                 precision = numeric(0),
                                 binwidth = numeric(0),
                                 number_of_bins = numeric(0),
                                 cut_off = numeric(0),
                                 bins = numeric(0),
                                 xmin = numeric(0),
                                 xmax = numeric(0),
                                 iter = numeric(0),
                                 L = numeric(0),
                                 strata = numeric(0))
  for (strata in L) {

    print(paste("Processing stratum: ", strata))

    for (i in 1:nrow(list_cutoff)) {
      #user_cutoff = NULL, remove_mode = each
      remove_mode <- list_cutoff$remove_mode[i]
      #print(remove_mode)

      user_cutoff <- list_cutoff$user_cutoff[i]

      bounds <- build_bounds(my_data, data_column_name, user_cutoff = user_cutoff, remove_mode = remove_mode)
      result <- dataframe_cutoff(my_data, data_column_name, bounds)
      cutted_data <- result$dataframe

      user_cutoff <- bounds$upper_bound
      best_n_dataframe <- best_cut_off(my_data, user_cutoff, cutted_data, data_column_name, strata, estimation_method, allocation_method, p1, alpha, desired_precision, best_n_dataframe, n_min, ni_min, break_n)
      print(best_n_dataframe)
    }


  }

  print(best_n_dataframe)
  optimum_result <- build_optimum_result(best_n_dataframe)

  achieved_precision <- optimum_result$precision

  #print(optimum_result)
  #print(optimum_result$strata[[1]])
  strata <- build_final_strata(optimum_result, estimation_method, p1, my_data, data_column_name)
  #print(strata)
  #strata <- optimum_result$strata[[1]]

  strata <- data.frame(strata)
  optimum_result <- data.frame(optimum_result)
  all_iteration_strata <- data.frame(best_n_dataframe)

  return(list(achieved_precision = achieved_precision, sample_planning = strata, optimum_result = optimum_result, all_iteration_strata = all_iteration_strata))
}


updateDateBase <- function(dataframe, data_column_name, primaryKey, bins) {

  updatedDataframe <- dataframe %>%
    mutate(
      !!sym(getPrimaryKey()) := row_number(),
      !!sym(getStratumName()) := as.character(cut(!!sym(getDataColumnName()), breaks = bins, labels = FALSE, include.lowest = TRUE)),
      !!sym(getStratumName()) := ifelse(is.na(!!sym(getStratumName())), getHigherValues(), !!sym(getStratumName()))
    ) %>%
    relocate(!!sym(getStratumName()) ) %>%
    relocate(!!sym(getPrimaryKey())) %>%
    arrange(!!sym(getStratumName()), !!sym(data_column_name))

  return(updatedDataframe)

}

