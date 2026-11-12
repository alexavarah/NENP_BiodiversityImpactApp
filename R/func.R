# ------------------------------------------------------------------------------
# Functions used in data cleaning and preparation
# ------------------------------------------------------------------------------

# functions `cv_avg` to `gen.miss` are from
# https://github.com/AlistairMcNairSenior/Miss_SD_Sim/blob/main/R/func.R 
# The github link came from Appendices here https://alistairmcnairsenior.github.io/Miss_SD_Sim/
# from Nakagawa, S.et.al, 2023. A robust and readily implementable method for 
# the meta-analysis of response ratios with and without missing standard deviations. 
# Ecol Lett 26, 232–244. https://doi.org/10.32942/OSF.IO/7THX9.

# --------------------------
# cv_avg
# --------------------------
#' @title cv_avg
#' @description Calculates the weighted average CV^2 within a study and the weighted average CV^2 across a study
#' @param x Mean of an experimental group
#' @param sd Standard deviation of an experimental group
#' @param n The sample size of an experimental group
#' @param group Study, grouping or cluster variable one wishes to calculate the within and between weighted CV^2. In meta-analysis this will most likely be 'study'.
#' @param data The dataframe containing the mean, sd, n and grouping variables
#' @param label A character string specifying the label one wishes to attach to columns to identify the treatment. Otherwise, if not specified it will default to the variable name for x
#' @param sub_b A logical indicating whether the *between* study CV^2 (b_CV2) should be appended to the data only ('TRUE') or whether both *within* study CV^2 (w_CV2), mean sample size (n_mean) and *between* study CV^2 (b_CV2) should all be appended to the data only ('FALSE')
#' @param cv2 A logical indicating whether one should take the weighted average of CV2 or the weighted average of CV followed by squaring this average. Default to FALSE.
#' @example \dontrun{
#' # test data for cv_avg function
#' library(tidyverse)
#' set.seed(76)
# x1 = rnorm(16, 6, 1)
# x2 = rnorm(16, 6, 1)
# test_dat <- data.frame(stdy = rep(c(1,2,3,4), each = 4),x1 = x1,sd1 = exp(log(x1)*1.5 + rnorm(16, 0,
# sd = 0.10)),n1 = rpois(16, 15),x2 = x2,sd2 = exp(log(x2)*1.5 + rnorm(16, 0, sd = 0.10)),n2 = rpois(16, 15))
# rm(list = c("x1", "x2"))
# # Now generate some missing data
# t2 <- gen.miss(test_dat, "sd1", "sd2", 6)
# t2_cv <- cv_avg(x = x1, sd = sd1, n = n1, stdy, data = t2, sub_b = FALSE, cv2 = TRUE)
# t2_cv2 <- cv_avg(x2, sd2, n2, stdy, label = "2", data = t2_cv, sub_b = FALSE) 
# # Check calculations are correct. All match what is expected
# test <- t2_cv2 %>%  filter(stdy == "1")
# # Within
# mean(test$n1) # Matches 14.75
# mean(test$n2) # Matches 14
# # CV^2
# weighted.mean((test$sd1 / test$x1)^2, test$n1, na.rm = T)
# weighted.mean((test$sd2 / test$x2)^2, test$n2, na.rm = T)
# # mean(CV)^2
# t2_cv <- cv_avg(x = x1, sd = sd1, n = n1, stdy, data =  t2, sub_b = FALSE, cv2 = FALSE)
# weighted.mean((test$sd1 / test$x1), test$n1, na.rm = T)^2
# # Between
# wCV1 = unique(t2_cv2$w_CV2_x1)
# w_nt1 = c(59,58,58,50)
# weighted.mean(wCV1, w_nt1)
# wCV2 = unique(t2_cv2$w_CV2_2)
# w_nt2 = c(56, 56, 72, 63)
# weighted.mean(wCV2, w_nt2)
# }

cv_avg <- function(x, sd, n, group, data, label = NULL, sub_b = TRUE, cv2=FALSE){

  # Check if the name is specified or not. If not, then assign it the name of the mean, x, variable input in the function. https://stackoverflow.com/questions/60644445/converting-tidyeval-arguments-to-string
  if(is.null(label)){
    label <- purrr::map_chr(enquos(x), rlang::as_label)
  }

  # Calculate between study CV. Take weighted mean CV within study, and then 
  # take a weighted mean across studies of the within study CV. 
  # Weighted based on sample size and pooled sample size.
  b_grp_cv_data <- data %>%
    dplyr::group_by({{group}}) %>%
    dplyr::mutate(   w_CV2 = weighted_CV({{sd}}, {{x}}, {{n}}, cv2=cv2),
                     n_mean = mean({{n}}, na.rm = TRUE)) %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(b_CV2 = weighted.mean(w_CV2, n_mean, na.rm = TRUE), .keep = "used")

  # Make sure that label of the calculated columns is distinct from any other columns
  names(b_grp_cv_data) <- paste0(names(b_grp_cv_data), "_", label)

  # Append these calculated columns back to the original data and return the full dataset.
  if(sub_b){
    b_grp_cv_data <- b_grp_cv_data %>% dplyr::select(grep("b_", names(b_grp_cv_data)))
    dat_new <- cbind(data, b_grp_cv_data)
  } else {
    dat_new <- cbind(data, b_grp_cv_data)
  }

  return(data.frame(dat_new))
}

# --------------------------
# weighted_cv
# --------------------------
#' @title weighted_CV
#' @description Calculates the weighted average CV^2 or CV followed by squaring within a study and the weighted averages CV^2 across studies
#' @param sd Standard deviation of an experimental group
#' @param x Mean of an experimental group
#' @param n The sample size of an experimental group
#' @param cv2 Logical indicating whether the weighted average of CV^2 or CV should be taken (followed by squaring weighted average CV). 
#' Defaults to weighted average of CV.
#' if cv2 = TRUE → compute weighted mean of CV²
#' if cv2 = FALSE → compute weighted mean of CV and then square it.
weighted_CV <- function(sd, x, n, cv2=FALSE){
  if(cv2){
    weighted.mean(na_if((sd / x)^2, Inf), n, na.rm = TRUE)
  }else{
    weighted.mean(na_if((sd / x), Inf), n, na.rm = TRUE)^2
  }
}


# --------------------------
# get_est
# --------------------------
#' @title get_est
#' @description Extracts estimates from rma.mv and rma model objects
#' @param model The rma.mv model object

get_est <- function(model){
  est <- coef(model)
  ci.lb <- model$ci.lb
  ci.ub <- model$ci.ub
  se <- model$se
  return(data.frame(Est. = est, SE=se, "95% LCI" = ci.lb, "95% UCI" = ci.ub, check.names = FALSE))
}

# --------------------------
# lnrr_laj
# --------------------------
#' @title lnrr_laj
#' @description Calculates log response ratio based on Taylor expansion from Lajeunesse 2011
#' @param m1 Mean of treatment group 1
#' @param m2 Mean of treatment group 2
#' @param cv1_2 Coefficient of variation squared (CV^2) for treatment group 1
#' @param cv2_2 Coefficient of variation squared (CV^2) for treatment group 2
#' @param n1 Sample size for treatment group 1
#' @param n2 Sample size for treatment group 2
#' @param taylor A logical indicating whether to calculate point estimate with Taylor expansion.
#'
lnrr_laj <- function(m1, m2, cv1_2, cv2_2, n1, n2, taylor=TRUE){
  if(taylor){
    log(m1 / m2) + 0.5*((cv1_2 / n1) - (cv2_2 / n2))
  } else {
    log(m1 / m2)
  }
}

# --------------------------
# v_lnrr_laj
# --------------------------
#' @title v_lnrr_laj
#' @description Calculates the sampling variance for log response ratio based on second order Taylor expansion proposed by Lajeunesse 2011
#' @param cv1_2 Coefficient of variation squared (CV^2) for treatment group 1
#' @param cv2_2 Coefficient of variation squared (CV^2) for treatment group 2
#' @param n1 Sample size for treatment group 1
#' @param n2 Sample size for treatment group 2
#' @param taylor A logical indicating whether to calculate point estimate with Taylor expansion.
v_lnrr_laj <- function(cv1_2, cv2_2, n1, n2,  taylor=TRUE){
  if(taylor){
    # this is equivalent to eq 5 in Nakagawa 2023 where CVs are directly  
    # calculated from SD, and eq 7 where CVs are weighted-avg CV.
  ((cv1_2) / n1) + ((cv2_2) / n2) +
    ((cv1_2)^2 / (2*n1^2)) + ((cv2_2)^2 / (2*n2^2))
  } else {
    # this is eq 2 in Nakagawa 2023
    ((cv1_2) / n1) + ((cv2_2) / n2)
  }
}



# --------------------------
# gen.miss
# --------------------------
#' @title gen.miss
#' @description Generates random missing data in two columns of a meta-analytic dataset
#' @param data The dataframe containing the mean, sd, n and grouping variables
#' @param missVar A character string specifying the first column one wishes to have missing data. Example, "sd1" column.
#' @param missCol2 A character string specifying the second column one wishes to have missing data. Example, "sd2" column.
#' @param n_miss The number of missing data (NA) one wishes to introduce to the missVar and missCol2
gen.miss <- function(data, missVar, missCol2, n_miss){
  data[sample(rownames(data), n_miss), missVar] <- NA
  data[is.na(data[,missVar]), missCol2] <- NA
  return(data)
}

# --------------------------
# check_and_remove_missing_n
# --------------------------
#' @title check_and_remove_missing_n
#' @description Checks for missing sample size values in n_i and n_c columns of a dataframe,
#' removes rows with missing values, saves them to a CSV file, and assigns the cleaned dataframe to a new variable in the global environment.
#' @param df The dataframe to be checked for missing values in n_i and n_c
check_and_remove_missing_n <- function(df) {
  
  required_cols <- c("n_i", "n_c", "study_ID", "effect_size_ID")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Find rows with NA in either column
  missing_rows <- df %>%
    filter(is.na(n_i) | is.na(n_c)) %>%
    select(study_ID, effect_size_ID, n_i, n_c)
  
  # Cleaned dataset
  cleaned_data <- df %>%
    filter(!is.na(n_i) & !is.na(n_c))
  
  # Only act if rows were removed
  if (nrow(missing_rows) > 0) {
    
    # Save removed rows to CSV
    write.csv(
      missing_rows,
      file = "../output/missing_n_removed.csv",
      row.names = FALSE
    )
    
    # Create new dataframe 'extracted' in the global environment
    assign("extracted", cleaned_data, envir = .GlobalEnv)
    
    message(nrow(missing_rows), 
            " rows removed. 
            - Saved to ../output/missing_n_removed.csv
            - Cleaned data assigned to dataframe 'extracted'.")
    
  } else {
    message("No NA values found in n_i or n_c. No files written and no new dataframe created.")
  }
}

# --------------------------
# check_baci_before_year
# --------------------------
#' @title check_baci_before_year
#' @description Function to ensure that, where there are multiple 'before' years
#' in a BACI (Before-After-Control-Impact) study, only the earliest sampling year 
#' is retained. It's grouped by user-specified variables.
#' The user-specified variables account for the fact that, within a study, 
#' there may be effect sizes for several different taxa etc.
check_baci_before_year <- function(df, group_cols) {
  
  # Step 1: isolate BACI "before" rows
  before_rows <- df %>%
    dplyr::filter(
      exptl_design == "BACI",
      BACI_before_after_year == "before"
    )
  
  # Step 2: summarise before-year structure
  # check_tbl <- before_rows %>%
  #   dplyr::group_by(
  #     dplyr::across(all_of(group_cols)),
  #     .drop = TRUE) %>%
  #   dplyr::summarise(
  #     n_years = dplyr::n_distinct(earliest_sampling_date_year),
  #     years = paste(sort(unique(earliest_sampling_date_year)), collapse = ", "),
  #     earliest_before_year = min(earliest_sampling_date_year, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  check_tbl <- before_rows %>%
    dplyr::group_by(
      dplyr::across(all_of(group_cols)),
      .drop = TRUE
    ) %>%
    dplyr::summarise(
      n_years = dplyr::n_distinct(earliest_sampling_date_year),
      years = paste(sort(unique(earliest_sampling_date_year)), collapse = ", "),
      earliest_before_year = {
        yrs <- earliest_sampling_date_year[!is.na(earliest_sampling_date_year)]
        
        if (length(yrs) == 0) {
          NA_integer_
        } else {
          min(yrs)
        }
      },
      .groups = "drop"
    )
  
  # Step 3: identify groups with multiple BEFORE years (message only)
  problem_groups <- check_tbl %>%
    dplyr::filter(n_years > 1)
  
  if (nrow(problem_groups) > 0) {
    message(
      "Note: Some BACI groups had multiple 'before' years. ",
      "The earliest year was retained and later 'before' rows were removed.\n\n",
      "Affected groups:\n",
      paste(capture.output(print(problem_groups)), collapse = "\n")
    )
  }
  
  # Step 4: create lookup of earliest BEFORE year
  before_lookup <- check_tbl %>%
    dplyr::select(all_of(group_cols), before_year = earliest_before_year)
  
  # Step 5: join back to full dataset
  df_out <- df %>%
    dplyr::left_join(before_lookup, by = group_cols)
  
  # Step 6: remove later BEFORE rows
  df_out <- df_out %>%
    dplyr::filter(
      !(exptl_design == "BACI" &
          BACI_before_after_year == "before" &
          earliest_sampling_date_year > before_year)
    )
  
  # Step 7: check for BACI groups missing ANY 'before' year
  all_baci_groups <- df %>%
    dplyr::filter(exptl_design == "BACI") %>%
    dplyr::distinct(dplyr::across(all_of(group_cols)))
  
  groups_with_before <- before_lookup %>%
    dplyr::distinct(dplyr::across(all_of(group_cols)))
  
  missing_before <- dplyr::anti_join(
    all_baci_groups,
    groups_with_before,
    by = group_cols
  )
  
  if (nrow(missing_before) > 0) {
    stop(
      "Error: Some BACI groups have no 'before' year.\n\n",
      "Affected groups:\n",
      paste(capture.output(print(missing_before)), collapse = "\n")
    )
  }
  
  return(df_out)
}


# --------------------------
# baci_lrr
# --------------------------
#' @title baci_lrr
#' @description Function to calculate the log-response ratio of BACI studies
#' @param mean_i_Before The mean value of the intervention treatment in the 'before' year
#' @param mean_i_After The mean value of the intervention treatment in the 'after' year
#' @param mean_c_Before The mean value of the control treatment in the 'before
#' @param mean_c_After The mean value of the control treatment in the 'after' year
baci_lrr <- function(mean_i_After, mean_c_After, mean_i_Before, mean_c_Before) {
  log( (mean_i_After / mean_c_After) / 
         (mean_i_Before / mean_c_Before) )
}

# --------------------------
# baci_var
# --------------------------
#' @title baci_var
#' @description Function to calculate the variance of the log-response ratio for BACI design studies using the Delta method
#' @param mean_i_Before The mean value of the intervention treatment in the 'before' year
#' @param mean_i_After The mean value of the intervention treatment in the 'after' year
#' @param mean_c_Before The mean value of the control treatment in the 'before
#' @param mean_c_After The mean value of the control treatment in the 'after' year
#' @param cv2_i_Before The coefficient of variation squared (CV²) of the intervention treatment in the 'before' year
#' @param cv2_i_After The CV² of the intervention treatment in the 'after' year
#' @param cv2_c_Before The CV² of the control treatment in the 'before' year
#' @param cv2_c_After The CV² of the control treatment in the 'after' year
#' @param n_i The sample size of the intervention treatment
#' @param n_c The sample size of the control treatment
#' @param r_i The correlation between the 'before' and 'after' measurements for the intervention treatment (default is 0.5)
#' @param r_c The correlation between the 'before' and 'after' measurements for the control treatment (default is 0.5)
baci_var <- function(
    cv2_i_Before, cv2_i_After, # CV² of interventions/treatments
    cv2_c_Before, cv2_c_After, # CV² of controls
    n_i_Before, n_i_After,     # sample size of interventions
    n_c_Before, n_c_After,     # sample size of controls
    r_i = 0.5, r_c = 0.5       # correlations (account for paired sampling design)
    # assume 0.5 by default, but can be varied in a sensitivity analysis
    ) {
  
  # Intervention term
  var_i <- (cv2_i_After / n_i_After) +
           (cv2_i_Before / n_i_Before) -
           2 * r_i * sqrt(cv2_i_After * cv2_i_Before / 
                            (n_i_After * n_i_Before))
  
  # Control term
  var_c <- (cv2_c_After / n_c_After) +
           (cv2_c_Before / n_c_Before) -
           2 * r_c * sqrt(cv2_c_After * cv2_c_Before / 
                            (n_c_After * n_c_Before))
  
  # Total variance
  var_i + var_c
}

# NB TO CHECK: the formula below is how I've calculated var of lnRR (Eq 7 in SI).
# I don't think this is currently implemented in the baci_var function
#((cv_i^2) / n_i) + ((cv_c^2) / n_c) +
#  ((cv_i^2)^2 / (2* n_i^2)) + ((cv_c^2)^2 / (2* n_c^2))


# --------------------------
# baci_lnrr_var
# --------------------------
#' @title baci_lnrr_var
#' @description Function that uses the functions 'baci_lrr', 'baci_var' and 'add_baci_before_year' to add columns to a dataframe containing calculated lnRR and variance for BACI studies
#' @param df The dataframe containing the mean, CV², n and grouping variables along with the experimental design column (exptl_design) and earliest sampling year column (earliest_sampling_date_year)
#' @param group_cols A character vector of column names to group by when calculating the earliest sampling year for a BACI study. 
#' Grouping columns should include all columns that define a unique BACI group, e.g., study_ID, taxon_scientific, life_stage, sampling_method, country, etc.

baci_lnrr_var <- function(df, group_cols) {
  
  # ----------------------
  # Process BACI studies
  # ----------------------
  baci_studies <- df %>%
    dplyr::filter(exptl_design == "BACI") %>%
    dplyr::group_by(dplyr::across(all_of(group_cols)))
  
  # ----------------------
  # Extract BEFORE values
  # ----------------------
  baci_studies_efs <- baci_studies %>%
    dplyr::mutate(
      
      mean_i_before = mean_i[earliest_sampling_date_year == before_year][1],
      mean_c_before = mean_c[earliest_sampling_date_year == before_year][1],
      
      cv2_i_before  = cv2_expt_new[earliest_sampling_date_year == before_year][1],
      cv2_c_before  = cv2_cont_new[earliest_sampling_date_year == before_year][1],
      
      n_i_before    = n_i[earliest_sampling_date_year == before_year][1],
      n_c_before    = n_c[earliest_sampling_date_year == before_year][1]
      
    ) %>%
    dplyr::ungroup() %>%
    
    # ----------------------
  # Calculate effect sizes
  # ----------------------
  dplyr::mutate(
    
    baci_lnRR = baci_lrr(
      mean_i_After  = mean_i,
      mean_c_After  = mean_c,
      mean_i_Before = mean_i_before,
      mean_c_Before = mean_c_before
    ),
    
    baci_var = baci_var(
      cv2_i_Before = cv2_i_before,
      cv2_i_After  = cv2_expt_new,
      cv2_c_Before = cv2_c_before,
      cv2_c_After  = cv2_cont_new,
      n_i_Before   = n_i_before,
      n_i_After    = n_i,
      n_c_Before   = n_c_before,
      n_c_After    = n_c
    )
  ) %>%
    
    # ----------------------
  # Remove BEFORE rows
  # ----------------------
  dplyr::filter(
    earliest_sampling_date_year != before_year
  )
  
  # --------------------------------
  # Keep non-BACI studies unchanged
  # --------------------------------
  others <- df %>%
    dplyr::filter(exptl_design != "BACI")
  
  # ------------------------
  # Return combined dataset
  # ------------------------
  dplyr::bind_rows(baci_studies_efs, others)
}



# --------------------------
# format_p
# --------------------------
#' @title format_p
#' @description Function that formats p-values into a more readable format, with thresholds for significance. For example, p-values less than 0.05 will be formatted as "<0.05".
#' @param p This must first be created from an anova object e.g., p <- anova_object$pval
format_p <- function(p) {
  if (p < 0.0001) {
    "<.0001"
  } else if (p < 0.001) {
    "<0.001"
  } else if (p < 0.01) {
    "<0.01"
  } else if (p < 0.05) {
    "<0.05"
  } else {
    sprintf("%.2f", round(p, 2))
  }
}

# -------------------------------
# Convert text months to numeric
# -------------------------------
#' @title convert_month 
#' @description Function that converts text values (e.g., "March", "3") into numeric months
convert_month <- function(x) {
  x <- trimws(as.character(x)) # convert all to character first
  
  case_when(
    is.na(x) ~ NA_integer_, # if value is missing, return integer NA
    grepl("^\\d+$", x) ~ as.integer(x), # if the value consists only of digits, convert to integer
    TRUE ~ match(tolower(x), tolower(month.name)) # otherwise assume e.g. "March" and match to month name to return integer month
  )
}

# -------------------------------
# Convert to sentence case
# -------------------------------
#' @title to_sentence_case
#' #' @description Function that converts strings to sentence case
to_sentence_case <- function(x) {
  
  ifelse(
    is.na(x),
    NA_character_,
    paste0(
      toupper(substr(x, 1, 1)),
      substring(x, 2)
    )
  )
  
}