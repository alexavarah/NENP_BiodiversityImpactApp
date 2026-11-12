# ==============================================================================
# Create analysis-ready dataset
# ==============================================================================

# ============================================
# Load packages and functions
# ============================================
library(pacman) # for p_load function
pacman::p_load(
  tidyverse, # for data manipulation and more
  readxl, # for reading excel files
  here, # for reproducible file paths
  lubridate # for fixing months
)

source("R/func.R")

# ============================================
# Load and tidy data
# ============================================

extracted <- read.csv(
  "data/biodiversity_data.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "-999")) |> 
  
  # remove pvalue (don't need it as using different method for missing errors)
  select(-pvalue) |> 
  
  # make a unique identifier for each effect size (aka observation)
  dplyr::mutate(es_ID = row_number()) |> # this is same as effect_size_ID but fail-safe
  
  # rename some columns
  dplyr::rename(mean_i = i_bd_mean,
                mean_c = c_bd_mean, 
                error_i = i_bd_error_value, 
                error_c = c_bd_error_value, 
                n_i = i_sample_size,
                n_c = c_sample_size,
                i_ID = intervention_treatment_ID,
                c_ID = control_treatment_ID) %>% 
  
  # Fill empty cells in shared-data variable
  dplyr::mutate(
    dataset_ID = if_else(
      is.na(dataset_ID),
      study_ID,
      dataset_ID
    )
  ) %>%
  
  # fix any missing life stages, functional groups, sampling methods, metrics...
  dplyr::mutate(
    across(
      c(taxon_common, life_stage, functional_group, sampling_method,
        intervention_level_1, intervention_level_2, intervention_level_3,
        control_type_level_1, control_type_level_2, bd_metric_type, bd_metric,
        effect_size_or_mean, bd_error_metric, scale_of_intervention, 
        intervention_proximity_weight, data_source, exptl_design, evidence_type, 
        literature_type, country, climate, geographic_proximity_weight,
        land_use_context_ur, ease_of_extraction),
      ~ ifelse(.x == "" | is.na(.x), "not entered by data extractor", .x)
    )) %>%
  
  # remove numeric code from exptl_design and standardise names
  dplyr::mutate(
    exptl_design = exptl_design %>%
      stringr::str_remove(" \\(\\d+\\)$") %>%
      dplyr::recode(
        "Randomised control trial" = "RCT",
        "After-Control-Impact" = "ACI",
        "Control-Impact" = "CI",
        "Reference-After" = "RA",
        "Chronosequence" = "Chron",
        "Before-After" = "BA",
        "After" = "A",
        "Time series" = "TS"
      )
  ) %>%
  
  # fix column classes
  dplyr::mutate(across(c(
    target_intervention, study_ID, effect_size_ID, 
    i_ID, c_ID, 
    taxon_common, life_stage, functional_group, sampling_method,
    intervention_level_1, intervention_level_2, intervention_level_3,
    control_type_level_1, control_type_level_2, bd_metric_type, bd_metric,
    effect_size_or_mean, bd_error_metric, scale_of_intervention, 
    intervention_proximity_weight, data_source, exptl_design, evidence_type, 
    literature_type, climate, geographic_proximity_weight,
    land_use_context_ur, ease_of_extraction, dataset_ID),
    as.factor)) %>%
  dplyr::mutate(across(c(
    mean_i, mean_c, 
    error_i, error_c, 
    n_i, n_c,
    i_bd_error_range_l, i_bd_error_range_u, 
    c_bd_error_range_l, c_bd_error_range_u,
    intervention_size_sqm, 
    i_months_since_establishment, c_months_since_establishment, 
    n_sampling_months_per_year, 
    latitude, longitude),
    as.numeric)) %>%
  dplyr::mutate(across(c(
    n_i, n_c, i_n_obs, c_n_obs, 
    i_planting_month, i_planting_year, c_planting_month, c_planting_year,
    earliest_sampling_date_day, earliest_sampling_date_month, earliest_sampling_date_year,
    latest_sampling_date_day, latest_sampling_date_month, latest_sampling_date_year),
    as.integer)) %>%
  
  # Collapse some categories and create new variables
  dplyr::mutate(
    
    # Make new variable that has just one 'beetles' category (taxon_common has 
    # beetles - ground beetles', and 'beetles - other beetles')
    tax_com = str_replace(taxon_common, "beetles.*", "beetles"),
    
    # Collapse taxa to deal with sparse categories and 
    # to allow learning of taxonomic groupings in schools
    tax = forcats::fct_collapse(
      tax_com,
      "insects" = c(
        "bees", "wasps", "ants", "beetles", 
        "flies - hoverflies", "flies - other flies", 
        "bugs", "orthoptera", "butterflies", "moths", "aphids"
      ),
      "arthropods" = c(
        "bees", "wasps", "ants", "beetles", "flies - hoverflies", 
        "flies - other flies", "bugs", "orthoptera", "butterflies", "moths", 
        "aphids", "spiders", "mites", "arthropods"),
      "invertebrates" = c(
        "bees", "wasps", "ants", "beetles", "flies - hoverflies", 
        "flies - other flies", "bugs", "orthoptera", "butterflies", "moths", 
        "aphids", "spiders", "mites", "arthropods", "worms", "nematodes", 
        "invertebrates"),
      "mammals"  = c("mammals", "bats"),
      "microbes" = c("bacteria", "fungi"),
    ),
    
    # Re-code mixed functional groups
    functional_group = case_when(
      taxon_common %in% c("butterflies", "hoverflies") ~ "pollinators",
      taxon_common == "wasps" ~ "natural enemies",
      TRUE ~ as.character(functional_group)
    ),
    functional_group = factor(functional_group),
    # ensure 'pollinators' is intercept
    functional_group = forcats::fct_relevel(functional_group, "pollinators"),
    functional_group = droplevels(functional_group),
    
    # Combine sparse functional group categories
    functional_group = forcats::fct_collapse(
      functional_group,
      "natural enemies" = c("natural enemies", "parasites")
    ),
    
    # Simplify biodiversity metric
    bdmetric = forcats::fct_collapse(
      bd_metric_type, 
      "diversity" = c("richness", "diversity")
    )
    
  ) %>% 
  
  dplyr::relocate(tax_com, .after = taxon_common) %>% 
  
  # fix column classes
  dplyr::mutate(across(c(
    bdmetric, tax_com, tax, functional_group
  ),
  as.factor)
  ) %>% 
  
  # ensure data_subset is all the same case
  dplyr::mutate(data_subset = toupper(data_subset))



# ============================================
# Create function to process data
# ============================================  
process_intervention <- function(extracted) {
  
  # ============================================
  # Collapse some categories and create new variables
  # ============================================
  
  
  # ============================================
  # Deal with sampling months entered as text
  # ============================================
  extracted_month <- extracted |>
    mutate(
      i_planting_month = convert_month(i_planting_month),
      c_planting_month = convert_month(c_planting_month),
      earliest_sampling_date_month = convert_month(earliest_sampling_date_month),
      latest_sampling_date_month = convert_month(latest_sampling_date_month)
    )

    # check no months greater than 12
    # extracted_month |>
    #   filter(
    #     !is.na(latest_sampling_date_month),
    #     suppressWarnings(!is.na(as.integer(latest_sampling_date_month))),
    #     as.integer(latest_sampling_date_month) == 16
    #   ) |>
    #   select(effect_size_ID, latest_sampling_date_month)
  

  # ============================================
  # Calculate intervention age 
  # ============================================
  # round to nearest whole month
  extracted_age <- extracted_month %>%
    dplyr::mutate(
      
      # Convert planting date (day never extracted, use 1st of month)
      planting_date = case_when(
        !is.na(i_planting_year) & !is.na(i_planting_month) ~
          make_date(
            year = i_planting_year,
            month = i_planting_month,
            day = 1
          ),
        TRUE ~ as.Date(NA)
      ),
      
      # N.B. lots of effect sizes have no planting months. In that case, use
      # months since establishment (converted to years). If that's blank,
      # use years for everything.
      
      # Convert first sampling date (use actual day if available, otherwise 1st)
      first_sampling_date = case_when(
        !is.na(earliest_sampling_date_year) &
          !is.na(earliest_sampling_date_month) &
          !is.na(earliest_sampling_date_day) ~
          make_date(
            year = earliest_sampling_date_year,
            month = earliest_sampling_date_month,
            day = earliest_sampling_date_day
          ),
        
        !is.na(earliest_sampling_date_year) &
          !is.na(earliest_sampling_date_month) &
          is.na(earliest_sampling_date_day) ~
          make_date(
            year = earliest_sampling_date_year,
            month = earliest_sampling_date_month,
            day = 1
          ),
        
        TRUE ~ as.Date(NA)
      ),
      
      # Convert last sampling date (use actual day if available, otherwise 1st)
      last_sampling_date = case_when(
        !is.na(latest_sampling_date_year) &
          !is.na(latest_sampling_date_month) &
          !is.na(latest_sampling_date_day) ~
          make_date(
            year = latest_sampling_date_year,
            month = latest_sampling_date_month,
            day = latest_sampling_date_day
          ),
        
        !is.na(latest_sampling_date_year) &
          !is.na(latest_sampling_date_month) &
          is.na(latest_sampling_date_day) ~
          make_date(
            year = latest_sampling_date_year,
            month = latest_sampling_date_month,
            day = 1
          ),
        
        TRUE ~ as.Date(NA)
      ),
      
      # Compute difference in years when both dates are available
      diff_yrs = case_when(
        !is.na(planting_date) & !is.na(last_sampling_date) ~
          as.numeric(
            difftime(
              last_sampling_date,
              planting_date,
              units = "days"
            )
          ) / 365.25,
        
        TRUE ~ NA_real_
      ),
      
      # MAIN LOGIC (IN PRIORITY ORDER) 
      i_age_yrs = round(
        case_when(
          
          # 1) If planting and sampling dates can be calculated,
          # use their difference
          !is.na(diff_yrs) ~ diff_yrs,
          
          # 2) Otherwise use months since establishment
          !is.na(i_months_since_establishment) ~
            i_months_since_establishment / 12,
          
          # 3) Otherwise use year difference if available
          !is.na(i_planting_year) &
            !is.na(latest_sampling_date_year) ~
            (latest_sampling_date_year - i_planting_year),
          
          # 4) Everything missing
          TRUE ~ NA_real_
        ),
        2
      )
    ) |> 
    dplyr::select(-diff_yrs)


  # ============================================
  # Create metric of internal validity 
  # ============================================
  # aka study reliability
  extracted_risk <- extracted_age %>%
    dplyr::mutate(
      
      # Use true sample sizes when available, non-zero, and smaller than the reported sample sizes
      effective_n_i = dplyr::case_when(
        !is.na(true_n_i) & true_n_i != 0 & true_n_i < n_i ~ true_n_i,
        TRUE ~ n_i
      ),
      
      effective_n_c = dplyr::case_when(
        !is.na(true_n_c) & true_n_c != 0 & true_n_c < n_c ~ true_n_c,
        TRUE ~ n_c
      ),
      
      risk_sample_size = if_else(
        effective_n_i >= 5 & effective_n_c >= 5,
        "low",
        "high"
      ),
      
      # equal sampling
      risk_control_intervention_balance = if_else(
        effective_n_i == effective_n_c,
        "low",
        "high"
      ),
      
      # exptl design
      risk_experimental_design = if_else(
        exptl_design %in% c("BACI", "RCT"),
        "low",
        "high"
      ),
      
      # age
      risk_time_since_establishment = if_else(
        i_age_yrs >= 1,
        "low",
        "high"
      ),
      
      # Overall risk of bias
      risk_overall = factor(
        dplyr::if_else(
          rowSums(
            dplyr::across(
              c(
                risk_sample_size,
                risk_control_intervention_balance,
                risk_experimental_design,
                risk_time_since_establishment
              ),
              ~ .x == "high"
            )
          ) >= 2,
          "high",
          "low",
          missing = "high"
        ),
        levels = c("low", "high")
      ) 
    ) %>%
    dplyr::select(
      -risk_sample_size,
      -risk_control_intervention_balance,
      -risk_experimental_design,
      -risk_time_since_establishment
    )
  
  
  # ============================================
  # Change all zero errors to NA 
  # ============================================
  extracted_1 <- extracted_risk %>%
    dplyr::mutate(
      error_i = ifelse(error_i == 0, NA, error_i),
      error_c = ifelse(error_c == 0, NA, error_c)
    )
  
  
  # ============================================
  # Calculate SD
  # ============================================
  extracted_2 <- extracted_1 %>%
    dplyr::mutate(
      
      # Where error is SD, use that; where error is SE, calculate SD
      # ...for intervention
      sd_i = case_when(
        bd_error_metric == "standard deviation" ~ error_i,
        bd_error_metric == "standard error" ~ error_i * sqrt(n_i)
      ),
      
      # ...for control
      sd_c = case_when(
        bd_error_metric == "standard deviation" ~ error_c,
        bd_error_metric == "standard error" ~ error_c * sqrt(n_c)
      )
    )
  
  
  # ============================================
  # Remove any data subsets
  # ============================================
  extracted_3 <- extracted_2 |> 
    dplyr::filter(data_subset != "Y" | is.na(data_subset)) 
  # keep rows that aren't Y (need to tell it to keep NAs, too)
  
  # ============================================
  # Remove large interventions
  # ============================================
  extracted_4 <- extracted_3 |> 
    # Remove intervention scales = 4 and 5
    dplyr::filter(!scale_of_intervention %in% c("large (4)", "very large (5)")) |> 
    # Drop unused factor levels
    droplevels()
  
  # ============================================
  # Calculate CV for all rows with SDs
  # ============================================
  # note missing data will get NA
  extracted_5 <- extracted_4 %>% 
    # Divide SD by the mean and 
    # replace infinite (e.g., when mean = 0) with NA using na_if()
    dplyr::mutate(cv_i = na_if(sd_i / mean_i, Inf),
                  cv_c = na_if(sd_c / mean_c, Inf)
    )
  
  
  # ============================================
  # BACI 'before' year
  # ============================================
  # Identify BACI 'before' year and check there is only one 'before' year
  # Define grouping variables to be used in the BACI helper
  group_cols <- c(
    "study_ID", "taxon_common", "taxon_other", "taxon_scientific", "life_stage",
    "sampling_method", "intervention_level_3", "control_type_level_2", "bd_metric",
    "scale_of_intervention", "country", "land_use_context_ur"
  )
  
  # Check there in only one BACI before year, and where this is true, 
  # remove all but the first before year
  extracted_5_beforeyear <- extracted_5 |>
    check_baci_before_year(group_cols = group_cols)
  
  
  # ============================================
  # Add tiny value to zero means
  # ============================================
  # find the smallest mean value and calculate half of it
  add_zero <- extracted_5_beforeyear |>
    summarise(add_zero = min(c(mean_i[mean_i > 0], mean_c[mean_c > 0]), na.rm = TRUE) / 2) |>
    pull(add_zero)
  
  # add this value to all zero means
  extracted_6 <- extracted_5_beforeyear |>
    dplyr::mutate(
      mean_i = if_else(mean_i == 0, mean_i + add_zero, mean_i),
      mean_c = if_else(mean_c == 0, mean_c + add_zero, mean_c)
    )
  
  # ============================================
  # Calculate weighted average CV 
  # ============================================
  # Use Missing-cases Method from Nakagawa 2023 doi 10.1111/ele.14144
  # Calculate between-study weighted average CV - this will replace missing values
  # Use default setting which computes the weighted mean of CV, then squares it
  extracted_7 <- cv_avg(x = mean_i, sd = sd_i,
                        n = n_i, group = study_ID, label = "i",
                        data = extracted_6)
  
  extracted_8 <- cv_avg(x = mean_c, sd = sd_c,
                        n = n_c, group = study_ID, label = "c",
                        data = extracted_7)
  
  
  # ============================================
  # For non-BACI studies: Calculate lnRR and its sampling variance
  # ============================================
  # apply Missing-cases Method
  
  # 1. Replace missing CVs with between-study weighted averages, and square them
  extracted_9 <- extracted_8 |>
    dplyr::mutate(
      # For the experimental group:
      # If the observed CV is missing, use the between-study weighted CV² (b_CV2_i);
      # otherwise use squared observed CV.
      cv2_expt_new = if_else(
        is.na(cv_i),
        b_CV2_i,    # From cv_avg() — study-level weighted CV² for interventions
        cv_i^2      # Use observed CV² if available
      ),
      
      # For the control group:
      # Same logic: replace missing CVs with between-study weighted CV² (b_CV2_c),
      # otherwise, square the observed CV value.
      cv2_cont_new = if_else(
        is.na(cv_c),
        b_CV2_c,    # From cv_avg() — study-level weighted CV² for controls
        cv_c^2      # Use observed CV² if available
      )
    )
  
  # 2. Calculate lnRR and its sampling variance based on presence/absence of SDs
  # Calculate yi and vi, called lnrr_laj & v_lnrr_laj, respectively.
  # This uses either the between individual CV^2 when missing or normal CV^2 when not missing.
  extracted_10 <- extracted_9 |>
    dplyr::mutate(
      lnrr_laj = lnrr_laj(
        m1 = mean_i, # Intervention
        m2 = mean_c, # Control
        cv1_2 = cv2_expt_new, # Intervention
        cv2_2 = cv2_cont_new, # Control
        n1 = n_i, # Intervention
        n2 = n_c, # Control
        taylor = TRUE # use Taylor correction as our studies have small n
      ), 
      
      v_lnrr_laj = v_lnrr_laj(
        cv1_2 = cv2_expt_new,
        n1 = n_i, 
        cv2_2 = cv2_cont_new,
        n2 = n_c,
        taylor = TRUE # use Taylor correction as our studies have small n
      ) 
    )
  
  # ============================================
  # For BACI studies: Calculate lnRR and its sampling variance
  # ============================================
  # Apply Missing-cases Method
  # Calculate lnRR for BACI studies following Sievers et al. (2017) and Conner et al. (2015)
  # lnRR = ln((IA / CA) / (IB / CB)) 
  # Use the delta method to calculate the variance of the log response ratio
  # Calculate lnRR for each combination of the grouping variables, relative to the before year
  
  # Apply the function to calculate lnrr and variance of lnrr for BACI studies
  extracted_11 <- baci_lnrr_var(extracted_10, group_cols)
  
  # Create column with the correct lnrr and v_lnrr for each exptl design type
  extracted_12 <- extracted_11 |>
    dplyr::mutate(
      lnrr   = if_else(exptl_design == "BACI", baci_lnRR, lnrr_laj),
      v_lnrr = if_else(exptl_design == "BACI", baci_var,  v_lnrr_laj)
    )

  return(extracted_12)
}

# ============================================
# Run the function for each intervention
# ============================================

# The calculations of missing error variance should be done for each 
# intervention separately.

interventions <- unique(extracted$target_intervention)
# At the moment the dataset only contains flower and grass interventions.
# When I have more interventions that don't all need to be processed the
# same as flower and grass interventions, I could to this instead:
# interventions <- c(
#   "flower_interventions",
#   "grass_interventions"
# )

dat <- purrr::map_dfr(
  
  interventions,
  
  \(x) {
    
    extracted |>
      
      dplyr::filter(
        target_intervention == x
      ) |>
      
      process_intervention()
    
  }
  
)

dataset_points <- dat |>
  dplyr::filter(
    !is.na(latitude),
    !is.na(longitude)
  ) |>
  dplyr::distinct(
    dataset_ID,
    latitude,
    longitude,
    .keep_all = TRUE
  )