### Install and load necessary packages ###

# Use pacman for efficient package management
if (!require(pacman)) {
  install.packages("pacman")
}

# Load all required packages, installing them if they are not already present
pacman::p_load(
  haven, # For reading .sav files
  tidyverse, # For data manipulation and plotting
  readxl, # For reading .xlsx files
  conflicted, # For managing function name conflicts
  systemfit, # For SUR estimation of the AIDS model
  car, # For deltaMethod to calculate standard errors
  sandwich, # For robust standard errors
  lmtest, # For coeftest
  lubridate, # For date manipulation
  stargazer, # For creating summary tables
  sjlabelled # For processing labels of sav files
)

# Resolve conflicts by explicitly stating preferences for dplyr functions
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::summarise)

### PART 1: DECOMPOSE EXPENDITURES AND PREPARE HOUSEHOLD DATA ###

# Load raw survey data
ssu_21_h <- read_sav("ssu_data/Households_microdani_anonimni_2021.sav")
ssu_21_w <- read_sav("ssu_data/Members_microdani_anonimni_2021.sav")
unicef_24_h <- read_excel(
  "unicef_data/teams-UKR-PrgmEff-UCO KnowledgeUNICEF-SESH Household-2.0.xlsx",
  skip = 4
)
unicef_24_m <- read_excel(
  "unicef_data/teams-UKR-PrgmEff-UCO KnowledgeUNICEF-SESHS IND 10042024 FIN-2.0.xlsx",
  skip = 3
)

# Process SSU 2021 Data with 3 expenditure categories
ssu_smokers <- ssu_21_w %>%
  group_by(code_fam) %>%
  summarise(n_smokers = sum(smoking == 1, na.rm = TRUE))

correct_hh_size <- ssu_21_w %>%
  group_by(code_fam) %>%
  summarise(hh_size_corrected = max(N_MEMBER, na.rm = TRUE), .groups = 'drop')

ssu_21_processed <- ssu_21_h %>%
  left_join(correct_hh_size, by = "code_fam") %>%
  left_join(ssu_smokers, by = "code_fam") %>%
  transmute(
    hh_id = as.character(code_fam),
    weight = w_q,
    cod_obl = sjlabelled::to_character(cod_obl),
    region = case_when(
      str_starts(cod_obl, "Вінницька") ~ "Вінницька",
      str_starts(cod_obl, "Волинська") ~ "Волинська",
      str_starts(cod_obl, "Дніпропетровська") ~ "Дніпропетровська",
      str_starts(cod_obl, "Донецька") ~ "Донецька",
      str_starts(cod_obl, "Житомирська") ~ "Житомирська",
      str_starts(cod_obl, "Закарпатська") ~ "Закарпатська",
      str_starts(cod_obl, "Запорізька") ~ "Запорізька",
      str_starts(cod_obl, "Івано-Франківська") ~ "Івано-Франківська",
      str_starts(cod_obl, "Київська") ~ "Київська",
      str_starts(cod_obl, "Кіровоградська") ~ "Кіровоградська",
      str_starts(cod_obl, "Луганська") ~ "Луганська",
      str_starts(cod_obl, "Львівська") ~ "Львівська",
      str_starts(cod_obl, "Миколаївська") ~ "Миколаївська",
      str_starts(cod_obl, "Одеська") ~ "Одеська",
      str_starts(cod_obl, "Полтавська") ~ "Полтавська",
      str_starts(cod_obl, "Рівненська") ~ "Рівненська",
      str_starts(cod_obl, "Сумська") ~ "Сумська",
      str_starts(cod_obl, "Тернопільська") ~ "Тернопільська",
      str_starts(cod_obl, "Харківська") ~ "Харківська",
      str_starts(cod_obl, "Херсонська") ~ "Херсонська",
      str_starts(cod_obl, "Хмельницька") ~ "Хмельницька",
      str_starts(cod_obl, "Черкаська") ~ "Черкаська",
      str_starts(cod_obl, "Чернівецька") ~ "Чернівецька",
      str_starts(cod_obl, "Чернігівська") ~ "Чернігівська",
      str_starts(cod_obl, "м.Київ") ~ "Київ",
      TRUE ~ cod_obl
    ),
    settlement_type = ifelse(tp_ns_p %in% c(1, 2), "Urban", "Rural"),
    hh_size = hh_size_corrected,
    has_children = ifelse(type_dom == 1, 1, 0),
    exp_total_consumption = h00,
    exp_food = h01,
    exp_tobacco_only = h0221,
    exp_alcohol_only = h0211 + h0212 + h0213,
    exp_ta = exp_tobacco_only + exp_alcohol_only,
    exp_other = exp_total_consumption - exp_food - exp_ta,
    period_id = paste0("2021", "Q", kvart_kd),
    n_smokers_dum = ifelse(n_smokers > 0, 1, 0)
  )

# Process UNICEF 2024 Data with 3 expenditure categories
unicef_24_processed <- unicef_24_h %>%
  mutate(across(
    c(starts_with("G"), "V1", "dity_n", "hh_size", "WGT_Fin_NEW"),
    as.numeric
  )) %>%
  mutate(exp_utilities = coalesce(G3_2_1, G3_2_2)) %>%
  rowwise() %>%
  mutate(
    exp_ta = G1_9,
    exp_food = G1_1,
    exp_non_food_essentials = sum(
      c(
        G2_3_1,
        G2_3_2,
        G2_3_3,
        G2_3_4,
        G2_3_5,
        G2_3_6,
        G2_3_7,
        G2_3_8,
        G2_3_9,
        G2_3_10,
        G2_5_1,
        G2_5_2,
        G2_5_3,
        G2_5_4,
        G2_5_5,
        G2_8_1,
        G2_8_2,
        G2_8_3,
        G2_8_4,
        G2_8_5,
        G2_8_6,
        G2_11,
        G2_13_1,
        G2_13_2,
        G2_15_1_2,
        G2_15_2_2,
        G2_15_3_2,
        G2_15_4_2,
        G2_15_5_2,
        G2_15_6_2,
        G2_15_7_2,
        G2_15_8_2,
        G2_15_9_2,
        G2_15_10_2,
        G2_15_11_2,
        G3_1,
        exp_utilities
      ),
      na.rm = TRUE
    ),
    exp_total_consumption = sum(
      exp_ta,
      exp_food,
      exp_non_food_essentials,
      na.rm = TRUE
    ),
    exp_other = exp_non_food_essentials
  ) %>%
  ungroup() %>%
  transmute(
    hh_id = KEY_QUEST,
    weight = WGT_Fin_NEW,
    region = OBLAST,
    settlement_type = SETTL_TYPE,
    hh_size,
    is_idp = ifelse(D1_Mult9 == "Так", 1, 0),
    has_children = ifelse(dity_n > 0, 1, 0),
    exp_ta,
    exp_food,
    exp_other,
    exp_total_consumption,
    period_id = "2024_SURVEY_DEC23_FEB24"
  ) %>%
  mutate(
    region = case_when(
      str_starts(region, "Вінницька") ~ "Вінницька",
      str_starts(region, "Волинська") ~ "Волинська",
      str_starts(region, "Дніпропетровська") ~ "Дніпропетровська",
      str_starts(region, "Донецька") ~ "Донецька",
      str_starts(region, "Житомирська") ~ "Житомирська",
      str_starts(region, "Закарпатська") ~ "Закарпатська",
      str_starts(region, "Запорізька") ~ "Запорізька",
      str_starts(region, "Івано-Франківська") ~ "Івано-Франківська",
      str_starts(region, "Київська") ~ "Київська",
      str_starts(region, "Кіровоградська") ~ "Кіровоградська",
      str_starts(region, "Луганська") ~ "Луганська",
      str_starts(region, "Львівська") ~ "Львівська",
      str_starts(region, "Миколаївська") ~ "Миколаївська",
      str_starts(region, "Одеська") ~ "Одеська",
      str_starts(region, "Полтавська") ~ "Полтавська",
      str_starts(region, "Рівненська") ~ "Рівненська",
      str_starts(region, "Сумська") ~ "Сумська",
      str_starts(region, "Тернопільська") ~ "Тернопільська",
      str_starts(region, "Харківська") ~ "Харківська",
      str_starts(region, "Херсонська") ~ "Херсонська",
      str_starts(region, "Хмельницька") ~ "Хмельницька",
      str_starts(region, "Черкаська") ~ "Черкаська",
      str_starts(region, "Чернівецька") ~ "Чернівецька",
      str_starts(region, "Чернігівська") ~ "Чернігівська",
      str_starts(region, "м.Київ") ~ "Київ",
      TRUE ~ region
    )
  )

### PART 2: LOAD AND PROCESS PRICE DATA ###

# Load and process average nominal price data
avg_price_data <- read_excel(
  "price_data/dataset_2025-07-04T08_52_55.294010379Z_DEFAULT_INTEGRATION_SSSU_DF_PRICE_CHANGE_CONSUMER_GOODS_SERVICE_24.0.0.xlsx"
)

avg_price_processed <- avg_price_data %>%
  mutate(
    year = as.numeric(str_sub(Період, 1, 4)),
    month = as.numeric(str_sub(Період, 7, 8)),
    region = `Територіальний розріз`,
    product = `Тип товарів і послуг`,
    price_nominal = as.numeric(`Значення cпостереження`)
  ) %>%
  filter(!is.na(price_nominal)) %>%
  select(region, product, price_nominal, year, month)

# Define product categories
categories_products <- unique(avg_price_processed$product)

food_products <- categories_products[1:33]

alcohol_products <- categories_products[34:36]

tobacco_products <- categories_products[37:40]

other_products <- categories_products[41:55]

avg_price_processed <- avg_price_processed %>%
  mutate(
    product_category = case_when(
      product %in% food_products ~ "food",
      product %in% tobacco_products ~ "tobacco",
      product %in% alcohol_products ~ "alcohol",
      TRUE ~ "other"
    )
  )

category_prices <- avg_price_processed %>%
  # Add category labels to each good
  group_by(region, month, year, product_category) %>%
  # For now, use simple geometric mean within categories
  # We'll discuss weighting schemes below
  summarise(
    category_price_index = exp(mean(log(price_nominal), na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  # Reshape to have one column per category
  pivot_wider(
    names_from = product_category,
    values_from = category_price_index,
    names_prefix = "price_"
  )

separate_spenders <- ssu_21_processed %>%
  filter(exp_tobacco_only > 0 & exp_alcohol_only > 0)


ta_weights <- separate_spenders %>%
  mutate(
    total_ta = exp_tobacco_only + exp_alcohol_only,
    share_tobacco = exp_tobacco_only / total_ta,
    share_alcohol = exp_alcohol_only / total_ta
  ) %>%
  summarise(
    avg_share_tobacco = mean(share_tobacco),
    avg_share_alcohol = mean(share_alcohol)
  )

tobacco_price <- avg_price_processed %>%
  filter(product_category == "tobacco") %>%
  group_by(region, year, month) %>%
  summarise(tobacco_index = exp(mean(log(price_nominal))), .groups = 'drop')

# Aggregate alcohol products into single alcohol price
alcohol_price <- avg_price_processed %>%
  filter(product_category == "alcohol") %>%
  group_by(region, year, month) %>%
  summarise(alcohol_index = exp(mean(log(price_nominal))), .groups = 'drop')

# Combine using fixed weights from 2021
combined_ta_price <- tobacco_price %>%
  left_join(alcohol_price, by = c("region", "year", "month")) %>%
  mutate(
    # This is your comparable tobacco+alcohol price index
    price_ta = (tobacco_index^ta_weights$avg_share_tobacco) *
      (alcohol_index^ta_weights$avg_share_alcohol)
  ) %>%
  select(-tobacco_index, -alcohol_index)


category_prices_final <- category_prices %>%
  left_join(combined_ta_price, by = c("region", "year", "month"))


create_relative_prices <- function(category_prices) {
  category_prices %>%
    mutate(
      # Express tobacco, alcohol, and food prices relative to "other" goods
      rel_price_tobacco = price_tobacco / price_other,
      rel_price_alcohol = price_alcohol / price_other,
      rel_price_food = price_food / price_other,
      rel_price_ta = price_ta / price_other,
      # Other goods price becomes 1 by construction
      rel_price_other = 1
    )
}

rel_prices <- create_relative_prices(category_prices_final)

assign_period <- function(year, month) {
  if ((year == 2023 & month == 12) | (year == 2024 & month %in% 1:2)) {
    return("2024_SURVEY_DEC23_FEB24")
  } else {
    return(paste0(year, "Q", ceiling(month / 3)))
  }
}

rel_prices_with_period <- rel_prices %>%
  mutate(period_id = map2_chr(year, month, assign_period))

# Aggregate the monthly relative prices into the corresponding quarter/survey periods
price_summary <- rel_prices_with_period %>%
  group_by(region, period_id) %>%
  # Calculate the mean for all relative price columns within each group
  summarise(
    across(starts_with("rel_price"), ~ mean(.x, na.rm = TRUE)),
    .groups = 'drop'
  )


# Merge household data with price data
ssu_merged <- ssu_21_processed %>%
  left_join(price_summary, by = c("region", "period_id"))

unicef_merged <- unicef_24_processed %>%
  left_join(price_summary, by = c("region", "period_id"))


# Filter price data to match expenditure periods exactly
ssu_period_prices <- rel_prices_with_period %>%
  # Only Q4 2021 for SSU data
  filter(period_id %in% c("2021Q4")) %>%
  group_by(region, period_id) %>%
  summarise(
    across(starts_with("rel_price"), ~ mean(.x, na.rm = TRUE)),
    n_months = n(), # Track how many months contributed to each average
    .groups = 'drop'
  )

unicef_period_prices <- rel_prices_with_period %>%
  # Only survey period for UNICEF data
  filter(period_id == "2024_SURVEY_DEC23_FEB24") %>%
  group_by(region, period_id) %>%
  summarise(
    across(starts_with("rel_price"), ~ mean(.x, na.rm = TRUE)),
    n_months = n(),
    .groups = 'drop'
  )

# Combine the period-specific price data
price_summary_refined <- bind_rows(ssu_period_prices, unicef_period_prices)

### PART 3: DATA PREPARATION FOR EACH MODEL ###

# The key insight here is that each model requires slightly different data structures
# because they partition the consumer's budget in different ways. Rather than trying
# to handle all cases in one function, we'll prepare each dataset explicitly.

# First, let's establish our baseline merged datasets
# We're joining the expenditure data with our carefully constructed price measures
ssu_base_data <- ssu_21_processed %>%
  left_join(price_summary, by = c("region", "period_id")) %>%
  # Remove households with zero total consumption (these are likely data errors)
  filter(exp_total_consumption > 0)

unicef_base_data <- unicef_24_processed %>%
  left_join(price_summary, by = c("region", "period_id")) %>%
  filter(exp_total_consumption > 0)

# Let's also create our demographic variable lists for clarity
demog_vars_ssu <- c("hh_size", "has_children", "n_smokers_dum")
demog_vars_unicef <- c("hh_size", "has_children", "is_idp")

### SSU MODEL 1: 2-GOOD (TOBACCO+ALCOHOL vs OTHER) ###

cat("=== PREPARING SSU MODEL 1: 2-GOOD SYSTEM ===\n")

# Create the data structure for our 2-good model
# The key insight: we're combining food and other goods into one "other" category
ssu_model1_data <- ssu_base_data %>%
  mutate(
    # Combine food and other expenditures into a single "other" category
    # This reflects the economic assumption that households first decide
    # whether to spend on tobacco/alcohol, then allocate remaining budget
    exp_other_combined = exp_food + exp_other,

    # Calculate budget shares for our two-good system
    # These must sum to 1 by construction (this is the adding-up constraint)
    w_ta = exp_ta / exp_total_consumption,
    w_other_combined = exp_other_combined / exp_total_consumption,

    # Create relative prices for our model
    # We need a combined price for the "other" category
    # Using expenditure-weighted geometric mean is standard practice
    rel_price_other_combined = (rel_price_food^(exp_food /
      exp_other_combined)) *
      (rel_price_other^(exp_other / exp_other_combined)),

    # Transform to logarithms for the LA/EASI specification
    log_rel_p_ta = log(rel_price_ta),
    log_rel_p_other_combined = log(rel_price_other_combined),

    # Construct the Stone price index for this specification
    # This represents the "typical" price level the household faces
    log_stone_p = w_ta *
      log_rel_p_ta +
      w_other_combined * log_rel_p_other_combined,

    # Calculate real expenditure (deflated by the Stone index)
    # This is the key variable in the EASI system
    y = log(exp_total_consumption) - log_stone_p,
    y_sq = y^2 # Quadratic term allows for nonlinear Engel curves
  ) %>%
  # Keep only households with positive expenditure in both categories
  # Zero expenditures cause problems in log-linear specifications
  filter(w_ta > 0, w_other_combined > 0, !is.na(y))

cat("Sample size for SSU Model 1:", nrow(ssu_model1_data), "\n")
cat("Mean tobacco+alcohol budget share:", mean(ssu_model1_data$w_ta), "\n")

# Estimate the model using ordinary least squares
# For a 2-good system, we only need to estimate one equation
# The second equation is determined by the adding-up constraint
ssu_model1_fit <- lm(
  # LA/EASI specification for tobacco+alcohol budget share
  w_ta ~
    # Expenditure terms (linear and quadratic)
    y +
      y_sq +
      # Price terms
      log_rel_p_ta +
      log_rel_p_other_combined +
      # Expenditure-price interactions (key feature of EASI)
      y:log_rel_p_ta +
      y:log_rel_p_other_combined +
      # Demographic controls
      hh_size +
      has_children +
      n_smokers_dum +
      # Expenditure-demographic interactions
      y:hh_size +
      y:has_children +
      y:n_smokers_dum,
  data = ssu_model1_data,
  weights = weight
)

cat("\n--- SSU Model 1 Results ---\n")
print(summary(ssu_model1_fit))

### SSU MODEL 2 CORRECTED: 3-GOOD (TOBACCO+ALCOHOL, FOOD, OTHER) ###

cat("\n=== CORRECTED SSU MODEL 2: 3-GOOD SYSTEM ===\n")

# The data preparation remains the same - the issue is in the model specification
ssu_model2_data <- ssu_base_data %>%
  mutate(
    w_ta = exp_ta / exp_total_consumption,
    w_food = exp_food / exp_total_consumption,
    w_other = exp_other / exp_total_consumption,

    # Keep all price variables for completeness, but we won't use all in regression
    log_rel_p_ta = log(rel_price_ta),
    log_rel_p_food = log(rel_price_food),
    log_rel_p_other = log(rel_price_other), # This will be 0, but we create it for clarity

    # Stone price index calculation (this remains correct)
    log_stone_p = w_ta *
      log_rel_p_ta +
      w_food * log_rel_p_food +
      w_other * log_rel_p_other,

    y = log(exp_total_consumption) - log_stone_p,
    y_sq = y^2
  ) %>%
  filter(w_ta > 0, w_food > 0, w_other > 0, !is.na(y))

# CORRECTED Equation 1: Tobacco+Alcohol share
# Notice: we EXCLUDE log_rel_p_other because it's the numeraire
ssu_model2_eq1_corrected <- lm(
  w_ta ~
    y +
      y_sq +
      log_rel_p_ta +
      log_rel_p_food + # No log_rel_p_other!
      y:log_rel_p_ta +
      y:log_rel_p_food + # No y:log_rel_p_other!
      hh_size +
      has_children +
      n_smokers_dum +
      y:hh_size +
      y:has_children +
      y:n_smokers_dum,
  data = ssu_model2_data,
  weights = weight
)

# CORRECTED Equation 2: Food share
ssu_model2_eq2_corrected <- lm(
  w_food ~
    y +
      y_sq +
      log_rel_p_ta +
      log_rel_p_food + # No log_rel_p_other!
      y:log_rel_p_ta +
      y:log_rel_p_food + # No y:log_rel_p_other!
      hh_size +
      has_children +
      n_smokers_dum +
      y:hh_size +
      y:has_children +
      y:n_smokers_dum,
  data = ssu_model2_data,
  weights = weight
)

cat("\n--- SSU Model 2 CORRECTED Results ---\n")
cat("Tobacco+Alcohol equation:\n")
print(summary(ssu_model2_eq1_corrected))
cat("\nFood equation:\n")
print(summary(ssu_model2_eq2_corrected))

ssu_model2_fit_corrected <- list(
  eq1 = ssu_model2_eq1_corrected,
  eq2 = ssu_model2_eq2_corrected
)

### SSU MODEL 3 CORRECTED: 4-GOOD (TOBACCO, ALCOHOL, FOOD, OTHER) ###

cat("\n=== CORRECTED SSU MODEL 3: 4-GOOD SYSTEM ===\n")

# Data preparation remains the same
ssu_model3_data <- ssu_base_data %>%
  mutate(
    w_tobacco = exp_tobacco_only / exp_total_consumption,
    w_alcohol = exp_alcohol_only / exp_total_consumption,
    w_food = exp_food / exp_total_consumption,
    w_other = exp_other / exp_total_consumption,

    log_rel_p_tobacco = log(rel_price_tobacco),
    log_rel_p_alcohol = log(rel_price_alcohol),
    log_rel_p_food = log(rel_price_food),
    log_rel_p_other = log(rel_price_other), # Will be 0

    log_stone_p = w_tobacco *
      log_rel_p_tobacco +
      w_alcohol * log_rel_p_alcohol +
      w_food * log_rel_p_food +
      w_other * log_rel_p_other,

    y = log(exp_total_consumption) - log_stone_p,
    y_sq = y^2
  ) %>%
  filter(w_tobacco > 0, w_alcohol > 0, w_food > 0, w_other > 0, !is.na(y))

# CORRECTED equations - exclude the numeraire price
ssu_model3_eq1_corrected <- lm(
  w_tobacco ~
    y +
      y_sq +
      log_rel_p_tobacco +
      log_rel_p_alcohol +
      log_rel_p_food + # No log_rel_p_other!
      y:log_rel_p_tobacco +
      y:log_rel_p_alcohol +
      y:log_rel_p_food + # No interactions with other!
      hh_size +
      has_children +
      n_smokers_dum +
      y:hh_size +
      y:has_children +
      y:n_smokers_dum,
  data = ssu_model3_data,
  weights = weight
)

ssu_model3_eq2_corrected <- lm(
  w_alcohol ~
    y +
      y_sq +
      log_rel_p_tobacco +
      log_rel_p_alcohol +
      log_rel_p_food +
      y:log_rel_p_tobacco +
      y:log_rel_p_alcohol +
      y:log_rel_p_food +
      hh_size +
      has_children +
      n_smokers_dum +
      y:hh_size +
      y:has_children +
      y:n_smokers_dum,
  data = ssu_model3_data,
  weights = weight
)

ssu_model3_eq3_corrected <- lm(
  w_food ~
    y +
      y_sq +
      log_rel_p_tobacco +
      log_rel_p_alcohol +
      log_rel_p_food +
      y:log_rel_p_tobacco +
      y:log_rel_p_alcohol +
      y:log_rel_p_food +
      hh_size +
      has_children +
      n_smokers_dum +
      y:hh_size +
      y:has_children +
      y:n_smokers_dum,
  data = ssu_model3_data,
  weights = weight
)

cat("\n--- SSU Model 3 CORRECTED Results ---\n")
cat("Tobacco equation:\n")
print(summary(ssu_model3_eq1_corrected))
cat("\nAlcohol equation:\n")
print(summary(ssu_model3_eq2_corrected))
cat("\nFood equation:\n")
print(summary(ssu_model3_eq3_corrected))

ssu_model3_fit_corrected <- list(
  eq1 = ssu_model3_eq1_corrected,
  eq2 = ssu_model3_eq2_corrected,
  eq3 = ssu_model3_eq3_corrected
)

### UNICEF MODEL 1: 2-GOOD (TOBACCO+ALCOHOL vs OTHER) ###

cat("\n=== PREPARING UNICEF MODEL 1: 2-GOOD SYSTEM ===\n")

# Prepare UNICEF data for 2-good analysis
# The structure mirrors the SSU 2-good model
unicef_model1_data <- unicef_base_data %>%
  mutate(
    # Combine food and other into single category
    exp_other_combined = exp_food + exp_other,

    # Budget shares
    w_ta = exp_ta / exp_total_consumption,
    w_other_combined = exp_other_combined / exp_total_consumption,

    # Combined price for "other" category
    rel_price_other_combined = (rel_price_food^(exp_food /
      exp_other_combined)) *
      (rel_price_other^(exp_other / exp_other_combined)),

    # Log prices
    log_rel_p_ta = log(rel_price_ta),
    log_rel_p_other_combined = log(rel_price_other_combined),

    # Stone price index
    log_stone_p = w_ta *
      log_rel_p_ta +
      w_other_combined * log_rel_p_other_combined,

    # Real expenditure
    y = log(exp_total_consumption) - log_stone_p,
    y_sq = y^2
  ) %>%
  filter(w_ta > 0, w_other_combined > 0, !is.na(y))

cat("Sample size for UNICEF Model 1:", nrow(unicef_model1_data), "\n")
cat("Mean tobacco+alcohol budget share:", mean(unicef_model1_data$w_ta), "\n")

# Estimate the model
unicef_model1_fit <- lm(
  w_ta ~
    y +
      y_sq +
      log_rel_p_ta +
      log_rel_p_other_combined +
      y:log_rel_p_ta +
      y:log_rel_p_other_combined +
      hh_size +
      has_children +
      is_idp +
      y:hh_size +
      y:has_children +
      y:is_idp,
  data = unicef_model1_data,
  weights = weight
)

cat("\n--- UNICEF Model 1 Results ---\n")
print(summary(unicef_model1_fit))


### UNICEF MODEL 2 CORRECTED: 3-GOOD (TOBACCO+ALCOHOL, FOOD, OTHER) ###

cat("\n=== CORRECTED UNICEF MODEL 2: 3-GOOD SYSTEM ===\n")

# Data preparation remains the same
unicef_model2_data <- unicef_base_data %>%
  mutate(
    w_ta = exp_ta / exp_total_consumption,
    w_food = exp_food / exp_total_consumption,
    w_other = exp_other / exp_total_consumption,

    log_rel_p_ta = log(rel_price_ta),
    log_rel_p_food = log(rel_price_food),
    log_rel_p_other = log(rel_price_other),

    log_stone_p = w_ta *
      log_rel_p_ta +
      w_food * log_rel_p_food +
      w_other * log_rel_p_other,

    y = log(exp_total_consumption) - log_stone_p,
    y_sq = y^2
  ) %>%
  filter(w_ta > 0, w_food > 0, w_other > 0, !is.na(y))

# CORRECTED equations
unicef_model2_eq1_corrected <- lm(
  w_ta ~
    y +
      y_sq +
      log_rel_p_ta +
      log_rel_p_food + # No numeraire price!
      y:log_rel_p_ta +
      y:log_rel_p_food +
      hh_size +
      has_children +
      is_idp +
      y:hh_size +
      y:has_children +
      y:is_idp,
  data = unicef_model2_data,
  weights = weight
)

unicef_model2_eq2_corrected <- lm(
  w_food ~
    y +
      y_sq +
      log_rel_p_ta +
      log_rel_p_food +
      y:log_rel_p_ta +
      y:log_rel_p_food +
      hh_size +
      has_children +
      is_idp +
      y:hh_size +
      y:has_children +
      y:is_idp,
  data = unicef_model2_data,
  weights = weight
)

cat("\n--- UNICEF Model 2 CORRECTED Results ---\n")
cat("Tobacco+Alcohol equation:\n")
print(summary(unicef_model2_eq1_corrected))
cat("\nFood equation:\n")
print(summary(unicef_model2_eq2_corrected))

unicef_model2_fit_corrected <- list(
  eq1 = unicef_model2_eq1_corrected,
  eq2 = unicef_model2_eq2_corrected
)

### UPDATED ELASTICITY CALCULATION FUNCTION ###

calculate_easi_elasticities_corrected <- function(
  model_fit,
  model_data,
  goods_names,
  price_vars_estimated,
  demog_vars = NULL
) {
  # This function handles the numeraire case where not all price variables are estimated

  n_goods <- length(goods_names)
  n_estimated_prices <- length(price_vars_estimated)

  # Build complete price variable list (including numeraire)
  # For 3-good case: we estimate 2 price effects, numeraire is constrained to 0
  # For 4-good case: we estimate 3 price effects, numeraire is constrained to 0

  # Calculate mean values
  w_vars_actual <- character(n_goods)
  for (i in 1:n_goods) {
    if (paste0("w_", goods_names[i]) %in% names(model_data)) {
      w_vars_actual[i] <- paste0("w_", goods_names[i])
    } else if (
      goods_names[i] == "other_combined" &&
        "w_other_combined" %in% names(model_data)
    ) {
      w_vars_actual[i] <- "w_other_combined"
    } else {
      possible_names <- names(model_data)[
        grepl(goods_names[i], names(model_data)) &
          grepl("^w_", names(model_data))
      ]
      if (length(possible_names) > 0) {
        w_vars_actual[i] <- possible_names[1]
      } else {
        stop(paste("Cannot find budget share variable for", goods_names[i]))
      }
    }
  }

  # Calculate means
  means <- model_data %>%
    summarise(
      across(all_of(w_vars_actual), mean, na.rm = TRUE),
      y_mean = mean(y, na.rm = TRUE)
    )

  w_i <- as.numeric(means[w_vars_actual])
  mean_y <- means$y_mean

  # Calculate mean log prices for estimated variables only
  mean_log_p_estimated <- model_data %>%
    summarise(across(all_of(price_vars_estimated), mean, na.rm = TRUE))
  log_p_k_estimated <- as.numeric(mean_log_p_estimated)

  # Add zero for numeraire price (which is log(1) = 0)
  log_p_k <- c(log_p_k_estimated, 0)

  # Initialize parameter matrices
  A <- matrix(0, n_goods, n_goods)
  B <- matrix(0, n_goods, n_goods)
  b1 <- numeric(n_goods)
  b2 <- numeric(n_goods)

  # Extract coefficients for estimated equations
  if (class(model_fit)[1] == "lm") {
    # Single equation case (won't have numeraire issues for 2-good models)
    all_coefs <- coef(model_fit)
    i <- 1
    b1[i] <- all_coefs["y"]
    b2[i] <- all_coefs["y_sq"]

    for (j in 1:n_estimated_prices) {
      A[i, j] <- all_coefs[price_vars_estimated[j]]
      B[i, j] <- all_coefs[paste0("y:", price_vars_estimated[j])]
    }
  } else {
    # Multiple equation case
    for (i in 1:(n_goods - 1)) {
      eq_coefs <- coef(model_fit[[i]])

      b1[i] <- eq_coefs["y"]
      b2[i] <- eq_coefs["y_sq"]

      for (j in 1:n_estimated_prices) {
        A[i, j] <- eq_coefs[price_vars_estimated[j]]
        B[i, j] <- eq_coefs[paste0("y:", price_vars_estimated[j])]
      }
      # Note: A[i, n_goods] and B[i, n_goods] remain 0 for numeraire
    }
  }

  # Recover parameters for the last good using adding-up constraints
  A[n_goods, ] <- -colSums(A[1:(n_goods - 1), , drop = FALSE])
  B[n_goods, ] <- -colSums(B[1:(n_goods - 1), , drop = FALSE])
  b1[n_goods] <- -sum(b1[1:(n_goods - 1)])
  b2[n_goods] <- -sum(b2[1:(n_goods - 1)])

  # Calculate elasticities (same formulas as before)
  e_hicks <- matrix(0, n_goods, n_goods)
  for (i in 1:n_goods) {
    for (j in 1:n_goods) {
      delta_ij <- ifelse(i == j, 1, 0)
      e_hicks[i, j] <- (A[j, i] + B[j, i] * mean_y) / w_i[i] + w_i[j] - delta_ij
    }
  }

  eta <- numeric(n_goods)
  for (i in 1:n_goods) {
    eta[i] <- 1 +
      (1 / w_i[i]) * (b1[i] + 2 * b2[i] * mean_y + sum(B[i, ] * log_p_k))
  }

  e_marshall <- e_hicks - outer(eta, w_i)

  # Assign names
  dimnames(e_hicks) <- dimnames(e_marshall) <- list(goods_names, goods_names)
  names(eta) <- goods_names

  return(list(
    expenditure_elasticities = eta,
    hicksian_price_elasticities = e_hicks,
    marshallian_price_elasticities = e_marshall,
    mean_budget_shares = w_i
  ))
}
### CALCULATE ELASTICITIES FOR ALL MODELS ###

cat("\n\n=== CALCULATING ELASTICITIES ===\n")

# SSU Model 1 elasticities (2-good)
cat("\n--- SSU Model 1 Elasticities (2-good: TA vs Other) ---\n")
ssu_elasticities_1 <- calculate_easi_elasticities(
  model_fit = ssu_model1_fit,
  model_data = ssu_model1_data,
  goods_names = c("ta", "other_combined"),
  price_vars = c("log_rel_p_ta", "log_rel_p_other_combined"),
  demog_vars = demog_vars_ssu
)
print(ssu_elasticities_1)

# SSU Model 2 elasticities (3-good)
cat("\n--- SSU Model 2 CORRECTED Elasticities (3-good: TA, Food, Other) ---\n")
ssu_elasticities_2_corrected <- calculate_easi_elasticities_corrected(
  model_fit = ssu_model2_fit_corrected,
  model_data = ssu_model2_data,
  goods_names = c("ta", "food", "other"),
  price_vars_estimated = c("log_rel_p_ta", "log_rel_p_food"), # No numeraire!
  demog_vars = demog_vars_ssu
)
print(ssu_elasticities_2_corrected)

# SSU Model 3 elasticities (4-good)
cat(
  "\n--- SSU Model 3 CORRECTED Elasticities (4-good: Tobacco, Alcohol, Food, Other) ---\n"
)
ssu_elasticities_3_corrected <- calculate_easi_elasticities_corrected(
  model_fit = ssu_model3_fit_corrected,
  model_data = ssu_model3_data,
  goods_names = c("tobacco", "alcohol", "food", "other"),
  price_vars_estimated = c(
    "log_rel_p_tobacco",
    "log_rel_p_alcohol",
    "log_rel_p_food"
  ), # No numeraire!
  demog_vars = demog_vars_ssu
)
print(ssu_elasticities_3_corrected)

# UNICEF Model 1 elasticities (2-good)
cat("\n--- UNICEF Model 1 Elasticities (2-good: TA vs Other) ---\n")
unicef_elasticities_1 <- calculate_easi_elasticities(
  model_fit = unicef_model1_fit,
  model_data = unicef_model1_data,
  goods_names = c("ta", "other_combined"),
  price_vars = c("log_rel_p_ta", "log_rel_p_other_combined"),
  demog_vars = demog_vars_unicef
)
print(unicef_elasticities_1)

# UNICEF Model 2 elasticities (3-good)
cat(
  "\n--- UNICEF Model 2 CORRECTED Elasticities (3-good: TA, Food, Other) ---\n"
)
unicef_elasticities_2_corrected <- calculate_easi_elasticities_corrected(
  model_fit = unicef_model2_fit_corrected,
  model_data = unicef_model2_data,
  goods_names = c("ta", "food", "other"),
  price_vars_estimated = c("log_rel_p_ta", "log_rel_p_food"), # No numeraire!
  demog_vars = demog_vars_unicef
)
print(unicef_elasticities_2_corrected)


### BOOTSTRAP INFERENCE FOR DEMAND SYSTEM ELASTICITIES ###

# First, let's create a function that performs the entire estimation procedure
# from start to finish for a given dataset. This encapsulates your entire
# analytical pipeline in a single, reusable function.

estimate_full_model_pipeline <- function(
  household_data,
  price_data,
  model_type,
  demog_vars
) {
  # This function replicates your entire estimation process from data preparation
  # through elasticity calculation. The key insight is that by wrapping everything
  # in a function, we can easily apply it to bootstrap samples.

  # Step 1: Merge household and price data
  merged_data <- household_data %>%
    left_join(price_data, by = c("region", "period_id")) %>%
    filter(exp_total_consumption > 0)

  # Step 2: Prepare model-specific data structures
  if (model_type == "2_good_ta_other") {
    # This replicates your 2-good model preparation
    model_data <- merged_data %>%
      mutate(
        exp_other_combined = exp_food + exp_other,
        w_ta = exp_ta / exp_total_consumption,
        w_other_combined = exp_other_combined / exp_total_consumption,

        # Handle the case where combined price calculation might fail
        rel_price_other_combined = ifelse(
          exp_other_combined > 0,
          (rel_price_food^(exp_food / exp_other_combined)) *
            (rel_price_other^(exp_other / exp_other_combined)),
          rel_price_other # Fallback to other price if food expenditure is zero
        ),

        log_rel_p_ta = log(rel_price_ta),
        log_rel_p_other_combined = log(rel_price_other_combined),

        log_stone_p = w_ta *
          log_rel_p_ta +
          w_other_combined * log_rel_p_other_combined,
        y = log(exp_total_consumption) - log_stone_p,
        y_sq = y^2
      ) %>%
      filter(w_ta > 0, w_other_combined > 0, !is.na(y), is.finite(y))

    # Estimate the single equation
    if (nrow(model_data) < 50) {
      # Safety check for small bootstrap samples
      return(NULL) # Skip this bootstrap iteration if sample is too small
    }

    model_fit <- tryCatch(
      {
        formula_string <- paste(
          "w_ta ~ y + y_sq + log_rel_p_ta + log_rel_p_other_combined + y:log_rel_p_ta + y:log_rel_p_other_combined +",
          paste(demog_vars, collapse = " + "),
          "+",
          paste(paste0("y:", demog_vars), collapse = " + ")
        )
        lm(as.formula(formula_string), data = model_data)
      },
      error = function(e) return(NULL)
    )

    if (is.null(model_fit)) {
      return(NULL)
    }

    # Calculate elasticities
    elasticities <- calculate_easi_elasticities_corrected(
      model_fit = model_fit,
      model_data = model_data,
      goods_names = c("ta", "other_combined"),
      price_vars_estimated = c("log_rel_p_ta", "log_rel_p_other_combined"),
      demog_vars = demog_vars
    )
  } else if (model_type == "3_good_ta_food_other") {
    # This replicates your 3-good model preparation
    model_data <- merged_data %>%
      mutate(
        w_ta = exp_ta / exp_total_consumption,
        w_food = exp_food / exp_total_consumption,
        w_other = exp_other / exp_total_consumption,

        log_rel_p_ta = log(rel_price_ta),
        log_rel_p_food = log(rel_price_food),

        log_stone_p = w_ta *
          log_rel_p_ta +
          w_food * log_rel_p_food +
          w_other * 0,
        y = log(exp_total_consumption) - log_stone_p,
        y_sq = y^2
      ) %>%
      filter(w_ta > 0, w_food > 0, w_other > 0, !is.na(y), is.finite(y))

    if (nrow(model_data) < 50) {
      return(NULL)
    }

    # Estimate both equations
    eq1 <- tryCatch(
      {
        formula_string <- paste(
          "w_ta ~ y + y_sq + log_rel_p_ta + log_rel_p_food + y:log_rel_p_ta + y:log_rel_p_food +",
          paste(demog_vars, collapse = " + "),
          "+",
          paste(paste0("y:", demog_vars), collapse = " + ")
        )
        lm(as.formula(formula_string), data = model_data)
      },
      error = function(e) return(NULL)
    )

    eq2 <- tryCatch(
      {
        formula_string <- paste(
          "w_food ~ y + y_sq + log_rel_p_ta + log_rel_p_food + y:log_rel_p_ta + y:log_rel_p_food +",
          paste(demog_vars, collapse = " + "),
          "+",
          paste(paste0("y:", demog_vars), collapse = " + ")
        )
        lm(as.formula(formula_string), data = model_data)
      },
      error = function(e) return(NULL)
    )

    if (is.null(eq1) || is.null(eq2)) {
      return(NULL)
    }

    model_fit <- list(eq1 = eq1, eq2 = eq2)

    # Calculate elasticities
    elasticities <- calculate_easi_elasticities_corrected(
      model_fit = model_fit,
      model_data = model_data,
      goods_names = c("ta", "food", "other"),
      price_vars_estimated = c("log_rel_p_ta", "log_rel_p_food"),
      demog_vars = demog_vars
    )
  }

  return(elasticities)
}


### BOOTSTRAP IMPLEMENTATION ###

perform_bootstrap_analysis <- function(
  household_data,
  price_data,
  model_type,
  demog_vars,
  n_bootstrap = 1000
) {
  # This function performs the complete bootstrap analysis for a single model
  # n_bootstrap determines how many bootstrap iterations we run
  # More iterations give more precise confidence intervals but take longer

  cat(
    "Starting bootstrap analysis for",
    model_type,
    "with",
    n_bootstrap,
    "iterations...\n"
  )

  # Storage for bootstrap results
  bootstrap_results <- list()
  successful_iterations <- 0

  # Set up progress reporting
  progress_interval <- max(1, floor(n_bootstrap / 10))

  for (b in 1:n_bootstrap) {
    # Progress reporting - this helps during long computations
    if (b %% progress_interval == 0) {
      cat(
        "Completed",
        b,
        "of",
        n_bootstrap,
        "iterations (",
        round(100 * b / n_bootstrap, 1),
        "%)\n"
      )
    }

    # Create bootstrap sample by sampling households with replacement
    # This is the core of the bootstrap procedure - we're creating a new
    # dataset that has the same size as the original but with some households
    # repeated and others omitted
    n_households <- nrow(household_data)
    bootstrap_indices <- sample(
      1:n_households,
      size = n_households,
      replace = TRUE
    )
    bootstrap_household_data <- household_data[bootstrap_indices, ]

    # Estimate the model on the bootstrap sample
    bootstrap_elasticities <- estimate_full_model_pipeline(
      household_data = bootstrap_household_data,
      price_data = price_data,
      model_type = model_type,
      demog_vars = demog_vars
    )

    # Store results if estimation was successful
    if (!is.null(bootstrap_elasticities)) {
      successful_iterations <- successful_iterations + 1
      bootstrap_results[[successful_iterations]] <- bootstrap_elasticities
    }
  }

  cat(
    "Bootstrap completed:",
    successful_iterations,
    "successful iterations out of",
    n_bootstrap,
    "attempts\n"
  )

  return(bootstrap_results)
}


### CONFIDENCE INTERVAL AND HYPOTHESIS TESTING FUNCTIONS ###

extract_bootstrap_confidence_intervals <- function(
  bootstrap_results,
  confidence_level = 0.95
) {
  # This function takes the list of bootstrap elasticity estimates and
  # calculates confidence intervals for each elasticity

  alpha <- 1 - confidence_level
  lower_percentile <- 100 * (alpha / 2)
  upper_percentile <- 100 * (1 - alpha / 2)

  # Extract all elasticity types
  elasticity_types <- c(
    "expenditure_elasticities",
    "marshallian_price_elasticities",
    "hicksian_price_elasticities"
  )

  results <- list()

  for (elasticity_type in elasticity_types) {
    if (elasticity_type == "expenditure_elasticities") {
      # For expenditure elasticities, we have a vector of values
      elasticity_names <- names(bootstrap_results[[1]][[elasticity_type]])
      elasticity_matrix <- matrix(
        NA,
        nrow = length(bootstrap_results),
        ncol = length(elasticity_names)
      )
      colnames(elasticity_matrix) <- elasticity_names

      for (i in 1:length(bootstrap_results)) {
        elasticity_matrix[i, ] <- bootstrap_results[[i]][[elasticity_type]]
      }

      # Calculate confidence intervals
      ci_results <- data.frame(
        elasticity = elasticity_names,
        mean = apply(elasticity_matrix, 2, mean, na.rm = TRUE),
        lower_ci = apply(
          elasticity_matrix,
          2,
          quantile,
          probs = lower_percentile / 100,
          na.rm = TRUE
        ),
        upper_ci = apply(
          elasticity_matrix,
          2,
          quantile,
          probs = upper_percentile / 100,
          na.rm = TRUE
        ),
        std_error = apply(elasticity_matrix, 2, sd, na.rm = TRUE)
      )

      results[[elasticity_type]] <- ci_results
    } else {
      # For price elasticities, we have matrices
      elasticity_matrix_sample <- bootstrap_results[[1]][[elasticity_type]]
      n_goods <- nrow(elasticity_matrix_sample)
      good_names <- rownames(elasticity_matrix_sample)

      # Create storage for all bootstrap iterations
      all_elasticities <- array(
        NA,
        dim = c(length(bootstrap_results), n_goods, n_goods)
      )

      for (i in 1:length(bootstrap_results)) {
        all_elasticities[i, , ] <- bootstrap_results[[i]][[elasticity_type]]
      }

      # Calculate confidence intervals for each elasticity
      ci_results <- data.frame(
        from_good = character(0),
        to_good = character(0),
        elasticity_type = character(0),
        mean = numeric(0),
        lower_ci = numeric(0),
        upper_ci = numeric(0),
        std_error = numeric(0)
      )

      for (i in 1:n_goods) {
        for (j in 1:n_goods) {
          elasticity_values <- all_elasticities[, i, j]

          ci_results <- rbind(
            ci_results,
            data.frame(
              from_good = good_names[i],
              to_good = good_names[j],
              elasticity_type = elasticity_type,
              mean = mean(elasticity_values, na.rm = TRUE),
              lower_ci = quantile(
                elasticity_values,
                probs = lower_percentile / 100,
                na.rm = TRUE
              ),
              upper_ci = quantile(
                elasticity_values,
                probs = upper_percentile / 100,
                na.rm = TRUE
              ),
              std_error = sd(elasticity_values, na.rm = TRUE)
            )
          )
        }
      }

      results[[elasticity_type]] <- ci_results
    }
  }

  return(results)
}

### ENHANCED HYPOTHESIS TESTING FOR ALL ELASTICITY TYPES ###

test_elasticity_difference_enhanced <- function(
  bootstrap_results_1,
  bootstrap_results_2,
  elasticity_type,
  elasticity_specification,
  confidence_level = 0.95
) {
  # This enhanced function can test differences in any type of elasticity:
  # expenditure elasticities, own-price elasticities, or cross-price elasticities
  #
  # elasticity_type: "expenditure", "own_price", or "cross_price"
  # elasticity_specification: depends on type
  #   - for expenditure: just the good name (e.g., "ta")
  #   - for own_price: the good name (e.g., "ta")
  #   - for cross_price: a list with from_good and to_good (e.g., list(from="ta", to="food"))

  # Extract elasticities from bootstrap results based on type
  if (elasticity_type == "expenditure") {
    # Extract expenditure elasticity for specified good
    good_name <- elasticity_specification

    elasticities_1 <- sapply(bootstrap_results_1, function(x) {
      if (good_name %in% names(x$expenditure_elasticities)) {
        return(x$expenditure_elasticities[good_name])
      } else {
        return(NA)
      }
    })

    elasticities_2 <- sapply(bootstrap_results_2, function(x) {
      if (good_name %in% names(x$expenditure_elasticities)) {
        return(x$expenditure_elasticities[good_name])
      } else {
        return(NA)
      }
    })

    test_description <- paste("Expenditure elasticity for", good_name)
  } else if (elasticity_type == "own_price") {
    # Extract own-price elasticity (diagonal element of price elasticity matrix)
    good_name <- elasticity_specification

    elasticities_1 <- sapply(bootstrap_results_1, function(x) {
      price_matrix <- x$marshallian_price_elasticities
      if (
        good_name %in%
          rownames(price_matrix) &&
          good_name %in% colnames(price_matrix)
      ) {
        return(price_matrix[good_name, good_name])
      } else {
        return(NA)
      }
    })

    elasticities_2 <- sapply(bootstrap_results_2, function(x) {
      price_matrix <- x$marshallian_price_elasticities
      if (
        good_name %in%
          rownames(price_matrix) &&
          good_name %in% colnames(price_matrix)
      ) {
        return(price_matrix[good_name, good_name])
      } else {
        return(NA)
      }
    })

    test_description <- paste("Own-price elasticity for", good_name)
  } else if (elasticity_type == "cross_price") {
    # Extract cross-price elasticity (off-diagonal element)
    from_good <- elasticity_specification$from
    to_good <- elasticity_specification$to

    elasticities_1 <- sapply(bootstrap_results_1, function(x) {
      price_matrix <- x$marshallian_price_elasticities
      if (
        from_good %in%
          rownames(price_matrix) &&
          to_good %in% colnames(price_matrix)
      ) {
        return(price_matrix[from_good, to_good])
      } else {
        return(NA)
      }
    })

    elasticities_2 <- sapply(bootstrap_results_2, function(x) {
      price_matrix <- x$marshallian_price_elasticities
      if (
        from_good %in%
          rownames(price_matrix) &&
          to_good %in% colnames(price_matrix)
      ) {
        return(price_matrix[from_good, to_good])
      } else {
        return(NA)
      }
    })

    test_description <- paste(
      "Cross-price elasticity:",
      from_good,
      "with respect to",
      to_good,
      "price"
    )
  }

  # Remove any NA values from both series
  valid_indices_1 <- !is.na(elasticities_1)
  valid_indices_2 <- !is.na(elasticities_2)

  elasticities_1_clean <- elasticities_1[valid_indices_1]
  elasticities_2_clean <- elasticities_2[valid_indices_2]

  # Check if we have sufficient data for testing
  if (length(elasticities_1_clean) < 50 || length(elasticities_2_clean) < 50) {
    warning("Insufficient bootstrap samples for reliable testing")
    return(NULL)
  }

  # Calculate bootstrap distribution of differences
  # We use a permutation approach to create paired differences
  n_comparisons <- min(
    length(elasticities_1_clean),
    length(elasticities_2_clean)
  )

  # Create paired differences - this is the key statistical insight
  # We're testing whether the distribution of elasticities changed between periods
  differences <- elasticities_2_clean[1:n_comparisons] -
    elasticities_1_clean[1:n_comparisons]

  # Calculate descriptive statistics for the difference
  mean_difference <- mean(differences, na.rm = TRUE)
  std_error_difference <- sd(differences, na.rm = TRUE)

  # Calculate confidence interval for the difference
  alpha <- 1 - confidence_level
  lower_ci <- quantile(differences, probs = alpha / 2, na.rm = TRUE)
  upper_ci <- quantile(differences, probs = 1 - alpha / 2, na.rm = TRUE)

  # Perform two-sided hypothesis test: H0: difference = 0
  # This uses the bootstrap distribution to calculate the p-value
  positive_differences <- sum(differences > 0, na.rm = TRUE)
  negative_differences <- sum(differences < 0, na.rm = TRUE)
  total_differences <- positive_differences + negative_differences

  # Two-sided p-value: probability of seeing a difference at least as extreme
  p_value <- 2 *
    min(positive_differences, negative_differences) /
    total_differences

  # Determine statistical significance
  is_significant <- p_value < (1 - confidence_level)

  # Calculate some additional useful statistics
  mean_elasticity_1 <- mean(elasticities_1_clean, na.rm = TRUE)
  mean_elasticity_2 <- mean(elasticities_2_clean, na.rm = TRUE)

  return(list(
    description = test_description,
    elasticity_type = elasticity_type,
    specification = elasticity_specification,

    # Period 1 (SSU 2021) statistics
    period_1_mean = mean_elasticity_1,
    period_1_std = sd(elasticities_1_clean, na.rm = TRUE),

    # Period 2 (UNICEF 2024) statistics
    period_2_mean = mean_elasticity_2,
    period_2_std = sd(elasticities_2_clean, na.rm = TRUE),

    # Difference statistics
    mean_difference = mean_difference,
    std_error_difference = std_error_difference,
    confidence_interval = c(lower_ci, upper_ci),

    # Hypothesis test results
    p_value = p_value,
    is_significant = is_significant,
    confidence_level = confidence_level,

    # Sample sizes for transparency
    n_bootstrap_1 = length(elasticities_1_clean),
    n_bootstrap_2 = length(elasticities_2_clean),
    n_comparisons = n_comparisons
  ))
}


### RUN BOOTSTRAP ANALYSIS FOR ALL MODELS ###

cat("=== RUNNING BOOTSTRAP ANALYSIS ===\n")

# Set the number of bootstrap iterations
# For development/testing, use 100-200. For final analysis, use 1000-2000
n_bootstrap <- 1000 # Adjust this based on your computational resources

# Bootstrap analysis for SSU Model 1 (2-good)
cat("\n--- Bootstrap Analysis: SSU Model 1 (2-good) ---\n")
ssu_bootstrap_1 <- perform_bootstrap_analysis(
  household_data = ssu_21_processed,
  price_data = price_summary,
  model_type = "2_good_ta_other",
  demog_vars = demog_vars_ssu,
  n_bootstrap = n_bootstrap
)

# Bootstrap analysis for UNICEF Model 1 (2-good)
cat("\n--- Bootstrap Analysis: UNICEF Model 1 (2-good) ---\n")
unicef_bootstrap_1 <- perform_bootstrap_analysis(
  household_data = unicef_24_processed,
  price_data = price_summary,
  model_type = "2_good_ta_other",
  demog_vars = demog_vars_unicef,
  n_bootstrap = n_bootstrap
)

# Bootstrap analysis for SSU Model 2 (3-good)
cat("\n--- Bootstrap Analysis: SSU Model 2 (3-good) ---\n")
ssu_bootstrap_2 <- perform_bootstrap_analysis(
  household_data = ssu_21_processed,
  price_data = price_summary,
  model_type = "3_good_ta_food_other",
  demog_vars = demog_vars_ssu,
  n_bootstrap = n_bootstrap
)

# Bootstrap analysis for UNICEF Model 2 (3-good)
cat("\n--- Bootstrap Analysis: UNICEF Model 2 (3-good) ---\n")
unicef_bootstrap_2 <- perform_bootstrap_analysis(
  household_data = unicef_24_processed,
  price_data = price_summary,
  model_type = "3_good_ta_food_other",
  demog_vars = demog_vars_unicef,
  n_bootstrap = n_bootstrap
)

# Extract confidence intervals for all models
cat("\n=== EXTRACTING CONFIDENCE INTERVALS ===\n")

ssu_ci_1 <- extract_bootstrap_confidence_intervals(ssu_bootstrap_1)
unicef_ci_1 <- extract_bootstrap_confidence_intervals(unicef_bootstrap_1)
ssu_ci_2 <- extract_bootstrap_confidence_intervals(ssu_bootstrap_2)
unicef_ci_2 <- extract_bootstrap_confidence_intervals(unicef_bootstrap_2)

# Test for significant differences in tobacco+alcohol elasticities
cat("\n=== HYPOTHESIS TESTING: CHANGES OVER TIME ===\n")

# Test difference in tobacco+alcohol expenditure elasticity between 2021 and 2024
ta_expenditure_test_2good <- test_elasticity_difference(
  bootstrap_results_1 = ssu_bootstrap_1,
  bootstrap_results_2 = unicef_bootstrap_1,
  elasticity_name = "ta",
  confidence_level = 0.95
)

ta_expenditure_test_3good <- test_elasticity_difference(
  bootstrap_results_1 = ssu_bootstrap_2,
  bootstrap_results_2 = unicef_bootstrap_2,
  elasticity_name = "ta",
  confidence_level = 0.95
)

cat("\n--- Tobacco+Alcohol Expenditure Elasticity Changes ---\n")
cat("2-good model comparison (2021 vs 2024):\n")
print(ta_expenditure_test_2good)
cat("\n3-good model comparison (2021 vs 2024):\n")
print(ta_expenditure_test_3good)

### COMPREHENSIVE HYPOTHESIS TESTING FOR TOBACCO+ALCOHOL ELASTICITIES ###

cat("\n=== COMPREHENSIVE ELASTICITY CHANGE ANALYSIS ===\n")

# Test changes in tobacco+alcohol expenditure elasticity (as before, but with new function)
cat("\n--- Testing Changes in Tobacco+Alcohol Expenditure Elasticity ---\n")

ta_expenditure_test_2good_enhanced <- test_elasticity_difference_enhanced(
  bootstrap_results_1 = ssu_bootstrap_1,
  bootstrap_results_2 = unicef_bootstrap_1,
  elasticity_type = "expenditure",
  elasticity_specification = "ta",
  confidence_level = 0.95
)

ta_expenditure_test_3good_enhanced <- test_elasticity_difference_enhanced(
  bootstrap_results_1 = ssu_bootstrap_2,
  bootstrap_results_2 = unicef_bootstrap_2,
  elasticity_type = "expenditure",
  elasticity_specification = "ta",
  confidence_level = 0.95
)

# Test changes in tobacco+alcohol own-price elasticity (this is the new analysis you requested)
cat("\n--- Testing Changes in Tobacco+Alcohol Own-Price Elasticity ---\n")

ta_own_price_test_2good <- test_elasticity_difference_enhanced(
  bootstrap_results_1 = ssu_bootstrap_1,
  bootstrap_results_2 = unicef_bootstrap_1,
  elasticity_type = "own_price",
  elasticity_specification = "ta",
  confidence_level = 0.95
)

ta_own_price_test_3good <- test_elasticity_difference_enhanced(
  bootstrap_results_1 = ssu_bootstrap_2,
  bootstrap_results_2 = unicef_bootstrap_2,
  elasticity_type = "own_price",
  elasticity_specification = "ta",
  confidence_level = 0.95
)

# Display results in a clear, interpretable format
cat("\n=== RESULTS SUMMARY ===\n")

print_elasticity_test_results <- function(test_result, model_name) {
  cat("\n", model_name, ":\n")
  cat("  Description:", test_result$description, "\n")
  cat(
    "  2021 Mean Elasticity:",
    round(test_result$period_1_mean, 4),
    "(SE:",
    round(test_result$period_1_std, 4),
    ")\n"
  )
  cat(
    "  2024 Mean Elasticity:",
    round(test_result$period_2_mean, 4),
    "(SE:",
    round(test_result$period_2_std, 4),
    ")\n"
  )
  cat("  Change (2024 - 2021):", round(test_result$mean_difference, 4), "\n")
  cat(
    "  95% Confidence Interval: [",
    round(test_result$confidence_interval[1], 4),
    ",",
    round(test_result$confidence_interval[2], 4),
    "]\n"
  )
  cat("  P-value:", round(test_result$p_value, 4), "\n")
  cat(
    "  Statistically Significant:",
    ifelse(test_result$is_significant, "YES", "NO"),
    "\n"
  )

  # Provide economic interpretation
  if (test_result$elasticity_type == "expenditure") {
    if (test_result$is_significant) {
      if (test_result$mean_difference > 0) {
        cat(
          "  Economic Interpretation: Tobacco+alcohol became MORE responsive to income changes\n"
        )
      } else {
        cat(
          "  Economic Interpretation: Tobacco+alcohol became LESS responsive to income changes\n"
        )
      }
    } else {
      cat(
        "  Economic Interpretation: No significant change in income responsiveness\n"
      )
    }
  } else if (test_result$elasticity_type == "own_price") {
    if (test_result$is_significant) {
      if (test_result$mean_difference < 0) {
        cat(
          "  Economic Interpretation: Households became MORE price-sensitive (higher elasticity)\n"
        )
      } else {
        cat(
          "  Economic Interpretation: Households became LESS price-sensitive (lower elasticity)\n"
        )
      }
    } else {
      cat(
        "  Economic Interpretation: No significant change in price sensitivity\n"
      )
    }
  }
}

# Print all test results
cat("\n--- EXPENDITURE ELASTICITY CHANGES ---")
print_elasticity_test_results(
  ta_expenditure_test_2good_enhanced,
  "2-Good Model"
)
print_elasticity_test_results(
  ta_expenditure_test_3good_enhanced,
  "3-Good Model"
)

cat("\n--- OWN-PRICE ELASTICITY CHANGES ---")
print_elasticity_test_results(ta_own_price_test_2good, "2-Good Model")
print_elasticity_test_results(ta_own_price_test_3good, "3-Good Model")

### ENHANCED BEAUTIFUL SUMMARY TABLE WITH ALL ELASTICITY TESTS ###

create_comprehensive_elasticity_table <- function(
  exp_test_2good,
  exp_test_3good,
  price_test_2good,
  price_test_3good
) {
  # This function creates a comprehensive table that includes both expenditure
  # and own-price elasticity comparisons across time periods and model specifications

  # Helper function to format test results for table display
  format_test_result <- function(test_result, digits = 3) {
    if (is.null(test_result)) {
      return("N/A")
    }

    # Format the change with significance indicator and confidence interval
    change_text <- round(test_result$mean_difference, digits)
    significance_indicator <- ifelse(test_result$is_significant, "***", "")
    ci_text <- paste0(
      "[",
      round(test_result$confidence_interval[1], digits),
      ", ",
      round(test_result$confidence_interval[2], digits),
      "]"
    )
    p_text <- paste0("p=", round(test_result$p_value, 3))

    return(paste0(
      change_text,
      significance_indicator,
      " ",
      ci_text,
      " (",
      p_text,
      ")"
    ))
  }

  # Helper function to format elasticity point estimates
  format_elasticity_point <- function(test_result, period, digits = 3) {
    if (is.null(test_result)) {
      return("N/A")
    }

    if (period == 1) {
      mean_val <- test_result$period_1_mean
      se_val <- test_result$period_1_std
    } else {
      mean_val <- test_result$period_2_mean
      se_val <- test_result$period_2_std
    }

    return(paste0(round(mean_val, digits), " (", round(se_val, digits), ")"))
  }

  # Create the comprehensive summary table
  comprehensive_table <- data.frame(
    "Model_Specification" = c(
      "2-Good System (TA vs Other)",
      "3-Good System (TA, Food, Other)",
      "2-Good System (TA vs Other)",
      "3-Good System (TA, Food, Other)"
    ),

    "Elasticity_Type" = c(
      "Expenditure Elasticity",
      "Expenditure Elasticity",
      "Own-Price Elasticity",
      "Own-Price Elasticity"
    ),

    "SSU_2021_Estimate" = c(
      format_elasticity_point(exp_test_2good, 1),
      format_elasticity_point(exp_test_3good, 1),
      format_elasticity_point(price_test_2good, 1),
      format_elasticity_point(price_test_3good, 1)
    ),

    "UNICEF_2024_Estimate" = c(
      format_elasticity_point(exp_test_2good, 2),
      format_elasticity_point(exp_test_3good, 2),
      format_elasticity_point(price_test_2good, 2),
      format_elasticity_point(price_test_3good, 2)
    ),

    "Change_2024_minus_2021" = c(
      format_test_result(exp_test_2good),
      format_test_result(exp_test_3good),
      format_test_result(price_test_2good),
      format_test_result(price_test_3good)
    ),

    stringsAsFactors = FALSE
  )

  # Clean up column names for display
  names(comprehensive_table) <- c(
    "Model Specification",
    "Elasticity Type",
    "SSU 2021\nMean (SE)",
    "UNICEF 2024\nMean (SE)",
    "Change (2024-2021)\n[95% CI] (p-value)"
  )

  return(comprehensive_table)
}

# Create the comprehensive table
comprehensive_elasticity_table <- create_comprehensive_elasticity_table(
  exp_test_2good = ta_expenditure_test_2good_enhanced,
  exp_test_3good = ta_expenditure_test_3good_enhanced,
  price_test_2good = ta_own_price_test_2good,
  price_test_3good = ta_own_price_test_3good
)

cat("\n=== COMPREHENSIVE ELASTICITY ANALYSIS TABLE ===\n")
print(comprehensive_elasticity_table)


cat("\n=== BOOTSTRAP QUALITY DIAGNOSTICS ===\n")

analyze_bootstrap_quality <- function(bootstrap_results, model_name) {
  # This function helps you assess whether your bootstrap procedure
  # generated reliable results

  cat("\n--- ", model_name, " ---\n")

  # Check sample sizes
  n_successful <- length(bootstrap_results)
  cat("Successful bootstrap iterations:", n_successful, "\n")

  if (n_successful < 500) {
    cat(
      "WARNING: Low number of successful bootstrap iterations may affect reliability\n"
    )
  }

  # Check for extreme outliers in key elasticities
  ta_expenditure_elasticities <- sapply(bootstrap_results, function(x) {
    x$expenditure_elasticities["ta"]
  })
  ta_expenditure_elasticities <- ta_expenditure_elasticities[
    !is.na(ta_expenditure_elasticities)
  ]

  cat("Tobacco+Alcohol Expenditure Elasticity Distribution:\n")
  cat("  Mean:", round(mean(ta_expenditure_elasticities), 4), "\n")
  cat("  Standard Deviation:", round(sd(ta_expenditure_elasticities), 4), "\n")
  cat(
    "  Range: [",
    round(min(ta_expenditure_elasticities), 4),
    ", ",
    round(max(ta_expenditure_elasticities), 4),
    "]\n"
  )

  # Check for theoretical consistency (expenditure elasticities should generally be positive)
  negative_expenditure <- sum(ta_expenditure_elasticities < 0)
  cat(
    "  Bootstrap samples with negative expenditure elasticity:",
    negative_expenditure,
    "(",
    round(100 * negative_expenditure / length(ta_expenditure_elasticities), 1),
    "%)\n"
  )

  if (negative_expenditure > 0.05 * length(ta_expenditure_elasticities)) {
    cat(
      "  NOTE: High proportion of negative expenditure elasticities may indicate model issues\n"
    )
  }
}

# Run diagnostics for all models
analyze_bootstrap_quality(ssu_bootstrap_1, "SSU Model 1 (2-good)")
analyze_bootstrap_quality(unicef_bootstrap_1, "UNICEF Model 1 (2-good)")
analyze_bootstrap_quality(ssu_bootstrap_2, "SSU Model 2 (3-good)")
analyze_bootstrap_quality(unicef_bootstrap_2, "UNICEF Model 2 (3-good)")
