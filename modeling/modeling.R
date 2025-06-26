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
  lubridate # For date manipulation
)

p_loaded() # Inspect loaded packages
conflict_scout() # Check for conflicts

# Resolve conflicts by explicitly stating preferences for dplyr functions
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::summarise)

### Load the datasets ###

# State Statistics Service of Ukraine (SSU) 2021 data
ssu_21_h <- read_sav("ssu_data/Households_microdani_anonimni_2021.sav") # Households
ssu_21_w <- read_sav("ssu_data/Members_microdani_anonimni_2021.sav") # Members

# UNICEF 2024 data (collected Dec 2023 - Feb 2024)
unicef_24_h <- read_excel(
  "unicef_data/teams-UKR-PrgmEff-UCO KnowledgeUNICEF-SESH Household-2.0.xlsx",
  skip = 4
)
unicef_24_m <- read_excel(
  "unicef_data/teams-UKR-PrgmEff-UCO KnowledgeUNICEF-SESHS IND 10042024 FIN-2.0.xlsx",
  skip = 3
)

### Load and process the CPI data ###

cpi_data <- read_excel(
  "price_data/dataset_2025-06-18T13_53_54.269907203Z_DEFAULT_INTEGRATION_SSSU_DF_PRICE_CHANGE_CONSUMER_GOODS_SERVICE_24.0.0.xlsx"
)

# Clean and filter CPI data
cpi_processed <- cpi_data %>%
  mutate(
    year = as.numeric(str_sub(Період, 1, 4)),
    month = as.numeric(str_sub(Період, 7, 8)),
    region = `Територіальний розріз`,
    cpi_value = as.numeric(`Значення cпостереження`),
    product_type = `Тип товарів і послуг`
  ) %>%
  filter(
    year %in% c(2021:2024),
    Періодичність == "Місячна",
    `Базисний період` == "До попереднього місяця",
    !is.na(cpi_value)
  ) %>%
  select(region, year, month, cpi_value, product_type)

# Function to create indexed CPI for different time periods
calculate_mixed_periods_regional <- function(monthly_cpi_data) {
  monthly_indexed <- monthly_cpi_data %>%
    arrange(region, year, month) %>%
    group_by(region) %>%
    mutate(
      growth_factor = cpi_value / 100,
      cpi_cumulative = 100 * cumprod(growth_factor)
    ) %>%
    ungroup()

  quarterly <- monthly_indexed %>%
    mutate(quarter = ceiling(month / 3)) %>%
    filter(!(year == 2023 & month == 12) & !(year == 2024 & month %in% 1:2)) %>%
    group_by(region, year, quarter) %>%
    summarise(cpi_indexed = mean(cpi_cumulative), .groups = 'drop') %>%
    mutate(period_id = paste0(year, "Q", quarter))

  survey_period <- monthly_indexed %>%
    filter((year == 2023 & month == 12) | (year == 2024 & month %in% 1:2)) %>%
    group_by(region) %>%
    summarise(cpi_indexed = mean(cpi_cumulative), .groups = 'drop') %>%
    mutate(period_id = "2024_SURVEY_DEC23_FEB24")

  combined <- bind_rows(quarterly, survey_period) %>%
    group_by(region) %>%
    mutate(
      base_value = first(cpi_indexed[period_id == "2021Q1"]),
      cpi_rebased = (cpi_indexed / base_value) * 100
    ) %>%
    ungroup() %>%
    select(region, period_id, cpi_rebased)
  return(combined)
}

# Create indexed CPI for each category
categories <- list(
  "Індекс споживчих цін" = "cpi_general",
  "Алкогольні напої, тютюнові вироби" = "cpi_t_a"
)

all_cpi_indexed <- names(categories) %>%
  map(
    ~ {
      cpi_processed %>%
        filter(product_type == .x) %>%
        calculate_mixed_periods_regional() %>%
        rename(!!categories[[.x]] := cpi_rebased)
    }
  ) %>%
  reduce(full_join, by = c("region", "period_id"))

# Extract national-level data to be used as a fallback for missing regional data
national_fallback_cpi <- all_cpi_indexed %>%
  filter(region == "Україна") %>%
  select(
    period_id,
    national_cpi_alcohol = cpi_alcohol,
    national_cpi_tobacco = cpi_tobacco
  )

# Join the national fallbacks and use coalesce to fill in NAs directly
all_cpi_indexed <- all_cpi_indexed %>%
  left_join(national_fallback_cpi, by = "period_id") %>%
  mutate(
    cpi_alcohol = coalesce(cpi_alcohol, national_cpi_alcohol),
    cpi_tobacco = coalesce(cpi_tobacco, national_cpi_tobacco)
  ) %>%
  select(-national_cpi_alcohol, -national_cpi_tobacco)

### Load and process the average price data ###

avg_price_data <- read_excel(
  "price_data/dataset_2025-06-18T11_06_01.402914933Z_DEFAULT_INTEGRATION_SSSU_DF_PRICE_CHANGE_CONSUMER_GOODS_SERVICE_24.0.0.xlsx"
)

avg_price_data_clean <- avg_price_data %>%
  mutate(
    year = as.numeric(str_sub(Період, 1, 4)),
    month = as.numeric(str_sub(Період, 7, 8))
  ) %>%
  select(-c("Базисний період", "Період", "Показник"))

### Process average price data and calculate real prices ###
avg_price_processed <- avg_price_data_clean %>%
  mutate(
    region = `Територіальний розріз`,
    product = `Тип товарів і послуг`,
    price_nominal = as.numeric(`Значення cпостереження`),
    year = year,
    month = month,
    date = ymd(paste(year, month, "01", sep = "-"))
  ) %>%
  filter(!is.na(price_nominal)) %>%
  select(region, product, price_nominal, year, month, date)

# Define product categories
tobacco_products <- c(
  "Сигарети з фільтром вітчизняних марок",
  "Сигарети з фільтром медіум класу",
  "Сигарети з фільтром преміум класу",
  "Сигарети без фільтру"
)
alcohol_products <- c(
  "Горілка",
  "Вина столові вітчизняні",
  "Пиво вітчизняних марок"
)

avg_price_processed <- avg_price_processed %>%
  mutate(
    product_category = case_when(
      product %in% tobacco_products ~ "tobacco",
      product %in% alcohol_products ~ "alcohol",
      TRUE ~ "other"
    )
  )

# Function to assign quarters and special survey period
assign_period <- function(year, month) {
  if (year == 2023 & month == 12) {
    return("2024_SURVEY_DEC23_FEB24")
  } else if (year == 2024 & month %in% 1:2) {
    return("2024_SURVEY_DEC23_FEB24")
  } else {
    quarter <- ceiling(month / 3)
    return(paste0(year, "Q", quarter))
  }
}

avg_price_with_period <- avg_price_processed %>%
  mutate(period_id = map2_chr(year, month, assign_period))

quarterly_avg_prices <- avg_price_with_period %>%
  group_by(region, product, product_category, period_id) %>%
  summarise(
    avg_nominal_price = mean(price_nominal, na.rm = TRUE),
    n_months = n(),
    .groups = 'drop'
  )

# Function to deflate prices using CPI data
deflate_prices <- function(price_data, cpi_data, category_name) {
  is_national_only <- length(unique(cpi_data$region)) == 1 &&
    unique(cpi_data$region)[1] == "Україна"
  if (is_national_only) {
    national_cpi <- cpi_data %>%
      filter(region == "Україна") %>%
      select(period_id, cpi_rebased)
    deflated <- price_data %>%
      left_join(national_cpi, by = "period_id") %>%
      mutate(
        real_price = (avg_nominal_price / cpi_rebased) * 100,
        price_category = category_name
      )
  } else {
    deflated <- price_data %>%
      left_join(
        cpi_data %>% select(region, period_id, cpi_rebased),
        by = c("region", "period_id")
      ) %>%
      mutate(
        real_price = (avg_nominal_price / cpi_rebased) * 100,
        price_category = category_name
      )
  }
  return(deflated)
}


# Calculate real prices for each category
tobacco_real_prices <- quarterly_avg_prices %>%
  filter(product_category == "tobacco") %>%
  deflate_prices(
    all_cpi_indexed %>% select(region, period_id, cpi_rebased = cpi_t_a),
    "tobacco"
  )

alcohol_real_prices <- quarterly_avg_prices %>%
  filter(product_category == "alcohol") %>%
  deflate_prices(
    all_cpi_indexed %>% select(region, period_id, cpi_rebased = cpi_t_a),
    "alcohol"
  )

t_and_a_real_prices <- quarterly_avg_prices %>%
  filter(product_category %in% c("tobacco", "alcohol")) %>%
  deflate_prices(
    all_cpi_indexed %>% select(region, period_id, cpi_rebased = cpi_t_a),
    "tobacco_alcohol"
  )

# Combine all real prices
all_real_prices <- bind_rows(
  tobacco_real_prices,
  alcohol_real_prices,
  t_and_a_real_prices
)

# Create summary by product category, period, and region
price_summary <- all_real_prices %>%
  group_by(price_category, period_id, region) %>%
  summarise(
    avg_real_price = mean(real_price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = price_category,
    values_from = avg_real_price,
    names_prefix = "price_"
  )


### Main data processing ###

# Process SSU 2021 data
ssu_smokers <- ssu_21_w %>%
  group_by(code_fam) %>%
  summarise(
    n_smokers = sum(smoking == 1, na.rm = TRUE),
    head_sex = first(SEX),
    head_age_cat = first(age),
    head_educ = first(L_EDUC_M)
  ) %>%
  mutate(head_sex = ifelse(head_sex == 1, "Male", "Female"))

correct_hh_size <- ssu_21_w %>%
  group_by(code_fam) %>%
  summarise(hh_size_corrected = max(N_MEMBER, na.rm = TRUE), .groups = 'drop')

ssu_21_processed <- ssu_21_h %>%
  left_join(correct_hh_size, by = "code_fam") %>%
  left_join(ssu_smokers, by = "code_fam") %>%
  transmute(
    hh_id = as.character(code_fam),
    weight = w_q,
    region = case_when(
      cod_obl == 5 ~ "Вінницька",
      cod_obl == 7 ~ "Волинська",
      cod_obl == 12 ~ "Дніпропетровська",
      cod_obl == 14 ~ "Донецька",
      cod_obl == 18 ~ "Житомирська",
      cod_obl == 21 ~ "Закарпатська",
      cod_obl == 23 ~ "Запорізька",
      cod_obl == 26 ~ "Івано-Франківська",
      cod_obl == 32 ~ "Київська",
      cod_obl == 35 ~ "Кіровоградська",
      cod_obl == 44 ~ "Луганська",
      cod_obl == 46 ~ "Львівська",
      cod_obl == 48 ~ "Миколаївська",
      cod_obl == 51 ~ "Одеська",
      cod_obl == 53 ~ "Полтавська",
      cod_obl == 56 ~ "Рівненська",
      cod_obl == 59 ~ "Сумська",
      cod_obl == 61 ~ "Тернопільська",
      cod_obl == 63 ~ "Харківська",
      cod_obl == 65 ~ "Херсонська",
      cod_obl == 68 ~ "Хмельницька",
      cod_obl == 71 ~ "Черкаська",
      cod_obl == 73 ~ "Чернівецька",
      cod_obl == 74 ~ "Чернігівська",
      cod_obl == 80 ~ "м. Київ",
      TRUE ~ "Інша"
    ),
    settlement_type = ifelse(tp_ns_p %in% c(1, 2), "Urban", "Rural"),
    hh_size = hh_size_corrected, # Use corrected size instead of hsize
    has_children = ifelse(type_dom == 1, 1, 0),
    head_sex,
    head_age_cat,
    head_educ,
    n_smokers,
    exp_tobacco = h0221,
    exp_alcohol = h0211 + h0212 + h0213,
    # Combine food into the 'other' category
    exp_other = h00 - (h0221 + h0211 + h0212 + h0213),
    exp_total_consumption = h00,
    income_total = totalinc,
    kvart = kvart_kd,
    year = "2021"
  ) %>%
  mutate(period_id = paste0(year, "Q", kvart))

# Process UNICEF 2024 data
unicef_head_info <- unicef_24_m %>%
  filter(memb == 1) %>%
  transmute(
    hh_id = as.character(KEY_QUEST),
    head_sex = ifelse(IND_A3 == "чоловіча", "Male", "Female"),
    head_age_cat = case_when(
      IND_A2 < 18 ~ 1,
      IND_A2 >= 18 & IND_A2 <= 35 ~ 2,
      IND_A2 >= 36 & IND_A2 <= 55 ~ 3,
      IND_A2 >= 56 & IND_A2 <= 59 ~ 4,
      IND_A2 >= 60 ~ 5,
      TRUE ~ NA_real_
    ),
    head_educ = IND_A7
  )

unicef_24_processed <- unicef_24_h %>%
  left_join(unicef_head_info, by = c("KEY_QUEST" = "hh_id")) %>%
  mutate(across(
    c(starts_with("G"), "V1", "dity_n", "hh_size", "WGT_Fin_NEW"),
    as.numeric
  )) %>%
  mutate(exp_utilities = coalesce(G3_2_1, G3_2_2)) %>%
  rowwise() %>%
  mutate(
    exp_tobacco_alcohol = G1_9,
    # Combine food into the 'other' category
    exp_other = sum(
      c(
        G1_1,
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
    exp_total_consumption = sum(exp_tobacco_alcohol, exp_other, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    hh_id = KEY_QUEST,
    weight = WGT_Fin_NEW,
    region = OBLAST,
    settlement_type = SETTL_TYPE,
    hh_size = hh_size,
    is_idp = ifelse(D1_Mult9 == "Так", 1, 0),
    has_children = ifelse(dity_n > 0, 1, 0),
    head_sex,
    head_age_cat,
    head_educ,
    exp_tobacco_alcohol,
    exp_other,
    exp_total_consumption,
    income_total = V1,
    period_id = "2024_SURVEY_DEC23_FEB24",
    year = "2024"
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
      str_starts(region, "м. Київ") ~ "м. Київ",
      TRUE ~ region
    )
  )

### Deflate SSU and UNICEF Data and prepare for AIDS ###

# Function to deflate expenditures and calculate budget shares
prepare_aids_data <- function(df, cpi_df, price_df, is_unicef = FALSE) {
  df <- df %>%
    left_join(cpi_df, by = c("region", "period_id")) %>%
    left_join(price_df, by = c("region", "period_id"))

  if (is_unicef) {
    df <- df %>%
      mutate(
        exp_tobacco_alcohol_real = exp_tobacco_alcohol / (cpi_t_a / 100),
        exp_other_real = exp_other / (cpi_general / 100) # 'other' now includes food
      ) %>%
      mutate(
        exp_total_consumption_real = exp_tobacco_alcohol_real + exp_other_real
      ) %>%
      mutate(
        wTA = exp_tobacco_alcohol_real / exp_total_consumption_real,
        wOther = exp_other_real / exp_total_consumption_real
      )
  } else {
    df <- df %>%
      mutate(
        exp_tobacco_real = exp_tobacco / (cpi_t_a / 100),
        exp_alcohol_real = exp_alcohol / (cpi_t_a / 100),
        exp_other_real = exp_other / (cpi_general / 100) # 'other' now includes food
      ) %>%
      mutate(
        exp_total_consumption_real = exp_tobacco_real +
          exp_alcohol_real +
          exp_other_real
      ) %>%
      mutate(
        wTobacco = exp_tobacco_real / exp_total_consumption_real,
        wAlcohol = exp_alcohol_real / exp_total_consumption_real,
        wOther = exp_other_real / exp_total_consumption_real
      )
  }
  return(df)
}

ssu_aids <- prepare_aids_data(
  ssu_21_processed,
  all_cpi_indexed,
  price_summary
)


unicef_aids <- prepare_aids_data(
  unicef_24_processed,
  all_cpi_indexed,
  price_summary,
  is_unicef = TRUE
)

ssu_clean <- ssu_aids %>%
  filter(
    # Remove households with zero or missing total expenditure
    !is.na(exp_total_consumption_real),
    exp_total_consumption_real > 0,
    # Remove households with missing location data (needed for price variation)
    !is.na(region),
    !is.na(period_id),
    # Remove households with impossible budget shares
    !is.na(wTobacco),
    !is.na(wAlcohol),
    !is.na(wOther),
    wTobacco >= 0,
    wAlcohol >= 0,
    wOther >= 0,
    # Budget shares should sum to approximately 1 (allow small rounding errors)
    abs(wTobacco + wAlcohol + wOther - 1) < 0.01,
    # Remove households with missing price data
    !is.na(price_tobacco),
    !is.na(price_alcohol)
  )


ssu_clean <- ssu_clean %>%
  mutate(
    wTA_combined = wTobacco + wAlcohol,
    # Log prices for each good category
    log_P_combined = log(price_tobacco_alcohol),
    log_p_other = log(cpi_general),

    # Create Stone price index: P = Σ w_i * log(p_i)
    # This is a simpler approximation to the ideal AIDS price index
    log_P_stone = wTA_combined *
      log_P_combined +
      wOther * log_p_other,

    # Real expenditure deflated by the Stone price index
    # This is the key income term in AIDS: log(x/P)
    log_real_exp = log(exp_total_consumption_real) - log_P_stone,

    # Create demographic control variables
    log_hh_size = log(hh_size),
    urban = ifelse(settlement_type == "Urban", 1, 0),
    has_children_dum = ifelse(has_children == 1, 1, 0),
    n_smokers_dum = ifelse(n_smokers > 0, 1, 0)
  ) %>%
  filter(
    !is.na(wTA_combined),
    !is.na(wOther),
    abs(wTA_combined + wOther - 1) < 0.01
  )

combined_ta_eq <- wTA_combined ~
  log_P_combined +
    log_p_other +
    log_real_exp_combined +
    log_hh_size +
    urban +
    has_children_dum +
    n_smokers_dum

# For two-good system, we only need to estimate one equation (other is residual)
ssu_combined_model <- lm(combined_ta_eq, data = ssu_combined_clean)

# Display results
print("SSU Combined Tobacco+Alcohol Model Results:")
summary(ssu_combined_model)

# Calculate elasticities for combined system
avg_w_ta_combined <- mean(ssu_combined_clean$wTA_combined, na.rm = TRUE)

ssu_combined_coefs <- coef(ssu_combined_model)

# Combined tobacco+alcohol elasticities
ta_combined_own_price_elast <- (ssu_combined_coefs["log_P_combined"] /
  avg_w_ta_combined) -
  1
ta_combined_income_elast <- (ssu_combined_coefs["log_real_exp_combined"] /
  avg_w_ta_combined) +
  1

print("=== SSU 2021 COMBINED TOBACCO+ALCOHOL ELASTICITIES ===")
print(paste(
  "Combined tobacco+alcohol own-price elasticity:",
  round(ta_combined_own_price_elast, 3)
))
print(paste(
  "Combined tobacco+alcohol income elasticity:",
  round(ta_combined_income_elast, 3)
))


unicef_clean <- unicef_aids %>%
  filter(
    # Basic data quality filters
    !is.na(exp_total_consumption_real),
    exp_total_consumption_real > 0,
    !is.na(region),
    !is.na(period_id),
    # UNICEF has only two categories: tobacco+alcohol combined, and other
    !is.na(wTA),
    !is.na(wOther),
    wTA >= 0,
    wOther >= 0,
    abs(wTA + wOther - 1) < 0.01,
    # Need price data for estimation
    !is.na(price_tobacco_alcohol)
  )

print(paste("UNICEF dataset: Cleaned to", nrow(unicef_clean), "households"))

# Create log price variables for UNICEF two-good system
unicef_clean <- unicef_clean %>%
  mutate(
    # Log prices - UNICEF only has combined tobacco+alcohol price
    log_p_ta = log(price_tobacco_alcohol),
    log_p_other = log(cpi_general),

    # Stone price index for two-good system
    log_P_stone = wTA * log_p_ta + wOther * log_p_other,

    # Real expenditure deflated by price index
    log_real_exp = log(exp_total_consumption_real) - log_P_stone,

    # Demographic controls (UNICEF has different variables than SSU)
    log_hh_size = log(hh_size),
    urban = ifelse(settlement_type == "urban", 1, 0),
    has_children_dum = ifelse(has_children == 1, 1, 0),
    # UNICEF doesn't have smoker count, so we use IDP status as additional control
    is_idp_dum = ifelse(is_idp == 1, 1, 0)
  ) %>%
  filter(wTA < 0.51)

### UNICEF ESTIMATION: Two-good AIDS model (Tobacco+Alcohol, Other) ###

print("--- Estimating UNICEF Tobacco+Alcohol AIDS Model ---")

# Tobacco+alcohol budget share equation for UNICEF data
unicef_ta_eq <- wTA ~
  log_p_ta +
    log_p_other +
    log_real_exp +
    log_hh_size +
    has_children_dum +
    urban +
    is_idp_dum

# Estimate single equation model (other equation is residual)
unicef_model <- lm(unicef_ta_eq, data = unicef_clean)

# Display estimation results
print("UNICEF 2024 Tobacco+Alcohol Model Results:")
summary(unicef_model)

# Calculate elasticities for UNICEF data
avg_w_ta_unicef <- mean(unicef_clean$wTA, na.rm = TRUE)

unicef_coefs <- coef(unicef_model)

# UNICEF tobacco+alcohol elasticities
unicef_ta_own_price_elast <- (unicef_coefs["log_p_ta"] / avg_w_ta_unicef) - 1
unicef_ta_income_elast <- (unicef_coefs["log_real_exp"] / avg_w_ta_unicef) + 1

print("=== UNICEF 2024 TOBACCO+ALCOHOL ELASTICITIES ===")
print(paste(
  "Tobacco+alcohol own-price elasticity:",
  round(unicef_ta_own_price_elast, 3)
))
print(paste(
  "Tobacco+alcohol income elasticity:",
  round(unicef_ta_income_elast, 3)
))
