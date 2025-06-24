### Install and load necessary packages ###

if (!require(pacman)) {
  install.packages("pacman")
}


pacman::p_load(
  haven,
  tidyverse,
  readxl,
  conflicted,
  micEconAids,
  systemfit,
  censReg,
  sampleSelection,
  car,
  sandwich,
  lmtest
)

p_loaded() # inspect loaded packages
conflict_scout() # check for conflicts

# Resolve conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)


### Load the datasets ###

ssu_21_h <- read_sav(
  "ssu_data/Households_microdani_anonimni_2021.sav"
) # households SSU 2021

ssu_21_w <- read_sav(
  "ssu_data/Members_microdani_anonimni_2021.sav"
) # members SSU 2021

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

cpi_processed <- cpi_data %>%
  mutate(
    year = as.numeric(stringr::str_sub(Період, 1, 4)),
    month = as.numeric(stringr::str_sub(Період, 7, 8)),
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
      base_value = cpi_indexed[period_id == "2021Q1"],
      cpi_rebased = (cpi_indexed / base_value) * 100
    ) %>%
    ungroup() %>%
    select(region, period_id, cpi_rebased)
  return(combined)
}

# Create indexed CPI for each category
categories <- list(
  "Індекс споживчих цін" = "cpi_general",
  "Алкогольні напої, тютюнові вироби" = "cpi_t_a",
  "Алкогольні напої" = "cpi_alcohol",
  "Тютюнові вироби" = "cpi_tobacco"
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

# Extract national-level data to be used as a fallback
national_fallback_cpi <- all_cpi_indexed %>%
  filter(region == "Україна") %>%
  select(
    period_id,
    national_cpi_alcohol = cpi_alcohol,
    national_cpi_tobacco = cpi_tobacco
  )

# Join the national fallbacks and use coalesce to fill in NAs directly into the main CPI table
all_cpi_indexed <- all_cpi_indexed %>%
  left_join(national_fallback_cpi, by = "period_id") %>%
  mutate(
    cpi_alcohol = coalesce(cpi_alcohol, national_cpi_alcohol),
    cpi_tobacco = coalesce(cpi_tobacco, national_cpi_tobacco)
  ) %>%
  # Clean up the temporary national columns
  select(-national_cpi_alcohol, -national_cpi_tobacco)


### Load and process the average price data ###

avg_price_data <- read_excel(
  "price_data/dataset_2025-06-18T11_06_01.402914933Z_DEFAULT_INTEGRATION_SSSU_DF_PRICE_CHANGE_CONSUMER_GOODS_SERVICE_24.0.0.xlsx"
)

avg_price_data_clean <- avg_price_data %>%
  dplyr::mutate(
    Рік = as.numeric(stringr::str_sub(Період, 1, 4)),
    Місяць = as.numeric(stringr::str_sub(Період, 7, 8))
  ) %>%
  select(
    -c(
      "Базисний період",
      "Період",
      "Показник"
    )
  )

### Inspect the data structure ###

glimpse(avg_price_data_clean)
unique(avg_price_data_clean$Періодичність)
unique(avg_price_data_clean$`Тип товарів і послуг`)


### Process average price data and calculate real prices ###

# First, let's process the average price data
avg_price_processed <- avg_price_data_clean %>%
  mutate(
    region = `Територіальний розріз`,
    product = `Тип товарів і послуг`,
    price_nominal = as.numeric(`Значення cпостереження`),
    year = Рік,
    month = Місяць,
    date = ymd(paste(year, month, "01", sep = "-"))
  ) %>%
  dplyr::filter(!is.na(price_nominal)) %>%
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

deflate_prices <- function(price_data, cpi_data, category_name) {
  # Check if CPI data is only at national level
  is_national_only <- length(unique(cpi_data$region)) == 1 &
    unique(cpi_data$region)[1] == "Україна"

  if (is_national_only) {
    # For national-only CPI (tobacco and alcohol), join only by period
    national_cpi <- cpi_data %>%
      dplyr::filter(region == "Україна") %>%
      select(period_id, cpi_indexed)

    deflated <- price_data %>%
      left_join(
        national_cpi,
        by = "period_id"
      ) %>%
      mutate(
        # Calculate real price (base period = Q1 2021)
        real_price = (avg_nominal_price / cpi_indexed) * 100,
        price_category = category_name
      )
  } else {
    # For regional CPI data (combined tobacco & alcohol), join by both region and period
    deflated <- price_data %>%
      left_join(
        cpi_data %>% select(region, period_id, cpi_indexed),
        by = c("region", "period_id")
      ) %>%
      mutate(
        # Calculate real price (base period = Q1 2021)
        real_price = (avg_nominal_price / cpi_indexed) * 100,
        price_category = category_name
      )
  }

  return(deflated)
}

tobacco_real_prices <- quarterly_avg_prices %>%
  dplyr::filter(product_category == "tobacco") %>%
  deflate_prices(
    all_cpi_indexed %>%
      select(region, period_id, cpi_indexed = cpi_tobacco),
    "tobacco"
  )

alcohol_real_prices <- quarterly_avg_prices %>%
  dplyr::filter(product_category == "alcohol") %>%
  deflate_prices(
    all_cpi_indexed %>%
      select(region, period_id, cpi_indexed = cpi_alcohol),
    "alcohol"
  )

t_and_a_real_prices <- quarterly_avg_prices %>%
  dplyr::filter(
    product_category == "tobacco" | product_category == "alcohol"
  ) %>%
  deflate_prices(
    all_cpi_indexed %>%
      select(region, period_id, cpi_indexed = cpi_t_a),
    "tobacco & alcohol"
  )

# Combine all real prices
all_real_prices <- bind_rows(
  tobacco_real_prices,
  alcohol_real_prices,
  t_and_a_real_prices
)

# Create summary by product category and period
price_summary <- all_real_prices %>%
  group_by(price_category, period_id, region) %>%
  summarise(
    avg_real_price = mean(real_price, na.rm = TRUE),
    avg_nominal_price = mean(avg_nominal_price, na.rm = TRUE),
    n_products = n_distinct(product_category),
    .groups = 'drop'
  )

# National level summary (Ukraine as a whole) - aggregate from all regions
national_price_summary <- all_real_prices %>%
  group_by(price_category, period_id) %>%
  summarise(
    avg_real_price = mean(real_price, na.rm = TRUE),
    avg_nominal_price = mean(avg_nominal_price, na.rm = TRUE),
    n_products = n_distinct(product),
    products = paste(unique(product), collapse = ", "),
    .groups = 'drop'
  ) %>%
  arrange(price_category, period_id)

# Calculate price indices (Q1 2021 = 100) for each category
price_indices <- national_price_summary %>%
  group_by(price_category) %>%
  mutate(
    base_real_price = avg_real_price[period_id == "2021Q1"],
    base_nominal_price = avg_nominal_price[period_id == "2021Q1"],
    real_price_index = (avg_real_price / base_real_price) * 100,
    nominal_price_index = (avg_nominal_price / base_nominal_price) * 100
  ) %>%
  ungroup()


# Plot real prices by category
real_price_plot <- ggplot(
  national_price_summary %>%
    dplyr::filter(period_id != "2024_SURVEY_DEC23_FEB24"), # Exclude survey period for cleaner plot
  aes(
    x = period_id,
    y = avg_real_price,
    color = price_category,
    group = price_category
  )
) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Real Average Prices by Product Category",
    subtitle = "Deflated to Q1 2021 prices",
    x = "Period",
    y = "Real Price (UAH, Q1 2021 = 100)",
    color = "Product Category"
  ) +
  scale_color_manual(
    values = c(
      "tobacco" = "#D55E00",
      "alcohol" = "#0072B2",
      "tobacco & alcohol" = "#009E73"
    ),
    labels = c("Tobacco", "Alcohol", "Tobacco & Alcohol Combined")
  )

print(real_price_plot)


### Main data processing ###

ssu_smokers <- ssu_21_w %>%
  group_by(code_fam) %>%
  summarise(
    n_smokers = sum(smoking == 1, na.rm = TRUE),
    # Get info for household head (assuming first member listed is head)
    head_sex = dplyr::first(SEX),
    head_age_cat = dplyr::first(age),
    head_educ = dplyr::first(L_EDUC_M)
  ) %>%
  mutate(
    # Harmonize head's sex to match UNICEF (1=Male, 2=Female)
    head_sex = case_when(
      head_sex == 1 ~ "Male",
      head_sex == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  )

ssu_21_processed <- ssu_21_h %>%
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

    settlement_type = case_when(
      tp_ns_p %in% c(1, 2) ~ "Urban",
      tp_ns_p == 3 ~ "Rural",
      TRUE ~ NA_character_
    ),

    hh_size = hsize,
    has_children = ifelse(
      type_dom == 1,
      1,
      0
    ),

    head_sex,
    head_age_cat,
    head_educ,
    n_smokers,

    # --- AIDS Expenditure Categories (Nominal) ---
    exp_food = h01,
    exp_tobacco = h0221,
    exp_alcohol = h0211 + h0212 + h0213,
    # "Other" is Total Consumption minus Food, Alcohol, and Tobacco
    exp_other = h00 - (h01 + h0221 + h0211 + h0212 + h0213),
    # Total consumption expenditure for budget shares
    exp_total_consumption = h00,
    income_total = totalinc,

    kvart = kvart_kd,
    year = "2021"
  ) %>%
  mutate(period_id = paste0(year, "Q", kvart))

unicef_head_info <- unicef_24_m %>%
  dplyr::filter(memb == 1) %>%
  transmute(
    hh_id = as.character(KEY_QUEST),
    head_sex = case_when(
      IND_A3 == "чоловіча" ~ 1,
      IND_A3 == "жіноча" ~ 2,
      TRUE ~ NA_real_ # Changed to NA_real_ for numeric consistency
    ),
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
  # Ensure all columns used for calculation are numeric.
  mutate(across(
    .cols = c(starts_with("G"), "V1", "dity_n", "hh_size", "WGT_Fin_NEW"),
    .fns = as.numeric
  )) %>%
  # Use coalesce to handle heating vs. non-heating utility costs
  mutate(
    exp_utilities = coalesce(G3_2_1, G3_2_2)
  ) %>%
  rowwise() %>%
  mutate(
    # --- AIDS Expenditure Categories (Nominal) ---
    exp_food = G1_1,
    # This dataset combines Alcohol and Tobacco
    exp_tobacco_alcohol = G1_9,
    # Sum all other consumption expenditure items explicitly
    exp_other = sum(
      c(
        # Non-food goods
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
        # Medical
        G2_5_1,
        G2_5_2,
        G2_5_3,
        G2_5_4,
        G2_5_5,
        # Education
        G2_8_1,
        G2_8_2,
        G2_8_3,
        G2_8_4,
        G2_8_5,
        # Education
        G2_8_6,
        # Recreation
        G2_11,
        # Transport
        G2_13_1,
        G2_13_2,
        # Services
        G2_15_1_2,
        G2_15_2_2,
        G2_15_3_2,
        G2_15_4_2,
        G2_15_5_2,
        G2_15_6_2,
        # More Services
        G2_15_7_2,
        G2_15_8_2,
        G2_15_9_2,
        G2_15_10_2,
        G2_15_11_2,
        # Rent
        G3_1,
        # Housing utilities
        exp_utilities
      ),
      na.rm = TRUE
    ),

    # Calculate total consumption for budget shares
    exp_total_consumption = sum(
      exp_food,
      exp_tobacco_alcohol,
      exp_other,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  transmute(
    hh_id = KEY_QUEST,
    weight = WGT_Fin_NEW,
    region = OBLAST,
    settlement_type = SETTL_TYPE,
    hh_size = hh_size,
    is_idp = ifelse(D1_Mult9 == "Так", 1, 0), # IDP status
    has_children = ifelse(dity_n > 0, 1, 0),
    head_sex,
    head_age_cat,
    head_educ,

    # Expenditures
    exp_food,
    exp_tobacco_alcohol,
    exp_other,
    exp_total_consumption,
    income_total = V1,

    period_id = "2024_SURVEY_DEC23_FEB24",
    year = "2024"
  ) %>%
  # Harmonize region and settlement_type columns to match ssu_21_processed
  mutate(
    region = case_when(
      stringr::str_starts(region, "Вінницька") ~ "Вінницька",
      stringr::str_starts(region, "Волинська") ~ "Волинська",
      stringr::str_starts(region, "Дніпропетровська") ~ "Дніпропетровська",
      stringr::str_starts(region, "Донецька") ~ "Донецька",
      stringr::str_starts(region, "Житомирська") ~ "Житомирська",
      stringr::str_starts(region, "Закарпатська") ~ "Закарпатська",
      stringr::str_starts(region, "Запорізька") ~ "Запорізька",
      stringr::str_starts(region, "Івано-Франківська") ~ "Івано-Франківська",
      stringr::str_starts(region, "Київська") ~ "Київська",
      stringr::str_starts(region, "Кіровоградська") ~ "Кіровоградська",
      stringr::str_starts(region, "Луганська") ~ "Луганська",
      stringr::str_starts(region, "Львівська") ~ "Львівська",
      stringr::str_starts(region, "Миколаївська") ~ "Миколаївська",
      stringr::str_starts(region, "Одеська") ~ "Одеська",
      stringr::str_starts(region, "Полтавська") ~ "Полтавська",
      stringr::str_starts(region, "Рівненська") ~ "Рівненська",
      stringr::str_starts(region, "Сумська") ~ "Сумська",
      stringr::str_starts(region, "Тернопільська") ~ "Тернопільська",
      stringr::str_starts(region, "Харківська") ~ "Харківська",
      stringr::str_starts(region, "Херсонська") ~ "Херсонська",
      stringr::str_starts(region, "Хмельницька") ~ "Хмельницька",
      stringr::str_starts(region, "Черкаська") ~ "Черкаська",
      stringr::str_starts(region, "Чернівецька") ~ "Чернівецька",
      stringr::str_starts(region, "Чернігівська") ~ "Чернігівська",
      stringr::str_starts(region, "м. Київ") ~ "м. Київ",
      TRUE ~ region
    )
  )


### Deflate SSU and UNICEF Data using CPI ###

# Deflate SSU Data using specific CPIs
ssu_final <- ssu_21_processed %>%
  left_join(all_cpi_indexed, by = c("region", "period_id")) %>%
  mutate(
    exp_food_real = exp_food / (cpi_general / 100),
    exp_tobacco_real = exp_tobacco / (cpi_tobacco / 100),
    exp_alcohol_real = exp_alcohol / (cpi_alcohol / 100),
    exp_other_real = exp_other / (cpi_general / 100),
    income_total_real = income_total / (cpi_general / 100),
    # Recalculate total real consumption from real components
    exp_total_consumption_real = exp_food_real +
      exp_tobacco_real +
      exp_alcohol_real +
      exp_other_real
  )

unicef_final <- unicef_24_processed %>%
  left_join(all_cpi_indexed, by = c("region", "period_id")) %>%
  mutate(
    exp_food_real = exp_food / (cpi_general / 100),
    exp_tobacco_alcohol_real = exp_tobacco_alcohol / (cpi_t_a / 100),
    exp_other_real = exp_other / (cpi_general / 100),
    income_total_real = income_total / (cpi_general / 100),
    # Recalculate total real consumption from real components
    exp_total_consumption_real = exp_food_real +
      exp_tobacco_alcohol_real +
      exp_other_real
  )


ssu_final <- ssu_final %>%
  mutate(
    w_food = exp_food_real / exp_total_consumption_real,
    w_tobacco = exp_tobacco_real / exp_total_consumption_real,
    w_alcohol = exp_alcohol_real / exp_total_consumption_real,
    w_tobacco_alcohol = (exp_tobacco_real + exp_alcohol_real) /
      exp_total_consumption_real,
    w_other = exp_other_real / exp_total_consumption_real
  )

unicef_final <- unicef_final %>%
  mutate(
    w_food = exp_food_real / exp_total_consumption_real,
    w_tobacco_alcohol = exp_tobacco_alcohol_real / exp_total_consumption_real,
    w_other = exp_other_real / exp_total_consumption_real
  )


### Tobacco Elasticity Estimation Script ###
# Create regional average real prices for merging
regional_prices <- price_summary %>%
  group_by(region, period_id, price_category) %>%
  summarise(
    regional_real_price = mean(avg_real_price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = price_category,
    values_from = regional_real_price,
    names_prefix = "price_"
  )


# Merge prices with SSU 2021 data
ssu_with_prices <- ssu_final %>%
  left_join(regional_prices, by = c("region", "period_id")) %>%
  # Create log variables for estimation
  mutate(
    log_exp_total = log(exp_total_consumption_real + 1), # Adding 1 to handle zeros
    log_price_tobacco = log(price_tobacco),
    log_price_alcohol = log(price_alcohol),
    log_price_ta = log(`price_tobacco & alcohol`),
    # Create indicator for positive tobacco expenditure
    tobacco_positive = ifelse(exp_tobacco_real > 0, 1, 0),
    alcohol_positive = ifelse(exp_alcohol_real > 0, 1, 0)
  )


unicef_with_prices <- unicef_final %>%
  left_join(regional_prices, by = c("region", "period_id")) %>%
  mutate(
    log_exp_total = log(exp_total_consumption_real + 1),
    log_price_ta = log(`price_tobacco & alcohol`),
    ta_positive = ifelse(exp_tobacco_alcohol_real > 0, 1, 0)
  )
