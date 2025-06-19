### Install and load necessary packages ###

if (!require(pacman)) {
  install.packages("pacman")
}


pacman::p_load(
  haven,
  tidyverse,
  data.table,
  readxl,
  conflicted
)

p_loaded() # inspect loaded packages
conflict_scout() # check for conflicts


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


### Load and process the CPI data ###

cpi_data <- read_excel(
  "price_data/dataset_2025-06-18T13_53_54.269907203Z_DEFAULT_INTEGRATION_SSSU_DF_PRICE_CHANGE_CONSUMER_GOODS_SERVICE_24.0.0.xlsx"
)

cpi_data_clean <- cpi_data %>%
  dplyr::mutate(
    Рік = as.numeric(stringr::str_sub(Період, 1, 4)),
    Місяць = as.numeric(stringr::str_sub(Період, 7, 8))
  ) %>%
  dplyr::filter(Рік %in% c(2021:2024)) %>%
  dplyr::filter(Періодичність == "Місячна")

glimpse(cpi_data_clean)
unique(cpi_data_clean$`Базисний період`)
unique(cpi_data_clean$`Тип товарів і послуг`)


cpi_processed <- cpi_data_clean %>%
  # Convert observation values to numeric
  mutate(
    region = `Територіальний розріз`,
    cpi_value = as.numeric(`Значення cпостереження`),
    year = Рік,
    month = Місяць,
    base_period = `Базисний період`,
    product_type = `Тип товарів і послуг`,
  ) %>%
  # Create date variable
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>%
  # Filter out missing values
  dplyr::filter(!is.na(cpi_value)) %>%
  select(
    -c(
      `Значення cпостереження`,
      Показник,
      Рік,
      Місяць,
      Період,
      `Базисний період`,
      `Тип товарів і послуг`,
      `Територіальний розріз`,
      Періодичність
    )
  )

# Check the data structure
cpi_processed %>%
  count(product_type, base_period) %>%
  print(n = Inf)


tobacco_cpi <- cpi_processed %>%
  dplyr::filter(product_type == "Тютюнові вироби") %>%
  dplyr::filter(base_period %in% c("До попереднього місяця"))

t_and_a_cpi <- cpi_processed %>%
  dplyr::filter(product_type == "Алкогольні напої, тютюнові вироби") %>%
  dplyr::filter(base_period %in% c("До попереднього місяця"))

alc_cpi <- cpi_processed %>%
  dplyr::filter(product_type == "Алкогольні напої") %>%
  dplyr::filter(base_period %in% c("До попереднього місяця"))

gen_cpi <- cpi_processed %>%
  dplyr::filter(product_type == "Індекс споживчих цін") %>%
  dplyr::filter(base_period %in% c("До попереднього місяця"))

# Calculate CPI with regional variation preserved
calculate_mixed_periods_regional <- function(monthly_cpi_data, product_name) {
  # First create cumulative index BY REGION
  monthly_indexed <- monthly_cpi_data %>%
    arrange(region, year, month) %>%
    group_by(region) %>% # KEY: Group by region first
    mutate(
      growth_factor = cpi_value / 100,
      cpi_cumulative = 100 * cumprod(growth_factor)
    ) %>%
    ungroup()

  # Standard quarterly periods BY REGION
  quarterly <- monthly_indexed %>%
    mutate(quarter = ceiling(month / 3)) %>%
    dplyr::filter(
      !(year == 2023 & month == 12) & !(year == 2024 & month %in% 1:2)
    ) %>%
    group_by(region, year, quarter) %>% # Group by region AND time
    summarise(period_cpi = mean(cpi_cumulative), .groups = 'drop') %>%
    mutate(period_id = paste0(year, "Q", quarter))

  # Special survey period (Dec 2023 - Feb 2024) BY REGION
  survey_period <- monthly_indexed %>%
    dplyr::filter(
      (year == 2023 & month == 12) | (year == 2024 & month %in% 1:2)
    ) %>%
    group_by(region) %>% # Group by region
    summarise(period_cpi = mean(cpi_cumulative), .groups = 'drop') %>%
    mutate(period_id = "2024_SURVEY_DEC23_FEB24")

  # Combine results, maintaining regional structure
  combined <- bind_rows(
    quarterly %>% select(region, period_id, period_cpi),
    survey_period %>% select(region, period_id, period_cpi)
  ) %>%
    # Index to Q1 2021 = 100 FOR EACH REGION SEPARATELY
    group_by(region) %>%
    mutate(
      base_value = period_cpi[period_id == "2021Q1"],
      cpi_indexed = (period_cpi / base_value) * 100,
      product_type = product_name
    ) %>%
    ungroup()

  return(combined)
}

tobacco_periods <- calculate_mixed_periods_regional(
  tobacco_cpi,
  "Тютюнові вироби"
)

t_and_a_periods <- calculate_mixed_periods_regional(
  t_and_a_cpi,
  "Алкогольні напої, тютюнові вироби"
)

alc_periods <- calculate_mixed_periods_regional(
  alc_cpi,
  "Алкогольні напої"
)

gen_periods <- calculate_mixed_periods_regional(
  gen_cpi,
  "Індекс споживчих цін"
)


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

glimpse(tobacco_periods)
glimpse(t_and_a_periods)
glimpse(alc_periods)
glimpse(gen_periods)

unique(tobacco_periods$region)
unique(t_and_a_periods$region)
unique(alc_periods$region)
unique(gen_periods$region)


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
  deflate_prices(tobacco_periods, "tobacco")

alcohol_real_prices <- quarterly_avg_prices %>%
  dplyr::filter(product_category == "alcohol") %>%
  deflate_prices(alc_periods, "alcohol")

t_and_a_real_prices <- quarterly_avg_prices %>%
  dplyr::filter(
    product_category == "tobacco" | product_category == "alcohol"
  ) %>%
  deflate_prices(t_and_a_periods, "tobacco & alcohol")

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
    n_products = n_distinct(product),
    .groups = 'drop'
  )

# National level summary (Ukraine as a whole)
national_price_summary <- all_real_prices %>%
  dplyr::filter(region == "Україна") %>%
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
