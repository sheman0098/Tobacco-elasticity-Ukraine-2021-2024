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

glimpse(avg_price_data_clean)
unique(avg_price_data_clean$Періодичність)
unique(avg_price_data_clean$`Тип товарів і послуг`)

glimpse(tobacco_periods)
glimpse(t_and_a_periods)
glimpse(alc_periods)
glimpse(gen_periods)
