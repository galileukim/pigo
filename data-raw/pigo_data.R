## code to prepare `pigo_data` dataset goes here
# dependencies ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(here)
library(stringr)
library(scales)
library(haven)
library(readxl)
library(purrr)

# process data ------------------------------------------------------------
# Load data files from the input folder
ctf_country <- read_dta(
  here("data-raw", "static_ctf_012225.dta")
) # Most recent data Ileana used for consistency

cliar_db_variables <- read_xlsx(
  here("data-raw","db_variables.xlsx")
)

# Read and clean WDI data
wdi <- read_excel(
  here("data-raw", "world_development_indicators.xlsx"), sheet = "Data") %>%
  replace(., . == "..", NA) %>%
  filter(!is.na(`Country Code`)) %>%
  filter(`Series Name` %in% c(
    "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
    "GDP per capita, PPP (constant 2021 international $)",
    "Unemployment, total (% of total labor force) (modeled ILO estimate)"
  )) %>%
  mutate(
    `2019_2023` = coalesce(
      `2023 [YR2023]`,
      `2022 [YR2022]`,
      `2021 [YR2021]`,
      `2020 [YR2020]`,
      `2019 [YR2019]`
    )
  ) %>%
  select(-matches("\\[YR\\d{4}\\]"), -`Series Code`) %>%
  pivot_wider(names_from = `Series Name`, values_from = `2019_2023`)

# Standardize `region` values and exclude North America and empty regions
ctf_country <- ctf_country %>%
  mutate(region = trimws(str_to_title(as.character(region)))) %>%
  filter(region != "North America" & region != "" & region != " ")

# Read allowed countries list from Excel (ensure the file has a column named "country_code")
allowed_countries <- read_excel(
    here("data-raw", "client_countries.xlsx")
  )

# Define variables and labels
wdi_vars <- c(
  "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
  "GDP per capita, PPP (constant 2021 international $)"
)

# Convert WDI variables to numeric where applicable
wdi <- wdi %>%
  mutate(across(all_of(wdi_vars), ~ as.numeric(as.character(.))))

# select cliar indicators
cliar_cluster_avg <- ctf_country |>
  # cluster-level averages end with _avg
  select(ends_with("_avg")) |>
  colnames()

# we need the cluster-level average names
cliar_clusters <- cliar_db_variables |>
  distinct(family_var, family_name) |>
  mutate(
    family_var = paste0(family_var, "_avg")
  ) |>
  # only retain records where cluster-level avg exists
  filter(
    family_var %in% cliar_cluster_avg
  )

ctf_vars <- cliar_clusters |>
  pull(family_var)

ctf_labels <- ctf_vars
names(ctf_labels) <- cliar_clusters |>
  pull(family_name)

# Merge data and rename columns in `ctf_vars`
merged_data <- ctf_country %>%
  select(country_code, all_of(ctf_vars), region) %>%
  left_join(wdi, by = c("country_code" = "Country Code")) %>%
  rename_at(
    vars(ctf_labels),
    ~ all_of(names(ctf_labels))
  ) |>
  mutate(
    `Logged GDP per capita, PPP (constant 2021 international $)` = log(
      `GDP per capita, PPP (constant 2021 international $)`
    )
  )

# Before filtering, print the country codes that will be dropped
merged_data_original <- merged_data  # Save the original merged data
not_matched <- merged_data_original %>%
  anti_join(allowed_countries, by = "country_code")

if (nrow(not_matched) > 0) {
  message("The following country codes did not match allowed_countries and will be dropped:")
  print(unique(not_matched$country_code))
} else {
  message("All country codes in merged_data matched allowed_countries.")
}

# Now limit the sample to only allowed countries
merged_data <- merged_data_original %>%
  inner_join(allowed_countries, by = "country_code")

# Read the 'Table Data' sheet and extract Country Risk Index
risk_data <- read_excel(
  here("data-raw", "country_risk_index.xlsx"),
  sheet = "Table Data"
  ) %>%
  select(`Country Name`, `Country Risk Index`) %>%
  rename(country_risk = `Country Risk Index`) %>%
  mutate(`Country Name`= str_trim(`Country Name`))  # Trim spaces for consistency

# Merge risk data with existing merged_data
pigo_data <- merged_data %>%
  left_join(risk_data |> rename(`Country Risk Index` = "country_risk"), by = c("Country Name" = "Country Name"))

usethis::use_data(pigo_data, overwrite = TRUE)
