## AI CODE

library(tidyverse)
library(janitor)


## PREPARE WORLD BANK MORTALITY DATA

# Convert one World Bank mortality data set from wide to long format.
prepare_mortality <- function(data, mortality_name) {
  data |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = mortality_name
    ) |>
    mutate(year = as.integer(year)) |>
    select(iso, year, all_of(mortality_name))
}

# Read the four World Bank mortality data sets.
matmort <- read.csv("./Data/Raw/raw/raw/maternal_mortality.csv")
infantmort <- read.csv("./Data/Raw/raw/raw/infant_mortality.csv")
neonmort <- read.csv("./Data/Raw/raw/raw/neonatal_mortality.csv")
under5mort <- read.csv("./Data/Raw/raw/raw/under5_mortality.csv")

# Apply the same manipulation to each data set.
matmort_lg <- prepare_mortality(matmort, "maternal_mortality")
infantmort_lg <- prepare_mortality(infantmort, "infant_mortality")
neonmort_lg <- prepare_mortality(neonmort, "neonatal_mortality")
under5mort_lg <- prepare_mortality(under5mort, "under5_mortality")


## PREPARE DISASTER DATA

# Read the disaster data and clean its variable names.
disaster <- read.csv("./Data/Raw/raw/raw/disaster.csv") |>
  clean_names()

# Keep the requested years, disaster types, and variables.
disaster_subset <- disaster |>
  filter(
    year >= 2000,
    year <= 2019,
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(year, iso, disaster_type)

# Create one record per country-year with binary disaster indicators.
disaster_prepared <- disaster_subset |>
  group_by(year, iso) |>
  summarise(
    earthquake = as.integer(any(disaster_type == "Earthquake")),
    drought = as.integer(any(disaster_type == "Drought")),
    .groups = "drop"
  ) |>
  select(year, iso, earthquake, drought)


## PREPARE ARMED CONFLICT DATA

# Read conflict records. The variable `best` is the best estimate of deaths.
conflict_raw <- read.csv("./Data/Raw/raw/raw/conflict.csv")

# Sum deaths where a country has multiple conflict records in the same year.
# Complete missing country-years so that years without conflict records are zero.
conflict_prepared <- conflict_raw |>
  group_by(iso, year) |>
  summarise(
    conflict_deaths = sum(best, na.rm = TRUE),
    .groups = "drop"
  ) |>
  complete(
    iso,
    year = 1999:2018,
    fill = list(conflict_deaths = 0)
  ) |>
  mutate(
    conflict = as.integer(conflict_deaths >= 25),
    # Assign the previous year's conflict status to the analysis year.
    year = year + 1L
  ) |>
  select(year, iso, conflict) |>
  arrange(iso, year)


## MERGE AND SAVE THE FINAL DATA SET

final_data <- conflict_prepared |>
  left_join(disaster_prepared, by = c("year", "iso")) |>
  left_join(matmort_lg, by = c("year", "iso")) |>
  left_join(infantmort_lg, by = c("year", "iso")) |>
  left_join(neonmort_lg, by = c("year", "iso")) |>
  left_join(under5mort_lg, by = c("year", "iso")) |>
  mutate(
    earthquake = replace_na(earthquake, 0L),
    drought = replace_na(drought, 0L)
  ) |>
  arrange(iso, year)

# Create the processed-data folder and save the final data set.
dir.create("./Data/Processed", recursive = TRUE, showWarnings = FALSE)
write.csv(
  final_data,
  "./Data/Processed/armed_conflict_health_data.csv",
  row.names = FALSE
)
