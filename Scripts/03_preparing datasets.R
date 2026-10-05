library(tidyverse)
library(janitor)


# Read in maternal mortality data
matmor <- read.csv("C:/Users/patie/OneDrive - University of Toronto./COURSES/FALL/2026/CHL5233 - Stats Programming/armed_conflict/Data/Raw/raw/raw/maternal_mortality.csv", header = TRUE)
infant_mort <- read.csv("C:/Users/patie/OneDrive - University of Toronto./COURSES/FALL/2026/CHL5233 - Stats Programming/armed_conflict/Data/Raw/raw/raw/infant_mortality.csv", header = TRUE)

# Change wide to long format
matmor_long <- matmor |>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = "maternal_mortality") |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, maternal_mortality)

## PREPARE WORLD BANK DATA
# Create a function that converts datasets to wide format
convert_long <- function(data, varname) { # function takes dataset and a variable name for the values
    data |>
      pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = varname) |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, varname)
}

infant_mort_long <- convert_long(infant_mort, "infant_mortality")


## PREPARE DISASTER DATA
# Read disaster dataset
disaster <- read.csv("C:/Users/patie/OneDrive - University of Toronto./COURSES/FALL/2026/CHL5233 - Stats Programming/armed_conflict/Data/Raw/raw/raw/disaster.csv", header = TRUE)

# Clean column names in dataset
disaster_clean <- clean_names(disaster)

# Subset dataset
subdisaster_clean <- filter(disaster_clean, year %in% c(2000:2019), 
    disaster_type %in% c("Earthquake", "Drought"))

# Further subset to few columns
subdisaster_clean2 <- subdisaster_clean |> select(iso, year, disaster_type)

# Dummy variables for drought and earthquake
subdisaster_clean2 <- subdisaster_clean2 |> group_by(iso, year) |>
  summarise(
    n1 = sum(disaster_type == "Drought", na.rm = TRUE),
    n2 = sum(disaster_type == "Earthquake", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    drought = if_else(n1>0, 1, 0),
    earthquake= if_else(n2>0, 1, 0)
) |> select(year, iso, earthquake, drought)


## PREPARE CONFLICT DATA
# Read disaster dataset
disaster <- read.csv("C:/Users/patie/OneDrive - University of Toronto./COURSES/FALL/2026/CHL5233 - Stats Programming/armed_conflict/Data/Raw/raw/raw/disaster.csv", header = TRUE)
