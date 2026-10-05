library(tidyverse)
library(janitor)
library(purrr)


# Read all mortality datasets
matmort <- read.csv("./Data/Raw/raw/raw/maternal_mortality.csv", header = TRUE)
infantmort <- read.csv("./Data/Raw/raw/raw/infant_mortality.csv", header = TRUE)
neonmort <- read.csv("./Data/Raw/raw/raw/neonatal_mortality.csv", header = TRUE)
under5mort <- read.csv("./Data/Raw/raw/raw/under5_mortality.csv", header = TRUE)


# Change wide to long format
#matmort_lg <- matmor |>
 # pivot_longer(cols = starts_with("X"),
  #               names_to = "year",
   #              names_prefix = "X", # removes X from year column
    #             values_to = "maternal_mortality") |> 
  #mutate(year = as.numeric(year)) |> # change year to numeric
  #select(iso, year, maternal_mortality)


## PREPARE WORLD BANK DATA
# Create a function that converts datasets to wide format
convert_long <- function(data, varname) {  # function takes dataset and a variable name for the values
    data |>
      pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = varname) |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, varname)
}

matmort_lg <- convert_long(matmort, "maternal_mortality")
infantmort_lg <- convert_long(infantmort, "infant_mortality")
neonmort_lg <- convert_long(neonmort, "neonatal_mortality")
under5mort_lg <- convert_long(under5mort, "under5_mortality")


## PREPARE DISASTER DATA
# Read disaster dataset
disaster <- read.csv("./Data/Raw/raw/raw/disaster.csv", header = TRUE)

# Clean column names in dataset
disaster <- clean_names(disaster)

# Subset dataset
subdisaster <- filter(disaster, year %in% c(2000:2019), 
    disaster_type %in% c("Earthquake", "Drought"))

# Further subset to few columns
nestdisaster <- subdisaster |> select(iso, year, disaster_type)

# Dummy variables for drought and earthquake
nestdisaster <- nestdisaster |> group_by(iso, year) |>
  summarise(
    n1 = sum(disaster_type == "Drought", na.rm = TRUE),
    n2 = sum(disaster_type == "Earthquake", na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    drought = as.integer(if_else(n1>0, 1, 0)),
    earthquake= as.integer(if_else(n2>0, 1, 0))
) |> select(year, iso, earthquake, drought)


## PREPARE CONFLICT DATA
# Read disaster dataset
conflict_dt <- read.csv("./Data/Raw/raw/raw/conflict.csv", header = TRUE)

# Binary conflict variable
# 0=No conflict, <25 conflict related deaths; 1=Yes conflict, >=25 deaths
conflict_dt <- conflict_dt |> group_by(iso, year) |>
  summarise(
    n_deaths = sum(best, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    conflict = as.integer(if_else(n_deaths>=25, 1, 0))
)


## MERGE ALL DATA
# Datasets to merge; conflict_dt, nestdisaster, infantmort_lg, matmort_lg, 
# neonmort_lg, under5mort_lg
mortality_dt <- list(conflict_dt, nestdisaster, infantmort_lg, matmort_lg, 
    neonmort_lg, under5mort_lg) |>
  reduce(left_join, by = c("iso", "year")) |>
  mutate(year=as.integer(year))

write.csv(mortality_dt, "./Report/mortality_data.csv", row.names = FALSE)

