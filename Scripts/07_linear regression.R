#################################################
# FITTING LINEAR REGRESSION MODELS
#################################################

#install.packages("lm")
#install.packages("plm")


library(dplyr)
library(modelsummary)
library(tinytable)
#library(gtsummary)
#library(gt)
#library(ggplot2)
#library(scales)
#library(lm)
library(plm)
library(broom)
library(purrr)


armed_conflict_data <- read.csv("./Data/Processed/armed_conflict_final_data.csv", header = TRUE)


# Fixed effects model with lm()
lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens +
            urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + iso,
            data = armed_conflict_data)

# Fixed effects model with plm()
plmmod <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens +
              urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso"), model = "within",
              data = armed_conflict_data)

summary(lmmod)
summary(plmmod)


lmmod2 <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso + as.factor(year), 
            data = armed_conflict_data)

plmmod2 <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = armed_conflict_data)

summary(lmmod2)
summary(plmmod2)


### In-class assignment ###

# log-transform gdp
armed_conflict_data <- armed_conflict_data |>
                  mutate(log_gdp = log(gdp_1000))

# Fit models
preds <- as.formula(" ~ armed_conflict + log_gdp + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

matmormod <- plm(update.formula(preds, maternal_mortality ~ .), 
              index = c("iso", "year"),
              effect = "twoways",
              model = "within", data = armed_conflict_data)
un5mormod <- plm(update.formula(preds, under5_mortality ~ .), 
              index = c("iso", "year"),
              effect = "twoways",
              model = "within", data = armed_conflict_data)
infmormod <- plm(update.formula(preds, infant_mortality ~ .), 
              index = c("iso", "year"),
              effect = "twoways",
              model = "within", data = armed_conflict_data)
neomormod <- plm(update.formula(preds, neonatal_mortality ~ .), 
              index = c("iso", "year"),
              effect = "twoways",
              model = "within", data = armed_conflict_data)

summary(matmormod)
summary(un5mormod)
summary(infmormod)
summary(neomormod)


# Create table 2 - table with model summaries

# naming the models
models <- list(
  "Maternal mortality ratio per 100,000 live births" = matmormod,
  "Under5 mortality rate per 1,000 live births" = un5mormod,
  "Infant mortaslity rate per 1,000 live births" = infmormod,
  "Neonatal mortality rate per 1,000 live births" = neomormod
)

# group-clustered covariance matrices
vcovs <- lapply(
  models,
  function(model) {
    plm::vcovHC(
      model,
      method = "arellano",
      type = "HC1",
      cluster = "group"
    )
  }
)

# create table
table2 <- modelsummary(
  models,
  vcov = vcovs,

  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
  statistic = NULL,
  conf_level = 0.95,
  fmt = 3,

  stars = c(
    "*"   = 0.05,
    "**"  = 0.01,
    "***" = 0.001
  ),
  
  coef_map = c(
      "armed_conflict" = "Armed conflict-binary",
      "log_gdp" = "GDP per capita (log)",
      "oecd" = "OECD member",
      "pop_dens" = "Population density",
      "urban" = "Urban population",
      "age_dep" = "Age dependency ratio",
      "male_edu" = "Male education",
      "temp" = "Temperature",
      "rainfall_1000" = "Rainfall",
      "earthquake" = "Earthquake",
      "drought" = "Drought"
  ),

  gof_omit = "AIC|BIC|Log.Lik.|RMSE|F",
  title = paste0("Table 2. The association between armed conflict and various mortalities: ",
      "adjusted beta coefficients(95\\% confidence internval)"),
    
  notes = paste0("Beta coefficients with group-clustered ",
  "95\\% confidence intervals"),
  width = c(0.28, 0.18, 0.18, 0.18, 0.18),
  output = "tinytable"
)

### Save to PDF ###
table2 <- tinytable::theme_latex(
  table2,
  outer = "label=none"
)

# View table
table2

tinytable::save_tt(
  table2,
  output = "./Report/Table02_armed_conflict.pdf",
  overwrite = TRUE
)



