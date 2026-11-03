#################################################
# CREATE TABLE 1
#################################################

pkgs <- c("gtsummary", "gt", "ggplot2", "scales")
to_install <- pkgs[!sapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(dplyr)
library(gtsummary)
library(gt)
library(ggplot2)
library(scales)


armed_conflict_data <- read.csv("./Data/Processed/armed_conflict_final_data.csv", header = TRUE)

table1 <- armed_conflict_data |> select(-iso, -year) |>
  mutate(armed_conflict = factor(armed_conflict,
                          levels=c(0,1),
                        labels=c("No armed conflict", "Armed conflict")
                      ),
          across(
            c(earthquake, drought, oecd),
            ~factor(.x, levels=c(0,1), labels=c("No", "Yes"))
          )
        ) |>
  tbl_summary(by=armed_conflict,
    type = list(all_categorical() ~ "categorical"),
    label = list(
      earthquake ~ "Earthquake",
      drought ~ "Drought",
      infant_mortality ~ "Infant mortality",
      maternal_mortality ~ "Maternal mortality",
      neonatal_mortality ~ "Neonatal mortality",
      under5_mortality ~ "Under_5 mortality",
      gdp_1000 ~ "GDP",
      oecd ~ "OECD member",
      pop_dens ~ "Population density",
      urban ~ "Urban population",
      age_dep ~ "Age dependency ratio",
      male_edu ~ "Male education",
      temp ~ "Temperature",
      rainfall_1000 ~ "Rainfall"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    percent = "column",
    missing = "always",
    missing_text = "Missing",
    missing_stat = "{N_miss}"
  ) |>
  add_overall(last = FALSE) |>
  bold_labels() |>
  modify_caption("**Table 1. Characteristics of countries stratified by armed-conflict status**")

# View table
table1

### Export to PDF ###

table1_pdf <- as_gt(table1)

gtsave(
  data = table1_pdf,
  filename = "./Report/Table01_armed_conflict.pdf"
)


#################################################
# CREATE LINE GRAPH FOR MATERNAL MORTALITY
#################################################

maternal_mortality_data <- armed_conflict_data |>
  select(iso, year, maternal_mortality) |>
  filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = last(maternal_mortality) - first(maternal_mortality)) |> ungroup() |>
  filter(diff_matmor > 0)

# Create figure
matmort_plot <- ggplot(maternal_mortality_data,
       aes(x = year, y = maternal_mortality, group = iso)) +
  geom_line(color = "#246B8E", linewidth = 0.9) +
  geom_point(color = "#D95F02", size = 1.5, alpha = 0.85) +
  facet_wrap(vars(iso), scales = "free_y", ncol = 4) +
  scale_x_continuous(
    breaks = seq(2000, 2016, by = 4),
    minor_breaks = NULL
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Maternal Mortality Trends for Countries with Increased Mortality (2000–2017)",
    x = NULL,
    y = "Maternal Mortality"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#matmort_plot <- ggplot(
 # plot_data,
  #aes(
   # x = year,
    #y = maternal_mortality,
    #color = iso,
    #group = iso
  #)
#) +
 # geom_line(linewidth = 1) +
  #labs(
   # title = "Mortality Trends in Countries with Increased Mortality (2000–2017)",
    #x = "Year",
    #y = "Maternal Mortality",
    #color = "Country"
  #) +
  #theme_minimal() +
  #theme(
   # plot.title = element_text(hjust = 0.5)
  #)

# Display plot
matmort_plot

# Save figure as PDF
ggsave(
  filename = "./Report/Figure01_Maternal_Mortality_Trend.png",
  plot = matmort_plot,
  width = 10,
  height = 7
)




