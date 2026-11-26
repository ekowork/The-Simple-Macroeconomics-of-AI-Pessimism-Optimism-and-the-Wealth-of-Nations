# Simple replication of the baseline scale model
#
# Packages used:
#   install.packages(c("dplyr", "readxl"))
#
# The AISA workbook contains cumulative task exposure for each occupation at
# capability levels from 1 to 6. We turn increases in cumulative exposure into
# task-difficulty cohorts, weight occupations by their wage bills, and then
# apply the equations in note4_scale_model.pdf.

library(dplyr)
library(readxl)


# 1. Read the AISA data -------------------------------------------------------
setwd("C:/Users/EKoWo/Documents/GitHub/AI-Impact-on-World-GDP")
aisa <- read_excel(
  "data/aisa/work1207_data.xlsx",
  sheet = "data"
)


# 2. Convert cumulative AISA exposure into difficulty cohorts ----------------

# AISA is reported in percentage points. Within each occupation, the increase
# between two capability levels is the share of tasks entering at that level.
# Each occupation is weighted by employment times its annual mean wage.

tasks <- aisa %>%
  transmute(
    occupation = OCCCode,
    capability = `AI capability`,
    exposure = AISA / 100,
    wage_bill = `Total Employment` * `Annual Mean Wage`
  ) %>%
  arrange(occupation, capability) %>%
  group_by(occupation) %>%
  mutate(
    task_share = exposure - lag(exposure, default = 0),
    occupation_wage_bill = first(wage_bill)
  ) %>%
  ungroup() %>%
  mutate(
    # Wage bills repeat over capability rows, so calculate the national total
    # from one row per occupation.
    total_wage_bill = sum(
      occupation_wage_bill[!duplicated(occupation)],
      na.rm = TRUE
    ),
    weight = task_share * occupation_wage_bill / total_wage_bill,
    difficulty = capability
  ) %>%
  filter(task_share > 0, !is.na(weight)) %>%
  select(occupation, difficulty, weight)


# 3. Model equations ----------------------------------------------------------

# This function implements Proposition 1. It returns one row per task cohort.
# z is capability surplus: AI capability minus task difficulty.

cohort_results <- function(z, b, theta, mu0) {
  gamma <- ifelse(z > 0, 1 - exp(-theta * z), 0)
  m <- mu0 + log(gamma)

  iota <- ifelse(
    gamma == 0,
    0,
    ifelse(
      m <= 0,
      (1 + b) / 2 * exp((1 - b) / b * m),
      1 - (1 - b) / 2 * exp(-(1 + b) / b * m)
    )
  )

  saving <- ifelse(
    gamma == 0,
    0,
    ifelse(
      m <= 0,
      gamma * b * (1 + b) / 2 * exp((1 - b) / b * m),
      gamma * (
        1 - (1 - b^2) * exp(-m) +
          b * (1 - b) / 2 * exp(-(1 + b) / b * m)
      )
    )
  )

  tibble(
    gamma = gamma,
    iota = iota,
    lambda = ifelse(iota > 0, saving / iota, 0),
    saving = saving
  )
}


# Aggregate the task cohorts at one capability level.

aggregate_model <- function(kappa, theta, mu0, b = 0.7) {
  outcomes <- cohort_results(
    z = kappa - tasks$difficulty,
    b = b,
    theta = theta,
    mu0 = mu0
  )

  tasks %>%
    bind_cols(outcomes) %>%
    summarise(
      kappa = kappa,
      exposure = sum(weight[difficulty <= kappa]),
      viable_wage = sum(weight * iota),
      gdp_gain = sum(weight * saving),
      iota = if_else(exposure > 0, viable_wage / exposure, 0),
      lambda = if_else(viable_wage > 0, gdp_gain / viable_wage, 0)
    )
}


# 4. Reproduce the constant-share benchmark ----------------------------------

constant_share_results <- tibble(kappa = c(2.6, 6)) %>%
  rowwise() %>%
  mutate(
    exposure = sum(tasks$weight[tasks$difficulty <= kappa]),
    gdp_gain = exposure * 0.23 * 0.27
  ) %>%
  ungroup()

print(constant_share_results)


# 5. Calibrate theta and mu0 --------------------------------------------------

# b is fixed at 0.7. At today's capability, theta and mu0 are chosen to match
# Acemoglu's viable share (0.23) and saving among adopters (0.27).

calibration_error <- function(parameters) {
  result <- aggregate_model(
    kappa = 2.6,
    theta = parameters[1],
    mu0 = parameters[2],
    b = 0.7
  )

  (result$iota - 0.23)^2 + (result$lambda - 0.27)^2
}

fit <- optim(
  par = c(theta = 0.48, mu0 = -1.74),
  fn = calibration_error,
  method = "L-BFGS-B",
  lower = c(0.01, -10),
  upper = c(5, 5)
)

theta_hat <- fit$par[1]
mu0_hat <- fit$par[2]

calibration <- tibble(
  parameter = c("b", "theta", "mu0"),
  value = c(0.7, theta_hat, mu0_hat)
)

print(calibration)
print(aggregate_model(2.6, theta_hat, mu0_hat))


# 6. Calculate the baseline path ---------------------------------------------

baseline_results <- bind_rows(
  lapply(
    seq(1, 6, by = 0.2),
    function(k) aggregate_model(k, theta_hat, mu0_hat)
  )
) %>%
  select(kappa, exposure, iota, lambda, gdp_gain)

print(
  baseline_results %>%
    filter(kappa %in% c(2.6, 6))
)


# 7. Save the small output tables --------------------------------------------

# dir.create("results", showWarnings = FALSE)
# 
# write.csv(
#   calibration,
#   "results/scale_model_simple_calibration.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   baseline_results,
#   "results/scale_model_simple_results.csv",
#   row.names = FALSE
# )

