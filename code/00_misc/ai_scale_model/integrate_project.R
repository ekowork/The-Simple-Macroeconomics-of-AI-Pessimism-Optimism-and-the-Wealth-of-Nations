# Run the original script through creation of tfp and gdp, then source this file.
# Copy the whole ai_scale_model directory into your calc directory first:
# source("ai_scale_model/integrate_project.R")
# The old baseline columns/data remain available for comparison.

source("ai_scale_model/ai_scale_model.R")

required_objects <- c("agg_us_tiva", "tfp", "world_internet", "world_gdp")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects)) stop("Run the existing data pipeline first. Missing: ",
                                paste(missing_objects, collapse = ", "))

# --- Choices to review -------------------------------------------------------
scale_kappa0 <- 2.6         # AI capability at the calibration benchmark.
scale_b <- 0.7              # Dispersion of log deployment wage pools, externally fixed.
scale_target_iota <- 0.49   # Viable share of the exposed US wage bill at the benchmark.
scale_target_lambda <- 0.33 # Saving among viable deployments; see saving_measure below.
scale_saving_measure <- "net" # Literal note: net. Use "gross" for reported time savings.
scale_allocation <- "uniform" # Approximation inside observed AISA difficulty intervals.
scale_quadrature <- 16L     # Quadrature nodes per interval, not a model parameter.
scale_missing <- "error"    # Alternative: "common_sample", with coverage reported.

# agg_us_tiva contains AISA percentages in the supplied 05-script.R;
# tfp contains fractions after its /100 conversion. Do not divide tfp again.
scale_curves <- data.frame(
  sector = as.character(agg_us_tiva$tiva),
  kappa = agg_us_tiva$`AI capability`,
  exposure = agg_us_tiva$AISA_bill_weighted / 100)
scale_sectors <- sort(unique(as.character(tfp$ACTIVITY)))
scale_curves <- scale_curves[scale_curves$sector %in% scale_sectors, ]
scale_kappas <- sort(unique(tfp$`AI capability`))
if (!any(abs(scale_kappas - scale_kappa0) < 1e-9))
  stop("The benchmark capability is absent from tfp.")

# A CDF requires a common occupation sample across capabilities.
# Exposure changes caused by changing sample coverage cannot be treated as cohorts.
if ("total_wage_bill" %in% names(agg_us_tiva)) {
  for (s in intersect(scale_sectors, as.character(agg_us_tiva$tiva))) {
    bill <- agg_us_tiva$total_wage_bill[as.character(agg_us_tiva$tiva) == s]
    if (anyNA(bill) || any(!is.finite(bill)) || any(bill < 0) ||
        diff(range(bill)) > 1e-8 * max(1, max(bill)))
      stop("The occupation wage-bill sample changes across capabilities in sector ", s)
  }
}

# Preserve explicitly zero-imputed sectors already present in the original tfp.
# Unknown positive exposure is an error, not an implicit zero assumption.
for (s in setdiff(scale_sectors, scale_curves$sector)) {
  rows <- tfp[as.character(tfp$ACTIVITY) == s, ]
  if (anyNA(rows$AISA_bill_weighted) || any(rows$AISA_bill_weighted != 0))
    stop("No AISA curve for nonzero/unknown sector ", s)
  scale_curves <- rbind(scale_curves,
    data.frame(sector = s, kappa = scale_kappas, exposure = 0))
}

# If the observed grid starts at 1 with positive exposure, this EXPLICITLY
# allocates that first mass over difficulties [0,1]. Obtain a finer AISA grid
# or supply raw difficulty weights when possible. This is not exact microdata.
scale_task_data <- scale_from_cdf(scale_curves,
  allocation = scale_allocation, quadrature = scale_quadrature,
  lower_bound = 0, first_bin = "uniform")

# Use the US compensation composition of the actual TFP/GDP pipeline:
# sector wage-bill weights are proportional to VA share * sector labour share.
# This makes the NET calibration reproduce the project's frozen US benchmark
# exactly on the same sample. It can differ from the note's OEWS composition.
scale_usa <- tfp[tfp$REF_AREA == "USA" &
                  abs(tfp$`AI capability` - scale_kappa0) < 1e-9, ]
if (!nrow(scale_usa) || anyDuplicated(scale_usa$ACTIVITY))
  stop("Need one US row per industry at the benchmark.")
scale_us_weights <- setNames(rep(0, length(scale_sectors)), scale_sectors)
scale_usa_ok <- is.finite(scale_usa$Lshare) & is.finite(scale_usa$share_of_gdp)
if (scale_missing == "error" && any(!scale_usa_ok))
  stop("Missing US labour/VA weights at the benchmark.")
scale_usa <- scale_usa[scale_usa_ok, ]
if (any(scale_usa$Lshare < 0) || any(scale_usa$share_of_gdp < 0))
  stop("Negative US calibration weights.")
scale_us_weights[as.character(scale_usa$ACTIVITY)] <-
  scale_usa$share_of_gdp * scale_usa$Lshare
if (sum(scale_us_weights) <= 0) stop("No positive US calibration wage bill.")
scale_us_weights <- scale_us_weights / sum(scale_us_weights)
scale_us_tasks <- scale_pool(scale_task_data, scale_us_weights)

scale_fit <- scale_calibrate(scale_us_tasks, kappa0 = scale_kappa0,
  b = scale_b, target_iota = scale_target_iota,
  target_lambda = scale_target_lambda, saving_measure = scale_saving_measure)
print(scale_fit)

# The same three primitives apply to all sectors/countries in this extension.
# Sector-specific margins differ because sector difficulty distributions differ.
# Country-specific wage levels or deployment costs require additional data.
scale_tiva <- scale_aggregate(scale_task_data, scale_kappas, scale_fit$pars)
tfp_endogenous <- scale_attach_tfp(tfp, scale_tiva)
gdp_endogenous <- scale_country(tfp_endogenous,
  target_iota = scale_target_iota, target_lambda = scale_target_lambda,
  missing = scale_missing)
gdp_endogenous <- scale_add_country_data(gdp_endogenous, world_internet, world_gdp)
global_endogenous <- scale_global(gdp_endogenous)

# Cross-check the national anchor against the project-compatible frozen model.
# With a gross target, net gains should differ; equality is not imposed.
if (scale_saving_measure == "net") {
  anchor <- gdp_endogenous[gdp_endogenous$REF_AREA == "USA" &
    abs(gdp_endogenous$`AI capability` - scale_kappa0) < 1e-9, ]
  stopifnot(nrow(anchor) == 1L,
            abs(anchor$gdp_endo - anchor$gdp_frozen) < 1e-6)
}

# Optional cost-only counterfactual. factor=0.2 cuts F five-fold; theta is fixed.
# cheap <- scale_change_cost(scale_fit$pars, factor = 0.2)
# cheap_margins <- scale_aggregate(scale_task_data, scale_kappas, cheap)

print(global_endogenous[global_endogenous$`AI capability` %in% c(scale_kappa0, 6), ])

# To export after reviewing the fit:
# dir.create("output/scale_model", recursive = TRUE, showWarnings = FALSE)
# write.csv(scale_tiva, "output/scale_model/sector_margins.csv", row.names = FALSE)
# write.csv(gdp_endogenous, "output/scale_model/country_gdp.csv", row.names = FALSE)
# write.csv(global_endogenous, "output/scale_model/global_gdp.csv", row.names = FALSE)
# saveRDS(scale_fit, "output/scale_model/calibration.rds")

# Plot using the tidyverse already loaded by the project:
# ggplot(global_endogenous, aes(x = `AI capability`)) +
#   geom_line(aes(y = gdp_endo_adj_internet, colour = "Endogenous")) +
#   geom_line(aes(y = gdp_frozen_adj_internet, colour = "Frozen"), linetype = 2) +
#   labs(x = "AI capability", y = "Global GDP gain (%)", colour = NULL) +
#   theme_minimal()
