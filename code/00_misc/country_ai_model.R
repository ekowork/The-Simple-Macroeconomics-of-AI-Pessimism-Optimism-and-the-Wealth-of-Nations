# Model version: 1.0.2. Load dplyr and tidyr before using this model.

gamma_fn <- function(z, theta = 0.48) {
  ifelse(
    z >= 0,
    1 - exp(-theta * z),
    0
  )
}

m_fn <- function(z, theta = 0.48, mu0 = -1.74) {
  mu0 + log(gamma_fn(z, theta))
}


iota_fn <- function(z, theta = 0.48, mu0 = -1.74, b = 0.7) {
  m <- m_fn(z, theta, mu0)
  ifelse(
    m <= 0,
    (1 + b) / 2 *
      exp((1 - b) / b * m),
    1 -
      (1 - b) / 2 *
      exp(-(1 + b) / b * m)
  )
}


pi_fn <- function(z, theta = 0.48, mu0 = -1.74, b = 0.7) {
  gamma <- gamma_fn(z, theta)
  m     <- m_fn(z, theta, mu0)

  ifelse(
    m <= 0,

    gamma *
      b *
      (1 + b) / 2 *
      exp((1 - b) / b * m),

    gamma * (
      1 -
        (1 - b / 2) * exp(-m) +
        b * (1 - b) / 2 *
        exp(-(1 + b) / b * m)
    )
  )
}



calculate_industry_savings <- function(cohorts, total_wages, capabilities,
                                       theta, mu0, b = 0.7) {
  tidyr::crossing(cohorts, `AI capability` = capabilities) %>%
      mutate(
        z = `AI capability` - difficulty,
        exposed_wage = if_else(difficulty <= `AI capability`, cohort_wage_bill, 0),
        implemented_wage = cohort_wage_bill * iota_fn(z, theta, mu0, b),
        saving_wage = cohort_wage_bill * pi_fn(z, theta, mu0, b)
      ) %>%
      group_by(tiva, `AI capability`) %>%
      summarise(
        across(c(exposed_wage, implemented_wage, saving_wage),
               ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      left_join(total_wages, by = "tiva", relationship = "many-to-one") %>%
      mutate(
        S_endog = exposed_wage / total_wage_bill,
        iota_endog = if_else(exposed_wage > 0,
                            implemented_wage / exposed_wage, 0),
        lambda_endog = if_else(implemented_wage > 0,
                              saving_wage / implemented_wage, 0),
        saving_share = saving_wage / total_wage_bill
      )
}

# Country model: explicit inputs, no dependency on global data objects.
# S is cumulative exposure in fractions (0..1), by tiva and AI capability.
# It must include the full difficulty grid, even when requesting only one kappa.

calculate_country_ai <- function(country_code, tiva, S, theta, mu0, b,
                                 capabilities = sort(unique(S$`AI capability`)),
                                 missing_exposure = c("error", "zero")) {
  missing_exposure <- match.arg(missing_exposure)
  stopifnot(length(country_code) == 1, !is.na(country_code),
            length(theta) == 1, is.finite(theta), theta > 0,
            length(mu0) == 1, is.finite(mu0),
            length(b) == 1, is.finite(b), b > 0, b < 1,
            length(capabilities) > 0, all(is.finite(capabilities)))
  country <- tiva %>% filter(REF_AREA == country_code, ACTIVITY != "_T")
  if (!nrow(country)) stop("No TiVA data for ", country_code, call. = FALSE)
  if (anyDuplicated(country$ACTIVITY)) {
    stop("TiVA must contain one row per country/industry (select a year first).")
  }
  if (any(!is.finite(country$share_of_gdp)) ||
      any(!is.na(country$Lshare) & !is.finite(country$Lshare))) {
    stop("GDP weights must be finite; Lshare must be finite or NA.")
  }
  # Lshare is a ratio, not a percentage; observed ratios can exceed 1
  # when employee compensation exceeds industry value added.
  if ("REF_AREA" %in% names(S)) S <- S %>% filter(REF_AREA == country_code)
  if (!nrow(S)) stop("No exposure profile for ", country_code, call. = FALSE)
  if (anyDuplicated(S[c("tiva", "AI capability")]) ||
      any(is.na(S$tiva)) || any(!is.finite(S$`AI capability`)) ||
      any(!is.finite(S$S) | S$S < 0 | S$S > 1)) {
    stop("S needs unique industry/capability rows and finite exposure in 0..1.")
  }
  cohorts <- S %>%
    arrange(tiva, `AI capability`) %>%
    group_by(tiva) %>%
    mutate(cohort_wage_bill = S - lag(S, default = 0)) %>%
    ungroup()
  if (any(cohorts$cohort_wage_bill < -1e-12)) {
    stop("Cumulative S must not decrease as AI capability increases.")
  }
  absent <- setdiff(country$ACTIVITY, S$tiva)
  if (length(absent) && missing_exposure == "error") {
    stop("Missing S for ", country_code, ": ", paste(absent, collapse = ", "))
  }
  savings <- calculate_industry_savings(
    cohorts %>% transmute(tiva, difficulty = `AI capability`,
                         cohort_wage_bill = pmax(cohort_wage_bill, 0)),
    S %>% distinct(tiva) %>% mutate(total_wage_bill = 1),
    sort(unique(capabilities)), theta = theta, mu0 = mu0, b = b
  )
  # With total_wage_bill = 1, the wage amounts above are fractions of
  # industry labour costs. TFP gain = saving_share * country labour share.
  industries <- tidyr::crossing(country, `AI capability` = sort(unique(capabilities))) %>%
    left_join(savings, by = c("ACTIVITY" = "tiva", "AI capability"),
              relationship = "one-to-one") %>%
    mutate(
      exposure_available = ACTIVITY %in% S$tiva,
      across(c(S_endog, iota_endog, lambda_endog, saving_share), ~ coalesce(.x, 0)),
      theta = theta, mu0 = mu0, b = b,
      tfp_endog = saving_share * Lshare,
      tfp_constant = S_endog * 0.23 * 0.27 * Lshare
    )
  gdp <- industries %>%
    group_by(REF_AREA, `Reference area`, `AI capability`) %>%
    summarise(
      across(c(tfp_endog, tfp_constant, Lshare),
             ~ weighted.mean(.x, w = share_of_gdp, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    # Country GDP gain (%) = value-added-weighted TFP / labour share * 100.
    mutate(theta = theta, mu0 = mu0, b = b,
           gdp_endog = tfp_endog / Lshare * 100,
           gdp_constant = tfp_constant / Lshare * 100)
  list(industries = industries, gdp = gdp)
}

# One object = one methodology. S can be shared or keyed by REF_AREA.
# Parameters can be a shared list or a table: REF_AREA, theta, mu0, b.
calculate_ai_approach <- function(tiva, S, parameters,
                                  countries = sort(unique(tiva$REF_AREA)),
                                  capabilities = sort(unique(S$`AI capability`)),
                                  missing_exposure = "error") {
  results <- lapply(countries, function(code) {
    p <- if (is.data.frame(parameters)) {
      parameters %>% filter(REF_AREA == code)
    } else parameters
    if (is.data.frame(p) && nrow(p) != 1) {
      stop("Need exactly one parameter row for ", code, call. = FALSE)
    }
    calculate_country_ai(code, tiva, S, theta = p$theta, mu0 = p$mu0, b = p$b,
                         capabilities = capabilities,
                         missing_exposure = missing_exposure)
  })
  list(industries = bind_rows(lapply(results, `[[`, "industries")),
       gdp = bind_rows(lapply(results, `[[`, "gdp")),
       S = S, parameters = parameters)
}
