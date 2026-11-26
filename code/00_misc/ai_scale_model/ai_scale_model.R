# AI capability depth and deployment scale -- base R implementation.
# Source: note4_scale_model.pdf, August 2026, equations (2), (6), (7),
# Proposition 1 and Corollaries 1--3. See README.md for identification limits.
# All monetary shares are fractions, NOT percentages. No external R packages.

.scale_numeric <- function(x, name, finite = TRUE) {
  if (!is.numeric(x) || !length(x) || anyNA(x) ||
      (finite && any(!is.finite(x))))
    stop(name, " must be a non-empty numeric vector without missing values.",
         call. = FALSE)
}

# b: dispersion of log deployment wage pools, 0 < b < 1.
# theta: decline in AI variable cost per unit of capability surplus, theta > 0.
# mu0 = median(log W) - log F: log median wage pool / fixed deployment cost.
scale_params <- function(b = 0.7, theta, mu0) {
  for (nm in c("b", "theta", "mu0")) {
    x <- get(nm)
    .scale_numeric(x, nm)
    if (length(x) != 1L) stop(nm, " must be scalar.", call. = FALSE)
  }
  if (b <= 0 || b >= 1 || theta <= 0)
    stop("Require 0 < b < 1 and theta > 0.", call. = FALSE)
  structure(list(b = b, theta = theta, mu0 = mu0), class = "scale_params")
}

# Rounded values reported in Table 2: reference scenarios, not a new fit.
scale_preset <- function(name = c("updated", "acemoglu", "platform")) {
  name <- match.arg(name)
  switch(name,
    updated = scale_params(0.7, 0.65, -0.20),
    acemoglu = scale_params(0.7, 0.48, -1.74),
    platform = scale_params(0.7, 0.48, 0.10))
}

# z = kappa - difficulty. Inf is permitted for the capability ceiling.
# gamma: gross savings / wage bill; iota: viable wage-bill share;
# lambda: NET savings / viable wage bill; pi = iota * lambda.
# p_adopt is a deployment COUNT share, a different object from iota.
scale_cohort <- function(z, pars) {
  .scale_numeric(z, "z", finite = FALSE)
  pars <- do.call(scale_params, pars[c("b", "theta", "mu0")])
  b <- pars$b
  gamma <- -expm1(-pars$theta * pmax(z, 0))
  m <- pars$mu0 + log(gamma)
  iota <- lambda <- net <- p_adopt <- numeric(length(z))
  tail <- gamma > 0 & m <= 0
  body <- gamma > 0 & m > 0
  iota[tail] <- (1 + b) / 2 * exp((1 - b) / b * m[tail])
  lambda[tail] <- b * gamma[tail]
  net[tail] <- iota[tail] * lambda[tail]
  p_adopt[tail] <- 0.5 * exp(m[tail] / b)
  if (any(body)) {
    u <- m[body]
    e <- exp(-(1 + b) / b * u)
    iota[body] <- 1 - (1 - b) / 2 * e
    # An expm1 rearrangement avoids cancellation when m and b are small.
    net[body] <- gamma[body] *
      (b * (1 + b) / 2 - (1 - b^2) * expm1(-u) +
         b * (1 - b) / 2 * expm1(-(1 + b) / b * u))
    lambda[body] <- net[body] / iota[body]
    p_adopt[body] <- 1 - 0.5 * exp(-u / b)
  }
  data.frame(z = z, gamma = gamma, m = m, iota = iota,
             lambda = lambda, pi = net, p_adopt = p_adopt,
             log_wstar_over_median = -m,
             wstar_over_median = exp(-m))
}

# A cost multiplier acts on F, holding capability and task scale fixed.
# For example, factor = 0.2 means a five-fold reduction in fixed cost.
scale_change_cost <- function(pars, factor) {
  .scale_numeric(factor, "factor")
  if (length(factor) != 1L || factor <= 0)
    stop("factor must be a positive scalar.", call. = FALSE)
  scale_params(pars$b, pars$theta, pars$mu0 - log(factor))
}

# Each weight is a share of the TOTAL sector wage bill.
# Therefore sum(weight) within a sector is its computer-work envelope Omega,
# at most 1. If omega sums to 1 within that envelope, use weight = Omega * omega.
scale_tasks <- function(difficulty, weight, sector = "all") {
  .scale_numeric(difficulty, "difficulty")
  .scale_numeric(weight, "weight")
  n <- length(difficulty)
  if (length(weight) != n || !(length(sector) %in% c(1L, n)))
    stop("difficulty, weight and sector have incompatible lengths.", call. = FALSE)
  sector <- rep(as.character(sector), length.out = n)
  if (any(difficulty < 0) || any(weight < 0) || anyNA(sector) || any(!nzchar(sector)))
    stop("Require nonnegative difficulty/weight and nonmissing sector names.",
         call. = FALSE)
  if (any(tapply(weight, sector, sum) > 1 + 1e-10))
    stop("Weights must sum to at most 1 in each sector (total wage-bill shares).",
         call. = FALSE)
  data.frame(sector = sector, difficulty = difficulty, weight = weight,
             stringsAsFactors = FALSE)
}

.scale_check_tasks <- function(tasks) {
  required <- c("sector", "difficulty", "weight")
  if (!is.data.frame(tasks) || !all(required %in% names(tasks)))
    stop("tasks must contain sector, difficulty and weight.", call. = FALSE)
  scale_tasks(tasks$difficulty, tasks$weight, tasks$sector)
}

.scale_moments <- function(d, w, kappa, pars) {
  x <- scale_cohort(kappa - d, pars)
  exposure <- sum(w[d <= kappa])
  viable <- sum(w * x$iota)
  saving <- sum(w * x$pi)
  c(exposure = exposure, viable_share = viable, saving_share = saving,
    iota = if (exposure > 0) viable / exposure else 0,
    lambda = if (viable > 0) saving / viable else 0,
    pi_exposed = if (exposure > 0) saving / exposure else 0,
    gross_saving_viable = if (viable > 0) sum(w * x$iota * x$gamma) / viable else 0,
    lambda_defined = as.numeric(viable > 0))
}

# Equations (6)--(7). Ratios of wage-bill sums, never mean(lambda).
# saving_share = exposure * iota * lambda already includes exposure.
# kappa: AI capability on the same scale as difficulty.
scale_aggregate <- function(tasks, kappa, pars) {
  tasks <- .scale_check_tasks(tasks)
  .scale_numeric(kappa, "kappa", finite = FALSE)
  if (any(kappa < 0)) stop("kappa must be nonnegative.", call. = FALSE)
  out <- lapply(split(tasks, tasks$sector), function(t) {
    vals <- t(vapply(kappa, function(k)
      .scale_moments(t$difficulty, t$weight, k, pars), numeric(8)))
    data.frame(sector = t$sector[1], kappa = kappa,
               envelope = sum(t$weight), vals, row.names = NULL)
  })
  ans <- do.call(rbind, out)
  ans$lambda_defined <- as.logical(ans$lambda_defined)
  rownames(ans) <- NULL
  ans
}

# Turn sector distributions into a national distribution with sector wage-bill
# weights. sector_weights must be a named vector summing to 1, with exact coverage.
scale_pool <- function(tasks, sector_weights) {
  tasks <- .scale_check_tasks(tasks)
  .scale_numeric(sector_weights, "sector_weights")
  if (is.null(names(sector_weights)) || anyDuplicated(names(sector_weights)) ||
      any(sector_weights < 0) || abs(sum(sector_weights) - 1) > 1e-8 ||
      !setequal(names(sector_weights), unique(tasks$sector)))
    stop("sector_weights must cover each sector once and sum to 1.", call. = FALSE)
  scale_tasks(tasks$difficulty,
              tasks$weight * unname(sector_weights[tasks$sector]), "national")
}

# Gauss-Legendre nodes for integrating a uniform difficulty bin.
.scale_quadrature <- function(n) {
  if (length(n) != 1L || !is.numeric(n) || !is.finite(n) || n < 1 ||
      n != as.integer(n) || n > 100)
    stop("quadrature must be an integer from 1 to 100.", call. = FALSE)
  if (n == 1L) return(list(x = 0, w = 2))
  j <- seq_len(n - 1L)
  a <- j / sqrt(4 * j^2 - 1)
  mat <- matrix(0, n, n)
  mat[cbind(j, j + 1L)] <- mat[cbind(j + 1L, j)] <- a
  eig <- eigen(mat, symmetric = TRUE)
  list(x = eig$values, w = 2 * eig$vectors[1, ]^2)
}

# Approximate a difficulty distribution from a cumulative AISA curve.
# curves: sector, kappa, exposure (exposure is a TOTAL wage-bill fraction).
# Positive differences in exposure are wage-bill masses of difficulty bins.
# uniform: quadrature within each bin; upper: put all mass at its right edge.
# first_bin="uniform" explicitly assumes uniform difficulties from lower_bound
# to the first observed kappa when the curve is left-censored.
scale_from_cdf <- function(curves, allocation = c("uniform", "upper"),
                           quadrature = 16L, lower_bound = 0,
                           first_bin = c("error", "uniform")) {
  allocation <- match.arg(allocation)
  first_bin <- match.arg(first_bin)
  if (!is.data.frame(curves) || !all(c("sector", "kappa", "exposure") %in% names(curves)))
    stop("curves must contain sector, kappa and exposure.", call. = FALSE)
  .scale_numeric(curves$kappa, "curves$kappa")
  .scale_numeric(curves$exposure, "curves$exposure")
  .scale_numeric(lower_bound, "lower_bound")
  if (length(lower_bound) != 1L || lower_bound < 0 ||
      any(curves$kappa < lower_bound) || any(curves$exposure < 0) ||
      any(curves$exposure > 1)) stop("Invalid difficulty bounds or exposure fractions.")
  if (anyNA(curves$sector) || any(!nzchar(as.character(curves$sector))))
    stop("curves$sector must be nonmissing.", call. = FALSE)
  quad <- .scale_quadrature(quadrature)
  censored <- character()
  out <- lapply(split(curves, as.character(curves$sector)), function(x) {
    x <- x[order(x$kappa), ]
    if (anyDuplicated(x$kappa)) stop("Duplicate capability rows in sector ", x$sector[1])
    if (any(diff(x$exposure) < -1e-12))
      stop("AISA decreases in sector ", x$sector[1],
           "; check sample composition. The code does not repair nonmonotone curves.")
    # Only machine-level negative noise is removed; no isotonic fit or smoothing.
    s <- cummax(x$exposure)
    if (x$kappa[1] > lower_bound && s[1] > 0) {
      if (first_bin == "error")
        stop("Positive exposure at the first observed kappa in sector ", x$sector[1],
             ". Supply earlier points or explicitly set first_bin='uniform'.")
      censored <<- c(censored, as.character(x$sector[1]))
    }
    lo <- c(lower_bound, head(x$kappa, -1))
    hi <- x$kappa
    mass <- diff(c(0, s))
    rows <- lapply(which(mass > 0), function(j) {
      if (allocation == "upper" || hi[j] == lo[j]) {
        d <- hi[j]; w <- mass[j]
      } else {
        d <- lo[j] + (hi[j] - lo[j]) * (quad$x + 1) / 2
        w <- mass[j] * quad$w / 2
      }
      scale_tasks(d, w, as.character(x$sector[1]))
    })
    if (!length(rows)) return(scale_tasks(lower_bound, 0, as.character(x$sector[1])))
    do.call(rbind, rows)
  })
  if (length(censored)) warning(
    "Assumed a first difficulty bin starting at ", lower_bound, " for ",
    length(censored), " sector(s). Fine-grid input or raw difficulties are preferable.",
    call. = FALSE)
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  attr(ans, "cdf_approximation") <- list(allocation = allocation, quadrature = quadrature,
    lower_bound = lower_bound, left_censored_sectors = censored)
  ans
}

# Match national adoption and saving moments at a fixed capability.
# Fix b externally: two moments identify theta and mu0 conditional on b and G.
# saving_measure="net" replicates the note. "gross" matches time-saving
# evidence to gross_saving_viable instead of assuming it measures net savings.
scale_calibrate <- function(tasks, kappa0 = 2.6, b = 0.7,
                            target_iota = 0.49, target_lambda = 0.33,
                            saving_measure = c("net", "gross"),
                            theta_bounds = c(1e-6, 1000), tolerance = 1e-7) {
  saving_measure <- match.arg(saving_measure)
  tasks <- .scale_check_tasks(tasks)
  if (length(unique(tasks$sector)) != 1L)
    stop("Pool sectors with scale_pool() before national calibration.", call. = FALSE)
  for (nm in c("kappa0", "target_iota", "target_lambda", "tolerance")) {
    val <- get(nm); .scale_numeric(val, nm)
    if (length(val) != 1L) stop(nm, " must be scalar.")
  }
  scale_params(b, 1, 0)
  if (kappa0 < 0 || target_iota <= 0 || target_iota >= 1 ||
      target_lambda <= 0 || target_lambda >= 1 || tolerance <= 0)
    stop("Invalid calibration targets or tolerance.", call. = FALSE)
  .scale_numeric(theta_bounds, "theta_bounds")
  if (length(theta_bounds) != 2L || theta_bounds[1] <= 0 ||
      theta_bounds[2] <= theta_bounds[1]) stop("Invalid theta_bounds.")
  d <- tasks$difficulty; w <- tasks$weight
  exposed <- sum(w[d <= kappa0])
  positive <- sum(w[d < kappa0])
  if (exposed <= 0 || target_iota >= positive / exposed)
    stop("Insufficient positive-depth exposure for the iota target. At d=kappa, savings are zero.")
  # Conditional on theta, iota strictly increases with mu0.
  at_theta <- function(logtheta) {
    theta <- exp(logtheta)
    f <- function(mu) .scale_moments(d, w, kappa0,
      scale_params(b, theta, mu))["iota"] - target_iota
    lo <- -16; hi <- 16
    for (j in seq_len(30)) {
      if (f(lo) <= 0) break
      lo <- lo * 2
    }
    for (j in seq_len(30)) {
      if (f(hi) >= 0) break
      hi <- hi * 2
    }
    if (f(lo) > 0 || f(hi) < 0) stop("Could not bracket deployment affordability.")
    mu <- uniroot(f, c(lo, hi), tol = 1e-11)$root
    pars <- scale_params(b, theta, mu)
    list(pars = pars, moments = .scale_moments(d, w, kappa0, pars))
  }
  metric <- if (saving_measure == "net") "lambda" else "gross_saving_viable"
  objective <- function(logtheta) at_theta(logtheta)$moments[metric] - target_lambda
  bracket <- log(theta_bounds)
  f_bounds <- vapply(bracket, objective, numeric(1))
  if (f_bounds[1] * f_bounds[2] > 0)
    stop("Saving target cannot be bracketed over theta_bounds. Check the target, b, and difficulty distribution.")
  root <- uniroot(objective, bracket, tol = 1e-10)
  fit <- at_theta(root$root)
  residuals <- c(iota = unname(fit$moments["iota"] - target_iota),
                 saving = unname(fit$moments[metric] - target_lambda))
  if (max(abs(residuals)) > tolerance) stop("Calibration did not meet its moment tolerance.")
  fit$kappa0 <- kappa0
  fit$targets <- c(iota = target_iota, saving = target_lambda)
  fit$saving_measure <- saving_measure
  fit$residuals <- residuals
  fit$max_m_at_anchor <- max(scale_cohort(kappa0 - d[w > 0], fit$pars)$m)
  fit$theta_bounds <- theta_bounds
  class(fit) <- "scale_fit"
  fit
}

print.scale_fit <- function(x, ...) {
  cat("Scale model fit at kappa =", x$kappa0,
      "(saving target:", x$saving_measure, ")\n")
  print(unlist(x$pars))
  print(x$moments[c("exposure", "iota", "lambda", "gross_saving_viable", "saving_share")])
  cat("Largest moment residual:", format(max(abs(x$residuals)), scientific = TRUE), "\n")
  invisible(x)
}

# Proposition 2(ii), continuous exponential difficulty.
# eta: rate of the exponential difficulty distribution (mean difficulty = 1/eta).
# Evaluate the integral directly. R's pbeta() is NOT valid for the negative
# second shape parameter in the note's incomplete-beta expression.
scale_exponential <- function(kappa, eta, pars) {
  .scale_numeric(kappa, "kappa", finite = FALSE)
  .scale_numeric(eta, "eta")
  if (length(eta) != 1L || eta <= 0 || any(kappa < 0)) stop("Invalid eta or kappa.")
  vapply(kappa, function(k) {
    if (k == 0) return(0)
    if (is.infinite(k)) return(scale_cohort(Inf, pars)$pi)
    integrate(function(d) scale_cohort(k - d, pars)$pi * dexp(d, eta),
              0, k, rel.tol = 1e-9, subdivisions = 1000L)$value
  }, numeric(1))
}

.scale_keys <- function(sector, kappa) {
  # A tiny tolerance for independently generated seq() grids; duplicate checks
  # ensure this does not silently collapse genuinely distinct observations.
  paste(as.character(sector), sprintf("%.10f", kappa), sep = "\r")
}

# Add endogenous columns to the existing tfp data frame AFTER its /100 conversion.
# Keep the original baseline columns for comparisons and existing figures.
scale_attach_tfp <- function(tfp, margins, exposure_tolerance = 1e-8) {
  required <- c("ACTIVITY", "AI capability", "AISA_bill_weighted", "Lshare")
  if (!all(required %in% names(tfp))) stop("tfp lacks: ", paste(setdiff(required, names(tfp)), collapse = ", "))
  required_m <- c("sector", "kappa", "exposure", "iota", "lambda", "saving_share")
  if (!all(required_m %in% names(margins))) stop("Incomplete margins table.")
  keys <- .scale_keys(margins$sector, margins$kappa)
  if (anyDuplicated(keys)) stop("Duplicate sector/capability rows in margins.")
  at <- match(.scale_keys(tfp$ACTIVITY, tfp$`AI capability`), keys)
  if (anyNA(at)) stop("Some tfp rows have no sector/capability match in margins.")
  m <- margins[at, ]
  s <- tfp$AISA_bill_weighted
  if (!is.numeric(s) || anyNA(s) || any(s < 0 | s > 1))
    stop("tfp$AISA_bill_weighted must be a fraction in [0,1], after /100.")
  if (max(abs(s - m$exposure)) > exposure_tolerance)
    stop("Reconstructed exposure does not match tfp. Check the grid, units and sector coverage.")
  if (!is.numeric(tfp$Lshare)) stop("tfp$Lshare must be numeric.")
  tfp$implementable_endo <- m$iota
  tfp$labour_savings_endo <- m$lambda
  tfp$saving_share_endo <- m$saving_share
  # saving_share ALREADY includes AISA exposure. Do not multiply it by AISA again.
  tfp$tfp_endo_bill_weighted <- m$saving_share * tfp$Lshare
  tfp
}

# Aggregate the attached tfp table using country sector value-added weights.
# For missing="error", incomplete Lshare is an error. "common_sample" uses
# the same available sectors for both models and returns coverage explicitly.
# Lshare here follows the original project's effective national labour share;
# sum(a_i * L_i) / sum(a_i). This is not an independently measured capital share.
scale_country <- function(tfp, target_iota = 0.49, target_lambda = 0.33,
                           missing = c("error", "common_sample")) {
  missing <- match.arg(missing)
  required <- c("REF_AREA", "ACTIVITY", "AI capability", "share_of_gdp", "Lshare",
                "AISA_bill_weighted", "implementable_endo", "saving_share_endo")
  if (!all(required %in% names(tfp))) stop("Incomplete tfp input for country aggregation.")
  for (nm in c("target_iota", "target_lambda")) {
    x <- get(nm); .scale_numeric(x, nm)
    if (length(x) != 1L || x < 0 || x > 1) stop("Invalid frozen-model target.")
  }
  if (anyNA(tfp$REF_AREA) || anyNA(tfp$ACTIVITY) || anyNA(tfp$`AI capability`))
    stop("Country, industry and capability keys must be nonmissing.")
  for (nm in c("share_of_gdp", "Lshare", "AISA_bill_weighted", "implementable_endo", "saving_share_endo"))
    if (!is.numeric(tfp[[nm]])) stop(nm, " must be numeric.")
  groups <- split(tfp, .scale_keys(tfp$REF_AREA, tfp$`AI capability`))
  out <- lapply(groups, function(x) {
    if (anyDuplicated(x$ACTIVITY)) stop("Duplicate industries within a country/capability group.")
    a <- x$share_of_gdp
    if (anyNA(a) || any(!is.finite(a)) || any(a < 0) || sum(a) <= 0 || sum(a) > 1 + 1e-6)
      stop("Invalid/overlapping sector VA shares in country ", x$REF_AREA[1])
    if (any(x$Lshare < 0, na.rm = TRUE)) stop("Negative sector labour share.")
    ok <- is.finite(x$Lshare) & is.finite(x$AISA_bill_weighted) &
      is.finite(x$implementable_endo) & is.finite(x$saving_share_endo)
    if (missing == "error" && any(!ok & a > 0))
      stop("Missing inputs in ", x$REF_AREA[1], ". Resolve them or explicitly use missing='common_sample'.")
    coverage <- sum(a[ok]); available <- sum(a)
    if (coverage <= 0) stop("No usable sector VA in country ", x$REF_AREA[1])
    x <- x[ok, ]; a <- a[ok]
    wage <- a * x$Lshare
    wage_total <- sum(wage)
    if (wage_total <= 0) stop("No positive labour share in country ", x$REF_AREA[1])
    exposed <- sum(wage * x$AISA_bill_weighted)
    viable <- sum(wage * x$AISA_bill_weighted * x$implementable_endo)
    saved <- sum(wage * x$saving_share_endo)
    tfp_endo <- saved / coverage
    tfp_frozen <- exposed * target_iota * target_lambda / coverage
    labour <- wage_total / coverage
    ans <- data.frame(REF_AREA = x$REF_AREA[1], kappa = x$`AI capability`[1],
      va_share_available = available, va_share_used = coverage,
      missing_va_share = available - coverage,
      Lshare = labour, exposure_wage_weighted = exposed / wage_total,
      implementable_endo = if (exposed > 0) viable / exposed else 0,
      labour_savings_endo = if (viable > 0) saved / viable else 0,
      tfp_endo_bill_weighted = tfp_endo, tfp_frozen_bill_weighted = tfp_frozen,
      gdp_endo = 100 * tfp_endo / labour,
      gdp_frozen = 100 * tfp_frozen / labour)
    names(ans)[names(ans) == "kappa"] <- "AI capability"
    for (nm in intersect(c("Reference area", "regions"), names(x))) {
      vals <- unique(x[[nm]][!is.na(x[[nm]])])
      if (length(vals) > 1L) stop("Inconsistent country metadata: ", nm)
      ans[[nm]] <- if (length(vals)) as.character(vals) else NA_character_
    }
    ans
  })
  ans <- do.call(rbind, out); rownames(ans) <- NULL
  ans
}

# Add the SAME external digital-readiness multiplier as the current project.
# No automatic cap at 1: internet_use is a relative index, not a probability.
scale_add_country_data <- function(gdp, world_internet, world_gdp) {
  inputs <- list(world_internet, world_gdp)
  columns <- c("internet_use", "gdp_2023")
  for (i in seq_along(inputs)) {
    dat <- inputs[[i]]; nm <- columns[i]
    if (!all(c("Country Code", nm) %in% names(dat)) || anyNA(dat$`Country Code`) ||
        anyDuplicated(dat$`Country Code`)) stop("Invalid country lookup for ", nm)
    val <- dat[[nm]][match(gdp$REF_AREA, dat$`Country Code`)]
    if (!is.numeric(val) || anyNA(val) || any(!is.finite(val)) ||
        any(val < 0) || (nm == "gdp_2023" && any(val == 0)))
      stop("Missing or invalid country values in ", nm, ". Use one explicit country sample.")
    gdp[[nm]] <- val
  }
  gdp$gdp_endo_adj_internet <- gdp$gdp_endo * gdp$internet_use
  gdp$gdp_frozen_adj_internet <- gdp$gdp_frozen * gdp$internet_use
  gdp
}

# Global results: calculate country GDP ratios FIRST, then GDP-weight them.
# A product/ratio of separate cross-country averages is a different calculation.
scale_global <- function(gdp) {
  required <- c("REF_AREA", "AI capability", "gdp_2023", "gdp_endo", "gdp_frozen",
                "gdp_endo_adj_internet", "gdp_frozen_adj_internet")
  if (!all(required %in% names(gdp))) stop("Incomplete country GDP table.")
  groups <- split(gdp, gdp$`AI capability`)
  if (!length(groups)) stop("No country GDP rows.")
  sample <- sort(groups[[1]]$REF_AREA)
  cols <- required[4:7]
  ans <- lapply(groups, function(x) {
    if (anyNA(x$REF_AREA) || anyDuplicated(x$REF_AREA) ||
        !identical(sort(x$REF_AREA), sample))
      stop("Global aggregation requires the same countries at every capability.")
    for (nm in c("gdp_2023", cols)) .scale_numeric(x[[nm]], nm)
    if (any(x$gdp_2023 <= 0)) stop("GDP weights must be positive.")
    vals <- vapply(cols, function(nm) weighted.mean(x[[nm]], x$gdp_2023), numeric(1))
    data.frame(kappa = x$`AI capability`[1], countries = nrow(x),
               as.list(vals), check.names = FALSE)
  })
  ans <- do.call(rbind, ans)
  ans <- ans[order(ans$kappa), ]; rownames(ans) <- NULL
  names(ans)[1] <- "AI capability"
  ans
}
