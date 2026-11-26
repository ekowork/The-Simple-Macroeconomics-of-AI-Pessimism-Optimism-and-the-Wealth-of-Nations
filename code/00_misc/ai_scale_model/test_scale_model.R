# Run from the parent directory: Rscript ai_scale_model/test_scale_model.R
source("ai_scale_model/ai_scale_model.R")
cat("VALIDATION OF THE SCALE MODEL\n")
cat("All country figures printed below use a SYNTHETIC test economy, not empirical data.\n\n")
checks <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop("FAILED: ", label, call. = FALSE)
  checks <<- checks + 1L
}
near <- function(x, y, tol = 1e-8) isTRUE(all.equal(unname(x), unname(y),
                                               tolerance = tol, check.attributes = FALSE))
throws <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")

# Independent integration of the original expectation definitions in log W.
# Set median(log W)=0, F=exp(-mu0); normalize by E[W]=1/(1-b^2).
# Combine exponents before exponentiation to avoid exp(x) overflow.
numeric_cohort <- function(z, pars) {
  g <- -expm1(-pars$theta * z)
  if (g <= 0) return(c(iota = 0, lambda = 0, pi = 0, p_adopt = 0))
  b <- pars$b; cutoff <- -pars$mu0 - log(g)
  integrate_split <- function(f) {
    bounds <- if (cutoff < 0) c(cutoff, 0, Inf) else c(cutoff, Inf)
    sum(vapply(seq_len(length(bounds) - 1L), function(i)
      integrate(f, bounds[i], bounds[i + 1L], rel.tol = 1e-10,
                abs.tol = 1e-12, subdivisions = 1000L)$value, numeric(1)))
  }
  adopted_bill <- integrate_split(function(x) exp(x - abs(x) / b) / (2 * b))
  net <- integrate_split(function(x)
    (g * exp(x - abs(x) / b) - exp(-pars$mu0 - abs(x) / b)) / (2 * b))
  count <- integrate_split(function(x) exp(-abs(x) / b) / (2 * b))
  c(iota = adopted_bill * (1 - b^2), lambda = net / adopted_bill,
    pi = net * (1 - b^2), p_adopt = count)
}

grid <- expand.grid(b = c(0.2, 0.7, 0.95), theta = c(0.1, 1.7),
                    mu0 = c(-3, 0, 3), z = c(0.05, 1, 5))
integration_error <- 0
for (i in seq_len(nrow(grid))) {
  p <- scale_params(grid$b[i], grid$theta[i], grid$mu0[i])
  closed <- unlist(scale_cohort(grid$z[i], p)[c("iota", "lambda", "pi", "p_adopt")])
  direct <- numeric_cohort(grid$z[i], p)
  err <- max(abs(closed - direct))
  integration_error <- max(integration_error, err)
  check(err < 2e-8, paste("expectation integrals", i))
}

# Check both sides of the Laplace kink and their first derivatives.
g <- 1 - exp(-0.8)
p0 <- scale_params(0.7, 0.8, -log(g))
at0 <- scale_cohort(1, p0)
check(near(at0$iota, 0.85) && near(at0$lambda, 0.7 * g), "value at m=0")
eps <- 1e-5
left <- scale_cohort(1, scale_params(0.7, 0.8, p0$mu0 - eps))
right <- scale_cohort(1, scale_params(0.7, 0.8, p0$mu0 + eps))
check(abs((at0$iota - left$iota) / eps - (right$iota - at0$iota) / eps) < 1e-5,
      "first derivative at branch boundary")

p <- scale_preset("acemoglu")
zero <- scale_cohort(c(-Inf, -1, 0), p)
check(all(zero$gamma == 0 & zero$iota == 0 & zero$lambda == 0 & zero$pi == 0), "infeasible/frontier cohorts")
check(all(is.infinite(zero$wstar_over_median)), "infinite break-even scale at frontier")
example <- scale_cohort(1, p)
check(abs(example$pi - 0.07117564) < 1e-8, "Section 6 numerical example")
ceiling <- scale_cohort(Inf, p)
check(abs(ceiling$pi - 0.28226522) < 1e-8, "baseline ceiling")
check(near(scale_cohort(Inf, scale_params(.7, 1, 0))$iota, .85), "mu0=0 ceiling")
cheap <- scale_cohort(c(.1, 1, 5), scale_params(.7, 1, 40))
check(max(abs(cheap$lambda - cheap$gamma)) < 1e-12, "vanishing fixed cost")
check(near(scale_change_cost(p, .2)$mu0, p$mu0 + log(5)), "cost multiplier sign")
for (p in list(scale_preset("acemoglu"), scale_preset("updated"), scale_params(.7, 1, 2))) {
  x <- scale_cohort(seq(0, 10, length.out = 501), p)
  check(all(diff(x$iota) >= -1e-12) && all(diff(x$lambda) >= -1e-12) &&
          all(diff(x$pi) >= -1e-12), "cohort monotonicity")
  check(all(x$pi >= 0 & x$pi <= x$iota & x$lambda <= x$gamma + 1e-12), "economic bounds")
  y <- scale_cohort(x$z, scale_change_cost(p, .2))
  check(all(y$iota >= x$iota - 1e-12 & y$lambda >= x$lambda - 1e-12), "cheaper deployment")
}

# Equations (6) and (7) must coincide for arbitrary weights and zero exposure.
tasks <- scale_tasks(c(.4, 1.5, 2.5, 4), c(.05, .1, .2, .15))
p <- scale_preset("updated")
agg <- scale_aggregate(tasks, c(0, 1, 2.6, 6), p)
check(near(agg$saving_share, agg$exposure * agg$iota * agg$lambda), "four-share identity")
check(!agg$lambda_defined[1] && agg$lambda[1] == 0, "no-adoption convention")
direct <- sum(tasks$weight * scale_cohort(2.6 - tasks$difficulty, p)$pi)
check(near(direct, agg$saving_share[3]), "direct wage-bill aggregation")

# Composition can lower conditional adoption margins as new difficult tasks enter.
mix <- scale_tasks(c(0, 2), c(.01, .8))
mixagg <- scale_aggregate(mix, c(1.99, 2.01), p)
check(diff(mixagg$iota) < 0 && diff(mixagg$lambda) < 0 &&
        diff(mixagg$saving_share) > 0, "do not impose aggregate margin monotonicity")

# Recover known parameters on the tail and across both branches.
caldata <- scale_tasks(c(.3, .9, 1.4, 2.2, 3.4, 5), c(.04, .08, .1, .12, .15, .11))
parameter_error <- 0
for (truth in list(scale_params(.7, .8, -.5), scale_params(.7, .8, 1.2))) {
  mom <- scale_aggregate(caldata, 2.6, truth)
  fit <- scale_calibrate(caldata, 2.6, .7, mom$iota, mom$lambda)
  err <- max(abs(unlist(fit$pars) - unlist(truth)))
  parameter_error <- max(parameter_error, err)
  check(err < 1e-6, "recover theta and mu0")
}
netfit <- scale_calibrate(caldata)
grossfit <- scale_calibrate(caldata, saving_measure = "gross")
check(near(netfit$moments["iota"], .49) && near(netfit$moments["lambda"], .33), "updated targets")
check(near(grossfit$moments["gross_saving_viable"], .33) &&
        grossfit$moments["lambda"] < .33, "gross is not net")

# Recover observed CDF knots; check quadrature convergence for a uniform density.
curve <- data.frame(sector = "A", kappa = c(0, 1, 2.6, 4, 6), exposure = c(0, .05, .2, .4, .6))
bins <- scale_from_cdf(curve)
check(near(scale_aggregate(bins, curve$kappa, p)$exposure, curve$exposure), "CDF knot reconstruction")
single_bin <- data.frame(sector = "A", kappa = c(0, 6), exposure = c(0, .6))
q16 <- scale_aggregate(scale_from_cdf(single_bin, quadrature = 16), 6, p)$saving_share
q64 <- scale_aggregate(scale_from_cdf(single_bin, quadrature = 64), 6, p)$saving_share
truth <- integrate(function(d) .1 * scale_cohort(6 - d, p)$pi, 0, 6, rel.tol = 1e-10)$value
check(abs(q64 - truth) < 1e-7 && abs(q64 - truth) < abs(q16 - truth), "uniform-bin quadrature")
check(throws(scale_from_cdf(curve[-1, ])), "reject implicit left-censored allocation")
bad <- curve; bad$exposure[4] <- .1
check(throws(scale_from_cdf(bad)), "reject nonmonotone AISA")
check(throws(scale_params(1, 1, 0)), "finite wage-bill requirement")
check(throws(scale_tasks(1, 10)), "reject percentage weights")
check(throws(scale_calibrate(caldata, target_lambda = .99)), "reject unmatchable targets")

# Independently evaluate the incomplete-beta representation in Proposition 2.
eta <- .8; theta <- .48; p <- scale_params(.7, theta, -1.74); k <- 2.6
c0 <- .7 * 1.7 / 2 * exp(.3 / .7 * p$mu0)
uend <- -expm1(-theta * k)
beta_integral <- c0 * eta / theta * exp(-eta * k) *
  integrate(function(u) u^(1 / .7) * (1 - u)^(-eta / theta - 1),
            0, uend, rel.tol = 1e-10)$value
check(near(scale_exponential(k, eta, p), beta_integral), "Proposition 2")

# Full integration script on a deliberately SYNTHETIC two-country economy.
# This exercises the exact entry point the user will source in their project.
kappas <- c(0, 1, 2.6, 4, 6)
agg_us_tiva <- data.frame(
  tiva = rep(c("A", "J"), each = 5), `AI capability` = rep(kappas, 2),
  AISA_bill_weighted = c(0, 2, 12, 25, 30, 0, 8, 30, 65, 85),
  total_wage_bill = rep(c(100, 300), each = 5), check.names = FALSE)
tfp <- expand.grid(REF_AREA = c("USA", "CZE"), ACTIVITY = c("A", "J", "T"),
                   kappa = kappas, stringsAsFactors = FALSE)
names(tfp)[3] <- "AI capability"
tfp$share_of_gdp <- ifelse(tfp$ACTIVITY == "T", .1,
  ifelse(tfp$ACTIVITY == "A", ifelse(tfp$REF_AREA == "USA", .25, .5),
    ifelse(tfp$REF_AREA == "USA", .65, .4)))
tfp$Lshare <- ifelse(tfp$ACTIVITY == "A", .45, ifelse(tfp$ACTIVITY == "J", .7, .6))
idx <- match(.scale_keys(tfp$ACTIVITY, tfp$`AI capability`),
             .scale_keys(agg_us_tiva$tiva, agg_us_tiva$`AI capability`))
tfp$AISA_bill_weighted <- agg_us_tiva$AISA_bill_weighted[idx] / 100
tfp$AISA_bill_weighted[tfp$ACTIVITY == "T"] <- 0
world_internet <- data.frame(`Country Code` = c("USA", "CZE"),
                             internet_use = c(1, .95), check.names = FALSE)
world_gdp <- data.frame(`Country Code` = c("USA", "CZE"),
                        gdp_2023 = c(1000, 100), check.names = FALSE)
before <- tfp
source("ai_scale_model/integrate_project.R")
check(identical(before, tfp), "integration preserves original tfp")
check(nrow(tfp_endogenous) == nrow(tfp) && nrow(global_endogenous) == length(kappas), "integration dimensions")
check(all(global_endogenous$gdp_endo >= 0), "finite nonnegative global output")
country6 <- gdp_endogenous[gdp_endogenous$`AI capability` == 6, ]
check(near(global_endogenous$gdp_endo[global_endogenous$`AI capability` == 6],
           sum(country6$gdp_endo * country6$gdp_2023) / sum(country6$gdp_2023)), "country-first global weights")
broken <- tfp_endogenous; broken$Lshare[1] <- NA_real_
check(throws(scale_country(broken)), "missing inputs are not silently dropped")
check(any(scale_country(broken, missing = "common_sample")$missing_va_share > 0), "report common-sample coverage")
check(throws(scale_country(rbind(tfp_endogenous, tfp_endogenous[1, ]))), "reject duplicate sectors")

cat("\nPASS:", checks, "checks\n")
cat("Maximum closed-form vs numerical-integration error:", format(integration_error, scientific = TRUE), "\n")
cat("Maximum recovered-parameter error:", format(parameter_error, scientific = TRUE), "\n")
cat("Runtime:", R.version.string, "\n")
cat("Original country microdata were not supplied; the integration fixture is SYNTHETIC.\n")
