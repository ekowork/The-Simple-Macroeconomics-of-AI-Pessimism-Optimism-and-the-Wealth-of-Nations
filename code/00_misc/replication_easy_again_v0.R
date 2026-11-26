# =========================================================
# Replikace "AI Optimism: Capability Depth in the Svanberg Model"
# Jednoduchy skript, jen base R (zadne balicky)
# =========================================================

## --- 1. Parametry z Tabulky 2 -----------------------------

b     <- 0.7     # rozptyl velikosti ukolu (task-size dispersion)
Omega <- 0.52    # podil "computer-based" prace na celkove mzdove sume (envelope)

# baseline kalibrace (matched to Acemoglu's adoption moments)
theta_base <- 0.48
mu0_base   <- -1.74

# alternativni kalibrace (updated adoption evidence - Bick et al. 2026)
theta_alt <- 0.65
mu0_alt   <- -0.20

kappa_today <- 2.6   # dnesni frontier (odhadnuto z exposure inversion)
kappa_top   <- 6     # horni konec skaly pouzity v clanku

## --- 2. Zakladni funkce modelu (rovnice 2 a Proposition 1) ----

# hruba uspora na dolar (gamma), rovnice (2)
gamma_fn <- function(z, theta) {
  ifelse(z >= 0, 1 - exp(-theta * z), 0)
}

# stavova promenna m(z)
m_fn <- function(z, mu0, theta) {
  mu0 + log(gamma_fn(z, theta))
}

# podil mzdove sumy kohorty, kde se AI vyplati nasadit (iota)
iota_fn <- function(z, b, mu0, theta) {
  mz <- m_fn(z, mu0, theta)
  ifelse(mz <= 0,
         (1 + b) / 2 * exp((1 - b) / b * mz),
         1 - (1 - b) / 2 * exp(-(1 + b) / b * mz))
}

# uspora kohorty jako podil jejiho mzdoveho balicku (pi = iota * lambda)
pi_fn <- function(z, b, mu0, theta) {
  g  <- gamma_fn(z, theta)
  mz <- m_fn(z, mu0, theta)
  ifelse(mz <= 0,
         g * b * (1 + b) / 2 * exp((1 - b) / b * mz),
         g * (1 - (1 - b / 2) * exp(-mz) +
                b * (1 - b) / 2 * exp(-(1 + b) / b * mz)))
}

# lambda dopocitame jako pi / iota (nemusi mit vlastni uzavreny tvar)
lambda_fn <- function(z, b, mu0, theta) {
  pi_fn(z, b, mu0, theta) / iota_fn(z, b, mu0, theta)
}

## --- 3. Kontrola na cisle z clanku (text u rovnice, z = 1, baseline) ---

z_test <- 1
cat("gamma(1)  =", round(gamma_fn(z_test, theta_base), 3), " (clanek: 0.38)\n")
cat("iota(1)   =", round(iota_fn(z_test, b, mu0_base, theta_base), 3), " (clanek: 0.27)\n")
cat("lambda(1) =", round(lambda_fn(z_test, b, mu0_base, theta_base), 3), " (clanek: 0.27)\n")
cat("pi(1)     =", round(pi_fn(z_test, b, mu0_base, theta_base), 3), " (clanek: 0.07)\n")

## --- 4. Task-level krivka pi(z) - replika Obrazku 4 -------------

z_grid  <- seq(0, 6, by = 0.05)
pi_base <- pi_fn(z_grid, b, mu0_base, theta_base)
pi_alt  <- pi_fn(z_grid, b, mu0_alt,  theta_alt)

plot(z_grid, pi_base, type = "l", lwd = 2, col = "darkgreen",
     xlab = "capability surplus z = kappa - d",
     ylab = "saving per exposed wage dollar",
     main = "Cohort saving rate pi(z)", ylim = c(0, 0.6))
lines(z_grid, pi_alt, lwd = 2, lty = 2, col = "darkgreen")
abline(h = 0.23 * 0.27, lty = 3, col = "grey40")  # Acemoglu konstanta: iota*lambda = 0.062
legend("bottomright",
       legend = c("matched to Acemoglu", "updated evidence", "Acemoglu konstanta"),
       col = c("darkgreen", "darkgreen", "grey40"),
       lty = c(1, 2, 3), lwd = 2, bty = "n")

## --- 5. Agregace na HDP (rovnice 6/7) ----------------------------
#
# pi_bar(kappa) = sum_j omega_j * pi(kappa - d_j)
# Delta Y / Y   = Omega * pi_bar(kappa)
#
# "AI capability" je jemna mrizka kappa (1.0, 1.2, ..., 6.0) na urovni
# profese; "AISA" je kumulativni podil expozice profese do dane urovne
# kappa. difficulty d_j proto neni vlastnost jednotlivych ukolu, ale
# ziskame ji tak, ze pro kazdou profesi rozdelime AISA na prirustky
# po mrizce (diff) - kazdy prirustek je jedna "kohorta ukolu" s
# difficulty = dana urovne kappa.


dat_aisa <-read_excel("./data/aisa/work1207_data.xlsx", sheet = "data") %>% as.data.frame()
dat_aisa = dat_aisa |> filter(complete.cases(dat_aisa))

# zjednoduseni nazvu sloupcu (poradi presne podle tveho head())
names(dat_aisa) <- c("occ_code", "occ_title", "industry", "occ_type",
                      "employment", "wage", "wage_decile",
                      "computer_share", "social_share", "physical_share",
                      "kappa", "skill_share", "core_share", "side_share",
                      "aisa", "aisa_core", "aisa_side")

## 5a. Omega - podil "computer-based" prace na celkove mzdove sume ---

occ <- unique(dat_aisa[, c("occ_code", "employment", "wage", "computer_share")])
occ$wagebill <- occ$employment * occ$wage
occ$envelope <- occ$wagebill * occ$computer_share / 100
occ$w_total  <- occ$wagebill / sum(occ$wagebill,na.rm=T)  # podil profese na CELE mzdove sume

Omega_computed <- sum(occ$envelope,na.rm=T) / sum(occ$wagebill,na.rm=T)
cat("Omega =", round(Omega_computed, 3), " (clanek: 0.52)\n")

## 5b. d_j a omega_j - prirustky AISA po profesich --------------------

dat_aisa <- dat_aisa[order(dat_aisa$occ_code, dat_aisa$kappa), ]
dat_aisa$aisa_marginal <- ave(dat_aisa$aisa, dat_aisa$occ_code,
                               FUN = function(x) c(x[1], diff(x)))
dat_aisa$aisa_marginal[dat_aisa$aisa_marginal < 0] <- 0  # zaokrouhlovaci sumy

dat_aisa <- merge(dat_aisa, occ[, c("occ_code", "w_total")], by = "occ_code")

# vaha (profese x kappa) jako podil na CELKOVE mzdove sume
dat_aisa$w_raw <- dat_aisa$w_total * dat_aisa$aisa_marginal / 100
d_j <- dat_aisa$kappa#- 0.1 # posouvame na stred binu

## kontrola proti clanku - G(kappa), nezavisle na (theta, mu0, b) ----
G_fn <- function(kapp) sum(dat_aisa$w_raw[d_j <= kapp], na.rm=T)
cat("G(2.6) =", round(100 * G_fn(2.6), 1), "% (clanek: 12.7 %)\n")
cat("G(6)   =", round(100 * G_fn(6),   1), "% (clanek: 53 %)\n")

# normalizace tak, aby sum(omega_j) = 1 pres cely envelope (definice v clanku)
omega_j <- dat_aisa$w_raw / sum(dat_aisa$w_raw)

## 5c. Samotna agregace na HDP ----------------------------------------

pi_bar <- function(kappa, b, mu0, theta) {
  sum(omega_j * pi_fn(kappa - d_j, b, mu0, theta))
}

# baseline (matched to Acemoglu)
gdp_today <- Omega_computed * pi_bar(kappa_today, b, mu0_base, theta_base)
gdp_top   <- Omega_computed * pi_bar(kappa_top,   b, mu0_base, theta_base)
cat("HDP dnes:       ", round(100 * gdp_today, 1), "% (clanek: 0.8 %)\n")
cat("HDP na vrcholu: ", round(100 * gdp_top, 1),   "% (clanek: 9.3 %)\n")

# alternativni kalibrace (updated adoption evidence)
gdp_today_alt <- Omega_computed * pi_bar(kappa_today, b, mu0_alt, theta_alt)
gdp_top_alt   <- Omega_computed * pi_bar(kappa_top,   b, mu0_alt, theta_alt)
cat("HDP dnes (alt):       ", round(100 * gdp_today_alt, 1), "% (clanek: 2.1 %)\n")
cat("HDP na vrcholu (alt): ", round(100 * gdp_top_alt, 1),   "% (clanek: 21 %)\n")

