
#--------------------------------------------------#
# 1. Data
#--------------------------------------------------#


income <- world_gdppc |> 
  transmute(country = `Country Code`, gdppc=gdppc_ppp)


d <- res_i %>%
  filter(
    scenario %in% c("S_us, mu0_c", "baseline", "S_c, mu0_c, internet"),
    kappa %in% c(2.6, 6),
    tiva != "_T",
    VALUE_ADDED > 0,
    !is.na(Lshare),
    !is.na(saving_share)
  ) %>%
  transmute(
    scenario,
    country, kappa, tiva,
    a = VALUE_ADDED,
    L = Lshare,
    q = saving_share
  ) %>%
  inner_join(income, by = "country")

#--------------------------------------------------#
# 2. Common industry reference
#--------------------------------------------------#

ref <- d %>%
  group_by(scenario,kappa, tiva) %>%
  summarise(
    a0 = sum(a),
    comp0 = sum(a * L),
    saved0 = sum(a * L * q),
    .groups = "drop"
  ) %>%
  group_by(scenario,kappa) %>%
  mutate(
    L0 = comp0 / a0,
    q0 = if_else(comp0 > 0, saved0 / comp0, 0),
    a0 = a0 / sum(a0)
  ) %>%
  ungroup() %>%
  select(scenario,kappa, tiva, a0, L0, q0)


d <- d %>%
  distinct(kappa, country, gdppc) %>%
  inner_join(ref, by = "kappa", relationship = "many-to-many") %>%
  left_join(
    d, by = c("scenario", "kappa", "country", "gdppc", "tiva")
  ) %>%
  mutate(
    a = coalesce(a, 0),
    L = coalesce(L, L0),
    q = coalesce(q, q0)
  ) %>%
  group_by(scenario,kappa, country) %>%
  mutate(a = a / sum(a)) %>%
  ungroup()

#--------------------------------------------------#
# 3. Shapley decomposition
#--------------------------------------------------#

shapley <- d %>%
  group_by(scenario,country, kappa, gdppc) %>%
  summarise(
    y000 = 100 * sum(a0 * L0 * q0) / sum(a0 * L0),
    y100 = 100 * sum(a  * L0 * q0) / sum(a  * L0),
    y010 = 100 * sum(a0 * L  * q0) / sum(a0 * L),
    y001 = 100 * sum(a0 * L0 * q ) / sum(a0 * L0),
    y110 = 100 * sum(a  * L  * q0) / sum(a  * L),
    y101 = 100 * sum(a  * L0 * q ) / sum(a  * L0),
    y011 = 100 * sum(a0 * L  * q ) / sum(a0 * L),
    y111 = 100 * sum(a  * L  * q ) / sum(a  * L),
    .groups = "drop"
  ) %>%
  mutate(
    gdp = y111,
    
    VA = (y100 - y000)/3 +
      (y110 - y010 + y101 - y001)/6 +
      (y111 - y011)/3,
    
    Labour = (y010 - y000)/3 +
      (y110 - y100 + y011 - y001)/6 +
      (y111 - y101)/3,
    
    Savings = (y001 - y000)/3 +
      (y101 - y100 + y011 - y010)/6 +
      (y111 - y110)/3
  )

#--------------------------------------------------#
# 4. Decomposition of the income gradient
#--------------------------------------------------#

gradients <- shapley %>%
  pivot_longer(
    c(gdp, VA, Labour, Savings),
    names_to = "component",
    values_to = "gain"
  ) %>%
  group_by(scenario,kappa, component) %>%
  summarise(
    beta = unname(coef(lm(gain ~ log(gdppc)))[2]),
    .groups = "drop"
  ) %>%
  mutate(pp_tenfold = beta * log(10))

#print(gradients, n = Inf)


out = shapley %>%
  group_by(scenario,kappa) %>%
  summarise(
    b_VA      = coef(lm(VA ~ log(gdppc)))[2],
    b_Labour  = coef(lm(Labour ~ log(gdppc)))[2],
    b_Savings = coef(lm(Savings ~ log(gdppc)))[2],
    b_total   = coef(lm(I(gdp - y000) ~ log(gdppc)))[2],
    .groups = "drop"
  ) %>%
  mutate(b_struct = b_VA + b_Labour,
         share_struct = b_struct / b_total,
         share_sav    = b_Savings / b_total)


print(out)

#



