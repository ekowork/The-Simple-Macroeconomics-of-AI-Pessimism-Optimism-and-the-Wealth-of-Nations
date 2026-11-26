
rm(list=ls())


library(haven)
library(tidyverse)
library(readxl)
library(readr)
library(writexl)
library(dplyr)
library(plotly)
library(gganimate)
library(ggtext)
library(transformr)
library(gifski)
library(patchwork)
library(MetBrewer)
library(ggpattern)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

options(scipen = 999)
setwd(here())

# ----------------------------###.
source("code/figures/fig_setting.R")
source("code/figures/figures_generation.R")



#-----------------------------------------------------###.
#Data####
#-----------------------------------------------------###.
tiva_data <- readr::read_csv("./data/tiva_database/OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV,1.1+VALU...W..A.csv") %>% as.data.frame()
tim_data <- readr::read_csv("./data/tim_database/OECD.STI.PIE,DSD_TIM_2025@DF_TIM_2025,1.0+EMPN+LABR+VALU_EMPN...W..A.csv") %>% as.data.frame()
hierarchy = read_excel("./data/tiva_database/hierarchy.xlsx") %>% as.data.frame()
dat_aisa_raw = read_excel("./data/aisa/work1207_data.xlsx", sheet = "data") %>% as.data.frame()
dat_employ = read_excel("./data/aisa/oesm22in4/oesm22in4/nat4d_M2022_dl.xlsx", sheet = "Nat4d_M2022_dl") %>% as.data.frame() %>%
  select(NAICS, NAICS_TITLE, OCC_CODE, OCC_TITLE, O_GROUP, TOT_EMP, A_MEAN) %>% rename(WAGE_AN_MEAN = A_MEAN)
dat_employ$OCC_CODE <- trimws(dat_employ$OCC_CODE)
dat_aisa_raw$OCCCode <- trimws(dat_aisa_raw$OCCCode)
crosswalk_raw = read_excel("./data/tiva_database/naics-2022-v1.0-isic4-en.xlsx") %>% as.data.frame()
crosswalk_isic_tiva = read_excel("./data/tiva_database/ISIC_to_TiVA_Mapping.xlsx") %>% as.data.frame()
geograph =  readr::read_csv("./data/continents/continents-according-to-our-world-in-data.csv") %>% as.data.frame() %>% rename(regions = "World regions according to OWID")
# internet access
world_internet_raw = read_excel("./data/other/API_IT.NET.USER.ZS_DS2_en_excel_v2_19993.xls", skip = 3) %>% 
  as.data.frame() 
# MISCH et al
mischetal = read_excel("./data/prod_gains_europe/plot_mischetal2025.xlsx") %>% 
  as.data.frame() 
# eloundou S exposure for US
acem_exposure_raw <- read_csv("./data/Simple Macroeconomics of AI (2024)/task_taxonomy/training_data/eloundou_task.csv") %>%
  select(`Task ID`, onet_soc = `O*NET-SOC Code`, gpt4_automation)

# data for adjusting S
occupations_global = read.csv("./data/other/occupation_contributions.csv")
occupations_global_worldbank = read.csv("./data/other/emp_occ1.csv")
crosswalk_isco_soc = read_excel("./data/other/isco_soc_crosswalk.xls")
# ppp gdp
world_gdppc <- read_excel("./data/other/API_NY.GDP.PCAP.PP.CD_DS2_en_excel_v2_273625.xls", skip = 3)
#gdp
world_gdp = read_excel("./data/other/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_19310.xls", skip = 3) %>% 
  as.data.frame() %>% select(1:2, "2023") %>% rename(gdp_2023 = "2023") 
# employ numbers in firms
bento <- read_dta("./data/DataAndCode_BR_AES/DataAndCode_BR_AES/Data_BRAES_Final.dta")
bento_ind <- read_dta("./data/DataAndCode_BR_AES/DataAndCode_BR_AES/Data_BRAES_indFinal.dta")
# population projection
world_pop_raw = read_excel(
  "./data/other/P_Data_Extract_From_Population_estimates_and_projections.xlsx"
)

#-----------------------------------------------------###.
# Prepare Value added dataset ####
#-----------------------------------------------------###.

source("code/dat_cleaning/value_added.R")
value_added = tiva_shares

#-----------------------------------------------------###.
# Prepare Labour Share dataset ####
#-----------------------------------------------------###.
labour_share = tim_data %>% 
  filter(Measure=="Compensation of employees", 
         `Unit of measure`=="Percentage of value added", 
         TIME_PERIOD==2022) %>% 
  select(REF_AREA, ACTIVITY, OBS_VALUE) %>% 
  rename(Lshare =OBS_VALUE ) |> 
  mutate(Lshare = Lshare)


#------------------------------------------------------###.
# Prepare internet penetration dataset####
#------------------------------------------------------###.
world_internet =  world_internet_raw %>%
  pivot_longer(cols = `1960`:`2024`, names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(year)) %>%
  arrange(`Country Name`, desc(year)) %>%
  group_by(`Country Name`, `Country Code`) %>%
  summarise(latest_value = coalesce(
    value[year == 2023],
    value[!is.na(value)][1]
  ), .groups = "drop")  

world_internet = world_internet |> 
  mutate(usa = world_internet %>% 
           filter(`Country Code`== "USA") %>% 
           pull(latest_value)) %>% 
  mutate(internet_use = latest_value/usa )


#--------------------------------------------------------------###.
# Figure: US Labour Share by Industry ####
figure_labour_share_us(save=T)
#--------------------------------------------------------------###.
# Figure: US Value Added by Industry ####
figure_value_added_us(save=T)
#--------------------------------------------------------------###.


#-------------------------------------------------#.
# Prepare S_{US} & S_c ####
#-------------------------------------------------#.

source("./code/dat_cleaning/prepare_S_US_fromAISA_and_Eloundou.R")

s_us = agg_us_tiva |> 
  select(-acem_exp_weighted) |> 
  mutate(REF_AREA="USA")

# this adjusts occupational structures based on individual countries, 
# "large omega" stays US based
source("./code/dat_cleaning/S_global.R")



#-------------------------------------------------#.
# Figure: Compar Eloundu vs AISA ####
#-------------------------------------------------#.
figure_p_eloundou_compar(save=T)


#-------------------------------------------------#.
# Prepare m0_c as an adjustment ####
#-------------------------------------------------#.

source("./code/dat_cleaning/country_mu0.R")


#------------------------------------#.
# Figure: two occupations AISA####
#---------------------------------###.

figure_p_occupations_aisa(save=T)





#-------------------------------------------------#.
# Define the iota and lambda calculation 
#-------------------------------------------------#.

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
        (1 - b ^ 2) * exp(-m) +
        b * (1 - b) / 2 *
        exp(-(1 + b) / b * m)
    )
  )
}

# GENERATE THEORETICAL MODEL
source("./code/figures/figures_for_the_theoretical_model.R")



# Country-specific exposure data.
# Currently available only for the US.
# Other countries can be added later.
# s_country <- s_us %>%
#   mutate(REF_AREA = "USA")

kappa_levels <- sort(unique(s_us$`AI capability`))


run_model <- function(country, scenario, b, mu0, theta,
                      s_mode, l_mode, iota_fixed, lambda_fixed) {
  
  #------------------------------------------------#,
  # 1. Select country-specific or US exposure
  #------------------------------------------------#,
  
  s_data <- if (s_mode == "country") {
    s_country %>% filter(REF_AREA == country)
  } else if (s_mode == "internet") {
    s_country_internet %>% filter(REF_AREA == country)
  } else {
    s_us
  }
  
  if(s_mode == "country" & nrow(s_data)==0){
    s_data <-s_country %>% filter(REF_AREA == "USA")
  }
  
  if(s_mode == "internet" & nrow(s_data)==0){
    s_data <-s_country_internet %>% filter(REF_AREA == "USA")
  }
  
  if (nrow(s_data) == 0) {
    stop("Missing exposure data for ", country)
  }
  
  #------------------------------------------------#.
  # 2. Construct marginal exposure cohorts
  #------------------------------------------------#.
  
  cohorts <- s_data %>%
    transmute(
      tiva,
      difficulty = `AI capability`,
      AISA = AISA_bill_weighted / 100,
      wage_bill
    ) %>%
    arrange(tiva, difficulty) %>%
    group_by(tiva) %>%
    mutate(
      AISA_marginal = AISA - lag(AISA, default = 0),
      cohort_wage_bill = wage_bill * pmax(AISA_marginal, 0)
    ) %>%
    ungroup()
  
  if (any(cohorts$AISA_marginal < -1e-8, na.rm = TRUE)) {
    stop("Non-monotonic exposure curve for ", country)
  }
  
  #------------------------------------------------#.
  # 3. Compute all kappa levels simultaneously
  #------------------------------------------------#.
  
  tfp_work <- tidyr::crossing(
      cohorts,
    kappa = kappa_levels
  ) %>%
    mutate(
      z = kappa - difficulty,
      
      iota_task = iota_fn(
        z, theta = theta, mu0 = mu0, b = b
      ),
      
      pi_task = pi_fn(
        z, theta = theta, mu0 = mu0, b = b
      ),
      
      exposed_wage = if_else(
        z >= 0, cohort_wage_bill, 0
      ),
      
      implemented_wage = cohort_wage_bill * iota_task,
      saving_wage = cohort_wage_bill * pi_task
    ) %>%
    
    group_by(tiva, kappa) %>%
    summarise(
      exposed_wage = sum(exposed_wage),
      implemented_wage = sum(implemented_wage),
      saving_wage = sum(saving_wage),
      total_wage_bill = first(wage_bill),
      .groups = "drop"
    ) %>%
    
    mutate(
      S = exposed_wage / total_wage_bill,
      
      iota = if_else(
        exposed_wage > 0,
        implemented_wage / exposed_wage, 0
      ),
      
      lambda = if_else(
        implemented_wage > 0,
        saving_wage / implemented_wage, 0
      ),
      
      # Fixed parameters take precedence when supplied
      iota = if_else(S > 0, coalesce(iota_fixed, iota), 0),
      lambda = if_else(S > 0, coalesce(lambda_fixed, lambda), 0),
      
      saving_share = S * iota * lambda
    )
  
  #------------------------------------------------#.
  # 4. Country-specific value added and labour share
  #------------------------------------------------#.
  
  va_c <- value_added %>%
    filter(REF_AREA == country, ACTIVITY != "_T") %>%
    transmute(
      tiva = ACTIVITY,
      VALUE_ADDED
    )
  
  labour_country <- if (l_mode == "US") "USA" else country
  
  l_c <- labour_share %>%
    filter(REF_AREA == labour_country) %>%
    select(ACTIVITY, Lshare)
  
  #------------------------------------------------#.
  # 5. Industry-level TFP
  #------------------------------------------------#.
  
  tfp_industry <- tidyr::crossing(
    va_c,
    kappa = kappa_levels
  ) %>%
    left_join(tfp_work, by = c("tiva", "kappa")) %>%
    left_join(l_c, by = c("tiva" = "ACTIVITY")) %>%
    mutate(
      across(
        c(S, iota, lambda, saving_share),
        ~ replace_na(.x, 0)
      ),
      
      Lshare = Lshare / 100,
      tfp = saving_share * Lshare,
      
      country = country,
      scenario = scenario
    )
  
  #------------------------------------------------#.
  # 6. Aggregate TFP and GDP
  #------------------------------------------------#.
  
  tfp_gdp <- tfp_industry %>%
    filter(!is.na(Lshare), !is.na(VALUE_ADDED)) %>%
    group_by(country, scenario, kappa) %>%
    summarise(
      labour_bill = sum(VALUE_ADDED * Lshare, na.rm = TRUE),
      exposed = sum(VALUE_ADDED * Lshare * S, na.rm = TRUE),
      implemented = sum(VALUE_ADDED * Lshare * S * iota, na.rm = TRUE),
      savings = sum(VALUE_ADDED * Lshare * S * iota * lambda, na.rm = TRUE),
      
      tfp = weighted.mean(tfp, w = VALUE_ADDED, na.rm = TRUE),
      Lshare = weighted.mean(Lshare, w = VALUE_ADDED, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      S = if_else(labour_bill > 0, exposed / labour_bill, 0),
      iota = if_else(exposed > 0, implemented / exposed, 0),
      lambda = if_else(implemented > 0, savings / implemented, 0),
      gdp = 100 * tfp / Lshare
    ) %>%
    select(-c(labour_bill, exposed, implemented, savings))
  
  return(list(
    industry = tfp_industry,
    gdp = tfp_gdp
  ))
}





#-------------------------------------------------#
# calibrate theta and mu0 for alternative b
#-------------------------------------------------#

calibrate_params <- function(b, iota_target, lambda_target, kappa_target = 2.6) {
  
  moments <- function(p) {
    run_model(
      country = "USA",
      scenario = "calibration",
      b = b,
      theta = exp(p[1]),
      mu0 = p[2],
      s_mode = "US",
      l_mode = "country",
      iota_fixed = NA_real_,
      lambda_fixed = NA_real_
    )$gdp |>
      filter(abs(kappa - kappa_target) < 1e-8) |>
      select(iota, lambda) |>
      unlist(use.names = FALSE)
  }
  
  objective <- function(p) {
    sum((moments(p) - c(iota_target, lambda_target))^2)
  }
  
  fit <- optim(
    par = c(0, 0),
    fn = objective
  )
  
  tibble(
    b = b,
    theta = exp(fit$par[1]),
    mu0 = fit$par[2],
    iota = moments(fit$par)[1],
    lambda = moments(fit$par)[2],
    error = fit$value
  )
}


purrr::map_dfr(
  c(0.6, 0.7, 0.75),
  ~ calibrate_params(
    b = .x,
    iota_target = 0.23,
    lambda_target = 0.27,
    kappa_target = 2.6
  )
)

calib_3.6  = calibrate_params(
  b = 0.7,
  iota_target = 0.23,
  lambda_target = 0.27,
  kappa_target = 3.6
)
print(calib_3.6)



calib_optim  = calibrate_params(
  b = 0.7,
  iota_target = 0.49,
  lambda_target = 0.27,
  kappa_target = 2.6
)
print(calib_optim)




# scenarios

# lets make it possible to do multiple scenarios
# scenarios ####
scenarios <- tribble(
  ~scenario,                          ~b,  ~mu0,  ~theta, ~s_mode, ~l_mode,  ~iota_fixed, ~lambda_fixed,
  "baseline",                         .7, -1.75,   .481,   "US",      "country", NA_real_,   NA_real_,
  "baseline_b6",                      .6, -0.774, 0.562,   "US",      "country", NA_real_,   NA_real_,
  "baseline_b75",                    .75, -2.59,   0.45,   "US",      "country", NA_real_,   NA_real_,
  "baseline_kappa3.6",                .7, -1.64,   .435,   "US",      "country", NA_real_,   NA_real_,    
  "baseline, upd_param",              .7, 0.0189, 0.481,   "US",      "country", NA_real_,   NA_real_, # svanberg .49
  "S_us, fix_lambda_iota",            .7, -1.75,   .481,   "US",      "country", .23,        .27,
  "S_us, fix_lambda_iota_upd",        .7, -1.75,   .481,   "US",      "country", 0.49,        .27,     # bick lambda.33
  "S_us, mu0_c",                      .7, NA_real_,.481,   "US",      "country", NA_real_,   NA_real_, 
  "S_us, mu0_c, upd_param",           .7, NA_real_,.481,   "US",      "country", NA_real_,   NA_real_, 
  "S_c, mu0_c",                       .7, NA_real_,.481,   "country", "country", NA_real_,   NA_real_, 
  "S_c, mu0_c, internet",             .7, NA_real_,.481,   "internet","country", NA_real_,   NA_real_, 
)

country_overrides <- tibble(
  REF_AREA      = c("xx", "xzz"),
  mu0_override  = c(-1.5, NA_real_),
  b_override    = c(NA_real_, .65),
  theta_override = c(NA_real_, NA_real_)
)


countries <- intersect(
  unique(value_added$REF_AREA),
  unique(labour_share$REF_AREA)
) 
# no country groups pls
countries <- countries[
  countries %in% geograph$Code[!is.na(geograph$regions)]
]

# Retain countries with available GDP data
countries <- countries[
  countries %in% world_gdp$`Country Code`[
    !is.na(world_gdp$gdp_2023) &
      world_gdp$gdp_2023 > 0
  ]
]



model_grid <- crossing(
  country = countries,
  scenarios
) %>%
  left_join(
    bento_final %>%
      transmute(
        country = countrycode,
        mu0_country = mu0_country, 
        mu0_country_upd
      ),
    by = "country"
  ) %>%
  
  
  left_join(
    country_overrides,
    by = c("country" = "REF_AREA")
  ) %>%
  
  mutate(
    b = coalesce(b_override, b),
    mu0 = coalesce(mu0_override, mu0, if_else(scenario == "S_us, mu0_c, upd_param",
    mu0_country_upd,mu0_country)),
    theta = coalesce(theta_override, theta)
  ) %>%
  
  select(
    country, scenario,
    b, mu0, theta,
    s_mode, l_mode,
    iota_fixed, lambda_fixed
  )


# results ####
results <- purrr::pmap(
  model_grid,
  run_model
)
# All country-level GDP results
res_agg <- purrr::map_dfr(results, "gdp")

# All country-industry results
res_i <- purrr::map_dfr(results, "industry")




# WHATS THE SHARE OF HOUSEHOLD INDUSTRY 
# --- AS A RESULT WE OMIT HOUSEHOLD CAUSE INSIGNIFICANT 
# --- deals with WHAT S TO ASSIGN AND THE EMPLOY SHARE
res_i %>%
  filter(kappa == 2.6, scenario == "S_us, mu0_c") %>%
  tidyr::crossing(region = c("USA", "World")) %>%
  filter(region == "World" | country == "USA") %>%
  group_by(region) %>%
  summarise(
    T = sum(VALUE_ADDED[tiva == "T"], na.rm = TRUE) / sum(VALUE_ADDED, na.rm = TRUE),
    U = sum(VALUE_ADDED[tiva == "U"], na.rm = TRUE) / sum(VALUE_ADDED, na.rm = TRUE),
    .groups = "drop"
  )


#---------------------###.
# Figure: US TFP####
#---------------------###.

figure_p_tfp_growth_us(save=T)

#---------------------###.
# Figure: US GDP####
#---------------------###.

figure_p_gdp_growth_us(save=T)

res_agg |> 
  filter(
    country == "USA",
    str_detect(scenario, "baseline") | str_detect(scenario, "S_us, fix_lambda_iota")
    ) |>
  select( scenario,kappa,gdp) |> 
  pivot_wider(names_from = scenario, values_from = gdp) |> 
  mutate(b_upd_c =  `baseline, upd_param`/baseline, 
         b_6_c= baseline_b6 / baseline, 
         b_75_c = baseline_b75/baseline, 
         kappa3_c = baseline_kappa3.6 /baseline) |> 
  print(n=50)

# whats the S, iota, lambda for the uUS
res_agg |> 
  filter(
    country == "USA",
    scenario == "baseline",
    kappa %in% c(2.6,6)
  ) |>  print(n=50)

# whats the gdp when margins fixed
res_agg |> 
  filter(
    country == "USA",
    scenario == "S_us, fix_lambda_iota",
    kappa %in% c(2.6,6)
  ) |>  print(n=50)

res_agg |> 
  filter(
    country == "USA",
    scenario == "baseline, upd_param",
    kappa %in% c(2.6,6)
  ) |>  print(n=50)
  

#-------------------------###.
# Figure: Param Decomp US ####
#-------------------------###.

figure_p_param_decomp(save=T)
  
#-------------------------###.
# Figure: Global GDP impact####
#-------------------------###.

figure_p_gdp_growth_world(save = TRUE)

# mu_0,c  affects iota only. 
res_agg %>%
  filter(scenario %in%  c(
    "baseline",
    "S_us, mu0_c"), 
         kappa == 2.6) %>%
  select(country,scenario, iota) |> 
  pivot_wider(names_from = scenario, values_from =iota ) |> 
  View()

res_agg %>%
  filter(scenario %in% 
           c(
             "baseline",
             "S_us, fix_lambda_iota",
             "baseline, upd_param", 
             "S_us, mu0_c",
             "S_us, mu0_c, upd_param"
           )) %>%
  filter(kappa %in% c(2.6,6)) |> 
  left_join(
    world_gdp %>% select(`Country Code`, gdp_2023),
    by = c("country" = "Country Code")
  ) %>%
  filter(!is.na(gdp), !is.na(gdp_2023), gdp_2023 > 0) %>%
  group_by(kappa, scenario) %>%
  summarise(
    gdp = weighted.mean(gdp, w = gdp_2023),
    .groups = "drop"
  ) |> pivot_wider(names_from = kappa, values_from = gdp )


#-------------------------###.
# Figure: Global GDP impact WITH alternative mu0, S and internet ####
#-------------------------###.

gdp_growth_world_adj(save = TRUE)


#-------------------------###.
# Figure: Global GDP impact WITH alternative mu0, S and internet ####
#-------------------------###.

figure_p_gdp_growth_world_all(save = TRUE)

res_agg %>%
  filter(scenario %in%  c(
    "baseline",
    "S_us, mu0_c", 
    "S_c, mu0_c",
    "S_c, mu0_c, internet"), 
    kappa == 2.6) %>%
  select(country,scenario, S) |> 
  pivot_wider(names_from = scenario, values_from =S ) |> 
  View()


#---------------------------------------###.
## ...how much world's gdp do we consider?####
#---------------------------------------###.
ourgdp = res_agg %>% 
  filter(scenario=="baseline") |> 
  filter(kappa==2.6) |> 
  right_join(world_gdp, by = join_by(country == `Country Code`)) %>% 
  filter(is.na(gdp)==FALSE) %>% 
  summarise(gdp_2023 = sum(gdp_2023)) %>% pull()
world_gdp_out = world_gdp %>% filter(`Country Name`=="World") %>% pull(gdp_2023)

cat("our dataset represents", ourgdp*100/world_gdp_out,
    "% of world's gdp according to data from world bank")


#-----------------------------------------------------###.
# Figure: GDP Impact per GDP PPP - its growing :)  ####
#-----------------------------------------------------###.
figure_p_gdp_growth_gdpppp(save=T)

#gdp gains in selected countries
res_agg %>%
  filter(kappa %in% c(2.6, 6),scenario == "S_us, mu0_c") %>%
  left_join(geograph, by = join_by(country == Code)) %>%
  left_join(world_gdp,
            by = join_by("country" == "Country Code")) |> 
  left_join(world_gdppc |> select(-`Country Name`), 
            by = join_by("country" == "Country Code")) %>%
  filter(!is.na(gdppc_ppp), !is.na(gdp)) |>
  mutate(
    capability_label = case_when(
      kappa == 2.6 ~ "(a) Current AI Frontier (κ = 2.6)",
      kappa == 6   ~ "(b) The Top of the Capability Scale (κ = 6)" #\u03ba
    ),
    gdppc_ppp_log = log(gdppc_ppp),
    gdp_trn = gdp_2023 / 1e12
  ) |> 
  group_by(kappa, regions) |> 
  summarise(gdp = weighted.mean(gdp, w = gdp_2023))

res_agg %>%
  filter(kappa %in% c(2.6, 6),scenario == "S_us, mu0_c",) %>%
  left_join(geograph, by = join_by(country == Code)) %>%
  left_join(world_gdp,
            by = join_by("country" == "Country Code")) |> 
  left_join(world_gdppc, 
            by = join_by("country" == "Country Code")) %>%
  filter(!is.na(gdppc_ppp), !is.na(gdp)) |> 
  mutate(
    capability_label = case_when(
      kappa == 2.6 ~ "(a) Current AI Frontier (κ = 2.6)",
      kappa == 6   ~ "(b) The Top of the Capability Scale (κ = 6)" #\u03ba
    ),
    gdppc_ppp_log = log(gdppc_ppp),
    gdp_trn = gdp_2023 / 1e12
  ) |> 
  filter(country %in%  
           c("USA", "CHN", "IND", "DEU", 
             "JPN", "BRA", "EGY")) |> 
  select(Entity, gdp, kappa) |> 
  pivot_wider(names_from=(kappa), values_from=gdp)


# what is the connection between the wealth of the countries 
# and gdp gain in the scenarios
res_agg |> 
  filter(
    kappa %in% c(2.6,6)) |> 
  left_join(world_gdppc, by = c("country" = "Country Code")) %>%
  filter(!is.na(gdppc_ppp), !is.na(gdp)) %>%
  left_join(world_gdp, 
            by = join_by("country"== "Country Code")) |> 
  filter(is.na(gdp_2023)==F) |> 
  mutate(
    gdppc_ppp_log = log(gdppc_ppp),
    gdp_trn = gdp_2023 / 1e12
  ) |> 
  group_by(scenario, kappa) |> 
  summarise(beta=coef (lm(gdp~log(gdppc_ppp)))[2]) |> 
  pivot_wider(names_from = kappa, values_from = beta)

#


#-----------------------------------------------------###.
# Figure: Industry Contributions across PPP :)  ####
#-----------------------------------------------------###.

figure_p_industry_ppp_decomposition1(save=T)
figure_p_industry_ppp_decomposition(save=T)

# shapely decomposition to beta gradient
source("./code/figures/ai_sectoral_decomposition.R")

#

#-----------------------------------------------------###.
# Figure: Industry decomposition :)  ####
#-----------------------------------------------------###.

figure_p_industry_decomposition(save = T)

#whats the global averages of my margins
global_drivers <- res_i %>%
  filter(
    kappa %in% c(2.6, 6),
    scenario == "S_us, mu0_c",
    VALUE_ADDED > 0
  ) %>%
  group_by(kappa) |> 
  summarise(
    S = weighted.mean(S, VALUE_ADDED, na.rm = TRUE) * 100,
    iota = weighted.mean(iota, VALUE_ADDED, na.rm = TRUE) * 100,
    lambda = weighted.mean(lambda, VALUE_ADDED, na.rm = TRUE) * 100,
    L = weighted.mean(Lshare, VALUE_ADDED, na.rm = TRUE) * 100,
    VA = sum(VALUE_ADDED, na.rm = TRUE),
    TFP = weighted.mean(tfp, VALUE_ADDED, na.rm = TRUE) * 100
  ) |> 
  mutate(gdp_approx_pct = 100 * (S/100) * (iota/100) * (lambda/100))

global_full <- res_i %>%
  filter(
    kappa %in% c(2.6, 6),
    scenario == "S_us, mu0_c",
    !is.na(tfp),
    VALUE_ADDED > 0
  ) %>%
  group_by(country, scenario, kappa) %>%
  summarise(country_va = sum(VALUE_ADDED), .groups = "drop") %>%
  inner_join(res_agg, by = c("country", "scenario", "kappa")) %>%
  filter(!is.na(gdp)) %>%
  group_by(kappa) %>%
  summarise(
    gdp_full_va_pct = weighted.mean(gdp, country_va),
    .groups = "drop"
  )

# 
global_drivers <- global_drivers %>%
  left_join(global_full, by = "kappa")

print(global_drivers)


#-----------------------------------------------------###.
# Figure: Country-Industry contribution :)  ####
#-----------------------------------------------------###.

figure_p_country_industry_contributions(save =T)


#-----------------------------------------------------###.
# Figure: Margins industry DECOMP - ALL COUNTRIES :)  ####
#-----------------------------------------------------###.

figure_p_param_decomp_all(save=T)
#


#-----------------------------------------------------###.
# Figure: Regional Gains :)  ####
#-----------------------------------------------------###.

figure_p_regional_gdp_gains(save=T)
#

#-----------------------------------------------------###.
# Figure: MAPS :)  ####
#-----------------------------------------------------###.

figure_p_map(save = T)
#



#----------------------------------------------###.
# Table: Individual country ####
#----------------------------------------------###.


tab_country = res_agg %>%
  filter(kappa%in% c(2.6,6), 
         scenario == "S_us, mu0_c") %>% 
  left_join(
    geograph %>% transmute(Code,Country = Entity,Region= regions),
    by = join_by(country == Code)
  ) |> 
  transmute(country, Country,Region, kappa, gdp) |> 
  pivot_wider(names_from = kappa,values_from = gdp ) %>% 
  rename("GDP Gain (kappa = 2.6)" = "2.6", 
         "GDP Gain (kappa = 6)" = "6") %>% 
  arrange(country)


library(xtable)

xt <- xtable(tab_country,
             digits = 2,
             caption = "Country GDP Gain")

print(xt,
      include.rownames = FALSE,
      booktabs = TRUE,
      sanitize.text.function = identity)
#


#-----------------------------------------------------###.
# Figure: Demography :)  ####
#-----------------------------------------------------###.

suppressWarnings(figure_p_demografic_S_us_mu0c(save=T))

#


#-----------------------------------------------------###.
# Figure: Cohort Distribution across the Industries ####
#-----------------------------------------------------###.

p_figure_cohort_dist(save=T)


#



#-----------------------------------------------------###.
# Figure: animation ####
#-----------------------------------------------------###.


fig_animate(save=T)





# END
