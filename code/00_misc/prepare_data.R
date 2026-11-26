# Data preparation version: 1.0.3
# Run from the project folder: source("code/prepare_data.R")
# Five outputs: Omega, omega, value_added, labour_compensation, country_industry.
# Data transformations use dplyr. readxl is used only to open Excel files.

library(dplyr)

data_year <- 2022
us_jobs_year <- 2022  # the OEWS workbook used below is the 2022 release

# 1. Read the source data --------------------------------------------------

tiva_raw <- read.csv(
  "data/tiva_database/OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV,1.1+VALU...W..A.csv",
  check.names = FALSE
)
tim_raw <- read.csv(
  "data/tim_database/OECD.STI.PIE,DSD_TIM_2025@DF_TIM_2025,1.0+EMPN+LABR+VALU_EMPN...W..A.csv",
  check.names = FALSE
)
hierarchy_raw <- readxl::read_excel("data/tiva_database/hierarchy.xlsx")
aisa_raw <- readxl::read_excel("data/aisa/work1207_data.xlsx", sheet = "data")
jobs_raw <- readxl::read_excel(
  "data/aisa/oesm22in4/oesm22in4/nat4d_M2022_dl.xlsx", sheet = "Nat4d_M2022_dl"
)
naics_isic_raw <- readxl::read_excel("data/tiva_database/naics-2022-v1.0-isic4-en.xlsx")
isic_tiva_raw <- readxl::read_excel("data/tiva_database/ISIC_to_TiVA_Mapping.xlsx")

# 2. Value added and industry weights -------------------------------------
# Keep the same non-overlapping industries as the existing hierarchy file.
# Dollar amounts in TiVA and TiM are in millions of USD.

industries <- hierarchy_raw %>%
  filter(include == 1, Industry_Code != "_T") %>%
  select(ACTIVITY = Industry_Code)

tiva_year <- tiva_raw %>%
  filter(TIME_PERIOD == data_year, MEASURE == "VALU", UNIT_MEASURE == "USD",
         COUNTERPART_AREA == "W", FREQ == "A", !is.na(OBS_VALUE))

country_totals <- tiva_year %>%
  filter(ACTIVITY == "_T") %>%
  select(REF_AREA, year = TIME_PERIOD, total_value_added = OBS_VALUE)

value_added <- tiva_year %>%
  inner_join(industries, by = "ACTIVITY") %>%
  select(REF_AREA, country = `Reference area`, ACTIVITY,
         industry = `Economic activity`, year = TIME_PERIOD,
         value_added = OBS_VALUE) %>%
  left_join(country_totals, by = c("REF_AREA", "year")) %>%
  mutate(industry_weight = value_added / total_value_added) %>%
  arrange(REF_AREA, ACTIVITY)

# 3. Labour compensation and labour share ---------------------------------
# Use the reported labour share: dividing rounded monetary values can differ.
# Labour share can exceed 1 in the data.
# Keep missing compensation as NA; missing data are not zero compensation.

compensation_amount <- tim_raw %>%
  filter(TIME_PERIOD == data_year, MEASURE == "LABR", UNIT_MEASURE == "USD",
         COUNTERPART_AREA == "W", FREQ == "A") %>%
  select(REF_AREA, ACTIVITY, year = TIME_PERIOD, compensation = OBS_VALUE)

compensation_share <- tim_raw %>%
  filter(TIME_PERIOD == data_year, MEASURE == "LABR", UNIT_MEASURE == "PT_VA",
         COUNTERPART_AREA == "W", FREQ == "A") %>%
  select(REF_AREA, ACTIVITY, year = TIME_PERIOD, labour_share_percent = OBS_VALUE) %>%
  mutate(labour_share = labour_share_percent / 100) %>%
  select(-labour_share_percent)

labour_compensation <- value_added %>%
  select(REF_AREA, country, ACTIVITY, industry, year) %>%
  left_join(compensation_amount, by = c("REF_AREA", "ACTIVITY", "year")) %>%
  left_join(compensation_share, by = c("REF_AREA", "ACTIVITY", "year")) %>%
  arrange(REF_AREA, ACTIVITY)

country_industry <- value_added %>%
  left_join(
    labour_compensation %>% select(REF_AREA, ACTIVITY, year, compensation, labour_share),
    by = c("REF_AREA", "ACTIVITY", "year")
  )

# 4. US occupations and exposure ------------------------------------------
# Exposure is currently observed for the US only. Do not copy it to countries
# here: assigning a shared US profile will be a later methodological choice.

jobs <- jobs_raw %>%
  filter(O_GROUP == "detailed") %>%
  mutate(OCC_CODE = trimws(OCC_CODE),
         employment = suppressWarnings(as.numeric(TOT_EMP)),
         annual_wage = suppressWarnings(as.numeric(A_MEAN)),
         naics = substr(as.character(NAICS), 1, 4))

occupation_employment <- jobs %>%
  filter(is.finite(employment), employment > 0) %>%
  group_by(OCC_CODE) %>%
  summarise(employment = sum(employment), .groups = "drop")

exposure_exact <- aisa_raw %>%
  select(OCC_CODE = OCCCode, kappa = `AI capability`, AISA) %>%
  mutate(OCC_CODE = trimws(OCC_CODE),
         broad = substr(OCC_CODE, 1, 6),
         minor = substr(OCC_CODE, 1, 4),
         major = substr(OCC_CODE, 1, 2)) %>%
  left_join(occupation_employment, by = "OCC_CODE")

# Missing occupations use the closest SOC group, weighted by employment.
# Write the three levels explicitly so the imputation is easy to inspect.

exposure_broad <- exposure_exact %>%
  group_by(broad, kappa) %>%
  summarise(AISA_broad = weighted.mean(AISA, employment, na.rm = TRUE),
            .groups = "drop")

exposure_minor <- exposure_exact %>%
  group_by(minor, kappa) %>%
  summarise(AISA_minor = weighted.mean(AISA, employment, na.rm = TRUE),
            .groups = "drop")

exposure_major <- exposure_exact %>%
  group_by(major, kappa) %>%
  summarise(AISA_major = weighted.mean(AISA, employment, na.rm = TRUE),
            .groups = "drop")

capability_levels <- exposure_exact %>% distinct(kappa)

# cross_join gives each occupation one row at each capability level.
occupation_exposure <- jobs %>%
  distinct(OCC_CODE) %>%
  mutate(broad = substr(OCC_CODE, 1, 6),
         minor = substr(OCC_CODE, 1, 4),
         major = substr(OCC_CODE, 1, 2)) %>%
  cross_join(capability_levels) %>%
  left_join(exposure_exact %>% select(OCC_CODE, kappa, AISA),
            by = c("OCC_CODE", "kappa")) %>%
  left_join(exposure_broad, by = c("broad", "kappa")) %>%
  left_join(exposure_minor, by = c("minor", "kappa")) %>%
  left_join(exposure_major, by = c("major", "kappa")) %>%
  mutate(exposure_source = case_when(
           !is.na(AISA) ~ "Exact occupation",
           !is.na(AISA_broad) ~ "SOC broad",
           !is.na(AISA_minor) ~ "SOC minor",
           !is.na(AISA_major) ~ "SOC major",
           TRUE ~ "Missing"
         ),
         AISA = coalesce(AISA, AISA_broad, AISA_minor, AISA_major)) %>%
  select(OCC_CODE, kappa, AISA, exposure_source)

# 5. Map US industries to TiVA --------------------------------------------
# Keep the existing concordance, fallback and manual mapping decisions.

concordance <- naics_isic_raw %>%
  select(donor_naics = `NAICS Canada 2022 Version 1.0 Code`,
         isic = `ISIC Rev. 4 Code`) %>%
  mutate(donor_naics = substr(as.character(donor_naics), 1, 4),
         isic = substr(sub("\\*$", "", isic), 1, 3)) %>%
  filter(!is.na(donor_naics), !is.na(isic)) %>%
  distinct(donor_naics, isic)

job_industries <- jobs %>% distinct(naics)

exact_links <- job_industries %>%
  inner_join(concordance, by = c("naics" = "donor_naics")) %>%
  mutate(donor_naics = naics)

unmatched_industries <- job_industries %>%
  anti_join(exact_links, by = "naics") %>%
  mutate(naics3 = substr(naics, 1, 3))

descendant_links <- unmatched_industries %>%
  inner_join(concordance %>% mutate(naics3 = substr(donor_naics, 1, 3)),
             by = "naics3", relationship = "many-to-many") %>%
  select(naics, donor_naics, isic)

automatic_links <- bind_rows(exact_links, descendant_links) %>%
  left_join(isic_tiva_raw, by = "isic") %>%
  select(naics, donor_naics, isic, ACTIVITY = tiva)

manual_links <- data.frame(
  naics = c("3250", "3270", "3320", "3330", "3370", "4840", "5170", "5182",
            "4230", "4231", "4234", "4238", "4240", "4243", "4245", "4251",
            "5251", "5259", "9991", "9992", "9993"),
  ACTIVITY = c("C20", "C23", "C25", "C28", "C31T33", "H49", "J61", "J62_63",
               "G", "G", "G", "G", "G", "G", "G", "G", "K", "K", "O", "O", "O")
)

industry_links <- automatic_links %>%
  anti_join(manual_links, by = "naics") %>%
  bind_rows(manual_links) %>%
  distinct(naics, donor_naics, isic, ACTIVITY)

industry_mapping <- industry_links %>%
  group_by(naics, ACTIVITY) %>%
  summarise(links = n(), .groups = "drop") %>%
  group_by(naics) %>%
  mutate(mapping_weight = links / sum(links)) %>%
  ungroup() %>%
  select(naics, ACTIVITY, mapping_weight)

us_jobs <- jobs %>%
  filter(is.finite(employment), employment > 0,
         is.finite(annual_wage), annual_wage > 0) %>%
  mutate(wage_bill = employment * annual_wage)

unmapped_industries <- us_jobs %>%
  distinct(naics) %>%
  anti_join(industry_mapping, by = "naics")

# Stop rather than silently discard occupations or industries.
stopifnot(nrow(unmapped_industries) == 0, !anyNA(industry_mapping$ACTIVITY))
stopifnot(!anyNA(occupation_exposure$AISA))

us_exposure <- us_jobs %>%
  left_join(occupation_exposure, by = "OCC_CODE", relationship = "many-to-many") %>%
  left_join(industry_mapping, by = "naics", relationship = "many-to-many") %>%
  mutate(weighted_wage_bill = wage_bill * mapping_weight,
         exposed_wage_bill = weighted_wage_bill * AISA / 100)

# 6. Omega: cumulative exposure. omega: marginal exposure ------------------
# Both use total industry wage costs as denominator; both are fractions.
# Each output has one row per country, industry and capability level.

Omega <- us_exposure %>%
  group_by(ACTIVITY, kappa) %>%
  summarise(wage_bill = sum(weighted_wage_bill),
            exposed_wage_bill = sum(exposed_wage_bill), .groups = "drop") %>%
  mutate(REF_AREA = "USA", year = us_jobs_year, Omega = exposed_wage_bill / wage_bill) %>%
  select(REF_AREA, ACTIVITY, year, kappa, Omega) %>%
  arrange(REF_AREA, ACTIVITY, kappa)

omega <- Omega %>%
  group_by(REF_AREA, ACTIVITY, year) %>%
  mutate(omega = Omega - lag(Omega, default = 0)) %>%
  ungroup() %>%
  select(REF_AREA, ACTIVITY, year, kappa, omega)

# The five ready datasets are now available as separate objects.
# Example: country_industry %>% filter(REF_AREA == "CZE")
# Example: Omega %>% filter(REF_AREA == "USA", kappa == 3)
