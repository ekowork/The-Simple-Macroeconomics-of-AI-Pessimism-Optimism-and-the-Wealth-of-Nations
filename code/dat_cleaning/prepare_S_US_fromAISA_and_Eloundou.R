
#-----------------------------------------------------#.
### 1. AISA and Eloundou exposure ####
#-----------------------------------------------------#.

acem_exposure <- acem_exposure_raw %>%
  mutate(
    soc6 = substr(onet_soc, 1, 7),
    automation = ifelse(gpt4_automation > 0.5, gpt4_automation, 0)
  ) %>%
  group_by(soc6) %>%
  summarise(
    gpt4_exposure = mean(automation, na.rm = TRUE),
    .groups = "drop"
  )

dat_aisa = dat_aisa_raw %>%
  left_join(acem_exposure %>% 
              transmute(soc6, acem_exp = gpt4_exposure), 
            by = join_by(OCCCode ==soc6 ))



#-----------------------------------------------------#.
### 2. adding US employment data ####
#-----------------------------------------------------#.

# first we calculate the number of employees in occupations 
national_soc_employment_2022 =  dat_employ |>
  filter(O_GROUP == "detailed") |>
  transmute(
    OCCCode = OCC_CODE,
    employment_2022 = suppressWarnings(as.numeric(TOT_EMP))
  ) |>
  filter(is.finite(employment_2022), employment_2022 > 0) |>
  group_by(OCCCode) |>
  summarise(
    employment_2022 = sum(employment_2022),
    .groups = "drop"
  )

exposure_columns <- c(
  "AISA",
  "acem_exp"
)

# here we say what is the individual position employment in the US
aisa_exposure_exact <- dat_aisa %>%
  select(
    OCCCode,
    `AI capability`,
    all_of(exposure_columns)
  ) %>%
  left_join(
    national_soc_employment_2022,
    by = "OCCCode",
    relationship = "many-to-one"
  )


#-------------------------------#.
#HERE IN THIS PART WE UPDATE THE EMPLOYMENT
#SOME OCCUPATIONS ARE missing IN THE AISA DATASET
#WE IMPUTE IT - the result is dat_us
# which has aisa and employment for each occupation o within industry i 


# positions for which we miss aisa 
unmatched_bls_soc <- setdiff(dat_employ %>%
                               filter(O_GROUP == "detailed") %>%
                               distinct(OCC_CODE) %>%
                               pull(OCC_CODE), 
                             unique(aisa_exposure_exact$OCCCode))

# we impute soc exposure for positions unavailable in aisa
# we do it as weighted average of the similar occupations at the 
# same level of detail - if none found - continue to more aggregate
impute_soc_exposure <- function(target_code, donor_data) {
  # we look trough 3 levels of detail in  occupation
  hierarchy_levels <- tibble::tribble(
    ~source,                  ~prefix_length,
    "SOC broad occupation",              6L,
    "SOC minor group",                   4L,
    "SOC major group",                   2L
  )
  
  for (level_index in seq_len(nrow(hierarchy_levels))) {
    prefix_length <- hierarchy_levels$prefix_length[[level_index]]
    donors <- donor_data %>%
      filter(
        substr(OCCCode, 1, prefix_length) ==
          substr(target_code, 1, prefix_length)
      )
    
    if (nrow(donors) > 0) {
      return(
        donors %>%
          group_by(`AI capability`) %>%
          summarise(
            across(
              all_of(exposure_columns),
              ~ weighted.mean(.x, w = employment_2022, na.rm = TRUE)
            ),
            occupation_exposure_donors = n_distinct(OCCCode),
            .groups = "drop"
          ) %>%
          mutate(
            OCCCode = target_code,
            occupation_exposure_source = hierarchy_levels$source[[level_index]]
          )
      )
    }
  }
  
  stop(
    sprintf("No AISA donor occupation found for BLS SOC code %s.", target_code),
    call. = FALSE
  )
}

aisa_exposure_imputed <- purrr::map_dfr(
  unmatched_bls_soc,
  impute_soc_exposure,
  donor_data = aisa_exposure_exact
)

aisa_exposure <- bind_rows(
  aisa_exposure_exact %>%
    transmute(
      OCCCode,
      `AI capability`,
      across(all_of(exposure_columns)),
      occupation_exposure_donors = 1L,
      occupation_exposure_source = "Exact O*NET-SOC match"
    ),
  aisa_exposure_imputed %>%
    select(
      OCCCode,
      `AI capability`,
      all_of(exposure_columns),
      occupation_exposure_donors,
      occupation_exposure_source
    )
)

# we also create the marginal AISA 
# to be able to sum across the dataset
aisa_exposure <- aisa_exposure %>%
  arrange(OCCCode, `AI capability`) %>%
  group_by(OCCCode) %>%
  mutate(
    AISA_marginal = AISA - lag(AISA, default = 0),
    AISA_marginal = pmax(AISA_marginal, 0)
  ) %>%
  ungroup()

# now we add the employment numbers and have the 
# us wide economy employment with AISA

dat_us <- dat_employ %>%
  filter(
    O_GROUP == "detailed",
    !is.na(TOT_EMP),
    !TOT_EMP %in% c("**", "*", "-", "N/A", ""),
    !is.na(WAGE_AN_MEAN)
  ) %>%
  mutate(
    TOT_EMP = suppressWarnings(as.numeric(TOT_EMP)),
    WAGE_AN_MEAN = suppressWarnings(as.numeric(WAGE_AN_MEAN)),
    naics = substr(as.character(NAICS), 1, 4),
    wage_bill = WAGE_AN_MEAN * TOT_EMP
  ) %>%
  filter(
    is.finite(TOT_EMP),
    is.finite(WAGE_AN_MEAN),
    TOT_EMP > 0,
    wage_bill > 0
  ) %>%
  left_join(
    aisa_exposure,
    by = join_by(OCC_CODE == OCCCode),
    relationship = "many-to-many"
  )

# add wage-bill
dat_us = dat_us %>%
  mutate(
    aisa_wage_bill = wage_bill * AISA,
    acem_exp_wage_bill = acem_exp*100*wage_bill
  )

# check the amount of imputed data
dat_us |> 
  group_by(occupation_exposure_source) |>
  summarise(wage_bill = sum(wage_bill)) |>
  mutate(share = wage_bill / sum(wage_bill))


#-----------------------------------------------------#.
### 3. We transform US S data to  ####
# international industry classification (NAICS to TiVA)
#-----------------------------------------------------#,

# use Canadian crosswalk and clean it
crosswalk <- crosswalk_raw %>%
  transmute(
    donor_naics = substr(
      as.character(`NAICS Canada 2022 Version 1.0 Code`),
      1,
      4
    ),
    isic = substr(stringr::str_remove(`ISIC Rev. 4 Code`, "\\*$"), 1, 3)
  ) %>%
  filter(!is.na(donor_naics), !is.na(isic)) %>%
  distinct(donor_naics, isic)

#fn for finding a mapping for one NAICS code
#it identifies what naics can be found in the crosswalk (4dig or at least 3dig) 
# and gives it the tiva code based on peer from upper aggregation
build_naics_links <- function(target_naics, concordance) {
  exact_links <- concordance %>%
    filter(donor_naics == target_naics)
  
  if (nrow(exact_links) > 0) {
    return(
      exact_links %>%
        mutate(
          naics = target_naics,
          naics_mapping_source = "Exact four-digit concordance"
        )
    )
  }
  descendant_links <- concordance %>%
    filter(substr(donor_naics, 1, 3) == substr(target_naics, 1, 3))
  
  if (nrow(descendant_links) > 0) {
    return(
      descendant_links %>%
        mutate(
          naics = target_naics,
          naics_mapping_source = "Three-digit descendant concordance"
        )
    )
  }
  tibble(
    donor_naics = character(),
    isic = character(),
    naics = character(),
    naics_mapping_source = character()
  )
}

# find naics in us data
crosswalk_automatic <- purrr::map_dfr(
  sort(unique(dat_us$naics)),
  build_naics_links,
  concordance = crosswalk
) %>%
  left_join(crosswalk_isic_tiva, by = "isic")

# OEWS publishes several industry aggregates rather than their constituent
# four-digit NAICS industries. Map the aggregates that correspond cleanly to a
# single included TiVA leaf directly; this avoids using excluded C/H/J parents
# or pulling in secondary links from descendants outside the published OEWS
# aggregate. US/Canada classification differences and the three OEWS government
# designations are also handled explicitly. Every override targets an included
# TiVA industry.
crosswalk_manual <- tibble::tribble(
  ~donor_naics, ~isic, ~naics, ~naics_mapping_source,                    ~tiva,
  NA, NA, "3250", "OEWS aggregate-to-leaf override", "C20",
  NA, NA, "3270", "OEWS aggregate-to-leaf override", "C23",
  NA, NA, "3320", "OEWS aggregate-to-leaf override", "C25",
  NA, NA, "3330", "OEWS aggregate-to-leaf override", "C28",
  NA, NA, "3370", "OEWS aggregate-to-leaf override", "C31T33",
  NA, NA, "4840", "OEWS aggregate-to-leaf override", "H49",
  NA, NA, "5170", "OEWS aggregate-to-leaf override", "J61",
  NA, NA, "5182", "OEWS aggregate-to-leaf override", "J62_63",
  NA, NA, "4230", "US/Canada override", "G",
  NA, NA, "4231", "US/Canada override", "G",
  NA, NA, "4234", "US/Canada override", "G",
  NA, NA, "4238", "US/Canada override", "G",
  NA, NA, "4240", "US/Canada override", "G",
  NA, NA, "4243", "US/Canada override", "G",
  NA, NA, "4245", "US/Canada override", "G",
  NA, NA, "4251", "US/Canada override", "G",
  NA, NA, "5251", "US/Canada override", "K",
  NA, NA, "5259", "US/Canada override", "K",
  NA, NA, "9991", "OEWS government designation", "O",
  NA, NA, "9992", "OEWS government designation", "O",
  NA, NA, "9993", "OEWS government designation", "O"
)

crosswalk <- bind_rows(
  crosswalk_automatic %>% filter(!naics %in% unique(crosswalk_manual$naics)),
  crosswalk_manual) %>%
  filter(naics %in% unique(dat_us$naics)) %>%
  distinct(donor_naics, isic, naics, naics_mapping_source, tiva)


# n counts distinct classification links, not economic size. For aggregate
# BLS codes, each distinct descendant-NAICS/ISIC pair is one link.
crosswalk_weights <- crosswalk %>%
  group_by(naics, tiva) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(naics) %>%
  mutate(weight = n / sum(n)) %>%
  ungroup()


# Join us_data with Crosswalk
#--------------------------------------------#.
# first check if all us data have the naics in crosswalk 
setdiff(
  unique(dat_us$naics),
  unique(crosswalk$naics)
)
setdiff(
  unique(crosswalk$naics),
  unique(dat_us$naics)
)

# here becuase we have some naics that appear in multiple 
# isics we need to adjust. 
# we do it based on occurrences on most granular level using weights from crosswalk_weights data. 
# thats why we need to multiply our estimates
dat_us_weighted = dat_us %>%
  inner_join(
    crosswalk_weights %>% select(-n), # we can do inner join cause we checked its ok
    by = "naics",
    relationship = "many-to-many"
  ) %>%
  mutate(
    weighted_emp = TOT_EMP * weight,
    weighted_wage_bill = wage_bill * weight,
    weighted_exposed = aisa_wage_bill * weight,
    weighted_acem_exp = acem_exp_wage_bill*weight
  )



#--------------------------------------------#.
# Aggregate US occupational data to TiVA-Level
#--------------------------------------------#.

# We aggregate via sums rather than a direct weighted mean to
# avoid problems

agg_us_tiva = dat_us_weighted %>%
  group_by(tiva,`AI capability`) %>%
  summarise(
    emp = sum(weighted_emp, na.rm = TRUE),
    wage_bill = sum(weighted_wage_bill, na.rm = TRUE),
    aisa_wage_bill = sum(weighted_exposed, na.rm = TRUE),
    acem_exp_wage_bill = sum(weighted_acem_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    AISA_bill_weighted = aisa_wage_bill / wage_bill,
    acem_exp_weighted = acem_exp_wage_bill/wage_bill
  ) %>%
  
  select(tiva, `AI capability`, emp, wage_bill,AISA_bill_weighted,acem_exp_weighted)

