
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

options(scipen = 999)
setwd(here())

# ---------------------

#-----------------------------------------------------###
#HELPERS FN####
#-----------------------------------------------------###
assert_conserved <- function(before, after, label, tolerance = 1e-8) { # fn to test conservation of data during crosswalk to tiva
  before <- as.numeric(before)
  after <- as.numeric(after)
  difference <- abs(before - after)
  scale <- pmax(abs(before), 1)

  if (length(before) != length(after) ||
      any(!is.finite(before)) ||
      any(!is.finite(after)) ||
      any(difference > tolerance * scale)) {
    stop(
      sprintf(
        "%s is not conserved (maximum absolute difference: %.6f).",
        label,
        max(difference, na.rm = TRUE)
      ),
      call. = FALSE
    )
  }
}


#-----------------------------------------------------###
#Figure setting####
#-----------------------------------------------------###
FIGURE_FONT_FAMILY <- "Latin Modern Roman" # this one aligns with the paper style
font_dir <- here::here("fonts", "latin-modern")
sysfonts::font_add(
  family = FIGURE_FONT_FAMILY,
  regular = file.path(font_dir, "lmroman10-regular.otf"),
  bold = file.path(font_dir, "lmroman10-bold.otf"),
  italic = file.path(font_dir, "lmroman10-italic.otf"),
  bolditalic = file.path(font_dir, "lmroman10-bolditalic.otf")
)
sysfonts::font_add(
  family = "Latin Modern Math",
  regular = here::here(
    "fonts",
    "latin-modern",
    "latinmodern-math.otf"
  )
)

showtext::showtext_auto()

theme_paper <- function(base_size = 11, ...) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = FIGURE_FONT_FAMILY,
    ...
  )+
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        family = "Latin Modern Math"
      )
    )
}

# Global ggplot font setting
ggplot2::theme_update(
  text = ggplot2::element_text(family = FIGURE_FONT_FAMILY)
)
ggplot2::update_geom_defaults(
  "text",
  list(family = FIGURE_FONT_FAMILY)
)
ggplot2::update_geom_defaults(
  "label",
  list(family = FIGURE_FONT_FAMILY)
)

figure_text_theme <- function(base_size = 18) {
  theme(
    text = element_text(size = base_size),
    plot.title = element_text(size = base_size * 1.10),
    plot.subtitle = element_text(size = base_size),
    axis.title = element_text(size = base_size),
    axis.text = element_text(size = base_size * 0.90),
    legend.title = element_text(size = base_size * 0.90),
    legend.text = element_text(size = base_size * 0.85),
    strip.text = element_text(size = base_size * 0.90),
    plot.caption = element_text(size = base_size * 0.80)
  )
}

bis_cairo_pdf <- function(filename, width, height, ...) {
  grDevices::cairo_pdf(
    filename = filename,
    width = width,
    height = height,
    ...
  )
}

# this is a special fn to save figs
save_fig <- function(filename,
                     plot = ggplot2::last_plot(),
                     type = c("wide", "standard", "tall", "portrait"),
                     placement = c("full", "half"),
                     full_width = 13,
                     format = c("pdf", "png"),
                     dpi = 200) {
  type <- match.arg(type)
  placement <- match.arg(placement)
  format <- match.arg(format)
  
  aspect_ratio <- c(
    wide = 2.15,
    standard = 1.80,
    tall = 1.15,
    portrait = 0.96
  )
  
  width <- switch(
    placement,
    full = full_width,
    half = full_width * 0.48
  )
  height <- width / aspect_ratio[[type]]
  
  # Apply typography last so plot-specific layout choices remain intact.
  if (inherits(plot, "patchwork")) {
    plot <- plot & figure_text_theme()
  } else {
    plot <- plot + figure_text_theme()
  }
  
  figure_dir <- here::here("figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  
  ggplot2::ggsave(
    filename = file.path(figure_dir, paste0(filename, ".", format)),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = switch(
      format,
      pdf = bis_cairo_pdf,
      png = "png"
    ),
    dpi = dpi,
    bg = "white"
  )
  
  invisible(c(width = width, height = height))
}




#-----------------------------------------------------###
#Data####
#-----------------------------------------------------###
tiva_data <- readr::read_csv("./data/tiva_database/OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV,1.1+VALU...W..A.csv") %>% as.data.frame()
tim_data <- readr::read_csv("./data/tim_database/OECD.STI.PIE,DSD_TIM_2025@DF_TIM_2025,1.0+EMPN+LABR+VALU_EMPN...W..A.csv") %>% as.data.frame()
hierarchy = read_excel("./data/tiva_database/hierarchy.xlsx") %>% as.data.frame()
dat_aisa = read_excel("./data/aisa/work1207_data.xlsx", sheet = "data") %>% as.data.frame()
dat_employ = read_excel("./data/aisa/oesm22in4/oesm22in4/nat4d_M2022_dl.xlsx", sheet = "Nat4d_M2022_dl") %>% as.data.frame() %>%
  select(NAICS, NAICS_TITLE, OCC_CODE, OCC_TITLE, O_GROUP, TOT_EMP, A_MEAN) %>% rename(WAGE_AN_MEAN = A_MEAN)
crosswalk_raw = read_excel("./data/tiva_database/naics-2022-v1.0-isic4-en.xlsx") %>% as.data.frame()
crosswalk_isic_tiva = read_excel("./data/tiva_database/ISIC_to_TiVA_Mapping.xlsx") %>% as.data.frame()
geograph =  readr::read_csv("./data/continents/continents-according-to-our-world-in-data.csv") %>% as.data.frame() %>% rename(regions = "World regions according to OWID")
world_internet_raw = read_excel("./data/API_IT.NET.USER.ZS_DS2_en_excel_v2_19993.xls", skip = 3) %>% 
  as.data.frame() 
world_gdp = read_excel("./data/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_19310.xls", skip = 3) %>% 
  as.data.frame() %>% 
  select(1:2, "2023") %>% 
  rename(gdp_2023 = "2023")
mischetal = read_excel("./data/prod_gains_europe/plot_mischetal2025.xlsx") %>% 
  as.data.frame() 




#-----------------------------------------------------###
# Procedure to ensure that we have highest granularity####
#-----------------------------------------------------###

# Pre-filter TiVA for relevant records
tiva_filtered <- tiva_data %>%
  select(REF_AREA,`Reference area`,ACTIVITY, `Economic activity`, OBS_VALUE) %>%  #mill US dollars
  filter( !is.na(OBS_VALUE))

# Join hierarchy info to TiVA
tiva_with_levels <- tiva_filtered %>%
  left_join(hierarchy %>% select(Industry_Code, include,Hierarchy_Level,ROWS), by = c("ACTIVITY" = "Industry_Code")) %>% 
  filter(include ==1) %>% 
  select(-include)

# Get total value added for share-of-GDP calculation
tiva_totals <- tiva_with_levels %>%
  filter(ACTIVITY == "_T") %>%
  select(REF_AREA, total_value = OBS_VALUE)

# Compute share of GDP per industry
tiva_shares <- tiva_with_levels %>%
  left_join(tiva_totals, by = "REF_AREA") %>%
  mutate(share_of_gdp = OBS_VALUE / total_value)


#--------------------------------------------------------------###
# Figure: US Labour Share by Industry ####
#--------------------------------------------------------------###
my_pal <- met.brewer("Hiroshige", n = 8)

hierarchy_descs <- hierarchy %>%
  select(ACTIVITY = Industry_Code, 
         `Economic activity` = Industry_Description, 
         Hierarchy_Level) 

agg_lshare_data <- tim_data %>% 
  filter(Measure=="Compensation of employees", 
         `Unit of measure`=="Percentage of value added", 
         TIME_PERIOD==2022, 
         REF_AREA=="USA") %>% 
  select(REF_AREA, ACTIVITY, OBS_VALUE) %>% 
  rename(Lshare =OBS_VALUE ) %>%
  left_join(hierarchy_descs, by = "ACTIVITY") %>%
  filter(Hierarchy_Level == 1) %>% 
  filter(!is.na(Lshare), Lshare > 0) %>%
  group_by(ACTIVITY, `Economic activity`) %>%
  summarise(
    avg_lshare_percent = mean(Lshare, na.rm = TRUE),
    .groups = "drop"
  )

p_labour_share_us <- ggplot(agg_lshare_data,
       aes(x = avg_lshare_percent, 
           y = reorder(`Economic activity`, avg_lshare_percent), 
           # Use reorder here too so the fill matches the bar order
           fill = reorder(`Economic activity`, avg_lshare_percent))) +
  
  geom_col(alpha = 0.8) +
  
  geom_text(aes(label = round(avg_lshare_percent, 1)), 
            hjust = -0.2, 
            size = 5, 
            color = "grey10") +
  
  # override.order = TRUE ensures the palette is used sequentially
  # direction = -1 can be used if you want to flip the color gradient
  scale_fill_met_d("Hiroshige", override.order = TRUE) + 
  
  scale_x_continuous(
    limits = c(0, max(agg_lshare_data$avg_lshare_percent, na.rm = TRUE) * 1.1),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Labour Share as Percentage of Value Added (%)",
    y = NULL 
  ) +
  theme_paper(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA)
  ) 
p_labour_share_us
save_fig("labour_share_us", plot = p_labour_share_us, type = "standard")
  


#--------------------------------------------------------------###
# Figure: US Value Added by Industry ####
#--------------------------------------------------------------###
# value added of USA
tiva_data_chart = tiva_data %>%
  filter(TIME_PERIOD==2022, 
         REF_AREA=="USA") %>% 
  select(REF_AREA, ACTIVITY, OBS_VALUE) %>% 
  rename(Lshare =OBS_VALUE ) %>%
  left_join(hierarchy_descs, by = "ACTIVITY") %>%
  filter(Hierarchy_Level == 1) %>% 
  filter(!is.na(Lshare), Lshare > 0) %>%
  group_by(ACTIVITY, `Economic activity`) %>%
  summarise(
    value_added = mean(Lshare, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    value_added = value_added / sum(value_added) * 100
  )



p_value_added_us <- ggplot(tiva_data_chart,
       aes(x = value_added, 
           y = reorder(`Economic activity`, value_added),
           # Map fill to the same reorder as y to align the palette
           fill = reorder(`Economic activity`, value_added))) +
  
  geom_col(alpha = 0.8) +
  
  geom_text(aes(label = round(value_added, 1)), 
            hjust = -0.2, 
            size = 5, 
            color = "grey10") +
  
  # Apply the Hiroshige palette sequentially
  scale_fill_met_d("Hiroshige", override.order = TRUE) +
  
  scale_x_continuous(
    limits = c(0, max(tiva_data_chart$value_added, na.rm = TRUE) * 1.1),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Share of Total Value Added (%)",
    y = NULL 
  ) +
  theme_paper(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none", # Set to none since the labels are on the y-axis
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA)
  )
p_value_added_us
save_fig("value_added_us", plot = p_value_added_us, type = "standard")


#-------------------------------------------------#
# US JOBS: AISA####
# attaching AI exposure to the US labour market
#-------------------------------------------------#

dat_employ$OCC_CODE <- trimws(dat_employ$OCC_CODE)
dat_aisa$OCCCode <- trimws(dat_aisa$OCCCode)

acem_exposure <- read_csv("./data/Simple Macroeconomics of AI (2024)/task_taxonomy/training_data/eloundou_task.csv") %>%
  select(`Task ID`, onet_soc = `O*NET-SOC Code`, gpt4_automation)
acem_exposure <- acem_exposure %>%
  mutate(
    soc6 = substr(onet_soc, 1, 7),
    automation = ifelse(gpt4_automation > 0.5, gpt4_automation, 0)
  ) %>%
  group_by(soc6) %>%
  summarise(
    gpt4_exposure = mean(automation, na.rm = TRUE),
    .groups = "drop"
  )

dat_aisa = dat_aisa %>%
  left_join(acem_exposure %>% 
              transmute(soc6, acem_exp = gpt4_exposure), 
            by = join_by(OCCCode ==soc6 ))

#---------------------------------------------------------------#
# we impute aisa for bls occupations without AISA #### 

# first we calculate the number of employees in occupations to impute with weighting scheme
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
  "AISA core skills",
  "AISA side skills",
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

#-------------------------------#
# test here: what is the b parameter for the task distribiution in the economy. 

read_tsv("./data/aisa/db_27_2_text/Task Statements.txt") |> View()
tasks = read_tsv("./data/aisa/db_27_2_text/Task Statements.txt") |> 
  rename("OCCCode" = "O*NET-SOC Code" ) |> 
  mutate(OCCCode = str_sub(OCCCode, 1,7) ) |> 
  group_by(OCCCode) |> 
  mutate(taskshare=1/n())


national_soc_employment_2022_test =  dat_employ |>
  filter(O_GROUP == "detailed") |>
  transmute(
    OCCCode = OCC_CODE,
    employment_2022 = suppressWarnings(as.numeric(TOT_EMP)), 
    wage = suppressWarnings(as.numeric(WAGE_AN_MEAN))
  ) |>
  filter(is.finite(employment_2022), employment_2022 > 0) |>
  filter(is.finite(wage), wage > 0) |>
  group_by(OCCCode) |>
  summarise(
    compensation_2022 = sum(wage),
    .groups = "drop"
  )


tasks_w <- tasks |>
  left_join(
    national_soc_employment_2022_test,
    by = "OCCCode"
  ) |>
  mutate(
    W = taskshare * compensation_2022,
    logW = log(W)
  ) |>
  filter(is.finite(logW))

nu_hat <- median(tasks_w$logW, na.rm = TRUE)

b_hat <- mean(
  abs(tasks_w$logW - nu_hat),
  na.rm = TRUE
)

nu_hat
b_hat


laplace_density <- function(x, nu, b) {
  1 / (2 * b) * exp(-abs(x - nu) / b)
}

ggplot(tasks_w, aes(x = logW)) +
  geom_density() +
  stat_function(
    fun = \(x) laplace_density(x, nu_hat, b_hat),
    linetype = "dashed"
  ) +
  theme_minimal()



W_min <- quantile(tasks_w$W, 0.8)

tail <- tasks_w |>
  filter(W >= W_min)

alpha_hat <- nrow(tail) /
  sum(log(tail$W / W_min))

b_tail <- 1 / alpha_hat

b_tail
  

purrr::map_dfr(c(.7, .8, .9, .95), \(p) {
  
  W_min <- quantile(tasks_w$W, p)
  
  x <- tasks_w |>
    filter(W >= W_min)
  
  alpha <- nrow(x) / sum(log(x$W / W_min))
  
  tibble(
    cutoff = p,
    n = nrow(x),
    alpha = alpha,
    b = 1 / alpha
  )
})

#-------------------------------#

# set of individual positions in other dataset 
bls_detailed_soc <- dat_employ %>%
  filter(O_GROUP == "detailed") %>%
  distinct(OCC_CODE) %>%
  pull(OCC_CODE)

# there are some positions not avaliable in bls_detailed_soc but exist in aisa_dataset
unmatched_bls_soc <- setdiff(bls_detailed_soc, unique(aisa_exposure_exact$OCCCode))
print(c("Share of imputing aisa scores" , length(unmatched_bls_soc)/length(bls_detailed_soc), "%"))

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
  
  #
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
#---------------------------------------------------------------#


#---------------------------------------------------------------#
# finishing the dataset ####

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

# the amount of imputed data
dat_us |> 
  group_by(occupation_exposure_source) |>
  summarise(wage_bill = sum(wage_bill)) |>
  mutate(share = wage_bill / sum(wage_bill))

#---------------------------------------------------------------#

#---------------------------------###
# Figure: two occupations AISA####
#---------------------------------###

p_occupations_aisa <- dat_aisa %>%
  filter(`OCC Title` %in% c("Musicians and Singers", "Mathematicians")) %>%
  mutate(`OCC Title` = factor(`OCC Title`, 
                              levels = c("Musicians and Singers", "Mathematicians"))) %>%
  ggplot(aes(x = `AI capability`, y = AISA, fill = `OCC Title`)) +
  
  # Removed the static fill and show.legend = FALSE to let the scale work
  geom_col(width = 0.2, alpha = 0.9) +
  
  # Use the Hiroshige palette. With only 2 items, it will pick the ends of the palette.
  scale_fill_met_d("Hiroshige", override.order = TRUE) +
  
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 80, 20)) +
  
  facet_wrap(~ `OCC Title`, ncol = 2) +
  
  labs(
    x = "AI Capability (κ)",
    y = "Share of Automatable Tasks (AISA, %)"
  ) +
  
  theme_paper(base_size = 14) +
  theme(
    axis.title.x = element_text(family = "Latin Modern Math"),
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "none", # Hidden because the facet titles already label the groups
    strip.text = element_text(size = 14),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

p_occupations_aisa
save_fig("occupations_aisa", plot = p_occupations_aisa, type = "wide")

#--------------------------#
# Crosswalk NAICS → TiVA####
#--------------------------#

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
  NA_character_, NA_character_, "3250", "OEWS aggregate-to-leaf override", "C20",
  NA_character_, NA_character_, "3270", "OEWS aggregate-to-leaf override", "C23",
  NA_character_, NA_character_, "3320", "OEWS aggregate-to-leaf override", "C25",
  NA_character_, NA_character_, "3330", "OEWS aggregate-to-leaf override", "C28",
  NA_character_, NA_character_, "3370", "OEWS aggregate-to-leaf override", "C31T33",
  NA_character_, NA_character_, "4840", "OEWS aggregate-to-leaf override", "H49",
  NA_character_, NA_character_, "5170", "OEWS aggregate-to-leaf override", "J61",
  NA_character_, NA_character_, "5182", "OEWS aggregate-to-leaf override", "J62_63",
  NA_character_, NA_character_, "4230", "US/Canada override", "G",
  NA_character_, NA_character_, "4231", "US/Canada override", "G",
  NA_character_, NA_character_, "4234", "US/Canada override", "G",
  NA_character_, NA_character_, "4238", "US/Canada override", "G",
  NA_character_, NA_character_, "4240", "US/Canada override", "G",
  NA_character_, NA_character_, "4243", "US/Canada override", "G",
  NA_character_, NA_character_, "4245", "US/Canada override", "G",
  NA_character_, NA_character_, "4251", "US/Canada override", "G",
  NA_character_, NA_character_, "5251", "US/Canada override", "K",
  NA_character_, NA_character_, "5259", "US/Canada override", "K",
  NA_character_, NA_character_, "9991", "OEWS government designation", "O",
  NA_character_, NA_character_, "9992", "OEWS government designation", "O",
  NA_character_, NA_character_, "9993", "OEWS government designation", "O"
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


#--------------------------------------------#
# Join us_data with Crosswalk
#--------------------------------------------#
dat_us_agg = dat_us %>%
  mutate(
    aisa_wage_bill = wage_bill * AISA,
    aisa_wage_bill_core = wage_bill * `AISA core skills`,
    aisa_wage_bill_side = wage_bill * `AISA side skills`, 
    acem_exp_wage_bill = acem_exp*100*wage_bill
  )

# check if all us data have the naics in crosswalk 
setdiff(
  unique(dat_us_agg$naics),
  unique(crosswalk$naics)
)
setdiff(
  unique(crosswalk$naics),
  unique(dat_us_agg$naics)
)

# here becuase we have some naics that appear in multiple 
# isics we need to adjust. 
# we do it based on occurances on most granular level using weights from crosswalk_weights data. 
# thats why we need to multiply our estimates

dat_us_weighted = dat_us_agg %>%
  inner_join(
    crosswalk_weights %>% select(-n), # we can do inner join cause we checked its ok
    by = "naics",
    relationship = "many-to-many"
  ) %>%
  mutate(
    weighted_emp = TOT_EMP * weight,
    weighted_wage_bill = wage_bill * weight,
    weighted_exposed = aisa_wage_bill * weight,
    weighted_exposed_core = aisa_wage_bill_core * weight,
    weighted_exposed_side = aisa_wage_bill_side * weight, 
    weighted_acem_exp = acem_exp_wage_bill*weight
  )


#--------------------------------------------#
# Aggregate US occupational data to TiVA-Level
#--------------------------------------------#

# We aggregate via sums rather than a direct weighted mean to
# avoid problems

agg_us_tiva = dat_us_weighted %>%
  group_by(`AI capability`, tiva) %>%
  summarise(
    total_emp = sum(weighted_emp, na.rm = TRUE),
    total_wage_bill = sum(weighted_wage_bill, na.rm = TRUE),
    aisa_wage_bill = sum(weighted_exposed, na.rm = TRUE),
    acem_exp_wage_bill = sum(weighted_acem_exp, na.rm = TRUE),
    AISA = weighted.mean(AISA, w = weighted_emp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    AISA_bill_weighted = aisa_wage_bill / total_wage_bill,
    acem_exp_weighted = acem_exp_wage_bill/total_wage_bill
  )


#---------------------------------------------------------------------#
# JOIN US data to world####
#---------------------------------------------------------------------#

# which tiva shares are not in us data
setdiff(unique(tiva_shares$ACTIVITY),
        unique(agg_us_tiva$tiva))

# so what we do we assign 0 val
missing_sectors <- setdiff(unique(tiva_shares$ACTIVITY),
                           unique(agg_us_tiva$tiva))

# Get distinct AI capability levels
kappa_levels <- unique(agg_us_tiva$`AI capability`)

# Create all-zero rows for each missing sector and each kappa level
fill_missing <- expand.grid(
  `AI capability` = kappa_levels,
  tiva = missing_sectors,
  stringsAsFactors = FALSE
) %>%
  mutate(
    total_emp = 0,
    total_wage_bill = 0,
    aisa_wage_bill = 0,
    acem_exp_wage_bill = 0,
    AISA = 0,
    AISA_bill_weighted = 0,
    acem_exp_weighted = 0
  )



#! TFP CALC ####

tfp = tiva_shares %>% 
  left_join(agg_us_tiva %>% 
              bind_rows(fill_missing), by = join_by(ACTIVITY == tiva )) 

tfp_tot = tfp %>% 
  filter(`Economic activity` =="Total - All activities")

tfp = tfp %>% 
  filter(`Economic activity` !="Total - All activities")


# ADDING SHARE OF EMPLOYEES AND SELF-EMPLOYED ON VALUE ADDED
#---------------------------------------------------------------------#
tim_data2 = tim_data %>% 
  filter(Measure=="Compensation of employees", 
         `Unit of measure`=="Percentage of value added", 
         TIME_PERIOD==2022) %>% 
  select(REF_AREA, ACTIVITY, OBS_VALUE) %>% 
  rename(Lshare =OBS_VALUE )

tfp = tfp %>% 
  left_join(tim_data2,
            by = join_by(REF_AREA, ACTIVITY))



#------------------------------------------------------###
# Prepare internet penetration to add to the dataset####
world_internet =  world_internet_raw %>%
  pivot_longer(cols = `1960`:`2024`, names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(year)) %>%
  arrange(`Country Name`, desc(year)) %>%
  group_by(`Country Name`, `Country Code`) %>%
  summarise(latest_value = coalesce(
    value[year == 2023],
    value[!is.na(value)][1]
  ), .groups = "drop") 

world_internet =  world_internet %>%
  mutate(usa = world_internet %>% 
           filter(`Country Code`== "USA") %>% 
           pull(latest_value)) %>% 
  mutate(internet_use = latest_value/usa )
#------------------------------------------------------###





#---------------------------------------------------------------------#
tfp = tfp %>% 
  mutate(implementable = 0.49, #.49 by svanberg  #0.23 acemoglu
         labour_savings = 0.33) %>% #33 by bick #0.27 acemoglu 
  mutate(AISA= AISA/100,
         AISA_bill_weighted= AISA_bill_weighted/100,
         acem_exp_bill_weighted = acem_exp_weighted /100,
         
         Lshare = Lshare / 100
  ) %>% 
  mutate(
    tfp_bas_bill_weighted = (AISA_bill_weighted*implementable) * ( labour_savings * Lshare ), 
    tfp_bas_bill_weighted_labsav = (AISA_bill_weighted*implementable) * ( .36 * Lshare ),   
    tfp_bas_bill_weighted_acemlambda = (AISA_bill_weighted*implementable) * ( .27 * Lshare ),
    tfp_bas_bill_weighted_acemoglu = (AISA_bill_weighted*.23) * ( .27 * Lshare ),
    
    tfp_acem_bill_weighted_current_param = (acem_exp_bill_weighted*implementable) * ( labour_savings * Lshare ), 
    tfp_acem_bill_weighted = (acem_exp_bill_weighted*0.23) * ( .27 * Lshare ),
    
  ) %>% 
  # only individual countries allowed
  left_join(geograph, by = join_by(REF_AREA == Code)) %>% 
  filter(is.na(regions)==FALSE) 

tfp_share_audit <- tfp %>%
  group_by(REF_AREA, `Reference area`, `AI capability`) %>%
  summarise(shares = sum(share_of_gdp), .groups = "drop")

gdp = tfp %>% 
  group_by(REF_AREA, `Reference area`, `AI capability`) %>% 
  summarise(
    tfp_bas_bill_weighted = weighted.mean(tfp_bas_bill_weighted, w = share_of_gdp, na.rm=T),
    tfp_acem_bill_weighted = weighted.mean(tfp_acem_bill_weighted, w = share_of_gdp, na.rm=T),
    tfp_acem_bill_weighted_current_param = weighted.mean(tfp_acem_bill_weighted_current_param, w = share_of_gdp, na.rm=T),
    tfp_bas_bill_weighted_labsav= weighted.mean(tfp_bas_bill_weighted_labsav, w = share_of_gdp, na.rm=T),
    tfp_bas_bill_weighted_acemoglu= weighted.mean(tfp_bas_bill_weighted_acemoglu, w = share_of_gdp, na.rm=T),
    tfp_bas_bill_weighted_acemlambda = weighted.mean(tfp_bas_bill_weighted_acemlambda, w = share_of_gdp, na.rm=T),
    AISA_bill_weighted = weighted.mean(AISA_bill_weighted, w = share_of_gdp, na.rm=T),
    AISA = weighted.mean(AISA, w = share_of_gdp, na.rm=T),
    Lshare = weighted.mean(Lshare, w = share_of_gdp, na.rm=T),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  left_join(geograph, by = join_by(REF_AREA == Code)) %>% 
  filter(is.na(regions)==FALSE) %>% 
  left_join(world_internet,  by = join_by(REF_AREA == `Country Code`)) %>% 
  mutate(gdp = tfp_bas_bill_weighted * (1/(1-(1-Lshare))) * 100, 
         gdp_eloundou = tfp_acem_bill_weighted_current_param* (1/(1-(1-Lshare))) * 100, 
         gdp_adj_eloundou = tfp_acem_bill_weighted_current_param* (1/(1-(1-Lshare))) * 100 * internet_use, 
         gdp_adj_internet = tfp_bas_bill_weighted * (1/(1-(1-Lshare))) * 100 * internet_use, 
         gdp_lab_sav = tfp_bas_bill_weighted_labsav* (1/(1-(1-Lshare))) * 100 * internet_use, 
         gdp_acemoglu = tfp_bas_bill_weighted_acemoglu* (1/(1-(1-Lshare))) * 100 * internet_use,
         gdp_acem_lambda = tfp_bas_bill_weighted_acemlambda* (1/(1-(1-Lshare))) * 100 * internet_use,
         ) %>% 
  left_join(world_gdp, by = join_by(REF_AREA == `Country Code`))

gdp =gdp %>% filter(is.na(gdp_adj_internet)==FALSE)
gdp %>% filter(`AI capability`==1) %>% nrow()

#---------------------###
# Figure: US TFP####
#---------------------###

my_pal <- met.brewer("Hiroshige", n = 8)

p_tfp_growth_us <- gdp %>%
  filter(REF_AREA == "USA") %>% 
  ggplot(aes(x = `AI capability`, y = tfp_bas_bill_weighted * 100)) +
  
  # 1. Background Shading - použijeme nejsvětlejší barvu z palety pro jemnost
  annotate("rect", xmin = 2.6, xmax = 6, ymin = -Inf, ymax = Inf, 
           fill = my_pal[5], alpha = 0.2) + # Prostřední barva palety, hodně průhledná
  
  # 2. Reference Lines
  # Acemoglu Baseline - použijeme tmavě okrovou/oranžovou z palety (první barva)
  geom_hline(yintercept = 0.71, linetype = "dotted", color = my_pal[1], linewidth = 1) +
  annotate("text", x = 4.5, y = 0.85, label = "Acemoglu (2025) TFP (0.71%)", 
           color = my_pal[1], size = 5, hjust = 0) +
  
  # Bick Estimate - jemná šedá nebo tmavší odstín z palety
  geom_hline(yintercept = 1.1, linetype = "dashed", color = my_pal[2], linewidth = 0.8) +
  annotate("text", x = 4.5, y = 1.25, label = "Bick (2026) Survey Estimate (1.1%)", 
           color = my_pal[2], size = 5, hjust = 0) +
  
  # 3. Main Data Layer - hlavní čára bude tmavě modrá (poslední barva palety)
  geom_line(color = my_pal[7], linewidth = 1.2) +
  geom_point(size = 3, color = my_pal[7], fill = my_pal[7], shape = 21, stroke = 1.5) +  
  
  # 5. Labels and Scales
  labs(
    x = "AI Capability (κ)",
    y = "TFP Gain (%)"
  ) +
  scale_x_continuous(breaks = 1:6, limits = c(1, 6), expand = c(0.01, 0.01)) +
  scale_y_continuous(
    breaks = seq(0, 5, by = 0.5), 
    limits = c(0, 5)
  ) +
  
  # 6. Theme and Polish
  theme_paper(base_size = 15) +
  theme(
    axis.title = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95") # Velmi jemná mřížka
  )
p_tfp_growth_us
save_fig("tfp_growth_us", plot = p_tfp_growth_us, type = "wide")
# Backward-compatible filename used in earlier drafts.
save_fig("tfp_growth_usa", plot = p_tfp_growth_us, type = "wide")





#---------------------###
# Figure: US GDP####
#---------------------###

my_pal <- met.brewer("Hiroshige", n = 8)

# acemoglu estimates 1.1gdp gain for USA so in our case this is something below level three of exposure. 
# maximum here 3.3%
p_gdp_growth_us <- gdp %>%
  filter(REF_AREA=="USA") %>% 
  ggplot(aes(x = `AI capability`, y =gdp) ) +
  geom_line(color = my_pal[7], linewidth = 1.2) +
  geom_point(size = 3, color = my_pal[7], fill = my_pal[7], shape = 21, stroke = 1.5) +  
  
  
  # 1. Background Shading - použijeme nejsvětlejší barvu z palety pro jemnost
  annotate("rect", xmin = 2.6, xmax = 6, ymin = -Inf, ymax = Inf, 
           fill = my_pal[5], alpha = 0.2) + # Prostřední barva palety, hodně průhledná
  
  # 2. Reference Lines
  # Acemoglu Baseline - použijeme tmavě okrovou/oranžovou z palety (první barva)
  geom_hline(yintercept = 1.1, linetype = "dotted", color = my_pal[1], linewidth = 1) +
  annotate("text", x = 4.5, y = 1.35, label = "Acemoglu (2025) GDP (1.1%)", 
           color = my_pal[1], size = 5, hjust = 0) +
   labs(
    x = "AI Capability (κ)",
    y = "GDP Gain (%)",
  ) +
  scale_x_continuous(breaks = 1:6, limits = c(1, 6), expand = c(0.01, 0.01)) +
  theme_paper(base_size = 12) +
  scale_y_continuous(
    breaks = seq(0, 9, by = 1),
    limits = c(0, 9)
  )+

  # 6. Theme and Polish
  theme_paper(base_size = 15) +
  theme(
    axis.title = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95") # Velmi jemná mřížka
  )
p_gdp_growth_us
save_fig("gdp_growth_us", plot = p_gdp_growth_us, type = "wide")




#-------------------------###
# Figure: Global GDP impact####
#-------------------------###

my_pal <- met.brewer("Hiroshige", n = 8)

global_gdp_by_capability <- gdp %>%
  group_by( `AI capability`) %>% 
  summarise(gdp_adj_internet = weighted.mean(gdp_adj_internet, w = gdp_2023), 
            gdp = weighted.mean(gdp, w = gdp_2023),
            .groups = "drop")

p_gdp_growth_world <- global_gdp_by_capability %>%
  ggplot(aes(x = `AI capability`, y = gdp_adj_internet)) +
  geom_line(color = my_pal[7], linewidth = 1.2) +
  geom_point(size = 3, color = my_pal[7], fill = my_pal[7], shape = 21, stroke = 1.5) +  
  
  # 1. Background Shading - použijeme nejsvětlejší barvu z palety pro jemnost
  annotate("rect", xmin = 2.6, xmax = 6, ymin = -Inf, ymax = Inf, 
           fill = my_pal[5], alpha = 0.2) + # Prostřední barva palety, hodně průhledná
  
  geom_line(aes(x = `AI capability`, y = gdp), col = my_pal[1], linetype= "dashed", linewidth = 1.2)+
  annotate("text", x = 4, y = 7,
           label = "GDP Growth without \nInternet Penetration Adjustment", 
           hjust = 1, color = my_pal[1], size = 4.5) +  
  annotate("text", x = 6, y = 6.5,
           label = "GDP Growth with Internet Penetration Adjustment", 
           hjust = 1, color = my_pal[7], size = 4.5) + 
  labs(
    x = "AI Capability (κ)",
    y = "GDP Gain (%)",
  ) +
  scale_x_continuous(breaks = 1:6, limits = c(1, 6), expand = c(0.01, 0.01)) +
  scale_y_continuous(
    breaks = seq(0, 8.5, by = 1),
    limits = c(0, 8.5)
  )+
  # 6. Theme and Polish
  theme_paper(base_size = 15) +
  theme(
    axis.title = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95") # Velmi jemná mřížka
  )
p_gdp_growth_world
save_fig("gdp_growth_world", plot = p_gdp_growth_world, type = "wide")


#---------------------------------------###
## ...how much world's gdp do we consider?####
#---------------------------------------###


# how much world's gdp we consider
ourgdp = gdp %>% 
  distinct(REF_AREA,regions) %>% 
  left_join(tfp_tot %>% 
              select(REF_AREA,`Reference area`,total_value)) %>% 
  right_join(world_gdp, by = join_by(REF_AREA == `Country Code`)) %>% 
  filter(is.na(total_value)==FALSE) %>% 
  summarise(gdp = sum(gdp_2023)) %>% pull()
world_gdp_out = world_gdp %>% filter(`Country Name`=="World") %>% pull(gdp_2023)

cat("our dataset represents", ourgdp*100/world_gdp_out,
    "of world's gdp according to data from world bank")



#-------------------------------------------------------#
#Figure: Country vs service share k=2.6 ####
#-------------------------------------------------------#


# 2) Important countries
important_countries <- c("USA", "CHN", "IND", "DEU", "JPN", "BRA", "EGY")
what_ai_capa_to_use = c(2.6)

service_share <- tfp %>%
  filter(`AI capability` == 1) %>%
  mutate(is_service = case_when(
    grepl("^G|^H|^I|^J|^K|^L|^M|^N|^O|^P|^Q|^R|^S|^T|^U", ACTIVITY) ~ TRUE,
    TRUE ~ FALSE  ) ) %>% 
  group_by(REF_AREA,is_service) %>%
  summarise(
    share_of_service_gdp = sum(share_of_gdp, na.rm = TRUE)
  ) %>% 
  filter(is_service == TRUE) %>% 
  select(-is_service)


# 4) Prepare label positions
label_positions <- tibble::tibble(
  REF_AREA = important_countries,
  highlight = c("United States", "China", "India", "Germany", "Japan", "Brazil", "Egypt"),
  x_label_shift = 0,    # shift X for each label
  y_label_shift = c(.2,.2,.2,.2,-.2,-.4,.25) # shift Y individually for each label
)


# 5) Merge label positions into main data
gdp_final <- gdp %>%
  left_join(service_share, by = join_by(REF_AREA)) %>%
  filter(`AI capability` %in% what_ai_capa_to_use) %>%
  left_join(label_positions, 
            #by = join_by(REF_AREA==REF_AREA, `AI capability`)) %>%
            by = join_by(REF_AREA==REF_AREA)) %>%
  
  left_join(
    gdp %>% 
      filter(`AI capability` %in% c(max(what_ai_capa_to_use))) %>%
      transmute(REF_AREA,maxgdp = gdp_adj_internet),
    by = join_by(REF_AREA)
  ) %>% 
  
  mutate(
    x_label = share_of_service_gdp*100 + coalesce(x_label_shift, 0),  # allow x movement
    y_label = maxgdp + coalesce(y_label_shift, 0)  # allow y movement from real point
  )



#plot
pp11=
  ggplot(gdp_final, aes(x = share_of_service_gdp*100, y = gdp_adj_internet)) +
  
  geom_smooth(method = "lm", se = FALSE, col = "grey70", linewidth = 0.5)+
  
  geom_segment(
    data = gdp_final %>% filter(!is.na(highlight)),
    aes(x = share_of_service_gdp*100, xend = x_label,
        y = gdp_adj_internet, yend = y_label),
    color = "grey90",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_point(aes(color = regions), size = 4, alpha = 1) +
  
  annotate(
    "text",
    x = gdp_final %>% filter(!is.na(highlight)) %>% pull(x_label),
    y = gdp_final %>% filter(!is.na(highlight)) %>% pull(y_label),
    label = gdp_final %>%
      filter(!is.na(highlight)) %>%
      mutate(label_for_plot = ifelse(`AI capability` == 2.6 & !is.na(highlight), highlight, "")) %>%
      pull(label_for_plot),
    size = 5,
    hjust = 0.5,
    vjust = 0
  ) +
  scale_color_met_d("Hiroshige")+
  scale_fill_met_d("Hiroshige") +
  
  scale_y_continuous(
    limits = c(0, 2.5),
    breaks = seq(0, 2.5, 0.5)
  ) +
  
  scale_x_continuous(
    limits = c(30, 95),
    breaks = seq(10, 90, 20)
  ) +
  labs(
    x = "Share of Services on Value Added (%)",
    y = "GDP Gain (%)",
    color = "World Region") +
  
  theme_paper(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5), #face = "bold",
    axis.title = element_text( size = 18,color = "grey30"),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # <<< cleaner vertical grid
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )



pp22 =ggplot(gdp_final) +
  
  geom_boxplot(aes(y = gdp_adj_internet, fill = regions), 
               color = "grey70",size = 0.1)+
  
  scale_color_met_d("Hiroshige")+
  scale_fill_met_d("Hiroshige") +
  scale_y_continuous(
    limits = c(0, 2.5),
    breaks = seq(0, 2.5, 0.5), 
    position = "right"
  ) +
  labs(
    x = "Share of Services on Value Added (%)",
    y = "GDP Gain (%)",
    color = "World Region") +
  
  theme_paper(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5), #face = "bold",
    axis.title = element_text( size = 18,color = "grey30"),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(), # <<< cleaner vertical grid
    legend.position = "none",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )



p_gdp_world_current <- pp11 + pp22 + plot_layout(widths = c(5, 1))
p_gdp_world_current
save_fig("gdp_world_current", plot = p_gdp_world_current, type = "standard")

lm(data = gdp_final %>% mutate(share_of_service_gdp = share_of_service_gdp*100), "gdp_adj_internet ~ share_of_service_gdp" )


#-------------------------------------------------------#
#Figure: Country vs service share k=6####
#-------------------------------------------------------#


# 2) Important countries
important_countries <- c("USA", "CHN", "IND", "DEU", "JPN", "BRA", "EGY")
what_ai_capa_to_use = c(6)

service_share <- tfp %>%
  filter(`AI capability` == 1) %>%
  mutate(is_service = case_when(
    grepl("^G|^H|^I|^J|^K|^L|^M|^N|^O|^P|^Q|^R|^S|^T|^U", ACTIVITY) ~ TRUE,
    TRUE ~ FALSE  ) ) %>% 
  group_by(REF_AREA,is_service) %>%
  summarise(
    share_of_service_gdp = sum(share_of_gdp, na.rm = TRUE)
  ) %>% 
  filter(is_service == TRUE) %>% 
  select(-is_service)


# 4) Prepare label positions
label_positions <- tibble::tibble(
  REF_AREA = important_countries,
  highlight = c("United States", "China", "India", "Germany", "Japan", "Brazil", "Egypt"),
  x_label_shift = 0,    # shift X for each label
  y_label_shift = c(.5,.5,.5,.5,-.5,-1.2,.5) # shift Y individually for each label
)


# 5) Merge label positions into main data
gdp_final <- gdp %>%
  left_join(service_share, by = join_by(REF_AREA)) %>%
  filter(`AI capability` %in% what_ai_capa_to_use) %>%
  left_join(label_positions, 
            #by = join_by(REF_AREA==REF_AREA, `AI capability`)) %>%
            by = join_by(REF_AREA==REF_AREA)) %>%
  
  left_join(
    gdp %>% 
      filter(`AI capability` %in% c(max(what_ai_capa_to_use))) %>%
      transmute(REF_AREA,maxgdp = gdp_adj_internet),
    by = join_by(REF_AREA)
  ) %>% 
  
  mutate(
    x_label = share_of_service_gdp*100 + coalesce(x_label_shift, 0),  # allow x movement
    y_label = maxgdp + coalesce(y_label_shift, 0)  # allow y movement from real point
  )



#plot
pp11=
  ggplot(gdp_final, aes(x = share_of_service_gdp*100, y = gdp_adj_internet)) +
  
  geom_smooth(method = "lm", se = FALSE, col = "grey70", linewidth = 0.5)+
  
  geom_segment(
    data = gdp_final %>% filter(!is.na(highlight)),
    aes(x = share_of_service_gdp*100, xend = x_label,
        y = gdp_adj_internet, yend = y_label),
    color = "grey90",
    linewidth = 0.3,
    inherit.aes = FALSE
  ) +
  geom_point(aes(color = regions), size = 4, alpha = 1) +
  
  annotate(
    "text",
    x = gdp_final %>% filter(!is.na(highlight)) %>% pull(x_label),
    y = gdp_final %>% filter(!is.na(highlight)) %>% pull(y_label),
    label = gdp_final %>%
      filter(!is.na(highlight)) %>%
      mutate(label_for_plot = ifelse(`AI capability` == 6 & !is.na(highlight), highlight, "")) %>%
      pull(label_for_plot),
    size = 5,
    hjust = 0.5,
    vjust = 0
  ) +
  scale_color_met_d("Hiroshige")+
  scale_fill_met_d("Hiroshige") +
  
  scale_y_continuous(
    limits = c(0,10),
    breaks = seq(0, 10, 1)
  ) +
  
  scale_x_continuous(
    limits = c(30, 95),
    breaks = seq(10, 90, 20)
  ) +
  labs(
    x = "Share of Services on Value Added (%)",
    y = "GDP Gain (%)",
    color = "World Region") +
  
  theme_paper(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5), #face = "bold",
    axis.title = element_text( size = 18,color = "grey30"),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # <<< cleaner vertical grid
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )



pp22 =ggplot(gdp_final) +
  
  geom_boxplot(aes(y = gdp_adj_internet, fill = regions), 
               color = "grey70",size = 0.4)+
  
  scale_color_met_d("Hiroshige")+
  scale_fill_met_d("Hiroshige") +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 1), 
    position = "right"
  ) +
  labs(
    x = "Share of Services on Value Added (%)",
    y = "GDP Gain (%)",
    color = "World Region") +
  
  theme_paper(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5), #face = "bold",
    axis.title = element_text( size = 18,color = "grey30"),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(), # <<< cleaner vertical grid
    legend.position = "none",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )



p_gdp_world_max <- pp11 + pp22 + plot_layout(widths = c(5, 1))
p_gdp_world_max
save_fig("gdp_world_max", plot = p_gdp_world_max, type = "standard")

lm(data = gdp_final %>% mutate(share_of_service_gdp = share_of_service_gdp*100), "gdp_adj_internet ~ share_of_service_gdp" )





#---------------------------------------------------###
# Figure: REGIONAL GDP IMPACT ####
#---------------------------------------------------###
gdp_cap = gdp %>% 
  left_join(world_gdp %>% select(-`Country Code`)) %>% 
  left_join(tfp_tot %>% 
              transmute(REF_AREA,added_value = total_value)) %>% 
  filter(`AI capability` %in% c(2.6, 6)) %>%
  group_by(`AI capability`, regions) %>% 
  summarise(GDP_Growth = weighted.mean(gdp_adj_internet, w = gdp_2023, na.rm=T))%>%
  mutate(`AI capability` = factor(`AI capability`, levels = c(2.6, 6))) %>% 
  arrange(regions, `AI capability`)


p_gdp_impact_regions <- ggplot(
  gdp_cap,
  aes(x = regions, y = GDP_Growth, fill = regions)
) +
  geom_bar(
    stat = "identity", 
    position = "identity",
    aes(alpha = `AI capability`)
  ) +
  scale_color_met_d("Hiroshige") +
  scale_fill_met_d("Hiroshige") +
  
  # The "Economist-style" scaler box for Alpha
  scale_alpha_manual(
    values = c("2.6" = 1, "6" = 0.4), # Dropped from 0.4 to 0.15 to make it much brighter/paler
    name = "AI Capability",
    labels = c("Current (κ=2.6)", "Max Potential (κ=6)"),
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",     
      title.hjust = 0.5,          # Centers the title cleanly over the two boxes
      label.position = "bottom",  
      keywidth = unit(4, "cm"),   
      keyheight = unit(0.5, "cm"),
      override.aes = list(fill = "gray40") 
    )
  ) +
  
  # Kills the redundant World Region legend
  guides(fill = "none") + 
  
  labs(
    x = "World Region",
    y = "GDP Gain (%)"
  ) +
  ylim(0, 9) +
  theme_paper(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 14,color = "grey30"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    
    # Move legend to the bottom and center it
    legend.position = "bottom",
    legend.justification = "center", 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.spacing.x = unit(0, "cm"), 
    panel.grid.major.x = element_blank(),
    legend.margin = margin(t = 15),    # Adds breathing room above the legend
    panel.grid.major = element_line(color = "grey95")
  )+
  scale_y_continuous(
    breaks = seq(0, 9, by = 1),
    limits = c(0, 9)
  )
p_gdp_impact_regions
save_fig("gdp_impact_regions", plot = p_gdp_impact_regions, type = "standard")


#------------------------------------###
# Figure: Robustness, Iota and Lambda####
#------------------------------------###

my_pal <- met.brewer("Hiroshige", n = 8)

global_robustness_by_capability <- gdp %>%
  group_by(`AI capability`) %>% 
  summarise(gdp_adj_internet = weighted.mean(gdp_adj_internet, w = gdp_2023), 
            gdp = weighted.mean(gdp, w = gdp_2023),
            gdp_adj_labsav = weighted.mean(gdp_lab_sav, w = gdp_2023),
            gdp_adj_acemoglu = weighted.mean(gdp_acemoglu, w = gdp_2023),
            gdp_adj_acem_lambda = weighted.mean(gdp_acem_lambda, w = gdp_2023),
            .groups = "drop")

p_gdp_growth_world_robustness <- global_robustness_by_capability %>%
  ggplot(aes(x = `AI capability`, y = gdp_adj_internet)) +
  
  # Baseline (Solid color)
  geom_line(color = my_pal[7], size = 1.2) +
  geom_point(size = 3, color = my_pal[7]) +
  
  # Alternative lines (using alpha for monochromatic shades)
  geom_line(aes(y = gdp_adj_labsav), color = my_pal[7], alpha = 0.8, linetype = "dashed", size = 1.1) +
  geom_line(aes(y = gdp_adj_acemoglu), color = my_pal[7], alpha = 0.6, linetype = "dotted", size = 1.1) +
  geom_line(aes(y = gdp_adj_acem_lambda), color = my_pal[7], alpha = 0.4, linetype = "longdash", size = 1.1) +
  
  # Shaded Areas (tinted with low alphas)
  geom_ribbon(aes(ymin = gdp_adj_internet, ymax = gdp_adj_labsav), 
              fill = my_pal[7], alpha = 0.1) +
  geom_ribbon(aes(ymin = gdp_adj_acemoglu, ymax = gdp_adj_internet), 
              fill = my_pal[7], alpha = 0.05) +
  geom_ribbon(aes(ymin = gdp_adj_acem_lambda, ymax = gdp_adj_internet), 
              fill = my_pal[7], alpha = 0.05) +
  
  # Annotations (Alphas match their corresponding lines)
  annotate("text", x = 6, y = 8,
           label = "λ=0.36", 
           hjust = 1, color = my_pal[7], size = 4) +  
  annotate("text", x = 6, y = 3.4,
           label = "ι=0.23,λ=0.27", 
           hjust = 1, color = my_pal[7],size = 4) +  
  annotate("text", x = 6, y = 6,
           label = "λ=0.27", 
           hjust = 1, color = my_pal[7],  size = 4) + 
  annotate("text", x = 6, y = 6.9,
           label = "Baseline \n(S = Auer et al. (2024), ι=0.49, λ=0.33)", 
           hjust = 1, color = my_pal[7], size = 4) + 
  
  # Labels & Scales
  labs(
    x = "AI Capability (κ)",
    y = "GDP Gain (%)",
  ) +
  scale_x_continuous(breaks = 1:6, limits = c(1, 6), expand = c(0.01, 0.01)) +
  scale_y_continuous(
    breaks = seq(0, 8.5, by = 1),
    limits = c(0, 8.5)
  ) +
  
  # Theme
  theme_paper(base_size = 15) +
  theme(
    axis.title = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )
p_gdp_growth_world_robustness
save_fig(
  "gdp_growth_world_robustness",
  plot = p_gdp_growth_world_robustness,
  type = "wide"
)




#------------------------------------###
# Figure: Robustness, Eloundu####
#------------------------------------###

# this shows global gdp when acemoglu param and exposure is used
eloundu_gdp = gdp %>% 
  filter(`AI capability`==3) %>% 
  summarise(
    internet_use = weighted.mean(internet_use, w = gdp_2023, na.rm=T),
    tfp_acem_bill_weighted= weighted.mean(tfp_acem_bill_weighted, w = gdp_2023, na.rm=T),
    
    Lshare = weighted.mean(Lshare, w = gdp_2023, na.rm=T),
    gdp = sum(gdp, na.rm=T),
    .groups = "drop"
  ) %>% 
  mutate(gdp_adj_acemoglu = 
           tfp_acem_bill_weighted * (1/(1-(1-Lshare))) * 100 * internet_use) 


eloundu_gdp_current_param = gdp %>% 
  filter(`AI capability`==3) %>% 
  summarise(
    internet_use = weighted.mean(internet_use, w = gdp_2023, na.rm=T),
    tfp_acem_bill_weighted_current_param= weighted.mean(tfp_acem_bill_weighted_current_param, w = gdp_2023, na.rm=T),
    
    Lshare = weighted.mean(Lshare, w = gdp_2023, na.rm=T),
    gdp = sum(gdp, na.rm=T),
    .groups = "drop"
  ) %>% 
  mutate(gdp_adj_acemoglu_current_param = 
           tfp_acem_bill_weighted_current_param * (1/(1-(1-Lshare))) * 100 * internet_use) 


p_gdp_growth_robust_eloundou <- gdp %>%
  group_by(`AI capability`) %>% 
  summarise(
    gdp_adj_internet = weighted.mean(gdp_adj_internet, w = gdp_2023)) %>%
  ggplot(aes(x = `AI capability`, y = gdp_adj_internet)) +
  
  # První oblast - Klasický výplň (spodní vrstva)
  geom_ribbon(aes(ymin = eloundu_gdp$gdp_adj_acemoglu,
                  ymax = pmax(eloundu_gdp$gdp_adj_acemoglu, gdp_adj_internet)), 
              fill = my_pal[4], alpha = 0.9) +
  
  # Druhá oblast - Šrafování (aby bylo vidět překrytí)
  ggpattern::geom_ribbon_pattern(aes(ymin = eloundu_gdp_current_param$gdp_adj_acemoglu_current_param,
                          ymax = pmax(eloundu_gdp_current_param$gdp_adj_acemoglu_current_param, gdp_adj_internet)),
                      fill            = "transparent",      # Průhledné pozadí
                      pattern_fill    = my_pal[5],          # Barva šraf
                      pattern_color   = my_pal[5],          # Barva linek šraf
                      pattern         = 'stripe',           # Typ: proužky
                      pattern_density = 0.4,                # Hustota šraf
                      pattern_spacing = 0.02,               # Mezery mezi šrafami
                      pattern_angle   = 45,                 # Úhel šraf
                      alpha           = 0.4) +
  
  # Baseline (Solid color)
  geom_line(color = my_pal[7], size = 1.2) +
  geom_point(size = 3, color = my_pal[7]) +
  
  # Hline
  geom_hline(yintercept = eloundu_gdp$gdp_adj_acemoglu, color = my_pal[4], size = 1) +
  geom_hline(yintercept = eloundu_gdp_current_param$gdp_adj_acemoglu_current_param, color = my_pal[5], size = 1) +
  
  # Annotations (Využívám černé barvy pro lepší čitelnost, jak jsme řešili)
  annotate("text", x = 6, y = 2.6,
           label = "S = Eloundou (2024), ι=0.49, λ=0.33", 
           hjust = 1, color = my_pal[5], size = 7, fontface = "bold") + 
  annotate("text", x = 6, y = .8,
           label = "S = Eloundou (2024), ι=0.23, λ=0.27", 
           hjust = 1, color = my_pal[4], size = 7, fontface = "bold") + 
  annotate("text", x = 6, y = 7,
           label = "Baseline",
           hjust = 1, color = my_pal[7], size = 7) + 
  
  # Zbytek grafu
  labs(x = "AI Capability (κ)", y = "GDP Gain (%)") +
  scale_x_continuous(breaks = 1:6, limits = c(1, 6), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 8, by = 1), limits = c(0, 8.1)) +
  theme_paper(base_size = 20) +
  theme(
    axis.title = element_text(color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )
p_gdp_growth_robust_eloundou
save_fig(
  "gdp_growth_robust_eloundou",
  plot = p_gdp_growth_robust_eloundou,
  type = "wide"
)
save_fig(
  "gdp_growth_robust_eloundou_2",
  plot = p_gdp_growth_robust_eloundou,
  type = "tall",
  placement = "half"
)


#----------------------------------###
# Figure: Comparison with Eloundu####
#----------------------------------###

# 5) Merge label positions into main data
gdp_compar <- gdp %>%
  left_join(service_share, by = join_by(REF_AREA)) %>%
  filter(`AI capability` %in% 3) %>%
  left_join(label_positions, 
            #by = join_by(REF_AREA==REF_AREA, `AI capability`)) %>%
            by = join_by(REF_AREA==REF_AREA)) %>%
  
  left_join(
    gdp %>% 
      filter(`AI capability` %in% c(max(3))) %>%
      transmute(REF_AREA,maxgdp = gdp_adj_internet),
    by = join_by(REF_AREA)
  ) %>% 
  
  mutate(
    x_label = share_of_service_gdp*100 + coalesce(x_label_shift, 0),  # allow x movement
    y_label = maxgdp + coalesce(y_label_shift, 0)  # allow y movement from real point
  )


# eloundu with param iota =49, lambda = 33
p_gdp_growth_diff_eloundou_compar <- gdp_compar %>%
  # 1. Ultra-short names
  mutate(
    `ι=0.49, λ=0.33` = gdp_adj_internet - gdp_adj_eloundou, 
    `ι=0.23, λ=0.27` = gdp_adj_internet - gdp_acemoglu
  ) %>% 
  
  pivot_longer(
    cols = c(`ι=0.49, λ=0.33`, `ι=0.23, λ=0.27`),
    names_to = "Comparison",
    values_to = "Difference"
  ) %>% 
  
  ggplot(aes(x = Difference, fill = Comparison, color = Comparison)) +
  geom_density(alpha = 1, linewidth = 0.5) +
  geom_rug(linewidth = 0.8, color = "gray60", alpha = 0.5, sides = "b") +
  
  # 2. Match the exact short names here
  scale_fill_manual(values = c("ι=0.49, λ=0.33" = my_pal[5], "ι=0.23, λ=0.27" = my_pal[4])) +
  scale_color_manual(values = c("ι=0.49, λ=0.33" = my_pal[5], "ι=0.23, λ=0.27" = my_pal[4])) +
  
  # 3. Compact legend title
  labs(
    x = "Country GDP Gain Difference (pp, AISA, κ = 3)",
    y = "Density",
    fill = "Eloundou (2024) & Params:",
    color = "Eloundou (2024) & Params:"
  )+
  scale_x_continuous(breaks = seq(-1, 1.5, 0.25), expand = c(0.01, 0.01)) +
  
  # 7. Consistent Formatting
  theme_paper(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    
    # We NEED the legend now, so we move it to the bottom
    legend.position = "bottom",
    #legend.title = element_text(face = "bold"),
    
    axis.text.y = element_blank(),        
    axis.ticks.y = element_blank(),
    axis.title = element_text(color = "grey30"),
    axis.title.y = element_text(margin = margin(r = 10)),
    strip.background = element_rect(fill = "grey95", color = NA)
  )
p_gdp_growth_diff_eloundou_compar
save_fig(
  "gdp_growth_diff_eloundou_compar",
  plot = p_gdp_growth_diff_eloundou_compar,
  type = "tall",
  placement = "half"
)

# 
# #------------------------------------------###
# # Figure: Country level using Eloundou for S ####
# #------------------------------------------###
# 
# ggplot(gdp_final, aes(x = share_of_service_gdp*100, y = gdp_adj_eloundou)) +
#   
#   geom_smooth(method = "lm", se = FALSE, col = "grey70", linewidth =  0.5)+
#   
#   geom_segment(
#     data = gdp_final %>% filter(!is.na(highlight)),
#     aes(x = share_of_service_gdp*100, xend = x_label,
#         y = gdp_adj_internet, yend = y_label),
#     color = "grey90",
#     linewidth = 0.3,
#     inherit.aes = FALSE
#   ) +
#   geom_point(aes(color = regions), size = 4, alpha = 0.8) +  # slightly larger points, transparent
#   
#   annotate(
#     "text",
#     x = gdp_final %>% filter(!is.na(highlight)) %>% pull(x_label),
#     y = gdp_final %>% filter(!is.na(highlight)) %>% pull(y_label),
#     label = gdp_final %>%
#       filter(!is.na(highlight)) %>%
#       mutate(label_for_plot = ifelse(`AI capability` == 3 & !is.na(highlight), highlight, "")) %>%
#       pull(label_for_plot),
#     size = 5,
#     #fontface = "bold",
#     hjust = 0.5,
#     vjust = 0
#   ) +
#   
#   #scale_color_viridis_d(option = "viridis")+
#   scale_color_brewer(palette = "Set2") +  # <<< better colors
#   scale_y_continuous(
#     limits = c(0, 3.5),
#     breaks = seq(0, 3.5, 0.5)
#   ) +
#   
#   scale_x_continuous(
#     limits = c(25, 95),
#     breaks = seq(10, 90, 20)
#   ) +
#   
#   
#   facet_wrap(
#     ~`AI capability`,
#     ncol = 3,
#     labeller = as_labeller(function(x) paste0("Country Specific GDP Growth at AI Capability κ = 3.0"))
#   ) +
#   
#   labs(
#     x = "Share of Services on Value Added (%)",
#     y = "GDP Gain (%)",
#     color = "World Region",
#     #title = "The Impact of AI Capability on GDP Growth by Sector Composition"
#   ) +
#   
#   theme_paper(base_size = 14) +
#   theme(
#     plot.title = element_text(size = 18, hjust = 0.5), #face = "bold", 
#     axis.title = element_text( size = 18),
#     axis.text = element_text(size = 14),
#     panel.grid.minor = element_blank(),
#     #panel.grid.major.x = element_blank(),  # <<< cleaner vertical grid
#     legend.position = "bottom",
#     legend.title = element_text(size = 14),
#     legend.text = element_text(size = 14),
#     legend.key.height = unit(0.5, "cm"),
#     legend.key.width = unit(1.5, "cm"),
#     #strip.text = element_text(size = 14),
#     #strip.background = element_rect(fill = "grey95", color = NA)
#     strip.text = element_blank(),
#     strip.background = element_blank(), 
#     
#   )





#----------------------------------------------###
#Figure: Europe comparison with misch et al (2025)####
#----------------------------------------------###
order_vec <- gdp %>% 
  filter(`AI capability` == 2.6) %>% 
  mutate(tfp_growth = tfp_bas_bill_weighted * 100) %>%
  semi_join(mischetal, by = "REF_AREA") %>% 
  arrange(tfp_growth) %>% 
  pull(REF_AREA)

order_vec = c(order_vec, "Europe")

compare_misch <- bind_rows(
  #1 2.6
  gdp %>% 
    filter(`AI capability` == 2.6,
           REF_AREA %in% order_vec) %>%
    mutate(
      tfp_growth = tfp_bas_bill_weighted * 100,
      source = "κ = 2.6"
    ) %>%
    select(REF_AREA, tfp_growth, source),
  
  #2 mean 2.6
  gdp %>% 
    filter(`AI capability` == 2.6,
           REF_AREA %in% order_vec) %>% 
    left_join(world_gdp %>% select(-`Country Code`) )%>%
    mutate(
      tfp_growth = tfp_bas_bill_weighted * 100,
      source = "κ = 2.6"
    ) %>%
    select(REF_AREA, tfp_growth, source, gdp_2023) %>% 
    summarise(tfp_growth = weighted.mean(tfp_growth , w=gdp_2023)) %>% 
    mutate(REF_AREA = "Europe",source = "κ = 2.6"),
  
  #3 6
  gdp %>% 
    filter(`AI capability` == 6,
           REF_AREA %in% order_vec) %>%
    mutate(
      tfp_growth = tfp_bas_bill_weighted * 100,
      source = "κ = 6"
    ) %>%
    select(REF_AREA, tfp_growth, source),
  
  #4 6 mean
  gdp %>% 
    filter(`AI capability` == 6,
           REF_AREA %in% order_vec) %>% 
    left_join(world_gdp %>% select(-`Country Code`) )%>%
    mutate(
      tfp_growth = tfp_bas_bill_weighted * 100,
      source = "κ = 6"
    ) %>%
    select(REF_AREA, tfp_growth, source, gdp_2023) %>% 
    summarise(tfp_growth = weighted.mean(tfp_growth , w=gdp_2023)) %>% 
    mutate(REF_AREA = "Europe",source = "κ = 6"),
  
  #5
  mischetal
) %>%
  mutate(REF_AREA = factor(REF_AREA, levels = order_vec))

# Find the position to place the vertical line (assuming Europe is the last factor)
num_countries <- length(unique(compare_misch$REF_AREA))
fence_position <- num_countries - 0.5 

# 1. Extract 6 colors from the Hiroshige palette (goes from warm to dark blue)
hiro_pal <- met.brewer("Hiroshige", n = 6)

##############################3
# 1. Manual color mapping
custom_colors <- c(
  "Acemoglu (2024) baseline" = hiro_pal[1],
  "Eloundou et al.(2024)" = hiro_pal[2],
  "Gmyrek (2023) - conservative" = hiro_pal[3],
  "Webb (2020) - 6th decile" = hiro_pal[5],  # Match kappas
  "κ = 2.6" = hiro_pal[6],               
  "κ = 6" = hiro_pal[6]                  
)

# 2. Manual shape mapping (16 = circle, 17 = triangle)
custom_shapes <- c(
  "Acemoglu (2024) baseline" = 16,
  "Eloundou et al.(2024)" = 16,
  "Gmyrek (2023) - conservative" = 16,
  "Webb (2020) - 6th decile" = 16,
  "κ = 2.6" = 16,               # Triangle
  "κ = 6" = 16                  # Circle
)

# 3. Generate the plot
p_eu_compar <- ggplot(compare_misch, aes(x = REF_AREA, y = tfp_growth)) +
  
  # Add a background highlight for Europe
  annotate("rect", xmin = fence_position, xmax = num_countries + 0.5, 
           ymin = -Inf, ymax = Inf, fill = "gray95", alpha = 1) +
  
  # === LINES (Separated) ===
  geom_line(data = compare_misch %>% filter(source == "κ = 2.6" & REF_AREA != "Europe"),
            aes(color = "κ = 2.6", group = 1), linewidth = 0.8, linetype = "dotted") +
  
  geom_line(data = compare_misch %>% filter(source == "κ = 6" & REF_AREA != "Europe"),
            aes(color = "κ = 6", group = 1), linewidth = 0.8) +
  
  # === POINTS (Separated for total control) ===
  geom_point(data = compare_misch %>% filter(source == "Acemoglu (2024) baseline"),
             aes(color = "Acemoglu (2024) baseline", shape = "Acemoglu (2024) baseline"), size = 3) +
  
  geom_point(data = compare_misch %>% filter(source == "Eloundou et al.(2024)"),
             aes(color = "Eloundou et al.(2024)", shape = "Eloundou et al.(2024)"), size = 3) +
  
  geom_point(data = compare_misch %>% filter(source == "Gmyrek (2023) - conservative"),
             aes(color = "Gmyrek (2023) - conservative", shape = "Gmyrek (2023) - conservative"), size = 3) +
  
  geom_point(data = compare_misch %>% filter(source == "Webb (2020) - 6th decile"),
             aes(color = "Webb (2020) - 6th decile", shape = "Webb (2020) - 6th decile"), size = 3) +
  
  geom_point(data = compare_misch %>% filter(source == "κ = 2.6"),
             aes(color = "κ = 2.6", shape = "κ = 2.6"), size = 3) +
  
  geom_point(data = compare_misch %>% filter(source == "κ = 6"),
             aes(color = "κ = 6", shape = "κ = 6"), size = 3) +
  
  # === SCALES ===
  # The 'breaks' argument ensures the legend stays in your desired order
  scale_color_manual(values = custom_colors, breaks = names(custom_colors), name = NULL) +
  scale_shape_manual(values = custom_shapes, breaks = names(custom_shapes), name = NULL) +
  
  
  # Theme
  theme_paper(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 14,color = "grey30"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    legend.box = "vertical", 
    panel.grid.major = element_line(color = "grey95")
  ) +
  labs(
    x = "",
    y = "TFP Gain (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.5))

p_eu_compar
save_fig("eu_compar", plot = p_eu_compar, type = "wide")


#----------------------------------------------###
# Table: corr comparison####
#----------------------------------------------###
cor_testing = compare_misch %>% 
  pivot_wider(names_from = source, values_from = tfp_growth)

misch_correlation_matrix <- cor(
  cor_testing %>% select(where(is.numeric)),
  use = "pairwise.complete.obs"
)

library(knitr)

knitr::kable(
  misch_correlation_matrix,
  digits = 3,
  caption = "Correlation Matrix of Exposure Measures"
)




#----------------------------------------------###
# Table: Individual country 
#----------------------------------------------###


tab_country = gdp %>%
  filter(`AI capability`%in% c(2.6,6)) %>% 
  transmute(REF_AREA,
            `Reference Area`=`Reference area`,
            Region = regions,
            `AI Capability`=`AI capability`,
            `GDP Growth`=gdp_adj_internet) %>% 
  pivot_wider(names_from = `AI Capability`,values_from = `GDP Growth` ) %>% 
  rename("GDP Growth for current AI Capability (kappa = 2.6)" = "2.6", 
         "GDP Growth for maximum AI Capability (kappa = 6)" = "6") %>% 
  arrange(REF_AREA)


library(xtable)

xt <- xtable(tab_country,
             digits = 2,
             caption = "GDP Growth across Countries for Current and Maximum AI Capability")

print(xt,
      include.rownames = FALSE,
      booktabs = TRUE,
      sanitize.text.function = identity)



#----------------------------------------------###
# FIG: DEMOGRAPHICS####
#----------------------------------------------###

#-----------------------------------------------------###
# 1. DATA: Demographic Drag (Working-Age Population) ###
#-----------------------------------------------------###
# Note: Download "Population ages 15-64, total" from the World Bank 
# (Indicator: SP.POP.1564.TO) and save it in your data folder.

world_pop_raw <- read_excel("./data/API_SP.POP.1564.TO_DS2_en_excel_v2_1288.xls", skip = 3) %>% 
  as.data.frame()

# Calculate projected demographic growth/drag (e.g., 2023 to 2030)
# We calculate the percentage change in the working-age population.
world_pop <- world_pop_raw %>%
  select(`Country Name`, `Country Code`, `2019`, `2024`) %>%
  filter(!is.na(`2019`), !is.na(`2024`)) %>%
  mutate(
    pop_growth_pct = (`2024` - `2019`) / `2019` * 100
  ) %>%
  select(`Country Code`, pop_growth_pct)

#-----------------------------------------------------###
# 2. MERGE: Demographics with AI GDP Growth Data ###
#-----------------------------------------------------###
# We take your final 'gdp' dataframe, filter for current AI capability (kappa = 3),
# and join it with the demographic data to see the net effect.

demographic_analysis <- gdp %>%
  filter(`AI capability` == 2.6) %>% # Change to 6 to test the advanced AI scenario
  left_join(world_pop, by = join_by(REF_AREA == `Country Code`)) %>%
  filter(!is.na(pop_growth_pct), !is.na(gdp_adj_internet)) %>%
  mutate(
    # Net economic growth = AI Productivity gains + Demographic labor force changes
    net_economic_growth = gdp_adj_internet + pop_growth_pct,
    
    # Categorize countries based on their structural situation
    offset_status = case_when(
      pop_growth_pct < 0 & net_economic_growth >= 0 ~ "AI Offsets Decline",
      pop_growth_pct < 0 & net_economic_growth < 0  ~ "AI Fails to Offset",
      TRUE ~ "Growing Population"
    )
  ) %>%
 mutate(gdp_2023 = as.numeric(gdp_2023))

#-----------------------------------------------------###
# 3. FIGURE: AI GDP Growth vs Demographic Drag ###
#-----------------------------------------------------###
ggplot(demographic_analysis, 
       aes(x = pop_growth_pct, 
           y = gdp_adj_internet, 
           color = regions)) +
  
  # Add reference lines to divide the chart into analytical quadrants
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Breakeven line: where AI growth exactly cancels out population decline (y = -x)
  geom_abline(slope = -1, intercept = 0, linetype = "dotted", color = my_pal[1], size = 1) + 
  
  # Plot points; size based on country's total GDP
  geom_point(aes(size = gdp_2023), alpha = 1) +
  
  # Label countries of particular interest (e.g., severe demographic drag or high AI gain)
  geom_text(aes(label = ifelse(pop_growth_pct < -3 | gdp_adj_internet > 1.2, REF_AREA, "")),
            vjust = -2, size = 4, show.legend = FALSE, color = "grey30") +
  
  scale_size_continuous(guide = "none") + # Hide size legend to keep it clean
  scale_color_met_d("Hiroshige") +
  labs(
    x = "Projected Working-Age Population Growth 2023-2030 (%)",
    y = "AI-Driven GDP Gain (%)",
    color = "World Region"
  ) +
  
  ylim(0,2.5)+
  scale_x_continuous(breaks = seq(-20,20,by =5), limits = c(-20, 20), expand = c(0.01, 0.01)) +
  
  
  # Match your established aesthetic
  # 6. Theme and Polish
  theme_paper(base_size = 15) +
  theme(
    axis.title = element_text(color = "grey10"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey95") # Velmi jemná mřížka
  )




#-----------------------------------------------------###
# Figure: industry 6 COUNTRIES ####
#-----------------------------------------------------###

#-----------------------------------------------------###
# 1. DATA: Heterogeneity WITHIN Services (Using TiVA Codes) ###
#-----------------------------------------------------###
service_heterogeneity <- tfp %>%
  # Filter for current AI capabilities
  filter(`AI capability` == 2.6) %>%
  
  # Select a few key advanced and emerging economies to contrast
  filter(REF_AREA %in% c("USA", "CHN", "IND", "DEU", "JPN", "BRA", "EGY")) %>%
  
  # Map your explicit TiVA ACTIVITY codes to the clean sub-sector labels
  mutate(
    short_desc = case_when(
      ACTIVITY == "I" ~ "Hospitality & Food",
      grepl("^J", ACTIVITY) ~ "IT & Information", # Catches J58T60, J61, J62_63
      ACTIVITY == "K" ~ "Finance & Insurance",
      ACTIVITY == "L" ~ "Real Estate",
      ACTIVITY == "M" ~ "Professional & Scientific",
      ACTIVITY == "N" ~ "Admin & Support",
      TRUE ~ NA_character_ # Tags everything else (Manufacturing, Ag, etc.) as NA
    )
  ) %>%
  
  # Keep ONLY the specific service sub-sectors we just named
  filter(!is.na(short_desc)) %>%
  
  # Group and summarize in case there are multiple rows per sub-sector (like the 'J' codes)
  group_by(REF_AREA, `Reference area`, short_desc) %>%
  summarise(
    avg_tfp_growth = weighted.mean(tfp_bas_bill_weighted, w = share_of_gdp, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(service_heterogeneity, 
       aes(x = avg_tfp_growth * 100, # Convert to percentages
           y = reorder(short_desc, avg_tfp_growth), 
           fill = REF_AREA)) +
  
  geom_col(alpha = 0.85, width = 0.7, show.legend = FALSE) +
  
  # Add the percentage labels to the end of the bars
  geom_text(aes(label = sprintf("%.2f%%", avg_tfp_growth * 100)),
            hjust = -0.2, size = 3.5, color = "black") +
  
  # Facet by country to show the contrast
  facet_wrap(~ `Reference area`, ncol = 2) +
  
  # Ensure the x-axis has enough room for the text labels
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  
  # Assign distinct colors for each country
  scale_fill_met_d("Hiroshige") +
  
  labs(
    x = "AI-Driven TFP Gain (%) at Current Capabilities (\u03ba=2.6)",
    y = NULL
  ) +
  
  theme_paper(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  )



#-----------------------------------------------------###
# Figure: Industries ####
#-----------------------------------------------------###


broad_industry_map <- hierarchy %>%
  arrange(ROWS) %>%
  mutate(
    level1_code = if_else(
      Hierarchy_Level == 1,
      Industry_Code,
      NA_character_
    ),
    broad_industry = if_else(
      Hierarchy_Level == 1,
      Industry_Description,
      NA_character_
    )
  ) %>%
  tidyr::fill(level1_code, broad_industry, .direction = "down") %>%
  filter(include == 1) %>%
  filter(!Industry_Code %in% c("_T", "TOTAL")) %>%
  distinct(Industry_Code, .keep_all = TRUE) %>%
  transmute(
    ACTIVITY = Industry_Code,
    broad_industry,
    level1_code
  )


broad_industry_map <- tfp %>% 
  distinct(ACTIVITY, `Economic activity`) %>% 
  filter(`Economic activity` != "Total - All activities") %>%
  mutate(
    activity_section = str_sub(ACTIVITY, 1, 1),
    broad_industry = case_when(
      activity_section == "A" ~ "Agriculture",
      activity_section %in% c("B", "D", "E") ~ "Mining & utilities",
      activity_section == "C" ~ "Manufacturing",
      activity_section == "F" ~ "Construction",
      activity_section %in% c("G", "H", "I") ~ "Trade, transport & hospitality",
      activity_section == "J" ~ "Information & communication",
      activity_section == "K" ~ "Finance & insurance",
      activity_section == "L" ~ "Real estate",
      activity_section %in% c("M", "N") ~ "Professional & administrative services",
      activity_section %in% c("O", "P", "Q") ~ "Public, education & health",
      activity_section %in% c("R", "S") ~ "Arts, recreation & personal services",
      activity_section %in% c("T", "U") ~ "Household & extraterritorial activities",
      TRUE ~ "Other"
    )
  )

country_info <- gdp %>%
  filter(`AI capability` == 2.6) %>%
  select(
    REF_AREA,
    `Reference area`,
    regions,
    country_lshare = Lshare,
    internet_use,
    gdp_2023,
    country_gdp_gain = gdp_adj_internet
  ) %>%
  distinct()


tfp_broad <- tfp %>%
  filter(`AI capability` == 2.6) %>%
  filter(`Economic activity` != "Total - All activities") %>%
  left_join(
    broad_industry_map,
    by = "ACTIVITY"
  ) %>%
  left_join(
    country_info,
    by = c("REF_AREA", "Reference area", "regions")
  ) %>%
  filter(
    !is.na(tfp_bas_bill_weighted),
    !is.na(share_of_gdp),
    !is.na(country_lshare),
    !is.na(internet_use),
    !is.na(gdp_2023),
    !is.na(broad_industry)
  )


#-----------------------------------------------------###
# Country × broad industry GDP contributions
#-----------------------------------------------------###

broad_country_gdp <- tfp_broad %>%
  group_by(REF_AREA, `Reference area`, regions, broad_industry) %>%
  summarise(
    sector_tfp_contribution =
      sum(share_of_gdp * tfp_bas_bill_weighted, na.rm = TRUE),
    
    sector_gdp_contribution =
      sector_tfp_contribution /
      first(country_lshare) *
      first(internet_use) *
      100,
    
    sector_va_share =
      sum(share_of_gdp, na.rm = TRUE),
    
    sector_va_usd =
      first(gdp_2023) * sector_va_share,
    
    sector_tfp_gain =
      weighted.mean(
        tfp_bas_bill_weighted,
        w = share_of_gdp,
        na.rm = TRUE
      ) * 100,
    
    country_gdp_gain =
      first(country_gdp_gain),
    
    country_lshare =
      first(country_lshare),
    
    gdp_2023 =
      first(gdp_2023),
    
    .groups = "drop"
  )


#-----------------------------------------------------###
# Global broad-industry GDP contributions
#-----------------------------------------------------###

global_gdp_denominator <- broad_country_gdp %>%
  distinct(REF_AREA, gdp_2023) %>%
  summarise(global_gdp = sum(gdp_2023, na.rm = TRUE)) %>%
  pull(global_gdp)

broad_gdp_means <- broad_country_gdp %>%
  group_by(broad_industry) %>%
  summarise(
    global_gdp_contribution =
      sum(sector_gdp_contribution * gdp_2023, na.rm = TRUE) /
      global_gdp_denominator,
    
    global_va =
      sum(sector_va_usd, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(global_gdp_contribution)

broad_country_gdp <- broad_country_gdp %>%
  left_join(
    broad_gdp_means %>%
      select(broad_industry, global_gdp_contribution),
    by = "broad_industry"
  ) %>%
  mutate(
    broad_industry = reorder(broad_industry, global_gdp_contribution)
  )

broad_gdp_means <- broad_gdp_means %>%
  mutate(
    broad_industry = reorder(broad_industry, global_gdp_contribution)
  )

# check if everything is ok
broad_country_gdp %>%
  distinct(REF_AREA, gdp_2023, country_gdp_gain) %>%
  summarise(
    global_country_gdp_gain =
      sum(country_gdp_gain * gdp_2023, na.rm = TRUE) /
      sum(gdp_2023, na.rm = TRUE)
  ) %>%
  mutate(
    global_sum_industry_contributions =
      sum(broad_gdp_means$global_gdp_contribution, na.rm = TRUE),
    
    difference =
      global_sum_industry_contributions - global_country_gdp_gain
  )


#-----------------------------------------------------###
# Chart: GDP contribution by broad industry
#-----------------------------------------------------###

my_pal <- met.brewer("Hiroshige", n = 8)

p_gdp_gain_industries <- ggplot(
  broad_country_gdp,
  aes(
    x = sector_gdp_contribution,
    y = broad_industry
  )
) +
  geom_point(
    aes(color = regions),
    alpha = 0.9,
    size = 2.4
  ) +
  
  geom_point(
    data = broad_gdp_means,
    aes(
      x = global_gdp_contribution,
      y = broad_industry
    ),
    inherit.aes = FALSE,
    shape = 18,
    size = 6,
    color = "black"
  ) +
  
  scale_color_met_d("Hiroshige") +
  
  labs(
    x = "Contribution to GDP Gain (percentage points)",
    y = NULL,
    color = "World region"
  ) +
  
  coord_cartesian(
    xlim = c(
      0,
      max(broad_country_gdp$sector_gdp_contribution, na.rm = TRUE) * 1.05
    )
  ) +
  
  theme_paper(base_size = 20) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    axis.title = element_text(color = "grey30"),
    panel.grid.major = element_line(color = "grey95")
  )
p_gdp_gain_industries
save_fig(
  "gdp_gain_industries",
  plot = p_gdp_gain_industries,
  type = "standard"
)





#-----------------------------------------------------###
# Figure: Industry Decomposition ####
#-----------------------------------------------------###

what_ai_capability <- 2.6

country_scalars <- gdp %>%
  filter(`AI capability` == what_ai_capability) %>%
  distinct(REF_AREA, .keep_all = TRUE) %>%
  select(
    REF_AREA,
    alpha_c = Lshare,
    internet_use,
    gdp_2023,
    country_gdp_gain = gdp_adj_internet
  )

total_gdp_sample <- country_scalars %>%
  summarise(total = sum(gdp_2023, na.rm = TRUE)) %>%
  pull(total)


#-----------------------------------------------------###
# Row-level contribution
#-----------------------------------------------------###

decomp_rows <- tfp_broad %>%
  filter(`AI capability` == what_ai_capability) %>%
  select(
    REF_AREA,
    `Reference area`,
    regions,
    ACTIVITY,
    broad_industry,
    share_of_gdp,
    AISA_bill_weighted,
    Lshare,
    implementable,
    labour_savings,
    tfp_bas_bill_weighted
  ) %>%
  left_join(country_scalars, by = "REF_AREA") %>%
  mutate(
    S = AISA_bill_weighted,
    L = Lshare,
    Z = internet_use / alpha_c,
    
    # Global value-added weight of this country-sector row
    row_va_share =
      (gdp_2023 / total_gdp_sample) * share_of_gdp,
    
    # Contribution to global GDP gain, in percentage points
    row_contribution =
      row_va_share *
      S *
      implementable *
      labour_savings *
      L *
      Z *
      100
  ) %>%
  filter(
    !is.na(broad_industry),
    !is.na(row_contribution),
    !is.na(row_va_share),
    row_va_share > 0
  ) %>%
  filter(
    !str_detect(
      broad_industry,
      regex("household|extraterritorial", ignore_case = TRUE)
    )
  )



#-----------------------------------------------------###
# Broad-industry decomposition
#-----------------------------------------------------###

broad_decomp <- decomp_rows %>%
  group_by(broad_industry) %>%
  summarise(
    global_contribution =
      sum(row_contribution, na.rm = TRUE),
    
    global_va_share =
      sum(row_va_share, na.rm = TRUE),
    
    mean_S =
      weighted.mean(S, w = row_va_share, na.rm = TRUE),
    
    mean_L =
      weighted.mean(L, w = row_va_share, na.rm = TRUE),
    
    mean_Z =
      weighted.mean(Z, w = row_va_share, na.rm = TRUE),
    
    product_without_interaction =
      global_va_share *
      mean_S *
      first(implementable) *
      first(labour_savings) *
      mean_L *
      mean_Z *
      100,
    
    interaction_residual =
      if_else(
        product_without_interaction > 0,
        global_contribution / product_without_interaction,
        NA_real_
      ),
    
    .groups = "drop"
  ) %>%
  arrange(global_contribution) %>%
  mutate(
    broad_industry = factor(
      broad_industry,
      levels = broad_industry
    )
  )


#-----------------------------------------------------###
# Reproducibility outputs for manuscript numbers ####
#-----------------------------------------------------###

results_dir <- here::here("results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

manuscript_numbers <- bind_rows(
  global_gdp_by_capability %>%
    filter(`AI capability` %in% c(2.6, 6)) %>%
    pivot_longer(
      cols = c(gdp_adj_internet, gdp),
      names_to = "estimate",
      values_to = "value"
    ) %>%
    transmute(
      section = "Global GDP",
      geography = "World",
      capability = `AI capability`,
      estimate,
      value,
      unit = "percent"
    ),
  gdp %>%
    filter(REF_AREA == "USA", `AI capability` %in% c(1, 2.6, 6)) %>%
    transmute(
      section = "US baseline",
      geography = `Reference area`,
      capability = `AI capability`,
      estimate = "TFP gain",
      value = tfp_bas_bill_weighted * 100,
      unit = "percent"
    ) %>%
    bind_rows(
      gdp %>%
        filter(REF_AREA == "USA", `AI capability` %in% c(1, 2.6, 6)) %>%
        transmute(
          section = "US baseline",
          geography = `Reference area`,
          capability = `AI capability`,
          estimate = "GDP gain",
          value = gdp_adj_internet,
          unit = "percent"
        )
    ),
  global_robustness_by_capability %>%
    filter(`AI capability` %in% c(2.6, 6)) %>%
    pivot_longer(
      cols = c(
        gdp_adj_internet,
        gdp,
        gdp_adj_labsav,
        gdp_adj_acemoglu,
        gdp_adj_acem_lambda
      ),
      names_to = "estimate",
      values_to = "value"
    ) %>%
    transmute(
      section = "Global robustness",
      geography = "World",
      capability = `AI capability`,
      estimate,
      value,
      unit = "percent"
    ),
  gdp_cap %>%
    ungroup() %>%
    transmute(
      section = "Regional GDP",
      geography = regions,
      capability = as.numeric(as.character(`AI capability`)),
      estimate = "Adoption-adjusted GDP gain",
      value = GDP_Growth,
      unit = "percent"
    ),
  broad_decomp %>%
    transmute(
      section = "Industry decomposition",
      geography = as.character(broad_industry),
      capability = what_ai_capability,
      estimate = "Contribution to global GDP gain",
      value = global_contribution,
      unit = "percentage points"
    ),
  compare_misch %>%
    filter(REF_AREA == "Europe") %>%
    transmute(
      section = "European TFP comparison",
      geography = "Europe",
      capability = case_when(
        source == "κ = 2.6" ~ 2.6,
        source == "κ = 6" ~ 6,
        TRUE ~ NA_real_
      ),
      estimate = source,
      value = tfp_growth,
      unit = "percent"
    )
)

readr::write_csv(
  manuscript_numbers,
  file.path(results_dir, "manuscript_numbers.csv")
)
readr::write_csv(
  tab_country,
  file.path(results_dir, "country_gdp_results.csv")
)
readr::write_csv(
  misch_correlation_matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column("estimate"),
  file.path(results_dir, "misch_correlation_matrix.csv")
)

country_table_rows <- tab_country %>%
  transmute(
    row = sprintf(
      "  %s & %s & %s & %.2f & %.2f \\\\",
      REF_AREA,
      `Reference Area`,
      Region,
      `GDP Growth for current AI Capability (kappa = 2.6)`,
      `GDP Growth for maximum AI Capability (kappa = 6)`
    )
  ) %>%
  pull(row)

writeLines(
  country_table_rows,
  file.path(results_dir, "country_gdp_table_rows.tex"),
  useBytes = TRUE
)

correlation_labels <- c(
  "κ = 2.6" = "\\makecell{$\\kappa$ = 2.6}",
  "κ = 6" = "\\makecell{$\\kappa$ = 6}",
  "Webb (2020) - 6th decile" = "\\makecell{Webb (2020)\\\\6th decile}",
  "Acemoglu (2024) baseline" = "\\makecell{Acemoglu baseline}",
  "Gmyrek (2023) - conservative" = "\\makecell{Gmyrek (2023)\\\\conservative}",
  "Eloundou et al.(2024)" = "\\makecell{Eloundou et al.\\\\(2024)}"
)

correlation_table_rows <- purrr::map_chr(
  seq_len(nrow(misch_correlation_matrix)),
  function(row_index) {
    cells <- purrr::map_chr(
      seq_len(ncol(misch_correlation_matrix)),
      function(column_index) {
        if (row_index == column_index) {
          return("---")
        }

        value <- misch_correlation_matrix[row_index, column_index]
        highlight <- ifelse(value > 0.50, "\\cellcolor{corrblue}", "")
        paste0(highlight, sprintf("%.2f", value))
      }
    )

    paste0(
      correlation_labels[[rownames(misch_correlation_matrix)[[row_index]]]],
      " & ",
      paste(cells, collapse = " & "),
      " \\\\"
    )
  }
)

writeLines(
  correlation_table_rows,
  file.path(results_dir, "misch_correlation_table_rows.tex"),
  useBytes = TRUE
)


#-----------------------------------------------------###
# Driver heatmap data
#-----------------------------------------------------###

driver_heatmap <- broad_decomp %>%
  transmute(
    broad_industry,
    
    `AI exposure` =
      mean_S * 100,
    
    `Labour share` =
      mean_L * 100,
    
    `Global VA share` =
      global_va_share * 100,
    
    `Adoption/GDP scaling` =
      mean_Z,
    
    `Within-sector alignment` =
      interaction_residual
  ) %>%
  pivot_longer(
    cols = -broad_industry,
    names_to = "driver",
    values_to = "value"
  ) %>%
  group_by(driver) %>%
  mutate(
    z_value = as.numeric(scale(value))
  ) %>%
  ungroup() %>%
  mutate(
    driver = factor(
      driver,
      levels = c(
        "AI exposure",
        "Labour share",
        "Global VA share",
        "Adoption/GDP scaling",
        "Within-sector alignment"
      )
    ),
    
    label = case_when(
      driver %in% c("AI exposure", "Labour share", "Global VA share") ~
        sprintf("%.1f", value),
      
      driver %in% c("Adoption/GDP scaling", "Within-sector alignment") ~
        sprintf("%.2f", value),
      
      TRUE ~ ""
    )
  )

#-----------------------------------------------------###
# Left panel: contribution
#-----------------------------------------------------###

n_industries <- n_distinct(broad_decomp$broad_industry)

hiro_cols <- met.brewer(
  name = "Hiroshige",
  n = n_industries,
  type = "continuous"
)
sum(broad_decomp$global_contribution)
p_contribution <- ggplot(
  broad_decomp,
  aes(
    x = global_contribution,
    fill = reorder(broad_industry,global_contribution),
    y = broad_industry
  )
) +
  geom_col(
  ) +
  geom_text(
    aes(label = sprintf("%.2f", global_contribution)),
    hjust = -0.15,
    size = 3.6,
    color = "grey20"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  scale_fill_manual(values = hiro_cols) +
  labs(
    x = "Contribution to Global GDP Gain\n(percentage points)",
    y = NULL
  ) +
  theme_paper(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(color = "grey30"),
    plot.margin = margin(5.5, 10, 5.5, 5.5),
    legend.position = "none",
    strip.background = element_rect(fill = "grey95", color = NA)
  )



#-----------------------------------------------------###
# Right panel: driver heatmap
#-----------------------------------------------------###

z_lim <- max(abs(driver_heatmap$z_value), na.rm = TRUE)

p_drivers <- ggplot(
  driver_heatmap,
  aes(
    x = driver,
    y = broad_industry,
    fill = z_value
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.45
  ) +
  
  geom_text(
    aes(label = label),
    size = 3.2,
    color = "grey15"
  ) +
  
  scale_fill_gradient2(
    low = my_pal[1],
    mid = "white",
    high = my_pal[6],
    midpoint = 0,
    limits = c(-z_lim, z_lim),
    name = "Relative to\nother industries",
    labels = function(x) ifelse(
      x > 0,
      sprintf("+%.1f", x),
      sprintf("%.1f", x)
    )
  ) +
  
  scale_x_discrete(
    position = "bottom",
    labels = c(
      "AI task \nexposure",
      "Labour\nshare",
      "Global VA\nshare",
      "Adoption/GDP\nscaling",
      "Driver\nalignment"
    )
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_paper(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x.top = element_text(
      size = 10.5,
      angle = 0,
      hjust = 0.5,
      vjust = 0
    ),
    legend.position = "bottom",
    legend.title = element_text(size = 10.5),
    legend.text = element_text(size = 9.5),
    plot.margin = margin(5.5, 5.5, 5.5, 0)
  )


p_industry_decomposition <-
  p_contribution + p_drivers + patchwork::plot_layout(widths = c(1.15, 1.6))
p_industry_decomposition
save_fig(
  "industry_decomposition",
  plot = p_industry_decomposition,
  type = "standard"
)
# Note: The left panel reports 
# each broad industry’s contribution to the global
# GDP gain at κ = 2.6. The right panel decomposes 
# this contribution into underlying drivers. Cell 
# colours show standardised values within each driver; 
# cell labels report actual values. AI exposure, labour
# share and global VA share are in percent. Country-level scaling combines digital adoption
# with the conversion from TFP gains into GDP gains. Driver alignment is a 
# residual: it captures the difference between the actual contribution, calculated across all country-industry observations, and the contribution implied 
# by multiplying average sector drivers. 
# Values above
# one indicate that favourable drivers tend
# to coincide in the same country-industry observations; 
# values below one indicate weaker alignment.

#-----------------------------------------------###
# Animation: Animation - Country level ####
#-----------------------------------------------###


# Label positions
label_positions <- tibble::tibble(
  REF_AREA = important_countries,
  highlight = c("United States", "China", "India", "Germany", "Japan", "Brazil"),
  x_label_shift = 0,    # shift X for each label
  y_label_shift = c(.5,.5,.5,.5,.5,.4) # shift Y individually for each label
)

largest_region_economies <- gdp %>% group_by(regions) %>%
  left_join(world_gdp %>% select(-`Country Code`)) %>% 
  filter(`AI capability` == 3) %>% 
  group_by(regions) %>%
  # Vybere 3 řádky s nejvyšší hodnotou ve sloupci GDP
  slice_max(order_by = gdp_2023, n = 1) %>%
  ungroup()

# 5) Merge label positions into main data# 5) Merge label positiongdps into main data
gdp_final <- gdp %>%
  select(REF_AREA,`Reference area`,`AI capability`,regions,gdp_adj_internet) %>% 
  left_join(service_share, by = join_by(REF_AREA)) %>%
  filter(`AI capability` %in% c(3,6)) %>%
  left_join(label_positions, 
            by = join_by(REF_AREA==REF_AREA)) %>%
  
  left_join(
    gdp %>% 
      filter(`AI capability` %in% c(max(3,6))) %>%
      transmute(REF_AREA,maxgdp = gdp_adj_internet),
    by = join_by(REF_AREA)
  ) %>% 
  
  mutate(
    x_label = share_of_service_gdp*100 + coalesce(x_label_shift, 0),  # allow x movement
    y_label = maxgdp + coalesce(y_label_shift, 0)  # allow y movement from real point
  ) %>% 
  mutate(
    big_region = dplyr::case_when(
      regions %in% c("North America", "South America") ~ "Americas",
      regions == "Europe"                              ~ "Europe",
      regions %in% c("Asia", "Oceania")               ~ "Asia",
      regions %in% c("Africa", "Middle East")         ~ "Africa",
      TRUE                                            ~ "Other"
    ) %>% factor(levels = c("Africa","Americas","Asia","Europe","Other"))
  ) %>% 
  mutate(phase = case_when (`AI capability` ==3~1, 
                            TRUE ~2))  
gdp_final = gdp_final %>% 
  bind_rows(gdp_final %>% 
              filter(phase==2) %>% 
              mutate(phase =3))

# labels only for important countries
lbl = gdp_final %>%                      
  filter(`AI capability` == 6,
         !is.na(highlight)) %>%
  mutate(
    phase = 3,             # only appears in third phase
    xend  = x_label,
    yend  = y_label
  )

plussizetext = 4
plotik  = ggplot(gdp_final, aes(x = share_of_service_gdp*100, y = gdp_adj_internet)) +
  
  geom_smooth(method = "lm", se = FALSE, col = "grey70", linewidth =  0.5)+
  geom_segment(data = lbl,
               aes(xend = xend, yend = yend),
               color = "grey80", linewidth = .35,
               show.legend = FALSE) +
  geom_point(aes(color = big_region), size = 3, alpha = 0.8) +  # slightly larger points, transparent
  
  geom_text(
    data = lbl,
    aes(x = xend, y = yend, label = highlight),
    size = 3.2,
    hjust = 0.5,       
    vjust = 0,         
    col = "black",
    show.legend = FALSE
  )+
  scale_color_brewer(palette = "Set2") +  
  scale_y_continuous(
    limits = c(0, 8.5),
    breaks = seq(1, 8, 1)
  ) +
  
  scale_x_continuous(
    limits = c(29, 95),
    breaks = seq(30, 90, 30)
  ) +
  
  labs(
    x = "Share of Services on Value Added (%)",
    y = "GDP Gain (%)",
    color = "World Region"
  ) +
  
  theme_paper(base_size = 14+plussizetext) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5), 
    axis.title = element_text( size = 14+plussizetext),
    axis.text = element_text(size = 12+plussizetext),
    panel.grid.minor = element_blank(),
    #panel.grid.major.x = element_blank(),  
    legend.position = "bottom",
    legend.title = element_text(size = 12+plussizetext),
    legend.text = element_text(size = 12+plussizetext),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 14+plussizetext),
    strip.background = element_rect(fill = "grey95", color = NA)
  )+
  
  transition_states(phase, transition_length = 2, state_length = 7)+
  enter_fade() + exit_fade()+
  ease_aes('cubic-in-out')+
  labs(
    title = "AI GDP Impact on World",
    subtitle = "{ifelse(
      closest_state == 1,
      \"<span style='font-weight:600;'>Where We Stand Today</span><br>
        (based on Acemoglu, 2025)\",
      \"<span style='font-weight:600;'>Maximal GDP Impact with Advanced AI</span><br>
        (based on Auer et al., 2024)\"
    )}"
  ) +
  theme(
    plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1.1)  # centre & allow <br>
  )

animate(plotik, fps = 3, duration = 7, width = 1200*0.8, height = 675*0.8)

# anim_res = animate(plotik, fps = 15, duration = 12, width = 1200, height = 675)
# anim_save("animated_gdp_impact_country_level.gif", animation = anim_res)

#-----------just trying to improve


# NEVIM CO TOHLE JE KOMENTUJU


#-----------------------------------------------###
# Animation: Animation 2- Country level ####
#-----------------------------------------------###

# Label positions (No changes here)
label_positions <- tibble::tibble(
  REF_AREA = important_countries,
  highlight = c("United States", "China", "India", "Germany", "Japan", "Brazil"),
  x_label_shift = 0,
  y_label_shift = c(.5,.5,.5,.5,.5,.4)
)

# 5) Merge label positions into main data
gdp_final <- gdp %>%
  select(REF_AREA,`Reference area`,`AI capability`,regions,gdp_adj_internet) %>%
  left_join(service_share, by = join_by(REF_AREA)) %>%
  filter(`AI capability` %in% c(3,6)) %>%
  left_join(label_positions,
            by = join_by(REF_AREA==REF_AREA)) %>%
  
  # ### CHANGE 1: We don't technically need 'maxgdp' anymore if we want
  # the label to move WITH the point. We will use current GDP in the next step.
  
  mutate(
    # ### CHANGE 2: Dynamic Coordinates
    # We use 'gdp_adj_internet' (the current value) instead of 'maxgdp'.
    # This calculates a label position for Phase 1 AND Phase 2.
    x_label_pos = share_of_service_gdp*100 + coalesce(x_label_shift, 0),
    y_label_pos = gdp_adj_internet + coalesce(y_label_shift, 0)
  ) %>%
  mutate(
    big_region = dplyr::case_when(
      regions %in% c("North America", "South America") ~ "Americas",
      regions == "Europe"                              ~ "Europe",
      regions %in% c("Asia", "Oceania")               ~ "Asia",
      regions %in% c("Africa", "Middle East")         ~ "Africa",
      TRUE                                            ~ "Other"
    ) %>% factor(levels = c("Africa","Americas","Asia","Europe","Other"))
  ) %>%
  mutate(phase = case_when (`AI capability` ==3~1,
                            TRUE ~2))

gdp_final = gdp_final %>%
  bind_rows(gdp_final %>%
              filter(phase==2) %>%
              mutate(phase =3))


# alternative animation
plussizetext = 4
plotik2  = ggplot(gdp_final, aes(x = share_of_service_gdp*100, y = gdp_adj_internet)) +
  
  geom_smooth(method = "lm", se = FALSE, col = "grey70", linewidth =  0.5)+
  
  # ### CHANGE 4: Segment uses main data
  # We filter gdp_final for rows where highlight is not NA.
  # We draw from the point (x,y) to the label position (x_label_pos, y_label_pos)
  geom_segment(
    data = gdp_final %>% filter(!is.na(highlight)),
    aes(xend = x_label_pos, yend = y_label_pos),
    color = "grey80", linewidth = .35,
    show.legend = FALSE
  ) +
  
  geom_point(aes(color = big_region), size = 3, alpha = 0.8) +
  
  geom_text(
    data = gdp_final %>% filter(!is.na(highlight)),
    aes(x = x_label_pos, y = y_label_pos, label = highlight),
    size = 3.2,
    hjust = 0.5,
    vjust = 0,
    col = "black",
    show.legend = FALSE
  )+
  
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(1, 10, 1)
  ) +
  scale_x_continuous(
    limits = c(29, 95),
    breaks = seq(30, 90, 30)
  ) +
  
  labs(
    x = "Share of Services on Value Added (%)",
    y = "GDP Gain (%)",
    color = "World Region"
  ) +
  
  theme_paper(base_size = 14+plussizetext) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_text( size = 14+plussizetext),
    axis.text = element_text(size = 12+plussizetext),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 12+plussizetext),
    legend.text = element_text(size = 12+plussizetext),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 14+plussizetext),
    strip.background = element_rect(fill = "grey95", color = NA)
  )+
  
  # Transition logic remains the same
  transition_states(phase, transition_length = 2, state_length = 7)+
  enter_fade() + exit_fade()+
  ease_aes('cubic-in-out')+
  labs(
    title = "AI GDP Impact on World",
    subtitle = "{ifelse(
      closest_state == 1,
      \"<span style='font-weight:600;'>Where We Stand Today</span><br>
        (based on Acemoglu, 2025)\",
      \"<span style='font-weight:600;'>Maximal GDP Impact with Advanced AI</span><br>
        (based on Auer et al., 2024)\"
    )}"
  ) +
  theme(
    plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1.1)
  )

# Render
animate(plotik2, fps = 15, duration = 12, width = 1200*0.8, height = 675*0.8)

# anim_res2 = animate(plotik2, fps = 15, duration = 12, width = 1200, height = 675)
# anim_save("animated_gdp_impact_country_level_with_labels2.gif", animation = anim_res2)


# ADDITIONAL CALCULATIONS AUER VS ELOUNDOU

compare_df <- dat_us %>%
  filter(!is.na(AISA), !is.na(acem_exp), !is.na(`AI capability`)) %>% 
  mutate(acem_exp = acem_exp*100)


capability_similarity <- compare_df %>%
  group_by(`AI capability`) %>%
  summarise(
    pearson_cor = cor(AISA, acem_exp, method = "pearson"),
    spearman_cor = cor(AISA, acem_exp, method = "spearman"),
    n_occupations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(pearson_cor))

print(capability_similarity)



#--------------------------------------------------------------###
# Figure: Correlation Bar Chart ####
#--------------------------------------------------------------###
num_caps <- length(unique(capability_similarity$`AI capability`))
extended_hiroshige <- colorRampPalette(met.brewer("Hiroshige"))(num_caps)

p_corr_auer_eloundou <- ggplot(
  capability_similarity,
  aes(x = factor(`AI capability`))
) +
  # 1. Pearson Bars (Main view)
  geom_col(aes(y = pearson_cor * 100, fill = factor(`AI capability`)), 
           alpha = 0.9, width = 0.95) +
  
  # 2. Spearman Points (Robustness view)
  # We use a distinct shape (diamond or cross) to show the rank correlation
  geom_point(aes(y = spearman_cor * 100), 
             color = "grey70", shape = 18, size = 3) +
  
  # 3. Pearson Labels (Matching the style of image_9ecb9b.png)
  geom_text(aes(y = pearson_cor * 100, label = paste0(round(pearson_cor * 100, 0), "%")),
            vjust = -0.5, size = 5, color = "grey30") +
  
  scale_fill_manual(values = extended_hiroshige) +
  
  # 4. Labels and Scales
  labs(
    x = "AI Capability (κ)",
    y = "Correlation (%)",
    # Legend to explain the two metrics
    #caption = "Bars: Pearson Correlation | Diamonds: Spearman Rank Correlation"
  ) +
   scale_x_discrete(breaks = as.character(1:6)) +
  
  # 5. Theme
  theme_paper(base_size = 18) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(color = "grey30"),
    strip.background = element_rect(fill = "grey95", color = NA)
  )
p_corr_auer_eloundou
save_fig("corr_auer_eloundou", plot = p_corr_auer_eloundou, type = "wide")
save_fig(
  "pearson_correlation_aisa_vs_eloundou",
  plot = p_corr_auer_eloundou,
  type = "portrait"
)

#--------------------------------------------------------------###
# Figure: Scatter Plot Grid (AISA vs Eloundou) ####
#--------------------------------------------------------------###

# 1. Pre-filter the data once into a smaller object
plot_subset <- compare_df #%>% 
  #filter(`AI capability` %in% 1:6)

# 2. Pre-calculate the 10% sample (10% is usually plenty for visualization)
plot_sample <- plot_subset %>% 
  group_by(`AI capability`) %>% 
  slice_sample(prop = 0.3) %>% 
  ungroup()

p_comparison_eloundou_auer <- ggplot(
  plot_subset,
  aes(x = AISA, y = acem_exp)
) +
  geom_point(data = plot_sample, alpha = 0.2, color = "darkcyan", size = 0.8) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey30") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE, linewidth = 1) + 
  
  # Crucial: This forces X and Y to have the same limits and physical scale
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) + 
  
  facet_wrap(~ `AI capability`, ncol = 10) +
  labs(
    x = "Auer (2025) AISA Exposure (%)",
    y = "Eloundou (2024) Exposure (%)",
   # subtitle = "Dashed = Perfect Agreement; Red = Actual Trend"
  ) +
  theme_paper(base_size = 14)+
  theme(strip.background = element_rect(fill = "grey95", color = NA)
  )

p_comparison_eloundou_auer
save_fig(
  "comparison_eloundou_auer",
  plot = p_comparison_eloundou_auer,
  type = "wide",
  format = "png",
  dpi = 200
)


p_aisa_vs_eloundou <- ggplot(
  plot_subset %>% filter(`AI capability` %in% c(2.6, 3)),
  aes(x = AISA, y = acem_exp)
) +
  geom_point(data = plot_subset%>% filter(`AI capability`%in% c(2.6,3)), alpha = 0.2, color = "darkcyan", size = 0.8) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey30") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE, linewidth = 1) + 
  
  # Crucial: This forces X and Y to have the same limits and physical scale
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) + 
  
  facet_wrap(~ `AI capability`, ncol = 1, 
             labeller = as_labeller(function(x) paste("AI Capability (κ) =", x)))+
  labs(
    x = "Auer (2024) AISA Exposure (%)",
    y = "Eloundou (2024) Exposure (%)",
    # subtitle = "Dashed = Perfect Agreement; Red = Actual Trend"
  ) +
  theme_paper(base_size = 20)+
  theme(strip.background = element_rect(fill = "grey95", color = NA), 
        axis.title = element_text(color = "grey30")
  )
p_aisa_vs_eloundou
save_fig("aisa_vs_eloundou", plot = p_aisa_vs_eloundou, type = "portrait")




#---------------------------------------------------###
# Figure: Global Map of GDP Impact (Max AI Capability) ####
#---------------------------------------------------###
library(rnaturalearth)
library(rnaturalearthdata)

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica") %>%
  mutate(iso_a3 = case_when(
    admin == "France" ~ "FRA",
    admin == "Norway" ~ "NOR",
    admin == "South Korea" ~ "KOR",
    admin == "Greenland" ~ "DNK",  # <--- Forces Greenland to inherit Denmark's data
    TRUE ~ iso_a3
  ))

map_pal_steps <- met.brewer("Hiroshige")

make_gdp_map <- function(capability, breaks) {
  map_data <- gdp %>%
    filter(`AI capability` == capability) %>%
    select(REF_AREA, gdp_adj_internet)

  world_sf %>%
    left_join(map_data, by = c("iso_a3" = "REF_AREA")) %>%
    ggplot() +
    geom_sf(aes(fill = gdp_adj_internet), color = "white", linewidth = 0.2) +
    scale_fill_stepsn(
      colors = map_pal_steps,
      breaks = breaks,
      na.value = "#f0f0f0",
      name = "GDP Gain (%)",
      guide = guide_colorsteps(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 20,
        barheight = 0.5,
        frame.colour = "grey20"
      )
    ) +
    theme_void(base_size = 18) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(color = "grey30")
    )
}

p_world_map_current <- make_gdp_map(
  capability = 2.6,
  breaks = seq(0, 2.5, by = 0.5)
)
p_world_map_current
save_fig("world_map_k2-6", plot = p_world_map_current, type = "wide")

p_world_map_max <- make_gdp_map(
  capability = 6,
  breaks = seq(0, 10, by = 2)
)
p_world_map_max
save_fig("world_map", plot = p_world_map_max, type = "wide")





us_tfp_comparison <- gdp %>%
  filter(`Reference area`=="United States") %>% 
  transmute(`AI capability`, tfp_bas_bill_weighted*100, 
            tfp_acem_bill_weighted)


global_gdp_diagnostic <- gdp %>%
  group_by( `AI capability`) %>% 
  summarise(
    internet_use = weighted.mean(internet_use, w = gdp_2023, na.rm=T),
    tfp_bas_bill_weighted = weighted.mean(tfp_bas_bill_weighted, w = gdp_2023, na.rm=T),
    AISA_bill_weighted = weighted.mean(AISA_bill_weighted, w = gdp_2023, na.rm=T),
    AISA = weighted.mean(AISA, w = gdp_2023, na.rm=T),
    Lshare = weighted.mean(Lshare, w = gdp_2023, na.rm=T),
    gdp = sum(gdp, na.rm=T),
    .groups = "drop"
  ) %>% 
  
  mutate(gdp = 
           tfp_bas_bill_weighted * (1/(1-(1-Lshare))) * 100, 
         gdp_adj_internet = 
           tfp_bas_bill_weighted * (1/(1-(1-Lshare))) * 100 * internet_use)
  

gdp_final %>% 
  filter(is.na(highlight)==FALSE) %>% 
  transmute(`Country Name` = `Reference area`, gdp_adj_internet) %>% print()






#-----------------------------------------------------###
# Fig: GDP per capita data ####
# Prefer PPP GDP per capita if available
# World Bank indicator: NY.GDP.PCAP.PP.CD
#-----------------------------------------------------###

world_gdppc <- read_excel(
  "./data/API_NY.GDP.PCAP.PP.CD_DS2_en_excel_v2_273625.xls",
  skip = 3
) %>%
  as.data.frame() %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "gdppc_ppp"
  ) %>%
  mutate(year = as.integer(year)) %>%
  arrange(`Country Code`, desc(year)) %>%
  group_by(`Country Code`, `Country Name`) %>%
  summarise(
    gdppc_ppp = coalesce(
      gdppc_ppp[year == 2023],
      gdppc_ppp[!is.na(gdppc_ppp)][1]
    ),
    .groups = "drop"
  )

#-----------------------------------------------------###
# Data for distributional chart
#-----------------------------------------------------###

rich_country_gain <- gdp %>%
  filter(`AI capability` %in% c(2.6, 6)) %>%
  left_join(world_gdppc, by = c("REF_AREA" = "Country Code")) %>%
  filter(!is.na(gdppc_ppp), !is.na(gdp_adj_internet)) %>%
  mutate(
    capability_label = case_when(
      `AI capability` == 2.6 ~ "Current capabilities (\u03ba = 2.6)",
      `AI capability` == 6   ~ "Maximum capabilities (\u03ba = 6)"
    ),
    gdppc_ppp_log = log(gdppc_ppp),
    gdp_trn = gdp_2023 / 1e12
  )


#-----------------------------------------------------###
# Figure: richer countries gain more
#-----------------------------------------------------###

p_per_capita <- ggplot(rich_country_gain,
       aes(x = gdppc_ppp,
           y = gdp_adj_internet)) +
  
  geom_point(
    aes(size = gdp_trn, color = regions),
    alpha = 0.95
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "grey35",
    linewidth = 0.8
  ) +
  
  scale_x_log10(
    labels = scales::label_dollar()
  ) +
  
  scale_size_continuous(
    range = c(1.5, 9),
    name = "GDP, 2023\n(USD trn)"
  ) +
  
  scale_color_met_d("Hiroshige") +
  
  facet_wrap(~ capability_label, ncol = 2) +
  
  labs(
    x = "GDP per capita, PPP (log scale)",
    y = "GDP Gain (%)",
    color = "World region"
  ) +
  
  theme_paper(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

save_fig("per_capita", plot = p_per_capita, type = "standard")


rich_country_gain %>%
  group_by(`AI capability`) %>%
  do(
    broom::tidy(
      lm(gdp_adj_internet ~ log(gdppc_ppp),
         data = .,
         weights = gdp_2023)
    )
  ) %>%
  filter(term == "log(gdppc_ppp)")


# 
# income_bins <- rich_country_gain %>%
#   group_by(`AI capability`) %>%
#   mutate(
#     income_quartile = ntile(gdppc_ppp, 4),
#     income_group = case_when(
#       income_quartile == 1 ~ "Lowest income quartile",
#       income_quartile == 2 ~ "Second quartile",
#       income_quartile == 3 ~ "Third quartile",
#       income_quartile == 4 ~ "Highest income quartile"
#     )
#   ) %>%
#   group_by(`AI capability`, capability_label, income_group, income_quartile) %>%
#   summarise(
#     avg_gain = weighted.mean(gdp_adj_internet, w = gdp_2023, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# ggplot(income_bins,
#        aes(x = reorder(income_group, income_quartile),
#            y = avg_gain,
#            fill = capability_label)) +
#   
#   geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.85) +
#   
#   geom_text(
#     aes(label = sprintf("%.1f%%", avg_gain)),
#     position = position_dodge(width = 0.75),
#     vjust = -0.3,
#     size = 3.8
#   ) +
#   
#   scale_fill_met_d("Hiroshige") +
#   
#   labs(
#     x = NULL,
#     y = "GDP-weighted average AI-driven GDP gain (%)",
#     fill = NULL
#   ) +
#   
#   theme_paper(base_size = 14) +
#   theme(
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 20, hjust = 1)
#   )
