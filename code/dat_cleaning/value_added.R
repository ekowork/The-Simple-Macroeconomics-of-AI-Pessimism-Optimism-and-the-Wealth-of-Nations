


#-----------------------------------------------------###.
# Prepare data for value added weights####
#-----------------------------------------------------###.

# Pre-filter TiVA for relevant records
tiva_filtered <- tiva_data %>%
  select(REF_AREA,`Reference area`,
         ACTIVITY, `Economic activity`, OBS_VALUE) %>%  #mill US dollars
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
  mutate(share_of_gdp = OBS_VALUE / total_value) %>%
  select(-c(Hierarchy_Level,ROWS))|> 
  rename(VALUE_ADDED = OBS_VALUE)

value_added = tiva_shares 