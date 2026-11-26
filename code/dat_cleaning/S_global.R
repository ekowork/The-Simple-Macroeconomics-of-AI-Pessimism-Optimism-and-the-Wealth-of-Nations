

occupations_global_worldbank |> distinct(industry,industry_str)
occupations_global_worldbank |> distinct(year)

occupations_global_worldbank |> 
  filter(year ==2022) 
  


tiva_data |> distinct(`Economic activity`,ACTIVITY) |> 
  mutate(x = str_count(ACTIVITY)) |> 
filter(x==1) |> 
  arrange(ACTIVITY)




AISA_to_ISIC = dat_us |>
  mutate(soc = OCC_CODE) |> 
  left_join(crosswalk_isco_soc |> 
              mutate(soc = `2010 SOC Code`)) |> 
  mutate(occupation = substr(`ISCO-08 Code`,1,1)) |> 
  group_by(occupation,`AI capability`) |> 
  summarise(AISA_w = weighted.mean(AISA, w = wage_bill,na.rm=T), 
            AISA = mean(AISA,na.rm=T), 
            wage_bill_us = sum(wage_bill), 
            empl_us= sum(TOT_EMP)) |> 
  mutate(occupation = as.numeric(occupation))


xx = AISA_to_ISIC |> 
  left_join(occupations_global_worldbank |>
              filter(
                (iso3 == "IDN" & year == 2023) |
                  (iso3 != "IDN" & year == 2022)
              )) |> 
  filter(is.na(emp)==F) |> 
  mutate(wage_bill_est= emp*(wage_bill_us/empl_us)) |> 
  group_by(`AI capability`, iso3,industry) |>
  summarise(AISA = weighted.mean(AISA_w, w=wage_bill_est, na.rm=T ), 
            wage_bill_est = sum(wage_bill_est, na.rm=T)) |> 
  ungroup()
  


crosswalk_industry_tiva <- tibble(
  industry = c(
    11,21,22,22,23,31,42,48,51,52,
    53,54,56,61,62,71,72,81,92
  ),
  industry_agg = c(
    "A","B","D","E","F","C","G","H","J","K",
    "L","M","N","P","Q","R","I","S","O"
  )
)

stopifnot(
  setequal(
    paste(
      crosswalk_industry_tiva$industry,
      crosswalk_industry_tiva$industry_agg
    ),
    paste(
      c(11,21,22,22,23,31,42,48,51,52,53,54,56,61,62,71,72,81,92),
      c("A","B","D","E","F","C","G","H","J","K","L","M","N","P","Q","R","I","S","O")
    )
  )
)


xx = xx |> left_join(crosswalk_industry_tiva) |> 
  left_join(s_us |> distinct(tiva)  |> 
              mutate(industry_agg = substr(tiva,1,1)))

xx =xx |> 
  group_by(iso3,`AI capability`) |> 
  mutate(
    AISA = coalesce(
      AISA, mean(AISA, na.rm=T) )
    ) |> 
  ungroup()



s_country = xx |> 
  transmute(tiva, `AI capability`, wage_bill = wage_bill_est, AISA_bill_weighted = AISA, REF_AREA = iso3)



s_country_internet = s_country |> 
  left_join(world_internet |> 
              transmute(REF_AREA =`Country Code`, internet_use)) |> 
  mutate(AISA_bill_weighted = AISA_bill_weighted*internet_use) |> 
  select(-internet_use)
