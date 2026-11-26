

bento1 = bento |> 
  filter(is.na(countrycode)==F) |> 
  mutate(size_us = bento |> 
           filter(countrycode =="USA") |> 
           pull(sizeNA) |> as.numeric() )|> 
  mutate(size_adjustment = log(sizeNA/size_us)) |> 
  select(countrycode, size_adjustment) |> 
  filter(is.na(size_adjustment)==F)
print(bento1)


library(countrycode)


world_gdppc = world_gdppc %>%
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




# Data for imputation
bento_data <- bento %>%
  select(countrycode, sizeNA, sizeM, sizeS) %>%
  distinct(countrycode, .keep_all = TRUE) %>%
  left_join(
    world_gdppc %>%
      transmute(countrycode = `Country Code`, gdppc_ppp),
    by = "countrycode"
  ) %>%
  mutate(
    continent = countrycode::countrycode(
      countrycode, "iso3c", "continent"
    )
  )

# Donors: only those countries with an observed aggregate
donors <- bento_data %>%
  filter(sizeNA > 0, gdppc_ppp > 0)



#Pro každou chybějící zemi vybereme pět donorů s nejbližším GDP per capita, 
#přednostně ze stejného kontinentu. Pokud máme sizeM nebo sizeS, použijeme 
#poměr mezi jejich sektorovou velikostí a agregátem u donorů. Pokud nemáme nic, 
#použijeme medián agregátní velikosti donorů.

impute_size <- function(cc, N, M, S, y, region, k = 5) {
  
  if (!is.na(N) && N > 0) return(N)
  if (is.na(y) || y <= 0) return(NA_real_)
  
  peers <- donors %>%
    filter(countrycode != cc)
  
  # Preferujeme srovnatelné země ze stejného kontinentu
  regional <- peers %>%
    filter(continent == region)
  
  if (nrow(regional) >= k) peers <- regional
  
  peers <- peers %>%
    slice_min(
      abs(log(gdppc_ppp / y)),
      n = k,
      with_ties = FALSE
    )
  
  estimates <- numeric(0)
  
  # Využijeme pozorovanou velikost manufacturing
  if (!is.na(M) && M > 0) {
    ratio <- log(peers$sizeNA / peers$sizeM)
    ratio <- ratio[is.finite(ratio)]
    
    if (length(ratio) > 0) {
      estimates <- c(estimates, log(M) + median(ratio))
    }
  }
  
  # Využijeme pozorovanou velikost services
  if (!is.na(S) && S > 0) {
    ratio <- log(peers$sizeNA / peers$sizeS)
    ratio <- ratio[is.finite(ratio)]
    
    if (length(ratio) > 0) {
      estimates <- c(estimates, log(S) + median(ratio))
    }
  }
  
  # Pokud nejsou sektorová data, použijeme donory
  if (length(estimates) == 0) {
    estimates <- median(log(peers$sizeNA))
  }
  
  exp(mean(estimates))
}

countries <- intersect(
  unique(value_added$REF_AREA),
  unique(labour_share$REF_AREA)
)


# fill only missing values
bento_final <- tibble(countrycode = countries) %>%
  left_join(bento_data, by = "countrycode") %>%
  mutate(
    imputed = is.na(sizeNA),
    N_c = pmap_dbl(
      list(countrycode, sizeNA, sizeM, sizeS,
           gdppc_ppp, continent),
      impute_size
    ),
    mu0_country = -1.75+log(
      N_c / N_c[countrycode == "USA"]
    ), 
    mu0_country_upd =  0.0189+log(
      N_c / N_c[countrycode == "USA"]
    )
  )


bento_final |> filter(is.na(N_c)==FALSE) |> group_by(imputed) |> summarise(n=n())

bento_final |>
filter(imputed, !is.na(N_c)) |>
pull(countrycode) |>
paste(collapse = ", ")

