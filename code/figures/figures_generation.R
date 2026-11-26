


#--------------------------------------------------------------###
# Figure: US Labour Share by Industry ####
#--------------------------------------------------------------###

figure_labour_share_us <- function(save = FALSE) {

my_pal <- met.brewer("Hiroshige", n = 8)

hierarchy_descs <- hierarchy %>%
  select(ACTIVITY = Industry_Code, 
         `Economic activity` = Industry_Description, 
         Hierarchy_Level) 

agg_lshare_data <- tim_data %>% 
  mutate(country = REF_AREA) |> 
  filter(Measure=="Compensation of employees", 
         `Unit of measure`=="Percentage of value added", 
         TIME_PERIOD==2022, 
         country=="USA") %>% 
  select(country, ACTIVITY, OBS_VALUE) %>% 
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
print(p_labour_share_us)

if (save) {
  save_fig(
    "labour_share_us",
    plot = p_labour_share_us,
    type = "standard"
  )
}

}




#--------------------------------------------------------------###
# Figure: US Value Added by Industry ####
#--------------------------------------------------------------###


figure_value_added_us <- function(save = FALSE) {
  
  hierarchy_descs <- hierarchy %>%
    select(ACTIVITY = Industry_Code, 
           `Economic activity` = Industry_Description, 
           Hierarchy_Level) 
  
  
  # value added of USA
  tiva_data_chart = tiva_data %>%
    mutate(country = REF_AREA) |> 
    filter(TIME_PERIOD==2022, 
           country=="USA") %>% 
    select(country, ACTIVITY, OBS_VALUE) %>% 
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
  
  print(p_value_added_us)
  if (save) {
    save_fig(
      "value_added_us",
      plot = p_value_added_us,
      type = "standard"
    )
    
  }
  
}







#-----------------------------------#.
# Figure: two occupations AISA####
#---------------------------------###.

figure_p_occupations_aisa = function(save=FALSE){
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
  
  print(p_occupations_aisa)
  
  if (save) {
    save_fig(
      "occupations_aisa",
      plot = p_occupations_aisa,
      type = "standard"
    )
    
}


}




















  figure_p_tfp_growth_us <- function(save = FALSE) {
    
    #---------------------###
    # Figure: US TFP
    #---------------------###
    
    my_pal <- met.brewer("Hiroshige", n = 8)
    
    plot_data <- res_agg %>%
      filter(
        country == "USA",
        scenario %in% c(
          "baseline",
          "S_us, fix_lambda_iota",
          "baseline, upd_param"
        )
      ) %>%
      mutate(tfp_pct = tfp * 100)
    
    # Automatic y-axis and legend positioning
    y_max <- max(5, ceiling(max(plot_data$tfp_pct, na.rm = TRUE) * 1.0))
    
    p_tfp_growth_us <- ggplot() +
      
      # Future AI capabilities
      annotate("rect",
               xmin = 2.6, xmax = 6,
               ymin = -Inf, ymax = Inf,
               fill = my_pal[5], alpha = 0.2) +
      
      # Baseline: solid line
      geom_line(
        data = plot_data %>%
          filter(scenario == "baseline"),
        aes(x = kappa, y = tfp_pct),
        color = my_pal[8],
        linewidth = 1.2
      ) +
      
      geom_point(
        data = plot_data %>%
          filter(scenario == "baseline"),
        aes(x = kappa, y = tfp_pct),
        color = my_pal[8],
        size = 3
      ) +
      
      # Fixed adoption and savings: dots only
      geom_point(
        data = plot_data %>%
          filter(scenario == "S_us, fix_lambda_iota"),
        aes(x = kappa, y = tfp_pct),
        color = my_pal[8],
        size = 2
      ) +
      
      # Optimism: dotdash
      geom_line(
        data = plot_data %>%
          filter(scenario == "baseline, upd_param"),
        aes(x = kappa, y = tfp_pct),
        color = my_pal[8],
        linewidth = 2,
        linetype = 42
      ) +
      
      # Acemoglu reference
      geom_hline(
        yintercept = 0.71,
        linetype = "dotted",
        color = my_pal[1],
        linewidth = 1.5
      ) +
      
      annotate("text",
               x = 4.5, y = 1.05,
               label = "Acemoglu (2025), TFP (0.71%)",
               color = my_pal[1],
               size = 5,
               hjust = 0) +
      
      labs(
        x = "AI Capability (κ)",
        y = "TFP Gain (%)"
      ) +
      
      scale_x_continuous(
        breaks = 1:6,
        limits = c(1, 6),
        expand = c(0.01, 0.01)
      ) +
      
      scale_y_continuous(
        breaks = pretty(c(0, 10), n = 5),
        limits = c(0, 12)
      ) +
      
      theme_paper(base_size = 15) +
      theme(
        legend.position = "none",
        axis.title = element_text(color = "grey30"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey95")
      ) +
      
      # Manual legend — upper left corner
      annotate("rect",
               xmin = 1.02, xmax = 2.57,
               ymin = y_max * 0.83,
               ymax = y_max * 0.985,
               fill = "white",
               color = "grey75",
               linewidth = 0.4) +
      
      # Baseline
      annotate("segment",
               x = 1.10, xend = 1.32,
               y = y_max * 0.95, yend = y_max * 0.95,
               color = my_pal[8],
               linewidth = 1.6,
               linetype = "solid") +
      annotate("point",
               x = 1.21, y = y_max * 0.95,
               color = my_pal[8], size = 2) +
      annotate("text",
               x = 1.40, y = y_max * 0.95,
               label = "Matched to Acemoglu (Baseline)",
               hjust = 0, size = 4) +
      
      # Fixed adoption and savings: actual dots
      annotate("point",
               x = seq(1.10, 1.32, length.out = 6),
               y = rep(y_max * 0.90, 6),
               color = my_pal[8],
               size = 1.3) +
      annotate("text",
               x = 1.40, y = y_max * 0.90,
               label = "Fixed Adoption and Savings",
               hjust = 0, size = 4) +
      
      # Optimism
      annotate("segment",
               x = 1.10, xend = 1.32,
               y = y_max * 0.85, yend = y_max * 0.85,
               color = my_pal[8],
               linewidth = 1.6,
               linetype = "dotdash") +
      annotate("text",
               x = 1.40, y = y_max * 0.85,
               label = "Optimism",
               hjust = 0, size = 4)
    
    print(p_tfp_growth_us)
    
    if (save) {
      save_fig("tfp_growth_us",
               plot = p_tfp_growth_us,
               type = "wide")
    }
    
    invisible(p_tfp_growth_us)
  }






 
 
 
 
 
 
 
 
 





figure_p_gdp_growth_us <- function(save = FALSE) {
  
  #---------------------###
  # Figure: US GDP
  #---------------------###
  
  my_pal <- met.brewer("Hiroshige", n = 8)
  
  plot_data <- res_agg %>%
    filter(
      country == "USA",
      scenario %in% c(
        "baseline",
        "S_us, fix_lambda_iota",
        "baseline, upd_param"
      )
    )
  
  p_gdp_growth_us <- ggplot() +
    
    # Future AI capabilities
    annotate("rect",
             xmin = 2.6, xmax = 6,
             ymin = -Inf, ymax = Inf,
             fill = my_pal[5], alpha = 0.2) +
    
    # Baseline: solid line
    geom_line(
      data = plot_data %>%
        filter(scenario == "baseline"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      linewidth = 1.2
    ) +
    
    geom_point(
      data = plot_data %>%
        filter(scenario == "baseline"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      size = 3
    ) +
    
    # Fixed adoption and savings: dots only
    geom_point(
      data = plot_data %>%
        filter(scenario == "S_us, fix_lambda_iota"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      size = 2
    ) +
    
    # Optimism: dotdash
    geom_line(
      data = plot_data %>%
        filter(scenario == "baseline, upd_param"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      linewidth = 2,
      linetype = 42
    ) +
    
    # Acemoglu reference
    geom_hline(
      yintercept = 1.1,
      linetype = "dotted",
      color = my_pal[1],
      linewidth = 1.5
    ) +
    
    annotate("text",
             x = 4.5, y = 1.8,
             label = "Acemoglu (2025), GDP (1.1%)",
             color = my_pal[1],
             size = 5,
             hjust = 0) +
    
    labs(
      x = "AI Capability (κ)",
      y = "GDP Gain (%)"
    ) +
    
    scale_x_continuous(
      breaks = 1:6,
      limits = c(1, 6),
      expand = c(0.01, 0.01)
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 20, by = 5),
      limits = c(0, 21.8)
    ) +
    
    theme_paper(base_size = 15) +
    theme(
      legend.position = "none",
      axis.title = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95")
    ) +
    
    # Manual legend — upper left corner
    annotate("rect",
             xmin = 1.02, xmax = 2.57,
             ymin = 18.1, ymax = 21.5,
             fill = "white",
             color = "grey75",
             linewidth = 0.4) +
    
    # Baseline
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 20.8, yend = 20.8,
             color = my_pal[8],
             linewidth = 1.6,
             linetype = "solid") +
    annotate("point",
             x = 1.21, y = 20.8,
             color = my_pal[8], size = 2) +
    annotate("text",
             x = 1.40, y = 20.8,
             label = "Matched to Acemoglu (Baseline)",
             hjust = 0, size = 4) +
    
    # Fixed adoption and savings: actual dots
    annotate("point",
             x = seq(1.10, 1.32, length.out = 6),
             y = rep(19.8, 6),
             color = my_pal[8],
             size = 1.3) +
    annotate("text",
             x = 1.40, y = 19.8,
             label = "Fixed Adoption and Savings",
             hjust = 0, size = 4) +
    
    # Optimism
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 18.8, yend = 18.8,
             color = my_pal[8],
             linewidth = 1.6,
             linetype = "dotdash") +
    annotate("text",
             x = 1.40, y = 18.8,
             label = "Optimism",
             hjust = 0, size = 4)
  
  print(p_gdp_growth_us)
  
  if (save) {
    save_fig("gdp_growth_us",
             plot = p_gdp_growth_us,
             type = "wide")
  }
  
  invisible(p_gdp_growth_us)
}



figure_p_param_decomp = function(save = F){
  
  
  #USA ONLY ON INDUSTRIES and a few industries
  plot_data <- res_i %>%
    filter(scenario %in% c("baseline", "baseline, upd_param", 
                           "S_us, fix_lambda_iota" )) |> 
    filter(country== "USA") |> 
    
    pivot_longer(
      cols = c(S, iota, lambda),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    
    mutate(
      parameter = factor(
        parameter,
        levels = c("S", "iota", "lambda"),
        labels = c("Exposure \n(S)", "Adoption share \n(\u03B9)", 
                   "Savings \n(\u03BB)")
      ),
      
      scenario = factor(
        scenario,
        levels = c(
          "baseline",
          "baseline, upd_param",
          "S_us, fix_lambda_iota"
        ),
        labels = c(
          "Matched to Acemoglu (Baseline)",
          "Optimism",
          "Fixed Adoption and Savings"
        )
      )
    ) |> 
    
    left_join(world_gdp,
              by = join_by("country" == "Country Code")) %>%
    filter(!is.na(gdp_2023), gdp_2023 > 0) %>%
    group_by(scenario, kappa, parameter) %>%
    summarise(
      p10 = quantile(value, 0.1, na.rm = TRUE),
      p50 = median(value, na.rm = TRUE),
      p50w = weighted.mean(value, w = VALUE_ADDED, na.rm = TRUE),
      p90 = quantile(value, 0.9, na.rm = TRUE),
      .groups = "drop"
    ) 
  
  p_gdp_growth_us = ggplot(plot_data, aes(x = kappa, y = p50)) +
    annotate("rect",
             xmin = 2.6, xmax = 6,
             ymin = -Inf, ymax = Inf,
             fill = my_pal[5], alpha = 0.2) +
    geom_ribbon(
      aes(ymin = p10, ymax = p90),
      fill = "gray", alpha = .4
    ) +
    geom_line(color = "#1E466E", linewidth = 0.9) +
    geom_line(aes(x = kappa, y = p50w), color = "gray60", 
              linetype="dashed", linewidth = 0.9) +
    facet_grid(parameter ~ scenario)+
    labs(
      x = "AI Capability (κ)",
      y = "Parameter value"
    ) +
    theme_paper(base_size = 15) +
    theme(
      strip.text = element_text(size = 14,
                                family = "Latin Modern Math"),
      axis.title = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95")
    )
  
  
  print(p_gdp_growth_us)
  
  if (save) {
    save_fig("param_decomp_us",
             plot = p_gdp_growth_us,
             type = "wide")
  }
  #
  
}


















figure_p_gdp_growth_world <- function(save = FALSE) {
  
  #---------------------###
  # Figure: WORLD GDP
  #---------------------###
  
  my_pal <- met.brewer("Hiroshige", n = 8)
  
  plot_scenarios <-  c(
    "baseline",
    "S_us, fix_lambda_iota",
    "baseline, upd_param", 
    "S_us, mu0_c",
    "S_us, mu0_c, upd_param"
  )
  
  # GDP-weighted aggregation across countries
  global_gdp_by_capability <- res_agg %>%
    filter(scenario %in% plot_scenarios) %>%
    left_join(
      world_gdp %>% select(`Country Code`, gdp_2023),
      by = c("country" = "Country Code")
    ) %>%
    filter(!is.na(gdp), !is.na(gdp_2023), gdp_2023 > 0) %>%
    group_by(kappa, scenario) %>%
    summarise(
      gdp = weighted.mean(gdp, w = gdp_2023),
      .groups = "drop"
    )
  
  p_gdp_growth_world <-
    ggplot() +
    
    # Future AI capabilities
    annotate("rect",
             xmin = 2.6, xmax = 6,
             ymin = -Inf, ymax = Inf,
             fill = my_pal[5], alpha = 0.2) +
    
    # Baseline: solid line
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "baseline"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      linewidth = .6
    ) +
    
    geom_point(
      data = global_gdp_by_capability %>%
        filter(scenario == "baseline"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      size = 1.5
    ) +
    
    #mu0_c solid line
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_us, mu0_c"),
      aes(x = kappa, y = gdp),
      color = my_pal[1],
      linewidth = 1.2
    ) +
    
    geom_point(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_us, mu0_c"),
      aes(x = kappa, y = gdp),
      color = my_pal[1],
      size = 3
    ) +
    
    # Fixed adoption and savings: dots only
    geom_point(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_us, fix_lambda_iota"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      size = 1
    ) +
    
    # Optimism: common deployment affordability: dotdash
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "baseline, upd_param"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      linewidth = 1,
      linetype = "dotdash"
    ) +
    
    # mu0_c Optimism: common deployment affordability: dotdash
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_us, mu0_c, upd_param"),
      aes(x = kappa, y = gdp),
      color = my_pal[1],
      linewidth = 2,
      linetype = "dotdash"
    ) +
    
    labs(
      x = "AI Capability (κ)",
      y = "GDP Gain (%)"
    ) +
    
    scale_x_continuous(
      breaks = 1:6,
      limits = c(1, 6),
      expand = c(0.01, 0.01)
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 20, by = 5),
      limits = c(0, 21.8)
    ) +
    
    theme_paper(base_size = 15) +
    theme(
      #legend.position = "none",
      axis.title = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95")
    ) +
    
    # Manual legend
    annotate("rect",
             xmin = 1.02, xmax = 3.50,
             ymin = 16.2, ymax = 21.5,
             fill = "white",
             color = "grey75",
             linewidth = 0.4) +
    
    # Common deployment affordability
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 20.8, yend = 20.8,
             color = my_pal[8],
             linewidth = 1.6) +
    annotate("point",
             x = 1.21, y = 20.8,
             color = my_pal[8], size = 2) +
    annotate("text",
             x = 1.40, y = 20.8,
             label = "US Adoption Conditions",
             hjust = 0, size = 4) +
    
    # Fixed adoption and savings
    annotate("point",
             x = seq(1.10, 1.32, length.out = 6),
             y = rep(19.8, 6),
             color = my_pal[8],
             size = 1.3) +
    annotate("text",
             x = 1.40, y = 19.8,
             label = "Fixed Adoption and Savings",
             hjust = 0, size = 4) +
    
    # Optimism: common deployment affordability
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 18.8, yend = 18.8,
             color = my_pal[8],
             linewidth = 1.6,
             linetype = "dotdash") +
    annotate("text",
             x = 1.40, y = 18.8,
             label = "Optimism: US Adoption Conditions",
             hjust = 0, size = 4) +
    
    # Country-specific mu0
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 17.8, yend = 17.8,
             color = my_pal[1],
             linewidth = 1.6) +
    annotate(
      "text",
      x = 1.40, y = 17.8,
      family = "Latin Modern Math",
      label = "Matched to Acemoglu (Baseline)",
      hjust = 0, size = 4
    ) +
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 16.8, yend = 16.8,
             color = my_pal[1], linewidth = 1.6,
             linetype = "dotdash") +
    annotate("text",
             x = 1.40, y = 16.8,
             label = "Optimism", hjust = 0, size = 4)
  
  print(p_gdp_growth_world)
  
  if (save) {
    save_fig("gdp_growth_world",
             plot = p_gdp_growth_world,
             type = "wide")
  }
  
  invisible(global_gdp_by_capability)
}






  
  figure_p_gdp_growth_world_all <- function(save = FALSE) {
    
    #---------------------###
    # Figure: WORLD GDP ALL 
    #---------------------###
    
    my_pal <- met.brewer("Hiroshige", n = 8)
    
    plot_scenarios <- c(
      "baseline" = "US Adoption Conditions",
      "S_us, fix_lambda_iota" = "Fixed Adoption and Savings",
      "baseline, upd_param" = "Optimism: US Adoption Conditions",
      "S_c, mu0_c" = "Country-Specific Exposure",
      "S_c, mu0_c, internet" = "Country-Specific Exposure with Internet Adjustment",
      "S_us, mu0_c" = "Matched to Acemoglu (Baseline)",
      "S_us, fix_lambda_iota_upd" = "Fixed Adoption and Savings (Higher Adoption)"
    )
    
    
    
    # plot_scenarios <- c(
    #   "baseline" = "US Adoption Conditions",
    #   "S_c, mu0_c" = "Country-Specific Exposure",
    #   "S_c, mu0_c, internet" = "Country-Specific Exposure with Internet Adjustment",
    #   "S_us, mu0_c" = "Matched to Acemoglu (Baseline)"
    # )
    # 
    # 
    
    # GDP-weighted aggregation across countries
    global_gdp_by_capability <- res_agg %>%
      filter(scenario %in% names(plot_scenarios)) |> 
      left_join(
        world_gdp %>% select(`Country Code`, gdp_2023),
        by = c("country" = "Country Code")
      ) %>%
      filter(!is.na(gdp), !is.na(gdp_2023), gdp_2023 > 0) %>%
      group_by(kappa, scenario) %>%
      summarise(
        gdp = weighted.mean(gdp, w = gdp_2023),
        .groups = "drop"
      ) %>%
      mutate(
        scenario_label = factor(
          scenario,
          levels = names(plot_scenarios),
          labels = unname(plot_scenarios)
        )
      )
    
    # Colours and linetypes
    scenario_colors <- setNames(
      my_pal[c(8, 8, 8, 1, 2, 4, 6, 4)],
      unname(plot_scenarios)
    )
    
    scenario_lines <- setNames(
      c("solid", "blank", "42", "dashed",
        "longdash", "twodash", "dotdash", "dotdash"),
      unname(plot_scenarios)
    )
    
    y_max <- ceiling(
      max(global_gdp_by_capability$gdp, na.rm = TRUE) / 5
    ) * 5
    
    p_gdp_growth_world_all <- ggplot(
      global_gdp_by_capability,
      aes(x = kappa, y = gdp, color = scenario_label)
    ) +
      
      # Future AI capabilities
      annotate("rect",
               xmin = 2.6, xmax = 6,
               ymin = -Inf, ymax = Inf,
               fill = my_pal[5], alpha = 0.2) +
      
      # All scenarios except fixed margins
      geom_line(
        data = ~ filter(
          .x, scenario != "S_us, fix_lambda_iota"
        ),
        aes(linetype = scenario_label),
        linewidth = 1.2
      ) +
      
      # Baseline points
      geom_point(
        data = ~ filter(.x, scenario == "baseline"),
        size = 3
      ) +
      
      # Fixed adoption and savings: points only
      geom_point(
        data = ~ filter(
          .x, scenario == "S_us, fix_lambda_iota"
        ),
        size = 2
      ) +
      
      scale_color_manual(
        values = scenario_colors,
        name = NULL
      ) +
      
      scale_linetype_manual(
        values = scenario_lines,
        guide = "none"
      ) +
      
      labs(
        x = "AI Capability (κ)",
        y = "GDP Gain (%)"
      ) +
      
      scale_x_continuous(
        breaks = 1:6,
        limits = c(1, 6),
        expand = c(0.01, 0.01)
      ) +
      
      scale_y_continuous(
        breaks = seq(0, y_max-5, by = 5),
        limits = c(0, y_max-4.5)
      ) +
      
      theme_paper(base_size = 15) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key.width = unit(1.5, "cm"),
        axis.title = element_text(color = "grey30"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey95")
      ) +
      
      guides(
        color = guide_legend(
          ncol = 2,
          byrow = TRUE
        )
      )
    
    print(p_gdp_growth_world_all)
    
    if (save) {
      save_fig(
        "gdp_growth_world_all",
        plot = p_gdp_growth_world_all,
        type = "wide"
      )
      
      print(global_gdp_by_capability |>
              filter(kappa %in% c(2.6,6)) |> 
              pivot_wider(names_from = kappa, values_from = gdp)
      )
    }
    
    invisible(global_gdp_by_capability)
  }
  
  
  
  
  
  
gdp_growth_world_adj <- function(save = FALSE) {
    
  
  #---------------------###
  # Figure: WORLD GDP - ADJ
  #---------------------###
  
  my_pal <- met.brewer("Hiroshige", n = 8)
  

  plot_scenarios <- c(
    "baseline",
    "S_c, mu0_c",
    "S_c, mu0_c, internet",
    "S_us, mu0_c"
  )
  
  # GDP-weighted aggregation across countries
  global_gdp_by_capability <- res_agg %>%
    filter(scenario %in% plot_scenarios) %>%
    left_join(
      world_gdp %>% select(`Country Code`, gdp_2023),
      by = c("country" = "Country Code")
    ) %>%
    filter(!is.na(gdp), !is.na(gdp_2023), gdp_2023 > 0) %>%
    group_by(kappa, scenario) %>%
    summarise(
      gdp = weighted.mean(gdp, w = gdp_2023),
      .groups = "drop"
    )
  
  p_gdp_growth_world <-
    ggplot() +
    
    # Future AI capabilities
    annotate("rect",
             xmin = 2.6, xmax = 6,
             ymin = -Inf, ymax = Inf,
             fill = my_pal[5], alpha = 0.2) +
    
    # Baseline: solid line
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "baseline"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      linewidth = .6
    ) +
    
    geom_point(
      data = global_gdp_by_capability %>%
        filter(scenario == "baseline"),
      aes(x = kappa, y = gdp),
      color = my_pal[8],
      size = 1.5
    ) +
    
    #mu0_c solid line
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_us, mu0_c"),
      aes(x = kappa, y = gdp),
      color = my_pal[1],
      linewidth = 1.2
    ) +
    
    geom_point(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_us, mu0_c"),
      aes(x = kappa, y = gdp),
      color = my_pal[1],
      size = 3
    ) +
    
    
    # S_c m0c
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_c, mu0_c"),
      aes(x = kappa, y = gdp),
      color = my_pal[6],
      linewidth = 1,
      linetype = "dotdash"
    ) +
    
    # S_c m0c internet
    geom_line(
      data = global_gdp_by_capability %>%
        filter(scenario == "S_c, mu0_c, internet"),
      aes(x = kappa, y = gdp),
      color = my_pal[3],
      linewidth = 1,
      linetype = "dotdash"
    ) +
    
    labs(
      x = "AI Capability (κ)",
      y = "GDP Gain (%)"
    ) +
    
    scale_x_continuous(
      breaks = 1:6,
      limits = c(1, 6),
      expand = c(0.01, 0.01)
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 10, by = 2.5),
      limits = c(0, 10)
    ) +
    
    theme_paper(base_size = 15) +
    theme(
      #legend.position = "none",
      axis.title = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95")
    ) +
    
    # Manual legend
    annotate("rect",
             xmin = 1.02, xmax = 3.60,
             ymin = 6.9, ymax = 9.5,
             fill = "white",
             color = "grey75",
             linewidth = 0.4) +
    
    # Common deployment affordability
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 9.15, yend = 9.15,
             color = my_pal[8],
             linewidth = 1.6) +
    annotate("point",
             x = 1.21, y = 9.15,
             color = my_pal[8], size = 2) +
    annotate("text",
             x = 1.40, y = 9.15,
             label = "US Adoption Conditions",
             hjust = 0, size = 4) +
    
    # Country-specific mu0 (red)
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 8.55, yend = 8.55,
             color = my_pal[1],
             linewidth = 1.6) +
    annotate("point",
             x = 1.21, y = 8.55,
             color = my_pal[1], size = 2) +
    annotate("text",
             x = 1.40, y = 8.55,
             family = "Latin Modern Math",
             label = "Matched to Acemoglu (Baseline)",
             hjust = 0, size = 4) +
    
    # Country-specific mu0 and S
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 7.95, yend = 7.95,
             color = my_pal[6],
             linewidth = 1.6,
             linetype = "dotdash") +
    annotate("text",
             x = 1.40, y = 7.95,
             family = "Latin Modern Math",
             label = "Country-Specific Exposure",
             hjust = 0, size = 4) +
    
    # Country-specific mu0, S and internet adjustment
    annotate("segment",
             x = 1.10, xend = 1.32,
             y = 7.35, yend = 7.35,
             color = my_pal[3],
             linewidth = 1.6,
             linetype = "dashed") +
    annotate("text",
             x = 1.40, y = 7.35,
             family = "Latin Modern Math",
             label = "Country-Specific Exposure with Internet Adjustment",
             hjust = 0, size = 4)
  
  print(p_gdp_growth_world)
  
    
    if (save) {
      save_fig(
        "gdp_growth_world_adj",
        plot = p_gdp_growth_world,
        type = "wide"
      )
    }
    
    invisible(global_gdp_by_capability)
    print(global_gdp_by_capability |>
            filter(kappa %in% c(2.6,6)) |> 
            pivot_wider(names_from = kappa, values_from = gdp)
          )
  }
  


figure_p_gdp_growth_gdpppp = function(save = FALSE){
  
  #-----------------------------------------------------###
  # Prepare data for the figure
  #-----------------------------------------------------###
  
  plot_data <-  res_agg %>%
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
    )
  
  #-----------------------------------------------------###
  # Country labels and their positions
  #-----------------------------------------------------###
  
  label_positions <- tibble::tibble(
    country = rep(
      c("USA", "CHN", "IND", "DEU", "JPN", "BRA", "EGY"),
      times = 2
    ),
    kappa = rep(c(2.6, 6), each = 7),
    
    highlight = rep(
      c("United States", "China", "India","Germany", "Japan", "Brazil", "Egypt"),
      times = 2
    ),
    
    # Multiplicative shift because the x-axis is logarithmic
    x_label_shift = c(
      1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,  # κ = 2.6
      1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00   # κ = 6
    ),
    
    y_label_shift = c(
      0.50,  0.40, -0.30,  0.30, -0.40, -0.30,  0.25,
      4.00,  1.40, -2.20,  2.00,  0.80, -2.50, -.80
    )
  )
  
  plot_data <- plot_data %>%
    left_join(label_positions, by = join_by(country, kappa)) %>%
    mutate(
      x_label = gdppc_ppp * coalesce(x_label_shift, 1),
      y_label = gdp + coalesce(y_label_shift, 0)
    )
  
  #-----------------------------------------------------###
  # Calculate beta for each capability level
  #-----------------------------------------------------###
  
  beta_labels <- plot_data %>%
    group_by(kappa, capability_label) %>%
    summarise(
      beta = unname(coef(lm(gdp ~ log(gdppc_ppp)))[2]),
      .groups = "drop"
    ) %>%
    mutate(
      label = sprintf("β = %.3f", beta),
      x = 2500,
      y = case_when(
        kappa == 2.6 ~ 0.8,
        kappa == 6   ~ 8.5
      )
    )
  
  #-----------------------------------------------------###
  # Figure: GDP gains versus GDP per capita
  #-----------------------------------------------------###
  
  p_per_capita <- ggplot(
    plot_data,
    aes(x = gdppc_ppp, y = gdp)
  ) +
    
    geom_segment(
      data = plot_data %>% filter(!is.na(highlight)),
      aes(
        x = gdppc_ppp,
        y = gdp,
        xend = x_label,
        yend = y_label
      ),
      inherit.aes = FALSE,
      color = "grey75",
      linewidth = 0.35
    ) +
    
    # Country observations
    geom_point(
      aes(size = gdp_trn, color = regions),
      alpha = 0.95
    ) +
    
    geom_text(
      data = plot_data %>% filter(!is.na(highlight)),
      aes(
        x = x_label,
        y = y_label,
        label = highlight
      ),
      inherit.aes = FALSE,
      size = 3.8,
      color = "grey20",
      family = "Latin Modern Roman",
      hjust = 0.5,
      vjust = -0.15
    ) +
    
    # Country-specific OLS
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "grey35",
      linewidth = 0.8
    ) +
    
    # Logarithmic x-axis
    scale_x_continuous(
      transform = "log",
      breaks = c(1000, 3000, 10000, 30000, 100000),
      labels = scales::label_dollar()
    )+
    
    # Beta coefficients: one per facet
    geom_text(
      data = beta_labels,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 5,
      fontface = "bold",
      color = "black",
      hjust = 0,
      vjust = 0.5,
      family = "Latin Modern Math"
    ) +
    # GDP bubble sizes
    scale_size_continuous(
      range = c(1.5, 9),
      name = "GDP, 2023\n(USD trn)"
    ) +
    
    # Regional colours
    scale_color_met_d("Hiroshige") +
    
    # Facets by AI capability
    facet_wrap(
      ~ capability_label,
      ncol = 2,
      scales = "free_y"
    ) +
    
    # Different y-axis scales
    ggh4x::facetted_pos_scales(
      y = list(
        scale_y_continuous(
          limits = c(0, 2.25),
          breaks = seq(0, 2, 0.5)
        ),
        scale_y_continuous(
          limits = c(0, 15),
          breaks = seq(0, 15, 5)
        )
      )
    ) +
    
    labs(
      x = "GDP per capita, PPP (log scale)",
      y = "GDP Gain (%)",
      color = "World region"
    ) +
    
    theme_paper(base_size = 14) +
    
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 14, face = "bold",
                                family = "Latin Modern Math"),
      strip.background = element_rect(
        fill = "grey95",
        color = NA
      )
    )
  
  print(p_per_capita)
  
  if (save) {
    
    ggsave(
      "figures/gdp_per_capita.pdf",
      plot = p_per_capita,
      width = 13,
      height = 7.5,
      units = "in"
    )
    
    
    # save_fig(
    #   "gdp_per_capita",
    #   plot = p_per_capita,
    #   type = "wide"
    # )
  }
  
  invisible(plot_data)
}
  






figure_p_industry_decomposition = function(save=F){
  
  
  
  #-----------------------------------------------------#.
  # Industry contributions
  #-----------------------------------------------------#.
  
  decomp_rows <- res_i %>%
    filter(
      kappa == 2.6,
      scenario == "S_us, mu0_c",
      !is.na(tfp),
      VALUE_ADDED > 0
    ) %>%
    left_join(
      res_agg %>%
        filter(
          kappa == 2.6,
          scenario == "S_us, mu0_c"
        ) %>%
        select(country, gdp),
      by = "country"
    ) %>%
    filter(!is.na(gdp)) %>%
    group_by(country) %>%
    mutate(
      country_va = sum(VALUE_ADDED),
      country_tfp = weighted.mean(tfp, VALUE_ADDED)
    ) %>%
    ungroup() %>%
    mutate(
      row_va_share = VALUE_ADDED / sum(VALUE_ADDED),
      row_contribution = row_va_share * tfp * (gdp / country_tfp)
    )
  
  decomp_rows <- decomp_rows %>%
    mutate(broad_code = substr(tiva, 1, 1)) %>%
    mutate(
      activity_section = str_sub(tiva, 1, 1),
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
        activity_section %in% c("U","T") ~ "Household & extraterritorial activities",
        TRUE ~ NA
      )
    ) |> 
    filter(
      !is.na(broad_industry),
      !str_detect(
        broad_industry,
        regex("household|extraterritorial", ignore_case = TRUE)
      )
    )
  
  
  #-----------------------------------------------------#.
  # Aggregate by broad industry
  #-----------------------------------------------------#.
  
  
  
  broad_decomp <- decomp_rows %>%
    group_by(broad_industry) %>%
    summarise(
      global_contribution = sum(row_contribution),
      global_va_share = sum(row_va_share),
      mean_S = weighted.mean(S, row_va_share),
      mean_iota = weighted.mean(iota, row_va_share),
      mean_lambda = weighted.mean(lambda, row_va_share),
      mean_L = weighted.mean(Lshare, row_va_share),
      .groups = "drop"
    )
  
  
  driver_heatmap <- broad_decomp %>%
    transmute(
      broad_industry,
      `AI exposure` = mean_S * 100,
      `Implementation` = mean_iota * 100,
      `Efficiency gain` = mean_lambda * 100,
      `Labour share` = mean_L * 100,
      `Global VA share` = global_va_share * 100
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
          "Implementation",
          "Efficiency gain",
          "Labour share",
          "Global VA share"
        )
      ),
      label = sprintf("%.1f", value)
    )
  
  #-----------------------------------------------------###.
  # Left panel: contribution
  #-----------------------------------------------------###.
  
  n_industries <- n_distinct(broad_decomp$broad_industry)
  industry_order <- broad_decomp %>%
    arrange(global_contribution) %>%
    pull(broad_industry) %>%
    as.character()
  
  hiro_cols <- met.brewer(
    name = "Hiroshige",
    n = n_industries,
    type = "continuous"
  )
  sum(broad_decomp$global_contribution)
  p_contribution <- ggplot(
    broad_decomp ,
    aes(
      x = global_contribution,
      fill = reorder(broad_industry,global_contribution),
      y = factor(broad_industry, levels = industry_order)
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
  p_contribution
  
  
  #-----------------------------------------------------###.
  # Right panel: driver heatmap
  #-----------------------------------------------------###.
  
  z_lim <- max(abs(driver_heatmap$z_value), na.rm = TRUE)
  
  p_drivers <- ggplot(
    driver_heatmap,
    aes(
      x = driver,
      y = factor(broad_industry, levels = industry_order),
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
      name = "Relative to Other Industries \n(Standard Deviation from Mean)",
      labels = function(x) ifelse(
        x > 0,
        sprintf("+%.1f", x),
        sprintf("%.1f", x)
      )
    ) +
    
    guides(
      fill = guide_colorbar(
        barwidth = unit(7, "cm"),
        barheight = unit(0.5, "cm"),
        title.position = "top"
      )
    ) +
    scale_x_discrete(
      breaks = c(
        "AI exposure",
        "Implementation",
        "Efficiency gain",
        "Labour share",
        "Global VA share"
      ),
      labels = expression(
        atop("Exposure", "(S)"),
        atop("Adoption share", "(" * iota * ")"),
        atop("Savings", "(" * lambda * ")"),
        atop("Labour share", "(L)"),
        atop("Value Added", "Share")
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
      legend.title = element_text(
        size = 10.5,
        hjust = 0.5
      ),
      legend.box.spacing = unit(0, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.position = "bottom",
      legend.text = element_text(size = 9.5),
      plot.margin = margin(5.5, 5.5, 5.5, 0),
      axis.text.x = element_text(
        family = "Latin Modern Math",
        size = 10.5,
        hjust = 0.5
      )
    )
  
  
  p_industry_decomposition <-
    p_contribution + p_drivers + patchwork::plot_layout(widths = c(1.15, 1.35))
  
  print(p_industry_decomposition)
  
  if (save) {
    save_fig(
      "industry_decomposition",
      plot = p_industry_decomposition,
      type = "standard"
    )
  }
  
}



figure_p_industry_ppp_decomposition1 <- function(save = FALSE) {
  
  #-----------------------------------------------------#
  # Industry contributions
  #-----------------------------------------------------#
  
  decomp_rows <- res_i %>%
    filter(
      kappa %in% c(2.6, 6),
      scenario == "S_us, mu0_c",
      !is.na(tfp),
      VALUE_ADDED > 0
    ) %>%
    inner_join(
      res_agg %>%
        filter(
          kappa %in% c(2.6, 6),
          scenario == "S_us, mu0_c"
        ) %>%
        select(country, kappa, gdp),
      by = c("country", "kappa")
    ) %>%
    group_by(kappa, country) %>%
    mutate(
      country_va = sum(VALUE_ADDED),
      country_tfp = weighted.mean(tfp, VALUE_ADDED),
      row_contribution =
        (VALUE_ADDED / country_va) *
        tfp * (gdp / country_tfp)
    ) %>%
    ungroup() %>%
    mutate(
      section = substr(tiva, 1, 1),
      broad_industry = case_when(
        section == "A" ~ "Agriculture",
        section %in% c("B", "D", "E") ~ "Mining & utilities",
        section == "C" ~ "Manufacturing",
        section == "F" ~ "Construction",
        section %in% c("G", "H", "I") ~ "Trade, transport & hospitality",
        section == "J" ~ "Information & communication",
        section == "K" ~ "Finance & insurance",
        section == "L" ~ "Real estate",
        section %in% c("M", "N") ~ "Professional & administrative services",
        section %in% c("O", "P", "Q") ~ "Public, education & health",
        section %in% c("R", "S") ~ "Arts, recreation & personal services",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(broad_industry))
  
  #-----------------------------------------------------#
  # Country income quartiles
  #-----------------------------------------------------#
  
  country_quartiles <- decomp_rows %>%
    distinct(country) %>%
    inner_join(
      world_gdppc %>%
        transmute(
          country = `Country Code`,
          gdppc_ppp
        ),
      by = "country"
    ) %>%
    filter(is.finite(gdppc_ppp)) %>%
    mutate(
      income_quartile = factor(
        ntile(gdppc_ppp, 4),
        levels = 1:4,
        labels = c("Q1", "Q2", "Q3", "Q4")
      )
    )
  
  decomp_rows <- decomp_rows %>%
    inner_join(
      country_quartiles %>%
        select(country, income_quartile),
      by = "country"
    )
  
  #-----------------------------------------------------#
  # Value-added-weighted industry contributions
  #-----------------------------------------------------#
  
  quartile_weights <- decomp_rows %>%
    distinct(kappa, income_quartile, country, country_va) %>%
    group_by(kappa, income_quartile) %>%
    summarise(
      quartile_va = sum(country_va),
      .groups = "drop"
    )
  
  broad_decomp <- decomp_rows %>%
    group_by(kappa, income_quartile, broad_industry) %>%
    summarise(
      weighted_contribution = sum(
        row_contribution * country_va
      ),
      .groups = "drop"
    ) %>%
    left_join(
      quartile_weights,
      by = c("kappa", "income_quartile")
    ) %>%
    mutate(
      gdp = weighted_contribution / quartile_va
    )
  
  #-----------------------------------------------------#
  # Difference between Q4 and Q1
  #-----------------------------------------------------#
  
  gap_decomp <- broad_decomp %>%
    filter(income_quartile %in% c("Q1", "Q4")) %>%
    select(kappa, broad_industry, income_quartile, gdp) %>%
    mutate(income_quartile = as.character(income_quartile)) %>%
    pivot_wider(
      names_from = income_quartile,
      values_from = gdp,
      values_fill = 0
    ) %>%
    mutate(
      gap_pp = Q4 - Q1
    )
  
  #-----------------------------------------------------#
  # Industry classification
  #-----------------------------------------------------#
  
  service_industries <- c(
    "Trade, transport & hospitality",
    "Information & communication",
    "Finance & insurance",
    "Real estate",
    "Professional & administrative services",
    "Public, education & health",
    "Arts, recreation & personal services"
  )
  
  gap_decomp <- gap_decomp %>%
    mutate(
      industry_group = if_else(
        broad_industry %in% service_industries,
        "Services",
        "Non-service industries"
      )
    )
  
  #-----------------------------------------------------#
  # Order industries by gap at kappa = 6
  #-----------------------------------------------------#
  
  industry_order <- gap_decomp %>%
    filter(kappa == 6) %>%
    arrange(gap_pp) %>%
    pull(broad_industry)
  
  #-----------------------------------------------------#
  # Total gap between Q4 and Q1
  #-----------------------------------------------------#
  
  gap_totals <- gap_decomp %>%
    group_by(kappa) %>%
    summarise(
      total_gap = sum(gap_pp),
      .groups = "drop"
    )
  
  gap_current <- gap_totals %>%
    filter(kappa == 2.6) %>%
    pull(total_gap)
  
  gap_top <- gap_totals %>%
    filter(kappa == 6) %>%
    pull(total_gap)
  
  gap_decomp <- gap_decomp %>%
    mutate(
      broad_industry = factor(
        broad_industry,
        levels = industry_order
      ),
      kappa = factor(
        kappa,
        levels = c(2.6, 6),
        labels = c(
          sprintf(
            "(a) Current AI Frontier (κ = 2.6)",
            gap_current
          ),
          sprintf(
            "(b) Top of the Capability Scale (κ = 6)",
            gap_top
          )
        )
      ),
      value_label = if_else(
        grepl("^\\(a\\)", as.character(kappa)),
        sprintf("%+.3f", gap_pp),
        sprintf("%+.2f", gap_pp)
      ),
      label_hjust = if_else(gap_pp >= 0, -0.15, 1.15)
    )
  
  
  hiro_cols <- met.brewer(
    name = "Hiroshige"
  )
  
  #-----------------------------------------------------#
  # Print underlying decomposition
  #-----------------------------------------------------#
  
  gap_decomp %>%
    arrange(kappa, desc(gap_pp)) %>%
    select(
      kappa, broad_industry, industry_group,
      Q1, Q4, gap_pp
    ) %>%
    print(n = 50)
  
  #-----------------------------------------------------#
  # Plot
  #-----------------------------------------------------#
  
  p_contribution <- ggplot(
    gap_decomp,
    aes(
      x = gap_pp,
      y = broad_industry,
      fill = industry_group
    )
  ) +
    
    geom_vline(
      xintercept = 0,
      colour = "grey45",
      linewidth = 0.45
    ) +
    
    geom_col(
      width = 0.72
    ) +
    
    geom_text(
      aes(
        label = value_label,
        hjust = label_hjust
      ),
      size = 2.9,
      colour = "grey20",
      show.legend = FALSE
    ) +
    
    facet_wrap(
      ~ kappa,
      ncol = 2,
      scales = "free_x"
    ) +
    
    scale_fill_manual(
      values = c(
        "Services" = hiro_cols[10],
        "Non-service industries" = hiro_cols[1]
      ),
      breaks = c("Services", "Non-service industries")
    ) +
    
    scale_x_continuous(
      expand = expansion(mult = c(0.19, 0.20))
    ) +
    
    labs(
      x = "Contribution to GDP gain gap, Q4 − Q1 (pp)",
      y = NULL,
      fill = NULL
    ) +
    
    theme_paper(base_size = 11) +
    
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.45, "cm"),
      
      strip.text = element_text(
        face = "bold",
        family = "Latin Modern Math",
        size = 10
      ),
      
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      axis.title.x = element_text(size = 11),
      
      plot.margin = margin(10, 15, 10, 10)
    )
  
  print(p_contribution)
  
  if (save) {
    save_fig(
      "industry_ppp_decomposition1",
      plot = p_contribution,
      type = "standard"
    )
  }
  
  invisible(p_contribution)
}



figure_p_industry_ppp_decomposition <- function(save = FALSE) {
  
  #-----------------------------------------------------#
  # Industry contributions
  #-----------------------------------------------------#
  
  decomp_rows <- res_i %>%
    filter(
      kappa %in% c(2.6, 6),
      scenario == "S_us, mu0_c",
      !is.na(tfp),
      VALUE_ADDED > 0
    ) %>%
    inner_join(
      res_agg %>%
        filter(
          kappa %in% c(2.6, 6),
          scenario == "S_us, mu0_c"
        ) %>%
        select(country, kappa, gdp),
      by = c("country", "kappa")
    ) %>%
    group_by(kappa, country) %>%
    mutate(
      country_va = sum(VALUE_ADDED),
      country_tfp = weighted.mean(tfp, VALUE_ADDED),
      row_contribution =
        (VALUE_ADDED / country_va) *
        tfp * (gdp / country_tfp)
    ) %>%
    ungroup() %>%
    mutate(
      section = substr(tiva, 1, 1),
      broad_industry = case_when(
        section == "A" ~ "Agriculture",
        section %in% c("B", "D", "E") ~ "Mining & utilities",
        section == "C" ~ "Manufacturing",
        section == "F" ~ "Construction",
        section %in% c("G", "H", "I") ~ "Trade, transport & hospitality",
        section == "J" ~ "Information & communication",
        section == "K" ~ "Finance & insurance",
        section == "L" ~ "Real estate",
        section %in% c("M", "N") ~ "Professional & administrative services",
        section %in% c("O", "P", "Q") ~ "Public, education & health",
        section %in% c("R", "S") ~ "Arts, recreation & personal services",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(broad_industry))
  
  #-----------------------------------------------------#
  # Country income quartiles
  #-----------------------------------------------------#
  
  country_quartiles <- decomp_rows %>%
    distinct(country) %>%
    inner_join(
      world_gdppc %>%
        transmute(
          country = `Country Code`,
          gdppc_ppp
        ),
      by = "country"
    ) %>%
    filter(is.finite(gdppc_ppp)) %>%
    mutate(
      income_quartile = factor(
        ntile(gdppc_ppp, 4),
        levels = 1:4,
        labels = c("Q1", "Q2", "Q3", "Q4")
      )
    )
  
  decomp_rows <- decomp_rows %>%
    inner_join(
      country_quartiles %>%
        select(country, income_quartile),
      by = "country"
    )
  
  #-----------------------------------------------------#
  # GDP-weighted industry contributions by quartile
  #-----------------------------------------------------#
  
  quartile_weights <- decomp_rows %>%
    distinct(kappa, income_quartile, country, country_va) %>%
    group_by(kappa, income_quartile) %>%
    summarise(
      quartile_va = sum(country_va),
      .groups = "drop"
    )
  
  broad_decomp <- decomp_rows %>%
    group_by(kappa, income_quartile, broad_industry) %>%
    summarise(
      weighted_contribution = sum(
        row_contribution * country_va
      ),
      .groups = "drop"
    ) %>%
    left_join(
      quartile_weights,
      by = c("kappa", "income_quartile")
    ) %>%
    mutate(
      gdp = weighted_contribution / quartile_va
    )
  
  #-----------------------------------------------------#
  # Colours and labels
  #-----------------------------------------------------#
  
  industry_order <- broad_decomp %>%
    group_by(broad_industry) %>%
    summarise(total = sum(gdp), .groups = "drop") %>%
    arrange(desc(total)) %>%
    pull(broad_industry)
  
  broad_decomp <- broad_decomp %>%
    mutate(
      broad_industry = factor(
        broad_industry,
        levels = industry_order
      ),
      kappa = factor(
        kappa,
        levels = c(2.6, 6),
        labels = c(
          "(a) Current AI Frontier (κ = 2.6)",
          "(b) Top of the Capability Scale (κ = 6)"
        )
      )
    )
  
  
  broad_decomp |> 
    select(-c(quartile_va,weighted_contribution)) |> 
    pivot_wider(names_from = income_quartile, values_from = gdp) |> 
    mutate(diffq4_q1 = Q4-Q1) |> 
    arrange(broad_industry) |> 
    print(n=50)
  
  
  
  
  hiro_cols <- met.brewer(
    name = "Hiroshige",
    n = length(industry_order),
    type = "continuous"
  )
  
  names(hiro_cols) <- industry_order
  hiro_cols[] <- rev(unname(hiro_cols))
  
  # Dark text on light colours; white on dark colours
  rgb_cols <- col2rgb(hiro_cols)
  
  luminance <- (
    0.299 * rgb_cols[1, ] +
      0.587 * rgb_cols[2, ] +
      0.114 * rgb_cols[3, ]
  ) / 255
  
  text_cols <- ifelse(
    luminance > 0.62, "#252525", "white"
  )
  names(text_cols) <- industry_order
  
  # Overall GDP gain for each bar
  totals <- broad_decomp %>%
    group_by(kappa, income_quartile) %>%
    summarise(
      total = sum(gdp),
      .groups = "drop"
    )
  
  #-----------------------------------------------------#
  # Stacked bar chart
  #-----------------------------------------------------#
  
  p_contribution <- ggplot(
    broad_decomp,
    aes(
      x = income_quartile,
      y = gdp,
      fill = broad_industry
    )
  ) +
    
    geom_col(
      width = 0.75,
      colour = "white",
      linewidth = 0.3
    ) +
    
    # Industry values INSIDE the segments
    geom_text(
      aes(
        label = sprintf("%.2f", gdp),
        colour = broad_industry
      ),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      fontface = "bold",
      show.legend = FALSE
    ) +
    
    # Total GDP gain ABOVE each bar
    geom_text(
      data = totals,
      aes(
        x = income_quartile,
        y = total,
        label = sprintf("%.2f", total)
      ),
      inherit.aes = FALSE,
      vjust = -0.6,
      size = 3.8,
      fontface = "bold",
      colour = "grey20"
    ) +
    
    facet_wrap(
      ~ kappa,
      ncol = 2,
      scales = "free_y"
    ) +
    
    scale_fill_manual(values = hiro_cols) +
    scale_colour_manual(values = text_cols) +
    
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.12))
    ) +
    
    labs(
      x = "GDP per capita, PPP (country quartiles)",
      y = "GDP Gain (pp)",
      fill = NULL
    ) +
    
    theme_paper(base_size = 12) +
    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.45, "cm"),
      
      strip.text = element_text(
        face = "bold",
        family = "Latin Modern Math",
        size = 12
      ),
      
      axis.text.x = element_text(size = 11),
      axis.title = element_text(size = 11),
      
      plot.margin = margin(10, 10, 10, 10)
    ) +
    
    guides(
      fill = guide_legend(
        ncol = 3,
        byrow = TRUE
      )
    )
  
  print(p_contribution)
  
  if (save) {
    save_fig(
      "industry_ppp_decomposition",
      plot = p_contribution,
      type = "standard"
    )
  }
  
  invisible(p_contribution)
}








figure_p_country_industry_contributions = function(save =F){
  
  
  #-----------------------------------------------------#
  # Country × broad industry GDP contributions
  #-----------------------------------------------------#
  
  broad_country_gdp <- res_i %>%
    filter(
      kappa == 2.6,
      scenario == "S_us, mu0_c",
      VALUE_ADDED > 0,
      !is.na(tfp),
      !is.na(Lshare)
    ) %>%
    left_join(
      geograph %>% select(Code, regions),
      by = join_by(country == Code)
    ) %>%
    filter(!is.na(regions)) %>%
    mutate(
      activity_section = substr(tiva, 1, 1),
      
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
        activity_section %in% c("U", "T") ~ "Household & extraterritorial activities",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(country) %>%
    mutate(
      country_va = sum(VALUE_ADDED),
      country_lshare = weighted.mean(Lshare, VALUE_ADDED)
    ) %>%
    ungroup() %>%
    group_by(country, regions, broad_industry) %>%
    summarise(
      sector_tfp_contribution =
        sum(VALUE_ADDED * tfp) / first(country_va),
      sector_gdp_contribution =
        sector_tfp_contribution / first(country_lshare) * 100,
      sector_va_share =
        sum(VALUE_ADDED) / first(country_va),
      country_va = first(country_va),
      .groups = "drop"
    )|> 
    filter(
      !is.na(broad_industry),
      !str_detect(
        broad_industry,
        regex("household|extraterritorial", ignore_case = TRUE)
      )
    )
  
  #-----------------------------------------------------#
  # Global GDP-weighted industry means
  #-----------------------------------------------------#
  
  country_weights <- world_gdp %>%
    transmute(
      country = `Country Code`,
      gdp_2023
    ) %>%
    semi_join(
      broad_country_gdp %>% distinct(country),
      by = "country"
    ) %>%
    mutate(
      weight = gdp_2023 / sum(gdp_2023)
    )
  
  industry_means <- broad_country_gdp %>%
    left_join(
      country_weights %>% select(country, weight),
      by = "country"
    ) %>%
    group_by(broad_industry) %>%
    summarise(
      global_contribution =
        sum(sector_gdp_contribution * weight, na.rm = TRUE),
      .groups = "drop"
    )
  #-----------------------------------------------------#
  # Industry order: largest at the top
  #-----------------------------------------------------#
  
  industry_order <- industry_means %>%
    arrange(global_contribution) %>%
    pull(broad_industry)
  
  broad_country_gdp <- broad_country_gdp %>%
    mutate(
      broad_industry = factor(broad_industry, levels = industry_order)
    )
  
  industry_means <- industry_means %>%
    mutate(
      broad_industry = factor(broad_industry, levels = industry_order)
    )
  
  #-----------------------------------------------------#
  # Plot
  #-----------------------------------------------------#
  
  p_industries <- ggplot(
    broad_country_gdp,
    aes(
      x = sector_gdp_contribution,
      y = broad_industry
    )
  ) +
    
    geom_point(
      aes(color = regions),
      size = 1.8,
      alpha = 0.9, 
      position = position_jitter(
        width = 0,
        height = 0.09,
        seed = 42
      ),
    ) +
    
    geom_point(
      data = industry_means,
      aes(x = global_contribution, y = broad_industry),
      inherit.aes = FALSE,
      shape = 18,
      size = 3.2,
      color = "black"
    ) +
    
    scale_color_met_d("Hiroshige") +
    
    scale_x_continuous(
      expand = expansion(mult = c(0.01, 0.05))
    ) +
    
    labs(
      x = "Contribution to Country GDP Gain (pp)",
      y = NULL,
      color = "World region"
    ) +
    
    theme_paper(base_size = 14) +
    
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey95"),
      axis.text.y = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
  
  
  print(p_industries)
  
  if (save) {
    save_fig(
      "country_industry_decomposition",
      plot = p_industries,
      type = "wide"
    )
  }
  
  invisible(broad_country_gdp)
  
  
  country_top_industry <- broad_country_gdp %>%
    group_by(country) %>%
    slice_max(
      sector_gdp_contribution,
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup()
  
  cat("what industry contributes the most in individual countries:")
  cat("")
  industry_ranks <- broad_country_gdp %>%
    group_by(country) %>%
    arrange(
      desc(sector_gdp_contribution),
      broad_industry,
      .by_group = TRUE
    ) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  n_countries <- n_distinct(industry_ranks$country)
  
  # Distribution of industry rankings
  rank_summary <- industry_ranks %>%
    group_by(broad_industry) %>%
    summarise(
      first  = sum(rank == 1),
      second = sum(rank == 2),
      third  = sum(rank == 3),
      top3   = sum(rank <= 3),
      mean_rank = mean(rank),
      mean_contribution = mean(sector_gdp_contribution),
      .groups = "drop"
    ) %>%
    mutate(
      share_first = 100 * first / n_countries
    ) %>%
    arrange(desc(first), desc(second))
  
  print(rank_summary, n = Inf)
  
  
}







figure_p_param_decomp_all = function(save = F){
  
  
  #USA ONLY ON INDUSTRIES and a few industries
  plot_data <- res_i %>%
    filter(!scenario %in% c("baseline_b6", "baseline_b75", 
                            "baseline_kappa3.6","S_us, fix_lambda_iota_upd"  )) |> 
    pivot_longer(
      cols = c(S, iota, lambda),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    
    mutate(
      parameter = factor(
        parameter,
        levels = c("S", "iota", "lambda"),
        labels = c("Exposure \n(S)", "Adoption share \n(\u03B9)", 
                   "Savings \n(\u03BB)")
      ),
      
      scenario = factor(
        scenario,
        levels = c(
          "baseline",
          "baseline, upd_param",
          "S_us, fix_lambda_iota",
          "S_us, mu0_c",
          "S_us, mu0_c, upd_param",
          "S_c, mu0_c",
          "S_c, mu0_c, internet"
        ),
        labels = c(
          "US Adoption\nConditions",
          "Optimism: US Adoption\nConditions",
          "Fixed Adoption\nand Savings",
          "Matched to Acemoglu \n(Baseline)",
          "Optimism",
          "Country-Specific\nExposure",
          "Country-Spec. Exposure\nwith Internet Adjust."
        )
    )) |> 
    
    left_join(world_gdp,
              by = join_by("country" == "Country Code")) %>%
    filter(!is.na(gdp_2023), gdp_2023 > 0) %>%
    group_by(scenario, kappa, parameter) %>%
    summarise(
      p10 = quantile(value, 0.1, na.rm = TRUE),
      p50 = median(value, na.rm = TRUE),
      p50w = weighted.mean(value, w = VALUE_ADDED, na.rm = TRUE),
      p90 = quantile(value, 0.9, na.rm = TRUE),
      .groups = "drop"
    ) 
  
  p_gdp_growth_us = ggplot(plot_data, aes(x = kappa, y = p50)) +
    annotate("rect",
             xmin = 2.6, xmax = 6,
             ymin = -Inf, ymax = Inf,
             fill = my_pal[5], alpha = 0.2) +
    geom_ribbon(
      aes(ymin = p10, ymax = p90),
      fill = "gray", alpha = .4
    ) +
    geom_line(color = "#1E466E", linewidth = 0.9) +
    geom_line(aes(x = kappa, y = p50w), color = "gray60", 
              linetype="dashed", linewidth = 0.9) +
    facet_grid( parameter~ scenario)+
    labs(
      x = "AI Capability (κ)",
      y = "Parameter value"
    ) +
    theme_paper(base_size = 15) +
    theme(
      strip.text = element_text(size = 12,
                                family = "Latin Modern Math"),
      axis.title = element_text(color = "grey30",family = "Latin Modern Math"),
      text = element_text(family = "Latin Modern Math"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95")
    )
  
  
  print(p_gdp_growth_us)
  
  if (save) {
    ggsave(
      "figures/figure_p_param_decomp_all.pdf",
      plot = p_gdp_growth_us,
      width = 13,
      height = 8,
      units = "in"
    )

  }
  
}



figure_p_regional_gdp_gains = function(save =F){
  
  # Regional GDP gains
  gdp_cap <- res_agg %>%
    filter(
      scenario == "S_us, mu0_c",
      kappa %in% c(2.6, 6)
    ) %>%
    left_join(
      geograph %>% distinct(Code, regions),
      by = join_by(country == Code)
    ) %>%
    left_join(
      world_gdp %>% distinct(`Country Code`, gdp_2023),
      by = join_by(country == `Country Code`)
    ) %>%
    filter(
      !is.na(regions),
      !is.na(gdp_2023),
      gdp_2023 > 0
    ) %>%
    group_by(regions, kappa) %>%
    summarise(
      GDP_Growth = weighted.mean(
        gdp, w = gdp_2023, na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  # Plot
  p_gdp_impact_regions <- ggplot(
    gdp_cap,
    aes(x = regions, y = GDP_Growth, fill = regions)
  ) +
    geom_col(
      data = ~ filter(.x, kappa == 6),
      aes(alpha = factor(kappa)),
      width = 0.7
    ) +
    geom_col(
      data = ~ filter(.x, kappa == 2.6),
      aes(alpha = factor(kappa)),
      width = 0.7
    ) +
    geom_text(
      data = ~ filter(.x, kappa == 2.6),
      aes(label = sprintf("%.1f", GDP_Growth)),
      vjust = 1.5,
      size = 4,
      color = "white"
    ) +
    geom_text(
      data = ~ filter(.x, kappa == 6),
      aes(label = sprintf("%.1f", GDP_Growth)),
      vjust = -0.5,
      size = 4,
      color = "grey30"
    ) +
    scale_fill_met_d("Hiroshige") +
    scale_alpha_manual(
      values = c("2.6" = 1, "6" = 0.4),
      name = "",
      labels = c(
        "Current AI Frontier (κ = 2.6)",
        "Top of the Scale (κ = 6)"
      ),
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        keywidth = unit(3, "cm"),
        keyheight = unit(0.5, "cm"),
        override.aes = list(fill = "gray40")
      )
    ) +
    guides(fill = "none") +
    labs(
      x = "World Region",
      y = "GDP Gain (%)"
    ) +
    theme_paper(base_size = 15) +
    theme(
      axis.title = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(family = "Latin Modern Math"), 
      
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      #panel.grid.major.y = element_blank(),
      #panel.grid.minor.y = element_blank(),
      axis.line.y = element_blank()
    )
  
  print(p_gdp_impact_regions)
  
  if (save) {
    save_fig(
      "p_gdp_impact_regions",
      plot = p_gdp_impact_regions,
      type = "wide"
    )
  }
}




figure_p_map = function(save = F){
  
  
  
  #---------------------------------------------------#
  # Figure: Global Map of GDP Impact
  #---------------------------------------------------#
  
  world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin != "Antarctica") %>%
    mutate(
      iso_a3 = case_when(
        admin == "France" ~ "FRA",
        admin == "Norway" ~ "NOR",
        admin == "South Korea" ~ "KOR",
        admin == "Greenland" ~ "DNK",
        TRUE ~ iso_a3
      )
    )
  
  map_pal_steps <- met.brewer("Hiroshige", n = 7)
  
  make_gdp_map <- function(data, scenario_use, capability, breaks, labels = waiver()) {
    
    map_data <- data %>%
      filter(
        scenario == scenario_use,
        kappa == capability
      ) %>%
      select(country, gdp) %>%
      rename(iso_a3 = country)
    
    world_sf %>%
      left_join(map_data, by = "iso_a3") %>%
      ggplot() +
      geom_sf(aes(fill = gdp), color = "white", linewidth = 0.15) +
      scale_fill_stepsn(
        colors = map_pal_steps,
        breaks = breaks,
        labels = labels,
        limits = range(breaks),
        na.value = "#f0f0f0",
        name = "GDP gain (%)",
        guide = guide_colorsteps(
          direction = "horizontal",
          title.position = "top",
          title.hjust = 0.5,
          barwidth = unit(12, "cm"),
          barheight = unit(0.5, "cm"),
          frame.colour = "grey30",
          ticks.colour = "grey30"
        )
      ) +
      theme_void(base_size = 16) +
      theme(
        text = element_text(family = "Latin Modern Math"),
        legend.position = "bottom",
        legend.title = element_text(color = "grey30"),
        legend.text = element_text(color = "grey30")
      )
  }
  
  # Current frontier: kappa = 2.6
  p_world_map_current <- make_gdp_map(
    data = res_agg,
    scenario_use = "S_us, mu0_c",
    capability = 2.6,
    breaks = seq(0.3,.9,by=0.1)
  )
  
  print(p_world_map_current)
  
  if (save) {
    save_fig(
      "world_map_k2_6",
      plot = p_world_map_current,
      type = "wide"
    )
  }
  
  # Maximum capability: kappa = 6
  p_world_map_max <- make_gdp_map(
    data = res_agg,
    scenario_use = "S_us, mu0_c",
    capability = 6,
    breaks = seq(4,10,by=1)
  )
  
  print(p_world_map_max)
   
  if (save) {
    save_fig(
      "world_map_k6",
      plot = p_world_map_max,
      type = "wide"
    )
  }
  
}



figure_p_demografic_S_us_mu0c= function(save=F){
  
  # Demography
  world_pop <- world_pop_raw %>%
    transmute(
      country = `Country Code`,
      Country = `Country Name`,
      pop_2022 = parse_number(as.character(`2022 [YR2022]`)),
      pop_2032 = parse_number(as.character(`2032 [YR2032]`)),
      pop_2042 = parse_number(as.character(`2042 [YR2042]`))
    ) %>%
    filter(if_all(starts_with("pop_"), ~ !is.na(.x)), pop_2022 > 0) %>%
    pivot_longer(
      cols = c(pop_2032, pop_2042),
      names_to = "horizon",
      values_to = "pop_future"
    ) %>%
    mutate(
      horizon = as.numeric(substr(horizon, 5, 8)) - 2022,
      pop_growth_pct = 100 * (pop_future / pop_2022 - 1)
    )
  
  #-----------------------------------------------------#
  # Regional demographic effects and AI gains
  #-----------------------------------------------------#
  
  region_summary <- res_agg %>%
    filter(
      scenario == "S_us, mu0_c",
      kappa %in% c(2.6, 6)
    ) %>%
    inner_join(world_pop, by = "country") %>%
    left_join(
      geograph %>% select(Code, regions),
      by = join_by(country == Code)
    ) %>%
    left_join(
      world_gdp %>% select(`Country Code`, gdp_2023),
      by = c("country" = "Country Code")
    ) %>%
    filter(
      !is.na(regions),
      !is.na(gdp),
      !is.na(gdp_2023),
      gdp_2023 > 0
    ) %>%
    group_by(regions, horizon, kappa) %>%
    summarise(
      n_countries = n_distinct(country),
      
      # Absolute regional population
      pop_2022 = sum(pop_2022),
      pop_future = sum(pop_future),
      
      # GDP-weighted demographic effect
      # we weight by gdp becuase we account for different effects
      # across individual countries
      demographic_effect = weighted.mean(pop_growth_pct, w = gdp_2023),
      
      # GDP-weighted AI gain
      ai_gain = weighted.mean(gdp, w = gdp_2023),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Actual regional population change
      pop_growth_pct = 100 * (pop_future / pop_2022 - 1),
      
      # Combined GDP effect
      net_effect = ai_gain + demographic_effect,
      
      # Does AI offset demographic decline?
      ai_offsets = net_effect >= 0
    ) %>%
    arrange(kappa,horizon, regions)
  
  print(region_summary, n = Inf)
  
  
  global = res_agg %>%
    filter(
      scenario == "S_us, mu0_c",
      kappa %in% c(2.6, 6)
    ) %>%
    inner_join(world_pop, by = "country") %>%
    left_join(
      geograph %>% select(Code, regions),
      by = join_by(country == Code)
    ) %>%
    left_join(
      world_gdp %>% select(`Country Code`, gdp_2023),
      by = c("country" = "Country Code")
    ) %>%
    filter(
      !is.na(regions),
      !is.na(gdp),
      !is.na(gdp_2023),
      gdp_2023 > 0
    ) %>%
    group_by(horizon, kappa) %>%
    summarise(
      n_countries = n_distinct(country),
      
      # Absolute regional population
      pop_2022 = sum(pop_2022),
      pop_future = sum(pop_future),
      
      # GDP-weighted demographic effect
      # we weight by gdp becuase we account for different effects
      # across individual countries
      demographic_effect = weighted.mean(pop_growth_pct, w = gdp_2023),
      
      # GDP-weighted AI gain
      ai_gain = weighted.mean(gdp, w = gdp_2023),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Actual regional population change
      pop_growth_pct = 100 * (pop_future / pop_2022 - 1),
      
      # Combined GDP effect
      net_effect = ai_gain + demographic_effect,
      
      # Does AI offset demographic decline?
      ai_offsets = net_effect >= 0
    ) %>%
    arrange(horizon, kappa) |> 
    select(-c(pop_2022,pop_future))
  
  cat("total global impact")
  print(global)
  
  #-----------------------------------------------------#
  # Figure: AI GDP gain vs demographic drag
  #-----------------------------------------------------#
  
  demographic_analysis <- res_agg %>%
    left_join(
      world_gdp %>% select(`Country Code`, gdp_2023),
      by = c("country" = "Country Code")
    ) |> 
    left_join(
      geograph %>% select(Code, regions),
      by = join_by(country == Code)
    ) %>%
    filter(
      scenario == "S_us, mu0_c",
      kappa %in% c(2.6, 6)
    ) %>%
    left_join(
      world_pop %>%
        transmute(
          country = country,
          pop_growth_pct = pop_growth_pct,
          horizon = factor(horizon, levels = c(10, 20),
                           labels = c("(a) 10-year projection", "(b) 20-year projection"))
        ),
      by = "country"
    ) %>%
    filter(!is.na(pop_growth_pct), !is.na(gdp), !is.na(gdp_2023)) %>%
    mutate(
      kappa_num = kappa,
      horizon_num = horizon,
      kappa = factor(kappa, levels = c(2.6, 6),
                     labels = c("Current AI Frontier (κ=2.6)", "Top of the Capability Scale (κ=6)"))
    )
  
  
  
  label_data <- demographic_analysis %>%
    mutate(horizon_num = readr::parse_number(as.character(horizon))) %>%
    filter(
      # Always label selected countries
      country %in% c("USA", "CHN", "IND", "DEU", "JPN", "BRA", "EGY", "CZE") |
        
        # Demographic thresholds by projection horizon
        (horizon_num == 10 & pop_growth_pct > 10) |
        (horizon_num == 20 & pop_growth_pct > 20) |
        
        # GDP thresholds by AI capabilities
        (kappa_num == 2.6 & gdp > 2) |
        (kappa_num == 6   & gdp > 9)
    )
  
  break_even <- tibble(
    pop_growth_pct = seq(-12, 0, length.out = 500)
  ) %>%
    mutate(gdp = -pop_growth_pct)
  
  
f_demographic =  ggplot(
    demographic_analysis,
    aes(x = pop_growth_pct,
        y = gdp,
        color = regions)
  ) +
    
    # reference lines
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    
    # breakeven line: AI gain offsets demographic decline
    geom_line(
      data = break_even,
      aes(x = pop_growth_pct, y = gdp),
      inherit.aes = FALSE,
      linetype = "dotted",
      color = my_pal[1],
      linewidth = 1
    ) +
    # points sized by GDP
    geom_point(aes(size = gdp_2023), alpha = 1) +
    
    # labels for selected countries
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = country),
      family = "Latin Modern Math",
      size = 3.5,
      color = "grey30",
      segment.color = NA,  
      show.legend = FALSE,
      max.overlaps = Inf,
      seed = 123
    ) +
    
    scale_size_continuous(guide = "none") +
    scale_color_met_d("Hiroshige") +
    
    labs(
      x = "Projected Working-Age Population Growth (%)",
      y = "GDP Gain (%)",
      color = "World Region"
    ) +
    
    # the curve comes from the calculation that the 
    # pop growth and gdp gain interacts. it does not have a 
    # significant effect on our data so we do use the simple one
    # stat_function(
    #   fun = function(x) -100 * x / (100 + x),
    #   linetype = "dotted",
    #   color = my_pal[1],
    #   linewidth = 1
    # )+ 
    
    #set scales
    facet_grid(kappa ~ horizon, scales = "free_y") +
    ggh4x::facetted_pos_scales(
      y = list(
        scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.5, 0.3)),
        scale_y_continuous(limits = c(0, 13), breaks = seq(0, 12, 2))
      )
    ) +
    
    scale_x_continuous(
      transform = scales::pseudo_log_trans(base = 10, sigma = 5),
      breaks = c(-50, -20, -10, -5, 0, 5, 10, 20, 50, 100)
    )+
    
    theme_paper(base_size = 15) +
    theme(
      text = element_text( family = "Latin Modern Math"),
      axis.title = element_text(color = "grey10"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(color = "grey20")
    )
  
  
  print(f_demographic)
  
  if (save) {
    
    ggsave(
      "figures/f_demographic_s_us_mu_0c.pdf",
      plot = f_demographic,
      width = 13,
      height = 7.5,
      units = "in"
    )

  
}

}




figure_p_global_shares =  function(save=F){
  
  
  world_shares <- res_agg %>%
    filter(
      scenario == "S_us, mu0_c",
      kappa %in% c(2.6, 6)
    ) %>%
    inner_join(world_pop, by = "country") %>%
    left_join(
      geograph %>% select(Code, regions),
      by = join_by(country == Code)
    ) %>%
    left_join(
      world_gdp %>% select(`Country Code`, gdp_2023),
      by = c("country" = "Country Code")
    ) %>%
    filter(
      !is.na(regions),
      !is.na(gdp),
      !is.na(gdp_2023),
      gdp_2023 > 0
    ) %>%
    mutate(
      gdp_future = gdp_2023 *
        (1 + (gdp + pop_growth_pct) / 100)
    ) %>%
    group_by(regions, horizon, kappa) %>%
    summarise(
      gdp_base = sum(gdp_2023),
      gdp_future = sum(gdp_future),
      .groups = "drop"
    ) %>%
    group_by(horizon, kappa) %>%
    mutate(
      share_base = 100 * gdp_base / sum(gdp_base),
      share_future = 100 * gdp_future / sum(gdp_future),
      change_pp = share_future - share_base
    ) %>%
    ungroup()
  
  
  
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(MetBrewer)
  
  # -------------------------------------------------
  # Data
  # -------------------------------------------------
  
  region_levels <- c(
    "Africa", "Asia", "Europe",
    "North America", "Oceania", "South America"
  )
  
  pie_current <- world_shares %>%
    filter(horizon == 10, kappa == 2.6) %>%
    distinct(regions, share_base) %>%
    mutate(
      regions = factor(regions, levels = region_levels),
      share = share_base
    ) %>%
    arrange(regions)
  
  pie_future <- world_shares %>%
    mutate(
      regions = factor(regions, levels = region_levels),
      share = share_future
    ) %>%
    arrange(regions)
  
  # -------------------------------------------------
  # Helpers
  # -------------------------------------------------
  
  make_donut <- function(df, title = NULL, show_legend = FALSE, text_size = 3.6) {
    ggplot(df, aes(x = 2, y = share, fill = regions)) +
      geom_col(width = 1, color = "white", linewidth = 0.35) +
      geom_text(
        aes(label = ifelse(share >= 3, paste0(round(share, 1), "%"), "")),
        position = position_stack(vjust = 0.5),
        family = "Latin Modern Math",
        size = text_size
      ) +
      coord_polar(theta = "y") +
      xlim(0.7, 2.5) +   # controls donut hole size
      scale_fill_met_d("Hiroshige") +
      labs(title = title, fill = NULL) +
      theme_void(base_family = "Latin Modern Math") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 13, color = "grey20"),
        legend.position = if (show_legend) "bottom" else "none",
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.45, "cm"),
        plot.margin = margin(2, 2, 2, 2)
      )
  }
  
  text_panel <- function(label, size = 4.8) {
    ggplot() +
      annotate(
        "text",
        x = 0.5, y = 0.5,
        label = label,
        family = "Latin Modern Math",
        size = size,
        color = "grey20",
        lineheight = 1.0,
        hjust = 0.5
      ) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      theme(plot.margin = margin(2, 2, 2, 2))
  }
  
  col_header <- function(label, size = 4.6) {
    ggplot() +
      annotate(
        "text",
        x = 0.5, y = 0.5,
        label = label,
        family = "Latin Modern Math",
        size = size,
        color = "grey20"
      ) +
      xlim(0, 1) + ylim(0, 1) +
      theme_void() +
      theme(plot.margin = margin(0, 2, 0, 2),
            strip.text = element_text(
              face = "bold",
              family = "Latin Modern Math",
              size = 12
            ))
  }
  
  # -------------------------------------------------
  # Current donut
  # -------------------------------------------------
  
  p_current <- make_donut(
    pie_current,
    title = "Current",
    show_legend = FALSE,
    text_size = 3.9
  )
  
  # -------------------------------------------------
  # Future donuts
  # -------------------------------------------------
  
  p_10_k26 <- make_donut(
    pie_future %>% filter(horizon == 10, kappa == 2.6),
    show_legend = FALSE,
    text_size = 3.5
  )
  
  p_20_k26 <- make_donut(
    pie_future %>% filter(horizon == 20, kappa == 2.6),
    show_legend = FALSE,
    text_size = 3.5
  )
  
  p_10_k6 <- make_donut(
    pie_future %>% filter(horizon == 10, kappa == 6),
    show_legend = FALSE,
    text_size = 3.5
  )
  
  p_20_k6 <- make_donut(
    pie_future %>% filter(horizon == 20, kappa == 6),
    show_legend = TRUE,
    text_size = 3.5
  )
  
  # -------------------------------------------------
  # Middle labels and headers
  # -------------------------------------------------
  
  p_head_10 <- col_header("10 years")
  p_head_20 <- col_header("20 years")
  
  p_mid_1 <- text_panel("Current AI Frontier (κ = 2.6)", size = 4.8)
  p_mid_2 <- text_panel("Top of the Capability Scale (κ = 6)", size = 4.8)
  
  # -------------------------------------------------
  # Right block: 2x2 donuts with labels in the middle
  # -------------------------------------------------
  
  future_block <-
    (
      p_head_10 | plot_spacer() | p_head_20
    ) /
    (
      p_10_k26 | p_mid_1 | p_20_k26
    ) /
    (
      p_10_k6  | p_mid_2 | p_20_k6
    ) +
    plot_layout(
      widths = c(1, 0.75, 1),
      heights = c(0.16, 1, 1),
      guides = "collect"
    )
  
  # -------------------------------------------------
  # Final combined plot
  # -------------------------------------------------
  
  f_world_pie_clean <-
    (p_current | future_block) +
    plot_layout(
      widths = c(1.05, 2.35),
      guides = "collect"
    )
  
  print(f_world_pie_clean)
  
  if(save==T){
    ggsave(
      "figures/f_world_pie_clean.pdf",
      f_world_pie_clean,
      width = 12.5,
      height = 6.8,
      units = "in"
    )
  }
  
}



p_figure_cohort_dist = function(save =F){
  
  
  
  #-----------------------------------------------------#
  # Figure: Distribution of difficulty cohorts by industry
  #-----------------------------------------------------#
  
  kappa_grid <- sort(unique(s_us$`AI capability`))
  bar_width <- min(diff(kappa_grid)) * 0.85
  
  # Construct the same cohorts as in run_model()
  cohort_distribution <- s_us %>%
    transmute(
      tiva,
      kappa = `AI capability`,
      S = AISA_bill_weighted / 100
    ) %>%
    arrange(tiva, kappa) %>%
    group_by(tiva) %>%
    mutate(
      omega = S - lag(S, default = 0),
      S_max = S[which.max(kappa)]
    ) %>%
    ungroup()
  
  # Check cumulative exposure
  stopifnot(
    all(is.finite(cohort_distribution$S)),
    all(cohort_distribution$omega >= -1e-8)
  )
  
  # Normalise within the exposed wage bill of each industry
  cohort_distribution <- cohort_distribution %>%
    mutate(
      share = if_else(
        S_max > 0,
        100 * pmax(omega, 0) / S_max,
        0
      )
    ) %>%
    left_join(
      hierarchy %>%
        transmute(
          tiva = Industry_Code,
          description = Industry_Description
        ) %>%
        distinct(tiva, .keep_all = TRUE),
      by = "tiva"
    ) %>%
    mutate(
      industry_label = paste0(
        tiva, "  ",
        str_trunc(coalesce(description, tiva), 32)
      ),
      industry_label = factor(
        industry_label,
        levels = unique(industry_label)
      )
    )
  
  # Plot all industries
  p_cohort_distribution <- ggplot(
    cohort_distribution,
    aes(x = kappa, y = share)
  ) +
    geom_col(
      width = bar_width,
      fill = "#1E466E"
    ) +
    
    facet_wrap(
      ~ industry_label,
      ncol = 5,
      scales = "free_y"
    ) +
    scale_x_continuous(
      breaks = c(0, 2, 4, 6),
      limits = c(0, 6),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = expression("AI capability " * kappa),
      y = "Share of Exposed Wage Bill"
    ) +
    theme_paper(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 7),
      panel.spacing = unit(0.8, "lines")
    )
  
  print(p_cohort_distribution)
  
  ggsave(
    "./figures/figure_cohort_distribution.pdf",
    p_cohort_distribution,
    width = 18,
    height = 20
  )
  
}





figure_p_eloundou_compar = function(save=F){
  
  
  #--------------------------------------------------------------#
  # Figure: AISA vs Eloundou across AI capabilities
  #--------------------------------------------------------------#
  
  compare_df <- dat_us %>%
    filter(
      !is.na(AISA),
      !is.na(acem_exp),
      !is.na(`AI capability`)
    ) %>%
    transmute(
      OCC_CODE,
      `AI capability`,
      AISA,
      acem_exp = acem_exp * 100
    ) %>%
    distinct()
  
  
  kappa_show <- c(2, 2.6, 3, 3.6, 4, 6)
  
  plot_comparison <- compare_df %>%
    transmute(
      occupation = OCC_CODE,
      kappa = round(`AI capability`, 2),
      AISA,
      acem_exp
    ) %>%
    filter(
      kappa %in% kappa_show,
      is.finite(AISA),
      is.finite(acem_exp)
    ) %>%
    distinct() %>%
    mutate(
      panel = factor(
        kappa,
        levels = kappa_show,
        labels = paste0("κ = ", kappa_show)
      )
    )
  
  # Correlation based on unique occupations
  comparison_stats <- plot_comparison %>%
    group_by(panel) %>%
    summarise(
      r = cor(AISA, acem_exp),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(label = sprintf("r = %.2f", r))
  
  #--------------------------------------------------------------#
  # Plot
  #--------------------------------------------------------------#
  
  p_comparison_eloundou_auer <- ggplot(
    plot_comparison,
    aes(x = AISA, y = acem_exp)
  ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      colour = "grey55",
      linewidth = 0.5
    ) +
    geom_point(
      alpha = 0.25,
      colour = "black",
      size = 0.9
    ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      colour = "#E76254",
      linewidth = 0.8
    ) +
    geom_text(
      data = comparison_stats,
      aes(
        x = 5,
        y = 95,
        label = label
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3.3,
      colour = "black"
    ) +
    facet_wrap(
      ~ panel,
      ncol = 3
    ) +
    coord_equal(
      xlim = c(0, 100),
      ylim = c(0, 100),
      expand = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(0, 100, 25)
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 25)
    ) +
    labs(
      x = "AISA AI Exposure (%)",
      y = "Eloundou et al. Exposure (%)"
    ) +
    theme_paper(base_size = 11) +
    theme(
      
      text = element_text(size = 14,
                                family = "Latin Modern Math"),
      strip.background = element_blank(),
      strip.text = element_text(
        size = 11,
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        colour = "grey92",
        linewidth = 0.25
      ),
      axis.text = element_text(size = 9),
      panel.spacing = unit(1.2, "lines")
    )
  
  print(p_comparison_eloundou_auer)
  
  ggsave(
    "./figures/aisa_eloundou_comparison.pdf",
    plot = p_comparison_eloundou_auer,
    width = 10,
    height = 7
  )
  
  
  
}




fig_animate = function(save=F){
  
  
  
  # Run after res_agg is created in script.R.
  # Uses objects already in memory: res_agg, world_gdp, world_gdppc, geograph, theme_paper.
  # Needs: dplyr, tidyr, ggplot2, gganimate, MetBrewer, ragg, gifski.
  
  library(gganimate)
  
  # ---- Fonts: measure text with the same engine that renders it ---------------
  # The warning "font family not found in Windows font database" means ggplot
  # measures text on the Windows device (fallback font) but renders with ragg
  # (real font) -> legend/label widths are wrong -> overlaps.
  # 1) make ragg the default device, so layout is measured with systemfonts:
  #options(device = function(...) ragg::agg_png(...))
  grDevices::graphics.off()
  # 2) belt and braces: also register the families for the Windows device
  if (.Platform$OS.type == "windows") {
    windowsFonts(
      `Latin Modern Roman` = windowsFont("Latin Modern Roman"),
      `Latin Modern Math`  = windowsFont("Latin Modern Math")
    )
  }
  
  # ---- Highlighted countries --------------------------------------------------
  # y_shift is a fraction of the phase's maximum GDP gain, so label offsets
  # scale properly when the y-range changes between kappa = 2.6 and kappa = 6.
  label_positions <- tibble::tibble(
    country   = c("USA", "CHN", "IND", "DEU", "JPN", "BRA"),
    highlight = c("United States", "China", "India", "Germany", "Japan", "Brazil"),
    x_mult    = c(1.12, 1.10, 1.12, 0.90, 0.90, 1.12),
    y_shift   = c(0.35, 0.35, 0.25, 0.35, 0.35, 0.35)
  )
  
  # ---- Data -------------------------------------------------------------------
  gdp_anim <- res_agg %>%
    filter(scenario == "S_us, mu0_c", kappa %in% c(2.6, 6)) %>%
    left_join(world_gdp   %>% select(`Country Code`, gdp_2023),
              by = c("country" = "Country Code")) %>%
    left_join(world_gdppc %>% select(`Country Code`, gdppc_ppp),
              by = c("country" = "Country Code")) %>%
    left_join(geograph %>% select(Code, regions),
              by = c("country" = "Code")) %>%
    left_join(label_positions, by = "country") %>%
    filter(is.finite(gdppc_ppp), gdppc_ppp > 0,
           is.finite(gdp), is.finite(gdp_2023), !is.na(regions)) %>%
    mutate(
      gdp_trn = gdp_2023 / 1e12,
      phase   = if_else(kappa == 2.6, 1L, 2L)
    ) %>%
    group_by(phase) %>%
    mutate(
      x_label = gdppc_ppp * coalesce(x_mult, 1),
      y_label = gdp + coalesce(y_shift, 0) * max(gdp) * if_else(phase == 2L, 0.6, 1)
    ) %>%
    ungroup() %>%
    arrange(desc(gdp_trn))               # big bubbles drawn first, small ones on top
  
  axis_limits <- tibble::tibble(
    phase = c(1L, 2L, 3L),
    x_axis = median(gdp_anim$gdppc_ppp),
    y_min = c(0, 0, 0),
    y_max = c(2.5, 7.2, 2.5)
  )
  
  # Out and back: 2.6 -> 6 -> 2.6 (phase 3 = copy of phase 1, so the loop is seamless)
  gdp_anim <- bind_rows(
    gdp_anim,
    gdp_anim %>% filter(phase == 1L) %>% mutate(phase = 3L)
  )
  
  xr <- range(gdp_anim$gdppc_ppp)
  
  # The line is fitted on all countries but DRAWN only from this quantile of
  # GDP per capita upwards (0 = full range). Raise it to shorten the line more.
  line_from_q <- 0.05
  x0 <- unname(quantile(gdp_anim$gdppc_ppp, line_from_q))
  
  # ---- OLS line + beta per phase ----------------------------------------------
  fits <- gdp_anim %>%
    group_by(phase) %>%
    group_modify(~ {
      m <- lm(gdp ~ log(gdppc_ppp), data = .x)
      tibble(a = unname(coef(m)[1]), b = unname(coef(m)[2]), y_top = max(.x$gdp))
    }) %>%
    ungroup()
  
  # 100 matched x positions per phase let gganimate morph the line smoothly.
  line_anim <- tidyr::crossing(
    fits,
    gdppc_ppp = exp(seq(log(x0), log(xr[2]), length.out = 100))
  ) %>%
    mutate(gdp = a + b * log(gdppc_ppp))
  
  beta_anim <- fits %>%
    mutate(label = sprintf("\u03b2 = %.2f", b),
           # sit right next to the left end of the line (tweens with it)
           x     = x0 * 0.66,
           y     = a + b * log(x0))
  
  # ---- Plot -------------------------------------------------------------------
  plot_anim <- ggplot(gdp_anim, aes(x = gdppc_ppp, y = gdp, group = country)) +
    # keep y-axis anchored at 0 and tall enough for labels in every frame
    geom_blank(
      data = axis_limits,
      aes(x = x_axis, y = y_min),
      inherit.aes = FALSE
    ) +
    geom_blank(
      data = axis_limits,
      aes(x = x_axis, y = y_max),
      inherit.aes = FALSE
    ) +
    geom_line(
      data = line_anim,
      aes(x = gdppc_ppp, y = gdp, group = 1),
      inherit.aes = FALSE,
      color = "grey35", linewidth = 0.8
    ) +
    geom_segment(
      data = gdp_anim %>% filter(!is.na(highlight)),
      aes(x = gdppc_ppp, y = gdp, xend = x_label, yend = y_label, group = country),
      inherit.aes = FALSE,
      color = "grey75", linewidth = 0.35
    ) +
    geom_point(aes(size = gdp_trn, color = regions), alpha = 0.92) +
    geom_text(
      data = gdp_anim %>% filter(!is.na(highlight)),
      aes(x = x_label, y = y_label, label = highlight, group = country),
      inherit.aes = FALSE,
      size = 3.8, color = "grey20", family = "Latin Modern Roman",
      hjust = 0.5, vjust = -0.15
    ) +
    geom_text(
      data = beta_anim,
      aes(x = x, y = y, label = label, group = 1),
      inherit.aes = FALSE,
      size = 5, fontface = "bold", color = "black",
      hjust = 0, vjust = 0.5, family = "Latin Modern Math"
    ) +
    scale_x_continuous(
      transform = "log",
      breaks = c(1000, 3000, 10000, 30000, 100000),
      labels = scales::label_dollar(accuracy = 1)
    ) +
    scale_size_continuous(
      range = c(1.5, 9),
      breaks = c(1, 5, 10, 20),
      name = "GDP, 2023 (USD trn)"
    ) +
    scale_color_met_d("Hiroshige") +
    guides(
      color = guide_legend(order = 1, nrow = 1, override.aes = list(size = 3)),
      size  = guide_legend(order = 2, nrow = 1, override.aes = list(color = "grey40"))
    ) +
    labs(
      title    = "AI GDP impact across countries",
      subtitle = "AI capability: \u03ba = {c('2.6', '6', '2.6')[as.integer(closest_state)]}",
      x = "GDP per capita, PPP (log scale)",
      y = "GDP Gain (%)",
      color = "World region"
    ) +
    theme_paper(base_size = 14) +
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(hjust = 0.5),
      # Latin Modern Math has the kappa glyph (the text font does not)
      plot.subtitle     = element_text(hjust = 0.5, size = 13, family = "Latin Modern Math"),
      legend.position   = "bottom",
      legend.box        = "horizontal",          # two legends side by side...
      legend.box.just   = "top",
      legend.title.position = "top",             # ...titles above keys: no overlap possible
      legend.title      = element_text(hjust = 0.5, size = 12, family = "Latin Modern Roman"),
      legend.text       = element_text(size = 11, family = "Latin Modern Roman",
                                       margin = margin(l = 3, r = 0)),
      legend.key.width  = unit(0.5, "cm"),
      # generous gap between entries: survives small text-width mismeasurements
      legend.key.spacing.x = unit(1.2, "cm"),
      legend.spacing.x  = unit(1.5, "cm")
    ) +
    transition_states(phase, transition_length = 3, state_length = 7, wrap = FALSE) +
    view_follow(fixed_x = TRUE) +
    ease_aes("cubic-in-out")
  
  # ---- Render -----------------------------------------------------------------
  anim <- animate(
    plot_anim,
    fps = 15, duration = 12,
    width = 1200, height = 675,
    device = "ragg_png",                 # reliable fonts (Latin Modern) and glyphs (kappa, beta)
    renderer = gifski_renderer(loop = TRUE)
  )
  
  print(anim)
  
  if(save==T){
    anim_save("./figures/animation_country_gdp.gif", animation = anim)
  }
  
}



