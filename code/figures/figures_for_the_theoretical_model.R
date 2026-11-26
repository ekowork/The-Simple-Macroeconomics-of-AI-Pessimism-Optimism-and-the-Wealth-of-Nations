

#-----------------------------------------------------###
# Figure 1: Minimum viable scale and task distribution
# Single-capability version: only kappa_1
#-----------------------------------------------------###


#-----------------------------------------------------###
# 1. Parameters
#-----------------------------------------------------###

# Only one capability
z1 <- 0.4

p <- list(b = .7, theta = 1, mu0 = 0)

t1 <- -m_fn(z1, theta = p$theta, mu0 = p$mu0)

orange <- "#F06436"
purple <- "#4939AA"
blue   <- "#2878DB"
grey   <- "#C5C3B9"

xmin <- -3.8
xmax <- 5.0

x <- sort(unique(c(
  seq(xmin, xmax, length.out = 2000),
  t1, 0
)))

laplace <- function(x) {
  exp(-abs(x) / p$b) / (2 * p$b)
}

df <- tibble(
  x = x,
  human = 1,
  ai1 = exp(-p$theta * z1) + exp(-p$mu0 - x),
  density = laplace(x)
)

# Exact shaded interval: viable at kappa_1
shade_old <- tibble(
  x = seq(t1, xmax, length.out = 500)
) %>%
  mutate(density = laplace(x))

#-----------------------------------------------------###
# 2. Rich text helper
#-----------------------------------------------------###

rich_label <- function(x, y, label, colour,
                       size = 4.5, hjust = 0.5,
                       vjust = 0.5) {
  
  ggtext::geom_richtext(
    data = data.frame(
      x = x,
      y = y,
      label = label
    ),
    mapping = aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    colour = colour,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    family = "Latin Modern Math",
    size = size,
    hjust = hjust,
    vjust = vjust,
    show.legend = FALSE
  )
}

#-----------------------------------------------------###
# 3. Common theme
#-----------------------------------------------------###

theme_scale <- theme_paper(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.line = element_line(
      colour = grey,
      linewidth = 0.45
    ),
    
    axis.title = element_text(
      colour = "grey30",
      size = 16
    ),
    
    axis.title.x = element_text(
      family = "Latin Modern Math",
      margin = margin(t = 6)
    ),
    
    plot.title = element_text(
      size = 17,
      face = "plain",
      hjust = 0.5,
      colour = "black",
      margin = margin(b = 10)
    ),
    
    plot.margin = margin(10, 10, 8, 8)
  )

#-----------------------------------------------------###
# 4. Panel A: Break-even threshold
#-----------------------------------------------------###

a <- ggplot(df, aes(x = x)) +
  
  # Human labour cost
  geom_hline(
    yintercept = 1,
    colour = blue,
    linewidth = 1.1
  ) +
  
  # AI average cost
  geom_line(
    aes(y = ai1),
    colour = orange,
    linewidth = 1.1,
    linetype = "longdash"
  ) +
  
  # Minimum viable scale threshold
  annotate(
    "segment",
    x = t1, xend = t1,
    y = 0, yend = 1,
    colour = "grey60",
    linewidth = 0.6,
    linetype = "dotted"
  ) +
  
  # Intersection
  annotate(
    "point",
    x = t1, y = 1,
    colour = orange,
    size = 3
  ) +
  
  # Human labour label
  annotate(
    "text",
    x = xmin + 0.4,
    y = 1.10,
    label = "human labour",
    colour = blue,
    family = FIGURE_FONT_FAMILY,
    hjust = 0,
    size = 4.8
  ) +
  
  # AI label
  rich_label(
    t1 + 0.75, 0.86,
    "AI system, capability κ<sub>1</sub>",
    orange,
    size = 4.3,
    hjust = 0
  ) +
  
  # Threshold label
  rich_label(
    t1 + 0.12, 0.12,
    "W<sup>*</sup>(κ<sub>1</sub>)",
    orange,
    size = 4.4,
    hjust = 0
  ) +
  
  scale_x_continuous(
    breaks = 0,
    labels = "ν",
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    breaks = NULL,
    expand = expansion(mult = 0)
  ) +
  
  coord_cartesian(
    xlim = c(xmin, xmax),
    ylim = c(0, 2.35),
    clip = "on"
  ) +
  
  labs(
    #title = "(a) The break-even threshold to automate a task",
    x = "Task Size W",
    y = "Average Cost to Perform Task"
  ) +
  
  theme_scale 

#-----------------------------------------------------###
# 5. Panel B: Distribution of task size
#-----------------------------------------------------###

b <- ggplot(df, aes(x = x)) +
  
  # Already viable deployments
  geom_area(
    data = shade_old,
    aes(y = density),
    fill = orange,
    alpha = 0.23
  ) +
  
  # Laplace density
  geom_line(
    aes(y = density),
    colour = purple,
    linewidth = 1.2
  ) +
  
  # Viability threshold
  geom_vline(
    xintercept = t1,
    colour = orange,
    linewidth = 0.9,
    linetype = "longdash"
  ) +
  
  # Viable area label
  rich_label(
    t1 + 1.1,
    0.14,
    "viable at κ<sub>1</sub>",
    orange,
    size = 4.4
  ) +
  
  scale_x_continuous(
    breaks = 0,
    labels = "ν",
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    breaks = NULL,
    expand = expansion(mult = 0)
  ) +
  
  coord_cartesian(
    xlim = c(xmin, xmax),
    ylim = c(0, 0.80),
    clip = "on"
  ) +
  
  labs(
    #title = "(b) The distribution of task size",
    x = "Task Size ln(W)",
    y = "Density of Tasks"
  ) +
  
  theme_scale +
  
  theme(
    axis.text.x = element_text(
      family = "Latin Modern Math",
      size = 13,
      colour = "grey50"
    )
  )

#-----------------------------------------------------###
# 6. Combine and display
#-----------------------------------------------------###

p_scale_threshold_single <-
  a + b +
  plot_layout(
    ncol = 2,
    widths = c(1, 1)
  )

print(p_scale_threshold_single)

#-----------------------------------------------------###
# 7. Export through your existing save_fig()
#-----------------------------------------------------###


save_fig(
  "figure1_scale_threshold_single_kappa1",
  plot = p_scale_threshold_single,
  type = "wide",
  placement = "full",
  format = "pdf"
)


# figure 2

#-----------------------------------------------------###
# 1. Parameters
#-----------------------------------------------------###

p <- list(b = .7, theta = 1, mu0 = 0)
z <- c(.4, 1.3)

thr <- -m_fn(z, theta = p$theta, mu0 = p$mu0)

stopifnot(
  length(thr) == 2,
  all(is.finite(thr)),
  thr[2] < thr[1]
)

t1 <- thr[1]  # lower capability
t2 <- thr[2]  # higher capability

green  <- "#12AE7B"
orange <- "#F06436"
purple <- "#4939AA"
blue   <- "#2878DB"
grey   <- "#C5C3B9"

green  <- "#019875"  # κ2 – světle modrá
orange <- "#e76254"  # κ1 – červená
purple <- "#1E466E"  # hustota – tmavě modrá
blue   <- "#72BCD5"  # human labour – oranžová
grey   <- "#C5C3B9"  

xmin <- -3.8
xmax <- 5.0

x <- sort(unique(c(
  seq(xmin, xmax, length.out = 2000),
  t1, t2, 0
)))



df <- tibble(
  x = x,
  human = 1,
  ai1 = exp(-p$theta * z[1]) + exp(-p$mu0 - x),
  ai2 = exp(-p$theta * z[2]) + exp(-p$mu0 - x),
  density = laplace(x)
)

# Exact shading intervals
shade_new <- tibble(
  x = seq(t2, t1, length.out = 500)
) %>%
  mutate(density = laplace(x))

shade_old <- tibble(
  x = seq(t1, xmax, length.out = 500)
) %>%
  mutate(density = laplace(x))

#-----------------------------------------------------###
# 2. Rich text helper
#-----------------------------------------------------###

rich_label <- function(x, y, label, colour,
                       size = 4.5, hjust = 0.5,
                       vjust = 0.5) {
  
  ggtext::geom_richtext(
    data = data.frame(
      x = x,
      y = y,
      label = label
    ),
    mapping = aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    colour = colour,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    family = "Latin Modern Math",
    size = size,
    hjust = hjust,
    vjust = vjust,
    show.legend = FALSE
  )
}

#-----------------------------------------------------###
# 3. Common theme
#-----------------------------------------------------###

theme_scale <- theme_paper(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.line = element_line(
      colour = grey,
      linewidth = 0.45
    ),
    
    axis.title = element_text(
      colour = "grey30",
      size = 16
    ),
    
    axis.title.x = element_text(
      family = "Latin Modern Math",
      margin = margin(t = 6)
    ),
    
    plot.title = element_text(
      size = 17,
      face = "plain",
      hjust = 0.5,
      colour = "black",
      margin = margin(b = 10)
    ),
    
    plot.margin = margin(10, 10, 8, 8)
  )

#-----------------------------------------------------###
# 4. Panel A: Minimum viable scale
#-----------------------------------------------------###

a <- ggplot(df, aes(x = x)) +
  
  # Human labour cost
  geom_hline(
    yintercept = 1,
    colour = blue,
    linewidth = 1.1
  ) +
  
  # AI average costs
  geom_line(
    aes(y = ai1),
    colour = orange,
    linewidth = 1.1,
    linetype = "longdash"
  ) +
  
  geom_line(
    aes(y = ai2),
    colour = green,
    linewidth = 1.1,
    linetype = "longdash"
  ) +
  
  # Minimum viable scale thresholds
  annotate(
    "segment",
    x = t1, xend = t1,
    y = 0, yend = 1,
    colour = "grey60",
    linewidth = 0.6,
    linetype = "dotted"
  ) +
  
  annotate(
    "segment",
    x = t2, xend = t2,
    y = 0, yend = 1,
    colour = "grey60",
    linewidth = 0.6,
    linetype = "dotted"
  ) +
  
  # Intersections
  annotate(
    "point",
    x = t1, y = 1,
    colour = orange,
    size = 3
  ) +
  
  annotate(
    "point",
    x = t2, y = 1,
    colour = green,
    size = 3
  ) +
  
  # Human labour label
  annotate(
    "text",
    x = xmin + 0.4,
    y = 1.10,
    label = "human labour",
    colour = blue,
    family = FIGURE_FONT_FAMILY,
    hjust = 0,
    size = 4.8
  ) +
  
  # AI labels with correctly rendered subscripts
  rich_label(
    t1 + .8, 0.87,
    "AI system, capability κ<sub>1</sub>",
    orange,
    size = 4.3,
    hjust = 0
  ) +
  
  rich_label(
    t2 + 1.6, 0.51,
    "AI system, capability κ<sub>2</sub> &gt; κ<sub>1</sub>",
    green,
    size = 4.3,
    hjust = 0
  ) +
  
  # Threshold labels
  rich_label(
    t2 - 0.12, 0.12,
    "W<sup>*</sup>(κ<sub>2</sub>)",
    green,
    size = 4.4,
    hjust = 1
  ) +
  
  rich_label(
    t1 + 0.12, 0.12,
    "W<sup>*</sup>(κ<sub>1</sub>)",
    orange,
    size = 4.4,
    hjust = 0
  ) +
  
  scale_x_continuous(
    breaks = 0,
    labels = "ν",
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    breaks = NULL,
    expand = expansion(mult = 0)
  ) +
  
  coord_cartesian(
    xlim = c(xmin, xmax),
    ylim = c(0, 2.35),
    clip = "on"
  ) +
  
  labs(
   # title = "(a) The break-even threshold to automate a task",
    x = "Task Size W",
    y = "Average Cost to Perform Task"
  ) +
  
  theme_scale

#-----------------------------------------------------###
# 5. Panel B: Distribution of task size
#-----------------------------------------------------###

b <- ggplot(df, aes(x = x)) +
  
  # Newly viable deployments
  geom_area(
    data = shade_new,
    aes(y = density),
    fill = green,
    alpha = 0.25
  ) +
  
  # Already viable deployments
  geom_area(
    data = shade_old,
    aes(y = density),
    fill = orange,
    alpha = 0.23
  ) +
  
  # Laplace density
  geom_line(
    aes(y = density),
    colour = purple,
    linewidth = 1.2
  ) +
  
  # Viability thresholds
  geom_vline(
    xintercept = t2,
    colour = green,
    linewidth = 0.9,
    linetype = "longdash"
  ) +
  
  geom_vline(
    xintercept = t1,
    colour = orange,
    linewidth = 0.9,
    linetype = "longdash"
  ) +
  
  # Capability arrow
  annotate(
    "segment",
    x = t1 - 0.05,
    xend = t2 + 0.05,
    y = 0.55,
    yend = 0.55,
    colour = "grey35",
    linewidth = 0.7,
    arrow = grid::arrow(
      length = grid::unit(0.18, "cm")
    )
  ) +
  
  annotate(
    "text",
    x = mean(c(t1, t2)),
    y = 0.595,
    label = "capability rises",
    colour = "grey30",
    family = FIGURE_FONT_FAMILY,
    size = 4.6
  ) +
  
  # Newly viable area
  rich_label(
    mean(c(t1, t2)) + 0.4,
    0.37,
    "newly viable<br>at κ<sub>2</sub>",
    green,
    size = 4.4
  ) +
  
  # Already viable area
  rich_label(
    t1 + 1.1,
    0.13,
    "viable at κ<sub>1</sub>",
    orange,
    size = 4.4
  ) +
  
  scale_x_continuous(
    breaks = 0,
    labels = "ν",
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    breaks = NULL,
    expand = expansion(mult = 0)
  ) +
  
  coord_cartesian(
    xlim = c(xmin, xmax),
    ylim = c(0, 0.80),
    clip = "on"
  ) +
  
  labs(
   # title = "(b)  the distribution of task size",
    x = "Task Size ln(W)",
    y = "Density of Tasks"
  ) +
  
  theme_scale +
  
  theme(
    axis.text.x = element_text(
      family = "Latin Modern Math",
      size = 13,
      colour = "grey50"
    )
  )

#-----------------------------------------------------###
# 6. Combine and display
#-----------------------------------------------------###

p_scale_threshold <-
  a + b +
  plot_layout(
    ncol = 2,
    widths = c(1, 1)
  )

print(p_scale_threshold)

#-----------------------------------------------------###
# 7. Export through your existing save_fig()
#-----------------------------------------------------###

save_fig(
  "figure1_scale_threshold",
  plot = p_scale_threshold,
  type = "wide",
  placement = "full",
  format = "png"
)

save_fig(
  "figure1_scale_threshold",
  plot = p_scale_threshold,
  type = "wide",
  placement = "full",
  format = "pdf"
)



save_fig("figure1_mpv", a, type = "tall", placement = "half",  format = "pdf")
save_fig("figure1_dist", b, type = "tall", placement = "half", format = "pdf")














#-----------------------------------------------------###
# Figures 2 & 3: Cohort margins (dispersion / fixed cost)
#-----------------------------------------------------###



library(MetBrewer)


z_grid <- seq(0, 6, length.out = 601)
measures <- c(
  "(a) Viable Share of Wage Bill: 𝜄(𝑧)",
  "(b) Savings per Viable Wage Bill: 𝜆(𝑧)",
  "(c) Saving of the Cohort Wage Bill: 𝜋(𝑧)"
) |> as.factor()
plot_margins <- function(params, labels, filename, legend_labels = labels) {
  
  results <- lapply(params, function(p) {
    
    gamma <- gamma_fn(z_grid, p$theta)
    iota  <- iota_fn(z_grid, p$theta, p$mu0, p$b)
    pi    <- pi_fn(z_grid, p$theta, p$mu0, p$b)
    
    list(
      gamma = gamma,
      iota = iota,
      lambda = ifelse(iota > 0, pi / iota, 0),
      pi = pi
    )
  })
  
  df_margins <- dplyr::bind_rows(lapply(seq_along(results), function(i) {
    data.frame(
      z = rep(z_grid, 3),
      value = c(results[[i]]$iota, results[[i]]$lambda, results[[i]]$pi),
      measure = rep(measures, each = length(z_grid)),
      scenario = labels[i]
    )
  }))
  
  df_margins$measure <- factor(df_margins$measure, levels = measures)
  df_margins$scenario <- factor(df_margins$scenario, levels = labels)
  
  
  
  # Original benchmark: gross saving in the middle panel only.
  gross <- data.frame(
    z = z_grid,
    value = results[[1]]$gamma,
    measure = factor(measures[2], levels = measures)
  )
  
  p_margins <- ggplot(df_margins,
                      aes(x = z, y = value, colour = scenario,
                          linetype = scenario)) +
    geom_line(data = gross, aes(x = z, y = value),
              inherit.aes = FALSE, colour = "black",
              linetype = "dotted", linewidth = 0.8) +
    geom_line(linewidth = 1.1) +
    geom_text(data = data.frame(z = 4, value = 0.93,
                                measure = measures[2], label = "Gross saving: 𝛾(𝑧)"),
              aes(x = z, y = value, label = label, family = "Latin Modern Math"),
              inherit.aes = FALSE, colour = "black", size = 3.4) +
    facet_wrap(~measure, nrow = 1) +
    scale_colour_manual(
      values = setNames(c(orange, green, purple), labels),
      labels = legend_labels
    ) +
    scale_linetype_manual(
      values = setNames(c("solid", "dashed", "dotdash"), labels),
      labels = legend_labels
    ) +
    scale_x_continuous(breaks = c(0, 2, 4, 6)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    coord_cartesian(xlim = c(0, 6), ylim = c(0, 1)) +
    labs(x = expression("Capability Surplus " * z == kappa - d),
         y  = "",
         colour = NULL, linetype = NULL) +
    theme_paper(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.background = element_blank(),
      legend.text = element_text(
        family = "Latin Modern Math",
        size = 14
      )
    )
  
  p_margins$theme$strip.text <- element_text(
    family = "Latin Modern Math",
    size = 14
  )
  
  print(p_margins)
  save_fig(filename, plot = p_margins,
           type = "wide", placement = "full", format = "pdf")
  invisible(p_margins)
}

# Figure 2: Task-size dispersion
b_values <- c(0.85, 0.60, 0.35)

p_dispersion <- plot_margins(
  params = lapply(b_values, function(b)
    list(b = b, theta = 1, mu0 = 0)),
  labels = paste0("b = ", b_values),
  filename = "figure2_dispersion"
)


# Figure 3: Fixed deployment cost
mu_values <- c(1, 0, -1)

p_fixed_cost <- plot_margins(
  params = lapply(mu_values, function(mu)
    list(b = .6, theta = 1, mu0 = mu)),
  labels = paste0("mu0 = ", mu_values),
  legend_labels = parse(text = paste0("mu[0] == ", mu_values)),
  filename = "figure3_fixed_cost"
)



#-----------------------------------------------------###
# Figure 4: Calibrated savings
#-----------------------------------------------------###

baseline <- pi_fn(
  z_grid, theta = .481, mu0 = -1.75, b = .7
)

updated <- pi_fn(
  z_grid, theta = .481, mu0 = .0189, b = .7
)

# series_names <- c(
#   "Optimism (𝜃 = 0.65, 𝜇₀ = −0.20)",
#   "Baseline (𝜃 = 0.48, 𝜇₀ = −1.74)",
#   "Acemoglu Constant (𝜄 × 𝜆 = 0.23 × 0.27)"
# )


series_names <- c(
  "Optimism",
  "Matched to Acemoglu (Baseline)",
  "Acemoglu Constant"
)

# series_labels <- expression(
#   paste("Optimism (", theta, " = 0.65, ", mu[0], " = -0.20)"),
#   paste("Baseline (", theta, " = 0.48, ", mu[0], " = -1.74)"),
#   paste("Acemoglu Constant (", iota, " × ", lambda, " = 0.23 × 0.27)")
# )


df_savings <- data.frame(
  z = rep(z_grid, 3),
  pi = c(
    updated,
    baseline,
    rep(.23 * .27, length(z_grid))
  ),
  series = factor(
    rep(series_names, each = length(z_grid)),
    levels = series_names
  )
)

my_pal <- met.brewer("Hiroshige", n = 8)

p_savings <- ggplot(
  df_savings,
  aes(x = z, y = pi, colour = series, linetype = series)
) +
  
  # Baseline: solid line
  geom_line(
    data = df_savings %>% filter(series == "Matched to Acemoglu (Baseline)"),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = df_savings %>%
      filter(series == "Matched to Acemoglu (Baseline)",
             abs(z - round(z)) < 1e-8),
    size = 3
  ) +
  
  # Optimism: thicker dashed line
  geom_line(
    data = df_savings %>% filter(series == "Optimism"),
    linewidth = 2
  ) +
  
  # Fixed adoption and savings: dots only
  geom_point(
    data = df_savings %>%
      filter(series == "Acemoglu Constant") %>%
      slice(seq(1, n(), by = 12)),
    size = 1.3
  ) +
  
  # Colours
  scale_colour_manual(
    values = setNames(c(my_pal[8],my_pal[8],my_pal[1]), series_names),
    breaks = series_names,
    labels = series_names,
    name = NULL
  ) +
  
  # Line types
  scale_linetype_manual(
    values = setNames(c("dotdash", "solid", "dotted"), series_names),
    breaks = series_names,
    labels = series_names,
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = 0:6
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 0.6, 0.1)
  ) +
  
  coord_cartesian(
    xlim = c(0, 6),
    ylim = c(0, 0.6)
  ) +
  
  labs(
    x = expression("Capability Surplus " * z == kappa - d),
    #y = "Saving per Cohort Wage Dollar: π(z)",
    y = "",
    colour = NULL,
    linetype = NULL
  ) +
  
  theme_paper(base_size = 15) +
  
  theme(
    axis.title = element_text(color = "grey30"),
    axis.title.y = element_text(family = "Latin Modern Math"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95"),
    
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.92),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = "grey75"),
    legend.title = element_blank(),
    legend.direction = "vertical",
    legend.text = element_text(
      family = "Latin Modern Math",
      size = 13
    )
  )


print(p_savings)

save_fig(
  "figure4_calibrated_savings",
  plot = p_savings,
  type = "wide",
  placement = "full",
  format = "pdf"
)
