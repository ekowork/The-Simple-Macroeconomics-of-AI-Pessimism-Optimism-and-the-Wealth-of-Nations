
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



