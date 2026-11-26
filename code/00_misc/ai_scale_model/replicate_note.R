# Deterministic replication of Figures 1--4 and the cohort-level calculations.
# From the parent directory: Rscript ai_scale_model/replicate_note.R
# No empirical US/global GDP estimates are fabricated when the input data are absent.
source("ai_scale_model/ai_scale_model.R")
outdir <- "ai_scale_model/replication"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

cols <- c("#147D92", "#CA694C", "#716499")
grey <- "#8A9198"
open_plot <- function(name, width = 13, height = 4.8) {
  png(file.path(outdir, name), width = width, height = height,
      units = "in", res = 180, type = "cairo", bg = "white")
  par(family = "sans", col = "#253341", col.axis = "#536170",
      col.lab = "#253341", fg = "#253341", las = 1, cex = 1,
      mar = c(4.4, 4.0, 3.4, 1.2), mgp = c(2.6, .7, 0), tcl = -.2,
      bty = "l", xaxs = "i", yaxs = "i")
}

# Figure 1: normalized cost = variable cost + F/W. Both thresholds refer to
# a single fixed difficulty cohort. Density areas in panel B are COUNT shares.
open_plot("figure1_scale_threshold.png", height = 5.1)
par(mfrow = c(1, 2), oma = c(2.1, 0, .5, 0))
p <- scale_params(.7, 1, 0)
z1 <- .4; z2 <- 1.3
x <- seq(-3.5, 4, length.out = 1001)
thr <- scale_cohort(c(z1, z2), p)$log_wstar_over_median
plot(x, rep(1, length(x)), type = "l", lwd = 2, col = "#535C64",
     xlim = range(x), ylim = c(0, 2.5),
     xlab = expression("Log wage pool relative to median: " * ln(W / e^nu)),
     ylab = "Cost per dollar of displaced labour", main = "A  |  Break-even scale")
lines(x, exp(-p$theta * z1) + exp(-p$mu0 - x), col = cols[1], lwd = 2.4)
lines(x, exp(-p$theta * z2) + exp(-p$mu0 - x), col = cols[2], lwd = 2.4)
for (j in 1:2) segments(thr[j], 0, thr[j], 1, lty = 3, col = cols[j])
points(thr, c(1, 1), pch = 21, bg = cols[1:2], col = "white", cex = 1.2)
legend("topleft", c("Human labour", "AI: depth z = 0.4", "AI: depth z = 1.3"),
       col = c("#535C64", cols[1:2]), lty = 1, lwd = 2, bty = "n", cex = .83)
density <- exp(-abs(x) / p$b) / (2 * p$b)
plot(x, density, type = "n", xlim = range(x), ylim = c(0, .8),
     xlab = expression("Log wage pool relative to median: " * ln(W / e^nu)),
     ylab = "Density of log deployment wage pools", main = "B  |  Newly viable deployments")
shade <- function(lo, hi, colour) {
  xx <- seq(lo, hi, length.out = 301)
  yy <- exp(-abs(xx) / p$b) / (2 * p$b)
  polygon(c(lo, xx, hi), c(0, yy, 0), col = adjustcolor(colour, .30), border = NA)
}
shade(thr[2], thr[1], cols[2]); shade(thr[1], max(x), cols[1])
lines(x, density, lwd = 2, col = "#535C64")
for (j in 1:2) abline(v = thr[j], lty = 3, col = cols[j])
legend("topright", c("Newly viable at z = 1.3", "Already viable at z = 0.4"),
       fill = adjustcolor(cols[c(2, 1)], .30), border = NA, bty = "n", cex = .8)
mtext("Illustrative parameters: b = 0.7, theta = 1, mu0 = 0. Shaded density areas are deployment counts; iota uses wage-bill weights.",
      side = 1, outer = TRUE, line = .5, cex = .8, col = "#536170")
dev.off()

# Figures 2 and 3 use the illustrative values specified in the note.
z <- seq(0, 6, length.out = 601)
titles <- c("A  |  Viable wage-bill share", "B  |  Net saving among adopters",
            "C  |  Saving across the cohort")
ylabs <- c(expression(iota(z)), expression(lambda(z)), expression(pi(z)))
series <- c("iota", "lambda", "pi")
plot_margins <- function(filename, parameters, labels, subtitle) {
  open_plot(filename)
  par(mfrow = c(1, 3), oma = c(1.6, 0, 1.3, 0))
  results <- lapply(parameters, function(p) scale_cohort(z, p))
  for (j in 1:3) {
    plot(z, rep(0, length(z)), type = "n", xlim = c(0, 6), ylim = c(0, 1),
         xlab = expression("Capability surplus " * z == kappa - d),
         ylab = ylabs[[j]], main = titles[j], cex.main = .92)
    abline(h = c(.25, .5, .75, 1), col = "#EDF0F2", lwd = .8)
    if (j == 2) lines(z, results[[1]]$gamma, lwd = 1.8, lty = 3, col = grey)
    for (i in seq_along(results)) lines(z, results[[i]][[series[j]]],
      col = cols[i], lwd = 2.5, lty = i)
    if (j == 1) legend("bottomright", labels, col = cols, lwd = 2.5,
                      lty = seq_along(labels), bty = "n", cex = .9)
    if (j == 2) text(3.7, .93, "Gross saving", col = grey, cex = .8)
  }
  mtext(subtitle, side = 3, outer = TRUE, cex = .88, col = "#536170")
  mtext("Shares are fractions. Net savings deduct the fixed cost of deployment.",
        side = 1, outer = TRUE, cex = .8, col = "#536170")
  dev.off()
  invisible(results)
}
b_values <- c(.85, .6, .35)
figure2 <- plot_margins("figure2_dispersion.png",
  lapply(b_values, function(b) scale_params(b, 1, 0)),
  paste0("b = ", b_values), "Task-size dispersion varies; theta = 1 and mu0 = 0")
mu_values <- c(1, 0, -1)
figure3 <- plot_margins("figure3_fixed_cost.png",
  lapply(mu_values, function(mu) scale_params(.6, 1, mu)),
  paste0("mu0 = ", mu_values),
  "Fixed deployment cost varies; b = 0.6 and theta = 1. The two curves with mu0 <= 0 coincide for lambda.")

# Figure 4 uses rounded reported parameters. This is an algebraic replication,
# not an independently estimated calibration on the unavailable difficulty data.
baseline <- scale_cohort(z, scale_preset("acemoglu"))
updated <- scale_cohort(z, scale_preset("updated"))
open_plot("figure4_calibrated_savings.png", width = 9, height = 5.5)
par(oma = c(2.2, 0, 0, 0))
plot(z, updated$pi, type = "l", ylim = c(0, .6), lwd = 2.7, lty = 2,
     col = cols[2], xlab = expression("Capability surplus " * z == kappa - d),
     ylab = expression("Net saving per cohort wage dollar: " * pi(z)),
     main = "Cohort savings under the two reported calibrations")
abline(h = seq(.1, .6, .1), col = "#EDF0F2")
lines(z, updated$pi, lwd = 2.7, lty = 2, col = cols[2])
lines(z, baseline$pi, lwd = 2.7, col = cols[1])
abline(h = .23 * .27, col = grey, lty = 3, lwd = 1.8)
ex <- scale_cohort(1, scale_preset("acemoglu"))
points(1, ex$pi, pch = 21, bg = cols[1], col = "white", cex = 1.3)
text(1.1, .032, sprintf("z = 1: pi = %.4f", ex$pi), adj = 0, cex = .85)
legend("topleft", c("Updated: theta = 0.65, mu0 = -0.20",
                    "Acemoglu anchor: theta = 0.48, mu0 = -1.74",
                    "Frozen Acemoglu product: 0.23 x 0.27"),
       col = c(cols[2], cols[1], grey), lty = c(2, 1, 3),
       lwd = c(2.7, 2.7, 1.8), bty = "n", cex = .85)
mtext("b = 0.7. Parameters are taken from Table 2. The horizontal axis is cohort depth, not economy-wide capability.",
      side = 1, outer = TRUE, line = .4, cex = .78, col = "#536170")
dev.off()

# Reproducible numeric outputs, including capability and cost ceilings.
cohort_tables <- lapply(c("acemoglu", "updated", "platform"), function(name) {
  result <- scale_cohort(c(0, .1, .5, 1, 2, 3, 4, 5, 6, Inf), scale_preset(name))
  data.frame(scenario = name, result)
})
cohort_table <- do.call(rbind, cohort_tables)
write.csv(cohort_table, file.path(outdir, "cohort_reference_values.csv"), row.names = FALSE)
curves <- rbind(data.frame(scenario = "acemoglu", baseline),
                data.frame(scenario = "updated", updated))
write.csv(curves, file.path(outdir, "figure4_curves.csv"), row.names = FALSE)
print(cohort_table[cohort_table$z == 1 | is.infinite(cohort_table$z),
                   c("scenario", "z", "gamma", "iota", "lambda", "pi", "p_adopt")])
cat("\nFigures 1--4 and cohort tables written to", outdir, "\n")
cat("The 9.3%, 20% and 21% aggregate GDP claims require the original difficulty/compensation data.\n")
