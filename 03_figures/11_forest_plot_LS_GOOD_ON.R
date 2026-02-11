###############################################################################
# Forest plot – LS subgroup
# Outcome: Good ON time (LS-mean Difference)
###############################################################################

weights_model   <- weights(model_LS_GOODON)
weights_percent <- round(weights_model / sum(weights_model) * 100, 2)
k <- model_LS_GOODON$k

windows(15, 10)

# Centered layout

xlim_expanded <- c(-5.5, 9.5)

forest(
  model_LS_GOODON,
  slab = LS_scales$REF_ID,
  xlab = "[reduction in good ON time] <---------- Good ON time ----------> [increase in good ON time]",
  mlab = "",
  xlim = xlim_expanded,
  alim = c(-1, 5),
  at = seq(-1, 5, by = 1),
  cex = 0.8,
  header = TRUE
)

# Study weights (%)

text(
  x = 6.5,
  y = k:1,
  labels = sprintf("%.2f%%", weights_percent),
  cex = 0.75,
  adj = 1,
  xpd = TRUE
)

text(
  x = 6.5,
  y = k + 2,
  labels = "Weight",
  font = 2,
  cex = 0.85,
  adj = 1,
  xpd = TRUE
)

# Panel label

usr <- par("usr")

text(
  x = usr[1],
  y = usr[4] + 0.5,
  labels = "(B)",
  font = 2,
  cex = 1.2,
  adj = c(0, 1),
  xpd = NA
)

# Heterogeneity statistics

text(
  x = -4.5,
  y = -1.3,
  labels = paste0(
    "I\u00B2 = ", round(model_LS_GOODON$I2, 1), "%, ",
    "\u03C4\u00B2 = ", round(model_LS_GOODON$tau2, 3), ", ",
    "p = ", ifelse(model_LS_GOODON$QEp < 0.001,
                   "< 0.001",
                   round(model_LS_GOODON$QEp, 3))
  ),
  cex = 0.8,
  adj = 0,
  xpd = TRUE
)
