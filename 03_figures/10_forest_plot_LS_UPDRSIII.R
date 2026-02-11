###############################################################################
# Forest plot – LS subgroup
# Outcome: UPDRS-III (LS-mean Difference)
###############################################################################

# Assumed object:
# - model_LS_UPDRSIII (rma object)

# Study weights (%)

weights_model   <- weights(model_LS_UPDRSIII)
weights_percent <- round(weights_model / sum(weights_model) * 100, 2)

k <- model_LS_UPDRSIII$k

# Expanded x-axis limits to create space for weight column
windows(15, 10)
xlim_expanded <- c(-25, 25)

# Forest plot

forest(
  model_LS_UPDRSIII,
  slab = LS_scales$REF_ID,
  xlab = "[improvement in motor function] <---------- UPDRS part III scores ----------> [worsening in motor function]",
  mlab = "",
  xlim = xlim_expanded,
  alim = c(-10, 10),
  at = seq(-10, 10, by = 5),
  cex = 0.8,
  header = TRUE
)

# Add study weights (%)

text(
  x = 16,
  y = k:1,
  labels = sprintf("%.2f%%", weights_percent),
  cex = 0.75,
  adj = 1
)

text(
  x = 16,
  y = k + 2,
  labels = "Weight",
  font = 2,
  cex = 0.85,
  adj = 1
)

# ---------------------------------------------------------------------------
# Panel label (A)

usr <- par("usr")

text(
  x = usr[1],
  y = usr[4] + 0.5,
  labels = "(A)",
  font = 2,
  cex = 1.2,
  adj = c(0, 1),
  xpd = NA
)

# Heterogeneity statistics

text(
  x = -23,
  y = -1,
  labels = paste0(
    "I\u00B2 = ", round(model_LS_UPDRSIII$I2, 1), "%, ",
    "tau\u00B2 = ", round(model_LS_UPDRSIII$tau2, 3), ", ",
    "p = ", ifelse(model_LS_UPDRSIII$QEp < 0.001,
                   "< 0.001",
                   round(model_LS_UPDRSIII$QEp, 3))
  ),
  cex = 0.8,
  adj = 0,
  xpd = NA
)
