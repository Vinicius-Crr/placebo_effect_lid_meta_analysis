###############################################################################
# Forest plot – Main meta-analysis (LS)
###############################################################################

# Model and data are assumed to be already available:
# - model_LS (rma object)
# - LS_main (data.frame)

# ---------------------------------------------------------------------------
# Study weights 

weights_model <- weights(model_LS)
weights_percent <- round(weights_model / sum(weights_model) * 100, 2)

# Forest plot

# Expanded x-axis limits to create space for the weight column
xlim_expanded <- c(-4, 3)

forest(
  model_LS,
  slab = LS_main$REF_ID,
  xlab = "[reduction in dyskinesia] <---------------------> [increase in dyskinesia]",
  mlab = "",                      # remove default model label
  xlim = xlim_expanded,
  alim = c(-1.51, 1),              # effect size scale
  at = seq(-1.5, 1, by = 0.5),
  cex = 0.8,
  header = TRUE
)

# Add study weights (%)

k <- length(LS_main$REF_ID)

text(
  x = 1.75,
  y = k:1,
  labels = sprintf("%.2f%%", weights_percent),
  cex = 0.75,
  adj = 1
)

text(
  x = 1.75,
  y = k + 2,
  labels = "Weight",
  font = 2,
  cex = 0.85,
  adj = 1
)

# Heterogeneity statistics

text(
  x = -3.85,
  y = -0.5,
  labels = paste0(
    "I\u00B2 = ", round(model_LS$I2, 1), "%, ",
    "tau\u00B2 = ", round(model_LS$tau2, 3), ", ",
    "p = ", ifelse(model_LS$QEp < 0.001, "< 0.001", round(model_LS$QEp, 3))
  ),
  cex = 0.8,
  adj = 0
)

text(
  x = -3.85,
  y = -1.5,
  labels = "Effect sizes are shown as standardized mean differences",
  cex = 0.7,
  adj = 0
)
