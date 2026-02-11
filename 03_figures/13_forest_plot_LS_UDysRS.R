###############################################################################
# Forest Plot – LS Subgroup
# Outcome: UDysRS
# Scale: -20 to 5
###############################################################################

# Weights

weights_model   <- weights(model_LS_UDYSRS)
weights_percent <- round(weights_model / sum(weights_model) * 100, 2)
k <- model_LS_UDYSRS$k

windows(15, 10)
xlim_expanded <- c(-38.75, 23.75)

forest(
  model_LS_UDYSRS,
  slab   = LS_scales$REF_ID,
  xlab   = "[improvement] <------------------- UDysRS score -------------------> [worsening]",
  mlab   = "",
  xlim   = xlim_expanded,
  alim   = c(-20, 5),
  at     = seq(-20, 5, by = 5),
  cex    = 0.8,
  header = TRUE
)

# Study Weights (%)

text(
  x = 14,
  y = k:1,
  labels = sprintf("%.2f%%", weights_percent),
  cex = 0.75,
  adj = 1,
  xpd = TRUE
)

text(
  x = 14,
  y = k + 2,
  labels = "Weight",
  font = 2,
  cex = 0.85,
  adj = 1,
  xpd = TRUE
)

# Panel Label

usr <- par("usr")

text(
  x = usr[1],
  y = usr[4] + 0.5,
  labels = "(C)",
  font = 2,
  cex = 1.2,
  adj = c(0, 1),
  xpd = NA
)

# Heterogeneity Statistics

text(
  x = -34,
  y = -1,
  labels = paste0(
    "I\u00B2 = ", round(model_LS_UDYSRS$I2, 1), "%, ",
    "\u03C4\u00B2 = ", round(model_LS_UDYSRS$tau2, 3), ", ",
    "p = ", ifelse(model_LS_UDYSRS$QEp < 0.001,
                   "< 0.001",
                   round(model_LS_UDYSRS$QEp, 3))
  ),
  cex = 0.8,
  adj = 0,
  xpd = TRUE
)
