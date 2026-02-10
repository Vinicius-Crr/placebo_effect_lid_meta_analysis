###############################################################################
# Leave-one-out analysis – LS main model
# Outcome: standardized mean difference (SMD)

# Assumed objects:
# - model_LS (rma object)
# - loo_LS (leave-one-out results)
# - LS_main (data.frame)

# Leave-one-out estimates
estimativas <- loo_LS$estimate

# Number of studies
k <- length(estimativas)

# ---------------------------------------------------------------------------
# Leave-one-out plot

plot(
  1:k, estimativas,
  type = "o",               # points + connecting line
  pch = 19,
  col = "black",
  lwd = 2,
  xlab = "Study removed",
  ylab = "SMD estimate",
  ylim = range(estimativas) + c(-0.05, 0.05),
  xlim = c(0.5, k + 0.5),
  xaxt = "n"
)

# Original pooled effect
abline(
  h = coef(model_LS),
  col = "red",
  lwd = 2,
  lty = 2
)

# X-axis labels
axis(1, at = 1:k, labels = 1:k)

# ---------------------------------------------------------------------------
# Highlight extreme values

min_id <- which.min(estimativas)
max_id <- which.max(estimativas)

points(min_id, estimativas[min_id], col = "grey", pch = 19, cex = 1.5)
points(max_id, estimativas[max_id], col = "grey", pch = 19, cex = 1.5)

# ---------------------------------------------------------------------------
# Labels for extreme studies

text(
  min_id,
  estimativas[min_id],
  labels = LS_main$REF_ID[min_id],
  pos = 1,
  cex = 0.9
)

text(
  max_id,
  estimativas[max_id],
  labels = LS_main$REF_ID[max_id],
  pos = 3,
  cex = 0.9
)

# ---------------------------------------------------------------------------
# Legend

legend(
  "topright",
  legend = c("Leave-one-out estimate", "Original pooled effect", "Extreme values"),
  col = c("black", "red", "grey"),
  lty = c(1, 2, NA),
  pch = c(19, NA, 19),
  lwd = c(2, 2, NA),
  bty = "n"
)
