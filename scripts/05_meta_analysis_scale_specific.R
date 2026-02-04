###############################################################################
# SCALE-SPECIFIC META-ANALYSES
# Direct clinical outcomes in original units
#
# Outcomes:
# - UPDRS Part III
# - Good ON time (hours)
# - UDysRS
#
# Data types:
# - LS mean change (LS_scales)
# - Mean change (MS_scales)
#
# Software:
# R version 4.5.1
# metafor package version 4.8.0
###############################################################################

## --------------------------------------
## LOAD LIBRARY AND DATA


library(metafor)

LS_scales <- read.csv("data/LS_scales.csv")
MS_scales <- read.csv("data/MS_scales.csv")

## --------------------------------------
## FUNCTION: RANDOM-EFFECTS META-ANALYSIS FOR DIRECT SCALES
=

run_scale_meta <- function(df, mean_col, se_col, label) {

  # Calculate sampling variance from standard error
  df$vi <- df[[se_col]]^2

  # Random-effects model (REML)
  model <- rma(
    yi = df[[mean_col]],
    vi = df$vi,
    data = df,
    method = "REML"
  )

  cat("\n=================================================\n")
  cat("SCALE-SPECIFIC META-ANALYSIS\n")
  cat("Outcome:", label, "\n")
  cat("=================================================\n")
  cat("Number of studies (k):", model$k, "\n")
  cat("Pooled mean change:", round(model$beta, 3),
      "95% CI [", round(model$ci.lb, 3), ",", round(model$ci.ub, 3), "]\n")
  cat("p-value:", format.pval(model$pval, digits = 3), "\n")
  cat("I²:", round(model$I2, 1), "%\n")
  cat("tau²:", round(model$tau2, 4), "\n")

  return(model)
}

## --------------------------------------------
## LS MEAN CHANGE ANALYSES


# UPDRS Part III (LS mean change)
model_LS_UPDRSIII <- run_scale_meta(
  df       = LS_scales,
  mean_col = "UPDRS_III_mean",
  se_col   = "UPDRS_III_se",
  label    = "UPDRS Part III (LS mean change)"
)

# Good ON time (hours)
model_LS_GOODON <- run_scale_meta(
  df       = LS_scales,
  mean_col = "GOOD_ON_mean",
  se_col   = "GOOD_ON_se",
  label    = "Good ON time (hours, LS mean change)"
)

# UDysRS
model_LS_UDYSRS <- run_scale_meta(
  df       = LS_scales,
  mean_col = "UDYSRS_mean",
  se_col   = "UDYSRS_se",
  label    = "UDysRS (LS mean change)"
)

## -----------------------------------------
## MEAN CHANGE (MS) ANALYSIS


# UPDRS Part III (mean change)
model_MS_UPDRSIII <- run_scale_meta(
  df       = MS_scales,
  mean_col = "UPDRS_III_mean",
  se_col   = "UPDRS_III_se",
  label    = "UPDRS Part III (mean change)"
)
