# ============================================================
# 07  POOLED REGIME TEST                                  (5.3)
#
# Pools both panels on common support and interacts the credit score with
# the regime. The thesis mentions this specification in 5.3 and explicitly
# does NOT use it as evidence, because it imposes a common residual scale
# across the two regimes. The separate models give the magnitudes.
#
# Only the credit score gets a regime interaction. Current LTV,
# unemployment, the origination rate and the loan-age smooth are forced to
# be common, and the crisis panel supplies about 94 per cent of the events.
#
# Everything printed also goes to  data/final_run/checks_pooled.txt
# ============================================================

source("00_setup.R")

sink(file.path(OUT_PATH, "checks_pooled.txt"), split = TRUE)
options(width = 200)

model_data_crisis <- readRDS(file.path(OUT_PATH, "model_data_crisis_final.rds"))
model_data_calm   <- readRDS(file.path(OUT_PATH, "model_data_calm_final.rds"))

model_data_crisis$regime <- "crisis"
model_data_calm$regime   <- "calm"

# no loan appears in both panels
cat("loans in both panels:",
    length(intersect(unique(model_data_calm$loan_id), unique(model_data_crisis$loan_id))), "\n")


# ---- 1. Common support (same rule as Table 4) ----
# Overlap of the 5th to 95th percentile ranges, origination-time variables only.

fico_lo <- max(as.numeric(quantile(model_data_crisis$credit_score, 0.05)),
               as.numeric(quantile(model_data_calm$credit_score,   0.05)))
fico_hi <- min(as.numeric(quantile(model_data_crisis$credit_score, 0.95)),
               as.numeric(quantile(model_data_calm$credit_score,   0.95)))
ltv_lo  <- max(as.numeric(quantile(model_data_crisis$ltv_orig, 0.05, na.rm = TRUE)),
               as.numeric(quantile(model_data_calm$ltv_orig,   0.05, na.rm = TRUE)))
ltv_hi  <- min(as.numeric(quantile(model_data_crisis$ltv_orig, 0.95, na.rm = TRUE)),
               as.numeric(quantile(model_data_calm$ltv_orig,   0.95, na.rm = TRUE)))
cat("FICO:", fico_lo, "-", fico_hi, "| LTV_orig:", ltv_lo, "-", ltv_hi, "\n")

model_data_pooled <- bind_rows(model_data_calm, model_data_crisis)
model_data_pooled$regime <- factor(model_data_pooled$regime,
                                   levels = c("calm", "crisis"))   # calm = reference
rm(model_data_calm, model_data_crisis); gc()

model_data_common <- model_data_pooled %>%
  filter(between(credit_score, fico_lo, fico_hi),
         between(ltv_orig,     ltv_lo,  ltv_hi))
rm(model_data_pooled); gc()


# ---- 2. Loan-level split ----
set.seed(123)
all_ids    <- unique(model_data_common$loan_id)
train_ids  <- sample(all_ids, size = floor(0.7 * length(all_ids)))
train_data <- model_data_common %>% filter(loan_id %in% train_ids)
rm(model_data_common); gc()

cat("training rows:", nrow(train_data), "| default rate:", round(mean(train_data$default), 5), "\n")


# ---- 3. Linear: slope difference ----
# The credit score is centred. This only changes the parameterisation: slopes
# and standard errors are the same, but the vintage dummies stay readable.
#   calm slope   = coefficient on credit_score_c
#   crisis slope = that plus the interaction term

train_data$credit_score_c <- train_data$credit_score - mean(train_data$credit_score)

lin_pooled <- bam(
  default ~ s(loan_age) + credit_score_c + credit_score_c:regime +
    ltv_current + unemployment + interest_rate_orig +
    amortization_type + vintage,
  family = binomial("cloglog"), data = train_data, discrete = TRUE
)
cat("\n\n===== LINEAR POOLED MODEL =====\n")
print(summary(lin_pooled))


# ---- 4. GAM: shape difference ----
# The ordered-factor form makes the base smooth the calm curve and the
# by-smooth the DIFFERENCE for crisis. A plain by = regime would alias
# against vintage, which is perfectly collinear with the regime.

train_data$regime_ord <- ordered(train_data$regime, levels = c("calm", "crisis"))

gam_pooled <- bam(
  default ~ vintage + s(loan_age) +
    s(credit_score) + s(credit_score, by = regime_ord) +
    s(ltv_current) + s(unemployment) + s(interest_rate_orig) +
    amortization_type,
  family = binomial("cloglog"), data = train_data, discrete = TRUE
)
cat("\n\n===== GAM POOLED MODEL =====\n")
print(summary(gam_pooled))


# ---- 5. Save ----
saveRDS(list(lin_pooled = lin_pooled, gam_pooled = gam_pooled),
        file.path(OUT_PATH, "models_pooled.rds"))

cat("\n\nALL DONE", format(Sys.time()), "\n")
sink()
