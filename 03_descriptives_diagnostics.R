# ============================================================
# 03  DESCRIPTIVES AND DIAGNOSTICS on the final data (minutes)
#
# Run after 02_main_run.R. Everything printed also goes to
#   data/final_run/checks_descriptives.txt
#
#   A1   Table 1: state of the drivers, unemployment decomposition
#   A2   ARM share, rate direction, fixed-rate loans with a rate change  (3.4, A.2)
#   A3   constructed LTV against the Freddie Mac ELTV                     (3.4)
#   A4   DTI censoring: default rate with and without DTI                 (3.4)
#   A5   loan-age resets                                                  (3.2)
#   A6   loan age range, first defaults per year
#   A7   modification sequence of fixed-rate loans                        (3.4, A.2)
#   A8   paired fold comparisons                                          (B.1)
#   A9   basis dimension check on the combined GAM                        (B.2)
#   A10  common-support bounds and sample sizes                           (B.5)
# ============================================================

source("00_setup.R")

sink(file.path(OUT_PATH, "checks_descriptives.txt"), split = TRUE)
options(width = 200)

crisis       <- readRDS(file.path(OUT_PATH, "model_data_crisis_final.rds"))
calm         <- readRDS(file.path(OUT_PATH, "model_data_calm_final.rds"))
panel_crisis <- readRDS(file.path(BASE_PATH, panel_files["crisis"]))
panel_calm   <- readRDS(file.path(BASE_PATH, panel_files["calm"]))

# ============================================================
# A1 - Table 1
# ============================================================
cat("\n\n===== A1  TABLE 1 =====\n")

driver_state <- function(df, label) {
  data.frame(
    window         = label,
    n_loanmonths   = nrow(df),
    n_loans        = length(unique(df$loan_id)),
    n_events       = sum(df$default),
    pct_underwater = mean(df$ltv_current > 100) * 100,
    ltv_p10        = unname(quantile(df$ltv_current, 0.10)),
    ltv_median     = median(df$ltv_current),
    ltv_p90        = unname(quantile(df$ltv_current, 0.90)),
    ltv_sd         = sd(df$ltv_current),
    unemp_min      = min(df$unemployment),
    unemp_median   = median(df$unemployment),
    unemp_max      = max(df$unemployment),
    unemp_sd       = sd(df$unemployment),
    fico_median    = median(df$credit_score),
    fico_sd        = sd(df$credit_score),
    rate_sd        = sd(df$interest_rate_orig),
    row.names = NULL
  )
}

driver_table <- rbind(
  driver_state(crisis, "crisis 2008-2011 | v2005-2007"),
  driver_state(calm,   "calm 2016-2019 | v2013-2015")
)
print(t(round(driver_table[, -1], 2)))

cat("\nloan-level default rate, %: crisis",
    round(sum(crisis$default) / length(unique(crisis$loan_id)) * 100, 2),
    "| calm", round(sum(calm$default) / length(unique(calm$loan_id)) * 100, 2), "\n")
cat("average monthly hazard, %: crisis", round(mean(crisis$default) * 100, 4),
    "| calm", round(mean(calm$default) * 100, 4), "\n")

unemp_time <- function(panel_df, label) {
  panel_df %>%
    group_by(reporting_period) %>%
    summarise(u = mean(unemployment), .groups = "drop") %>%
    summarise(window = label, time_sd = sd(u), time_range = max(u) - min(u))
}
unemp_cross <- function(panel_df, label) {
  panel_df %>%
    group_by(reporting_period) %>%
    summarise(s = sd(unemployment), .groups = "drop") %>%
    summarise(window = label, cross_state_sd = mean(s))
}

cat("\nunemployment decomposition, estimation frames:\n")
print(as.data.frame(bind_rows(
  left_join(unemp_time(crisis, "crisis"), unemp_cross(crisis, "crisis"), by = "window"),
  left_join(unemp_time(calm,   "calm"),   unemp_cross(calm,   "calm"),   by = "window"))), digits = 3)

cat("\nunemployment decomposition, full panels:\n")
print(as.data.frame(bind_rows(
  left_join(unemp_time(panel_crisis, "crisis"), unemp_cross(panel_crisis, "crisis"), by = "window"),
  left_join(unemp_time(panel_calm,   "calm"),   unemp_cross(panel_calm,   "calm"),   by = "window"))), digits = 3)

cat("\ncorrelations between the covariates:\n")
print(round(cor(crisis[, c("credit_score", "ltv_current", "unemployment", "interest_rate_orig")]), 3))
print(round(cor(calm[,   c("credit_score", "ltv_current", "unemployment", "interest_rate_orig")]), 3))


# ============================================================
# A2 - ARM share, rate direction, fixed-rate loans with a rate change
# (Table 1 ARM row; share of fixed-rate loans with a rate change in 3.4)
# ============================================================
cat("\n\n===== A2  RATE CHANGES BY PRODUCT =====\n")

rate_diag <- function(panel_df, label) {
  panel_df %>%
    filter(!is.na(interest_rate_current)) %>%
    arrange(loan_id, reporting_period) %>%
    group_by(loan_id, amortization_type) %>%
    summarise(first_rate   = first(interest_rate_current),
              last_rate    = last(interest_rate_current),
              rate_change  = last_rate - first_rate,
              changed      = n_distinct(interest_rate_current) > 1,
              ever_default = max(default),
              .groups = "drop") %>%
    group_by(amortization_type) %>%
    summarise(n_loans        = n(),
              pct_changed    = mean(changed) * 100,
              pct_down       = mean(rate_change[changed] < 0) * 100,
              median_change  = median(rate_change[changed]),
              def_if_changed = mean(ever_default[changed]) * 100,
              def_if_stable  = mean(ever_default[!changed]) * 100,
              .groups = "drop") %>%
    mutate(panel = label, .before = 1)
}

rate_summary <- bind_rows(rate_diag(panel_crisis, "crisis"),
                          rate_diag(panel_calm,   "calm"))
rate_summary <- rate_summary %>%
  group_by(panel) %>%
  mutate(share_of_loans = n_loans / sum(n_loans) * 100) %>%
  ungroup()
print(as.data.frame(rate_summary), digits = 3)

cat("\nARM share in the estimation frames, % of loans and % of loan-months:\n")
arm_crisis <- crisis %>% group_by(loan_id) %>% summarise(arm = first(amortization_type) == "1")
arm_calm   <- calm   %>% group_by(loan_id) %>% summarise(arm = first(amortization_type) == "1")
cat("crisis", round(mean(arm_crisis$arm) * 100, 2), "/", round(mean(crisis$amortization_type == "1") * 100, 2),
    "| calm", round(mean(arm_calm$arm) * 100, 2),   "/", round(mean(calm$amortization_type == "1") * 100, 2), "\n")


# ============================================================
# A3 - constructed LTV against Freddie Mac ELTV (correlation in 3.4; ELTV exists from 2017)
# ============================================================
cat("\n\n===== A3  LTV vs ELTV =====\n")
print(panel_calm %>%
        filter(!is.na(eltv)) %>%
        summarise(n = n(), cor_ltv_eltv = cor(eltv, ltv_current, use = "complete.obs")))


# ============================================================
# A4 - DTI censoring (default rate without and with a reported DTI, 3.4)
# ============================================================
cat("\n\n===== A4  DTI =====\n")

dti_diag <- function(panel_df, label) {
  panel_df %>%
    group_by(loan_id) %>%
    summarise(no_dti   = all(is.na(dti)),
              ever_def = max(default),
              ltv_o    = first(ltv_orig),
              fico     = first(credit_score), .groups = "drop") %>%
    group_by(no_dti) %>%
    summarise(n               = n(),
              pct_default     = mean(ever_def) * 100,
              median_ltv_orig = median(ltv_o, na.rm = TRUE),
              pct_ltv_over_80 = mean(ltv_o > 80, na.rm = TRUE) * 100,
              median_fico     = median(fico, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(panel = label, .before = 1)
}
print(as.data.frame(bind_rows(dti_diag(panel_crisis, "crisis"),
                              dti_diag(panel_calm,   "calm"))), digits = 4)


# ============================================================
# A5 - loan-age resets (share of loans, panel and estimation frame, 3.2)
# ============================================================
cat("\n\n===== A5  LOAN-AGE RESETS =====\n")

reset_rate <- function(df, label) {
  df %>%
    arrange(loan_id, reporting_period) %>%
    group_by(loan_id) %>%
    summarise(resets = any(diff(loan_age) < 0), .groups = "drop") %>%
    summarise(frame = label, n_loans = n(), pct_reset = mean(resets) * 100)
}
print(as.data.frame(bind_rows(
  reset_rate(panel_crisis, "panel crisis"),
  reset_rate(panel_calm,   "panel calm"),
  reset_rate(crisis,       "model crisis"),
  reset_rate(calm,         "model calm"))), row.names = FALSE)


# ============================================================
# A6 - loan age range and first defaults per year
# ============================================================
cat("\n\n===== A6  LOAN AGE AND DEFAULTS PER YEAR =====\n")
print(summary(crisis$loan_age)); print(summary(calm$loan_age))
print(crisis %>% filter(default == 1) %>% count(year_current, name = "first_defaults"))
print(calm   %>% filter(default == 1) %>% count(year_current, name = "first_defaults"))

# ============================================================
# A7 - modification sequence of fixed-rate loans, crisis panel
# The modified terms are reported one period before the rate changes, so the
# balance and the delinquency status move first. The sequence needs T-2.
# ============================================================
cat("\n\n===== A7  MODIFICATION SEQUENCE =====\n")

mod_sequence <- panel_crisis %>%
  filter(amortization_type == "FRM") %>%
  arrange(loan_id, reporting_period) %>%
  group_by(loan_id) %>%
  mutate(prev_rate      = lag(interest_rate_current),
         rate_cut       = !is.na(prev_rate) & interest_rate_current < prev_rate,
         rate_drop      = interest_rate_current - prev_rate,
         term_change    = months_to_maturity - lag(months_to_maturity),
         upb_jump_prior = (lag(upb_current) - lag(upb_current, 2)) /
                           lag(upb_current, 2) * 100,
         dq_two_before  = lag(suppressWarnings(as.numeric(delinquency_status)), 2)) %>%
  ungroup() %>%
  filter(rate_cut)

cat("loan-months with a rate cut:", nrow(mod_sequence), "\n")
cat("rate change, percentage points:\n");              print(summary(mod_sequence$rate_drop))
cat("change in remaining term, months:\n");            print(summary(mod_sequence$term_change))
cat("balance change one period before, per cent:\n");  print(summary(mod_sequence$upb_jump_prior))
cat("delinquency status two periods before:\n");       print(summary(mod_sequence$dq_two_before))
cat("share at ninety days or more two periods before, %:",
    round(mean(mod_sequence$dq_two_before >= 3, na.rm = TRUE) * 100, 1), "\n")
rm(mod_sequence)


rm(panel_crisis, panel_calm, arm_crisis, arm_calm); gc()


# ============================================================
# A8 - paired fold comparisons (Appendix)
# ============================================================
cat("\n\n===== A8  PAIRED FOLDS =====\n")

for (reg in c("crisis", "calm")) {
  cv <- readRDS(file.path(OUT_PATH, paste0("cv_results_", reg, "_final.rds")))
  bo <- readRDS(file.path(OUT_PATH, paste0("boost_cv_", reg, "_final.rds")))
  
  paired <- data.frame(
    fold               = cv$fold,
    gam_minus_lin_stat = cv$gam_static   - cv$lin_static,
    gam_minus_lin_dyn  = cv$gam_dynamic  - cv$lin_dynamic,
    gam_minus_lin_comb = cv$gam_combined - cv$lin_combined,
    boost_minus_gam_stat = bo$auc_static   - cv$gam_static,
    boost_minus_gam_dyn  = bo$auc_dynamic  - cv$gam_dynamic,
    boost_minus_gam_comb = bo$auc_combined - cv$gam_combined,
    incr_lin   = cv$fico_incr_lin,
    incr_gam   = cv$fico_incr_gam,
    incr_boost = bo$increment
  )
  cat("\n", reg, "\n")
  print(round(paired, 4), row.names = FALSE)
  print(round(data.frame(mean = colMeans(paired[, -1]), sd = apply(paired[, -1], 2, sd)), 5))
  saveRDS(paired, file.path(OUT_PATH, paste0("paired_folds_", reg, ".rds")))
}


# ============================================================
# A9 - k.check on the combined GAM
# ============================================================
cat("\n\n===== A9  K.CHECK =====\n")
main_res <- readRDS(file.path(OUT_PATH, "main_split_results.rds"))

models <- readRDS(file.path(OUT_PATH, "models_crisis_final.rds"))
set.seed(123)   # k.check draws a random subsample, the seed makes the table reproducible
cat("crisis\n"); print(k.check(models$gam_combined))
rm(models); gc()
models <- readRDS(file.path(OUT_PATH, "models_calm_final.rds"))
set.seed(123)
cat("calm\n");   print(k.check(models$gam_combined))
rm(models); gc()

# ============================================================
# A10 - common-support bounds and sample sizes (Table 4)
# Overlap of the 5th to 95th percentile ranges of the two panels, on the
# origination-time variables only.
# ============================================================
cat("\n\n===== A10  COMMON SUPPORT =====\n")

fico_lo <- max(as.numeric(quantile(crisis$credit_score, 0.05)), as.numeric(quantile(calm$credit_score, 0.05)))
fico_hi <- min(as.numeric(quantile(crisis$credit_score, 0.95)), as.numeric(quantile(calm$credit_score, 0.95)))
ltv_lo  <- max(as.numeric(quantile(crisis$ltv_orig, 0.05, na.rm = TRUE)), as.numeric(quantile(calm$ltv_orig, 0.05, na.rm = TRUE)))
ltv_hi  <- min(as.numeric(quantile(crisis$ltv_orig, 0.95, na.rm = TRUE)), as.numeric(quantile(calm$ltv_orig, 0.95, na.rm = TRUE)))
cat("FICO:", fico_lo, "-", fico_hi, "| LTV_orig:", ltv_lo, "-", ltv_hi, "\n")

crisis_cs <- crisis %>% filter(between(credit_score, fico_lo, fico_hi), between(ltv_orig, ltv_lo, ltv_hi))
calm_cs   <- calm   %>% filter(between(credit_score, fico_lo, fico_hi), between(ltv_orig, ltv_lo, ltv_hi))
cat("crisis | rows", nrow(crisis_cs), "| events", sum(crisis_cs$default), "\n")
cat("calm   | rows", nrow(calm_cs),   "| events", sum(calm_cs$default), "\n")

cat("\n\nALL DONE", format(Sys.time()), "\n")
sink()
