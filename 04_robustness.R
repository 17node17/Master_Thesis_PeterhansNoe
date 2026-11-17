# ============================================================
# 04  ROBUSTNESS - model fits on the final data (one to two hours)
#
# Run after 02_main_run.R. Everything printed also goes to
#   data/final_run/checks_robustness.txt
# Common support (Table 4) and the within-month AUC (Table 5) are part of
# 02_main_run.R, because they use the models of the main split.
#
#   B1  crisis combined GAM with k = 20 for current LTV   (footnote 6, B.2)
#   B1b the same with k = 20 for loan age                  (B.2)
#   B2  lag check, K = 1 and K = 4                         (5.6, B.5)
#   B3  alternative vintage sets                           (5.6, B.5)
# ============================================================

source("00_setup.R")

sink(file.path(OUT_PATH, "checks_robustness.txt"), split = TRUE)
options(width = 200)

crisis   <- readRDS(file.path(OUT_PATH, "model_data_crisis_final.rds"))
calm     <- readRDS(file.path(OUT_PATH, "model_data_calm_final.rds"))
main_res <- readRDS(file.path(OUT_PATH, "main_split_results.rds"))

# ============================================================
# B1 - crisis combined GAM with k = 20 for current LTV (footnote 6)
# ============================================================
cat("\n\n===== B1  K = 20 FOR CURRENT LTV, CRISIS =====", format(Sys.time()), "\n")

set.seed(123)
all_ids    <- unique(crisis$loan_id)
train_ids  <- sample(all_ids, size = floor(0.7 * length(all_ids)))
train_data <- crisis %>% filter(loan_id %in% train_ids)
test_data  <- crisis %>% filter(!(loan_id %in% train_ids))

gam_k20 <- bam(default ~ s(loan_age) + s(credit_score) + s(ltv_current, k = 20) + s(unemployment) +
                 s(interest_rate_orig) + amortization_type + vintage,
               family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
p_k20 <- as.numeric(predict(gam_k20, newdata = test_data, type = "response"))

print(summary(gam_k20)$s.table)
cat("test AUC with k = 20:", round(as.numeric(auc(roc(test_data$default, p_k20, quiet = TRUE))), 4),
    "| with k = 10:", round(main_res$auc[main_res$regime == "crisis" & main_res$model == "gam_combined"], 4), "\n")
rm(gam_k20, p_k20); gc()


# ============================================================
# B1b - crisis combined GAM with k = 20 for loan age (B.2)
# Same check for the baseline smooth, which the basis dimension check can flag.
# ============================================================
cat("\n\n===== B1b  K = 20 FOR LOAN AGE, CRISIS =====", format(Sys.time()), "\n")

gam_age20 <- bam(default ~ s(loan_age, k = 20) + s(credit_score) + s(ltv_current) + s(unemployment) +
                   s(interest_rate_orig) + amortization_type + vintage,
                 family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
p_age20 <- as.numeric(predict(gam_age20, newdata = test_data, type = "response"))

print(summary(gam_age20)$s.table)
cat("test AUC with k = 20 for loan age:", round(as.numeric(auc(roc(test_data$default, p_age20, quiet = TRUE))), 4),
    "| with k = 10:", round(main_res$auc[main_res$regime == "crisis" & main_res$model == "gam_combined"], 4), "\n")
rm(gam_age20, train_data, test_data, p_age20); gc()


# ============================================================
# B2 - lag check, K = 1 and K = 4
# Sorted on reporting_period (not loan_age), as everywhere else.
# ============================================================

lagres <- data.frame()

for (K in c(1, 4)) {
  for (reg in c("crisis", "calm")) {
    
    cat("\n\n===== B2  LAG", K, reg, "=====", format(Sys.time()), "\n")
    if (reg == "crisis") d <- crisis
    if (reg == "calm")   d <- calm
    
    d <- d %>%
      arrange(loan_id, reporting_period) %>%
      group_by(loan_id) %>%
      mutate(ltv_lag   = dplyr::lag(ltv_current,  K),
             unemp_lag = dplyr::lag(unemployment, K),
             age_gap   = loan_age - dplyr::lag(loan_age, K)) %>%
      ungroup() %>%
      filter(!is.na(ltv_lag), !is.na(unemp_lag), age_gap == K) %>%
      as.data.frame()
    
    set.seed(123)
    ids   <- unique(d$loan_id)
    tr    <- sample(ids, size = floor(0.7 * length(ids)))
    train <- d %>% filter(loan_id %in% tr)
    test  <- d %>% filter(!(loan_id %in% tr))
    
    lin_dyn <- bam(default ~ s(loan_age) + ltv_lag + unemp_lag +
                     amortization_type + vintage + interest_rate_orig,
                   family = binomial("cloglog"), data = train, discrete = TRUE)
    lin_com <- bam(default ~ s(loan_age) + credit_score + ltv_lag + unemp_lag +
                     amortization_type + vintage + interest_rate_orig,
                   family = binomial("cloglog"), data = train, discrete = TRUE)
    gam_dyn <- bam(default ~ s(loan_age) + s(ltv_lag) + s(unemp_lag) +
                     s(interest_rate_orig) + amortization_type + vintage,
                   family = binomial("cloglog"), data = train, discrete = TRUE)
    gam_com <- bam(default ~ s(loan_age) + s(credit_score) + s(ltv_lag) +
                     s(unemp_lag) + s(interest_rate_orig) +
                     amortization_type + vintage,
                   family = binomial("cloglog"), data = train, discrete = TRUE)
    
    a_lin_dyn <- as.numeric(auc(roc(test$default, as.numeric(predict(lin_dyn, newdata = test, type = "response")), quiet = TRUE)))
    a_lin_com <- as.numeric(auc(roc(test$default, as.numeric(predict(lin_com, newdata = test, type = "response")), quiet = TRUE)))
    a_gam_dyn <- as.numeric(auc(roc(test$default, as.numeric(predict(gam_dyn, newdata = test, type = "response")), quiet = TRUE)))
    a_gam_com <- as.numeric(auc(roc(test$default, as.numeric(predict(gam_com, newdata = test, type = "response")), quiet = TRUE)))
    
    lagres <- rbind(lagres, data.frame(
      regime = reg, K = K, rows = nrow(d), events = sum(d$default),
      lin_dyn = a_lin_dyn, lin_com = a_lin_com, gam_dyn = a_gam_dyn, gam_com = a_gam_com,
      incr_lin = a_lin_com - a_lin_dyn, incr_gam = a_gam_com - a_gam_dyn))
    saveRDS(lagres, file.path(OUT_PATH, "robust_lag_final.rds"))
    
    rm(d, train, test, lin_dyn, lin_com, gam_dyn, gam_com); gc()
  }
}

cat("\n\n===== B2  LAG RESULTS =====\n")
print(lagres, row.names = FALSE, digits = 4)
for (K in c(1, 4)) {
  cat("K =", K, "| ratio linear:",
      round(lagres$incr_lin[lagres$K == K & lagres$regime == "calm"] /
              lagres$incr_lin[lagres$K == K & lagres$regime == "crisis"], 2),
      "| ratio GAM:",
      round(lagres$incr_gam[lagres$K == K & lagres$regime == "calm"] /
              lagres$incr_gam[lagres$K == K & lagres$regime == "crisis"], 2), "\n")
}

rm(crisis, calm); gc()


# ============================================================
# B3 - alternative vintage sets
# The six models on the main split, on the panels of the two alternative
# specifications (built by 01_build_panels.R). A panel that is not on disk
# is skipped.
# ============================================================

alt_res <- data.frame()

for (i in 1:length(alt_panel_files)) {

  f <- file.path(BASE_PATH, alt_panel_files[i])
  cat("\n\n===== B3 ", names(alt_panel_files)[i], "=====", format(Sys.time()), "\n")
  if (!file.exists(f)) { cat("panel not found, skipped:", f, "\n"); next }

  panel <- readRDS(f)

  d <- panel %>%
    arrange(loan_id, reporting_period) %>%
    group_by(loan_id) %>%
    mutate(cum_default = cumsum(default)) %>%
    filter(cum_default == 0 | (cum_default == 1 & default == 1)) %>%
    ungroup() %>%
    mutate(amortization_type = ifelse(amortization_type == "ARM", 1, 0)) %>%
    select(loan_id, default, vintage, loan_age, credit_score, ltv_current,
           unemployment, interest_rate_orig, amortization_type) %>%
    filter(!is.na(credit_score), !is.na(ltv_current), !is.na(interest_rate_orig),
           !is.na(loan_age), !is.na(unemployment)) %>%
    mutate(vintage = factor(vintage), amortization_type = factor(amortization_type)) %>%
    as.data.frame()
  rm(panel); gc()

  cat("rows", nrow(d), "| loans", length(unique(d$loan_id)), "| defaults", sum(d$default),
      "| zero LTV", sum(d$ltv_current == 0), "\n")

  set.seed(123)
  all_ids    <- unique(d$loan_id)
  train_ids  <- sample(all_ids, size = floor(0.7 * length(all_ids)))
  train_data <- d %>% filter(loan_id %in% train_ids)
  test_data  <- d %>% filter(!(loan_id %in% train_ids))
  rm(d); gc()
  
  m <- list()
  m$lin_static <- bam(default ~ s(loan_age) + credit_score +
                        amortization_type + vintage + interest_rate_orig,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  m$lin_dynamic <- bam(default ~ s(loan_age) + ltv_current + unemployment +
                         amortization_type + vintage + interest_rate_orig,
                       family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  m$lin_combined <- bam(default ~ s(loan_age) + credit_score + ltv_current + unemployment +
                          amortization_type + vintage + interest_rate_orig,
                        family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  m$gam_static <- bam(default ~ s(loan_age) + s(credit_score) +
                        s(interest_rate_orig) + amortization_type + vintage,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  m$gam_dynamic <- bam(default ~ s(loan_age) + s(ltv_current) + s(unemployment) +
                         s(interest_rate_orig) + amortization_type + vintage,
                       family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  m$gam_combined <- bam(default ~ s(loan_age) + s(credit_score) + s(ltv_current) + s(unemployment) +
                          s(interest_rate_orig) + amortization_type + vintage,
                        family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  
  aucs <- c()
  for (nm in names(m)) {
    p <- as.numeric(predict(m[[nm]], newdata = test_data, type = "response"))
    aucs[nm] <- as.numeric(auc(roc(test_data$default, p, quiet = TRUE)))
  }
  
  alt_res <- rbind(alt_res, data.frame(
    spec = names(alt_panel_files)[i],
    lin_static = aucs["lin_static"], lin_dynamic = aucs["lin_dynamic"], lin_combined = aucs["lin_combined"],
    gam_static = aucs["gam_static"], gam_dynamic = aucs["gam_dynamic"], gam_combined = aucs["gam_combined"],
    incr_lin = aucs["lin_combined"] - aucs["lin_dynamic"],
    incr_gam = aucs["gam_combined"] - aucs["gam_dynamic"], row.names = NULL))
  saveRDS(alt_res, file.path(OUT_PATH, "alt_vintage_results.rds"))
  
  rm(m, train_data, test_data); gc()
}

cat("\n\n===== B3  ALTERNATIVE VINTAGE RESULTS =====\n")
print(alt_res, row.names = FALSE, digits = 4)

cat("\nmain specification, main split, increment linear / GAM:\n")
for (reg in c("crisis", "calm")) {
  cat(reg,
      round(main_res$auc[main_res$regime == reg & main_res$model == "lin_combined"] -
              main_res$auc[main_res$regime == reg & main_res$model == "lin_dynamic"], 4), "/",
      round(main_res$auc[main_res$regime == reg & main_res$model == "gam_combined"] -
              main_res$auc[main_res$regime == reg & main_res$model == "gam_dynamic"], 4), "\n")
}

cat("\n\nALL DONE", format(Sys.time()), "\n")
sink()
