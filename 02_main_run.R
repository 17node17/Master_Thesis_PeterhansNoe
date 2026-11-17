# ============================================================
# 02  MAIN RUN - every model of the thesis on the final panels
#
# Needs the two panels written by 01_build_panels.R.
# Everything is written to  data/final_run/  and saved after each step
# (cross-validation after each fold), so a crash does not lose what is done.
# Runs for several hours.
#
#   STEP 0  estimation frames (absorbing-event truncation)      (3.2)
#   STEP 1  main 70/30 split: six models, AUC, within-month AUC,
#           Brier score, calibration deciles, model summaries   (Tables 3, 5, B.4)
#   STEP 2  five-fold CV, linear and GAM                        (Table 2)
#   STEP 3  common support                                      (Table 4)
#   STEP 4  five-fold CV, boosting, three information sets      (Table 2)
#   STEP 5  depth ladder, SHAP pairs, GAM with interaction      (5.5)
# ============================================================

source("00_setup.R")


# ============================================================
# STEP 0 - modelling frames
# ============================================================

frames <- list()

for (reg in c("crisis", "calm")) {
  
  panel <- readRDS(file.path(BASE_PATH, panel_files[reg]))
  
  md <- panel %>%
    arrange(loan_id, reporting_period) %>%
    group_by(loan_id) %>%
    mutate(cum_default = cumsum(default)) %>%
    filter(cum_default == 0 | (cum_default == 1 & default == 1)) %>%
    ungroup() %>%
    mutate(amortization_type = ifelse(amortization_type == "ARM", 1, 0)) %>%
    select(loan_id, default, vintage, dataset, year_current, reporting_period,
           loan_age, credit_score, ltv_current,
           unemployment, interest_rate_orig,
           amortization_type, ltv_orig)
  
  md <- md %>%
    filter(!is.na(credit_score), !is.na(ltv_current),
           !is.na(interest_rate_orig),
           !is.na(loan_age), !is.na(unemployment))
  
  md <- md %>%
    mutate(vintage           = factor(vintage),
           amortization_type = factor(amortization_type)) %>%
    as.data.frame()
  
  # expected: crisis 6,690,621 rows / 205,052 loans / 33,252 defaults
  #           calm   about 1,952 defaults
  cat(reg, "| rows", nrow(md), "| loans", length(unique(md$loan_id)),
      "| defaults", sum(md$default), "| zero LTV", sum(md$ltv_current == 0), "\n")
  if (sum(md$ltv_current == 0) > 0) stop("zero LTV rows found - wrong panel file")
  
  saveRDS(md, file.path(OUT_PATH, paste0("model_data_", reg, "_final.rds")))
  frames[[reg]] <- md
  rm(panel, md); gc()
}


# ============================================================
# STEP 1 - main 70/30 split
# ============================================================

main_res <- data.frame()
cal_all  <- data.frame()

for (reg in c("crisis", "calm")) {
  
  cat("\nSTEP 1", reg, format(Sys.time()), "\n")
  d <- frames[[reg]]
  
  set.seed(123)
  all_ids    <- unique(d$loan_id)
  train_ids  <- sample(all_ids, size = floor(0.7 * length(all_ids)))
  train_data <- d %>% filter(loan_id %in% train_ids)
  test_data  <- d %>% filter(!(loan_id %in% train_ids))
  
  models <- list()
  models$lin_static <- bam(default ~ s(loan_age) + credit_score +
                             amortization_type + vintage + interest_rate_orig,
                           family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  models$lin_dynamic <- bam(default ~ s(loan_age) + ltv_current + unemployment +
                              amortization_type + vintage + interest_rate_orig,
                            family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  models$lin_combined <- bam(default ~ s(loan_age) + credit_score + ltv_current + unemployment +
                               amortization_type + vintage + interest_rate_orig,
                             family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  models$gam_static <- bam(default ~ s(loan_age) + s(credit_score) +
                             s(interest_rate_orig) + amortization_type + vintage,
                           family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  models$gam_dynamic <- bam(default ~ s(loan_age) + s(ltv_current) + s(unemployment) +
                              s(interest_rate_orig) + amortization_type + vintage,
                            family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  models$gam_combined <- bam(default ~ s(loan_age) + s(credit_score) + s(ltv_current) + s(unemployment) +
                               s(interest_rate_orig) + amortization_type + vintage,
                             family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  
  saveRDS(models, file.path(OUT_PATH, paste0("models_", reg, "_final.rds")))
  
  capture.output(
    summary(models$lin_static),  summary(models$lin_dynamic),  summary(models$lin_combined),
    summary(models$gam_static),  summary(models$gam_dynamic),  summary(models$gam_combined),
    file = file.path(OUT_PATH, paste0("summaries_", reg, ".txt")))
  
  # across-window AUC, within-month AUC, Brier for each of the six models
  for (nm in names(models)) {
    test_data$p <- as.numeric(predict(models[[nm]], newdata = test_data, type = "response"))
    
    monthly <- test_data %>%
      group_by(reporting_period) %>%
      summarise(n_def = sum(default == 1),
                n_sur = sum(default == 0),
                auc_m = (sum(rank(p)[default == 1]) - n_def * (n_def + 1) / 2) / (n_def * n_sur),
                .groups = "drop") %>%
      filter(n_def > 0 & n_sur > 0)
    
    main_res <- rbind(main_res, data.frame(
      regime           = reg,
      model            = nm,
      auc              = as.numeric(auc(roc(test_data$default, test_data$p, quiet = TRUE))),
      auc_within_month = mean(monthly$auc_m),
      brier            = mean((test_data$p - test_data$default)^2),
      brier_null       = mean(test_data$default) * (1 - mean(test_data$default)),
      months           = nrow(monthly),
      events_per_month = mean(monthly$n_def)))
  }
  
  # calibration deciles, combined GAM
  test_data$p <- as.numeric(predict(models$gam_combined, newdata = test_data, type = "response"))
  cal <- test_data %>%
    mutate(decile = ntile(p, 10)) %>%
    group_by(decile) %>%
    summarise(n = n(), pred_rate = mean(p), actual_rate = mean(default), .groups = "drop") %>%
    mutate(regime = reg)
  cal_all <- rbind(cal_all, as.data.frame(cal))
  
  saveRDS(main_res, file.path(OUT_PATH, "main_split_results.rds"))
  saveRDS(cal_all,  file.path(OUT_PATH, "calibration_deciles.rds"))
  print(main_res, row.names = FALSE, digits = 4)
  
  rm(models, train_data, test_data, d); gc()
}


# ============================================================
# STEP 2 - five-fold CV, linear and GAM
# ============================================================

fit_one_fold <- function(k) {
  test_ids   <- id_fold$loan_id[id_fold$fold == k]
  train_data <- model_data_clean %>% filter(!(loan_id %in% test_ids))
  test_data  <- model_data_clean %>% filter(  loan_id %in% test_ids)
  
  lin_static   <- bam(default ~ s(loan_age) + credit_score +
                        amortization_type + vintage + interest_rate_orig,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  lin_dynamic  <- bam(default ~ s(loan_age) + ltv_current + unemployment +
                        amortization_type + vintage + interest_rate_orig,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  lin_combined <- bam(default ~ s(loan_age) + credit_score + ltv_current + unemployment +
                        amortization_type + vintage + interest_rate_orig,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  gam_static   <- bam(default ~ s(loan_age) + s(credit_score) +
                        s(interest_rate_orig) + amortization_type + vintage,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  gam_dynamic  <- bam(default ~ s(loan_age) + s(ltv_current) + s(unemployment) +
                        s(interest_rate_orig) + amortization_type + vintage,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  gam_combined <- bam(default ~ s(loan_age) + s(credit_score) + s(ltv_current) + s(unemployment) +
                        s(interest_rate_orig) + amortization_type + vintage,
                      family = binomial(link = "cloglog"), data = train_data, discrete = TRUE)
  
  auc_of <- function(model) {
    p <- predict(model, newdata = test_data, type = "response")
    as.numeric(auc(roc(test_data$default, p, quiet = TRUE)))
  }
  a_lin_stat <- auc_of(lin_static);   a_lin_dyn <- auc_of(lin_dynamic);  a_lin_comb <- auc_of(lin_combined)
  a_gam_stat <- auc_of(gam_static);   a_gam_dyn <- auc_of(gam_dynamic);  a_gam_comb <- auc_of(gam_combined)
  
  data.frame(
    fold          = k,
    lin_static    = a_lin_stat,  lin_dynamic = a_lin_dyn,  lin_combined = a_lin_comb,
    gam_static    = a_gam_stat,  gam_dynamic = a_gam_dyn,  gam_combined = a_gam_comb,
    fico_incr_lin = a_lin_comb - a_lin_dyn,
    fico_incr_gam = a_gam_comb - a_gam_dyn
  )
}

for (reg in c("crisis", "calm")) {
  
  model_data_clean <- frames[[reg]]
  
  set.seed(123)
  all_ids <- unique(model_data_clean$loan_id)
  folds   <- sample(rep(1:5, length.out = length(all_ids)))
  id_fold <- data.frame(loan_id = all_ids, fold = folds)
  
  cv <- data.frame()
  for (k in 1:5) {
    cat("\nSTEP 2", reg, "fold", k, format(Sys.time()), "\n")
    cv <- bind_rows(cv, fit_one_fold(k))
    saveRDS(cv, file.path(OUT_PATH, paste0("cv_results_", reg, "_final.rds")))
  }
  cv$gap_diff <- cv$fico_incr_lin - cv$fico_incr_gam
  saveRDS(cv, file.path(OUT_PATH, paste0("cv_results_", reg, "_final.rds")))
  
  print(cv, row.names = FALSE, digits = 4)
  print(round(data.frame(mean = colMeans(cv[, -1]), sd = apply(cv[, -1], 2, sd)), 4))
}
rm(model_data_clean); gc()


# ============================================================
# STEP 3 - common support (main split on the restricted frames)
# ============================================================

cat("\nSTEP 3", format(Sys.time()), "\n")

qfun <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))

fico_lo <- max(qfun(frames$crisis$credit_score, 0.05), qfun(frames$calm$credit_score, 0.05))
fico_hi <- min(qfun(frames$crisis$credit_score, 0.95), qfun(frames$calm$credit_score, 0.95))
ltv_lo  <- max(qfun(frames$crisis$ltv_orig, 0.05),     qfun(frames$calm$ltv_orig, 0.05))
ltv_hi  <- min(qfun(frames$crisis$ltv_orig, 0.95),     qfun(frames$calm$ltv_orig, 0.95))
cat("FICO:", fico_lo, "-", fico_hi, "| LTV_orig:", ltv_lo, "-", ltv_hi, "\n")

cs_results <- data.frame()

for (reg in c("crisis", "calm")) {
  
  dat <- frames[[reg]] %>% filter(between(credit_score, fico_lo, fico_hi),
                                  between(ltv_orig,     ltv_lo,  ltv_hi))
  cat(reg, "common support | rows", nrow(dat), "| events", sum(dat$default), "\n")
  
  set.seed(123)
  all_ids   <- unique(dat$loan_id)
  train_ids <- sample(all_ids, size = floor(0.7 * length(all_ids)))
  tr <- dat %>% filter(  loan_id %in% train_ids)
  te <- dat %>% filter(!(loan_id %in% train_ids))
  
  m <- list()
  m$lin_static <- bam(default ~ s(loan_age) + credit_score +
                        amortization_type + vintage + interest_rate_orig,
                      family = binomial("cloglog"), data = tr, discrete = TRUE)
  m$lin_dynamic <- bam(default ~ s(loan_age) + ltv_current + unemployment +
                         amortization_type + vintage + interest_rate_orig,
                       family = binomial("cloglog"), data = tr, discrete = TRUE)
  m$lin_combined <- bam(default ~ s(loan_age) + credit_score + ltv_current +
                          unemployment + amortization_type + vintage + interest_rate_orig,
                        family = binomial("cloglog"), data = tr, discrete = TRUE)
  m$gam_static <- bam(default ~ s(loan_age) + s(credit_score) +
                        s(interest_rate_orig) + amortization_type + vintage,
                      family = binomial("cloglog"), data = tr, discrete = TRUE)
  m$gam_dynamic <- bam(default ~ s(loan_age) + s(ltv_current) + s(unemployment) +
                         s(interest_rate_orig) + amortization_type + vintage,
                       family = binomial("cloglog"), data = tr, discrete = TRUE)
  m$gam_combined <- bam(default ~ s(loan_age) + s(credit_score) + s(ltv_current) +
                          s(unemployment) + s(interest_rate_orig) +
                          amortization_type + vintage,
                        family = binomial("cloglog"), data = tr, discrete = TRUE)
  
  for (nm in names(m)) {
    p <- as.numeric(predict(m[[nm]], newdata = te, type = "response"))
    cs_results <- rbind(cs_results, data.frame(
      regime = reg, model = nm,
      auc    = as.numeric(auc(roc(te$default, p, quiet = TRUE))),
      rows   = nrow(dat), events = sum(dat$default)))
  }
  saveRDS(cs_results, file.path(OUT_PATH, "common_support_results.rds"))
  rm(m, tr, te, dat); gc()
}
print(cs_results, row.names = FALSE, digits = 4)


# ============================================================
# STEP 4 - five-fold CV, boosting, three information sets
# ============================================================

feats_combined <- c("credit_score", "ltv_current", "unemployment",
                    "interest_rate_orig", "loan_age",
                    "amortization_type", "vintage")
feats_dynamic  <- c("ltv_current", "unemployment",
                    "interest_rate_orig", "loan_age",
                    "amortization_type", "vintage")
feats_fico     <- c("credit_score",
                    "interest_rate_orig", "loan_age",
                    "amortization_type", "vintage")

XGB_PARAMS <- list(objective        = "binary:logistic",
                   eval_metric      = "auc",
                   eta              = 0.05,
                   subsample        = 0.8,
                   colsample_bytree = 0.8,
                   min_child_weight = 50)

fit_xgb <- function(train_df, test_df, depth, features, seed = 456) {
  set.seed(seed)
  tr_ids  <- unique(train_df$loan_id)
  val_ids <- sample(tr_ids, size = floor(0.2 * length(tr_ids)))
  
  sub_train <- train_df %>% filter(!(loan_id %in% val_ids))
  sub_val   <- train_df %>% filter(  loan_id %in% val_ids)
  
  X_sub  <- model.matrix(~ . - 1, data = sub_train[, features])
  X_val  <- model.matrix(~ . - 1, data = sub_val[,   features])
  X_test <- model.matrix(~ . - 1, data = test_df[,   features])
  
  m <- xgb.train(
    params  = c(XGB_PARAMS, list(max_depth = depth)),
    data    = xgb.DMatrix(X_sub, label = sub_train$default),
    nrounds = 2000,
    evals   = list(val = xgb.DMatrix(X_val, label = sub_val$default)),
    early_stopping_rounds = 30, verbose = 0
  )
  
  p <- predict(m, xgb.DMatrix(X_test))
  
  list(model     = m,
       depth     = depth,
       features  = features,
       best_iter = xgb.attr(m, "best_iteration"),
       auc       = as.numeric(auc(roc(test_df$default, p, quiet = TRUE))),
       brier     = mean((p - test_df$default)^2))
}

for (reg in c("crisis", "calm")) {
  
  model_data_clean <- frames[[reg]]
  
  set.seed(123)
  all_ids <- unique(model_data_clean$loan_id)
  folds   <- sample(rep(1:5, length.out = length(all_ids)))
  id_fold <- data.frame(loan_id = all_ids, fold = folds)
  
  boost_cv <- data.frame()
  
  for (k in 1:5) {
    cat("\nSTEP 4", reg, "fold", k, format(Sys.time()), "\n")
    test_ids <- id_fold$loan_id[id_fold$fold == k]
    tr_k <- model_data_clean %>% filter(!(loan_id %in% test_ids))
    te_k <- model_data_clean %>% filter(  loan_id %in% test_ids)
    
    r_comb <- fit_xgb(tr_k, te_k, 5, feats_combined, seed = 1000 + k)
    r_dyn  <- fit_xgb(tr_k, te_k, 5, feats_dynamic,  seed = 1000 + k)
    r_stat <- fit_xgb(tr_k, te_k, 5, feats_fico,     seed = 1000 + k)
    
    boost_cv <- rbind(boost_cv, data.frame(
      fold          = k,
      auc_static    = r_stat$auc,
      auc_dynamic   = r_dyn$auc,
      auc_combined  = r_comb$auc,
      increment     = r_comb$auc - r_dyn$auc,
      iter_static   = r_stat$best_iter,
      iter_dynamic  = r_dyn$best_iter,
      iter_combined = r_comb$best_iter))
    
    saveRDS(boost_cv, file.path(OUT_PATH, paste0("boost_cv_", reg, "_final.rds")))
    rm(r_comb, r_dyn, r_stat, tr_k, te_k); gc()
  }
  print(boost_cv, row.names = FALSE, digits = 4)
}
rm(model_data_clean); gc()


# ============================================================
# STEP 5 - depth ladder, SHAP pairs, GAM with explicit interaction
# ============================================================

main_res <- readRDS(file.path(OUT_PATH, "main_split_results.rds"))

boost_ladder    <- data.frame()
gam_int_results <- data.frame()

for (reg in c("crisis", "calm")) {
  
  d <- frames[[reg]]
  set.seed(123)
  all_ids    <- unique(d$loan_id)
  train_ids  <- sample(all_ids, size = floor(0.7 * length(all_ids)))
  train_data <- d %>% filter(loan_id %in% train_ids)
  test_data  <- d %>% filter(!(loan_id %in% train_ids))
  
  # depth ladder
  for (dep in c(1, 2, 3, 5)) {
    cat("\nSTEP 5", reg, "depth", dep, format(Sys.time()), "\n")
    r <- fit_xgb(train_data, test_data, dep, feats_combined)
    boost_ladder <- rbind(boost_ladder,
                          data.frame(regime = reg, depth = dep, auc = r$auc,
                                     brier = r$brier, best_iter = r$best_iter))
    saveRDS(boost_ladder, file.path(OUT_PATH, "boost_depth_ladder.rds"))
    
    # SHAP interaction pairs: crisis, depth 2
    if (reg == "crisis" & dep == 2) {
      X_test <- model.matrix(~ . - 1, data = test_data[, feats_combined])
      set.seed(789)
      idx <- sample(nrow(X_test), min(200000, nrow(X_test)))
      shap_int <- predict(r$model, xgb.DMatrix(X_test[idx, ]), predinteraction = TRUE)
      m_int <- apply(abs(shap_int), c(2, 3), mean)
      diag(m_int) <- 0
      pair_idx <- which(upper.tri(m_int), arr.ind = TRUE)
      shap_pairs <- data.frame(
        f1       = colnames(X_test)[pair_idx[, 1]],
        f2       = colnames(X_test)[pair_idx[, 2]],
        strength = m_int[upper.tri(m_int)])
      shap_pairs <- shap_pairs[order(-shap_pairs$strength), ]
      saveRDS(shap_pairs, file.path(OUT_PATH, "boost_shap_pairs.rds"))
      print(head(shap_pairs, 10), row.names = FALSE)
      rm(X_test, shap_int, m_int); gc()
    }
    rm(r); gc()
  }
  
  # GAM with the FICO x LTV interaction made explicit
  cat("\nSTEP 5", reg, "GAM with interaction", format(Sys.time()), "\n")
  gam_int <- bam(
    default ~ s(loan_age) + s(credit_score) + s(ltv_current) + s(unemployment) +
      ti(ltv_current, credit_score) +
      s(interest_rate_orig) + amortization_type + vintage,
    family = binomial("cloglog"), data = train_data, discrete = TRUE)
  p_int <- as.numeric(predict(gam_int, newdata = test_data, type = "response"))
  
  auc_no_int <- main_res$auc[main_res$regime == reg & main_res$model == "gam_combined"]
  auc_int    <- as.numeric(auc(roc(test_data$default, p_int, quiet = TRUE)))
  gam_int_results <- rbind(gam_int_results, data.frame(
    regime = reg, auc_with_int = auc_int, auc_no_int = auc_no_int,
    gain = auc_int - auc_no_int))
  saveRDS(gam_int_results, file.path(OUT_PATH, "gam_interaction_results.rds"))
  
  rm(gam_int, train_data, test_data, d); gc()
}

print(boost_ladder,    row.names = FALSE, digits = 4)
print(gam_int_results, row.names = FALSE, digits = 4)

cat("\nALL DONE", format(Sys.time()), "\n")
