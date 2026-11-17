# ============================================================
# 05  FIGURES - run after 02_main_run.R
# Reads only from data/final_run/ and writes to data/final_run/figures/
#
#   Figure 1   federal funds rate and monthly default rate     (3.4)
#   Figure 2   thin plate basis functions, illustration        (4.1.2)
#   Figure 3   fold-level increments, three model classes      (5.1)
#   Figure 4   partial effects, GAM against linear             (5.4)
#   Figure 5   boosting depth ladder                           (5.5)
#   Figure B1  calibration deciles                             (B.4)
#
# Each figure is its own block and can be run alone after the first lines.
# All figures are saved at their printed size (text width 6.1 inches), so
# the font sizes in the files are the font sizes on the page.
# ============================================================

source("00_setup.R")

crisis <- readRDS(file.path(OUT_PATH, "model_data_crisis_final.rds"))
calm   <- readRDS(file.path(OUT_PATH, "model_data_calm_final.rds"))

# ============================================================
# FIGURE 1 - federal funds rate and monthly default rate
# ============================================================

rate_raw <- read.csv2(file.path(BASE_PATH, "chart_20261107T080541.csv"), stringsAsFactors = FALSE)
rate_raw$Date <- gsub(".", "/", rate_raw$Date, fixed = TRUE)
rate_raw$Date <- as.Date(rate_raw$Date, format = "%m/%d/%Y")

rate_month <- rate_raw %>%
  mutate(month = as.Date(format(Date, "%Y-%m-01"))) %>%
  group_by(month) %>%
  summarise(rate = mean(Value)) %>%
  ungroup()

def_crisis <- crisis %>%
  group_by(reporting_period) %>%
  summarise(hazard = mean(default)) %>%
  ungroup() %>%
  mutate(panel = "crisis")

def_calm <- calm %>%
  group_by(reporting_period) %>%
  summarise(hazard = mean(default)) %>%
  ungroup() %>%
  mutate(panel = "calm")

def_month <- bind_rows(def_crisis, def_calm) %>%
  mutate(month = as.Date(paste0(reporting_period, "01"), format = "%Y%m%d"),
         default_rate = 100 * (1 - (1 - hazard)^12))

# Look at the first months before deciding whether anything still has to be
# dropped. With the pre-window defaults excluded there should be no bump left.
head(def_month[def_month$panel == "crisis", c("month", "default_rate")], 6)
head(def_month[def_month$panel == "calm",   c("month", "default_rate")], 6)

periods <- data.frame(
  start = as.Date(c("2005-01-01", "2008-01-01", "2013-01-01", "2016-01-01")),
  end   = as.Date(c("2008-01-01", "2012-01-01", "2016-01-01", "2020-01-01")),
  type  = c("Origination (vintages)", "Observation window",
            "Origination (vintages)", "Observation window")
)

regimes <- data.frame(
  mid   = as.Date(c("2008-07-01", "2016-07-01")),
  label = c("Crisis", "Calm")
)

fig1 <- ggplot() +
  geom_rect(data = periods,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = type)) +
  geom_text(data = regimes, aes(x = mid, y = 10.1, label = label),
            size = 3.2, colour = "grey15", fontface = "bold") +
  geom_line(data = rate_month,
            aes(x = month, y = rate, colour = "Federal funds rate"),
            linewidth = 0.8) +
  geom_line(data = def_month,
            aes(x = month, y = default_rate, group = panel,
                colour = "Default rate (annualised)"),
            linewidth = 0.8) +
  scale_fill_manual(values = c("Origination (vintages)" = "grey93",
                               "Observation window" = "grey82"), name = NULL) +
  scale_colour_manual(values = c("Federal funds rate" = "#1f4e79",
                                 "Default rate (annualised)" = "#c0392b"),
                      name = NULL) +
  scale_x_date(breaks = seq(as.Date("2004-01-01"), as.Date("2022-01-01"), by = "2 years"),
               date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_cartesian(xlim = as.Date(c("2004-01-01", "2022-12-31")),
                  ylim = c(0, 10.6), expand = FALSE) +
  labs(x = NULL, y = "per cent") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

print(fig1)
ggsave(file.path(FIG_PATH, "fig1_rates_default.pdf"), fig1, width = 6.1, height = 3.6)
ggsave(file.path(FIG_PATH, "fig1_rates_default.png"), fig1, width = 6.1, height = 3.6, dpi = 300, bg = "white")

summary(def_month$default_rate[def_month$panel == "crisis"])
summary(def_month$default_rate[def_month$panel == "calm"])

# ============================================================
# FIGURE 2 - thin plate basis at three basis dimensions, s(loan_age)
# Illustration only: baseline-only fit on 40'000 crisis loans.
# ============================================================

set.seed(123)
all_ids  <- unique(crisis$loan_id)
keep_ids <- sample(all_ids, size = min(40000, length(all_ids)))
dat      <- crisis[crisis$loan_id %in% keep_ids, ]

dat$default  <- as.numeric(dat$default)
dat$loan_age <- as.numeric(dat$loan_age)

# empirical monthly default rate by loan age, on the cloglog scale
emp_n   <- tapply(dat$default, dat$loan_age, length)
emp_d   <- tapply(dat$default, dat$loan_age, sum)
emp_age <- as.numeric(names(emp_n))
emp_h   <- emp_d / emp_n
emp_lp  <- log(-log(1 - emp_h))

ok      <- is.finite(emp_lp) & emp_n >= 500
emp_age <- emp_age[ok]
emp_lp  <- emp_lp[ok]

grid <- data.frame(loan_age = seq(min(dat$loan_age), max(dat$loan_age), length.out = 300))

k_values <- c(3, 7, 10)

# fit all three first, so the y range can be set from the fitted curves
fits   <- list()
allval <- emp_lp

for (i in 1:3) {
  fits[[i]] <- bam(default ~ s(loan_age, k = k_values[i], bs = "tp"),
                   data = dat, family = binomial(link = "cloglog"), discrete = TRUE)
  X  <- predict(fits[[i]], newdata = grid, type = "lpmatrix")
  cf <- coef(fits[[i]])
  allval <- c(allval, as.numeric(X %*% cf))
}

y_lo <- min(allval) - 0.3
y_hi <- max(allval) + 0.3

# drawn twice, once into the pdf and once into the png
for (dev_type in c("pdf", "png")) {

  if (dev_type == "pdf") pdf(file.path(FIG_PATH, "fig2_basis_functions.pdf"), width = 6.1, height = 2.7)
  if (dev_type == "png") png(file.path(FIG_PATH, "fig2_basis_functions.png"), width = 6.1, height = 2.7,
                             units = "in", res = 300)

  par(mfrow = c(1, 3), mar = c(3.4, 3.4, 2.2, 0.5), mgp = c(2.1, 0.7, 0),
      cex.main = 1.0, cex.lab = 1.1, cex.axis = 1.0)

  for (i in 1:3) {

    fit  <- fits[[i]]
    X    <- predict(fit, newdata = grid, type = "lpmatrix")
    cf   <- coef(fit)
    full <- as.numeric(X %*% cf)

    # each basis function times its coefficient, shifted by the intercept
    n_basis <- ncol(X) - 1
    scaled  <- matrix(NA, nrow = nrow(grid), ncol = n_basis)
    for (j in 1:n_basis) {
      scaled[, j] <- X[, j + 1] * cf[j + 1] + cf[1]
    }

    plot(emp_age, emp_lp,
         pch = 16, col = rgb(0.55, 0.55, 0.55, 0.5), cex = 0.6,
         xlim = range(grid$loan_age), ylim = c(y_lo, y_hi),
         xlab = "loan age (months)",
         ylab = if (i == 1) "cloglog(hazard)" else "",
         main = paste0("k = ", k_values[i], " (edf = ",
                       round(sum(fit$edf[-1]), 2), ")"))

    cols <- rainbow(n_basis, v = 0.85)

    # drop points outside the panel instead of clipping them flat
    for (j in 1:n_basis) {
      yj <- scaled[, j]
      yj[yj < y_lo | yj > y_hi] <- NA
      lines(grid$loan_age, yj, col = cols[j], lwd = 1)
    }

    lines(grid$loan_age, full, col = "black", lwd = 2.8)
  }

  dev.off()
}

rm(dat, fits, grid); gc()


# ============================================================
# FIGURE 3 - fold-level increments
# ============================================================

cv_crisis    <- readRDS(file.path(OUT_PATH, "cv_results_crisis_final.rds"))
cv_calm      <- readRDS(file.path(OUT_PATH, "cv_results_calm_final.rds"))
boost_crisis <- readRDS(file.path(OUT_PATH, "boost_cv_crisis_final.rds"))
boost_calm   <- readRDS(file.path(OUT_PATH, "boost_cv_calm_final.rds"))

incr <- bind_rows(
  data.frame(panel = lev_reg[1], model = "linear",   increment = cv_crisis$fico_incr_lin),
  data.frame(panel = lev_reg[1], model = "GAM",      increment = cv_crisis$fico_incr_gam),
  data.frame(panel = lev_reg[1], model = "boosting", increment = boost_crisis$increment),
  data.frame(panel = lev_reg[2], model = "linear",   increment = cv_calm$fico_incr_lin),
  data.frame(panel = lev_reg[2], model = "GAM",      increment = cv_calm$fico_incr_gam),
  data.frame(panel = lev_reg[2], model = "boosting", increment = boost_calm$increment)
)
incr$panel <- factor(incr$panel, levels = lev_reg)
incr$model <- factor(incr$model, levels = c("linear", "GAM", "boosting"))
incr$ypos  <- as.numeric(incr$panel) +
  ifelse(incr$model == "linear", -0.14, ifelse(incr$model == "GAM", 0, 0.14))

gap_lo <- max(incr$increment[incr$panel == lev_reg[1]])
gap_hi <- min(incr$increment[incr$panel == lev_reg[2]])
cat("highest crisis fold:", round(gap_lo, 4), " lowest calm fold:", round(gap_hi, 4), "\n")

# same gap without boosting, for the sentence in 5.1
cat("linear and GAM only:",
    round(max(incr$increment[incr$panel == lev_reg[1] & incr$model != "boosting"]), 4), "to",
    round(min(incr$increment[incr$panel == lev_reg[2] & incr$model != "boosting"]), 4), "\n")

incr %>% group_by(panel, model) %>%
  summarise(mean = mean(increment), sd = sd(increment), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 3)

fig3 <- ggplot() +
  annotate("rect", xmin = gap_lo, xmax = gap_hi, ymin = -Inf, ymax = Inf, fill = "grey88") +
  annotate("text", x = (gap_lo + gap_hi) / 2, y = 2.42,
           label = paste0("no overlap: ", sprintf("%.4f", gap_lo), " to ", sprintf("%.4f", gap_hi)),
           size = 3, colour = "grey35") +
  geom_point(data = incr,
             aes(increment, ypos, colour = model, shape = model),
             size = 2.6, alpha = 0.9) +
  scale_y_continuous(breaks = c(1, 2),
                     labels = c("crisis\n2008-2011", "calm\n2016-2019"),
                     limits = c(0.65, 2.55)) +
  scale_colour_manual(values = c(linear = "grey45", GAM = "steelblue", boosting = "darkorange3")) +
  scale_shape_manual(values = c(linear = 16, GAM = 17, boosting = 15)) +
  labs(x = "Incremental AUC of the credit score", y = NULL, colour = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())

print(fig3)
ggsave(file.path(FIG_PATH, "fig3_fold_increments.pdf"), fig3, width = 6.1, height = 3.1)
ggsave(file.path(FIG_PATH, "fig3_fold_increments.png"), fig3, width = 6.1, height = 3.1, dpi = 300, bg = "white")


# ============================================================
# FIGURE 4 - partial effects on the link scale, GAM against linear
# Each ROW is one fitted model: curves within a row are comparable.
# Vertical size is NOT comparable between the two rows (scale, see 5.3).
# ============================================================

models_crisis <- readRDS(file.path(OUT_PATH, "models_crisis_final.rds"))
models_calm   <- readRDS(file.path(OUT_PATH, "models_calm_final.rds"))

centred_link <- function(gam_model, lin_model, dat, var, ref_val, n_grid = 200) {
  rng  <- as.numeric(quantile(dat[[var]], c(0.01, 0.99), na.rm = TRUE))
  grid <- seq(rng[1], rng[2], length.out = n_grid)
  
  base_row <- data.frame(
    credit_score       = median(dat$credit_score),
    ltv_current        = median(dat$ltv_current),
    unemployment       = median(dat$unemployment),
    interest_rate_orig = median(dat$interest_rate_orig),
    loan_age           = median(dat$loan_age),
    vintage            = factor(names(sort(table(dat$vintage), TRUE))[1],
                                levels = levels(dat$vintage)),
    amortization_type  = factor(names(sort(table(dat$amortization_type), TRUE))[1],
                                levels = levels(dat$amortization_type))
  )
  
  nd <- base_row[rep(1, n_grid), ]
  nd[[var]] <- grid
  nd_ref <- base_row
  nd_ref[[var]] <- ref_val
  
  X     <- predict(gam_model, newdata = nd,     type = "lpmatrix", discrete = FALSE)
  X_ref <- predict(gam_model, newdata = nd_ref, type = "lpmatrix", discrete = FALSE)
  D   <- sweep(X, 2, as.numeric(X_ref), "-")
  eta <- as.numeric(D %*% coef(gam_model))
  se  <- sqrt(pmax(0, rowSums((D %*% vcov(gam_model)) * D)))
  
  eta_lin <- as.numeric(predict(lin_model, newdata = nd,     type = "link", discrete = FALSE)) -
    as.numeric(predict(lin_model, newdata = nd_ref, type = "link", discrete = FALSE))
  
  data.frame(x = grid, eta = eta, lo = eta - 1.96 * se, hi = eta + 1.96 * se,
             eta_lin = eta_lin)
}

lev_var <- c("FICO at origination", "Current LTV", "State unemployment (%)")

ref_fico         <- 700
ref_ltv          <- 80
ref_unemp_crisis <- median(crisis$unemployment)
ref_unemp_calm   <- median(calm$unemployment)

d1 <- centred_link(models_crisis$gam_combined, models_crisis$lin_combined, crisis, "credit_score", ref_fico)
d1$regime <- lev_reg[1]; d1$variable <- lev_var[1]
d2 <- centred_link(models_crisis$gam_combined, models_crisis$lin_combined, crisis, "ltv_current", ref_ltv)
d2$regime <- lev_reg[1]; d2$variable <- lev_var[2]
d3 <- centred_link(models_crisis$gam_combined, models_crisis$lin_combined, crisis, "unemployment", ref_unemp_crisis)
d3$regime <- lev_reg[1]; d3$variable <- lev_var[3]
d4 <- centred_link(models_calm$gam_combined, models_calm$lin_combined, calm, "credit_score", ref_fico)
d4$regime <- lev_reg[2]; d4$variable <- lev_var[1]
d5 <- centred_link(models_calm$gam_combined, models_calm$lin_combined, calm, "ltv_current", ref_ltv)
d5$regime <- lev_reg[2]; d5$variable <- lev_var[2]
d6 <- centred_link(models_calm$gam_combined, models_calm$lin_combined, calm, "unemployment", ref_unemp_calm)
d6$regime <- lev_reg[2]; d6$variable <- lev_var[3]

d <- bind_rows(d1, d2, d3, d4, d5, d6)
d$regime   <- factor(d$regime,   levels = lev_reg)
d$variable <- factor(d$variable, levels = lev_var)

set.seed(123)
r1 <- data.frame(x = sample(crisis$credit_score, 4000), regime = lev_reg[1], variable = lev_var[1])
r1 <- r1[r1$x >= min(d1$x) & r1$x <= max(d1$x), ]
r2 <- data.frame(x = sample(crisis$ltv_current, 4000),  regime = lev_reg[1], variable = lev_var[2])
r2 <- r2[r2$x >= min(d2$x) & r2$x <= max(d2$x), ]
r3 <- data.frame(x = sample(crisis$unemployment, 4000), regime = lev_reg[1], variable = lev_var[3])
r3 <- r3[r3$x >= min(d3$x) & r3$x <= max(d3$x), ]
r4 <- data.frame(x = sample(calm$credit_score, 4000),   regime = lev_reg[2], variable = lev_var[1])
r4 <- r4[r4$x >= min(d4$x) & r4$x <= max(d4$x), ]
r5 <- data.frame(x = sample(calm$ltv_current, 4000),    regime = lev_reg[2], variable = lev_var[2])
r5 <- r5[r5$x >= min(d5$x) & r5$x <= max(d5$x), ]
r6 <- data.frame(x = sample(calm$unemployment, 4000),   regime = lev_reg[2], variable = lev_var[3])
r6 <- r6[r6$x >= min(d6$x) & r6$x <= max(d6$x), ]

rug <- bind_rows(r1, r2, r3, r4, r5, r6)
rug$regime   <- factor(rug$regime,   levels = lev_reg)
rug$variable <- factor(rug$variable, levels = lev_var)

refs <- data.frame(
  regime   = factor(c(lev_reg[1], lev_reg[1], lev_reg[1], lev_reg[2], lev_reg[2], lev_reg[2]), levels = lev_reg),
  variable = factor(c(lev_var, lev_var), levels = lev_var),
  ref      = c(ref_fico, ref_ltv, ref_unemp_crisis, ref_fico, ref_ltv, ref_unemp_calm)
)

fig4 <- ggplot(d, aes(x)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_vline(data = refs, aes(xintercept = ref),
             colour = "grey70", linewidth = 0.3, linetype = "dotted") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.18) +
  geom_line(aes(y = eta_lin, colour = "linear", linetype = "linear"), linewidth = 0.7) +
  geom_line(aes(y = eta,     colour = "GAM",    linetype = "GAM"),    linewidth = 0.8) +
  geom_rug(data = rug, aes(x = x), inherit.aes = FALSE,
           alpha = 0.03, length = unit(0.03, "npc")) +
  facet_grid(regime ~ variable, scales = "free_x", switch = "x") +
  scale_colour_manual(values = c(GAM = "steelblue", linear = "grey35")) +
  scale_linetype_manual(values = c(GAM = "solid", linear = "dashed")) +
  labs(x = NULL, y = "log hazard ratio against the reference value",
       colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", strip.placement = "outside")

print(fig4)
ggsave(file.path(FIG_PATH, "fig4_partial_effects.pdf"), fig4, width = 6.1, height = 4.4)
ggsave(file.path(FIG_PATH, "fig4_partial_effects.png"), fig4, width = 6.1, height = 4.4, dpi = 300, bg = "white")

# numbers for the text: vertical range of each curve (compare within a regime only)
d %>%
  group_by(regime, variable) %>%
  summarise(range_gam = max(eta) - min(eta),
            range_lin = max(eta_lin) - min(eta_lin),
            x_min = min(x), x_max = max(x), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 3)

# crisis LTV: change from 55 to 65 against 95 to 105
d2$eta[which.min(abs(d2$x - 65))]  - d2$eta[which.min(abs(d2$x - 55))]
d2$eta[which.min(abs(d2$x - 105))] - d2$eta[which.min(abs(d2$x - 95))]

# EDF table for 5.4
summary(models_crisis$gam_combined)$s.table
summary(models_calm$gam_combined)$s.table

# standardised contributions for Table 3 (combined linear model, training rows)
b_crisis <- coef(models_crisis$lin_combined)
b_calm   <- coef(models_calm$lin_combined)
for (v in c("credit_score", "ltv_current", "unemployment", "interest_rate_orig")) {
  cat(v, "| crisis", round(abs(b_crisis[v]) * sd(crisis[[v]]), 3),
      "| calm",   round(abs(b_calm[v])   * sd(calm[[v]]),   3), "\n")
}

rm(models_crisis, models_calm); gc()


# ============================================================
# FIGURE 5 - boosting depth ladder, gain over depth one
# ============================================================

ladder <- readRDS(file.path(OUT_PATH, "boost_depth_ladder.rds"))
print(ladder, row.names = FALSE, digits = 4)

ladder$gain <- NA
ladder$gain[ladder$regime == "crisis"] <- ladder$auc[ladder$regime == "crisis"] -
  ladder$auc[ladder$regime == "crisis" & ladder$depth == 1]
ladder$gain[ladder$regime == "calm"] <- ladder$auc[ladder$regime == "calm"] -
  ladder$auc[ladder$regime == "calm" & ladder$depth == 1]
ladder$panel <- factor(ifelse(ladder$regime == "crisis", lev_reg[1], lev_reg[2]), levels = lev_reg)

fig5 <- ggplot(ladder, aes(x = depth, y = gain, linetype = panel, shape = panel)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_line(colour = "grey30") +
  geom_point(size = 2.6, colour = "grey20") +
  scale_shape_manual(values = c(16, 1), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_continuous(breaks = c(1, 2, 3, 5)) +
  labs(x = "Maximum tree depth", y = "AUC gain over depth one") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

print(fig5)
ggsave(file.path(FIG_PATH, "fig5_depth_ladder.pdf"), fig5, width = 4.6, height = 3.1)
ggsave(file.path(FIG_PATH, "fig5_depth_ladder.png"), fig5, width = 4.6, height = 3.1, dpi = 300, bg = "white")


# ============================================================
# FIGURE B1 - calibration deciles, combined GAM
# ============================================================

cal <- readRDS(file.path(OUT_PATH, "calibration_deciles.rds"))
cal$panel <- factor(ifelse(cal$regime == "crisis", lev_reg[1], lev_reg[2]), levels = lev_reg)

fig_cal <- ggplot(cal, aes(pred_rate * 100, actual_rate * 100)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(colour = "steelblue", size = 2) +
  geom_line(colour = "steelblue", linewidth = 0.5) +
  facet_wrap(~ panel, scales = "free") +
  labs(x = "Mean predicted monthly default rate (%)",
       y = "Realised monthly default rate (%)") +
  theme_minimal(base_size = 10)

print(fig_cal)
ggsave(file.path(FIG_PATH, "figB1_calibration_deciles.pdf"), fig_cal, width = 6.1, height = 3.0)
ggsave(file.path(FIG_PATH, "figB1_calibration_deciles.png"), fig_cal, width = 6.1, height = 3.0, dpi = 300, bg = "white")
