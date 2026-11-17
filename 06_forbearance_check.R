# ============================================================
# 06  FORBEARANCE AND THE END OF THE CALM WINDOW   (3.3, Appendix A.1)
#
# Everything printed also goes to  data/final_run/checks_forbearance.txt
#
#   PART 1  raw 2005 files: ninety-day loan-months with the forbearance flag
#   PART 2  2016 to 2020 panel: first defaults in 2020 against earlier years
#
# The borrower assistance field (column 30 of the servicing file) is only
# populated from January 2014. A zero count in the crisis window therefore
# does not show that forbearance was absent there; it cannot be observed.
# ============================================================

source("00_setup.R")

sink(file.path(OUT_PATH, "checks_forbearance.txt"), split = TRUE)
options(width = 200)


# ============================================================
# PART 1 - raw servicing files of the 2005 vintage
# Column 2 = reporting period, 4 = delinquency status,
# 30 = borrower assistance status ("F" = forbearance).
# ============================================================
cat("\n\n===== PART 1  FORBEARANCE FLAG IN THE RAW 2005 FILES =====\n")

raw_files <- c(
  "Standard sample 2005" =
    file.path(BASE_PATH, "sample_2005/sample_svcg_2005.txt"),
  "Non-Standard 2005 Q1" =
    file.path(BASE_PATH, "non_std_historical_data/historical_data_excl_2005/historical_data_excl_2005Q1/historical_data_excl_time_2005Q1.txt")
)

forb <- data.frame()

for (i in 1:length(raw_files)) {

  if (!file.exists(raw_files[i])) { cat("file not found, skipped:", raw_files[i], "\n"); next }

  raw <- fread(raw_files[i], sep = "|", header = FALSE, showProgress = FALSE,
               colClasses = "character")

  d <- data.frame(period = as.numeric(raw$V2),
                  dq     = raw$V4,
                  assist = raw$V30)
  rm(raw); gc()

  ninety   <- d$dq == "3"
  flag     <- d$assist == "F"
  crisis_w <- d$period > 200712 & d$period < 201201
  calm_w   <- d$period > 201512 & d$period < 202001
  after_w  <- d$period > 201912

  forb <- rbind(forb, data.frame(
    file             = names(raw_files)[i],
    rows             = nrow(d),
    ninety_all       = sum(ninety),
    ninety_F_all     = sum(ninety & flag),
    ninety_crisis    = sum(ninety & crisis_w),
    ninety_F_crisis  = sum(ninety & flag & crisis_w),
    ninety_calm      = sum(ninety & calm_w),
    ninety_F_calm    = sum(ninety & flag & calm_w),
    ninety_F_2020on  = sum(ninety & flag & after_w)))

  rm(d); gc()
}

forb$pct_F_calm    <- round(forb$ninety_F_calm / forb$ninety_calm * 100, 2)
forb$pct_F_windows <- round((forb$ninety_F_crisis + forb$ninety_F_calm) /
                              (forb$ninety_crisis + forb$ninety_calm) * 100, 2)
print(forb, row.names = FALSE)


# ============================================================
# PART 2 - first defaults in 2020 against 2016 to 2019
# Uses the 2016 to 2020 panel (vintages 2014 to 2016), which only serves to
# justify ending the calm window in 2019. Skipped if the files are not on disk.
# ============================================================
cat("\n\n===== PART 2  FIRST DEFAULTS, 2020 AGAINST 2016 TO 2019 =====\n")

f_panel <- file.path(BASE_PATH, "panel_calm_16_20_vintages_14-15-16_clean.rds")
f_frame <- file.path(BASE_PATH, "model_data_calm_16_20_vintages_14-15-16_clean.rds")

if (file.exists(f_frame)) {

  data_1620 <- readRDS(f_frame)

  # credit score of first defaulters: 2020 against the earlier years
  print(as.data.frame(
    data_1620 %>%
      filter(default == 1) %>%
      mutate(period = ifelse(year_current == 2020, "2020", "2016-19")) %>%
      group_by(period) %>%
      summarise(n           = n(),
                mean_fico   = mean(credit_score, na.rm = TRUE),
                median_fico = median(credit_score, na.rm = TRUE))))
  rm(data_1620); gc()

} else {
  cat("estimation frame 2016 to 2020 not found, skipped\n")
}

if (file.exists(f_panel)) {

  panel_1620 <- readRDS(f_panel)

  # Assistance status of defaulted loans by year of first default.
  # Needs borrower_assistance_status in the panel, that is, the panel has to
  # be built with that column added to svcg_keep in 01_build_panels.R.
  if ("borrower_assistance_status" %in% names(panel_1620)) {

    print(as.data.frame(
      panel_1620 %>%
        filter(default == 1) %>%
        group_by(loan_id) %>%
        summarise(first_yr = min(year_current),
                  code = paste(sort(unique(trimws(borrower_assistance_status))),
                               collapse = ""),
                  .groups = "drop") %>%
        group_by(first_yr) %>%
        summarise(n     = n(),
                  pct_F = mean(grepl("F", code)) * 100,     # forbearance
                  pct_R = mean(grepl("R", code)) * 100,     # repayment plan
                  pct_T = mean(grepl("T", code)) * 100)))   # trial modification

  } else {
    cat("borrower_assistance_status is not in the panel, table by year skipped\n")
  }
  rm(panel_1620); gc()

} else {
  cat("panel 2016 to 2020 not found, skipped\n")
}

cat("\n\nALL DONE", format(Sys.time()), "\n")
sink()
