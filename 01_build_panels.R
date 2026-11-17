# ============================================================
# 01  BUILD THE LOAN-MONTH PANELS            (thesis chapter 3)
#
# Raw Freddie Mac files -> one loan-month panel per specification.
#
#   STEP A  per vintage: read the raw files, draw the Non-Standard sample,
#           keep the observation window, cache               (3.1)
#   STEP B  clean, define default, merge house prices and
#           unemployment, construct current LTV              (3.2, 3.4)
#   STEP C  the two data treatments            (3.2, 3.4)
#             1 balance on the final record
#             2 loans already in default before the window
#
# Output per specification, in BASE_PATH:
#   panel_<tag>_clean.rds   panel before the two data treatments
#   panel_<tag>_final.rds   panel used everywhere else
#
# A file that already exists is not built again. The Non-Standard sample of
# a vintage is drawn once and then taken from the cache, so the cache folder
# is the authoritative record of the sample and should be kept.
#
# Every raw file is read as TEXT and converted to numbers in one place
# (B1 and B2), so nothing is guessed while reading.
# ============================================================

source("00_setup.R")
setDTthreads(0)       # use all CPU cores for reading


# ---- Specifications ----
# 1 and 2 are the main specification, 3 and 4 the alternative vintages (5.6).

spec_regime   <- c("crisis", "calm", "crisis", "calm")
spec_vintages <- list(c(2005, 2006, 2007),
                      c(2013, 2014, 2015),
                      c(1999, 2001, 2003, 2005, 2006, 2007),
                      c(2014, 2015, 2016))
spec_window   <- list(c("2008", "2009", "2010", "2011"),
                      c("2016", "2017", "2018", "2019"),
                      c("2008", "2009", "2010", "2011"),
                      c("2016", "2017", "2018", "2019"))


# ---- Constants ----

# Total Standard loans per vintage (Freddie Mac dataset documentation).
# Used only to size the Non-Standard draw so the mix matches the book.
n_std_total <- c("1999" = 1217693, "2000" = 746368, "2001" = 2497048,
                 "2002" = 2992675, "2003" = 3904406, "2004" = 1677201,
                 "2005" = 1611457, "2006" = 1193544, "2007" = 1238192,
                 "2008" = 1233144, "2009" = 2016079, "2010" = 1271746,
                 "2011" = 955928,  "2012" = 1332377, "2013" = 1303416,
                 "2014" = 982229,  "2015" = 1352901, "2016" = 1606745,
                 "2017" = 1332583, "2018" = 1272375, "2019" = 1780033,
                 "2020" = 3913742, "2021" = 4096370)

STD_SAMPLE_N <- 50000   # size of the official Standard sample per vintage

orig_col_names <- c(
  "credit_score", "first_payment_date", "first_time_homebuyer",
  "maturity_date", "msa", "mi_pct", "n_units", "occupancy_status",
  "cltv", "dti", "upb_orig", "ltv_orig", "interest_rate_orig",
  "channel", "ppm_flag", "amortization_type", "property_state",
  "property_type", "postal_code", "loan_id", "loan_purpose",
  "loan_term", "n_borrowers", "seller_name", "servicer_name",
  "super_conforming_flag", "pre_relief_refinance_id",
  "special_eligibility", "relief_refinance_indicator"
)
svcg_col_names <- c(
  "loan_id", "reporting_period", "upb_current", "delinquency_status",
  "loan_age", "months_to_maturity", "defect_settlement_date",
  "modification_flag", "zero_balance_code", "zero_balance_date",
  "interest_rate_current", "upb_non_interest_bearing", "ddlpi",
  "mi_recoveries", "net_sale_proceeds", "non_mi_recoveries",
  "total_expenses", "legal_costs", "maintenance_costs",
  "taxes_insurance", "misc_expenses", "actual_loss",
  "cumulative_modification_cost", "interest_rate_step_indicator",
  "payment_deferral_flag", "eltv", "zero_balance_removal_upb",
  "delinquent_accrued_interest", "delinquency_due_to_disaster",
  "borrower_assistance_status", "current_month_modification_cost",
  "upb_interest_bearing"
)

# Only the columns the thesis needs (keeps the cached files small)
orig_keep <- c("loan_id", "credit_score", "first_payment_date", "maturity_date",
               "msa", "dti", "ltv_orig", "upb_orig", "interest_rate_orig",
               "amortization_type", "property_state", "occupancy_status", "n_units")
svcg_keep <- c("loan_id", "reporting_period", "upb_current", "delinquency_status",
               "loan_age", "months_to_maturity", "interest_rate_current", "eltv")


# ============================================================
# Loop over the specifications
# ============================================================

for (i in 1:length(spec_regime)) {

  REGIME       <- spec_regime[i]
  vintages     <- spec_vintages[[i]]
  window_years <- spec_window[[i]]

  # tags, e.g. "crisis_08_11" and "crisis_08_11_vintages_05-06-07"
  WINDOW_TAG  <- paste0(substr(min(window_years), 3, 4), "_",
                        substr(max(window_years), 3, 4))
  TAG         <- paste0(REGIME, "_", WINDOW_TAG)
  RESULTS_TAG <- paste0(TAG, "_vintages_", paste0(substr(vintages, 3, 4), collapse = "-"))

  panel_clean_file <- file.path(BASE_PATH, paste0("panel_", RESULTS_TAG, "_clean.rds"))
  panel_final_file <- file.path(BASE_PATH, paste0("panel_", RESULTS_TAG, "_final.rds"))

  cat("\n\n==== Specification", i, "|", RESULTS_TAG, "====\n")

  if (file.exists(panel_final_file)) {
    cat("final panel exists, skipped\n")
    next
  }

  if (file.exists(panel_clean_file)) {

    panel <- readRDS(panel_clean_file)
    cat("loaded panel before the data treatments:", nrow(panel), "loan-months\n")

  } else {

    # ========================================================
    # STEP A - per vintage: read, sample, keep the window, cache
    # ========================================================

    set.seed(123)   # reproducible Non-Standard draw

    for (v in vintages) {
      cat("\n-- vintage", v, "--\n")

      orig_cache <- file.path(CACHE_DIR, paste0("orig_", v, ".rds"))            # window-independent
      svcg_cache <- file.path(CACHE_DIR, paste0("svcg_", TAG, "_", v, ".rds"))  # window-dependent

      if (file.exists(orig_cache) & file.exists(svcg_cache)) {
        cat("  cached, skipped\n")
        next
      }

      # -- A1. Origination files and the Non-Standard draw --
      if (file.exists(orig_cache)) {

        # the sample of this vintage was drawn before: reuse exactly those loans
        orig_v   <- readRDS(orig_cache)
        keep_ids <- orig_v$loan_id[orig_v$dataset == "non_standard"]
        cat("  Non-Standard loans (from cache):", length(keep_ids), "\n")

      } else {

        # Non-Standard origination: all four quarters
        nonstd_orig <- data.frame()
        for (q in 1:4) {
          f <- file.path(BASE_PATH, "non_std_historical_data",
                         paste0("historical_data_excl_", v),
                         paste0("historical_data_excl_", v, "Q", q),
                         paste0("historical_data_excl_", v, "Q", q, ".txt"))
          if (!file.exists(f)) next
          part <- fread(f, sep = "|", header = FALSE, showProgress = FALSE,
                        colClasses = "character")
          setnames(part, 1:length(orig_col_names), orig_col_names)
          part <- setDF(part)[, orig_keep]
          nonstd_orig <- bind_rows(nonstd_orig, part)
        }
        cat("  Non-Standard loans (full):", nrow(nonstd_orig), "\n")

        if (nrow(nonstd_orig) == 0) {
          stop("No Non-Standard origination files found for vintage ", v)
        }
        if (is.na(n_std_total[as.character(v)])) {
          stop("Fill n_std_total for vintage ", v, " before running.")
        }

        # draw sized so that Standard / Non-Standard matches the book in vintage v
        draw_n   <- round(STD_SAMPLE_N * nrow(nonstd_orig) / n_std_total[as.character(v)])
        keep_ids <- sample(nonstd_orig$loan_id, size = min(draw_n, nrow(nonstd_orig)))
        nonstd_orig <- nonstd_orig %>% filter(loan_id %in% keep_ids)
        cat("  Non-Standard loans (drawn):", nrow(nonstd_orig), "\n")

        # Standard origination: the official 50k sample
        f <- file.path(BASE_PATH, paste0("sample_", v), paste0("sample_orig_", v, ".txt"))
        std_orig <- fread(f, sep = "|", header = FALSE, showProgress = FALSE,
                          colClasses = "character")
        setnames(std_orig, 1:length(orig_col_names), orig_col_names)
        std_orig <- setDF(std_orig)[, orig_keep]

        nonstd_orig$dataset <- "non_standard"; nonstd_orig$vintage <- v
        std_orig$dataset    <- "standard";     std_orig$vintage    <- v

        orig_v <- bind_rows(std_orig, nonstd_orig)
        saveRDS(orig_v, orig_cache)
        rm(nonstd_orig, std_orig)
      }

      # -- A2. Non-Standard servicing: sampled loans, window only --
      nonstd_svcg <- data.frame()
      for (q in 1:4) {
        f <- file.path(BASE_PATH, "non_std_historical_data",
                       paste0("historical_data_excl_", v),
                       paste0("historical_data_excl_", v, "Q", q),
                       paste0("historical_data_excl_time_", v, "Q", q, ".txt"))
        if (!file.exists(f)) next
        part <- fread(f, sep = "|", header = FALSE, showProgress = FALSE,
                      colClasses = "character")
        setnames(part, 1:length(svcg_col_names), svcg_col_names)
        part <- part[loan_id %in% keep_ids &
                       substr(reporting_period, 1, 4) %in% window_years]
        part <- setDF(part)[, svcg_keep]
        nonstd_svcg <- bind_rows(nonstd_svcg, part)
      }
      cat("  Non-Standard servicing rows:", nrow(nonstd_svcg), "\n")

      # -- A3. Standard servicing: the sample, window only --
      f <- file.path(BASE_PATH, paste0("sample_", v), paste0("sample_svcg_", v, ".txt"))
      std_svcg <- fread(f, sep = "|", header = FALSE, showProgress = FALSE,
                        colClasses = "character")
      setnames(std_svcg, 1:length(svcg_col_names), svcg_col_names)
      std_svcg <- std_svcg[substr(reporting_period, 1, 4) %in% window_years]
      std_svcg <- setDF(std_svcg)[, svcg_keep]
      cat("  Standard servicing rows:", nrow(std_svcg), "\n")

      svcg_v <- bind_rows(std_svcg, nonstd_svcg)
      saveRDS(svcg_v, svcg_cache)
      rm(nonstd_svcg, std_svcg, svcg_v, orig_v)
      gc()
    }

    # -- A4. Stack the vintages --
    orig_all <- data.frame()
    svcg_all <- data.frame()
    for (v in vintages) {
      orig_all <- bind_rows(orig_all,
                            readRDS(file.path(CACHE_DIR, paste0("orig_", v, ".rds"))))
      svcg_all <- bind_rows(svcg_all,
                            readRDS(file.path(CACHE_DIR, paste0("svcg_", TAG, "_", v, ".rds"))))
    }
    cat("\norigination loans:", nrow(orig_all), "| servicing rows:", nrow(svcg_all), "\n")


    # ========================================================
    # STEP B - clean and build the loan-month panel
    # ========================================================

    # ---- B0. House price index (FHFA) and unemployment (BLS LAUS via FRED) ----
    data_hpi_msa   <- read_excel(file.path(BASE_PATH, "HPI/hpi_at_metro.xlsx"), skip = 2)
    data_hpi_state <- read_excel(file.path(BASE_PATH, "HPI/hpi_at_state.xlsx"), skip = 2)

    unemp_file <- file.path(CACHE_DIR, "unemp_state.rds")
    if (file.exists(unemp_file)) {
      unemp_state <- readRDS(unemp_file)
    } else {
      # Needs a personal FRED API key. Store it once on the computer with
      #   Sys.setenv(FRED_API_KEY = "...")   and never write it into a script.
      library(fredr)
      fredr_set_key(Sys.getenv("FRED_API_KEY"))
      state_codes <- c(state.abb, "DC")   # 50 states + District of Columbia
      unemp_state <- data.frame()
      for (s in state_codes) {
        out <- try(
          fredr(series_id         = paste0(s, "UR"),
                observation_start = as.Date("1999-01-01"),
                observation_end   = as.Date("2021-12-31")),
          silent = TRUE
        )
        if (inherits(out, "try-error") || nrow(out) == 0) {
          message("  no series for ", s); next
        }
        part <- data.frame(
          property_state  = s,
          year_current    = as.numeric(format(out$date, "%Y")),
          month_current   = as.numeric(format(out$date, "%m")),
          unemp_state_val = out$value
        )
        unemp_state <- bind_rows(unemp_state, part)
      }
      saveRDS(unemp_state, unemp_file)
    }

    # ---- B1. Clean origination (text -> numbers, then derived fields) ----
    orig_all <- orig_all %>%
      mutate(
        credit_score       = suppressWarnings(as.numeric(credit_score)),
        dti                = suppressWarnings(as.numeric(dti)),
        ltv_orig           = suppressWarnings(as.numeric(ltv_orig)),
        upb_orig           = suppressWarnings(as.numeric(upb_orig)),
        interest_rate_orig = suppressWarnings(as.numeric(interest_rate_orig)),
        n_units            = suppressWarnings(as.numeric(n_units)),
        msa                = suppressWarnings(as.numeric(msa)),
        ltv_orig   = ifelse(ltv_orig == 999, NA, ltv_orig),
        value_orig = ifelse(is.na(ltv_orig), NA, upb_orig / (ltv_orig / 100)),
        year_orig        = as.numeric(substr(first_payment_date, 1, 4)),
        month_orig       = as.numeric(substr(first_payment_date, 5, 6)),
        quarter_orig     = ceiling(month_orig / 3),
        yearquarter_orig = year_orig * 10 + quarter_orig
      )

    # ---- B2. Clean servicing and define default ----
    # Default = ninety or more days delinquent, or REO acquisition ("RA").
    svcg_all <- svcg_all %>%
      mutate(
        upb_current           = suppressWarnings(as.numeric(upb_current)),
        loan_age              = suppressWarnings(as.numeric(loan_age)),
        months_to_maturity    = suppressWarnings(as.numeric(months_to_maturity)),
        interest_rate_current = suppressWarnings(as.numeric(interest_rate_current)),
        eltv                  = suppressWarnings(as.numeric(eltv)),
        dq_num  = suppressWarnings(as.numeric(delinquency_status)),
        default = ifelse((!is.na(dq_num) & dq_num >= 3) |
                           trimws(delinquency_status) %in% "RA", 1, 0),
        year_current        = as.numeric(substr(reporting_period, 1, 4)),
        month_current       = as.numeric(substr(reporting_period, 5, 6)),
        quarter_current     = ceiling(month_current / 3),
        yearquarter_current = year_current * 10 + quarter_current
      )

    # ---- B3. Prepare the house price index ----
    data_hpi_msa <- data_hpi_msa %>%
      filter(Year >= 1999) %>%
      mutate(yearquarter = Year * 10 + Quarter, CBSACode = as.numeric(CBSACode))

    data_hpi_state <- data_hpi_state %>%
      filter(Year >= 1999) %>%
      mutate(yearquarter = Year * 10 + Quarter)

    # ---- B4. House price index at origination (MSA level, state fallback) ----
    orig_all <- orig_all %>%
      left_join(
        data_hpi_msa %>% select(CBSACode, yearquarter, `Index (NSA)`) %>%
          rename(hpi_orig_msa = `Index (NSA)`),
        by = c("msa" = "CBSACode", "yearquarter_orig" = "yearquarter")
      ) %>%
      left_join(
        data_hpi_state %>% select(Area, yearquarter, `Index (NSA)`) %>%
          rename(hpi_orig_state = `Index (NSA)`),
        by = c("property_state" = "Area", "yearquarter_orig" = "yearquarter")
      ) %>%
      mutate(hpi_orig = ifelse(!is.na(hpi_orig_msa), hpi_orig_msa, hpi_orig_state))

    # ---- B5. Merge, current house price index, current LTV, unemployment ----
    panel <- svcg_all %>%
      left_join(
        orig_all %>%
          select(loan_id, msa, property_state, ltv_orig, upb_orig, value_orig,
                 hpi_orig, credit_score, dti, occupancy_status,
                 amortization_type, n_units, interest_rate_orig,
                 dataset, vintage),
        by = "loan_id"
      ) %>%
      left_join(
        data_hpi_msa %>% select(CBSACode, yearquarter, `Index (NSA)`) %>%
          rename(hpi_current_msa = `Index (NSA)`),
        by = c("msa" = "CBSACode", "yearquarter_current" = "yearquarter")
      ) %>%
      left_join(
        data_hpi_state %>% select(Area, yearquarter, `Index (NSA)`) %>%
          rename(hpi_current_state = `Index (NSA)`),
        by = c("property_state" = "Area", "yearquarter_current" = "yearquarter")
      ) %>%
      mutate(
        hpi_current = ifelse(!is.na(hpi_current_msa), hpi_current_msa, hpi_current_state),
        hpi_current = as.numeric(hpi_current),
        hpi_orig    = as.numeric(hpi_orig)
      ) %>%
      filter(!property_state %in% c("PR", "GU", "VI")) %>%
      mutate(
        value_current = value_orig * (hpi_current / hpi_orig),
        ltv_current   = (upb_current / value_current) * 100
      ) %>%
      left_join(unemp_state,
                by = c("property_state", "year_current", "month_current")) %>%
      mutate(unemployment = unemp_state_val) %>%
      filter(!is.na(ltv_current))

    # ---- B6. Missing-value codes, labels, save ----
    panel <- panel %>%
      mutate(
        credit_score = ifelse(credit_score == 9999, NA, credit_score),
        dti          = ifelse(dti == 999,           NA, dti),
        n_units      = ifelse(n_units == 99,        NA, n_units),
        eltv         = ifelse(eltv == 999,          NA, eltv),
        regime       = REGIME
      ) %>%
      select(-hpi_current_msa, -hpi_current_state, -value_current, -unemp_state_val)

    cat("unemployment matched:", round(mean(!is.na(panel$unemployment)) * 100, 2), "%\n")

    saveRDS(panel, panel_clean_file)
    cat("saved:", panel_clean_file, "\n")
    rm(orig_all, svcg_all, data_hpi_msa, data_hpi_state, unemp_state); gc()
  }


  # ==========================================================
  # STEP C - the two data treatments
  # ==========================================================

  panel <- panel %>%
    arrange(loan_id, reporting_period) %>%
    group_by(loan_id) %>%
    mutate(row_nr   = row_number(),
           upb_prev = lag(upb_current)) %>%
    ungroup()

  # Treatment 1: Freddie Mac reports a balance of zero on the final record of
  # a terminated loan. That zero records the exit, not the equity position, so
  # the balance of the previous month is used. A loan whose only record in the
  # window is the final one has no previous balance; its LTV becomes missing
  # and the loan drops out when the estimation frame is built.
  cat("final records with a zero balance:", sum(panel$upb_current == 0, na.rm = TRUE),
      "| of which without a previous record:",
      sum(panel$upb_current == 0 & is.na(panel$upb_prev), na.rm = TRUE), "\n")

  panel$upb_fixed   <- ifelse(panel$upb_current == 0, panel$upb_prev, panel$upb_current)
  panel$ltv_current <- (panel$upb_fixed / (panel$value_orig * panel$hpi_current / panel$hpi_orig)) * 100

  # Treatment 2: loans that are 120 or more days delinquent or in REO in their
  # first observed month were in default before the window opened. Loans at
  # exactly ninety days in their first month are kept.
  first_rows <- panel %>% filter(row_nr == 1)
  first_rows$dq_first <- suppressWarnings(as.numeric(first_rows$delinquency_status))
  entry_default_ids <- first_rows$loan_id[first_rows$default == 1 &
                                            (is.na(first_rows$dq_first) | first_rows$dq_first >= 4)]
  cat("loans excluded (in default before the window):", length(entry_default_ids), "\n")
  panel <- panel %>% filter(!(loan_id %in% entry_default_ids))

  saveRDS(panel, panel_final_file)
  cat("saved:", panel_final_file, "|", nrow(panel), "loan-months\n")

  rm(panel, first_rows, entry_default_ids); gc()
}

cat("\nALL DONE", format(Sys.time()), "\n")
