# ============================================================
# RUN ALL - runs every script of the repository in order and compares the
# new results with the results that are already on disk.
#
# Start it with one line, from the folder that holds the scripts:
#     source("run_all.R")
#
# What it does
#   1  renames the existing results folder  final_run  to  final_run_reference
#      (nothing is deleted; if final_run_reference already exists it stops)
#   2  runs 01 to 07, each in a fresh R session, and writes one log per
#      script into the folder  run_logs
#   3  compares every result file of the new run with the reference and
#      prints one line per file
#
# Runs for many hours. The computer must not go to sleep.
# ============================================================

source("00_setup.R")

REF_PATH <- file.path(BASE_PATH, "final_run_reference")
LOG_PATH <- file.path(getwd(), "run_logs")

scripts <- c("01_build_panels.R",
             "02_main_run.R",
             "03_descriptives_diagnostics.R",
             "04_robustness.R",
             "05_figures.R",
             "06_forbearance_check.R",
             "07_pooled_regime_test.R")


# ---- 1. Keep the existing results as the reference ----
# 00_setup.R has just created final_run if it did not exist, so an empty
# folder means there is nothing to compare with.

if (dir.exists(REF_PATH)) {
  stop("final_run_reference already exists. Rename or remove it by hand first.")
}

has_reference <- length(list.files(OUT_PATH)) > 0
if (has_reference) {
  ok <- file.rename(OUT_PATH, REF_PATH)
  if (!ok) stop("could not rename final_run - close files that are open in it")
  dir.create(OUT_PATH)
  cat("existing results moved to:", REF_PATH, "\n")
} else {
  cat("no existing results found, running without a comparison\n")
}
dir.create(FIG_PATH, showWarnings = FALSE)
dir.create(LOG_PATH, showWarnings = FALSE)


# ---- 2. Run the scripts, one fresh R session each ----

rscript <- file.path(R.home("bin"), "Rscript")
status  <- data.frame()

for (s in scripts) {
  cat("\nstarting", s, format(Sys.time()), "\n")
  t0  <- Sys.time()
  log <- file.path(LOG_PATH, sub("\\.R$", ".log", s))
  code <- system2(rscript, shQuote(s), stdout = log, stderr = log)
  status <- rbind(status, data.frame(
    script  = s,
    result  = ifelse(code == 0, "finished", "FAILED - see log"),
    minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)))
  cat("  ", ifelse(code == 0, "finished", "FAILED - see log"), "\n")
}

cat("\n\n===== SCRIPTS =====\n")
print(status, row.names = FALSE)


# ---- 3. Compare the new results with the reference ----
# Every .rds result file except the fitted models, which are too large and
# contain nothing that is not in the result tables.

if (has_reference) {

  files <- list.files(OUT_PATH, pattern = "\\.rds$")
  files <- files[!grepl("^models_", files)]

  comparison <- data.frame()

  for (f in files) {

    if (!file.exists(file.path(REF_PATH, f))) {
      comparison <- rbind(comparison, data.frame(file = f, same = NA, max_abs_diff = NA,
                                                 note = "not in the reference"))
      next
    }

    new <- readRDS(file.path(OUT_PATH, f))
    ref <- readRDS(file.path(REF_PATH, f))

    if (!is.data.frame(new) | !is.data.frame(ref)) {
      comparison <- rbind(comparison, data.frame(file = f, same = identical(new, ref),
                                                 max_abs_diff = NA, note = "not a table"))
      next
    }

    if (nrow(new) != nrow(ref) | !identical(names(new), names(ref))) {
      comparison <- rbind(comparison, data.frame(file = f, same = FALSE, max_abs_diff = NA,
                                                 note = "different rows or columns"))
      next
    }

    # largest absolute difference over all numeric columns
    max_diff <- 0
    for (v in names(new)) {
      if (is.numeric(new[[v]]) & is.numeric(ref[[v]])) {
        d <- suppressWarnings(max(abs(new[[v]] - ref[[v]]), na.rm = TRUE))
        if (is.finite(d) & d > max_diff) max_diff <- d
      } else {
        if (!identical(as.character(new[[v]]), as.character(ref[[v]]))) max_diff <- Inf
      }
    }

    comparison <- rbind(comparison, data.frame(
      file = f, same = max_diff < 1e-8, max_abs_diff = max_diff,
      note = ifelse(max_diff < 1e-8, "", "check")))

    rm(new, ref); gc()
  }

  cat("\n\n===== NEW RESULTS AGAINST THE REFERENCE =====\n")
  print(comparison, row.names = FALSE, digits = 3)

  missing <- setdiff(list.files(REF_PATH, pattern = "\\.rds$"), list.files(OUT_PATH, pattern = "\\.rds$"))
  if (length(missing) > 0) cat("\nin the reference but not produced now:", paste(missing, collapse = ", "), "\n")

  write.csv(comparison, file.path(LOG_PATH, "comparison.csv"), row.names = FALSE)
  cat("\ncomparison saved to:", file.path(LOG_PATH, "comparison.csv"), "\n")
}

cat("\nALL DONE", format(Sys.time()), "\n")
