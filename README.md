# Code for the master's thesis

*Static Credit Scores and Dynamic Risk Drivers: The Incremental Predictive Value of FICO for Mortgage Default Across Economic Regimes*

Noé Peterhans, University of St. Gallen (HSG)

## How to run

1. Put all scripts in one folder and set the R working directory to it.
2. Change `BASE_PATH` in `00_setup.R` if the data are somewhere else. Nothing else has to be changed.
3. Run the scripts in the order of their numbers. Each one starts with `source("00_setup.R")`.

| Script | What it does | Thesis | Time |
|---|---|---|---|
| `00_setup.R` | Packages, paths, file names | | |
| `01_build_panels.R` | Raw Freddie Mac files to loan-month panels, incl. the two data treatments | 3.1 to 3.4 | hours from raw files, minutes if the panels exist |
| `02_main_run.R` | All models: main split, cross-validation, common support, boosting, depth ladder, SHAP | Tables 2 to 5, 5.5, B.3, B.4 | several hours |
| `03_descriptives_diagnostics.R` | Table 1 and all data diagnostics | Table 1, 3.2, 3.4, A.2, A.3, B.1, B.2 | minutes |
| `04_robustness.R` | Basis check with k = 20, lag check, alternative vintages | 5.6, footnote 6, B.5 | one to two hours |
| `05_figures.R` | Figures 1 to 5 and B1 | | minutes |
| `06_forbearance_check.R` | Forbearance flag in the raw files, 2020 against earlier years | 3.3, A.1 | minutes |
| `07_pooled_regime_test.R` | Pooled model with a credit score by regime interaction | 5.3 (mentioned, not used as evidence) | about an hour |

To run everything in one go and compare the new results with the ones already on disk, use `source("run_all.R")`. It moves the existing `final_run` folder to `final_run_reference`, runs scripts 01 to 07 in fresh R sessions, writes one log per script to `run_logs/`, and prints a file-by-file comparison.

## Where the output goes

- Panels: `BASE_PATH/panel_<tag>_clean.rds` (before the two data treatments) and `panel_<tag>_final.rds` (used everywhere).
- Everything else: `BASE_PATH/final_run/`. Results as `.rds`, printed output as `checks_*.txt`, figures in `final_run/figures/`.

## Input files expected in `BASE_PATH`

- `sample_<year>/sample_orig_<year>.txt` and `sample_svcg_<year>.txt` (Freddie Mac Standard sample)
- `non_std_historical_data/historical_data_excl_<year>/...` (Freddie Mac Non-Standard files)
- `HPI/hpi_at_metro.xlsx` and `HPI/hpi_at_state.xlsx` (FHFA)
- `chart_20261107T080541.csv` (effective federal funds rate, for Figure 1)
- `cache/unemp_state.rds` (state unemployment). If it is missing, `01_build_panels.R` downloads it from FRED and needs a personal API key, set once with `Sys.setenv(FRED_API_KEY = "...")`. The key is not written into any script.

## Things to know

- **Seeds.** Loan-level 70/30 split and five folds: seed 123. Early-stopping split of boosting: seed 456 on the main split and 1000 + fold in the cross-validation. SHAP subsample: seed 789.
- **Non-Standard sample.** It is drawn once per vintage and then read from `cache/`. Keep that folder: it is the record of which loans are in the sample.
- **Existing files are not rebuilt.** `01_build_panels.R` skips a specification whose `_final.rds` panel is already on disk.
- **Figure 2** is an illustration (baseline-only fit on 40'000 crisis loans) and is not part of the estimation.
- **Table by year in `06`** (forbearance, repayment plan, trial modification) only prints if the 2016 to 2020 panel contains `borrower_assistance_status`. The panel on disk does not, so that table is skipped.
