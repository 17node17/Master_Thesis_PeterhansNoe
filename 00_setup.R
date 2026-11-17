# ============================================================
# MASTER THESIS - Static Credit Scores and Dynamic Risk Drivers
# Noe Peterhans, University of St. Gallen (HSG)
#
# 00  SETUP - packages, paths and file names
#
# Every other script starts with  source("00_setup.R").
# Set the working directory to the folder that holds these scripts first.
# The only line to change on another computer is BASE_PATH.
# ============================================================

# data.table is loaded before dplyr on purpose, so that the dplyr versions
# of between(), first() and last() are the ones in use.
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(pROC)
library(xgboost)

BASE_PATH <- "path/to/data"   # set this to the folder that holds the data
CACHE_DIR <- file.path(BASE_PATH, "cache")       # per-vintage raw extracts
OUT_PATH  <- file.path(BASE_PATH, "final_run")   # every result of the thesis
FIG_PATH  <- file.path(OUT_PATH, "figures")      # every figure of the thesis

dir.create(CACHE_DIR, showWarnings = FALSE)
dir.create(OUT_PATH,  showWarnings = FALSE)
dir.create(FIG_PATH,  showWarnings = FALSE)

# The two panels of the main specification (written by 01_build_panels.R)
panel_files <- c(crisis = "panel_crisis_08_11_vintages_05-06-07_final.rds",
                 calm   = "panel_calm_16_19_vintages_13-14-15_final.rds")

# The two panels of the alternative vintage check (5.6)
alt_panel_files <- c(
  "crisis 2008-2011 | v 1999-2007 (6)" = "panel_crisis_08_11_vintages_99-01-03-05-06-07_final.rds",
  "calm 2016-2019 | v 2014-2016 (3)"   = "panel_calm_16_19_vintages_14-15-16_final.rds"
)

# Labels used in the figures
lev_reg <- c("crisis 2008-2011", "calm 2016-2019")
