# ============================================================
# Script: 00_master_run.R
# Project: ERRD-CHT Longitudinal Outcome Monitoring and Assessment
# Purpose: Master execution script — runs all data preparation
#          scripts in sequence, then renders the Quarto report
# Author:  MD. Kamrul Hasan | DM WATCH LIMITED
# Client:  UNDP Bangladesh / Global Affairs Canada (GAC)
# ============================================================

cat("============================================================\n")
cat(" ERRD-CHT Annual Outcome Assessment — Master Run Script\n")
cat(" Author: MD. Kamrul Hasan | DM WATCH LIMITED\n")
cat(" Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n\n")

# Set working directory to project root
proj_root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(proj_root)

# ---- Step 1: Data preparation --------------------------------
cat("▶ Step 1/6 — Generating CoRLIA survey data...\n")
source("scripts/01_generate_corlia_data.R")

cat("▶ Step 2/6 — Generating BERCR survey data...\n")
source("scripts/02_generate_bercr_data.R")

cat("▶ Step 3/6 — Generating WGEIE survey data...\n")
source("scripts/03_generate_wgeie_data.R")

# ---- Step 2: Verify datasets ---------------------------------
cat("\n▶ Step 4/6 — Verifying datasets...\n")

corlia <- read.csv("data/raw/corlia_survey.csv")
bercr  <- read.csv("data/raw/bercr_survey.csv")
wgeie  <- read.csv("data/raw/wgeie_survey.csv")

total_n <- nrow(corlia) + nrow(bercr) + nrow(wgeie)

cat("  ✅ CoRLIA: ", nrow(corlia), "respondents ×", ncol(corlia), "variables\n")
cat("  ✅ BERCR:  ", nrow(bercr),  "respondents ×", ncol(bercr),  "variables\n")
cat("  ✅ WGEIE:  ", nrow(wgeie),  "respondents ×", ncol(wgeie),  "variables\n")
cat("  ─────────────────────────────────────────────────\n")
cat("  ✅ TOTAL:  ", total_n, "respondents across 3 components\n\n")

# ---- Step 3: Export figures ----------------------------------
cat("▶ Step 5/6 — Exporting all figures to output/figures/...\n")
source("scripts/04_export_figures.R")

# ---- Step 4: Render report -----------------------------------
cat("\n▶ Step 6/6 — Rendering Quarto report...\n")
quarto::quarto_render(
  input  = "report/ERRD_CHT_Outcome_Assessment_Report.qmd",
  output_format = "html"
)

cat("\n============================================================\n")
cat(" ✅ All done! Report available at:\n")
cat("    report/ERRD_CHT_Outcome_Assessment_Report.html\n")
cat(" Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")
