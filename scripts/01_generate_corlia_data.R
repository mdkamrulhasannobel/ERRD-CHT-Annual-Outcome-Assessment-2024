# ============================================================
# Script: 01_generate_corlia_data.R
# Project: ERRD-CHT Longitudinal Outcome Monitoring and Assessment
# Component: CoRLIA — Community Resilience through Locally-Led
#             Inclusive Adaptation
# Author: MD. Kamrul Hasan | DM WATCH LIMITED
# Client: UNDP Bangladesh / Global Affairs Canada (GAC)
# Data Period: September – November 2024
# Target N: 620 respondents
# ============================================================

set.seed(2024)
library(dplyr)
library(lubridate)

# ---- Geography -----------------------------------------------
districts   <- c("Rangamati", "Bandarban", "Khagrachari")
upazilas <- list(
  Rangamati   = c("Kaptai", "Rajasthali", "Baghaichhari", "Naniarchar", "Barkal"),
  Bandarban   = c("Bandarban Sadar", "Thanchi", "Ruma", "Rowangchhari", "Alikadam"),
  Khagrachari = c("Khagrachari Sadar", "Dighinala", "Panchhari", "Lakshmichhari", "Mahalchhari")
)
ethnic_groups <- c("Chakma", "Marma", "Tripura", "Tanchangya", "Murung",
                   "Khumi", "Bawm", "Bengali", "Pangkhua", "Khyang")

n <- 620

district_draw <- sample(districts, n, replace = TRUE, prob = c(0.38, 0.30, 0.32))

upazila_draw <- mapply(function(d) {
  sample(upazilas[[d]], 1)
}, district_draw)

# ---- Respondent groups ---------------------------------------
respondent_type <- sample(
  c("Village Women Group (VWG)", "Women's Climate Resilience Committee (WCRC)",
    "Youth Group", "Community Stakeholder", "Local Government Representative"),
  n, replace = TRUE,
  prob = c(0.35, 0.25, 0.20, 0.12, 0.08)
)

# ---- Socio-demographics --------------------------------------
age <- case_when(
  respondent_type == "Youth Group" ~ round(rnorm(n, 22, 3)),
  TRUE ~ round(rnorm(n, 38, 9))
)
age <- pmax(18, pmin(age, 65))

sex <- ifelse(
  respondent_type %in% c("Village Women Group (VWG)", "Women's Climate Resilience Committee (WCRC)"),
  "Female",
  sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.62, 0.38))
)

ethnicity <- sample(ethnic_groups, n, replace = TRUE,
                    prob = c(0.28, 0.22, 0.15, 0.08, 0.05, 0.04, 0.04, 0.08, 0.03, 0.03))

education_level <- sample(
  c("No formal education", "Primary (Class 1-5)", "Secondary (Class 6-10)",
    "SSC/Equivalent", "HSC/Equivalent", "Graduate and above"),
  n, replace = TRUE,
  prob = c(0.18, 0.28, 0.24, 0.14, 0.10, 0.06)
)

household_size <- round(rnorm(n, 4.8, 1.2))
household_size <- pmax(2, pmin(household_size, 10))

monthly_income_bdt <- round(rnorm(n, 11500, 3800))
monthly_income_bdt <- pmax(3000, pmin(monthly_income_bdt, 35000))

land_ownership <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.54, 0.46))

# ---- Climate vulnerability indicators ------------------------
climate_shocks_experienced <- sample(
  c("Landslide", "Flash Flood", "Drought", "Erratic Rainfall", "None", "Multiple"),
  n, replace = TRUE,
  prob = c(0.22, 0.20, 0.12, 0.18, 0.08, 0.20)
)

crop_loss_last3yrs <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.68, 0.32))

livelihood_disrupted <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.61, 0.39))

# ---- Programme participation & outcomes ----------------------
years_in_programme <- sample(1:4, n, replace = TRUE, prob = c(0.15, 0.30, 0.35, 0.20))

attended_climate_training <- sample(c("Yes", "No"), n, replace = TRUE,
                                     prob = c(0.78, 0.22))

# Climate adaptation practices adopted (scale 0-10)
adaptation_practices_score_baseline <- round(rnorm(n, 3.2, 1.5))
adaptation_practices_score_baseline <- pmax(0, pmin(adaptation_practices_score_baseline, 10))

adaptation_practices_score_endline <- adaptation_practices_score_baseline +
  round(rnorm(n, 2.8, 1.1) * (years_in_programme / 4))
adaptation_practices_score_endline <- pmax(0, pmin(adaptation_practices_score_endline, 10))

# Household resilience index (composite, 0-100)
resilience_index_baseline <- round(rnorm(n, 38.4, 12.5))
resilience_index_baseline <- pmax(10, pmin(resilience_index_baseline, 100))

resilience_index_endline <- resilience_index_baseline +
  round(rnorm(n, 14.2, 6.8) * (years_in_programme / 4))
resilience_index_endline <- pmax(10, pmin(resilience_index_endline, 100))

# Access to early warning systems
access_early_warning_baseline <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.22, 0.78))
access_early_warning_endline  <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.64, 0.36))

# Women in decision-making
women_decision_making_baseline <- sample(
  c("Never", "Rarely", "Sometimes", "Often", "Always"),
  n, replace = TRUE, prob = c(0.28, 0.30, 0.22, 0.14, 0.06)
)
women_decision_making_endline <- sample(
  c("Never", "Rarely", "Sometimes", "Often", "Always"),
  n, replace = TRUE, prob = c(0.08, 0.14, 0.28, 0.32, 0.18)
)

# Livelihood diversification (number of income sources)
livelihood_sources_baseline <- sample(1:5, n, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.10, 0.05))
livelihood_sources_endline  <- sample(1:6, n, replace = TRUE, prob = c(0.12, 0.25, 0.30, 0.20, 0.08, 0.05))

# WCRC functionality score (0-5)
wcrc_functionality <- ifelse(
  respondent_type == "Women's Climate Resilience Committee (WCRC)",
  sample(1:5, sum(respondent_type == "Women's Climate Resilience Committee (WCRC)"),
         replace = TRUE, prob = c(0.05, 0.15, 0.30, 0.35, 0.15)),
  NA
)

# Satisfaction with programme services
satisfaction_score <- sample(1:5, n, replace = TRUE,
                              prob = c(0.04, 0.08, 0.22, 0.42, 0.24))

# Savings (BDT/month)
savings_monthly_bdt <- ifelse(
  attendd_saving <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.65, 0.35)),
  round(rnorm(n, 1800, 650)), 0
)
savings_monthly_bdt <- pmax(0, savings_monthly_bdt)

# Food security status
food_security <- sample(
  c("Food Secure", "Mildly Food Insecure", "Moderately Food Insecure", "Severely Food Insecure"),
  n, replace = TRUE, prob = c(0.42, 0.28, 0.20, 0.10)
)

# Knowledge of climate adaptation (score 0-20)
climate_knowledge_score <- round(rnorm(n, 13.6, 3.8))
climate_knowledge_score <- pmax(0, pmin(climate_knowledge_score, 20))

# Interview date
interview_date <- sample(
  seq(as.Date("2024-09-01"), as.Date("2024-11-30"), by = "day"),
  n, replace = TRUE
)

# ---- Assemble dataset ----------------------------------------
corlia_df <- data.frame(
  respondent_id              = paste0("COR-", sprintf("%04d", 1:n)),
  interview_date             = interview_date,
  district                   = district_draw,
  upazila                    = upazila_draw,
  respondent_type            = respondent_type,
  age                        = age,
  sex                        = sex,
  ethnicity                  = ethnicity,
  education_level            = education_level,
  household_size             = household_size,
  monthly_income_bdt         = monthly_income_bdt,
  land_ownership             = land_ownership,
  climate_shocks_experienced = climate_shocks_experienced,
  crop_loss_last3yrs         = crop_loss_last3yrs,
  livelihood_disrupted       = livelihood_disrupted,
  years_in_programme         = years_in_programme,
  attended_climate_training  = attended_climate_training,
  adaptation_practices_score_baseline = adaptation_practices_score_baseline,
  adaptation_practices_score_endline  = adaptation_practices_score_endline,
  resilience_index_baseline  = resilience_index_baseline,
  resilience_index_endline   = resilience_index_endline,
  access_early_warning_baseline = access_early_warning_baseline,
  access_early_warning_endline  = access_early_warning_endline,
  women_decision_making_baseline = women_decision_making_baseline,
  women_decision_making_endline  = women_decision_making_endline,
  livelihood_sources_baseline = livelihood_sources_baseline,
  livelihood_sources_endline  = livelihood_sources_endline,
  wcrc_functionality         = wcrc_functionality,
  satisfaction_score         = satisfaction_score,
  savings_monthly_bdt        = savings_monthly_bdt,
  food_security              = food_security,
  climate_knowledge_score    = climate_knowledge_score,
  stringsAsFactors            = FALSE
)

# Save
write.csv(corlia_df,
          "/Users/md.kamrulhasan/All my project/R project/data/raw/corlia_survey.csv",
          row.names = FALSE)

cat("✅ CoRLIA dataset generated:", nrow(corlia_df), "rows x", ncol(corlia_df), "columns\n")
