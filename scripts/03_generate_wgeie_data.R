# ============================================================
# Script: 03_generate_wgeie_data.R
# Project: ERRD-CHT Longitudinal Outcome Monitoring and Assessment
# Component: WGEIE — Women & Girls Empowerment through
#             Inclusive Education
# Author: MD. Kamrul Hasan | DM WATCH LIMITED
# Client: UNDP Bangladesh / Global Affairs Canada (GAC)
# Data Period: September – December 2024
# Target N: 1,104 respondents
# ============================================================

set.seed(2026)
library(dplyr)
library(lubridate)

districts <- c("Rangamati", "Bandarban", "Khagrachari")
upazilas <- list(
  Rangamati   = c("Kaptai", "Rajasthali", "Baghaichhari", "Naniarchar", "Longadu"),
  Bandarban   = c("Bandarban Sadar", "Lama", "Ruma", "Rowangchhari", "Naikhongchhari"),
  Khagrachari = c("Khagrachari Sadar", "Dighinala", "Panchhari", "Mahalchhari", "Ramgarh")
)
ethnic_groups <- c("Chakma", "Marma", "Tripura", "Tanchangya", "Murung",
                   "Khumi", "Bawm", "Bengali", "Chak", "Pangkhua")

n <- 1104

district_draw <- sample(districts, n, replace = TRUE, prob = c(0.36, 0.30, 0.34))
upazila_draw  <- sapply(district_draw, function(d) sample(upazilas[[d]], 1))

# ---- Respondent types (WGEIE has most diverse groups) --------
respondent_type <- sample(
  c("School Student (Girl)", "School Student (Boy)", "Household (Parent/Guardian)",
    "Teacher", "School Management Committee (SMC)",
    "Reproductive Health Community Worker (RHCW)", "Mother Club Member",
    "Police/Law Enforcement", "Community Stakeholder"),
  n, replace = TRUE,
  prob = c(0.28, 0.14, 0.22, 0.10, 0.06, 0.08, 0.06, 0.03, 0.03)
)

# ---- Demographics (vary by respondent type) ------------------
age <- case_when(
  respondent_type %in% c("School Student (Girl)", "School Student (Boy)") ~ round(rnorm(n, 13.5, 2.2)),
  respondent_type == "Teacher"                        ~ round(rnorm(n, 36, 8)),
  respondent_type == "Household (Parent/Guardian)"    ~ round(rnorm(n, 40, 9)),
  respondent_type == "Mother Club Member"             ~ round(rnorm(n, 34, 6)),
  respondent_type == "Reproductive Health Community Worker (RHCW)" ~ round(rnorm(n, 30, 5)),
  TRUE ~ round(rnorm(n, 42, 10))
)
age <- pmax(10, pmin(age, 65))

sex <- case_when(
  respondent_type == "School Student (Girl)"  ~ "Female",
  respondent_type == "School Student (Boy)"   ~ "Male",
  respondent_type == "Mother Club Member"     ~ "Female",
  respondent_type == "Reproductive Health Community Worker (RHCW)" ~ "Female",
  TRUE ~ sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.54, 0.46))
)

ethnicity <- sample(ethnic_groups, n, replace = TRUE,
                    prob = c(0.27, 0.21, 0.15, 0.08, 0.05, 0.04, 0.04, 0.10, 0.03, 0.03))

# ---- School-level indicators ---------------------------------
school_type <- sample(
  c("Government Primary School", "Government Secondary School",
    "NGO-run School", "Madrasa", "Community Learning Centre"),
  n, replace = TRUE, prob = c(0.34, 0.30, 0.18, 0.10, 0.08)
)

school_has_safe_water <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.64, 0.36))
school_has_separate_toilet <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.58, 0.42))
school_has_menstrual_hygiene_facility <- sample(c("Yes", "No"), n, replace = TRUE,
                                                  prob = c(0.44, 0.56))
school_gender_responsive_policy <- sample(c("Yes", "No", "Partially"),
                                           n, replace = TRUE, prob = c(0.42, 0.32, 0.26))

# ---- Enrolment & attendance ----------------------------------
enrollment_rate_baseline_pct <- round(rnorm(n, 72.4, 11.8), 1)
enrollment_rate_baseline_pct <- pmax(40, pmin(enrollment_rate_baseline_pct, 100))

enrollment_rate_endline_pct  <- enrollment_rate_baseline_pct +
  round(rnorm(n, 9.6, 4.2), 1)
enrollment_rate_endline_pct  <- pmax(40, pmin(enrollment_rate_endline_pct, 100))

# Girls attendance rate (days/month out of 22)
girls_attendance_days_baseline <- round(rnorm(n, 15.8, 3.2))
girls_attendance_days_baseline <- pmax(5, pmin(girls_attendance_days_baseline, 22))

girls_attendance_days_endline  <- girls_attendance_days_baseline +
  round(rnorm(n, 2.4, 1.2))
girls_attendance_days_endline  <- pmax(5, pmin(girls_attendance_days_endline, 22))

# Dropout rate (past academic year %)
dropout_rate_girls_baseline_pct <- round(rnorm(n, 18.6, 7.4), 1)
dropout_rate_girls_baseline_pct <- pmax(0, pmin(dropout_rate_girls_baseline_pct, 50))

dropout_rate_girls_endline_pct  <- dropout_rate_girls_baseline_pct -
  round(rnorm(n, 6.2, 2.8), 1)
dropout_rate_girls_endline_pct  <- pmax(0, pmin(dropout_rate_girls_endline_pct, 50))

# ---- Learning outcomes (only for student respondents) --------
years_in_programme <- sample(1:4, n, replace = TRUE, prob = c(0.16, 0.30, 0.34, 0.20))

learning_score_baseline <- round(rnorm(n, 48.2, 14.6))  # out of 100
learning_score_baseline <- pmax(15, pmin(learning_score_baseline, 100))

learning_score_endline   <- learning_score_baseline +
  round(rnorm(n, 12.4, 5.8) * (years_in_programme / 4))
learning_score_endline   <- pmax(15, pmin(learning_score_endline, 100))

# ---- Gender empowerment indicators ---------------------------
# Agency / decision-making score (0-10)
agency_score_baseline <- round(rnorm(n, 3.8, 1.6), 1)
agency_score_baseline <- pmax(0, pmin(agency_score_baseline, 10))

agency_score_endline  <- agency_score_baseline +
  round(rnorm(n, 2.2, 1.0) * (years_in_programme / 4), 1)
agency_score_endline  <- pmax(0, pmin(agency_score_endline, 10))

# Child marriage prevalence
child_marriage_prevalence_baseline_pct <- round(rnorm(n, 42.8, 14.2), 1)
child_marriage_prevalence_baseline_pct <- pmax(10, pmin(child_marriage_prevalence_baseline_pct, 85))

child_marriage_prevalence_endline_pct  <- child_marriage_prevalence_baseline_pct -
  round(rnorm(n, 8.4, 3.8), 1)
child_marriage_prevalence_endline_pct  <- pmax(0, pmin(child_marriage_prevalence_endline_pct, 85))

# GBV awareness (Likert 1-5)
gbv_awareness_baseline <- sample(1:5, n, replace = TRUE,
                                  prob = c(0.18, 0.26, 0.28, 0.18, 0.10))
gbv_awareness_endline  <- sample(1:5, n, replace = TRUE,
                                  prob = c(0.04, 0.10, 0.24, 0.36, 0.26))

# Menstrual health management (MHM) knowledge
mhm_knowledge_baseline <- sample(c("Poor", "Moderate", "Good", "Excellent"),
                                   n, replace = TRUE, prob = c(0.26, 0.34, 0.28, 0.12))
mhm_knowledge_endline  <- sample(c("Poor", "Moderate", "Good", "Excellent"),
                                   n, replace = TRUE, prob = c(0.06, 0.18, 0.42, 0.34))

# Participation in school clubs/committees
participates_in_school_activities <- sample(c("Yes", "No"), n, replace = TRUE,
                                             prob = c(0.61, 0.39))

# ---- Teacher capacity indicators (for teacher respondents) ---
teacher_trained_gender_responsive <- sample(c("Yes", "No"), n, replace = TRUE,
                                             prob = c(0.68, 0.32))
teacher_uses_inclusive_methods    <- sample(c("Yes", "Partially", "No"),
                                             n, replace = TRUE, prob = c(0.48, 0.34, 0.18))

# ---- Household-level (for household respondents) -------------
parent_education_support <- sample(c("Strongly supports", "Supports",
                                      "Neutral", "Does not support"),
                                    n, replace = TRUE, prob = c(0.28, 0.40, 0.20, 0.12))

household_distance_to_school_km <- round(rnorm(n, 3.2, 1.8), 1)
household_distance_to_school_km <- pmax(0.2, pmin(household_distance_to_school_km, 12))

economic_barrier_to_schooling <- sample(c("Yes - major barrier",
                                           "Yes - minor barrier",
                                           "No barrier"),
                                         n, replace = TRUE, prob = c(0.28, 0.38, 0.34))

# ---- RHCW indicators -----------------------------------------
rhcw_households_served <- round(rnorm(n, 48, 18))
rhcw_households_served <- pmax(10, pmin(rhcw_households_served, 120))

rhcw_mhm_sessions_conducted <- round(rnorm(n, 8.4, 3.2))
rhcw_mhm_sessions_conducted <- pmax(1, pmin(rhcw_mhm_sessions_conducted, 24))

# ---- Overall satisfaction ------------------------------------
satisfaction_score <- sample(1:5, n, replace = TRUE,
                              prob = c(0.03, 0.07, 0.20, 0.44, 0.26))

# Interview date
interview_date <- sample(
  seq(as.Date("2024-09-03"), as.Date("2024-12-05"), by = "day"),
  n, replace = TRUE
)

# ---- Assemble dataset ----------------------------------------
wgeie_df <- data.frame(
  respondent_id                          = paste0("WGE-", sprintf("%04d", 1:n)),
  interview_date                         = interview_date,
  district                               = district_draw,
  upazila                                = upazila_draw,
  respondent_type                        = respondent_type,
  age                                    = age,
  sex                                    = sex,
  ethnicity                              = ethnicity,
  school_type                            = school_type,
  school_has_safe_water                  = school_has_safe_water,
  school_has_separate_toilet             = school_has_separate_toilet,
  school_has_menstrual_hygiene_facility  = school_has_menstrual_hygiene_facility,
  school_gender_responsive_policy        = school_gender_responsive_policy,
  years_in_programme                     = years_in_programme,
  enrollment_rate_baseline_pct           = enrollment_rate_baseline_pct,
  enrollment_rate_endline_pct            = enrollment_rate_endline_pct,
  girls_attendance_days_baseline         = girls_attendance_days_baseline,
  girls_attendance_days_endline          = girls_attendance_days_endline,
  dropout_rate_girls_baseline_pct        = dropout_rate_girls_baseline_pct,
  dropout_rate_girls_endline_pct         = dropout_rate_girls_endline_pct,
  learning_score_baseline                = learning_score_baseline,
  learning_score_endline                 = learning_score_endline,
  agency_score_baseline                  = agency_score_baseline,
  agency_score_endline                   = agency_score_endline,
  child_marriage_prevalence_baseline_pct = child_marriage_prevalence_baseline_pct,
  child_marriage_prevalence_endline_pct  = child_marriage_prevalence_endline_pct,
  gbv_awareness_baseline                 = gbv_awareness_baseline,
  gbv_awareness_endline                  = gbv_awareness_endline,
  mhm_knowledge_baseline                 = mhm_knowledge_baseline,
  mhm_knowledge_endline                  = mhm_knowledge_endline,
  participates_in_school_activities      = participates_in_school_activities,
  teacher_trained_gender_responsive      = teacher_trained_gender_responsive,
  teacher_uses_inclusive_methods         = teacher_uses_inclusive_methods,
  parent_education_support               = parent_education_support,
  household_distance_to_school_km        = household_distance_to_school_km,
  economic_barrier_to_schooling          = economic_barrier_to_schooling,
  rhcw_households_served                 = rhcw_households_served,
  rhcw_mhm_sessions_conducted            = rhcw_mhm_sessions_conducted,
  satisfaction_score                     = satisfaction_score,
  stringsAsFactors                        = FALSE
)

write.csv(wgeie_df,
          "/Users/md.kamrulhasan/All my project/R project/data/raw/wgeie_survey.csv",
          row.names = FALSE)

cat("✅ WGEIE dataset generated:", nrow(wgeie_df), "rows x", ncol(wgeie_df), "columns\n")
