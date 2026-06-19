# ============================================================
# Script: 02_generate_bercr_data.R
# Project: ERRD-CHT Longitudinal Outcome Monitoring and Assessment
# Component: BERCR — Biodiversity Ecosystems Restoration for
#             Community Resilience
# Author: MD. Kamrul Hasan | DM WATCH LIMITED
# Client: UNDP Bangladesh / Global Affairs Canada (GAC)
# Data Period: September – November 2024
# Target N: 495 respondents
# ============================================================

set.seed(2025)
library(dplyr)
library(lubridate)

# ---- Geography -----------------------------------------------
districts <- c("Rangamati", "Bandarban", "Khagrachari")
upazilas <- list(
  Rangamati   = c("Kaptai", "Rajasthali", "Baghaichhari", "Naniarchar", "Barkal", "Belaichhari"),
  Bandarban   = c("Bandarban Sadar", "Thanchi", "Ruma", "Rowangchhari", "Naikhongchhari"),
  Khagrachari = c("Khagrachari Sadar", "Dighinala", "Panchhari", "Guimara", "Ramgarh")
)
ethnic_groups <- c("Chakma", "Marma", "Tripura", "Tanchangya", "Murung",
                   "Khumi", "Bawm", "Bengali", "Lushai", "Chak")

n <- 495

district_draw <- sample(districts, n, replace = TRUE, prob = c(0.40, 0.32, 0.28))

upazila_draw <- sapply(district_draw, function(d) sample(upazilas[[d]], 1))

# ---- Respondent types ----------------------------------------
respondent_type <- sample(
  c("Biodiversity Conservation Group (BCG)", "Community Forest Management Group",
    "Youth Eco-Entrepreneur", "Community Leader/Headman",
    "Forest Department Staff", "Local NGO Representative"),
  n, replace = TRUE,
  prob = c(0.30, 0.25, 0.18, 0.12, 0.08, 0.07)
)

# ---- Demographics --------------------------------------------
age <- round(rnorm(n, 40, 10))
age <- pmax(18, pmin(age, 68))

sex <- sample(c("Male", "Female"), n, replace = TRUE,
              prob = c(0.55, 0.45))

ethnicity <- sample(ethnic_groups, n, replace = TRUE,
                    prob = c(0.26, 0.20, 0.16, 0.09, 0.06, 0.05, 0.05, 0.07, 0.03, 0.03))

education_level <- sample(
  c("No formal education", "Primary (Class 1-5)", "Secondary (Class 6-10)",
    "SSC/Equivalent", "HSC/Equivalent", "Graduate and above"),
  n, replace = TRUE,
  prob = c(0.15, 0.26, 0.25, 0.16, 0.11, 0.07)
)

household_size <- round(rnorm(n, 5.1, 1.3))
household_size <- pmax(2, pmin(household_size, 11))

monthly_income_bdt <- round(rnorm(n, 12800, 4200))
monthly_income_bdt <- pmax(3500, pmin(monthly_income_bdt, 40000))

# ---- Forest & biodiversity indicators ------------------------
# Forest land managed (acres)
forest_land_managed_acres <- round(rnorm(n, 8.6, 4.2), 1)
forest_land_managed_acres <- pmax(0.5, pmin(forest_land_managed_acres, 40))

# Number of tree species planted (cumulative)
tree_species_planted <- round(rnorm(n, 6.4, 2.8))
tree_species_planted <- pmax(1, pmin(tree_species_planted, 18))

# Seedlings distributed (per respondent group/cycle)
seedlings_distributed <- round(rnorm(n, 320, 140))
seedlings_distributed <- pmax(50, pmin(seedlings_distributed, 1200))

# Survival rate of planted seedlings (%)
seedling_survival_rate_pct <- round(rnorm(n, 71.4, 12.8), 1)
seedling_survival_rate_pct <- pmax(30, pmin(seedling_survival_rate_pct, 98))

# Biodiversity index (species richness composite, 0-100)
biodiversity_index_baseline <- round(rnorm(n, 42.1, 13.5))
biodiversity_index_baseline <- pmax(10, pmin(biodiversity_index_baseline, 100))

years_in_programme <- sample(1:4, n, replace = TRUE, prob = c(0.18, 0.28, 0.32, 0.22))

biodiversity_index_endline <- biodiversity_index_baseline +
  round(rnorm(n, 11.8, 5.4) * (years_in_programme / 4))
biodiversity_index_endline <- pmax(10, pmin(biodiversity_index_endline, 100))

# Forest cover change assessment (self-reported)
forest_cover_change <- sample(
  c("Significantly increased", "Moderately increased", "No change",
    "Slightly decreased", "Significantly decreased"),
  n, replace = TRUE,
  prob = c(0.18, 0.38, 0.22, 0.14, 0.08)
)

# Watershed co-management participation
watershed_comanagement <- sample(c("Active", "Occasional", "Not participating"),
                                  n, replace = TRUE, prob = c(0.48, 0.30, 0.22))

# ---- NTFPs & sustainable livelihoods -------------------------
# Non-timber forest product (NTFP) income (BDT/month)
ntfp_income_bdt <- round(rnorm(n, 2850, 1100))
ntfp_income_bdt <- pmax(200, pmin(ntfp_income_bdt, 12000))

# Eco-tourism or green enterprise involvement
eco_enterprise_involved <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.34, 0.66))

# Sustainable agriculture practices adopted
sustainable_ag_practices <- sample(0:6, n, replace = TRUE,
                                    prob = c(0.08, 0.14, 0.22, 0.26, 0.18, 0.08, 0.04))

# ---- Knowledge & attitudes -----------------------------------
# Biodiversity conservation knowledge (0-20)
conservation_knowledge_baseline <- round(rnorm(n, 10.2, 3.4))
conservation_knowledge_baseline <- pmax(0, pmin(conservation_knowledge_baseline, 20))

conservation_knowledge_endline <- conservation_knowledge_baseline +
  round(rnorm(n, 3.8, 1.6))
conservation_knowledge_endline <- pmax(0, pmin(conservation_knowledge_endline, 20))

# Attitude towards conservation (Likert 1-5)
attitude_conservation_baseline <- sample(1:5, n, replace = TRUE,
                                          prob = c(0.06, 0.14, 0.28, 0.34, 0.18))
attitude_conservation_endline  <- sample(1:5, n, replace = TRUE,
                                          prob = c(0.02, 0.06, 0.22, 0.40, 0.30))

# ---- Governance & institutions --------------------------------
# Community forest rules existence
community_rules_exist <- sample(c("Yes", "No", "Under development"),
                                 n, replace = TRUE, prob = c(0.56, 0.24, 0.20))

# Conflict over forest resources (in past 12 months)
resource_conflict_12mo <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.28, 0.72))

# Satisfaction with forest management
satisfaction_forest_mgmt <- sample(1:5, n, replace = TRUE,
                                    prob = c(0.04, 0.10, 0.24, 0.40, 0.22))

# Overall programme satisfaction
satisfaction_score <- sample(1:5, n, replace = TRUE,
                              prob = c(0.04, 0.08, 0.22, 0.42, 0.24))

# Training on biodiversity received
training_received <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.72, 0.28))

# ---- Climate linkage -----------------------------------------
climate_impact_on_biodiversity <- sample(
  c("Severe", "Moderate", "Mild", "No impact observed"),
  n, replace = TRUE, prob = c(0.24, 0.38, 0.26, 0.12)
)

# Interview date
interview_date <- sample(
  seq(as.Date("2024-09-05"), as.Date("2024-11-28"), by = "day"),
  n, replace = TRUE
)

# ---- Assemble dataset ----------------------------------------
bercr_df <- data.frame(
  respondent_id                  = paste0("BER-", sprintf("%04d", 1:n)),
  interview_date                 = interview_date,
  district                       = district_draw,
  upazila                        = upazila_draw,
  respondent_type                = respondent_type,
  age                            = age,
  sex                            = sex,
  ethnicity                      = ethnicity,
  education_level                = education_level,
  household_size                 = household_size,
  monthly_income_bdt             = monthly_income_bdt,
  years_in_programme             = years_in_programme,
  forest_land_managed_acres      = forest_land_managed_acres,
  tree_species_planted           = tree_species_planted,
  seedlings_distributed          = seedlings_distributed,
  seedling_survival_rate_pct     = seedling_survival_rate_pct,
  biodiversity_index_baseline    = biodiversity_index_baseline,
  biodiversity_index_endline     = biodiversity_index_endline,
  forest_cover_change            = forest_cover_change,
  watershed_comanagement         = watershed_comanagement,
  ntfp_income_bdt                = ntfp_income_bdt,
  eco_enterprise_involved        = eco_enterprise_involved,
  sustainable_ag_practices       = sustainable_ag_practices,
  conservation_knowledge_baseline = conservation_knowledge_baseline,
  conservation_knowledge_endline  = conservation_knowledge_endline,
  attitude_conservation_baseline = attitude_conservation_baseline,
  attitude_conservation_endline  = attitude_conservation_endline,
  community_rules_exist          = community_rules_exist,
  resource_conflict_12mo         = resource_conflict_12mo,
  satisfaction_forest_mgmt       = satisfaction_forest_mgmt,
  satisfaction_score             = satisfaction_score,
  training_received              = training_received,
  climate_impact_on_biodiversity = climate_impact_on_biodiversity,
  stringsAsFactors                = FALSE
)

write.csv(bercr_df,
          "/Users/md.kamrulhasan/All my project/R project/data/raw/bercr_survey.csv",
          row.names = FALSE)

cat("✅ BERCR dataset generated:", nrow(bercr_df), "rows x", ncol(bercr_df), "columns\n")
