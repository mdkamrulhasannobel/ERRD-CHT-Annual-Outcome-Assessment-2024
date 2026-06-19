# ============================================================
# Script: 04_export_figures.R
# Project: ERRD-CHT Longitudinal Outcome Monitoring and Assessment
# Purpose: Export all key analytical figures to output/figures/
#          as high-resolution PNG files (300 dpi)
# Author:  MD. Kamrul Hasan | DM WATCH LIMITED
# Client:  UNDP Bangladesh / Global Affairs Canada (GAC)
# ============================================================

set.seed(2024)
library(tidyverse)
library(scales)
library(patchwork)
library(ggridges)
library(RColorBrewer)
library(viridis)
library(corrplot)

# ---- Paths ---------------------------------------------------
fig_dir <- "/Users/md.kamrulhasan/All my project/R project/output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load data -----------------------------------------------
corlia <- read.csv("/Users/md.kamrulhasan/All my project/R project/data/raw/corlia_survey.csv",
                   stringsAsFactors = FALSE)
bercr  <- read.csv("/Users/md.kamrulhasan/All my project/R project/data/raw/bercr_survey.csv",
                   stringsAsFactors = FALSE)
wgeie  <- read.csv("/Users/md.kamrulhasan/All my project/R project/data/raw/wgeie_survey.csv",
                   stringsAsFactors = FALSE)

# ---- Colour palettes -----------------------------------------
pal_comp   <- c("CoRLIA" = "#1B7A4E", "BERCR" = "#D4700A", "WGEIE" = "#1F4E8C")
pal_gender <- c("Female" = "#E05C5C", "Male" = "#4A90D9")
pal_main   <- c("#1B7A4E", "#2E9E72", "#5CB8A0", "#A8D8C8", "#D4EDE7")

# Factor levels
dec_levels <- c("Never", "Rarely", "Sometimes", "Often", "Always")
mhm_levels <- c("Poor", "Moderate", "Good", "Excellent")
corlia$women_decision_making_baseline <- factor(corlia$women_decision_making_baseline, levels = dec_levels)
corlia$women_decision_making_endline  <- factor(corlia$women_decision_making_endline,  levels = dec_levels)
wgeie$mhm_knowledge_baseline <- factor(wgeie$mhm_knowledge_baseline, levels = mhm_levels)
wgeie$mhm_knowledge_endline  <- factor(wgeie$mhm_knowledge_endline,  levels = mhm_levels)

# Helper: save figure
save_fig <- function(plot, filename, w = 10, h = 6) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot     = plot,
    width    = w, height = h, dpi = 300,
    bg       = "white"
  )
  cat("  ✅ Saved:", filename, "\n")
}

cat("\n============================================================\n")
cat(" Exporting figures to output/figures/\n")
cat("============================================================\n\n")

# ============================================================
# SECTION 1: SAMPLE OVERVIEW
# ============================================================
cat("── Section 1: Sample Overview\n")

all_resp <- bind_rows(
  corlia |> select(respondent_id, district, sex, ethnicity) |> mutate(component = "CoRLIA"),
  bercr  |> select(respondent_id, district, sex, ethnicity) |> mutate(component = "BERCR"),
  wgeie  |> select(respondent_id, district, sex, ethnicity) |> mutate(component = "WGEIE")
)

# Fig 1.1 — Sample by district and component
fig_sample_dist <- all_resp |>
  count(component, district) |>
  ggplot(aes(x = district, y = n, fill = component)) +
  geom_col(position = "dodge", width = 0.7, colour = "white") +
  geom_text(aes(label = comma(n)), position = position_dodge(0.7),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = pal_comp) +
  scale_y_continuous(labels = comma, limits = c(0, 500)) +
  labs(x = NULL, y = "Number of Respondents", fill = "Component",
       title = "Survey Sample Distribution by District and Programme Component",
       subtitle = "ERRD-CHT Annual Outcome Assessment 2024 | Total n = 2,219",
       caption = "Source: ERRD-CHT AOA Field Survey, 2024 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_sample_dist, "fig_01_sample_distribution.png", w = 10, h = 6)

# Fig 1.2 — Ethnic group breakdown
fig_ethnicity <- all_resp |>
  count(ethnicity) |>
  mutate(ethnicity = fct_reorder(ethnicity, n), pct = n / sum(n)) |>
  ggplot(aes(x = ethnicity, y = pct, fill = pct)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct * 100, 1), "%")), hjust = -0.15, size = 3.8) +
  scale_fill_gradient(low = "#A8D8C8", high = "#1B7A4E") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.33)) +
  coord_flip() +
  labs(x = NULL, y = "Share of Total Sample",
       title = "Ethnic Group Representation — All Respondents (n = 2,219)",
       subtitle = "CHT is home to 13 distinct ethnic minority groups alongside Bengali community",
       caption = "Source: ERRD-CHT AOA Field Survey, 2024 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_ethnicity, "fig_02_ethnic_composition.png", w = 10, h = 6)

# ============================================================
# SECTION 2: CoRLIA — CLIMATE RESILIENCE
# ============================================================
cat("\n── Section 2: CoRLIA — Climate Resilience\n")

# Fig 2.1 — Climate shocks
fig_shocks <- corlia |>
  count(climate_shocks_experienced) |>
  mutate(climate_shocks_experienced = fct_reorder(climate_shocks_experienced, n),
         pct = n / sum(n)) |>
  ggplot(aes(x = climate_shocks_experienced, y = n, fill = climate_shocks_experienced)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = paste0(n, "  (", round(pct * 100, 1), "%)")),
            hjust = -0.05, size = 3.6) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_y_continuous(limits = c(0, 210)) +
  coord_flip() +
  labs(x = NULL, y = "Number of Respondents",
       title = "Climate Shocks Experienced — CoRLIA Respondents",
       subtitle = "Landslide and flash flooding are the dominant climate hazards in CHT",
       caption = "Source: CoRLIA Survey, n = 620 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_shocks, "fig_03_corlia_climate_shocks.png", w = 10, h = 6)

# Fig 2.2 — Resilience index ridge plot
fig_resilience <- corlia |>
  select(resilience_index_baseline, resilience_index_endline) |>
  pivot_longer(everything(), names_to = "period", values_to = "score") |>
  mutate(period = recode(period,
    "resilience_index_baseline" = "Baseline",
    "resilience_index_endline"  = "Endline")) |>
  ggplot(aes(x = score, y = period, fill = period)) +
  geom_density_ridges(alpha = 0.75, scale = 1.2, quantile_lines = TRUE,
                      quantiles = c(0.25, 0.5, 0.75), rel_min_height = 0.01) +
  scale_fill_manual(values = c("Baseline" = "#B0C4DE", "Endline" = "#1B7A4E")) +
  labs(x = "Household Resilience Index Score (0–100)", y = NULL,
       title = "Household Resilience Index — Baseline vs. Endline Distribution",
       subtitle = paste0("Mean improved from ",
                         round(mean(corlia$resilience_index_baseline), 1), " to ",
                         round(mean(corlia$resilience_index_endline), 1),
                         " (+14.2 pts) | Quartile lines shown"),
       caption = "Source: CoRLIA Survey, n = 620 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_resilience, "fig_04_corlia_resilience_index.png", w = 10, h = 6)

# Fig 2.3 — Resilience by district
fig_res_district <- corlia |>
  group_by(district) |>
  summarise(Baseline = mean(resilience_index_baseline),
            Endline  = mean(resilience_index_endline)) |>
  pivot_longer(-district, names_to = "Period", values_to = "Score") |>
  ggplot(aes(x = district, y = Score, fill = Period)) +
  geom_col(position = "dodge", width = 0.6, colour = "white") +
  geom_text(aes(label = round(Score, 1)), position = position_dodge(0.6),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#B0C4DE", "Endline" = "#1B7A4E")) +
  scale_y_continuous(limits = c(0, 70)) +
  labs(x = NULL, y = "Mean Resilience Index (0–100)", fill = "Period",
       title = "Household Resilience Index by District — Baseline vs. Endline",
       subtitle = "All three districts show consistent improvement across the programme period",
       caption = "Source: CoRLIA Survey, n = 620 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_res_district, "fig_05_corlia_resilience_by_district.png", w = 10, h = 6)

# Fig 2.4 — Women's decision-making
fig_decision <- bind_rows(
  corlia |> filter(sex == "Female") |>
    count(level = women_decision_making_baseline) |>
    mutate(period = "Baseline", pct = n / sum(n)),
  corlia |> filter(sex == "Female") |>
    count(level = women_decision_making_endline) |>
    mutate(period = "Endline",  pct = n / sum(n))
) |>
  mutate(level = factor(level, levels = dec_levels)) |>
  ggplot(aes(x = level, y = pct, fill = period)) +
  geom_col(position = "dodge", width = 0.65, colour = "white") +
  geom_text(aes(label = paste0(round(pct * 100, 0), "%")),
            position = position_dodge(0.65), vjust = -0.4, size = 3.6) +
  scale_fill_manual(values = c("Baseline" = "#F4A261", "Endline" = "#1B7A4E")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.40)) +
  labs(x = "Frequency of Participation", y = "Proportion of Women Respondents",
       fill = "Period",
       title = "Women's Participation in Community Decision-Making",
       subtitle = "Structural shift: 'Often/Always' rose from 20% to 50% | CoRLIA female respondents",
       caption = "Source: CoRLIA Survey, n = 620 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_decision, "fig_06_corlia_womens_decision_making.png", w = 10, h = 6)

# Fig 2.5 — Early warning system access
fig_ews <- corlia |>
  group_by(district) |>
  summarise(Baseline = mean(access_early_warning_baseline == "Yes") * 100,
            Endline  = mean(access_early_warning_endline  == "Yes") * 100) |>
  pivot_longer(-district, names_to = "Period", values_to = "Pct") |>
  ggplot(aes(x = district, y = Pct, fill = Period)) +
  geom_col(position = "dodge", width = 0.6, colour = "white") +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            position = position_dodge(0.6), vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#B0C4DE", "Endline" = "#1B7A4E")) +
  scale_y_continuous(limits = c(0, 85), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "% with Access to Early Warning Systems", fill = "Period",
       title = "Access to Climate Early Warning Systems — Baseline vs. Endline",
       subtitle = "Near-tripling of EWS access across all three districts",
       caption = "Source: CoRLIA Survey, n = 620 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_ews, "fig_07_corlia_early_warning_access.png", w = 10, h = 6)

# ============================================================
# SECTION 3: BERCR — BIODIVERSITY & ECOSYSTEM RESTORATION
# ============================================================
cat("\n── Section 3: BERCR — Biodiversity & Ecosystem Restoration\n")

# Fig 3.1 — Biodiversity index by district
fig_bio_district <- bercr |>
  group_by(district) |>
  summarise(Baseline = mean(biodiversity_index_baseline),
            Endline  = mean(biodiversity_index_endline)) |>
  pivot_longer(-district, names_to = "Period", values_to = "Index") |>
  ggplot(aes(x = district, y = Index, fill = Period)) +
  geom_col(position = "dodge", width = 0.6, colour = "white") +
  geom_text(aes(label = round(Index, 1)), position = position_dodge(0.6),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#A8D8C8", "Endline" = "#1B7A4E")) +
  scale_y_continuous(limits = c(0, 72)) +
  labs(x = NULL, y = "Mean Biodiversity Index (0–100)", fill = "Period",
       title = "Community Biodiversity Index — Baseline vs. Endline by District",
       subtitle = "Composite score reflecting species richness, habitat quality, and management practices",
       caption = "Source: BERCR Survey, n = 495 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_bio_district, "fig_08_bercr_biodiversity_index.png", w = 10, h = 6)

# Fig 3.2 — Biodiversity gain vs. years in programme
fig_bio_dose <- bercr |>
  mutate(bio_gain = biodiversity_index_endline - biodiversity_index_baseline) |>
  ggplot(aes(x = years_in_programme, y = bio_gain, colour = district)) +
  geom_jitter(alpha = 0.45, width = 0.15, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 1.2) +
  scale_colour_manual(values = c("#1B7A4E", "#D4700A", "#1F4E8C")) +
  labs(x = "Years of Programme Participation", y = "Biodiversity Index Gain (Endline − Baseline)",
       colour = "District",
       title = "Dose-Response: Programme Duration and Biodiversity Recovery",
       subtitle = "Longer participation strongly associated with greater biodiversity improvements",
       caption = "Source: BERCR Survey, n = 495 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_bio_dose, "fig_09_bercr_biodiversity_dose_response.png", w = 10, h = 6)

# Fig 3.3 — Forest cover change perceptions
fc_levels <- c("Significantly decreased", "Slightly decreased", "No change",
               "Moderately increased", "Significantly increased")

fig_forest_cover <- bercr |>
  mutate(forest_cover_change = factor(forest_cover_change, levels = fc_levels)) |>
  count(district, forest_cover_change) |>
  group_by(district) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = district, y = pct, fill = forest_cover_change)) +
  geom_col(colour = "white", width = 0.7) +
  scale_fill_manual(values = c("#C0392B", "#E67E22", "#F1C40F", "#82C341", "#1B7A4E")) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = NULL, y = "Proportion", fill = "Perceived Forest Cover Change",
       title = "Community Perception of Forest Cover Change by District",
       subtitle = "56% report moderate or significant increase — up from 18% at baseline",
       caption = "Source: BERCR Survey, n = 495 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8),
        legend.text = element_text(size = 9))

save_fig(fig_forest_cover, "fig_10_bercr_forest_cover_change.png", w = 10, h = 6)

# Fig 3.4 — Conservation knowledge
fig_cons_know <- bercr |>
  select(conservation_knowledge_baseline, conservation_knowledge_endline) |>
  pivot_longer(everything(), names_to = "period", values_to = "score") |>
  mutate(period = recode(period,
    "conservation_knowledge_baseline" = "Baseline",
    "conservation_knowledge_endline"  = "Endline")) |>
  ggplot(aes(x = score, fill = period)) +
  geom_density(alpha = 0.65, colour = "white") +
  geom_vline(xintercept = mean(bercr$conservation_knowledge_baseline),
             colour = "#B0C4DE", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = mean(bercr$conservation_knowledge_endline),
             colour = "#1B7A4E", linetype = "dashed", linewidth = 1.2) +
  scale_fill_manual(values = c("Baseline" = "#B0C4DE", "Endline" = "#1B7A4E")) +
  labs(x = "Conservation Knowledge Score (0–20)", y = "Density", fill = "Period",
       title = "Biodiversity Conservation Knowledge — Baseline vs. Endline",
       subtitle = paste0("Mean score: Baseline = ",
                         round(mean(bercr$conservation_knowledge_baseline), 1),
                         " → Endline = ",
                         round(mean(bercr$conservation_knowledge_endline), 1),
                         " | Dashed lines = group means"),
       caption = "Source: BERCR Survey, n = 495 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_cons_know, "fig_11_bercr_conservation_knowledge.png", w = 10, h = 6)

# Fig 3.5 — NTFP income by watershed co-management
fig_ntfp <- bercr |>
  ggplot(aes(x = ntfp_income_bdt, fill = watershed_comanagement,
             colour = watershed_comanagement)) +
  geom_density(alpha = 0.5, linewidth = 0.9) +
  scale_fill_manual(values  = c("Active" = "#1B7A4E", "Occasional" = "#D4700A",
                                "Not participating" = "#999999")) +
  scale_colour_manual(values = c("Active" = "#1B7A4E", "Occasional" = "#D4700A",
                                 "Not participating" = "#999999")) +
  scale_x_continuous(labels = function(x) paste0("৳", comma(x))) +
  labs(x = "Monthly NTFP Income (BDT)", y = "Density",
       fill = "Watershed Co-Management Status",
       colour = "Watershed Co-Management Status",
       title = "NTFP Income Distribution by Watershed Co-Management Participation",
       subtitle = "Active co-managers earn approximately 28% more monthly NTFP income",
       caption = "Source: BERCR Survey, n = 495 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_ntfp, "fig_12_bercr_ntfp_income.png", w = 10, h = 6)

# ============================================================
# SECTION 4: WGEIE — INCLUSIVE EDUCATION & EMPOWERMENT
# ============================================================
cat("\n── Section 4: WGEIE — Inclusive Education & Empowerment\n")

# Fig 4.1 — Girls' enrolment rate
fig_enrolment <- wgeie |>
  group_by(district) |>
  summarise(Baseline = mean(enrollment_rate_baseline_pct),
            Endline  = mean(enrollment_rate_endline_pct)) |>
  pivot_longer(-district, names_to = "Period", values_to = "Rate") |>
  ggplot(aes(x = district, y = Rate, fill = Period)) +
  geom_col(position = "dodge", width = 0.6, colour = "white") +
  geom_text(aes(label = paste0(round(Rate, 1), "%")),
            position = position_dodge(0.6), vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#B0C4DE", "Endline" = "#1F4E8C")) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Girls' Enrolment Rate (%)", fill = "Period",
       title = "Girls' School Enrolment Rate — Baseline vs. Endline by District",
       subtitle = "Overall enrolment rose from 72.4% to 82.0% (+9.6 percentage points)",
       caption = "Source: WGEIE Survey, n = 1,104 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_enrolment, "fig_13_wgeie_girls_enrolment.png", w = 10, h = 6)

# Fig 4.2 — Dropout rate reduction
fig_dropout <- wgeie |>
  group_by(district) |>
  summarise(Baseline = mean(dropout_rate_girls_baseline_pct),
            Endline  = mean(dropout_rate_girls_endline_pct)) |>
  pivot_longer(-district, names_to = "Period", values_to = "Rate") |>
  ggplot(aes(x = district, y = Rate, fill = Period)) +
  geom_col(position = "dodge", width = 0.6, colour = "white") +
  geom_text(aes(label = paste0(round(Rate, 1), "%")),
            position = position_dodge(0.6), vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#E05C5C", "Endline" = "#82C341")) +
  scale_y_continuous(limits = c(0, 28), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Girls' Dropout Rate (%)", fill = "Period",
       title = "Girls' School Dropout Rate — Baseline vs. Endline by District",
       subtitle = "Consistent reduction across all districts; overall decline of ~6.2 percentage points",
       caption = "Source: WGEIE Survey, n = 1,104 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_dropout, "fig_14_wgeie_dropout_rate.png", w = 10, h = 6)

# Fig 4.3 — Agency score ridge plot
fig_agency <- wgeie |>
  filter(sex == "Female") |>
  select(agency_score_baseline, agency_score_endline) |>
  pivot_longer(everything(), names_to = "period", values_to = "score") |>
  mutate(period = recode(period,
    "agency_score_baseline" = "Baseline",
    "agency_score_endline"  = "Endline")) |>
  ggplot(aes(x = score, y = period, fill = period)) +
  geom_density_ridges(alpha = 0.75, scale = 1.1, quantile_lines = TRUE,
                      quantiles = 0.5, rel_min_height = 0.01) +
  scale_fill_manual(values = c("Baseline" = "#F4C2C2", "Endline" = "#C2185B")) +
  labs(x = "Agency / Empowerment Score (0–10)", y = NULL,
       title = "Girls' Agency and Empowerment Score — Baseline vs. Endline",
       subtitle = paste0("Mean improved from ",
                         round(mean(wgeie$agency_score_baseline[wgeie$sex == "Female"], na.rm=TRUE), 1),
                         " to ",
                         round(mean(wgeie$agency_score_endline[wgeie$sex == "Female"], na.rm=TRUE), 1),
                         " | Median line shown"),
       caption = "Source: WGEIE Survey, female respondents | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_agency, "fig_15_wgeie_girls_agency_score.png", w = 10, h = 6)

# Fig 4.4 — Child marriage prevalence
fig_child_marriage <- wgeie |>
  group_by(district) |>
  summarise(Baseline = mean(child_marriage_prevalence_baseline_pct),
            Endline  = mean(child_marriage_prevalence_endline_pct)) |>
  pivot_longer(-district, names_to = "Period", values_to = "Rate") |>
  ggplot(aes(x = district, y = Rate, fill = Period)) +
  geom_col(position = "dodge", width = 0.6, colour = "white") +
  geom_text(aes(label = paste0(round(Rate, 1), "%")),
            position = position_dodge(0.6), vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#E05C5C", "Endline" = "#82C341")) +
  scale_y_continuous(limits = c(0, 55), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Child Marriage Prevalence (%)", fill = "Period",
       title = "Reported Child Marriage Prevalence — Baseline vs. Endline",
       subtitle = "Decline of ~8.4 percentage points overall; continued intervention required",
       caption = "Source: WGEIE Survey, n = 1,104 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_child_marriage, "fig_16_wgeie_child_marriage.png", w = 10, h = 6)

# Fig 4.5 — MHM knowledge shift
fig_mhm <- bind_rows(
  wgeie |> count(level = mhm_knowledge_baseline) |>
    mutate(period = "Baseline", pct = n / sum(n)),
  wgeie |> count(level = mhm_knowledge_endline) |>
    mutate(period = "Endline",  pct = n / sum(n))
) |>
  mutate(level = factor(level, levels = mhm_levels)) |>
  ggplot(aes(x = level, y = pct, fill = period)) +
  geom_col(position = "dodge", width = 0.65, colour = "white") +
  geom_text(aes(label = paste0(round(pct * 100, 0), "%")),
            position = position_dodge(0.65), vjust = -0.4, size = 3.8) +
  scale_fill_manual(values = c("Baseline" = "#F4A261", "Endline" = "#C2185B")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.50)) +
  labs(x = "MHM Knowledge Level", y = "Proportion", fill = "Period",
       title = "Menstrual Health Management (MHM) Knowledge — Baseline vs. Endline",
       subtitle = "'Poor' knowledge: 26% → 6% | 'Excellent' knowledge: 12% → 34%",
       caption = "Source: WGEIE Survey, n = 1,104 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_mhm, "fig_17_wgeie_mhm_knowledge.png", w = 10, h = 6)

# ============================================================
# SECTION 5: CROSS-CUTTING
# ============================================================
cat("\n── Section 5: Cross-Cutting Analysis\n")

# Fig 5.1 — Programme satisfaction
sat_all <- rbind(
  data.frame(component = "CoRLIA", satisfaction_score = corlia$satisfaction_score),
  data.frame(component = "BERCR",  satisfaction_score = bercr$satisfaction_score),
  data.frame(component = "WGEIE",  satisfaction_score = wgeie$satisfaction_score)
)

fig_satisfaction <- sat_all |>
  ggplot(aes(x = factor(satisfaction_score), fill = component)) +
  geom_bar(position = "dodge", colour = "white", width = 0.7) +
  scale_fill_manual(values = pal_comp) +
  scale_y_continuous(labels = comma) +
  labs(x = "Satisfaction Score  (1 = Very Dissatisfied  →  5 = Very Satisfied)",
       y = "Number of Respondents", fill = "Component",
       title = "Overall Programme Satisfaction — All Respondents (n = 2,219)",
       subtitle = "More than 65% of respondents across all components rated 4 or 5",
       caption = "Source: ERRD-CHT AOA Field Survey, 2024 | DM WATCH LIMITED") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8))

save_fig(fig_satisfaction, "fig_18_programme_satisfaction.png", w = 10, h = 6)

# Fig 5.2 — Correlation matrix (CoRLIA)
cor_vars <- corlia |>
  select(resilience_index_endline, adaptation_practices_score_endline,
         climate_knowledge_score, livelihood_sources_endline,
         savings_monthly_bdt, monthly_income_bdt, household_size)

colnames(cor_vars) <- c("Resilience\nIndex", "Adaptation\nScore", "Climate\nKnowledge",
                         "Livelihood\nSources", "Savings\n(BDT)", "Income\n(BDT)", "HH Size")

png(file.path(fig_dir, "fig_19_corlia_correlation_matrix.png"),
    width = 2800, height = 2400, res = 300, bg = "white")
corrplot::corrplot(
  cor(cor_vars, use = "complete.obs"),
  method     = "color",
  type       = "upper",
  tl.cex     = 0.9,
  addCoef.col = "black",
  number.cex  = 0.75,
  col  = colorRampPalette(c("#1F4E8C", "white", "#1B7A4E"))(200),
  title = "Pearson Correlation — CoRLIA Outcome Indicators",
  mar  = c(0, 0, 2, 0)
)
dev.off()
cat("  ✅ Saved: fig_19_corlia_correlation_matrix.png\n")

# Fig 5.3 — PMF Summary bar chart
pmf_summary <- tibble(
  Indicator = c(
    "Resilience Index\n(CoRLIA)",
    "EWS Access\n(CoRLIA)",
    "Women in Decision-Making\n(CoRLIA)",
    "Adaptation Practices\n(CoRLIA)",
    "Biodiversity Index\n(BERCR)",
    "Forest Cover Increase\n(BERCR)",
    "Conservation Knowledge\n(BERCR)",
    "Girls' Enrolment\n(WGEIE)",
    "Girls' Dropout ↓\n(WGEIE)",
    "Agency Score\n(WGEIE)"
  ),
  Component = c(rep("CoRLIA", 4), rep("BERCR", 3), rep("WGEIE", 3)),
  Baseline  = c(38.4, 22.0, 20.0, 32.0, 42.1, 18.0, 51.0, 72.4, 18.6, 38.0),
  Endline   = c(52.6, 64.0, 50.0, 59.0, 53.9, 56.0, 69.0, 82.0, 12.4, 59.0),
  Target    = c(50.0, 65.0, 55.0, 55.0, 54.0, 50.0, 70.0, 82.0, 13.0, 58.0)
)

fig_pmf <- pmf_summary |>
  mutate(Indicator = factor(Indicator, levels = rev(Indicator)),
         on_target = Endline >= Target) |>
  ggplot(aes(y = Indicator)) +
  geom_segment(aes(x = Baseline, xend = Endline, yend = Indicator, colour = Component),
               linewidth = 2.5, alpha = 0.8) +
  geom_point(aes(x = Baseline), colour = "grey60", size = 3.5) +
  geom_point(aes(x = Endline, colour = Component), size = 4.5, shape = 19) +
  geom_point(aes(x = Target), shape = 124, size = 5, colour = "black") +
  scale_colour_manual(values = pal_comp) +
  labs(x = "Score / Percentage", y = NULL, colour = "Component",
       title = "PMF Indicator Progress — Baseline → Endline vs. Target",
       subtitle = "Grey dot = Baseline  |  Colour dot = Endline  |  | = Target",
       caption = "Source: ERRD-CHT AOA Field Survey, 2024 | DM WATCH LIMITED") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey50", size = 8),
        axis.text.y = element_text(size = 9))

save_fig(fig_pmf, "fig_20_pmf_progress_tracker.png", w = 11, h = 8)

# ---- Final summary -------------------------------------------
cat("\n============================================================\n")
fig_files <- list.files(fig_dir, pattern = "\\.png$")
cat(" ✅ Export complete —", length(fig_files), "figures saved to:\n")
cat("    output/figures/\n\n")
for (f in fig_files) cat("   ", f, "\n")
cat("============================================================\n")
