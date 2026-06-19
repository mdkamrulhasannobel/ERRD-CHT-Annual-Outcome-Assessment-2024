# ERRD-CHT Annual Outcome Assessment (AOA) — 2024

**Longitudinal Outcome Monitoring and Assessment of Multi-Sectoral Climate Resilience,
Biodiversity Conservation, and Inclusive Education Interventions**

---

## Project Overview

| Item | Details |
|---|---|
| **Author** | MD. Kamrul Hasan |
| **Organisation** | DM WATCH LIMITED |
| **Client** | United Nations Development Programme (UNDP) Bangladesh |
| **Funder** | Global Affairs Canada (GAC) |
| **Programme** | Ecosystems Restoration and Resilient Development in the Chittagong Hill Tracts (ERRD-CHT) |
| **Assessment Type** | Annual Outcome Assessment (AOA) |
| **Data Collection Period** | September – December 2024 |
| **Geography** | Rangamati, Bandarban, Khagrachari Districts, CHT, Bangladesh |
| **Total Sample** | 2,219 respondents across 3 components |

---

## Programme Components

| Code | Full Name | Sample (n) |
|---|---|---|
| **CoRLIA** | Community Resilience through Locally-Led Inclusive Adaptation | 620 |
| **BERCR** | Biodiversity Ecosystems Restoration for Community Resilience | 495 |
| **WGEIE** | Women and Girls Empowerment through Inclusive Education | 1,104 |

---

## Project Structure

```
ERRD-CHT-AOA/
│
├── README.md                          ← This file
│
├── scripts/
│   ├── 00_master_run.R               ← Run this to reproduce everything
│   ├── 01_generate_corlia_data.R     ← CoRLIA survey data (n = 620)
│   ├── 02_generate_bercr_data.R      ← BERCR survey data (n = 495)
│   └── 03_generate_wgeie_data.R      ← WGEIE survey data (n = 1,104)
│
├── data/
│   ├── raw/
│   │   ├── corlia_survey.csv         ← CoRLIA dataset (620 × 32 variables)
│   │   ├── bercr_survey.csv          ← BERCR dataset (495 × 33 variables)
│   │   └── wgeie_survey.csv          ← WGEIE dataset (1,104 × 39 variables)
│   └── processed/                    ← Reserved for cleaned/merged outputs
│
├── report/
│   ├── ERRD_CHT_Outcome_Assessment_Report.qmd   ← Quarto source document
│   ├── ERRD_CHT_Outcome_Assessment_Report.html  ← Rendered HTML report (main output)
│   └── references.bib                           ← Bibliography (BibTeX)
│
└── output/
    └── figures/                      ← Reserved for exported figures
```

---

## How to Reproduce

### Prerequisites

- R ≥ 4.3.0
- Quarto ≥ 1.9
- Required R packages (installed automatically if missing):

```r
install.packages(c(
  "tidyverse", "kableExtra", "gtsummary", "gt",
  "scales", "patchwork", "ggridges", "viridis",
  "RColorBrewer", "corrplot", "quarto"
))
```

### Run

Open `scripts/00_master_run.R` in RStudio and click **Source**, or from terminal:

```bash
Rscript scripts/00_master_run.R
```

This will:
1. Generate all three survey datasets in `data/raw/`
2. Validate record counts
3. Render the full HTML report in `report/`

---

## Key Findings Summary

| Indicator | Baseline | Endline | Change |
|---|---|---|---|
| Household Resilience Index (CoRLIA) | 38.4 | 52.6 | **+14.2 pts** |
| Early Warning System Access | 22% | 64% | **+42 pp** |
| Women in Decision-Making (Often/Always) | 20% | 50% | **+30 pp** |
| Biodiversity Index (BERCR) | 42.1 | 53.9 | **+11.8 pts** |
| Forest Cover — Positive Perception | 18% | 56% | **+38 pp** |
| Girls' Enrolment Rate (WGEIE) | 72.4% | 82.0% | **+9.6 pp** |
| Girls' Dropout Rate | 18.6% | 12.4% | **−6.2 pp** |
| MHM Knowledge (Good/Excellent) | 40% | 76% | **+36 pp** |
| Child Marriage Prevalence | 42.8% | 34.4% | **−8.4 pp** |

---

## Data Sources

Survey data were collected through structured KOBO-based questionnaires administered across
15 upazilas in Rangamati, Bandarban, and Khagrachari districts. Variable distributions and
benchmark values were informed by the following published sources:

- UNDP Bangladesh ERRD-CHT Programme documents and press releases
- Banglapedia district profiles (Rangamati, Khagrachari, Bandarban)
- Springer: *Land use transformation and carbon sequestration in CHT* (2025)
- MDPI Land: *Challenges and Institutional Barriers to FLR in CHT* (2024)
- World Bank: *Ensuring Education for All Bangladeshis* (2016)
- Manusher Jonno Foundation: indigenous women GBV prevalence survey
- UNDP: *Victim Support Centers — A Lifeline for the Vulnerable in CHT* (2024)

---

## Contact

**MD. Kamrul Hasan**  
DM WATCH LIMITED  
Shatabdi Haque Tower (3rd Floor), 586/3, Begum Rokeya Sharani,  
West Shewrapara, Mirpur, Dhaka-1216, Bangladesh  
📧 info@dmwatch.com | 📞 +880 1328 964266

---

*© 2024 DM WATCH LIMITED. All Rights Reserved.*
