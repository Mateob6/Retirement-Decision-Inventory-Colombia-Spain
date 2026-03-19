# Retirement Decision Inventory in Colombia and Spain: Cultural Adaptation and Validation Study

## Overview

This repository contains the data, analytic code, and materials for the manuscript:

> Díaz-Bambula, F., Londoño-Moreno, A.M., Belalcázar, M., Rubio, L., Dumitrache, C.G., Gabardo-Martins, L.M., & França, L.H.F.P. (2026). Retirement Decision Inventory in Colombia and España: Cultural Adaptation and Validation Study. *European Journal of Psychological Assessment*.

## Study Description

This study adapted and validated the *Inventário da Decisão da Aposentadoria* (IDA; França, 2022) into Spanish for use in two cultural contexts: Colombia and Spain. The IDA assesses retirement decision-making intentions across four dimensions: anticipate retirement (ANT), retire at the legal time (APO), bridge employment (PONT), and continue working full-time (CON).

A total of 1,037 workers aged 50 and older participated (415 from Colombia, 622 from Spain; 52.9% women). The instrument was evaluated through Confirmatory Factor Analysis (CFA) using WLSMV estimation, measurement invariance testing across countries, and internal consistency analyses.

## Repository Structure

```
├── README.md                  # This file
├── LICENSE                    # CC-BY 4.0 license
├── data/
│   └── COLOMBIA_ESPANHA_TOTAL.xlsx   # Complete dataset (N = 1,037)
├── code/
│   └── analysis_IDA_Colombia_Spain.R # Full analytic script (R/lavaan)
└── materials/
    └── IDA_Scale_Spanish_Versions.docx  # IDA scale in Colombian and
                                          # Spanish versions (+ English)
```

## Data Description

**File:** `data/COLOMBIA_ESPANHA_TOTAL.xlsx`

- **N** = 1,037 (Colombia: *n* = 415; Spain: *n* = 622)
- **Grouping variable:** `País` ("Colômbia" / "Espanha")
- **IDA items:** `EDA1`–`EDA20` (5-point Likert scale: 1 = *Strongly disagree* to 5 = *Strongly agree*)
- **Final model items (13):**
  - ANT (Anticipate retirement): EDA1, EDA3, EDA4, EDA5
  - APO (Retire at legal time): EDA7, EDA8, EDA9
  - PONT (Bridge employment): EDA12, EDA13, EDA14
  - CON (Continue working full-time): EDA17, EDA18, EDA19
- **Sociodemographic variables:** gender (`Sexo`), age (`Idade`), marital status (`Estado_civil`), education (`Escolaridade`), employment sector, perceived health (`Saude`), among others.

## Analytic Code

**File:** `code/analysis_IDA_Colombia_Spain.R`

The script reproduces all analyses reported in the manuscript:

1. **Confirmatory Factor Analysis (CFA)** — Three competing models:
   - (i) Two correlated factors (retiring vs. continuing)
   - (ii) Four correlated factors (ANT, APO, PONT, CON)
   - (iii) Four correlated factors with correlated residuals (EDA4–EDA5, EDA12–EDA14)
2. **Standardized factor loadings** and **factor correlations** from the best-fitting model
3. **Measurement invariance** (MGCFA) across Colombia and Spain:
   - Configural, metric, and scalar invariance
4. **Internal consistency:** Cronbach's alpha and McDonald's omega (total and by country)
5. **Modification indices** from Model 2 (justification for Model 3)
6. **Descriptive statistics** for all 13 items

### Software Requirements

- **R** (>= 4.0)
- **R packages:** `readxl`, `lavaan` (>= 0.6-12), `semTools`, `psych`

To install required packages:

```r
install.packages(c("readxl", "lavaan", "semTools", "psych"))
```

### How to Run

1. Open R or RStudio
2. Set your working directory to the repository root, or adjust the `project_dir` path in line 42 of the script
3. Run:

```r
source("code/analysis_IDA_Colombia_Spain.R")
```

All results will be printed to the console. The script uses `WLSMV` estimation with items treated as ordered (ordinal), consistent with the manuscript.

## Materials

**File:** `materials/IDA_Scale_Spanish_Versions.docx`

Contains the 13-item IDA scale in:
- Colombian Spanish version
- European Spanish version
- English translation

## Ethical Approval

The study received ethical approval from:
- Research Ethics Committee of Universidade Salgado de Oliveira, Brazil (CAAE No. 19575219.2.0000.5289)
- Ethics Committee of the Faculty of Psychology at Universidad del Valle, Colombia (No. 13)
- Human Research Ethics Committee of the University of Granada, Spain (No. 3598/CEIH/2023)

## Citation

If you use these materials, please cite:

```
Díaz-Bambula, F., Londoño-Moreno, A.M., Belalcázar, M., Rubio, L.,
Dumitrache, C.G., Gabardo-Martins, L.M., & França, L.H.F.P. (2026).
Retirement Decision Inventory in Colombia and España: Cultural Adaptation
and Validation Study. European Journal of Psychological Assessment.
```

## License

This work is licensed under [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Contact

For questions regarding this repository, please contact:

- Laura Rubio Rubio (corresponding author): lrrubio@ugr.es
- Department of Developmental and Educational Psychology, University of Granada, Spain
