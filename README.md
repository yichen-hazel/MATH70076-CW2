# MATH70076 Coursework – Depression and Risk Factors (NHANES)

This repository presents the final project for the MATH70076 Data Science module at Imperial College London. The project investigates the relationship between depressive symptoms (PHQ-9 scores) and various demographic, socioeconomic, and health-related risk factors based on the NHANES dataset 

---

## 📁 Repository Structure

- `Reflective_summary/`: Reflective summary of this project
- `data/`: Cleaned and raw NHANES data files, Official NHANES questionnaire documentation and variable codebooks, List of cross-cycle consistent NHANES variables
- `graph/`: All visualizations used in the analysis process and report
- `scripts/`: R scripts for each analysis process
- `report/`: Final report in PDF and RMarkdown format
---

## 📊 Analysis Summary

- **Objective**: Model depressive symptom scores (PHQ-9) using Negative Binomial regression due to overdispersion of data.
- **Key Variables**: various demographic, socioeconomic, and health-related risk factors from DEMO, DPQ, ALQ, SMQ, PAD, DIQ, and MCQ modules.
- **Interactions**: Gender*Age , Asthma*Age, Sedentary_Minutes*Gender were tested for moderation effects.
- **Diagnostics**: Residual plots, IRR interpretation, and influence checks were conducted.

---

## 📄 Precise Variable Description
### Dependent variable (outcome):
- **PHQ_total**: Total score of the Patient Health Questionnaire-9 (PHQ-9), measuring depressive symptoms (range: 0–27).

### Independent variables (predictors):
- **gender**: Biological sex of the participant (Male or Female).
- **race**: Self-identified race/ethnicity, categorized as Non-Hispanic White (reference), Non-Hispanic Black, Mexican American, Other Hispanic, Non-Hispanic Asian, or Other Race including Multi-Racial.
- **birth_country**: Country of birth: “US-born” or “Foreign-born” (reference: US-born).
- **heavy_drinker**: Whether consumed 4/5 or more alcoholic drinks per day in the past year (reference: No).
- **diabetes**: Doctor-diagnosed diabetes status (reference: No).
- **has_asthma**: History of asthma diagnosis (reference: No).
- **has_liver**: History of liver condition (reference: No).
- **sedentary_minutes**: Minutes spent in sedentary activities on a typical day.
- **poverty_ratio**: Ratio of family income to poverty level (higher poverty ratios indicate higher income relative to the poverty threshold).
- **age**: Age of participant age at the time of interview.


## 🛠 R, R Markdown package Used

- ggplot2  
- patchwork  
- dplyr  
- forcats  
- MASS  
- broom  
- knitr  
- kableExtra  
- stringr
- nhanesA
---

## 📎 Data used

- NHANES: https://www.cdc.gov/nchs/nhanes/index.htm
Note: The data was downloaded using the nhanesA package It can also be accessed from the official website [NHANES](https://wwwn.cdc.gov/nchs/nhanes/)

---

## 👤 Author

02051768
Imperial College London  
May 2025
