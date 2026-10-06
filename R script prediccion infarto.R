

## Carga de datasets
demographic_base <- read.csv('Dataset/demographic.csv', header = TRUE)
examination_base <- read.csv('Dataset/examination.csv', header = TRUE)
questionnaire_base <- read.csv('Dataset/questionnaire.csv', header = TRUE)


demographic <- demographic_base
examination <- examination_base
questionnaire <- questionnaire_base

## Se instala dplyr
install.packages("dplyr")
library(dplyr)

## Selelcionar y renombrar campos demographic
demographic <- demographic %>%
  select(
    SEQN,
    RIDAGEYR,
    RIAGENDR,
    RIDRETH3,
    INDFMPIR,
    DMDEDUC2
  ) %>%
  rename(
    age = RIDAGEYR,
    sex = RIAGENDR,
    ethnicity_race = RIDRETH3,
    income_poverty_ratio= INDFMPIR,
    education_level  = DMDEDUC2
  )

## Selelcionar y renombrar campos examination
examination <- examination %>%
  select(
    SEQN,
    BPXSY1,
    BPXSY2,
    BPXDI1,
    BPXDI2,
    BPXPLS,
    BMXBMI,
    BMXWAIST
  ) %>%
  rename(
    systolic_bp_reading_1  = BPXSY1,
    systolic_bp_reading_2 = BPXSY2,
    diastolic_bp_reading_1  = BPXDI1,
    diastolic_bp_reading_2 = BPXDI2,
    resting_heart_rate  = BPXPLS,
    body_mass_index = BMXBMI,
    waist_circumference= BMXWAIST
  )


View(demographic)

## Selelcionar y renombrar campos questionnaire
questionnaire <- questionnaire %>%
  select(
    SEQN,
    MCQ160E,
    MCQ160C,
    MCQ160D,
    SMQ020,
    SMQ040,
    PAQ650,
    PAD680,
    SLQ050,
    MCQ300C
  ) %>%
  rename(
    previous_myocardial_infarction = MCQ160E,
    coronary_heart_disease = MCQ160C,
    angina_pectoris = MCQ160D,
    cumulative_smoking_100_tabacco = SMQ020,
    current_tobacco_use_frequency = SMQ040,
    weekly_vigorous_physical_activity = PAQ650,
    daily_sedentary_minutes = PAD680,
    sleep_disorder_insomnia = SLQ050,
    family_history_mi_under_50 = MCQ300C
  )


## Unificar información

data_merge_demographic_examination <- merge(demographic, examination, by = "SEQN")

data_merge <- merge(data_merge_demographic_examination, questionnaire)

View(data_merge)

