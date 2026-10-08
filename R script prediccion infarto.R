

## Carga de datasets
demographic_base <- read.csv('Dataset/demographic.csv', header = TRUE)
examination_base <- read.csv('Dataset/examination.csv', header = TRUE)
questionnaire_base <- read.csv('Dataset/questionnaire.csv', header = TRUE)


demographic <- demographic_base
examination <- examination_base
questionnaire <- questionnaire_base

## Se carga dplyr
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
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

data_merge <- merge(data_merge_demographic_examination, questionnaire, by = "SEQN")

## Filtro poblacional: adultos (edad >= 20 años)
data_merge <- data_merge %>%
  filter(age >= 20)

View(data_merge)

## Trasformar información
table(data_merge$sex)
map_sex_type <- c("Male" = 1, "Female" = 2)
map_race_type <- c(
  "Mexican American" = 1,
  "Other Hispanic" = 2,
  "Non-Hispanic White" = 3,
  "Non-Hispanic Black" = 4,
  "Non-Hispanic Asian" = 6,
  "Other Race - Including Multi-Racial" = 7,
  "Don't Know / Refused" = 9
)
map_education_type <- c(
  "Less than 9th grade" = 1,
  "9-11th grade" = 2,
  "High school graduate/GED or equivalent" = 3,
  "Some college or AA degree" = 4,
  "College graduate or above" = 5,
  "Refused" = 7,
  "Don't know" = 9
)

getKeyByValue <- function(data, map) {
  names(map)[match(data, map)]
}

data_merge$sex <- getKeyByValue(data_merge$sex, map_sex_type)
data_merge$ethnicity_race <- getKeyByValue(data_merge$ethnicity_race, map_race_type)
data_merge$education_level <- getKeyByValue(data_merge$education_level, map_education_type)

## Recodificar variables binarias de NHANES (1 = Sí -> 1, 2 = No -> 0; 7/9 -> NA)
clean_nhanes_binary <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE   ~ NA_real_
  )
}

data_merge <- data_merge %>%
  mutate(
    previous_myocardial_infarction    = clean_nhanes_binary(previous_myocardial_infarction),
    coronary_heart_disease            = clean_nhanes_binary(coronary_heart_disease),
    angina_pectoris                   = clean_nhanes_binary(angina_pectoris),
    cumulative_smoking_100_tabacco    = clean_nhanes_binary(cumulative_smoking_100_tabacco),
    weekly_vigorous_physical_activity = clean_nhanes_binary(weekly_vigorous_physical_activity),
    sleep_disorder_insomnia           = clean_nhanes_binary(sleep_disorder_insomnia),
    family_history_mi_under_50        = clean_nhanes_binary(family_history_mi_under_50)
  )
## Promedio de constantes clínicas (Presión arterial sistólica y diastólica)
data_merge <- data_merge %>%
  mutate(
    systolic_bp = rowMeans(cbind(systolic_bp_reading_1, systolic_bp_reading_2), na.rm = TRUE),
    diastolic_bp = rowMeans(cbind(diastolic_bp_reading_1, diastolic_bp_reading_2), na.rm = TRUE)
  )

## Depuración de sedentarismo (eliminar valores  > 1440 min) y escala en horas
data_merge <- data_merge %>%
  mutate(
    daily_sedentary_minutes = if_else(daily_sedentary_minutes > 1440, NA_real_, as.numeric(daily_sedentary_minutes)),
    daily_sedentary_hours   = daily_sedentary_minutes / 60
  )

names(data_merge)
