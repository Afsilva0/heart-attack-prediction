## ==============================================================================
# Carga, limpieza y partición de los datos NHANES para predicción cardiovascular
## ==============================================================================

## Carga de librerías necesarias
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
}
suppressPackageStartupMessages(library(dplyr))

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC", repos = "https://cloud.r-project.org/")
}
suppressPackageStartupMessages(library(pROC))

## Carga de datasets
demographic_base <- read.csv("Dataset/demographic.csv", header = TRUE)
examination_base <- read.csv("Dataset/examination.csv", header = TRUE)
questionnaire_base <- read.csv("Dataset/questionnaire.csv", header = TRUE)

demographic <- demographic_base
examination <- examination_base
questionnaire <- questionnaire_base

## Seleccionar y renombrar campos demographic
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
    income_poverty_ratio = INDFMPIR,
    education_level = DMDEDUC2
  )

## Seleccionar y renombrar campos examination
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
    systolic_bp_reading_1 = BPXSY1,
    systolic_bp_reading_2 = BPXSY2,
    diastolic_bp_reading_1 = BPXDI1,
    diastolic_bp_reading_2 = BPXDI2,
    resting_heart_rate = BPXPLS,
    body_mass_index = BMXBMI,
    waist_circumference = BMXWAIST
  )

## Seleccionar y renombrar campos questionnaire
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

## Transformar información
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
map_yes_no <- c(
  "Yes" = 1,
  "No" = 2
)

getKeyByValue <- function(data, map) {
  names(map)[match(data, map)]
}

data_merge$sex <- getKeyByValue(data_merge$sex, map_sex_type)
data_merge$ethnicity_race <- getKeyByValue(data_merge$ethnicity_race, map_race_type)
data_merge$education_level <- getKeyByValue(data_merge$education_level, map_education_type)

## Recodificar variables binarias de NHANES (1 = Sí -> 1, 2 = No -> 0; 7/9 -> NA)
transform_nhanes_binary <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    TRUE ~ NA_real_
  )
}

data_merge <- data_merge %>%
  mutate(
    previous_myocardial_infarction    = transform_nhanes_binary(previous_myocardial_infarction),
    coronary_heart_disease            = transform_nhanes_binary(coronary_heart_disease),
    angina_pectoris                   = transform_nhanes_binary(angina_pectoris),
    cumulative_smoking_100_tabacco    = transform_nhanes_binary(cumulative_smoking_100_tabacco),
    weekly_vigorous_physical_activity = transform_nhanes_binary(weekly_vigorous_physical_activity),
    sleep_disorder_insomnia           = transform_nhanes_binary(sleep_disorder_insomnia),
    family_history_mi_under_50        = transform_nhanes_binary(family_history_mi_under_50)
  )

## Estatus tabáquico en 3 niveles (estándar epidemiológico CDC / AHA):
# - Nunca fumador: no ha fumado 100 cigarrillos en su vida (cumulative_smoking_100_tabacco == 0)
# - Exfumador: fumó >= 100 cigarrillos pero actualmente ya no fuma nada (current_tobacco_use_frequency == 3)
# - Fumador activo: fumó >= 100 cigarrillos y actualmente fuma a diario u ocasionalmente (current_tobacco_use_frequency in 1, 2)
data_merge <- data_merge %>%
  mutate(
    smoking_status = case_when(
      cumulative_smoking_100_tabacco == 0 ~ "Nunca fumador",
      cumulative_smoking_100_tabacco == 1 & current_tobacco_use_frequency == 3 ~ "Exfumador",
      cumulative_smoking_100_tabacco == 1 & current_tobacco_use_frequency %in% c(1, 2) ~ "Fumador activo",
      TRUE ~ NA_character_
    ),
    smoking_status = factor(smoking_status, levels = c("Nunca fumador", "Exfumador", "Fumador activo"))
  )

## Promedio de constantes clínicas (Presión arterial sistólica y diastólica)
data_merge <- data_merge %>%
  mutate(
    systolic_average = rowMeans(cbind(systolic_bp_reading_1, systolic_bp_reading_2), na.rm = TRUE),
    diastolic_average = rowMeans(cbind(diastolic_bp_reading_1, diastolic_bp_reading_2), na.rm = TRUE)
  )

## Depuración de sedentarismo (eliminar valores > 1440 min) y escala en horas
data_merge <- data_merge %>%
  mutate(
    daily_sedentary_minutes = if_else(daily_sedentary_minutes > 1440, NA_real_, as.numeric(daily_sedentary_minutes)),
    daily_sedentary_hours   = daily_sedentary_minutes / 60
  )

# Unificar variables para obtener un valor compuesto previous_myocardial_infarction, coronary_heart_disease y angina_pectoris

# Construccion de variables diana
# 1. ami_target (Infarto Aislado): Diagnóstico puntual de Infarto Agudo de Miocardio (1 = Sí, 0 = No).
# 2. mace_target (Evento Compuesto MACE): Criterio clínico cardiológico integral.
#    Vale 1 si el paciente ha tenido Infarto, Enfermedad Coronaria O Angina de Pecho.

data_merge <- data_merge %>%
  mutate(
    ami_target = previous_myocardial_infarction,
    mace_target = case_when(
      previous_myocardial_infarction == 1 |
        coronary_heart_disease == 1 |
        angina_pectoris == 1 ~ 1,
      is.na(previous_myocardial_infarction) &
        is.na(coronary_heart_disease) &
        is.na(angina_pectoris) ~ NA_real_,
      TRUE ~ 0
    )
  )

# 3. tipo_evento (Especificación de la patología): categoría ÚNICA y EXCLUYENTE.
#    Un mismo paciente puede declarar varias patologías a la vez (p.ej. infarto + angina),
#    así que se le asigna la más grave según la jerarquía clínica:
#    Infarto  >  Enfermedad coronaria (sin infarto)  >  Angina aislada  >  Sin evento
data_merge <- data_merge %>%
  mutate(
    tipo_evento = case_when(
      previous_myocardial_infarction == 1 ~ "Infarto",
      coronary_heart_disease == 1         ~ "Coronaria",
      angina_pectoris == 1                ~ "Angina",
      mace_target == 0                    ~ "Sin evento",
      TRUE                                ~ NA_character_
    )
  )

# Definición del conjunto de variables que alimentarán los modelos
# Nota metodológica (Cambio 3): 'education_level' (DMDEDUC2) fue preprocesada en el cruce inicial,
# pero se excluye deliberadamente de vars_modelo por principio de parsimonia estadística:
# el nivel socioeconómico queda sólidamente capturado de forma continua por 'income_poverty_ratio'
# (ratio ingresos/pobreza), evitando sobreparametrizar los modelos con 4 variables dummy categóricas.
vars_modelo <- c(
  # Identificador y dianas
  "SEQN", "ami_target", "mace_target", "tipo_evento",
  # Bloque 0: Demografía y nivel socioeconómico
  "age", "sex", "ethnicity_race", "income_poverty_ratio",
  # Bloque 1: Exploración clínica de consulta
  "systolic_average", "diastolic_average", "resting_heart_rate",
  "body_mass_index", "waist_circumference",
  # Bloque 2: Hábitos de vida y antecedentes
  "smoking_status", "weekly_vigorous_physical_activity",
  "daily_sedentary_hours", "sleep_disorder_insomnia", "family_history_mi_under_50"
)

# Filtrado de casos completos en las variables del estudio (eliminando NAs)
data_model <- data_merge %>%
  select(all_of(vars_modelo)) %>%
  na.omit()

cat("\n--- COHORTE ANALÍTICA FINAL ---\n")
cat("Registros completos disponibles para modelado:", nrow(data_model), "\n")
cat(
  "Prevalencia Infarto Aislado (ami_target):",
  round(mean(data_model$ami_target) * 100, 2), "%\n"
)
cat(
  "Prevalencia Evento Compuesto MACE (mace_target):",
  round(mean(data_model$mace_target) * 100, 2), "%\n"
)

# Partición Estratificada 70% Entrenamiento / 30% Prueba
# Fijamos semilla para asegurar la reproducibilidad exacta
set.seed(654)

# Muestreo estratificado por la diana principal (mace_target) para preservar la proporción de positivos
idx_pos <- which(data_model$mace_target == 1)
idx_neg <- which(data_model$mace_target == 0)

train_idx <- c(
  sample(idx_pos, size = round(0.70 * length(idx_pos))),
  sample(idx_neg, size = round(0.70 * length(idx_neg)))
)

train_data <- data_model[train_idx, ]
test_data <- data_model[-train_idx, ]

cat("\n--- PARTICIÓN ESTRATIFICADA (70% Train / 30% Test) ---\n")
cat("Train set:", nrow(train_data), "individuos (Positivos:", sum(train_data$mace_target == 1), ")\n")
cat("Test set: ", nrow(test_data), "individuos (Positivos:", sum(test_data$mace_target == 1), ")\n")

cat("\nDatos preparados y listos. Objetos disponibles: train_data, test_data, data_model, vars_modelo\n")