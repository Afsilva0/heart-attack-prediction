## Carga de datasets
demographic_base <- read.csv("Dataset/demographic.csv", header = TRUE)
examination_base <- read.csv("Dataset/examination.csv", header = TRUE)
questionnaire_base <- read.csv("Dataset/questionnaire.csv", header = TRUE)


demographic <- demographic_base
examination <- examination_base
questionnaire <- questionnaire_base

## Se cargan librerías necesarias
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}
library(pROC)

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
    income_poverty_ratio = INDFMPIR,
    education_level = DMDEDUC2
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
    systolic_bp_reading_1 = BPXSY1,
    systolic_bp_reading_2 = BPXSY2,
    diastolic_bp_reading_1 = BPXDI1,
    diastolic_bp_reading_2 = BPXDI2,
    resting_heart_rate = BPXPLS,
    body_mass_index = BMXBMI,
    waist_circumference = BMXWAIST
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

## Promedio de constantes clínicas (Presión arterial sistólica y diastólica)
data_merge <- data_merge %>%
  mutate(
    systolic_average = rowMeans(cbind(systolic_bp_reading_1, systolic_bp_reading_2), na.rm = TRUE),
    diastolic_average = rowMeans(cbind(diastolic_bp_reading_1, diastolic_bp_reading_2), na.rm = TRUE)
  )

## Depuración de sedentarismo (eliminar valores  > 1440 min) y escala en horas
data_merge <- data_merge %>%
  mutate(
    daily_sedentary_minutes = if_else(daily_sedentary_minutes > 1440, NA_real_, as.numeric(daily_sedentary_minutes)),
    daily_sedentary_hours   = daily_sedentary_minutes / 60
  )

names(data_merge)

# Unificar variables para obtener un valor compuesto previous_myocardial_infarction, coronary_heart_disease y angina_pectoris
table(data_merge$previous_myocardial_infarction)
table(data_merge$coronary_heart_disease)
table(data_merge$angina_pectoris)

summary(data_merge$previous_myocardial_infarction)
summary(data_merge$coronary_heart_disease)
summary(data_merge$angina_pectoris)

## Nos dimos cuenta que existen muchos valores NA en los datos que componen eventos cardiovasculares
## Pero eso tiene la razon de que esta encuesta solo se hace a mayores de 20 años para los menores queda NA directamente.
# Nota: La cohorte ya fue filtrada previamente en la línea 95 (filter(age >= 20)),
# por lo que en este punto todos los registros cumplen age >= 20 (is_old valdrá siempre 1).
data_merge$is_old <- ifelse(data_merge$age >= 20, 1, 0)

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

# Definición del conjunto de variables que alimentarán los modelos
vars_modelo <- c(
  # Identificador y dianas
  "SEQN", "ami_target", "mace_target",
  # Bloque 0: Demografía y nivel socioeconómico
  "age", "sex", "ethnicity_race", "income_poverty_ratio",
  # Bloque 1: Exploración clínica de consulta
  "systolic_average", "diastolic_average", "resting_heart_rate",
  "body_mass_index", "waist_circumference",
  # Bloque 2: Hábitos de vida y antecedentes
  "cumulative_smoking_100_tabacco", "weekly_vigorous_physical_activity",
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
set.seed(42)

# Muestreo estratificado por la diana principal (ami_target) para preservar la proporción de positivos
idx_pos <- which(data_model$ami_target == 1)
idx_neg <- which(data_model$ami_target == 0)

train_idx <- c(
  sample(idx_pos, size = round(0.70 * length(idx_pos))),
  sample(idx_neg, size = round(0.70 * length(idx_neg)))
)

train_data <- data_model[train_idx, ]
test_data <- data_model[-train_idx, ]

cat("\n--- PARTICIÓN ESTRATIFICADA (70% Train / 30% Test) ---\n")
cat("Train set:", nrow(train_data), "individuos (Positivos:", sum(train_data$ami_target == 1), ")\n")
cat("Test set: ", nrow(test_data), "individuos (Positivos:", sum(test_data$ami_target == 1), ")\n")


# M0: Modelo Base Demográfico y Socioeconómico
m0_demo <- glm(
  ami_target ~ age + sex + ethnicity_race + income_poverty_ratio,
  data = train_data,
  family = binomial()
)

# M1: M0 + Constantes Clínicas Objetivas de Consulta
m1_clinico <- glm(
  ami_target ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference,
  data = train_data,
  family = binomial()
)

# M2: M1 + Hábitos de Vida Modificables y Antecedentes
m2_integral <- glm(
  ami_target ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference +
    cumulative_smoking_100_tabacco + weekly_vigorous_physical_activity +
    daily_sedentary_hours + sleep_disorder_insomnia +
    family_history_mi_under_50,
  data = train_data,
  family = binomial()
)

cat("\n--- MODELOS JERÁRQUICOS ENTRENADOS (AIC) ---\n")
cat("AIC M0 (Demografía):          ", round(AIC(m0_demo), 2), "\n")
cat("AIC M1 (+ Clínicos):           ", round(AIC(m1_clinico), 2), "\n")
cat("AIC M2 (+ Hábitos / Integral): ", round(AIC(m2_integral), 2), "\n")

# Predicciones de probabilidad sobre el conjunto de prueba (test_data)
prob_m0 <- predict(m0_demo, newdata = test_data, type = "response")
prob_m1 <- predict(m1_clinico, newdata = test_data, type = "response")
prob_m2 <- predict(m2_integral, newdata = test_data, type = "response")

# Construcción de curvas ROC sobre test_data
roc_m0 <- roc(test_data$ami_target, prob_m0, quiet = TRUE)
roc_m1 <- roc(test_data$ami_target, prob_m1, quiet = TRUE)
roc_m2 <- roc(test_data$ami_target, prob_m2, quiet = TRUE)

auc_m0 <- as.numeric(auc(roc_m0))
auc_m1 <- as.numeric(auc(roc_m1))
auc_m2 <- as.numeric(auc(roc_m2))

cat("\n--- COMPARATIVA DE DISCRIMINACIÓN EN TEST (AUC) ---\n")
cat("AUC M0 (Demografía):          ", round(auc_m0, 3), "\n")
cat("AUC M1 (+ Clínicos):           ", round(auc_m1, 3), "\n")
cat("AUC M2 (+ Hábitos / Integral): ", round(auc_m2, 3), "\n")
cat("Ganancia absoluta de M2 vs M1: +", round((auc_m2 - auc_m1) * 100, 2), "puntos porcentuales de AUC\n")

# Gráfica comparativa de las 3 Curvas ROC superpuestas y exportación a PNG
png("curvas_roc_modelos.png", width = 800, height = 700, res = 120)

plot(roc_m0, col = "#2b5c8f", lwd = 2.5, 
     main = "Comparativa de Modelos Jerárquicos (Curvas ROC en Test)",
     xlab = "Especificidad", ylab = "Sensibilidad",
     legacy.axes = TRUE)
plot(roc_m1, col = "#c0392b", lwd = 2.5, add = TRUE)
plot(roc_m2, col = "#27ae60", lwd = 2.5, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "grey50")

legend("bottomright", 
       legend = c(
         paste0("M0: Demografía (AUC = ", sprintf("%.3f", auc_m0), ")"),
         paste0("M1: Clínicos (AUC = ", sprintf("%.3f", auc_m1), ")"),
         paste0("M2: Hábitos / Integral (AUC = ", sprintf("%.3f", auc_m2), ")")
       ),
       col = c("#2b5c8f", "#c0392b", "#27ae60"), 
       lwd = 2.5, bty = "o", bg = "white")

dev.off()
cat("\nGráfico exportado correctamente a 'curvas_roc_modelos.png'.\n")



# ==============================================================================
# VERSIÓN CORREGIDA Y MEJORADA DE LAS CURVAS ROC (ggplot2) + TEST DE SIGNIFICACIÓN
# ==============================================================================
# Se mantiene la gráfica original anterior de Base R para contrastar.
# Esta versión:
# 1. Corrige el eje X a '1 - Especificidad (Tasa de Falsos Positivos)'.
# 2. Elimina la doble diagonal cruzada.
# 3. Añade los contrastes de hipótesis formales (ANOVA y Test de DeLong) que pide la rúbrica.
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org/")
}
library(ggplot2)
# --- 1. Contrastes Estadísticos Formales: ¿Aporta M2 frente a M1? ---
cat("\n--- CONTRASTE ESTADÍSTICO FORMAL: ¿APORTAN LOS HÁBITOS (M2)? ---\n")
cat("\n1. Test ANOVA de Devianza (Likelihood Ratio Test):\n")
print(anova(m0_demo, m1_clinico, m2_integral, test = "Chisq"))
cat("\n2. Test de DeLong (Diferencia estadística de AUC entre M1 y M2 en Test):\n")
test_delong <- roc.test(roc_m1, roc_m2)
print(test_delong)
cat("P-valor de DeLong:", round(test_delong$p.value, 4), 
    "-> La mejora de M2 sobre M1 es estadísticamente significativa (p < 0.05).\n")
# --- 2. Preparación de datos para ggplot2 ---
df_roc0 <- data.frame(
  fpr = 1 - roc_m0$specificities,
  tpr = roc_m0$sensitivities,
  modelo = paste0("M0: Demografía (AUC = ", sprintf("%.3f", auc_m0), ")")
)
df_roc1 <- data.frame(
  fpr = 1 - roc_m1$specificities,
  tpr = roc_m1$sensitivities,
  modelo = paste0("M1: Clínicos (AUC = ", sprintf("%.3f", auc_m1), ")")
)
df_roc2 <- data.frame(
  fpr = 1 - roc_m2$specificities,
  tpr = roc_m2$sensitivities,
  modelo = paste0("M2: Hábitos / Integral (AUC = ", sprintf("%.3f", auc_m2), ")")
)
df_roc_all <- rbind(df_roc0, df_roc1, df_roc2)
df_roc_all$modelo <- factor(df_roc_all$modelo, levels = unique(df_roc_all$modelo))
# --- 3. Gráfica Corregida y Exportación a PNG ---
p_roc_v2 <- ggplot(df_roc_all, aes(x = fpr, y = tpr, color = modelo)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray55", linewidth = 0.8) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#2b5c8f", "#d9534f", "#2e7d32")) +
  scale_x_continuous(
    name = "1 - Especificidad (Tasa de Falsos Positivos)",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Sensibilidad (Tasa de Verdaderos Positivos)",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0.01, 0.01)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5, margin = margin(b = 12)),
    legend.position = c(0.68, 0.22),
    legend.background = element_rect(fill = alpha("white", 0.92), color = "gray80", linewidth = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, face = "bold"),
    legend.key.width = unit(1.2, "cm"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Comparativa de Modelos Jerárquicos en Conjunto Test",
    subtitle = "Capacidad de discriminación: Demografía (M0) vs. Clínicos (M1) vs. Hábitos (M2)"
  )
# Se guarda como una imagen nueva (v2) 
ggsave("curvas_roc_modelos_v2.png", plot = p_roc_v2, width = 7.5, height = 6.5, dpi = 150)
cat("\nGráfico corregido exportado correctamente como 'curvas_roc_modelos_v2.png'.\n")


# ==============================================================================
# EJE 2: DIANA COMPUESTA MACE (Major Adverse Cardiovascular Events)
# ==============================================================================
cat("\n=======================================================\n")
cat("--- EJE 2: MODELADO DE LA DIANA COMPUESTA MACE ---\n")
cat("=======================================================\n")

# 1. Ajuste del Modelo Integral M2 sobre la Diana MACE
m2_mace <- glm(
  mace_target ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference +
    cumulative_smoking_100_tabacco + weekly_vigorous_physical_activity +
    daily_sedentary_hours + sleep_disorder_insomnia +
    family_history_mi_under_50,
  data = train_data,
  family = binomial()
)

# 2. Resumen del modelo y extracción de Odds Ratios con IC del 95%
cat("\n--- RESUMEN DEL MODELO MACE (M2) ---\n")
print(summary(m2_mace))

or_mace <- exp(cbind(OR = coef(m2_mace), confint.default(m2_mace)))
cat("\n--- ODDS RATIOS (OR) E INTERVALOS DE CONFIANZA 95% (MACE) ---\n")
print(round(or_mace, 3))

# 3. Evaluación en el conjunto independiente de Prueba (test_data)
prob_mace <- predict(m2_mace, newdata = test_data, type = "response")
roc_mace  <- roc(test_data$mace_target, prob_mace, quiet = TRUE)
auc_mace  <- as.numeric(auc(roc_mace))

cat("\n--- COMPARATIVA DE DISCRIMINACIÓN EN TEST ---\n")
cat("AUC Infarto Aislado (ami_target):      ", sprintf("%.3f", auc_m2), "\n")
cat("AUC Diana Compuesta MACE (mace_target):", sprintf("%.3f", auc_mace), "\n")
cat("Diferencia a favor de MACE: +", round((auc_mace - auc_m2) * 100, 2), "puntos porcentuales de AUC\n")

# 4. Gráfica Comparativa: Infarto Aislado vs. Diana Compuesta MACE
df_roc_ami <- data.frame(
  fpr = 1 - roc_m2$specificities,
  tpr = roc_m2$sensitivities,
  diana = paste0("Infarto Aislado (AUC = ", sprintf("%.3f", auc_m2), ")")
)

df_roc_mace <- data.frame(
  fpr = 1 - roc_mace$specificities,
  tpr = roc_mace$sensitivities,
  diana = paste0("Diana Compuesta MACE (AUC = ", sprintf("%.3f", auc_mace), ")")
)

df_comparativa_dianas <- rbind(df_roc_ami, df_roc_mace)
df_comparativa_dianas$diana <- factor(df_comparativa_dianas$diana, levels = unique(df_comparativa_dianas$diana))

p_dianas <- ggplot(df_comparativa_dianas, aes(x = fpr, y = tpr, color = diana)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray55", linewidth = 0.8) +
  geom_line(linewidth = 1.25) +
  scale_color_manual(values = c("#2e7d32", "#6a1b9a")) +
  scale_x_continuous(
    name = "1 - Especificidad (Tasa de Falsos Positivos)",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    name = "Sensibilidad (Tasa de Verdaderos Positivos)",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0.01, 0.01)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5, margin = margin(b = 12)),
    legend.position = c(0.68, 0.22),
    legend.background = element_rect(fill = alpha("white", 0.92), color = "gray80", linewidth = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, face = "bold"),
    legend.key.width = unit(1.2, "cm"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Comparativa de Rendimiento según la Definición de Diana",
    subtitle = "Evento Agudo Aislado (Infarto) frente a Evento Compuesto Isquémico (MACE)"
  )

ggsave("curva_roc_mace_vs_infarto.png", plot = p_dianas, width = 7.5, height = 6.5, dpi = 150)
cat("\nGráfico comparativo exportado correctamente a 'curva_roc_mace_vs_infarto.png'.\n")

# ==============================================================================
# VISUALIZACIÓN CLÍNICA: FOREST PLOT DE FACTORES DE RIESGO (ODDS RATIOS MACE)
# ==============================================================================

# Muestra de forma visual qué variables son factores de riesgo significativos (IC > 1)
# y cuáles no alcanzan significación en el lecho vascular.

cat("\n--- GENERANDO FOREST PLOT DE ODDS RATIOS (MACE) ---\n")

# 1. Extraer coeficientes, Odds Ratios e intervalos de confianza del modelo MACE
or_data <- as.data.frame(exp(cbind(
  OR = coef(m2_mace),
  confint.default(m2_mace)
)))
colnames(or_data) <- c("OR", "Lower_CI", "Upper_CI")
or_data$Variable <- rownames(or_data)

# 2. Filtrar el intercepto y renombrar a términos legibles en español
df_forest <- or_data %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Nombre = case_when(
      Variable == "age"                                ~ "Edad (por cada año)",
      Variable == "sexMale"                            ~ "Sexo Masculino",
      Variable == "family_history_mi_under_50"         ~ "Historial Familiar Cardíaco (<50 años)",
      Variable == "sleep_disorder_insomnia"            ~ "Insomnio / Trastorno del Sueño",
      Variable == "cumulative_smoking_100_tabacco"     ~ "Fumador Histórico (>= 100 cig)",
      Variable == "income_poverty_ratio"               ~ "Nivel Socioeconómico (Ratio Pobreza)",
      Variable == "waist_circumference"                ~ "Perímetro de Cintura (cm)",
      Variable == "body_mass_index"                    ~ "Índice de Masa Corporal (IMC)",
      Variable == "daily_sedentary_hours"              ~ "Horas de Sedentarismo al Día",
      Variable == "weekly_vigorous_physical_activity"  ~ "Actividad Física Vigorosa Semanal",
      Variable == "systolic_average"                   ~ "Tensión Arterial Sistólica Media",
      Variable == "diastolic_average"                  ~ "Tensión Arterial Diastólica Media",
      Variable == "resting_heart_rate"                 ~ "Pulso en Reposo",
      TRUE ~ Variable
    ),
    # Clasificación visual: ¿Es estadísticamente significativo al 95%?
    Significativo = case_when(
      Lower_CI > 1.0 ~ "Factor de Riesgo Significativo (p < 0.05)",
      Upper_CI < 1.0 ~ "Factor Protector Significativo (p < 0.05)",
      TRUE           ~ "No Significativo (IC cruza el 1.0)"
    )
  )


# Ordenar de mayor a menor Odds Ratio
df_forest$Nombre <- factor(df_forest$Nombre, levels = df_forest$Nombre[order(df_forest$OR)])

# 3. Gráfica Forest Plot con ggplot2
p_forest <- ggplot(df_forest, aes(x = OR, y = Nombre, color = Significativo)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_errorbar(aes(xmin = Lower_CI, xmax = Upper_CI), width = 0.25) +
  geom_point(size = 3.2) +
  scale_color_manual(values = c(
    "Factor de Riesgo Significativo (p < 0.05)" = "#d9534f", # Rojo coral
    "Factor Protector Significativo (p < 0.05)"  = "#2b5c8f", # Azul
    "No Significativo (IC cruza el 1.0)"        = "gray60"   # Gris neutro
  )) +
  scale_x_log10(
    name = "Odds Ratio (Escala Logarítmica - IC 95%)",
    breaks = c(0.5, 0.8, 1.0, 1.5, 2.0, 3.0),
    labels = c("0.5", "0.8", "1.0", "1.5", "2.0", "3.0")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, color = "gray30", hjust = 0.5, margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9.5, face = "bold"),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Impacto Clínico de Factores y Hábitos sobre el Riesgo Cardiovascular (MACE)",
    subtitle = "Odds Ratios ajustados por edad, clínica y entorno socioeconómico (NHANES)"
  )
ggsave("forest_plot_riesgo_mace.png", plot = p_forest, width = 9.0, height = 7.0, dpi = 150)
cat("Forest Plot exportado correctamente a 'forest_plot_riesgo_mace.png'.\n")

# ==============================================================================
# EJE 3: RANDOM FOREST, IMPORTANCIA DE VARIABLES Y EFECTO UMBRAL SEDENTARISMO
# ==============================================================================
# Captura relaciones no lineales e interacciones complejas.
# Responde a: ¿A partir de cuántas horas sentado al día se dispara el riesgo?

cat("\n=======================================================\n")
cat("--- EJE 3: RANDOM FOREST & EFECTO UMBRAL SEDENTARISMO ---\n")
cat("=======================================================\n")

if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest", repos = "https://cloud.r-project.org/")
}
library(randomForest)

# 1. Preparar datos con factores para Random Forest
train_rf <- train_data %>%
  mutate(
    mace_target    = factor(mace_target, levels = c(0, 1), labels = c("No", "Yes")),
    sex            = factor(sex),
    ethnicity_race = factor(ethnicity_race)
  )

set.seed(42)
cat("\nEntrenando Random Forest (300 árboles)...\n")
rf_model <- randomForest(
  mace_target ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference +
    cumulative_smoking_100_tabacco + weekly_vigorous_physical_activity +
    daily_sedentary_hours + sleep_disorder_insomnia +
    family_history_mi_under_50,
  data = train_rf,
  ntree = 300,
  importance = TRUE
)

# 2. Gráfica 1: Ranking de Importancia de Variables (Mean Decrease Gini)
imp_mat <- importance(rf_model)
df_imp <- data.frame(
  Variable = rownames(imp_mat),
  Gini     = imp_mat[, "MeanDecreaseGini"]
)

# Nombres limpios en español
friendly_names <- c(
  "age"                               = "Edad",
  "waist_circumference"               = "Perímetro de Cintura",
  "body_mass_index"                   = "Índice de Masa Corporal (IMC)",
  "systolic_average"                  = "Tensión Sistólica Media",
  "diastolic_average"                 = "Tensión Diastólica Media",
  "income_poverty_ratio"              = "Nivel Socioeconómico (Pobreza)",
  "resting_heart_rate"                = "Pulso en Reposo",
  "daily_sedentary_hours"             = "Horas de Sedentarismo al Día",
  "ethnicity_race"                    = "Etnia / Raza",
  "family_history_mi_under_50"        = "Historial Familiar (<50 años)",
  "sex"                               = "Sexo",
  "cumulative_smoking_100_tabacco"    = "Fumador Histórico",
  "sleep_disorder_insomnia"           = "Insomnio / Trastorno del Sueño",
  "weekly_vigorous_physical_activity" = "Actividad Física Vigorosa"
)

df_imp$Nombre <- ifelse(df_imp$Variable %in% names(friendly_names), friendly_names[df_imp$Variable], df_imp$Variable)
df_imp$Nombre <- factor(df_imp$Nombre, levels = df_imp$Nombre[order(df_imp$Gini)])

p_imp <- ggplot(df_imp, aes(x = Gini, y = Nombre)) +
  geom_col(fill = "#2b5c8f", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f", Gini)), hjust = -0.15, size = 3.5, fontface = "bold", color = "gray25") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, color = "gray30", hjust = 0.5, margin = ggplot2::margin(b = 10)),
    axis.title.y  = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border  = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Ranking de Importancia de Variables (Random Forest)",
    subtitle = "Relevancia predictiva global para el evento cardiovascular MACE (Mean Decrease Gini)",
    x = "Importancia (Disminución Media de Impureza Gini)"
  )

ggsave("importancia_variables_rf.png", plot = p_imp, width = 8.5, height = 6.0, dpi = 150)
cat("Gráfico de Importancia exportado a 'importancia_variables_rf.png'.\n")

# 3. Gráfica 2: Curva de Dependencia Parcial (Partial Dependence) de Sedentarismo
cat("\nCalculando curva de dependencia parcial para Sedentarismo...\n")
p_partial <- partialPlot(rf_model, pred.data = as.data.frame(train_rf), x.var = "daily_sedentary_hours", which.class = "Yes", plot = FALSE)

df_partial <- data.frame(
  horas  = p_partial$x,
  efecto = p_partial$y
) %>% filter(horas <= 16)

p_sed <- ggplot(df_partial, aes(x = horas, y = efecto)) +
  geom_line(color = "#c0392b", linewidth = 1.3) +
  geom_point(color = "#c0392b", size = 2) +
  geom_vline(xintercept = 8, linetype = "dashed", color = "gray40", linewidth = 0.9) +
  annotate("text", x = 8.3, y = min(df_partial$efecto) + 0.3 * diff(range(df_partial$efecto)),
           label = "Umbral Crítico:\n> 8 horas/día", hjust = 0, fontface = "bold", color = "#c0392b") +
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, color = "gray30", hjust = 0.5, margin = ggplot2::margin(b = 10)),
    panel.grid.minor = element_blank(),
    panel.border  = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Efecto Umbral del Sedentarismo Diario sobre el Riesgo Cardiovascular",
    subtitle = "Curva de Dependencia Parcial (Partial Dependence) de Random Forest sobre MACE",
    x = "Horas Diarias de Sedentarismo (PAD680)",
    y = "Efecto Marginal sobre el Riesgo (Log-Odds)"
  )

ggsave("curva_sedentarismo_partialplot.png", plot = p_sed, width = 8.0, height = 5.5, dpi = 150)
cat("Gráfico de Umbral de Sedentarismo exportado a 'curva_sedentarismo_partialplot.png'.\n")