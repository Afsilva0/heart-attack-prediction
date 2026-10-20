## ==============================================================================
# Ajuste de modelos predictivos (regresión logística y Random Forest)
# Deja disponibles: m0_mace, m1_mace, m2_mace, m2_ami, rf_model,
#                   prob_*, roc_*, auc_*
## ==============================================================================

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}
library(pROC)

if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest", repos = "https://cloud.r-project.org/")
}
library(randomForest)

# ==============================================================================
# MODELOS JERÁRQUICOS DE REGRESIÓN LOGÍSTICA (Diana MACE)
# ==============================================================================

# M0: Modelo Base Demográfico y Socioeconómico (Diana MACE)
m0_mace <- glm(
  mace_target ~ age + sex + ethnicity_race + income_poverty_ratio,
  data = train_data,
  family = binomial()
)

# M1: M0 + Constantes Clínicas Objetivas de Consulta (Diana MACE)
m1_mace <- glm(
  mace_target ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference,
  data = train_data,
  family = binomial()
)

# M2: M1 + Hábitos de Vida Modificables y Antecedentes (Diana MACE)
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

# ==============================================================================
# MODELO DE REGRESIÓN LOGÍSTICA (Diana Infarto Aislado - ami_target)
# ==============================================================================

m2_ami <- glm(
  ami_target ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference +
    cumulative_smoking_100_tabacco + weekly_vigorous_physical_activity +
    daily_sedentary_hours + sleep_disorder_insomnia +
    family_history_mi_under_50,
  data = train_data,
  family = binomial()
)

# ==============================================================================
# RANDOM FOREST (Diana MACE)
# ==============================================================================

train_rf <- train_data %>%
  mutate(
    mace_target    = factor(mace_target, levels = c(0, 1), labels = c("No", "Yes")),
    sex            = factor(sex),
    ethnicity_race = factor(ethnicity_race)
  )

set.seed(654)
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

# ==============================================================================
# PREDICCIONES Y OBJETOS ROC / AUC EN CONJUNTO DE PRUEBA
# ==============================================================================

# Predicciones modelos MACE
prob_m0_mace <- predict(m0_mace, newdata = test_data, type = "response")
prob_m1_mace <- predict(m1_mace, newdata = test_data, type = "response")
prob_m2_mace <- predict(m2_mace, newdata = test_data, type = "response")

# Curvas ROC y AUC MACE
roc_m0_mace <- roc(test_data$mace_target, prob_m0_mace, quiet = TRUE)
roc_m1_mace <- roc(test_data$mace_target, prob_m1_mace, quiet = TRUE)
roc_m2_mace <- roc(test_data$mace_target, prob_m2_mace, quiet = TRUE)

auc_m0_mace <- as.numeric(auc(roc_m0_mace))
auc_m1_mace <- as.numeric(auc(roc_m1_mace))
auc_m2_mace <- as.numeric(auc(roc_m2_mace))

# Predicciones modelo ami_target
prob_ami <- predict(m2_ami, newdata = test_data, type = "response")
roc_ami  <- roc(test_data$ami_target, prob_ami, quiet = TRUE)
auc_ami  <- as.numeric(auc(roc_ami))

# Resumen numérico rápido
cat("\n--- MODELOS AJUSTADOS ---\n")
cat("AIC M0 (Demografía - MACE):          ", round(AIC(m0_mace), 2), "\n")
cat("AIC M1 (+ Clínicos - MACE):           ", round(AIC(m1_mace), 2), "\n")
cat("AIC M2 (+ Hábitos - MACE):            ", round(AIC(m2_mace), 2), "\n")
cat("\nAUC en Test:\n")
cat("  M0 (Demografía - MACE):      ", sprintf("%.3f", auc_m0_mace), "\n")
cat("  M1 (+ Clínicos - MACE):      ", sprintf("%.3f", auc_m1_mace), "\n")
cat("  M2 (+ Hábitos - MACE):       ", sprintf("%.3f", auc_m2_mace), "\n")
cat("  M2 (Infarto Aislado - ami):  ", sprintf("%.3f", auc_ami), "\n")
cat("\nGanancia absoluta de M2 vs M1 (MACE): +", 
    round((auc_m2_mace - auc_m1_mace) * 100, 2), "puntos porcentuales de AUC\n")

cat("\n--- 02_modelos.R finalizado correctamente ---\n")