## ==============================================================================
# Ajuste de modelos predictivos (Regresión Logística y Random Forest)
# Deja disponibles: m0_mace, m1_mace, m2_mace, m2_ami, rf_model,
#                   prob_*, roc_*, auc_*, tabla_metricas_test
## ==============================================================================

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC", repos = "https://cloud.r-project.org/")
}
suppressPackageStartupMessages(library(pROC))

if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest", repos = "https://cloud.r-project.org/")
}
suppressPackageStartupMessages(library(randomForest))

if (!requireNamespace("nnet", quietly = TRUE)) {
  install.packages("nnet", repos = "https://cloud.r-project.org/")
}
suppressPackageStartupMessages(library(nnet))

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
    smoking_status + weekly_vigorous_physical_activity +
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
    smoking_status + weekly_vigorous_physical_activity +
    daily_sedentary_hours + sleep_disorder_insomnia +
    family_history_mi_under_50,
  data = train_data,
  family = binomial()
)

# ==============================================================================
# RANDOM FOREST CON BALANCEO ESTRATIFICADO EN TRAIN (Diana MACE)
# ==============================================================================

# Preparar conjunto de entrenamiento con factores adecuados
train_rf <- as.data.frame(train_data) %>%
  mutate(
    mace_target    = factor(mace_target, levels = c(0, 1), labels = c("No", "Yes")),
    sex            = factor(sex),
    ethnicity_race = factor(ethnicity_race),
    smoking_status = factor(smoking_status)
  )

# Número de casos positivos en entrenamiento para muestreo balanceado por árbol
n_pos_rf <- sum(train_rf$mace_target == "Yes")

set.seed(654)
cat("\nEntrenando Random Forest con submuestreo balanceado por árbol (300 árboles)...\n")
rf_model <- randomForest(
  mace_target ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference +
    smoking_status + weekly_vigorous_physical_activity +
    daily_sedentary_hours + sleep_disorder_insomnia +
    family_history_mi_under_50,
  data = train_rf,
  ntree = 300,
  strata = train_rf$mace_target,
  sampsize = c(n_pos_rf, n_pos_rf),
  importance = TRUE
)

cat("Matriz de confusión OOB de Random Forest (Balanceado):\n")
print(rf_model$confusion)

# ==============================================================================
# PREDICCIONES Y OBJETOS ROC / AUC EN CONJUNTO DE PRUEBA INDEPENDIENTE
# ==============================================================================

# Predicciones modelos logísticos MACE
prob_m0_mace <- predict(m0_mace, newdata = test_data, type = "response")
prob_m1_mace <- predict(m1_mace, newdata = test_data, type = "response")
prob_m2_mace <- predict(m2_mace, newdata = test_data, type = "response")

# Curvas ROC y AUC modelos logísticos MACE
roc_m0_mace <- roc(test_data$mace_target, prob_m0_mace, quiet = TRUE)
roc_m1_mace <- roc(test_data$mace_target, prob_m1_mace, quiet = TRUE)
roc_m2_mace <- roc(test_data$mace_target, prob_m2_mace, quiet = TRUE)

auc_m0_mace <- as.numeric(auc(roc_m0_mace))
auc_m1_mace <- as.numeric(auc(roc_m1_mace))
auc_m2_mace <- as.numeric(auc(roc_m2_mace))

# Predicciones y ROC de Random Forest en Test
test_rf <- as.data.frame(test_data) %>%
  mutate(
    mace_target    = factor(mace_target, levels = c(0, 1), labels = c("No", "Yes")),
    sex            = factor(sex, levels = levels(train_rf$sex)),
    ethnicity_race = factor(ethnicity_race, levels = levels(train_rf$ethnicity_race)),
    smoking_status = factor(smoking_status, levels = levels(train_rf$smoking_status))
  )

prob_rf_mace <- predict(rf_model, newdata = test_rf, type = "prob")[, "Yes"]
roc_rf_mace  <- roc(test_data$mace_target, prob_rf_mace, quiet = TRUE)
auc_rf_mace  <- as.numeric(auc(roc_rf_mace))

# Predicciones modelo ami_target (Infarto aislado)
prob_ami <- predict(m2_ami, newdata = test_data, type = "response")
roc_ami  <- roc(test_data$ami_target, prob_ami, quiet = TRUE)
auc_ami  <- as.numeric(auc(roc_ami))

# ==============================================================================
# TABLA COMPARATIVA DE MÉTRICAS EN TEST (SENSIBILIDAD, ESPECIFICIDAD, PRECISIÓN, F1)
# ==============================================================================

calcular_metricas_test <- function(y_real, prob, umbral, nombre_modelo, aic_val = NA) {
  pred <- ifelse(prob >= umbral, 1, 0)
  tp <- sum(y_real == 1 & pred == 1)
  tn <- sum(y_real == 0 & pred == 0)
  fp <- sum(y_real == 0 & pred == 1)
  fn <- sum(y_real == 1 & pred == 0)
  
  sens <- tp / (tp + fn)
  espec <- tn / (tn + fp)
  prec <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  f1   <- ifelse((prec + sens) > 0, 2 * prec * sens / (prec + sens), 0)
  acc  <- (tp + tn) / length(y_real)
  
  data.frame(
    Modelo        = nombre_modelo,
    AIC           = ifelse(is.na(aic_val), "-", sprintf("%.1f", aic_val)),
    AUC           = sprintf("%.3f", as.numeric(auc(roc(y_real, prob, quiet = TRUE)))),
    Umbral_Corte  = sprintf("%.3f", umbral),
    Sensibilidad  = paste0(sprintf("%.1f", sens * 100), "%"),
    Especificidad = paste0(sprintf("%.1f", espec * 100), "%"),
    Precision     = paste0(sprintf("%.1f", prec * 100), "%"),
    F1_Score      = sprintf("%.3f", f1),
    Falsos_Neg    = fn,
    Falsos_Pos    = fp,
    Coste_10a1    = 10 * fn + fp,  # Coste total con la matriz C(FN) = 10 x C(FP)
    stringsAsFactors = FALSE
  )
}

# Umbral de corte elegido SOLO con datos de entrenamiento (punto más cercano a la
# esquina superior izquierda de la ROC). Elegirlo sobre test sería hacer trampa:
# el test debe usarse únicamente para medir, nunca para tomar decisiones.
# - Regresiones logísticas: probabilidades ajustadas en train (fitted).
# - Random Forest: votos OOB (cada paciente solo lo votan árboles que no lo vieron).
umbral_train <- function(y, prob) {
  coords(roc(y, prob, quiet = TRUE), "best", best.method = "closest.topleft")$threshold[1]
}
th_m0 <- umbral_train(train_data$mace_target, fitted(m0_mace))
th_m1 <- umbral_train(train_data$mace_target, fitted(m1_mace))
th_m2 <- umbral_train(train_data$mace_target, fitted(m2_mace))
th_rf <- umbral_train(train_data$mace_target, rf_model$votes[, "Yes"])

tabla_metricas_test <- rbind(
  calcular_metricas_test(test_data$mace_target, prob_m0_mace, th_m0, "M0: Demográfico", AIC(m0_mace)),
  calcular_metricas_test(test_data$mace_target, prob_m1_mace, th_m1, "M1: M0 + Clínicos", AIC(m1_mace)),
  calcular_metricas_test(test_data$mace_target, prob_m2_mace, th_m2, "M2: M1 + Hábitos (Integral)", AIC(m2_mace)),
  calcular_metricas_test(test_data$mace_target, prob_rf_mace, th_rf, "Random Forest (Balanceado)", NA)
)

# Resumen numérico en consola
cat("\n=======================================================\n")
cat("--- RESUMEN COMPARATIVO EN CONJUNTO DE TEST (MACE) ---\n")
cat("=======================================================\n")
cat("AUC M0 (Demografía):          ", sprintf("%.3f", auc_m0_mace), "\n")
cat("AUC M1 (+ Clínicos):           ", sprintf("%.3f", auc_m1_mace), "\n")
cat("AUC M2 (+ Hábitos):            ", sprintf("%.3f", auc_m2_mace), "\n")
cat("AUC Random Forest:             ", sprintf("%.3f", auc_rf_mace), "\n")
cat("AUC M2 (Infarto Aislado - ami):", sprintf("%.3f", auc_ami), "\n")
cat("\nGanancia absoluta de M2 vs M1 (MACE): +", 
    round((auc_m2_mace - auc_m1_mace) * 100, 2), "puntos porcentuales de AUC\n")
cat("Test de DeLong RF vs M2 (p-valor):", round(roc.test(roc_rf_mace, roc_m2_mace)$p.value, 3), "\n")

cat("\n--- TABLA COMPLETA DE RENDIMIENTO CLÍNICO EN TEST ---\n")
print(tabla_metricas_test)


# ==============================================================================
# ESTRATIFICACIÓN DEL RIESGO CARDIOVASCULAR Y ESPECIFICACIÓN DEL TIPO DE EVENTO
# ==============================================================================

# ETAPA 1: Niveles de riesgo a partir de la probabilidad de M2
  #Cortes fijados A PRIORI (nunca ajustados mirando el test):
  #   <5% Bajo  - 5-10% Moderado - 10-20%  Alto - >=20%   Muy alto   
  cortes_riesgo  <- c(0, 0.05, 0.10, 0.20, 1)
niveles_riesgo <- c("Bajo", "Moderado", "Alto", "Muy alto")

asignar_nivel <- function(prob) {
  cut(prob, breaks = cortes_riesgo, labels = niveles_riesgo,
      right = FALSE, include.lowest = TRUE)
}

acciones_nivel <- c(
  "Bajo"     = "Consejo general sobre hábitos de vida",
  "Moderado" = "Revisión en atención primaria (analítica y control de tensión)",
  "Alto"     = "Estudio cardiológico (ECG y prueba de esfuerzo)",
  "Muy alto" = "Derivación prioritaria a cardiología"
)

# ETAPA 2: Modelo multinomial del tipo de evento (entrenado solo con enfermos) 
niveles_tipo <- c("Infarto", "Coronaria", "Angina") # "Infarto" = categoría de referencia
train_pos <- train_data %>%
  filter(mace_target == 1) %>%
  mutate(tipo_evento = factor(tipo_evento, levels = niveles_tipo))
test_pos <- test_data %>%
  filter(mace_target == 1) %>%
  mutate(tipo_evento = factor(tipo_evento, levels = niveles_tipo))

cat("\nEnfermos por tipo de evento en Train:\n")
print(table(train_pos$tipo_evento))
# Comparamos un modelo completo (14 variables) con uno parsimonioso (edad + sexo).
# Solo hay unas 25 anginas en train: con muchas variables el modelo sobreajusta.
# La elección se hace con el AIC en TRAIN (nunca mirando el test).
m_tipo_completo <- multinom(
  tipo_evento ~ age + sex + ethnicity_race + income_poverty_ratio +
    systolic_average + diastolic_average + resting_heart_rate +
    body_mass_index + waist_circumference +
    smoking_status + weekly_vigorous_physical_activity +
    daily_sedentary_hours + sleep_disorder_insomnia +
    family_history_mi_under_50,
  data = train_pos, trace = FALSE
)
m_tipo <- multinom(tipo_evento ~ age + sex, data = train_pos, trace = FALSE)
cat("AIC modelo tipo completo (14 variables):", round(AIC(m_tipo_completo), 1), "\n")
cat("AIC modelo tipo parsimonioso (edad+sexo):", round(AIC(m_tipo), 1), " <- elegido\n")
# Razones de riesgo relativo (RRR) frente a la categoría de referencia (Infarto)
cat("\nRazones de riesgo relativo (exp(coef)) frente a Infarto:\n")
print(round(exp(coef(m_tipo)), 3))
# Evaluación de la Etapa 2 en los enfermos de TEST: ¿sabe distinguir el tipo?
prob_tipo_test_pos <- predict(m_tipo, newdata = test_pos, type = "probs")
auc_tipo_multiclase <- as.numeric(
  multiclass.roc(test_pos$tipo_evento, prob_tipo_test_pos, quiet = TRUE)$auc
)
auc_tipo_ovr <- sapply(niveles_tipo, function(k) {
  as.numeric(auc(roc(as.numeric(test_pos$tipo_evento == k), prob_tipo_test_pos[, k], quiet = TRUE)))
})
cat("\nAUC multiclase (Hand & Till) distinguiendo el tipo entre enfermos de test:",
    sprintf("%.3f", auc_tipo_multiclase), "\n")
cat("AUC uno-contra-resto por tipo:\n")
print(round(auc_tipo_ovr, 3))


# COMBINACIÓN: ficha de riesgo de cada paciente de test

prob_tipo_cond <- predict(m_tipo, newdata = test_data, type = "probs") # P(tipo | MACE)
estratificacion_test <- test_data %>%
  transmute(
    SEQN, age, sex, mace_target, tipo_evento,
    prob_mace      = prob_m2_mace,
    nivel_riesgo   = asignar_nivel(prob_m2_mace),
    accion         = unname(acciones_nivel[as.character(nivel_riesgo)]),
    prob_infarto   = prob_m2_mace * prob_tipo_cond[, "Infarto"],
    prob_coronaria = prob_m2_mace * prob_tipo_cond[, "Coronaria"],
    prob_angina    = prob_m2_mace * prob_tipo_cond[, "Angina"]
  )
# Validación de los niveles en TEST: el riesgo observado debe crecer de nivel en nivel
total_eventos_test <- sum(estratificacion_test$mace_target)
tabla_niveles_riesgo <- estratificacion_test %>%
  group_by(nivel_riesgo, .drop = FALSE) %>%
  summarise(
    Pacientes          = n(),
    Pct_Poblacion      = round(100 * n() / nrow(estratificacion_test), 1),
    Riesgo_Predicho    = round(100 * mean(prob_mace), 1),
    Eventos_Reales     = sum(mace_target),
    Riesgo_Observado   = round(100 * mean(mace_target), 1),
    Pct_Enfermos_Total = round(100 * sum(mace_target) / total_eventos_test, 1),
    Infartos           = sum(tipo_evento == "Infarto"),
    Coronarias         = sum(tipo_evento == "Coronaria"),
    Anginas            = sum(tipo_evento == "Angina"),
    .groups = "drop"
  )
cat("\n--- VALIDACIÓN DE LOS NIVELES DE RIESGO EN TEST (Modelo M2) ---\n")
print(as.data.frame(tabla_niveles_riesgo))
# Capacidad de la probabilidad específica de infarto para detectar infartos en TODO el test
auc_prob_infarto <- as.numeric(auc(roc(
  as.numeric(estratificacion_test$tipo_evento == "Infarto"),
  estratificacion_test$prob_infarto, quiet = TRUE
)))
cat("\nAUC de P(Infarto) detectando infartos en todo el test:", sprintf("%.3f", auc_prob_infarto), "\n")


# Guardar los objetos actualizados para que persistan en el espacio de trabajo
save(
  m0_mace, m1_mace, m2_mace, m2_ami, rf_model,
  prob_m0_mace, prob_m1_mace, prob_m2_mace, prob_rf_mace, prob_ami,
  roc_m0_mace, roc_m1_mace, roc_m2_mace, roc_rf_mace, roc_ami,
  auc_m0_mace, auc_m1_mace, auc_m2_mace, auc_rf_mace, auc_ami,
  tabla_metricas_test,
  th_m0, th_m1, th_m2, th_rf,
  train_data, test_data,
  m_tipo, estratificacion_test, tabla_niveles_riesgo,
  cortes_riesgo, niveles_riesgo, acciones_nivel,
  auc_tipo_multiclase, auc_tipo_ovr, auc_prob_infarto,
  file = "modelos_mace_entrenados.RData"
)

cat("\nModelos y métricas guardados en 'modelos_mace_entrenados.RData'.\n")
cat("--- modelos.R finalizado correctamente ---\n")
