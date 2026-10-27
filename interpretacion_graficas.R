## ==============================================================================
# Interpretación de resultados, contrastes estadísticos y gráficas
## ==============================================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org/")
}
library(ggplot2)
library(pROC)
library(dplyr)
library(randomForest)

# ==============================================================================
# CONTRASTES ESTADÍSTICOS FORMALES (Diana MACE)
# ==============================================================================

cat("\n=======================================================\n")
cat("--- CONTRASTES ESTADÍSTICOS (Diana MACE) ---\n")
cat("=======================================================\n")

cat("\n1. Test ANOVA de Devianza (Likelihood Ratio Test):\n")
print(anova(m0_mace, m1_mace, m2_mace, test = "Chisq"))

cat("\n2. Test de DeLong (Diferencia estadística de AUC entre M1 y M2 en Test):\n")
test_delong <- roc.test(roc_m1_mace, roc_m2_mace)
print(test_delong)
cat("P-valor de DeLong:", round(test_delong$p.value, 4), 
    "-> La mejora de M2 sobre M1 es", 
    ifelse(test_delong$p.value < 0.05, "estadísticamente significativa (p < 0.05)", "no significativa (p >= 0.05)"), ".\n")

# ==============================================================================
# GRÁFICAS ROC - MODELOS JERÁRQUICOS MACE
# ==============================================================================

cat("\n--- Generando gráficas ROC (Modelos Jerárquicos MACE) ---\n")

# Versión Base R
png("curvas_roc_modelos.png", width = 800, height = 700, res = 120)

plot(roc_m0_mace, col = "#2b5c8f", lwd = 2.5, 
     main = "Comparativa de Modelos Jerárquicos (Curvas ROC en Test - Diana MACE)",
     xlab = "Especificidad", ylab = "Sensibilidad",
     legacy.axes = TRUE)
plot(roc_m1_mace, col = "#c0392b", lwd = 2.5, add = TRUE)
plot(roc_m2_mace, col = "#27ae60", lwd = 2.5, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "grey50")

legend("bottomright", 
       legend = c(
         paste0("M0: Demografía (AUC = ", sprintf("%.3f", auc_m0_mace), ")"),
         paste0("M1: Clínicos (AUC = ", sprintf("%.3f", auc_m1_mace), ")"),
         paste0("M2: Hábitos / Integral (AUC = ", sprintf("%.3f", auc_m2_mace), ")")
       ),
       col = c("#2b5c8f", "#c0392b", "#27ae60"), 
       lwd = 2.5, bty = "o", bg = "white")

dev.off()
cat("Gráfico exportado correctamente a 'curvas_roc_modelos.png'.\n")

# Versión ggplot2 (corregida)
df_roc0 <- data.frame(
  fpr = 1 - roc_m0_mace$specificities,
  tpr = roc_m0_mace$sensitivities,
  modelo = paste0("M0: Demografía (AUC = ", sprintf("%.3f", auc_m0_mace), ")")
)
df_roc1 <- data.frame(
  fpr = 1 - roc_m1_mace$specificities,
  tpr = roc_m1_mace$sensitivities,
  modelo = paste0("M1: Clínicos (AUC = ", sprintf("%.3f", auc_m1_mace), ")")
)
df_roc2 <- data.frame(
  fpr = 1 - roc_m2_mace$specificities,
  tpr = roc_m2_mace$sensitivities,
  modelo = paste0("M2: Hábitos / Integral (AUC = ", sprintf("%.3f", auc_m2_mace), ")")
)
df_roc_all <- rbind(df_roc0, df_roc1, df_roc2)
df_roc_all$modelo <- factor(df_roc_all$modelo, levels = unique(df_roc_all$modelo))

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

ggsave("curvas_roc_modelos_v2.png", plot = p_roc_v2, width = 7.5, height = 6.5, dpi = 150)
cat("Gráfico corregido exportado correctamente como 'curvas_roc_modelos_v2.png'.\n")

# ==============================================================================
# EJE 2: COMPARATIVA INFARTO AISLADO vs MACE (ami_target vs mace_target)
# ==============================================================================

cat("\n=======================================================\n")
cat("--- EJE 2: COMPARATIVA DE DIANAS (ami_target vs MACE) ---\n")
cat("=======================================================\n")

cat("\n--- DISCRIMINACIÓN EN TEST ---\n")
cat("AUC M2 jerárquico (MACE):       ", sprintf("%.3f", auc_m2_mace), "\n")
cat("AUC modelo directo (ami_target): ", sprintf("%.3f", auc_ami), "\n")

# Gráfica Comparativa
df_roc_ami <- data.frame(
  fpr = 1 - roc_ami$specificities,
  tpr = roc_ami$sensitivities,
  diana = paste0("Infarto Aislado - ami_target (AUC = ", sprintf("%.3f", auc_ami), ")")
)

df_roc_mace <- data.frame(
  fpr = 1 - roc_m2_mace$specificities,
  tpr = roc_m2_mace$sensitivities,
  diana = paste0("Diana Compuesta - MACE (AUC = ", sprintf("%.3f", auc_m2_mace), ")")
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
    subtitle = "Evento Agudo Aislado (ami_target) frente a Evento Compuesto Isquémico (MACE)"
  )

ggsave("curva_roc_mace_vs_infarto.png", plot = p_dianas, width = 7.5, height = 6.5, dpi = 150)
cat("Gráfico comparativo exportado correctamente a 'curva_roc_mace_vs_infarto.png'.\n")

# ==============================================================================
# FOREST PLOT - ODDS RATIOS (Diana MACE)
# ==============================================================================

cat("\n--- Generando Forest Plot de Odds Ratios (Diana MACE) ---\n")

or_mace <- exp(cbind(OR = coef(m2_mace), confint.default(m2_mace)))
cat("\n--- ODDS RATIOS (OR) E INTERVALOS DE CONFIANZA 95% (MACE) ---\n")
print(round(or_mace, 3))

or_data <- as.data.frame(or_mace)
colnames(or_data) <- c("OR", "Lower_CI", "Upper_CI")
or_data$Variable <- rownames(or_data)

df_forest <- or_data %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Nombre = case_when(
      Variable == "age"                                                 ~ "Edad (por cada año)",
      Variable == "sexMale"                                             ~ "Sexo Masculino",
      Variable == "family_history_mi_under_50"                           ~ "Historial Familiar Cardíaco (<50 años)",
      Variable == "sleep_disorder_insomnia"                             ~ "Insomnio / Trastorno del Sueño",
      Variable == "cumulative_smoking_100_tabacco"                      ~ "Fumador Histórico (>= 100 cig)",
      Variable == "income_poverty_ratio"                                ~ "Nivel Socioeconómico (Ratio Pobreza)",
      Variable == "waist_circumference"                                 ~ "Perímetro de Cintura (cm)",
      Variable == "body_mass_index"                                     ~ "Índice de Masa Corporal (IMC)",
      Variable == "daily_sedentary_hours"                               ~ "Horas de Sedentarismo al Día",
      Variable == "weekly_vigorous_physical_activity"                   ~ "Actividad Física Vigorosa Semanal",
      Variable == "systolic_average"                                    ~ "Tensión Arterial Sistólica Media",
      Variable == "diastolic_average"                                   ~ "Tensión Arterial Diastólica Media",
      Variable == "resting_heart_rate"                                  ~ "Pulso en Reposo",
      Variable == "ethnicity_raceNon-Hispanic White"                    ~ "Etnia: Blanco no hispano",
      Variable == "ethnicity_raceOther Hispanic"                        ~ "Etnia: Otro hispano",
      Variable == "ethnicity_raceNon-Hispanic Asian"                    ~ "Etnia: Asiático no hispano",
      Variable == "ethnicity_raceNon-Hispanic Black"                    ~ "Etnia: Negro no hispano",
      Variable == "ethnicity_raceOther Race - Including Multi-Racial"   ~ "Etnia: Multirracial / Otra",
      TRUE ~ Variable
    ),
    Significativo = case_when(
      Lower_CI > 1.0 ~ "Factor de Riesgo Significativo (p < 0.05)",
      Upper_CI < 1.0 ~ "Factor Protector Significativo (p < 0.05)",
      TRUE           ~ "No Significativo (IC cruza el 1.0)"
    )
  )

df_forest$Nombre <- factor(df_forest$Nombre, levels = df_forest$Nombre[order(df_forest$OR)])

p_forest <- ggplot(df_forest, aes(x = OR, y = Nombre, color = Significativo)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_errorbar(aes(xmin = Lower_CI, xmax = Upper_CI), width = 0.25) +
  geom_point(size = 3.2) +
  scale_color_manual(values = c(
    "Factor de Riesgo Significativo (p < 0.05)" = "#d9534f",
    "Factor Protector Significativo (p < 0.05)"  = "#2b5c8f",
    "No Significativo (IC cruza el 1.0)"        = "gray60"
  )) +
  scale_x_log10(
    name = "Odds Ratio (Escala Logarítmica - IC 95%)",
    breaks = c(0.5, 0.8, 1.0, 1.5, 2.0, 3.0),
    labels = c("0.5", "0.8", "1.0", "1.5", "2.0", "3.0")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, color = "gray30", hjust = 0.5, margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9.0, face = "bold"),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Impacto Clínico de Factores y Hábitos\nsobre el Riesgo Cardiovascular (MACE)",
    subtitle = "Odds Ratios ajustados por edad, clínica y entorno socioeconómico (NHANES)"
  )

ggsave("forest_plot_riesgo_mace.png", plot = p_forest, width = 9.0, height = 7.0, dpi = 150)
cat("Forest Plot exportado correctamente a 'forest_plot_riesgo_mace.png'.\n")

# ==============================================================================
# RANDOM FOREST - IMPORTANCIA DE VARIABLES Y CURVA DEPENDENCIA PARCIAL
# ==============================================================================

cat("\n=======================================================\n")
cat("--- INTERPRETACIÓN RANDOM FOREST (MACE) ---\n")
cat("=======================================================\n")

# Importancia de variables por Permutación (específica para la clase positiva "Yes")
imp_mat <- importance(rf_model)
df_imp <- data.frame(
  Variable = rownames(imp_mat),
  Importance_Yes = imp_mat[, "Yes"]
)

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
df_imp$Nombre <- factor(df_imp$Nombre, levels = df_imp$Nombre[order(df_imp$Importance_Yes)])

p_imp <- ggplot(df_imp, aes(x = Importance_Yes, y = Nombre)) +
  geom_col(fill = "#2b5c8f", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f", Importance_Yes)), hjust = -0.15, size = 3.5, fontface = "bold", color = "gray25") +
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
    title = "Ranking de Importancia por Permutación (Random Forest)",
    subtitle = "Impacto predictivo directo sobre la detección de casos MACE (Permutation Importance - Clase 'Yes')",
    x = "Importancia (Disminución de Precisión al Permutar en Clase 'Yes')"
  )

ggsave("importancia_variables_rf.png", plot = p_imp, width = 8.5, height = 6.0, dpi = 150)
cat("Gráfico de Importancia exportado a 'importancia_variables_rf.png'.\n")

# Curva de Dependencia Parcial de Sedentarismo
cat("\nCalculando curva de dependencia parcial para Sedentarismo...\n")
p_partial <- partialPlot(rf_model, pred.data = as.data.frame(train_rf), x.var = "daily_sedentary_hours", which.class = "Yes", plot = FALSE)

df_partial <- data.frame(
  horas  = p_partial$x,
  efecto = p_partial$y
) %>% filter(horas <= 16)

min_efecto <- min(df_partial$efecto)
rango_efecto <- diff(range(df_partial$efecto))

p_sed <- ggplot(df_partial, aes(x = horas, y = efecto)) +
  annotate("rect", xmin = 5, xmax = 16, ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "#c0392b") +
  annotate("text",
    x = 5.3, y = min_efecto + 0.18 * rango_efecto,
    label = "Zona de Riesgo Creciente:\nIncremento continuado a partir de ~5h/día",
    hjust = 0, fontface = "bold", size = 3.6, color = "#962d00"
  ) +
  geom_line(color = "#c0392b", linewidth = 1.3) +
  geom_point(color = "#c0392b", size = 2) +
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, color = "gray30", hjust = 0.5, margin = ggplot2::margin(b = 10)),
    panel.grid.minor = element_blank(),
    panel.border  = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Efecto Marginal del Sedentarismo Diario sobre el Riesgo Cardiovascular",
    subtitle = "Curva de Dependencia Parcial (Partial Dependence) de Random Forest sobre MACE",
    x = "Horas Diarias de Sedentarismo (PAD680)",
    y = "Efecto Marginal Centrado (1/2 Log-Odds)"
  )

ggsave("curva_sedentarismo_partialplot.png", plot = p_sed, width = 8.0, height = 5.5, dpi = 150)
cat("Gráfico de Sedentarismo exportado a 'curva_sedentarismo_partialplot.png'.\n")

# ==============================================================================
# AUDITORÍA DE EQUIDAD (FAIRNESS) Y SESGO DE GÉNERO
# ==============================================================================

cat("\n=======================================================\n")
cat("--- AUDITORÍA DE EQUIDAD POR GÉNERO (FAIRNESS) ---\n")
cat("=======================================================\n")

# Evaluar discriminación y calibración separada por sexo en test_data
df_test_fairness <- test_data
df_test_fairness$prob <- prob_m2_mace

# Curvas ROC desagregadas
roc_female <- roc(df_test_fairness$mace_target[df_test_fairness$sex == "Female"],
                  df_test_fairness$prob[df_test_fairness$sex == "Female"],
                  quiet = TRUE
)
roc_male <- roc(df_test_fairness$mace_target[df_test_fairness$sex == "Male"],
                df_test_fairness$prob[df_test_fairness$sex == "Male"],
                quiet = TRUE
)

auc_female <- as.numeric(auc(roc_female))
auc_male <- as.numeric(auc(roc_male))

# Umbral clínico balanceado general (Youden)
coords_youden <- coords(roc_m2_mace, "best", best.method = "closest.topleft")
th_opt <- coords_youden$threshold[1]

# Métricas por subgrupo
df_test_fairness$pred_opt <- ifelse(df_test_fairness$prob >= th_opt, 1, 0)

by_sex <- split(df_test_fairness, df_test_fairness$sex)
fairness_summary <- do.call(rbind, lapply(names(by_sex), function(s) {
  sub <- by_sex[[s]]
  n_total <- nrow(sub)
  n_pos <- sum(sub$mace_target == 1)
  tp <- sum(sub$mace_target == 1 & sub$pred_opt == 1)
  fn <- sum(sub$mace_target == 1 & sub$pred_opt == 0)
  fp <- sum(sub$mace_target == 0 & sub$pred_opt == 1)
  tn <- sum(sub$mace_target == 0 & sub$pred_opt == 0)
  
  data.frame(
    Grupo = ifelse(s == "Female", "Mujeres", "Hombres"),
    N_Total = n_total,
    Eventos = n_pos,
    Prevalencia_pct = round((n_pos / n_total) * 100, 2),
    AUC = round(ifelse(s == "Female", auc_female, auc_male), 3),
    Sensibilidad_pct = round((tp / n_pos) * 100, 1),
    Especificidad_pct = round((tn / (tn + fp)) * 100, 1),
    Tasa_Falsos_Neg_pct = round((fn / n_pos) * 100, 1)
  )
}))

cat("\n--- TABLA DE EQUIDAD Y DISPARIDAD DE GÉNERO (Umbral Youden =", round(th_opt, 3), ") ---\n")
print(fairness_summary)

# Gráfico comparativo de Curvas ROC por Género (ggplot2)
df_roc_fem <- data.frame(
  fpr = 1 - roc_female$specificities,
  tpr = roc_female$sensitivities,
  sexo = paste0("Mujeres (AUC = ", sprintf("%.3f", auc_female), ")")
)
df_roc_masc <- data.frame(
  fpr = 1 - roc_male$specificities,
  tpr = roc_male$sensitivities,
  sexo = paste0("Hombres (AUC = ", sprintf("%.3f", auc_male), ")")
)
df_roc_sexo <- rbind(df_roc_fem, df_roc_masc)
df_roc_sexo$sexo <- factor(df_roc_sexo$sexo, levels = unique(df_roc_sexo$sexo))

p_fairness <- ggplot(df_roc_sexo, aes(x = fpr, y = tpr, color = sexo)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray55", linewidth = 0.8) +
  geom_line(linewidth = 1.25) +
  scale_color_manual(values = c("#8e24aa", "#1976d2")) +
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
    title = "Auditoría de Equidad Algorítmica por Género (Fairness)",
    subtitle = "Capacidad de discriminación del Modelo Integral M2 desagregada por sexo en Test"
  )

ggsave("curvas_roc_fairness_genero.png", plot = p_fairness, width = 7.5, height = 6.5, dpi = 150)
cat("Gráfico de Equidad por Género exportado a 'curvas_roc_fairness_genero.png'.\n")

cat("\n=======================================================\n")
cat("--- EVALUACIÓN SENSIBLE AL COSTE E ISO-COSTE ---\n")
cat("=======================================================\n")

# Parámetros clínicos de la Matriz de Costes:
# Un Falso Negativo (enfermo no detectado) se penaliza 10 veces más que un Falso Positivo (falsa alarma)
coste_fp <- 1
coste_fn <- 10
prev_test <- mean(test_data$mace_target == 1)

# Pendiente matemática de la recta de Iso-Coste
m_iso <- ((1 - prev_test) / prev_test) * (coste_fp / coste_fn)

# Búsqueda de corte óptimo en todas las coordenadas ROC
coords_mace <- coords(roc_m2_mace, "all", ret = c("threshold", "specificity", "sensitivity"))
coords_mace$coste_esperado <- (1 - prev_test) * coste_fp * (1 - coords_mace$specificity) +
  prev_test * coste_fn * (1 - coords_mace$sensitivity)

idx_opt_coste <- which.min(coords_mace$coste_esperado)
opt_coste <- coords_mace[idx_opt_coste, ]

th_opt_coste <- opt_coste$threshold
x_opt <- 1 - opt_coste$specificity
y_opt <- opt_coste$sensitivity
b_iso <- y_opt - m_iso * x_opt

cat("Prevalencia MACE en Test:        ", round(prev_test * 100, 2), "%\n")
cat("Costes relativos:                 C(FP) =", coste_fp, "| C(FN) =", coste_fn, "\n")
cat("Pendiente Recta Iso-Coste (m):   ", round(m_iso, 4), "\n")
cat("Umbral Óptimo Sensible al Coste: ", round(th_opt_coste, 4), "(frente al 0.5 por defecto)\n")
cat("  -> Sensibilidad conseguida:    ", round(y_opt * 100, 1), "%\n")
cat("  -> Especificidad conseguida:   ", round(opt_coste$specificity * 100, 1), "%\n")
cat("  -> Coste esperado mínimo:      ", round(opt_coste$coste_esperado, 4), "\n")

# Gráfica de la Curva ROC con la Recta de Iso-Coste tangente
df_roc_m2_curve <- data.frame(
  fpr = 1 - roc_m2_mace$specificities,
  tpr = roc_m2_mace$sensitivities
)

p_isocost <- ggplot(df_roc_m2_curve, aes(x = fpr, y = tpr)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  geom_line(color = "#27ae60", linewidth = 1.3) +
  geom_abline(intercept = b_iso, slope = m_iso, color = "#d35400", linetype = "longdash", linewidth = 1.1) +
  annotate("point", x = x_opt, y = y_opt, color = "#d35400", size = 4.2) +
  annotate("text",
           x = x_opt + 0.04, y = y_opt - 0.06,
           label = paste0(
             "Punto Óptimo Tangente\nUmbral = ", round(th_opt_coste, 3),
             "\nSens = ", round(y_opt * 100, 1), "%\nEspec = ", round(opt_coste$specificity * 100, 1), "%"
           ),
           hjust = 0, fontface = "bold", size = 3.6, color = "#962d00"
  ) +
  annotate("text",
    x = 0.55, y = 0.82,
    label = paste0("Recta de Iso-Coste (m = ", round(m_iso, 2), ")\nMatriz de Costes: C(FN)/C(FP) = 10"),
    hjust = 0, fontface = "italic", size = 3.4, color = "#d35400"
  ) +
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
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Evaluación Sensible al Coste y Recta de Iso-Coste (M2)",
    subtitle = "Optimización de umbral asistencial penalizando el Falso Negativo (C(FN) = 10 x C(FP))"
  )

ggsave("curva_roc_isocoste_optimo.png", plot = p_isocost, width = 7.5, height = 6.5, dpi = 150)
cat("Gráfico de Recta de Iso-Coste exportado a 'curva_roc_isocoste_optimo.png'.\n")

# ==============================================================================
# ESTRATIFICACIÓN DEL RIESGO Y TIPO DE EVENTO (Gráfica y fichas de paciente)
# ==============================================================================

cat("\n=======================================================\n")
cat("--- ESTRATIFICACIÓN DEL RIESGO CARDIOVASCULAR ---\n")
cat("=======================================================\n")

# Datos para la gráfica: % de pacientes de cada nivel que tienen cada tipo de evento
df_estrat_plot <- estratificacion_test %>%
  filter(tipo_evento != "Sin evento") %>%
  count(nivel_riesgo, tipo_evento, .drop = FALSE) %>%
  left_join(tabla_niveles_riesgo %>% select(nivel_riesgo, Pacientes), by = "nivel_riesgo") %>%
  mutate(
    pct         = 100 * n / Pacientes,
    tipo_evento = factor(tipo_evento,
                         levels = c("Angina", "Coronaria", "Infarto"),
                         labels = c("Angina aislada", "Enfermedad coronaria (sin infarto)", "Infarto de miocardio"))
  )

df_etiquetas <- tabla_niveles_riesgo %>%
  mutate(etiqueta = paste0(Riesgo_Observado, "%\n(", Eventos_Reales, " de ", Pacientes, ")"))

y_max_estrat <- max(c(df_etiquetas$Riesgo_Observado, df_etiquetas$Riesgo_Predicho)) * 1.30

p_estrat <- ggplot() +
  geom_col(data = df_estrat_plot, aes(x = nivel_riesgo, y = pct, fill = tipo_evento),
           width = 0.62, color = "white", linewidth = 0.4) +
  geom_point(data = tabla_niveles_riesgo, aes(x = nivel_riesgo, y = Riesgo_Predicho, shape = "Riesgo medio predicho por M2"),
             size = 4, color = "black", fill = "gold") +
  geom_text(data = df_etiquetas, aes(x = nivel_riesgo, y = pmax(Riesgo_Observado, Riesgo_Predicho), label = etiqueta),
            vjust = -0.5, size = 3.6, fontface = "bold", color = "gray20", lineheight = 0.9) +
  scale_fill_manual(values = c(
    "Angina aislada"                     = "#f5b041",
    "Enfermedad coronaria (sin infarto)" = "#e67e22",
    "Infarto de miocardio"               = "#c0392b"
  )) +
  scale_shape_manual(values = c("Riesgo medio predicho por M2" = 23)) +
  scale_y_continuous(name = "Pacientes con evento cardiovascular real (%)",
                     limits = c(0, y_max_estrat), expand = c(0, 0)) +
  scale_x_discrete(name = "Nivel de riesgo asignado por el modelo M2",
                   labels = c("Bajo\n(< 5%)", "Moderado\n(5-10%)", "Alto\n(10-20%)", "Muy alto\n(>= 20%)")) +
  guides(fill = guide_legend(order = 1, nrow = 1), shape = guide_legend(order = 2)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, color = "gray30", hjust = 0.5, margin = ggplot2::margin(b = 10)),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 9.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Estratificación del Riesgo Cardiovascular y Tipo de Evento",
    subtitle = "Riesgo observado en el conjunto Test por nivel asignado, desglosado por patología"
  )

ggsave("estratificacion_riesgo_tipo_evento.png", plot = p_estrat, width = 8.5, height = 6.5, dpi = 150)
cat("Gráfico de estratificación exportado a 'estratificacion_riesgo_tipo_evento.png'.\n")

# Fichas de ejemplo: el paciente "típico" (mediana de riesgo) de cada nivel
ficha_paciente <- function(fila) {
  cat("\n------------------------------------------------------------\n")
  cat(sprintf("Paciente SEQN %d | %s, %d años\n", fila$SEQN,
              ifelse(fila$sex == "Female", "Mujer", "Hombre"), fila$age))
  cat(sprintf("Riesgo cardiovascular global: %.1f%%  ->  NIVEL %s\n",
              100 * fila$prob_mace, toupper(as.character(fila$nivel_riesgo))))
  cat("Desglose por tipo de evento:\n")
  cat(sprintf("   - Infarto de miocardio:            %.1f%%\n", 100 * fila$prob_infarto))
  cat(sprintf("   - Enfermedad coronaria (sin inf.): %.1f%%\n", 100 * fila$prob_coronaria))
  cat(sprintf("   - Angina aislada:                  %.1f%%\n", 100 * fila$prob_angina))
  cat("Acción recomendada:", fila$accion, "\n")
  cat("Realidad (NHANES):", fila$tipo_evento, "\n")
}

ejemplos <- estratificacion_test %>%
  group_by(nivel_riesgo) %>%
  slice_min(abs(prob_mace - median(prob_mace)), n = 1, with_ties = FALSE) %>%
  ungroup()

cat("\n--- FICHAS DE RIESGO DE EJEMPLO (paciente mediano de cada nivel) ---\n")
for (i in seq_len(nrow(ejemplos))) ficha_paciente(ejemplos[i, ])

cat("\n--- interpretacion_graficas.R finalizado correctamente ---\n")