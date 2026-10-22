## ==============================================================================
# Interpretación de resultados, contrastes estadísticos y gráficas
## ==============================================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org/")
}
library(ggplot2)

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

# Versión ggplot2 (corregida)
df_roc0 <- data.frame(
  fpr = 1 - roc_m0_mace$specificities,
  tpr = roc_m0_mace$sensitivities,
  modelo = paste0("M0: Demografía y nivel socioeconómico (AUC = ", sprintf("%.3f", auc_m0_mace), ")")
)
df_roc1 <- data.frame(
  fpr = 1 - roc_m1_mace$specificities,
  tpr = roc_m1_mace$sensitivities,
  modelo = paste0("M1: M0 + variables clínicas (AUC = ", sprintf("%.3f", auc_m1_mace), ")")
)
df_roc2 <- data.frame(
  fpr = 1 - roc_m2_mace$specificities,
  tpr = roc_m2_mace$sensitivities,
  modelo = paste0("M2: M1 + hábitos y antecedentes (AUC = ", sprintf("%.3f", auc_m2_mace), ")")
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
cat("AUC M2 jerárquico (MACE):      ", sprintf("%.3f", auc_m2_mace), "\n")
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
# FOREST PLOT - ODDS RATIOS (Infarto Aislado - ami_target)
# ==============================================================================

cat("\n--- Generando Forest Plot de Odds Ratios (Infarto Aislado) ---\n")

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
    subtitle = "Odds Ratios ajustados por edad, clínica y entorno socioeconómico (NHANES)",
  )

ggsave("forest_plot_riesgo_mace.png", plot = p_forest, width = 9.0, height = 7.0, dpi = 150)
cat("Forest Plot exportado correctamente a 'forest_plot_riesgo_ami.png'.\n")

# ==============================================================================
# RANDOM FOREST - IMPORTANCIA DE VARIABLES Y CURVA DEPENDENCIA PARCIAL
# ==============================================================================

cat("\n=======================================================\n")
cat("--- INTERPRETACIÓN RANDOM FOREST (MACE) ---\n")
cat("=======================================================\n")

# Importancia de variables
imp_mat <- importance(rf_model)
df_imp <- data.frame(
  Variable = rownames(imp_mat),
  Gini     = imp_mat[, "MeanDecreaseGini"]
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

# Curva de Dependencia Parcial de Sedentarismo
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

cat("\n--- interpretacion_graficas.R finalizado correctamente ---\n")
