## ==============================================================================
# ANÁLISIS EXPLORATORIO DE DATOS (EDA) Y SANEAMIENTO PREVIO
## ==============================================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", repos = "https://cloud.r-project.org/")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", repos = "https://cloud.r-project.org/")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr", repos = "https://cloud.r-project.org/")

library(ggplot2)
library(dplyr)
library(tidyr)

cat("\n=======================================================\n")
cat("--- ANÁLISIS EXPLORATORIO DE DATOS ---\n")
cat("=======================================================\n")

# ------------------------------------------------------------------------------
# 1. FIGURA EDA 1: Demostración formal de Skip Patterns y Calidad del Dato
# ------------------------------------------------------------------------------
cat("\n1. Generando gráfico de saneamiento y Skip Patterns...\n")

# Reutilizar las tablas originales cargadas por preparacion_datos.R.
raw_merge_all <- demographic_base %>%
  inner_join(examination_base, by = "SEQN") %>%
  inner_join(questionnaire_base, by = "SEQN")

raw_merge_adults <- raw_merge_all %>% filter(RIDAGEYR >= 20)

items_check <- data.frame(
  cod = c("MCQ160E", "MCQ160C", "MCQ160D", "SMQ020", "SLQ050", "PAQ650", "PAD680", "BPXSY1"),
  etiqueta = c(
    "Infarto de miocardio (MCQ160E)",
    "Enfermedad coronaria (MCQ160C)",
    "Angina de pecho (MCQ160D)",
    "Tabaquismo >= 100 cig (SMQ020)",
    "Trastorno del sueño / Insomnio (SLQ050)",
    "Actividad física vigorosa (PAQ650)",
    "Horas de sedentarismo (PAD680)",
    "Tensión arterial sistólica (BPXSY1)"
  ),
  stringsAsFactors = FALSE
)

calc_pct_missing <- function(df, vars) {
  sapply(vars, function(v) {
    mean(is.na(df[[v]]) | df[[v]] %in% c(7, 9)) * 100
  })
}

pct_na_all <- calc_pct_missing(raw_merge_all, items_check$cod)
pct_na_adults <- calc_pct_missing(raw_merge_adults, items_check$cod)

df_eda_na <- data.frame(
  Variable = rep(items_check$etiqueta, 2),
  Poblacion = rep(c(
    "1. Cohorte Total Bruta (N = 9.813)\n[Incluye menores de edad]",
    "2. Cohorte Adulta Filtrada (N = 5.588)\n[Criterio de inclusión: Edad >= 20 años]"
  ), each = nrow(items_check)),
  Pct_NA = c(pct_na_all, pct_na_adults),
  stringsAsFactors = FALSE
)

df_eda_na$Variable <- factor(df_eda_na$Variable, levels = rev(items_check$etiqueta))

p_eda1 <- ggplot(df_eda_na, aes(x = Pct_NA, y = Variable, fill = Poblacion)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f%%", Pct_NA)),
    position = position_dodge(width = 0.75),
    hjust = -0.15, size = 3.2, fontface = "bold", color = "gray25"
  ) +
  scale_fill_manual(values = c(
    "1. Cohorte Total Bruta (N = 9.813)\n[Incluye menores de edad]" = "#c0392b",
    "2. Cohorte Adulta Filtrada (N = 5.588)\n[Criterio de inclusión: Edad >= 20 años]" = "#27ae60"
  )) +
  scale_x_continuous(limits = c(0, 52), breaks = seq(0, 50, 10), expand = expansion(mult = c(0, 0.08))) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12.5, hjust = 0.5),
    plot.subtitle = element_text(size = 9.5, color = "gray30", hjust = 0.5, margin = ggplot2::margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 9.2),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold", size = 10.5, margin = ggplot2::margin(t = 6)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Saneamiento de Datos: Resolución de Skip Patterns Mediante el Filtro Adulto",
    subtitle = "Porcentaje de valores ausentes (NA / No sabe / Rechaza) antes y después del filtro de inclusión (Edad >= 20)",
    x = "Porcentaje de Registros Ausentes o No Aplicables (%)"
  )

ggsave("eda_datos_ausentes_filtro_edad.png", plot = p_eda1, width = 9.5, height = 5.8, dpi = 150)
cat("Gráfico de saneamiento exportado a 'eda_datos_ausentes_filtro_edad.png'.\n")


# ------------------------------------------------------------------------------
# 2. FIGURA EDA 2: Correlograma de Variables Cuantitativas y Clínicas
# ------------------------------------------------------------------------------
cat("\n2. Generando matriz de correlaciones clínicas...\n")

vars_num <- data_model %>%
  select(
    age, income_poverty_ratio, systolic_average, diastolic_average,
    resting_heart_rate, body_mass_index, waist_circumference, daily_sedentary_hours
  )

num_names <- c(
  "age"                   = "Edad",
  "income_poverty_ratio"  = "Ratio Pobreza",
  "systolic_average"      = "Tensión Sistólica",
  "diastolic_average"     = "Tensión Diastólica",
  "resting_heart_rate"    = "Pulso en Reposo",
  "body_mass_index"       = "IMC",
  "waist_circumference"   = "Perím. Cintura",
  "daily_sedentary_hours" = "Horas Sedentarismo"
)

colnames(vars_num) <- num_names[colnames(vars_num)]
cor_mat <- cor(vars_num, use = "complete.obs", method = "pearson")

cor_df <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Var1", "Var2", "Corr")

var_order_labels <- unname(num_names)
cor_df$Var1 <- factor(cor_df$Var1, levels = var_order_labels)
cor_df$Var2 <- factor(cor_df$Var2, levels = rev(var_order_labels))

p_eda2 <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Corr)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Corr)),
    size = 3.6, fontface = "bold",
    color = ifelse(abs(cor_df$Corr) > 0.45, "white", "gray20")
  ) +
  scale_fill_gradient2(
    low = "#2980b9", mid = "#f8f9fa", high = "#c0392b",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlación\nde Pearson (r)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13.5, hjust = 0.5),
    plot.subtitle = element_text(size = 9.8, color = "gray30", hjust = 0.5, margin = ggplot2::margin(b = 10)),
    axis.text.x   = element_text(angle = 35, hjust = 1, face = "bold", size = 9.5),
    axis.text.y   = element_text(face = "bold", size = 9.5),
    axis.title    = element_blank(),
    panel.grid    = element_blank(),
    panel.border  = element_rect(color = "gray75", fill = NA, linewidth = 0.8),
    legend.title  = element_text(size = 9.5, face = "bold"),
    legend.text   = element_text(size = 8.5)
  ) +
  labs(
    title = "Matriz de Correlación Lineal entre Variables Cuantitativas",
    subtitle = "Inspección de multicolinealidad clínica (IMC vs Cintura) y linealidad con hábitos de vida"
  )

ggsave("eda_correlaciones_clinicas.png", plot = p_eda2, width = 8.2, height = 6.4, dpi = 150)
cat("Gráfico de correlación exportado a 'eda_correlaciones_clinicas.png'.\n")


# ------------------------------------------------------------------------------
# 3. FIGURA EDA 3: Pirámide Demográfica y Prevalencia MACE por Sexo y Edad
# ------------------------------------------------------------------------------
cat("\n3. Generando pirámide demográfica y prevalencia MACE...\n")

df_piramide <- data_model %>%
  mutate(
    grupo_edad = cut(age,
      breaks = c(19, 29, 39, 49, 59, 69, 85),
      labels = c("20-29", "30-39", "40-49", "50-59", "60-69", ">= 70 años")
    ),
    sexo = ifelse(sex == "Male", "Hombres", "Mujeres"),
    estado = ifelse(mace_target == 1, "Evento Cardiovascular (MACE)", "Sin Evento (Sano)")
  ) %>%
  count(grupo_edad, sexo, estado) %>%
  group_by(grupo_edad, sexo) %>%
  mutate(total_grupo = sum(n)) %>%
  ungroup() %>%
  mutate(
    # Para la pirámide: Hombres hacia la izquierda (negativo), Mujeres hacia la derecha (positivo)
    n_plot = ifelse(sexo == "Hombres", -n, n)
  )

df_tasas_mace <- data_model %>%
  mutate(
    grupo_edad = cut(age,
      breaks = c(19, 29, 39, 49, 59, 69, 85),
      labels = c("20-29", "30-39", "40-49", "50-59", "60-69", ">= 70 años")
    ),
    sexo = ifelse(sex == "Male", "Hombres", "Mujeres")
  ) %>%
  group_by(grupo_edad, sexo) %>%
  summarise(
    pct_mace = round(100 * mean(mace_target == 1), 1),
    casos = sum(mace_target == 1),
    total = n(),
    .groups = "drop"
  ) %>%
  mutate(
    x_pos = ifelse(sexo == "Hombres", -525, 525),
    label_text = paste0(pct_mace, "% MACE")
  )

p_eda3 <- ggplot() +
  geom_col(
    data = df_piramide, aes(x = n_plot, y = grupo_edad, fill = interaction(sexo, estado)),
    width = 0.68, color = "white", linewidth = 0.3
  ) +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.8) +
  geom_text(
    data = df_tasas_mace, aes(x = x_pos, y = grupo_edad, label = label_text, color = sexo),
    size = 3.3, fontface = "bold"
  ) +
  scale_fill_manual(
    name = "Perfil Poblacional y Diagnóstico:",
    values = c(
      "Hombres.Sin Evento (Sano)" = "#7fb3d5",
      "Hombres.Evento Cardiovascular (MACE)" = "#1b4f72",
      "Mujeres.Sin Evento (Sano)" = "#f1948a",
      "Mujeres.Evento Cardiovascular (MACE)" = "#78281f"
    ),
    labels = c(
      "Hombres.Sin Evento (Sano)" = "Hombres - Sanos",
      "Hombres.Evento Cardiovascular (MACE)" = "Hombres - Con MACE",
      "Mujeres.Sin Evento (Sano)" = "Mujeres - Sanas",
      "Mujeres.Evento Cardiovascular (MACE)" = "Mujeres - Con MACE"
    )
  ) +
  scale_color_manual(values = c("Hombres" = "#1b4f72", "Mujeres" = "#78281f"), guide = "none") +
  scale_x_continuous(
    name = "<- Número de Hombres en la Cohorte  |  Número de Mujeres en la Cohorte ->",
    breaks = seq(-500, 500, 100),
    labels = abs(seq(-500, 500, 100)),
    limits = c(-600, 600)
  ) +
  scale_y_discrete(name = "Grupo de Edad") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13.5, hjust = 0.5),
    plot.subtitle = element_text(size = 9.8, color = "gray30", hjust = 0.5, margin = ggplot2::margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 9.5, face = "bold"),
    legend.text = element_text(size = 9.0),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray75", fill = NA, linewidth = 0.8)
  ) +
  labs(
    title = "Radiografía Demográfica y Prevalencia de MACE por Sexo y Edad",
    subtitle = "Cohorte analítica final (N = 4.597): Distribución poblacional y tasa de eventos cardiovasculares"
  )

ggsave("eda_piramide_edad_mace.png", plot = p_eda3, width = 9.5, height = 6.0, dpi = 150)
cat("Gráfico de pirámide demográfica exportado a 'eda_piramide_edad_mace.png'.\n")

cat("\n--- analisis_exploratorio_eda.R finalizado correctamente ---\n")
