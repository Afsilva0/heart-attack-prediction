cat("\n=======================================================\n")
cat("  INICIANDO PROYECTO: PREDICCIÓN DE RIESGO CARDIOVASCULAR\n")
cat("=======================================================\n")

cat("\n Ejecutando 01_preparacion_datos.R ...\n")
source("01_preparacion_datos.R")

cat("\n Ejecutando 02_modelos.R ...\n")
source("02_modelos.R")

cat("\n Ejecutando 03_interpretacion_graficas.R ...\n")
source("03_interpretacion_graficas.R")

cat("\n=======================================================\n")
cat("  PROYECTO COMPLETADO EXITOSAMENTE\n")
cat("=======================================================\n")