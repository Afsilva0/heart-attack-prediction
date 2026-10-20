cat("\n=======================================================\n")
cat("  INICIANDO PROYECTO: PREDICCIÓN DE RIESGO CARDIOVASCULAR\n")
cat("=======================================================\n")

cat("\n Ejecutando preparacion_datos.R ...\n")
source("preparacion_datos.R")

cat("\n Ejecutando modelos.R ...\n")
source("modelos.R")

cat("\n Ejecutando interpretacion_graficas.R ...\n")
source("interpretacion_graficas.R")

cat("\n=======================================================\n")
cat("  PROYECTO COMPLETADO EXITOSAMENTE\n")
cat("=======================================================\n")