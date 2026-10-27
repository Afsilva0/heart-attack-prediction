# Proyecto de análisis del riesgo cardiovascular

Este directorio contiene el proyecto de R y los datos utilizados para analizar antecedentes cardiovasculares en adultos de NHANES 2013–2014.

## Memoria del proyecto

La documentación formal, con metodología, resultados y figuras, está en [Documentacion-del-Proyecto.md](../Documentacion-del-Proyecto.md).

## Ejecución

Abre `Proyecto predicción de Infarto.Rproj` en RStudio y ejecuta desde esta carpeta:

```r
source("R script prediccion infarto.R")
```

El flujo ejecuta la preparación de datos, el análisis exploratorio, los modelos y la interpretación. Consulta la memoria para conocer las definiciones, limitaciones y resultados.

## Scripts principales

- `preparacion_datos.R`: carga, transforma y particiona los datos.
- `analisis_exploratorio_eda.R`: produce los gráficos exploratorios.
- `modelos.R`: ajusta y evalúa los modelos.
- `interpretacion_graficas.R`: ejecuta contrastes y genera visualizaciones.
