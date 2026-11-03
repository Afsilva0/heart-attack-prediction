# Análisis del riesgo cardiovascular en adultos
## NHANES 2013–2014 — Memoria del proyecto

**Asignatura:** Data Science (DAS)  
**Integrantes:** Rubén Guaita Daviu, Raúl Hoyos Zulla y Andrés Felipe Silva Guzmán  
**Docente:** pendiente de completar  
**Fecha de entrega:** pendiente de completar

---

> **Delimitación del estudio.** NHANES es una encuesta transversal y las respuestas cardiovasculares analizadas describen diagnósticos declarados con anterioridad. Por tanto, este trabajo clasifica antecedentes registrados en la encuesta; no predice la aparición futura de un infarto ni constituye una herramienta clínica.

## Resumen

Este proyecto compara el rendimiento de modelos que incorporan, de forma progresiva, información demográfica, mediciones clínicas y variables de estilo de vida para clasificar un evento cardiovascular compuesto. Se utilizaron participantes adultos de NHANES 2013–2014 y se analizaron tres ficheros unidos por el identificador `SEQN`.

La cohorte final de casos completos contiene **4.597 participantes**. La prevalencia observada fue del **3,85 %** para el antecedente aislado de infarto y del **6,63 %** para el evento compuesto definido por el proyecto (MACE). En el conjunto de prueba independiente (N = 1.379), el AUC fue **0,804** para M0 (demográfico), **0,815** para M1 (clínico) y **0,829** para M2 (integral con hábitos). La comparación DeLong M0–M2 fue estadísticamente significativa (*p* = 0,0212), mientras que la ganancia incremental estricta de hábitos sobre clínica (M1–M2) mostró una tendencia cercana a la significancia estadística (*p* = 0,0687; con $\Delta\text{Devianza} = 40,704$, *p* < 0,001 en el ANOVA de devianza).

## Índice

1. [Planteamiento y objetivos](#1-planteamiento-y-objetivos)
2. [Datos y metodología](#2-datos-y-metodología)
3. [Resultados](#3-resultados)
4. [Discusión](#4-discusión)
5. [Limitaciones y aspectos metodológicos](#5-limitaciones-y-aspectos-metodológicos)
6. [Conclusiones](#6-conclusiones)
7. [Fuentes y documentación](#7-fuentes-y-documentación)

## 1. Planteamiento y objetivos

### 1.1 Problema y alcance

El proyecto aborda la pregunta de cuánto añade conocer hábitos de vida —sedentarismo, actividad física, tabaquismo y sueño— a la información demográfica y a las mediciones clínicas habituales al clasificar antecedentes cardiovasculares declarados.

Para responderla, se comparan tres modelos anidados: un perfil demográfico y socioeconómico (M0), ese perfil ampliado con mediciones clínicas (M1) y un modelo integral que añade variables de estilo de vida y antecedentes familiares (M2). El objetivo principal del código es `mace_target`, un evento compuesto definido a partir de infarto de miocardio, enfermedad coronaria o angina. También se analiza el antecedente aislado de infarto mediante `ami_target`.

La denominación MACE se utiliza aquí para el compuesto particular del proyecto. No se afirma que coincida con una definición clínica universal de MACE.

### 1.2 Objetivo general

Determinar en qué medida las variables de estilo de vida aportan información adicional sobre la clasificación del antecedente cardiovascular compuesto, frente a la información demográfica y clínica, en participantes adultos de NHANES 2013–2014.

### 1.3 Objetivos específicos

1. Comparar M0, M1 y M2 con curvas ROC, AUC y métricas de clasificación en un conjunto de prueba independiente con umbrales fijados exclusivamente en entrenamiento.
2. Analizar un evento compuesto (MACE) además del antecedente aislado de infarto.
3. Examinar predictores mediante odds ratios, importancia por permutación de Random Forest focalizada en la clase positiva y una curva de dependencia parcial del sedentarismo.
4. Describir diferencias de rendimiento y equidad por sexo (fairness).
5. Explorar un umbral de clasificación sensible al coste que penalice los falsos negativos frente a los falsos positivos mediante tangencia de iso-coste.

### 1.4 Preguntas de investigación

Las preguntas de trabajo incluyen cuánto añaden los hábitos frente a las mediciones clínicas, si se observa un patrón de sedentarismo asociado a mayor salida del modelo, el impacto del hábito tabáquico discriminando fumadores activos de exfumadores (*sick quitter effect*), si el rendimiento difiere por sexo y cómo estratificar clínicamente a los pacientes según su nivel de riesgo.

### 1.5 Utilidad y aportación

La aportación metodológica del trabajo es la comparación progresiva de bloques de información, junto con análisis de discriminación, clasificación, equidad descriptiva y sensibilidad al coste. Por el diseño observacional y transversal, los resultados se interpretan como asociaciones y capacidad de clasificación dentro de esta muestra; no como efectos de intervención ni como recomendaciones médicas.

## 2. Datos y metodología

### 2.1 Fuente de datos

La fuente indicada en los objetivos es NHANES 2013–2014, encuesta de salud y nutrición de Estados Unidos. Los datos se obtuvieron del conjunto disponible en [Kaggle: National Health and Nutrition Examination Survey](https://www.kaggle.com/datasets/cdc/national-health-and-nutrition-examination-survey).

| Fichero             | Variables principales utilizadas                                                                                                                 |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `demographic.csv`   | Identificador, edad, sexo, etnia/raza, ratio ingresos/pobreza y nivel educativo.                                                                 |
| `examination.csv`   | Presión arterial sistólica y diastólica, pulso en reposo, IMC y perímetro de cintura.                                                            |
| `questionnaire.csv` | Diagnósticos cardiovasculares declarados, tabaquismo (acumulado y frecuencia actual), actividad física, sedentarismo, sueño e historia familiar. |

Los ficheros se unen por `SEQN` mediante uniones internas. El análisis incluye personas de 20 años o más.

### 2.2 Preparación de los datos

El flujo de preparación en `preparacion_datos.R` realiza las siguientes operaciones:

1. Selecciona las variables de los tres CSV y las combina por `SEQN`.
2. Filtra la población adulta, con edad igual o superior a 20 años (`RIDAGEYR >= 20`), resolviendo de partida los *skip patterns* protocolarios del CDC.
3. Recodifica sexo y etnia/raza a etiquetas legibles. El nivel educativo (`DMDEDUC2`) se preprocesa inicialmente, pero se excluye por parsimonia estadística de las fórmulas predictivas dado que el nivel socioeconómico queda sólidamente capturado de forma continua por el ratio ingresos/pobreza (`income_poverty_ratio`), evitando añadir 4 variables dummy innecesarias.
4. En las variables binarias seleccionadas, recodifica `1` como sí y `2` como no; los códigos especiales `7` (*Refused*) y `9` (*Don't know*) se convierten en `NA`.
5. Construye el estatus tabáquico en tres niveles (`smoking_status`): *Nunca fumador* (<100 cigarrillos), *Exfumador* ($\ge$100 cigarrillos pero no fuma actualmente) y *Fumador activo* ($\ge$100 cigarrillos y fuma diaria u ocasionalmente), permitiendo aislar el efecto del abandono por enfermedad (*sick quitter effect*).
6. Calcula el promedio de las dos lecturas sistólicas y las dos diastólicas.
7. Convierte a `NA` los valores de sedentarismo mayores de 1440 minutos y expresa los restantes en horas.
8. Construye las variables objetivo y elimina registros incompletos en las variables incluidas en `vars_modelo`.
9. Divide la cohorte en entrenamiento (70 %) y prueba (30 %), estratificada por `mace_target` y con semilla reproducible `654`.

La ejecución consolidó 4.597 casos completos para modelado.

### 2.3 Variables objetivo

- **`ami_target`:** antecedente declarado de infarto agudo de miocardio (`MCQ160E`).
- **`mace_target`:** diana compuesta. Vale 1 si el paciente responde afirmativamente a infarto (`MCQ160E`), enfermedad coronaria (`MCQ160C`) o angina (`MCQ160D`); vale `NA` si las tres respuestas son desconocidas; en los demás casos vale 0.
- **`tipo_evento`:** categoría exclusiva asignada por jerarquía clínica: infarto, enfermedad coronaria (sin infarto), angina aislada y, finalmente, sin evento.

### 2.4 Modelos

Los modelos M0–M2 son regresiones logísticas binomiales para `mace_target`.

| Modelo               | Predictores incluidos en el código                                                                                                                                                        |
| -------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **M0 — Demográfico** | Edad, sexo, etnia/raza y ratio ingresos/pobreza.                                                                                                                                          |
| **M1 — Clínico**     | M0 más presión sistólica y diastólica medias, pulso en reposo, IMC y perímetro de cintura.                                                                                                |
| **M2 — Integral**    | M1 más estatus tabáquico (3 niveles), actividad física vigorosa semanal, horas de sedentarismo diario, trastorno del sueño/insomnio e historial familiar de infarto prematuro (<50 años). |

Además, el proyecto ajusta una regresión logística para `ami_target` con las variables de M2, un Random Forest de 300 árboles para MACE con submuestreo balanceado por clase en entrenamiento (`sampsize = c(214, 214)`) e importancia por permutación, y un modelo multinomial para distinguir el tipo de patología entre participantes con MACE.

### 2.5 Evaluación

En el conjunto de prueba se calculan curvas ROC y AUC. **Los umbrales de decisión se seleccionan estrictamente con datos de entrenamiento** (criterio `closest.topleft` para regresiones y votos OOB para Random Forest), evitando fuga de información (*data leakage*). La tabla de rendimiento incluye sensibilidad, especificidad, precisión, F1, falsos negativos, falsos positivos y coste asistencial ponderado.

El código también evalúa el ANOVA de devianza en entrenamiento y pruebas de DeLong en test: M0 frente a M2, M1 frente a M2 y Random Forest frente a M2.

### 2.6 Implementación y ejecución

El proyecto R se encuentra en `heart-attack-prediction/`. Se abre `Proyecto predicción de Infarto.Rproj` en RStudio y se ejecuta desde la carpeta del proyecto:

```r
source("R script prediccion infarto.R")
```

El flujo verifica e instala dependencias (`dplyr`, `pROC`, `randomForest`, `ggplot2`, `tidyr`, `nnet`), y ejecuta secuencialmente preparación, EDA, modelos e interpretación gráfica.

## 3. Resultados

### 3.1 Cohorte analítica

| Conjunto                    | Participantes | MACE positivos | Prevalencia MACE | Infartos aislados | Prevalencia infarto |
| --------------------------- | ------------: | -------------: | ---------------: | ----------------: | ------------------: |
| Entrenamiento (Train)       |         3.218 |            214 |           6,65 % |               118 |              3,67 % |
| Prueba (Test)               |         1.379 |             91 |           6,60 % |                59 |              4,28 % |
| **Total cohorte analítica** |     **4.597** |        **305** |       **6,63 %** |           **177** |          **3,85 %** |

La prevalencia del evento compuesto MACE (6,63 %) prácticamente duplica a la del infarto agudo aislado (3,85 %), otorgando mayor masa estadística y relevancia clínica al estudio.

### 3.2 Exploración de datos

En el gráfico de datos ausentes, los cuestionarios cardiovasculares presentan alrededor del 43 % de valores ausentes o códigos 7/9 en la cohorte bruta y caen a entre 0,0 % y 0,3 % en la cohorte adulta. El patrón demuestra formalmente la existencia de *skip patterns* protocolarios del CDC para menores de 20 años, justificando el filtrado poblacional.

![Porcentaje de valores ausentes antes y después del filtro de edad](heart-attack-prediction/eda_datos_ausentes_filtro_edad.png)

**Figura 1.** Porcentajes de valores ausentes antes y después del filtro de edad ($\ge$ 20 años).

La matriz de correlación evidencia una colinealidad masiva entre IMC y perímetro de cintura (**r = 0,91**), moderada entre edad y presión sistólica (**r = 0,44**) y entre presiones sistólica y diastólica (**r = 0,34**). Por el contrario, las horas de sedentarismo presentan correlaciones lineales prácticamente nulas con las variables biométricas ($|r| \le 0,09$), sugiriendo la necesidad de explorar relaciones no lineales.

![Matriz de correlación entre variables cuantitativas](heart-attack-prediction/eda_correlaciones_clinicas.png)

**Figura 2.** Matriz de correlación lineal de Pearson entre variables cuantitativas.

La pirámide demográfica muestra un marcado gradiente etario y una clara brecha de género: por debajo de los 40 años el evento es excepcional (<1,9 %), mientras que a partir de los 50 años la prevalencia masculina duplica a la femenina (7,4 % vs. 3,6 % en 50–59 años; 15,9 % vs. 9,0 % en 60–69 años; y 25,4 % vs. 15,7 % en $\ge$ 70 años).

| Grupo de edad | Hombres con MACE | Mujeres con MACE |
| ------------- | ---------------: | ---------------: |
| 20–29         |            0,3 % |            0,0 % |
| 30–39         |            1,8 % |            1,9 % |
| 40–49         |            2,9 % |            2,5 % |
| 50–59         |            7,4 % |            3,6 % |
| 60–69         |           15,9 % |            9,0 % |
| $\ge$ 70      |           25,4 % |           15,7 % |

![Distribución de participantes y frecuencia de MACE por edad y sexo](heart-attack-prediction/eda_piramide_edad_mace.png)

**Figura 3.** Pirámide poblacional y prevalencia de MACE observada por grupo etario y sexo.

### 3.3 Comparación de modelos

Los umbrales de decisión se fijaron en entrenamiento mediante `closest.topleft` (probabilidades ajustadas en regresión logística y votos OOB en Random Forest) y se evaluaron en el conjunto de prueba independiente (1.379 individuos, 91 positivos):

| Modelo                          | AIC (Train) | AUC (Test) | Umbral (Train) | Sensibilidad | Especificidad |  Precisión |  F1-Score | Falsos neg. | Falsos pos. | Coste 15:1 |
| ------------------------------- | ----------: | ---------: | -------------: | -----------: | ------------: | ---------: | --------: | ----------: | ----------: | ---------: |
| **M0: Demográfico**             |      1306,1 |      0,804 |          0,078 |       65,9 % |        75,2 % |     15,8 % |     0,255 |          31 |         320 |        785 |
| **M1: M0 + Clínicos**           |      1285,5 |      0,815 |          0,076 |       68,1 % |        76,1 % |     16,8 % |     0,269 |          29 |         308 |        743 |
| **M2: M1 + Hábitos (Integral)** |  **1256,8** |  **0,829** |          0,066 |   **73,6 %** |        74,2 % | **16,8 %** | **0,273** |      **24** |         332 |    **692** |
| **Random Forest (Balanceado)**  |           — |      0,815 |          0,416 |       68,1 % |        74,5 % |     15,9 % |     0,258 |          29 |         328 |        763 |

El modelo integral M2 presenta el mayor AUC (0,829), el menor AIC (1256,8), la mayor sensibilidad (73,6 %, capturando 67 de los 91 enfermos) y el menor coste total asistencial ponderado (692 penalizando 15:1). Random Forest alcanza un AUC equivalente a M1 (0,815).

![Curvas ROC de M0, M1 y M2 en el conjunto de prueba](heart-attack-prediction/curvas_roc_modelos.png)

**Figura 4.** Comparativa de curvas ROC en test para los modelos jerárquicos M0 (0,804), M1 (0,815) y M2 (0,829).

### 3.4 Contrastes estadísticos

El ANOVA de devianza en entrenamiento muestra un descenso estadísticamente significativo al transicionar de M0 a M1 ($\Delta\text{Devianza} = 30,613$; 5 gl; *p* = $1,117 \times 10^{-5}$) y de M1 a M2 ($\Delta\text{Devianza} = 40,704$; 6 gl; *p* = $3,312 \times 10^{-7}$).

| Comparación en test      | AUC modelo 1 | AUC modelo 2 | Diferencia de AUC ($\Delta$) | *p*-valor DeLong | IC 95 % de la diferencia |
| ------------------------ | -----------: | -----------: | ---------------------------: | ---------------: | ------------------------ |
| **M0 vs. M2**            |       0,8044 |       0,8294 |                      +0,0250 |       **0,0212** | [−0,0462; −0,0037]       |
| **M1 vs. M2**            |       0,8151 |       0,8294 |                      +0,0142 |       **0,0687** | [−0,0296; +0,0011]       |
| **Random Forest vs. M2** |       0,8147 |       0,8294 |                      +0,0147 |           0,2190 | [−0,0381; +0,0087]       |

La ganancia global de discriminación entre M0 y M2 es **estadísticamente significativa** (*p* = 0,0212). Al aislar la aportación incremental pura de los hábitos sobre la clínica (M1 vs. M2), la ganancia en AUC roza la significancia estadística (*p* = 0,0687), lo que unido a la reducción de devianza del ANOVA ($p < 0,001$) y a la reducción de 28,7 puntos de AIC confirma que los hábitos de vida aportan información genuina.

Para el objetivo de infarto agudo aislado, `m2_ami` obtuvo un AUC de 0,817 frente al 0,829 del modelo M2 para MACE.

![Comparación ROC de infarto aislado y evento compuesto](heart-attack-prediction/curva_roc_mace_vs_infarto.png)

**Figura 5.** Curvas ROC comparativas para infarto aislado (`ami_target`, AUC = 0,817) frente a diana compuesta (`mace_target`, AUC = 0,829).

### 3.5 Predictores e interpretabilidad

Odds Ratios ajustados e intervalos de confianza al 95 % obtenidos del modelo M2:

| Variable                                  | Odds Ratio (OR) |        IC 95 % | Interpretación clínica                                         |
| ----------------------------------------- | --------------: | -------------: | -------------------------------------------------------------- |
| **Sexo masculino (vs. mujer)**            |       **2,051** | [1,414; 2,975] | Factor de riesgo significativo ($p < 0,001$).                  |
| **Fumador activo (vs. nunca)**            |       **1,705** | [1,120; 2,597] | Factor de riesgo significativo ($p = 0,013$).                  |
| **Historial familiar infarto (<50 años)** |       **1,647** | [1,203; 2,256] | Factor de riesgo significativo ($p = 0,002$).                  |
| **Trastorno del sueño / Insomnio**        |       **1,605** | [1,164; 2,213] | Factor de riesgo significativo ($p = 0,004$).                  |
| **Edad (por año)**                        |       **1,082** | [1,067; 1,097] | Incremento de riesgo del 8,2 % anual ($p < 0,001$).            |
| **Horas de sedentarismo diario**          |       **1,064** | [1,012; 1,118] | Incremento del 6,4 % por hora sentado al día ($p = 0,015$).    |
| **Ratio ingresos / pobreza**              |       **0,824** | [0,740; 0,917] | Factor protector socioeconómico ($p < 0,001$).                 |
| **Tensión sistólica media**               |       **0,988** | [0,979; 0,997] | Asociación inversa por causalidad farmacológica ($p = 0,009$). |
| **Pulso en reposo**                       |       **0,985** | [0,972; 0,998] | Asociación inversa por medicación cronotrópica ($p = 0,024$).  |
| Tabaco: Exfumador (vs. nunca)             |           0,788 | [0,549; 1,133] | No significativo tras ajustar por edad (*sick quitter*).       |
| Perímetro de cintura (cm)                 |           1,018 | [0,992; 1,045] | No significativo por colinealidad con IMC.                     |
| Índice de masa corporal (IMC)             |           0,998 | [0,939; 1,060] | No significativo por colinealidad con cintura.                 |
| Tensión diastólica media                  |           0,992 | [0,981; 1,004] | No significativo.                                              |
| Actividad física vigorosa semanal         |           0,907 | [0,543; 1,515] | Intervalo incluye el 1.0.                                      |
| Categorías de etnia/raza                  |               — |              — | Intervalos cruzan el 1.0 respecto a México-Americano.          |

La distinción en 3 niveles de tabaquismo demostró que el tabaquismo activo casi duplica el riesgo ($\text{OR} = 1,705$), mientras que los exfumadores presentan un OR neutro/atenuado ($\text{OR} = 0,788$), explicando por qué en los análisis planos de Kaggle el tabaco parecía no influir. La aparente protección de la tensión sistólica y el pulso refleja causalidad inversa por tratamiento farmacológico intensivo en pacientes crónicos (antihipertensivos y betabloqueantes).

![Forest Plot de Odds Ratios del modelo logístico M2](heart-attack-prediction/forest_plot_riesgo_mace.png)

**Figura 6.** Odds Ratios e intervalos de confianza del 95 % para los predictores del modelo M2.

En el ranking de importancia por permutación de Random Forest (clase positiva MACE), la edad lidera ampliamente (43,2), seguida del perímetro de cintura (10,2), sexo (7,6), ratio ingresos/pobreza (6,6), tensión diastólica (6,5) y etnia/raza (6,2). La importancia por permutación focalizada en la clase positiva evita el sesgo de Gini hacia variables continuas y rescata el peso de factores clínicos y demográficos.

![Importancia por permutación en Random Forest](heart-attack-prediction/importancia_variables_rf.png)

**Figura 7.** Importancia por permutación en Random Forest para la detección de la clase MACE positivo.

La curva de dependencia parcial (*partial plot*) de Random Forest para el sedentarismo confirma una relación no lineal: tras una meseta en valores bajos, el riesgo relativo se incrementa de forma continuada a partir de aproximadamente 5 horas diarias de sedentarismo.

![Dependencia parcial del sedentarismo en Random Forest](heart-attack-prediction/curva_sedentarismo_partialplot.png)

**Figura 8.** Efecto marginal centrado (1/2 log-odds) del sedentarismo diario estimado por Random Forest.

### 3.6 Rendimiento por sexo y evaluación sensible al coste

Evaluación en el conjunto de prueba con el umbral clínico balanceado fijado en entrenamiento (`th = 0,066`):

| Grupo       | N total | Eventos MACE | Prevalencia | AUC (Test) | Sensibilidad | Especificidad | Tasa falsos negativos |
| ----------- | ------: | -----------: | ----------: | ---------: | -----------: | ------------: | --------------------: |
| **Mujeres** |     731 |           39 |      5,34 % |  **0,831** |       71,8 % |        78,8 % |                28,2 % |
| **Hombres** |     648 |           52 |      8,02 % |  **0,821** |       75,0 % |        69,0 % |                25,0 % |

La capacidad discriminativa global es alta y homogénea en ambos sexos (AUC 0,831 en mujeres y 0,821 en hombres). Con un umbral asistencial común, las mujeres presentan una especificidad superior pero una sensibilidad inferior (71,8 % vs. 75,0 %), resultando en una mayor tasa de falsos negativos (28,2 % vs. 25,0 %). Esto refleja la necesidad de calibrar umbrales más sensibles en mujeres para contrarrestar el infradiagnóstico clínico.

![Curvas ROC de M2 separadas por sexo](heart-attack-prediction/curvas_roc_fairness_genero.png)

**Figura 9.** Auditoría de equidad algorítmica: curvas ROC de M2 para mujeres (0,831) y hombres (0,821).

En la evaluación sensible al coste, dado que en la cohorte existen aproximadamente 14 personas sanas por cada paciente enfermo ($[1-p]/p \approx 14,04$), se definió una matriz de costes penalizando el falso negativo 15 veces más que la falsa alarma ($C(\text{FN})/C(\text{FP}) = 15$, pendiente $m = 0,94$). Calibrando el punto óptimo **estrictamente en entrenamiento** se obtuvo un umbral asistencial de **`th = 0,0522`**.

Al evaluar este umbral en test, la **sensibilidad se incrementa al 84,6 %** (capturando a 77 de los 91 enfermos; solo 14 falsos negativos frente a los 89 del umbral convencional al 0,50), con una especificidad del 68,6 %. La recta de iso-coste resulta tangente exterior a la envolvente convexa de la curva ROC.

![Curva ROC y recta de iso-coste para M2](heart-attack-prediction/curva_roc_isocoste_optimo.png)

**Figura 10.** Optimización de umbral asistencial sensible al coste ($C(\text{FN})/C(\text{FP}) = 15$) y recta de iso-coste tangente.

### 3.7 Estratificación y tipo de evento

Estratificación en el conjunto de prueba dividiendo las probabilidades predichas por M2 en cortes fijados a priori:

| Nivel de riesgo asignado  | Participantes | % de test | Riesgo predicho | Eventos reales | Riesgo observado | % de enfermos de test | Infarto / Coronaria / Angina | Acción recomendada                    |
| ------------------------- | ------------: | --------: | --------------: | -------------: | ---------------: | --------------------: | ---------------------------: | ------------------------------------- |
| **Bajo (< 5 %)**          |           886 |    64,2 % |           1,6 % |             14 |        **1,6 %** |                15,4 % |                   11 / 2 / 1 | Consejo general sobre hábitos de vida |
| **Moderado (5–10 %)**     |           225 |    16,3 % |           7,1 % |             25 |       **11,1 %** |                27,5 % |                   13 / 4 / 8 | Revisión en atención primaria         |
| **Alto (10–20 %)**        |           156 |    11,3 % |          14,2 % |             20 |       **12,8 %** |                22,0 % |                   15 / 3 / 2 | Estudio cardiológico (ECG y esfuerzo) |
| **Muy alto ($\ge$ 20 %)** |           112 |     8,1 % |          31,4 % |             32 |       **28,6 %** |                35,2 % |                  20 / 10 / 2 | Derivación prioritaria a cardiología  |

La tasa observada de eventos cardiovasculares crece de forma monótona y consistente a través de los estratos (de 1,6 % en nivel bajo a 28,6 % en muy alto). Concentrando la atención médica en los grupos de riesgo moderado, alto y muy alto (el 35,8 % de la población), el sistema cubre al **84,6 % de todos los pacientes que desarrollaron eventos cardiovasculares**.

![Riesgo observado y tipos de evento por nivel asignado](heart-attack-prediction/estratificacion_riesgo_tipo_evento.png)

**Figura 11.** Estratificación de riesgo en test y distribución de patologías reales observadas.

Para la segunda etapa (especificación del tipo de patología mediante regresión logística multinomial entre enfermos), el modelo parsimonioso de edad y sexo obtuvo menor AIC (408,9 frente a 443,8 del modelo completo). Su capacidad de discriminación multiclase fue moderada (AUC multiclase = 0,590; AUC one-vs-rest: 0,544 infarto, 0,546 coronaria y 0,658 angina), lo cual evidencia que las variables epidemiológicas generales bastan para predecir si un paciente sufrirá un evento isquémico grave, pero se requieren biomarcadores sanguíneos específicos (p. ej., troponinas) para determinar el subtipo exacto. La probabilidad específica $P(\text{Infarto})$ en toda la cohorte alcanzó un AUC de **0,813** detectando infartos en test.

## 4. Discusión

La progresión de modelos demuestra una mejora continuada al incorporar capas de información: el AUC en test se incrementa de 0,804 (M0) a 0,815 (M1) y culmina en **0,829** (M2). El contraste global M0 frente a M2 es estadísticamente significativo en DeLong (*p* = 0,0212), y la aportación incremental de hábitos sobre constantes clínicas (M1 vs. M2) muestra una reducción de devianza altamente significativa en entrenamiento ($p = 3,312 \times 10^{-7}$, $\Delta\text{AIC} = -28,7$) y roza la significancia en el conservador test de DeLong en prueba (*p* = 0,0687).

La modelización del hábito tabáquico en 3 niveles resolvió una paradoja metodológica: al distinguir el abandono por enfermedad (*sick quitter effect*), el tabaco activo se reveló como uno de los principales factores de riesgo ajustados ($\text{OR} = 1,705$). Asimismo, el modelado del sedentarismo mediante Random Forest evidenció un umbral de riesgo creciente a partir de las 5 horas diarias. En el plano ético y asistencial, la auditoría de equidad identificó un mayor riesgo de infradiagnóstico en mujeres con cortes genéricos, justificando la optimización sensible al coste, que reduce drásticamente los falsos negativos de 89 a 14.

## 5. Limitaciones y aspectos metodológicos

### 5.1 Limitaciones metodológicas

- **Diseño transversal:** NHANES es una encuesta transversal; los diagnósticos reflejan antecedentes declarados. No es posible inferir causalidad estricta ni temporalidad longitudinal.
- **Causalidad inversa por medicación:** La aparente asociación protectora de la tensión arterial y el pulso se explica porque los pacientes con antecedentes coronarios se encuentran bajo tratamiento intensivo con antihipertensivos y betabloqueantes.
- **Autodeclaración:** Las variables de estilo de vida provienen de cuestionarios estructurados, susceptibles a sesgo de recuerdo o deseabilidad social.
- **Multicolinealidad biométrica:** El IMC y el perímetro de cintura presentan una correlación de $r = 0,91$, lo que infla los errores estándar al coexistir en el modelo e impide que alcancen significancia individual a pesar de su relevancia biológica.
- **Uso no clínico:** Los estratos y recomendaciones son demostrativos para auditoría de ciencia de datos y no constituyen pautas de prescripción médica.

### 5.2 Decisiones de diseño e implementación

1. **Nivel educativo:** Se procesó en la fase exploratoria pero se excluyó deliberadamente de las fórmulas predictivas por parsimonia, dado que el ratio ingresos/pobreza resume cuantitativamente el nivel socioeconómico sin añadir 4 variables dummy.
2. **Umbrales calculados estrictamente en entrenamiento:** Tanto el umbral de coste ($0,0522$) como el de Youden/closest-topleft ($0,066$) se calibraron exclusivamente en `train_data`, garantizando una evaluación independiente y sin *data leakage* en `test_data`.
3. **Calibración de Random Forest:** Se utilizó muestreo estratificado balanceado por árbol (`sampsize = c(214, 214)`), lo que corrigió la ceguera algorítmica hacia los enfermos y permitió calcular una importancia por permutación focalizada en la clase positiva.

## 6. Conclusiones

1. Los hábitos de vida modificables aportan información cuantitativa y predictiva relevante: el modelo M2 alcanza un **AUC de 0,829** en test, superando a M0 (*p* = 0,0212) y mejorando el ajuste clínico de M1 ($\Delta\text{AIC} = -28,7$; ANOVA $p < 0,001$; DeLong $p = 0,0687$).
2. La definición de la diana compuesta MACE atenúa el desbalanceo severo (prevalencia 6,63 % frente a 3,85 % del infarto aislado) y refleja con mayor fidelidad el sustrato aterosclerótico global.
3. El tabaquismo activo ($\text{OR} = 1,705$), el historial familiar ($\text{OR} = 1,647$), el insomnio ($\text{OR} = 1,605$) y el sedentarismo diario ($\text{OR} = 1,064$ por hora) constituyen los predictores modificables clave.
4. El sedentarismo presenta un impacto no lineal, con un riesgo creciente a partir de aproximadamente 5 horas diarias de inactividad.
5. Los modelos exhiben paridad discriminativa global por sexo (AUC 0,831 en mujeres vs. 0,821 en hombres), pero aplicar un corte único sin calibrar provoca una mayor tasa de falsos negativos en mujeres (28,2 % vs. 25,0 %).
6. La evaluación sensible al coste ($C(\text{FN})/C(\text{FP}) = 15$) determina un umbral óptimo de 0,052 que incrementa la sensibilidad al **84,6 %**, reduciendo los falsos negativos a solo 14 casos.
7. La estratificación en cuatro niveles de riesgo concentra al 84,6 % de los enfermos en el tercio superior de riesgo de la población, demostrando la aplicabilidad de los modelos jerárquicos para priorizar el cribado preventivo.

## 7. Fuentes y documentación

1. Centers for Disease Control and Prevention (CDC). **National Health and Nutrition Examination Survey (NHANES), 2013–2014.** Conjunto de datos utilizado mediante [Kaggle](https://www.kaggle.com/datasets/cdc/national-health-and-nutrition-examination-survey).
2. Scripts del proyecto en el repositorio: `preparacion_datos.R`, `analisis_exploratorio_eda.R`, `modelos.R`, `interpretacion_graficas.R` y `R script prediccion infarto.R`.
3. Flach, P. (2012). *Machine Learning: The Art and Science of Algorithms that Make Sense of Data*. Cambridge University Press (Análisis ROC y rectas de iso-coste).
