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

La cohorte final de casos completos contiene **4.597 participantes**. La prevalencia observada fue del **3,85 %** para el antecedente aislado de infarto y del **6,63 %** para el evento compuesto definido por el proyecto. En el conjunto de prueba, el AUC fue 0,804 para M0, 0,815 para M1 y 0,825 para M2. La comparación DeLong M0–M2 fue nominalmente significativa (*p* = 0,04338, sin ajuste por comparaciones múltiples), mientras que M1–M2 no lo fue (*p* = 0,126). Por ello, los resultados no demuestran que añadir hábitos mejore la discriminación respecto al modelo que ya incluye variables clínicas.

## Índice

1. [Planteamiento y objetivos](#1-planteamiento-y-objetivos)
2. [Datos y metodología](#2-datos-y-metodología)
3. [Resultados](#3-resultados)
4. [Discusión](#4-discusión)
5. [Limitaciones y aspectos pendientes](#5-limitaciones-y-aspectos-pendientes)
6. [Conclusiones](#6-conclusiones)
7. [Fuentes y documentación](#7-fuentes-y-documentación)

## 1. Planteamiento y objetivos

### 1.1 Problema y alcance

El proyecto aborda la pregunta de cuánto añade conocer hábitos de vida —sedentarismo, actividad física, tabaquismo y sueño— a la información demográfica y a las mediciones clínicas habituales al clasificar antecedentes cardiovasculares declarados.

Para responderla, se comparan tres modelos anidados: un perfil demográfico y socioeconómico (M0), ese perfil ampliado con mediciones clínicas (M1) y un modelo integral que añade variables de estilo de vida y un antecedente familiar (M2). El objetivo principal del código es `mace_target`, un evento compuesto definido a partir de infarto de miocardio, enfermedad coronaria o angina. También se analiza el antecedente aislado de infarto mediante `ami_target`.

La denominación MACE se utiliza aquí para el compuesto particular del proyecto. No se afirma que coincida con una definición clínica universal de MACE.

### 1.2 Objetivo general

Determinar en qué medida las variables de estilo de vida aportan información adicional sobre la clasificación del antecedente cardiovascular compuesto, frente a la información demográfica y clínica, en participantes adultos de NHANES 2013–2014.

### 1.3 Objetivos específicos

1. Comparar M0, M1 y M2 con curvas ROC, AUC y métricas de clasificación.
2. Analizar un evento compuesto además del antecedente aislado de infarto.
3. Examinar predictores mediante odds ratios, importancia por permutación de Random Forest y una curva de dependencia parcial del sedentarismo.
4. Describir diferencias de rendimiento por sexo.
5. Explorar un umbral de clasificación que asigne un coste mayor a los falsos negativos que a los falsos positivos.

### 1.4 Preguntas de investigación

Las preguntas de trabajo incluyen cuánto añaden los hábitos frente a las mediciones clínicas, si se observa un patrón de sedentarismo asociado a mayor salida del modelo, si la asociación entre ejercicio y sedentarismo puede variar, si el rendimiento difiere por sexo y si existen hábitos asociados con el antecedente cardiovascular en perfiles clínicos limítrofes.

Los análisis implementados responden parcialmente a estas preguntas. En particular, los modelos no incluyen interacciones para determinar si el ejercicio modifica la asociación del sedentarismo, ni interacciones con educación o ingresos.

### 1.5 Utilidad y aportación

La aportación metodológica del trabajo es la comparación progresiva de bloques de información, junto con análisis de discriminación, clasificación, equidad descriptiva y sensibilidad al coste. Por el diseño observacional y transversal, los resultados se interpretan como asociaciones y capacidad de clasificación dentro de esta muestra; no como efectos de intervención ni como recomendaciones médicas.

## 2. Datos y metodología

### 2.1 Fuente de datos

La fuente indicada en los objetivos es NHANES 2013–2014, encuesta de salud y nutrición de Estados Unidos. Los datos se obtuvieron del conjunto disponible en [Kaggle: National Health and Nutrition Examination Survey](https://www.kaggle.com/datasets/cdc/national-health-and-nutrition-examination-survey).

| Fichero | Variables principales utilizadas |
| --- | --- |
| `demographic.csv` | Identificador, edad, sexo, etnia/raza, ratio ingresos/pobreza y nivel educativo. |
| `examination.csv` | Presión arterial, pulso en reposo, IMC y perímetro de cintura. |
| `questionnaire.csv` | Diagnósticos cardiovasculares declarados, tabaquismo, actividad física, sedentarismo, sueño e historia familiar. |

Los ficheros se unen por `SEQN` mediante uniones internas. El análisis incluye personas de 20 años o más.

### 2.2 Preparación de los datos

El flujo de preparación en `preparacion_datos.R` realiza las siguientes operaciones:

1. Selecciona las variables de los tres CSV y las combina por `SEQN`.
2. Filtra la población adulta, con edad igual o superior a 20 años.
3. Recodifica sexo, etnia/raza y educación a etiquetas legibles.
4. En las variables binarias seleccionadas, recodifica `1` como sí y `2` como no; otros valores se convierten en `NA`.
5. Calcula el promedio de las dos lecturas sistólicas y las dos diastólicas.
6. Convierte a `NA` los valores de sedentarismo mayores de 1440 minutos y expresa los restantes en horas.
7. Construye las variables objetivo y elimina registros incompletos en las variables incluidas en `vars_modelo`.
8. Divide la cohorte en entrenamiento y prueba con proporción aproximada 70/30, estratificada por `mace_target` y con semilla `654`.

La ejecución dejó 4.597 casos completos. El script no imprime un desglose de las exclusiones por variable o por cada etapa del filtrado.

### 2.3 Variables objetivo

- **`ami_target`:** antecedente declarado de infarto de miocardio.
- **`mace_target`:** vale 1 si cualquiera de las respuestas de infarto, enfermedad coronaria o angina es positiva; vale `NA` si las tres respuestas son desconocidas; en los demás casos vale 0.
- **`tipo_evento`:** categoría exclusiva asignada por prioridad: infarto, enfermedad coronaria, angina y, finalmente, sin evento.

La regla para `mace_target` merece atención: los objetivos describen el valor 0 como negación de las tres condiciones, pero la implementación asigna 0 cuando no hay ningún sí y no están ausentes las tres respuestas. Por ello, una respuesta negativa junto con respuestas desconocidas también puede acabar clasificada como 0. Esta diferencia se conserva documentada y requiere decisión del equipo.

### 2.4 Modelos

Los modelos M0–M2 son regresiones logísticas binomiales para `mace_target`.

| Modelo | Predictores incluidos en el código |
| --- | --- |
| **M0 — Demográfico** | Edad, sexo, etnia/raza y ratio ingresos/pobreza. |
| **M1 — Clínico** | M0 más presión sistólica y diastólica medias, pulso en reposo, IMC y perímetro de cintura. |
| **M2 — Integral** | M1 más tabaquismo histórico (≥100 cigarrillos), actividad vigorosa semanal, horas sedentarias, trastorno del sueño e historia familiar de infarto antes de los 50 años. |

Además, el proyecto ajusta una regresión logística para `ami_target`, un Random Forest de 300 árboles para MACE con muestreo equilibrado por clase en entrenamiento y un modelo multinomial para distinguir el tipo de evento entre participantes con MACE. Para este último, el código compara un modelo completo con uno de edad y sexo y utiliza el modelo parsimonioso.

### 2.5 Evaluación

En el conjunto de prueba se calculan curvas ROC y AUC. Los umbrales de M0–M2 se seleccionan con datos de entrenamiento mediante `closest.topleft`; para Random Forest se utilizan votos OOB del entrenamiento. La tabla de rendimiento incluye sensibilidad, especificidad, precisión, F1, falsos negativos, falsos positivos y coste `10 × falsos negativos + falsos positivos`.

El código también imprime un ANOVA de devianza en entrenamiento y pruebas de DeLong en test: M0 frente a M2, M1 frente a M2 y Random Forest frente a M2. La comparación M0–M2 evalúa el efecto conjunto de añadir clínica y hábitos a M0; M1–M2 es la comparación que evalúa específicamente la contribución incremental de los hábitos.

### 2.6 Implementación y ejecución

El proyecto R se encuentra en `heart-attack-prediction/`. Se abre `Proyecto predicción de Infarto.Rproj` en RStudio y se ejecuta desde la carpeta del proyecto:

```r
source("R script prediccion infarto.R")
```

El flujo llama a preparación, EDA, modelos e interpretación. Las dependencias utilizadas incluyen `dplyr`, `pROC`, `randomForest`, `ggplot2`, `tidyr` y `nnet`. El código carga `nnet` sin comprobar ni instalar automáticamente ese paquete.

## 3. Resultados

### 3.1 Cohorte analítica

| Conjunto | Participantes | MACE positivos |
| --- | ---: | ---: |
| Entrenamiento | 3.218 | 214 |
| Prueba | 1.379 | 91 |
| Total de casos completos | 4.597 | 305 |

La prevalencia del antecedente aislado de infarto fue **3,85 %** y la del evento compuesto **6,63 %**.

### 3.2 Exploración de datos

En el gráfico de datos ausentes, los tres componentes de la respuesta tienen alrededor del 43 % de valores ausentes o códigos 7/9 en la cohorte bruta y entre 0,1 % y 0,3 % en la cohorte adulta. El patrón es compatible con preguntas no aplicables a menores. La presión sistólica mantiene un 8,5 % de valores ausentes/no aplicables en adultos. Los tamaños de las cohortes que aparecen en la leyenda (9.813 y 5.588) son etiquetas escritas literalmente en el código; deben verificarse antes de presentarse como recuentos calculados.

![Porcentaje de valores ausentes antes y después del filtro de edad](heart-attack-prediction/eda_datos_ausentes_filtro_edad.png)

**Figura 1.** Porcentajes de valores ausentes, incluidos los códigos 7/9, antes y después del filtro de edad.

La correlación más alta de la matriz es la de IMC con perímetro de cintura (**r = 0,91**). También se observa correlación entre edad y presión sistólica (**r = 0,44**) y entre las presiones sistólica y diastólica (**r = 0,34**). IMC y cintura se incluyen simultáneamente en M1 y M2; el proyecto no presenta un diagnóstico formal de multicolinealidad.

![Matriz de correlación entre variables cuantitativas](heart-attack-prediction/eda_correlaciones_clinicas.png)

**Figura 2.** Correlaciones de Pearson de las variables cuantitativas incluidas en el EDA.

La frecuencia observada de MACE aumenta con la edad en ambos sexos. En el grupo de 70 años o más, las etiquetas muestran 25,4 % en hombres y 15,7 % en mujeres; para 60–69 años indican 15,9 % y 9,0 %, respectivamente. No se muestran intervalos de confianza ni contrastes estadísticos para estas diferencias.

| Edad | Hombres con MACE | Mujeres con MACE |
| --- | ---: | ---: |
| 20–29 | 0,3 % | 0,0 % |
| 30–39 | 1,8 % | 1,9 % |
| 40–49 | 2,9 % | 2,5 % |
| 50–59 | 7,4 % | 3,6 % |
| 60–69 | 15,9 % | 9,0 % |
| ≥70 | 25,4 % | 15,7 % |

![Distribución de participantes y frecuencia de MACE por edad y sexo](heart-attack-prediction/eda_piramide_edad_mace.png)

**Figura 3.** Pirámide demográfica y proporción de MACE observada por edad y sexo.

### 3.3 Comparación de modelos

| Modelo | AIC | AUC | Umbral | Sensibilidad | Especificidad | Precisión | F1 | Falsos negativos | Falsos positivos | Coste 10:1 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| M0: Demográfico | 1306,1 | 0,804 | 0,078 | 65,9 % | 75,2 % | 15,8 % | 0,255 | 31 | 320 | 630 |
| M1: M0 + Clínicos | 1285,5 | 0,815 | 0,076 | 68,1 % | 76,1 % | 16,8 % | 0,269 | 29 | 308 | 598 |
| M2: M1 + Hábitos | 1265,7 | **0,825** | 0,074 | 70,3 % | **76,6 %** | **17,5 %** | **0,280** | 27 | 302 | **572** |
| Random Forest balanceado | — | 0,811 | 0,415 | **71,4 %** | 74,8 % | 16,7 % | 0,270 | **26** | 325 | 585 |

M2 presenta el mayor AUC puntual y el menor coste de esta tabla. Random Forest tiene la sensibilidad más alta, pero también más falsos positivos y un coste mayor que M2. La precisión de los modelos va del 15,8 % al 17,5 %. El AIC corresponde al ajuste de los modelos logísticos en entrenamiento, no es una métrica de rendimiento en test.

![Curvas ROC de M0, M1 y M2 en el conjunto de prueba](heart-attack-prediction/curvas_roc_modelos_v2.png)

**Figura 4.** Comparación ROC en test; AUC de 0,804, 0,815 y 0,825 para M0, M1 y M2.

### 3.4 Contrastes estadísticos

El ANOVA de devianza en entrenamiento detecta mejora de ajuste de M0 a M1 (diferencia de devianza 30,613; 5 grados de libertad; *p* = 1,117 × 10⁻⁵) y de M1 a M2 (29,772; 5 grados de libertad; *p* = 1,635 × 10⁻⁵).

| Comparación en test | AUC modelo 1 | AUC modelo 2 | Diferencia de AUC | *p* DeLong | IC 95 % de la diferencia indicada |
| --- | ---: | ---: | ---: | ---: | --- |
| M0 − M2 | 0,8044 | 0,8255 | M2 − M0 = 0,0211 | 0,04338 | M0 − M2: [−0,04158; −0,00063] |
| M1 − M2 | 0,8151 | 0,8255 | M2 − M1 = 0,0104 | 0,126 | M1 − M2: [−0,02363; 0,00291] |
| Random Forest − M2 | 0,811 | 0,825 | — | 0,261 | No informado en la salida |

La diferencia M0–M2 es **nominalmente significativa al 5 %** en esta ejecución (*p* = 0,04338), sin ajuste por comparaciones múltiples. El resultado favorece M2, pero la comparación incluye conjuntamente la adición de variables clínicas y de hábitos. Para evaluar si los hábitos añaden discriminación después de la información clínica, la comparación relevante es M1–M2; esta no fue significativa (*p* = 0,126). Por tanto, los resultados no permiten atribuir a los hábitos, por sí solos, la diferencia observada entre M0 y M2.

El ANOVA y DeLong no son pruebas equivalentes: el primero compara ajuste de modelos anidados en entrenamiento y el segundo compara AUC en test. Para el objetivo aislado de infarto, `m2_ami` obtuvo AUC 0,816; el M2 del objetivo compuesto obtuvo 0,825. No se proporcionó una prueba estadística directa entre esas dos curvas.

### 3.5 Predictores e interpretabilidad

En la regresión logística M2, los siguientes intervalos de confianza del 95 % no incluyen 1:

| Variable | Odds ratio | IC 95 % | Escala |
| --- | ---: | ---: | --- |
| Edad | 1,074 | [1,060; 1,089] | Por año adicional. |
| Hombre frente a mujer | 1,951 | [1,348; 2,825] | Comparación de categorías. |
| Ratio ingresos/pobreza | 0,805 | [0,724; 0,894] | Por unidad del ratio. |
| Presión sistólica media | 0,989 | [0,980; 0,998] | Por mmHg. |
| Pulso en reposo | 0,985 | [0,972; 0,999] | Por pulsación/minuto. |
| Horas sedentarias al día | 1,063 | [1,012; 1,117] | Por hora adicional. |
| Trastorno del sueño/insomnio | 1,633 | [1,186; 2,250] | Sí frente a no. |
| Antecedente familiar antes de los 50 | 1,693 | [1,237; 2,316] | Sí frente a no. |

En las demás categorías de etnia/raza y en presión diastólica, IMC, cintura, tabaquismo histórico y actividad vigorosa, los intervalos incluyen 1. Los odds ratios son asociaciones ajustadas, no efectos causales. Las asociaciones inversas de presión sistólica y pulso requieren cautela, especialmente por el carácter transversal y la correlación entre predictores clínicos.

![Odds ratios del modelo logístico M2](heart-attack-prediction/forest_plot_riesgo_mace.png)

**Figura 5.** Odds ratios e intervalos del 95 % del modelo M2.

En la importancia por permutación del Random Forest, edad ocupa el primer lugar (44,1), seguida de perímetro de cintura (10,6), sexo (8,1), actividad vigorosa (7,4) y ratio ingresos/pobreza (6,7). Esta importancia mide la disminución de precisión al permutar la variable para la clase positiva; no indica la dirección de la asociación ni significancia estadística.

![Importancia por permutación en Random Forest](heart-attack-prediction/importancia_variables_rf.png)

**Figura 6.** Ranking de importancia para la detección de MACE positivo.

La dependencia parcial del sedentarismo muestra un patrón no lineal y oscilante. Visualmente, la salida del modelo tiende a aumentar desde aproximadamente cinco horas diarias, pero la curva no fija un punto de corte estable. El eje representa un efecto marginal centrado en media escala de log-odds, no una probabilidad absoluta; la anotación “~5 h/día” es exploratoria y no demuestra un umbral clínico ni causalidad.

![Dependencia parcial del sedentarismo en Random Forest](heart-attack-prediction/curva_sedentarismo_partialplot.png)

**Figura 7.** Efecto marginal estimado por el Random Forest para las horas sedentarias.

### 3.6 Rendimiento por sexo y evaluación sensible al coste

| Grupo | N | Eventos | Prevalencia | AUC | Sensibilidad | Especificidad | Tasa de falsos negativos |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Mujeres | 731 | 39 | 5,34 % | 0,827 | 71,8 % | 79,5 % | 28,2 % |
| Hombres | 648 | 52 | 8,02 % | 0,819 | 78,8 % | 69,5 % | 21,2 % |

Los AUC por sexo son próximos. Con el umbral aplicado, los hombres tienen mayor sensibilidad y menor especificidad; las mujeres, menor tasa de falsos positivos y mayor tasa de falsos negativos. Esta auditoría es descriptiva: el script llama “Youden” al umbral, pero usa `closest.topleft`; además, lo obtiene y evalúa en el mismo conjunto de prueba.

![Curvas ROC de M2 separadas por sexo](heart-attack-prediction/curvas_roc_fairness_genero.png)

**Figura 8.** Curvas ROC de M2 para mujeres y hombres.

Con coste relativo 10:1 para falso negativo frente a falso positivo, la búsqueda sobre la ROC de test encontró umbral 0,0662, sensibilidad 75,8 %, especificidad 74,8 % y coste esperado 0,3945. Como el umbral se optimizó en test, estas métricas no son una evaluación independiente ni constituyen un umbral clínico validado.

![Curva ROC y recta de iso-coste para M2](heart-attack-prediction/curva_roc_isocoste_optimo.png)

**Figura 9.** Optimización exploratoria sensible al coste en el conjunto de prueba.

### 3.7 Estratificación y tipo de evento

| Nivel | Participantes | % de test | Riesgo medio predicho | Eventos observados | Riesgo observado | % de eventos test | Infarto / coronaria / angina |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Bajo | 891 | 64,6 % | 1,7 % | 18 | 2,0 % | 19,8 % | 15 / 2 / 1 |
| Moderado | 218 | 15,8 % | 7,1 % | 19 | 8,7 % | 20,9 % | 8 / 3 / 8 |
| Alto | 160 | 11,6 % | 14,3 % | 25 | 15,6 % | 27,5 % | 16 / 7 / 2 |
| Muy alto | 110 | 8,0 % | 31,6 % | 29 | 26,4 % | 31,9 % | 20 / 7 / 2 |

En esta partición, la frecuencia observada de MACE aumenta de forma ordenada entre los niveles. Esto describe la estratificación obtenida en test; no valida los cortes ni las acciones asociadas para uso clínico.

![Riesgo observado y tipos de evento por nivel asignado](heart-attack-prediction/estratificacion_riesgo_tipo_evento.png)

**Figura 10.** Eventos observados por nivel de riesgo; los puntos muestran el riesgo medio predicho.

El entrenamiento del modelo multinomial incluyó 118 casos de infarto, 71 de enfermedad coronaria y 25 de angina. El modelo de edad y sexo seleccionado tuvo AIC 408,9, frente a 441,4 para el modelo completo. En test, el AUC multiclase fue 0,590; los AUC uno-contra-resto fueron 0,544 para infarto, 0,546 para enfermedad coronaria y 0,658 para angina. La capacidad para distinguir entre tipos de evento fue limitada. El AUC de `P(Infarto)` frente a infarto/no infarto en todo test fue 0,811; es una tarea distinta de la clasificación multinomial entre participantes con MACE.

![Comparación ROC de infarto aislado y evento compuesto](heart-attack-prediction/curva_roc_mace_vs_infarto.png)

**Figura 11.** Curvas ROC para `ami_target` (AUC 0,816) y `mace_target` (AUC 0,825).

Las fichas individuales generadas por el script se omiten; se presentan únicamente resultados agregados, sin identificadores de participantes.

## 4. Discusión

El resultado central depende de la comparación considerada. M2 supera a M0 en AUC en la prueba de DeLong nominal y no ajustada; sin embargo, esa comparación combina la adición del bloque clínico y del bloque de hábitos. En el contraste que aísla el aporte incremental de hábitos —M1 frente a M2— la diferencia de AUC no alcanza significancia estadística en esta partición. Por ello, la evidencia de que los hábitos mejoren la discriminación más allá de las variables clínicas es inconclusa.

El análisis también muestra que una curva de dependencia parcial puede sugerir patrones no lineales, pero no identifica por sí sola un umbral real de riesgo. Del mismo modo, la importancia por permutación sirve para ordenar el uso predictivo de las variables en este Random Forest, no para establecer si una variable aumenta o reduce el riesgo.

## 5. Limitaciones y aspectos pendientes

### 5.1 Limitaciones metodológicas

- **Diseño transversal:** no establece temporalidad ni causalidad; los diagnósticos son antecedentes declarados.
- **Autodeclaración:** los cuestionarios pueden tener errores de recuerdo o declaración.
- **Casos completos:** la exclusión de registros incompletos puede cambiar la composición de la cohorte; no se imprime el desglose de exclusiones.
- **Población y periodo:** los datos corresponden a participantes adultos de NHANES 2013–2014; la generalización a otros periodos o poblaciones no está demostrada.
- **Umbrales elegidos en test:** la búsqueda sensible al coste y el umbral de la comparación por sexo se realizan usando la ROC del conjunto de prueba. Los resultados asociados no son evaluaciones independientes.
- **Comparaciones múltiples:** el DeLong M0–M2 tiene *p* = 0,04338 sin corrección por las varias comparaciones realizadas; debe leerse como resultado nominal.
- **Uso no clínico:** los niveles y acciones de riesgo están codificados y no tienen validación clínica. No deben utilizarse para diagnosticar ni decidir tratamientos.

### 5.2 Diferencias entre objetivos e implementación

1. Los objetivos incluyen `education_level` en M0, pero el código la selecciona y recodifica sin incluirla en `vars_modelo` ni en las fórmulas.
2. Los objetivos incluyen frecuencia actual de tabaquismo (`SMQ040`), pero M2 no la usa. El código incorpora tabaquismo histórico (`SMQ020`) e historia familiar.
3. La regla de valores desconocidos en `mace_target` no coincide exactamente con la frase de objetivos que define 0 como negación de las tres condiciones.
4. No se implementan interacciones para examinar si educación o ingresos modifican la asociación entre hábitos y MACE ni si el ejercicio modifica la asociación del sedentarismo.
5. La etiqueta de la auditoría por sexo dice “Youden”, aunque el código usa `closest.topleft`.
6. Algunos tamaños de cohorte y la anotación “~5 h/día” están escritos literalmente en las figuras. Deben verificarse como cifras calculadas antes de usarlos como resultados.

Las decisiones sobre estas diferencias deben acordarse antes de presentar el trabajo como una implementación completa de todos los objetivos.

## 6. Conclusiones

En la ejecución analizada, el modelo integral M2 obtuvo AUC 0,825 frente a 0,804 para M0, con una diferencia DeLong nominalmente significativa en test (*p* = 0,04338, sin ajuste por comparaciones múltiples). M2 también registró el coste menor en la tabla de clasificación. No obstante, la comparación M1–M2 (*p* = 0,126) no demuestra una mejora significativa de AUC al añadir hábitos después de las variables clínicas.

La estratificación mostró un aumento de la proporción observada de eventos entre niveles en este conjunto de prueba, pero no valida los cortes ni las acciones clínicas del código. El modelo multinomial para distinguir el tipo de evento tuvo discriminación limitada. Debido al carácter transversal y autodeclarado de NHANES, las conclusiones se restringen a clasificación de antecedentes en esta muestra y no permiten afirmar causalidad, predecir eventos futuros individuales ni formular recomendaciones médicas.

## 7. Fuentes y documentación

1. Centers for Disease Control and Prevention (CDC). **National Health and Nutrition Examination Survey (NHANES), 2013–2014.** Conjunto de datos utilizado mediante [Kaggle](https://www.kaggle.com/datasets/cdc/national-health-and-nutrition-examination-survey).
2. Scripts del proyecto: `preparacion_datos.R`, `analisis_exploratorio_eda.R`, `modelos.R`, `interpretacion_graficas.R` y `R script prediccion infarto.R`.
