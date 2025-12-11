# Predicción de precios de vivienda - Ejercicios de Feedback 1
Máster en Inteligencia Artificial - Matemáticas y Estadística para la IA

Autor: Tania Évora Vargas Martínez

## 1. Descripción del proyecto
Este proyecto desarrolla un flujo de trabajo de ciencia de datos para la predicción de la variable `SalePrice` de un dataset de viviendas. El análisis incluye exploración de datos (EDA), preprocesamiento, reducción de dimensionalidad (PCA) y modelado mediante regresión, Ridge y Lasso.

## 2. Estructura de directorios
El proyecto se organiza en las siguientes carpetas. Para la correcta ejecución, se recomienda mantener esta estructura:
```text
.
├── README.txt              <-- Este archivo.
├── informe/
│    └── informe.pdf        <-- Informe del proyecto.
├── data/
│    ├── train.csv          <-- Dataset original.
│    ├── data_description.txt
│    └── preprocessed/      <-- Subcarpeta con los datos tras ejecutar el script 02.
│         ├── train.csv
│         ├── valid.csv
│         └── test.csv
├── models/                 <-- Contiene los objetos R (modelos y PCA) guardados por el script 03.
├── resultados/             <-- Salidas generados por los scripts.
└── R/                      <-- Código fuente (Scripts 01 a 04).
```

## 3. Librerías necesarias
- tidyverse
- tidymodels
- caret
- glmnet
- corrplot
- e1071

Puede instalarlas ejecutando:
`install.packages(c("tidyverse", "tidymodels", "caret", "glmnet", "corrplot", "e1071"))`

## 4. Instrucciones de ejecución 
Para que las rutas relativas funcionen correctamente, establezca el **directorio de trabajo** en la carpeta que contiene a 'R', 'data', etc.

Ejecute los scripts en el siguiente orden secuencial:

Paso 1: `R/01_analisis_exploratorio.R`
- Realiza el EDA inicial.
- Genera gráficos en la carpeta 'resultados/'.

Paso 2: `R/02_preprocesamiento.R`
- Divide los datos (Train/Val/Test), imputa valores y normaliza.
- Guarda los archivos csv procesados en 'data/preprocessed/'.

Paso 3: `R/03_modelos.R`
- Entrena los modelos (PCR, Ridge, Lasso) y ajusta hiperparámetros.
- Guarda los modelos entrenados en 'models/'.

Paso 4: `R/04_evaluacion_comparacion.R`
- Carga los modelos guardados y evalúa su rendimiento en el conjunto de Test.
- Muestra métricas finales y gráficos de error.