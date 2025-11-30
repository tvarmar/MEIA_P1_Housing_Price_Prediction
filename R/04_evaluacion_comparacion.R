# Evaluación y Comparación
# - Cálculo de métricas (RMSE, MAE, MSE)
# - Comparación de modelos
# - Gráficos de predicciones vs valores reales
# - Tablas resumen
# --------------------------------------------------------------
# 1. CONFIGURACIÓN
#    - Librerías (Metrics, ggplot2)
#    - Cargar modelos entrenados
#    - Cargar datos de test
# 
# 2. PREDICCIONES EN TEST
#    - Obtener predicciones de cada modelo en test
#    - Verificar que no hay NAs en predicciones
#    - Des-estandarizar SalePrice si es necesario
# 
# 3. CÁLCULO DE MÉTRICAS POR MODELO
#    Para cada modelo calcular:
#    - RMSE (Root Mean Squared Error)
#    - MAE (Mean Absolute Error)
#    - MSE (Mean Squared Error)
#    - R² en test
#    - MAPE (Mean Absolute Percentage Error)
# 
# 4. TABLA COMPARATIVA
#    - Crear dataframe con todas las métricas
#    - Tabla ordenada por RMSE
#    - Identificar mejor modelo
#    - Exportar tabla a CSV y LaTeX
# 
# 5. VISUALIZACIONES COMPARATIVAS
#    - Gráfico de barras: RMSE por modelo
#    - Gráfico de barras: MAE por modelo
#    - Gráfico comparativo de todas las métricas
# 
# 6. ANÁLISIS DETALLADO POR MODELO
#    Para cada modelo:
#    - Scatter plot: predicciones vs valores reales
#    - Residuos vs valores predichos
#    - Distribución de errores (histograma)
#    - Q-Q plot de residuos
# 
# 7. ANÁLISIS DE RESIDUOS COMPARATIVO
#    - Boxplots de residuos por modelo
#    - Identificar patrones de sesgo
#    - Analizar heterocedasticidad
# 
# 8. ANÁLISIS DE COEFICIENTES/VARIABLES
#    - Comparar variables seleccionadas (Lasso)
#    - Top 10 variables más importantes
#    - Gráfico de coeficientes
# 
# 9. INTERPRETACIÓN DE COMPONENTES (PCA)
#    - Cargas de variables en componentes principales
#    - Interpretación práctica de los componentes
#    - Relación con variables originales
# 
# 10. ANÁLISIS DE CASOS EXTREMOS
#     - Identificar predicciones con mayor error
#     - Analizar características de estos casos
#     - Posibles causas de mal ajuste
# 
# 11. VALIDACIÓN CRUZADA COMPLETA (opcional)
#     - K-fold CV sobre todo el dataset
#     - Comparar estabilidad de modelos
#     - Intervalos de confianza de métricas
# 
# 12. CONCLUSIONES Y RECOMENDACIONES
#     - Resumen ejecutivo de resultados
#     - Modelo recomendado y justificación
#     - Limitaciones identificadas
#     - Posibles mejoras futuras
# 
# 13. EXPORTACIÓN FINAL
#     - Guardar todas las gráficas
#     - Guardar tablas de resultados
#     - Crear documento resumen con cifras clave
#     - Preparar elementos para el informe PDF