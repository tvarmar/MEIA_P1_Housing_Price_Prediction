#rm(list=ls())
#setwd("Trabajo1")

library(tidyverse)
library(caret)
library(glmnet) # Para Ridge y Lasso
library(stats) # PCA y regresión lineal

load("models/trained_models.RData")

test <- read.csv(file.path("data", "preprocessed", "test.csv"))
x_test <- as.matrix(select(test, -SalePrice))
y_test <- test$SalePrice

x_test_pca <- predict(pca, newdata = x_test)[, 1:n_comp]

# Funciones para métricas de error
rmse <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2))
mae <- function(y_true, y_pred) mean(abs(y_true - y_pred))
r2 <- function(y_true, y_pred) cor(y_true, y_pred)^2

# 1) PCR
df_pcr <- data.frame(x_test_pca)
pred_pcr <- predict(model_pcr, newdata = df_pcr)

# 2) Ridge
pred_ridge <- as.vector(predict(model_ridge, s = best_lambda_ridge, newx = x_test))

# 3) Lasso
pred_lasso <- as.vector(predict(model_lasso, s = best_lambda_lasso, newx = x_test))

# 4) Ridge en PCA
pred_pca_ridge <- as.vector(predict(model_pca_ridge, s = best_lambda_pca_ridge, newx = x_test_pca))

# 5) Lasso en PCA
pred_pca_lasso <- as.vector(predict(model_pca_lasso, s = best_lambda_pca_lasso, newx = x_test_pca))

# ------------------------------------------------------------------------------
# Resultados ----
# ------------------------------------------------------------------------------
resultados_test <- data.frame(
  Modelo = c("PCR", "Ridge", "Lasso", "Ridge PCA", "Lasso PCA"),
  RMSE = c(rmse(y_test, pred_pcr),
           rmse(y_test, pred_ridge),
           rmse(y_test, pred_lasso),
           rmse(y_test, pred_pca_ridge),
           rmse(y_test, pred_pca_lasso)),
  MAE = c(mae(y_test, pred_pcr),
          mae(y_test, pred_ridge),
          mae(y_test, pred_lasso),
          mae(y_test, pred_pca_ridge),
          mae(y_test, pred_pca_lasso)),
  R2 = c(r2(y_test, pred_pcr),
         r2(y_test, pred_ridge),
         r2(y_test, pred_lasso),
         r2(y_test, pred_pca_ridge),
         r2(y_test, pred_pca_lasso))
)

print(resultados_test)

mejor_rmse <- resultados_test$Modelo[which.min(resultados_test$RMSE)]
mejor_mae <- resultados_test$Modelo[which.min(resultados_test$MAE)]
mejor_r2 <- resultados_test$Modelo[which.max(resultados_test$R2)]

cat("Mejor modelo\n")
cat(" - RMSE:", mejor_rmse, "\n")
cat(" - MAE:", mejor_mae, "\n")
cat(" - R2:", mejor_r2, "\n")

# ------------------------------------------------------------------------------
# Visualización de resultados
# ------------------------------------------------------------------------------
# Comparación de métricas
par(mfrow = c(1, 3), mar = c(5, 4, 3, 1))
barplot(resultados_test$RMSE,
        names.arg = resultados_test$Modelo,
        main = "RMSE por Modelo",
        ylab = "RMSE",
        col = "lightblue",
        las = 2)
barplot(resultados_test$MAE,
        names.arg = resultados_test$Modelo,
        main = "MAE por Modelo",
        ylab = "MAE",
        col = "darkgreen",
        las = 2)
barplot(resultados_test$R2,
        names.arg = resultados_test$Modelo,
        main = "R² por Modelo",
        ylab = "R²",
        col = "purple",
        las = 2,
        ylim = c(0, 1))
par(mfrow = c(1,1))

# Valores reales vs predichos
mejor_modelo <- resultados_test$Modelo[which.min(resultados_test$RMSE)]
mejor_pred <- switch(mejor_modelo,
                      "PCR" = pred_pcr,
                      "Ridge" = pred_ridge,
                      "Lasso" = pred_lasso,
                      "Ridge PCA" = pred_pca_ridge,
                      "Lasso PCA" = pred_pca_lasso)

plot(y_test, mejor_pred, pch = 19, col = "lightblue",
     xlab = "SalePrice Reales (log10)", ylab = "SalePrice Predichos (log10)",
     main = paste("Predicciones vs Reales -", mejor_modelo))

abline(0, 1, col = "red", lwd = 2, lty = 2)
grid()

# Distribución de errores (mejor modelo)
errores <- y_test - mejor_pred
hist(errores, breaks=30, col="lightblue",
     main=paste("Distribución de errores -", mejor_modelo),
     xlab="Error (Real - Predicho)")
abline(v=0, col="red", lwd=2) 

# Boxplot de errores absolutos por modelo
errores_abs <- data.frame(
  PCR = abs(y_test - pred_pcr),
  Ridge = abs(y_test - pred_ridge),
  Lasso = abs(y_test - pred_lasso),
  RidgePCA = abs(y_test - pred_pca_ridge),
  LassoPCA = abs(y_test - pred_pca_lasso)
)

boxplot(errores_abs, col=c("steelblue","darkgreen","darkorange","purple","red"),
        main="Errores absolutos por modelo", ylab="|Error|")

