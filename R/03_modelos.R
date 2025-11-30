rm(list=ls())

library(tidyverse)
library(caret)
library(glmnet) # Para Ridge y Lasso
library(stats) # PCA y regresión lineal

set.seed(123)

# ------------------------------------------------------------------------------
# Fase 1: Carga de datos ----
# ------------------------------------------------------------------------------
setwd("C:/Users/tania/Documents/UAX/MEIA/Trabajo1")

train <- read.csv(file.path("data", "preprocessed", "train.csv"))
val <- read.csv(file.path("data", "preprocessed", "valid.csv"))

# Separación de variables predictoras (x) y objetivo (y)
x_train <- as.matrix(select(train, -SalePrice))
y_train <- train$SalePrice

x_val <- as.matrix(select(val, -SalePrice))
y_val <- val$SalePrice

# Funciones para métricas de error
rmse <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2))
mae <- function(y_true, y_pred) mean(abs(y_true - y_pred))
r2 <- function(y_true, y_pred) cor(y_true, y_pred)^2

# ------------------------------------------------------------------------------
 # Fase 2: Aplicación del PCA ----
# ------------------------------------------------------------------------------
pca <- prcomp(x_train, center = FALSE, scale. = FALSE)

var_explained <- summary(pca)$importance[3,]
n_comp <- min(which(var_explained >= 0.95))

cat("Se necesitan", n_comp, "componentes para explicar el 95% de la varianza\n")
cat("Total de variables originales:", ncol(x_train), "\n")

x_train_pca <- pca$x[, 1:n_comp]
x_val_pca <- predict(pca, newdata = x_val)[, 1:n_comp]

# ------------------------------------------------------------------------------
# Fase 3: Modelado ----
# ------------------------------------------------------------------------------
## 1) Regresión sobre PCA (PCR) ----
df_train_pcr <- data.frame(SalePrice = y_train, x_train_pca)
model_pcr <- lm(SalePrice ~ ., data = df_train_pcr)

df_val_pcr <- data.frame(x_val_pca)
pred_pcr <- predict(model_pcr, newdata = df_val_pcr)

rmse_pcr <- rmse(y_val, pred_pcr)
mae_pcr <- mae(y_val, pred_pcr)
r2_pcr <- r2(y_val, pred_pcr)

cat("Resultados PCR en validación:\n")
cat(" - RMSE:", round(rmse_pcr, 4), "\n")
cat(" - MAE:", round(mae_pcr, 4), "\n")
cat(" - R2:", round(r2_pcr, 4), "\n")

## 2) Ridge y Lasso sobre variables originales ----
lambda_grid <- 10^seq(2, -2, length = 100)

# Ridge
cv_ridge <- cv.glmnet(x_train, y_train,
                      alpha = 0,
                      lambda = lambda_grid,
                      standardize = FALSE,
                      nfolds = 10)

best_lambda_ridge <- cv_ridge$lambda.min
cat("Mejor Lambda para Ridge:", round(best_lambda_ridge, 4), "\n")

model_ridge <- glmnet(x_train, y_train, 
                      alpha = 0, 
                      lambda = best_lambda_ridge, 
                      standardize = FALSE)

pred_ridge <- predict(model_ridge, newx = x_val)

rmse_ridge <- rmse(y_val, pred_ridge)
mae_ridge <- mae(y_val, pred_ridge)
r2_ridge <- r2(y_val, pred_ridge)

cat("Resultados Ridge en validación:\n")
cat(" - RMSE:", round(rmse_ridge, 4), "\n")
cat(" - MAE:", round(mae_ridge, 4), "\n")
cat(" - R2:", round(r2_ridge, 4), "\n")

# Lasso
cv_lasso <- cv.glmnet(x_train, y_train,
                      alpha = 1,
                      lambda = lambda_grid,
                      standardize = FALSE,
                      nfolds = 10)

best_lambda_lasso <- cv_lasso$lambda.min
cat("Mejor Lambda para Lasso:", round(best_lambda_lasso, 4), "\n")

model_lasso <- glmnet(x_train, y_train, 
                      alpha = 1, 
                      lambda = best_lambda_lasso, 
                      standardize = FALSE)

coefs <- coef(model_lasso)
n_eliminadas <- sum(coefs == 0)
n_total <- length(coefs)

cat("Lasso eliminó", n_eliminadas, "variables de", n_total, "\n")

pred_lasso <- predict(model_lasso, newx = x_val)

rmse_lasso <- rmse(y_val, pred_lasso)
mae_lasso <- mae(y_val, pred_lasso)
r2_lasso <- r2(y_val, pred_lasso)

cat("Resultados Lasso en validación:\n")
cat(" - RMSE:", round(rmse_lasso, 4), "\n")
cat(" - MAE:", round(mae_lasso, 4), "\n")
cat(" - R2:", round(r2_lasso, 4), "\n")

## 3) Ridge y Lasso sobre componentes del PCA ----
# Ridge
cv_pca_ridge <- cv.glmnet(x_train_pca, y_train,
                          alpha = 0,
                          lambda = lambda_grid,
                          standardize = FALSE,
                          nfolds = 10)

best_lambda_pca_ridge <- cv_pca_ridge$lambda.min
cat("Mejor Lambda para Ridge (sobre PCA):", round(best_lambda_pca_ridge, 4), "\n")

model_pca_ridge <- glmnet(x_train_pca, y_train, 
                          alpha = 0, 
                          lambda = best_lambda_pca_ridge, 
                          standardize = FALSE)

pred_pca_ridge <- predict(model_pca_ridge, newx = x_val_pca)

rmse_pca_ridge <- rmse(y_val, pred_pca_ridge)
mae_pca_ridge <- mae(y_val, pred_pca_ridge)
r2_pca_ridge <- r2(y_val, pred_pca_ridge)

cat("Resultados Ridge (sobre PCA) en validación:\n")
cat(" - RMSE:", round(rmse_pca_ridge, 4), "\n")
cat(" - MAE:", round(mae_pca_ridge, 4), "\n")
cat(" - R2:", round(r2_pca_ridge, 4), "\n")

# Lasso
cv_pca_lasso <- cv.glmnet(x_train_pca, y_train,
                          alpha = 1,
                          lambda = lambda_grid,
                          standardize = FALSE,
                          nfolds = 10)

best_lambda_pca_lasso <- cv_pca_lasso$lambda.min
cat("Mejor Lambda (sobre PCA) para Lasso:", round(best_lambda_pca_lasso, 4), "\n")

model_pca_lasso <- glmnet(x_train_pca, y_train,
                          alpha = 1, 
                          lambda = best_lambda_pca_lasso, 
                          standardize = FALSE)

coefs_pca <- coef(model_pca_lasso)
n_comps_eliminadas <- sum(coefs_pca == 0)
n_total_pca <- length(coefs_pca)

cat("Lasso eliminó", n_comps_eliminadas, "variables de", n_total_pca, "\n")

pred_pca_lasso <- predict(model_pca_lasso, newx = x_val_pca)

rmse_pca_lasso <- rmse(y_val, pred_pca_lasso)
mae_pca_lasso <- mae(y_val, pred_pca_lasso)
r2_pca_lasso <- r2(y_val, pred_pca_lasso)

cat("Resultados Lasso (sobre PCA) en validación:\n")
cat(" - RMSE:", round(rmse_pca_lasso, 4), "\n")
cat(" - MAE:", round(mae_pca_lasso, 4), "\n")
cat(" - R2:", round(r2_pca_lasso, 4), "\n")

# ------------------------------------------------------------------------------
# Fase 4: Comparación y guardado de modelos
resultados <- data.frame(
  Modelo = c("PCR", "Ridge", "Lasso", "Ridge (PCA)", "Lasso (PCA)"),
  RMSE = c(rmse_pcr, rmse_ridge, rmse_lasso, rmse_pca_ridge, rmse_pca_lasso),
  MAE = c(mae_pcr, mae_ridge, mae_lasso, mae_pca_ridge, mae_pca_lasso),
  R2 = c(r2_pcr, r2_ridge, r2_lasso, r2_pca_ridge, r2_pca_lasso)
)

print(resultados)

mejor_rmse <- resultados$Modelo[which.min(resultados$RMSE)]
mejor_mae <- resultados$Modelo[which.min(resultados$MAE)]
mejor_r2 <- resultados$Modelo[which.min(resultados$R2)]

cat("Mejor modelo\n")
cat(" - RMSE:", mejor_rmse, "\n")
cat(" - MAE:", mejor_mae, "\n")
cat(" - R2:", mejor_r2, "\n")

# Guardado de modelos
if(!dir.exists("models")) dir.create("models")

save(
  pca, n_comp,
  # Modelos
  model_pcr,
  model_ridge, model_lasso,
  model_pca_ridge, model_pca_lasso,
  # Hiperparametros
  best_lambda_ridge, best_lambda_lasso,
  best_lambda_pca_ridge, best_lambda_pca_lasso,
  file = file.path("models", "trained_models.RData")
)

cat("\nModelos y objetos PCA guadados en 'models/trained_models.RData")
