setwd("C:/Users/tania/Documents/UAX/MEIA/Trabajo1")
rm(list = ls())
library(caret) # Crear particiones
library(tidyverse)
library(tidymodels)
# ------------------------------------------------------------------------------
# Fase 1: División 60-20-20 ----
# ------------------------------------------------------------------------------
data <- read.csv(file.path("data", "train.csv"))
set.seed(123)

trainIndex <- createDataPartition(data$SalePrice, p = 0.6, 
                                  list = FALSE, 
                                  times = 1)
df_train <- data[trainIndex,]
df_temp <- data[-trainIndex,]

valIndex <- createDataPartition(df_temp$SalePrice, p = 0.5, 
                                list = FALSE, 
                                times = 1)
df_val <- df_temp[valIndex,]
df_test <- df_temp[-valIndex,]

cat("Tamaños de conjuntos:\n")
cat("Train:", nrow(df_train), "(60%)\n")
cat("Val:", nrow(df_val), "(20%)\n")
cat("Test:", nrow(df_test), "(20%)\n")
rm(df_temp)

# ------------------------------------------------------------------------------
# Fase 2: Preprocesamiento ----
# ------------------------------------------------------------------------------
# Variables especiales
special_cat <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature")

dependent_num <- c("MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath", "GarageCars", "GarageArea")

# Definición de mapas
map_qual <- c("NA" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)
map_exposure <- c("NA" = 0, "No" = 1, "Mn" = 2, "Av" = 3, "Gd" = 4)
map_fence <- c("NA" = 0, "MnWw" = 1, "GdWo" = 2, "MnPrv" = 3, "GdPrv" = 4)
map_shape <- c("IR3" = 0, "IR2" = 1, "IR1" = 2, "Reg" = 3)
map_types <- c("NA" = 0, "Unf"	= 1, "LwQ"	= 2, "Rec"	= 3, "BLQ"	= 4, "ALQ"	= 5, "GLQ"	= 6)
map_finish <- c("NA" = 0, "Unf" = 1, "RFn" = 2, "Fin" = 3)

recipe <- recipe(SalePrice ~ ., data = df_train) %>%
  # Transformar MSSubClass a categórica
  step_mutate(MSSubClass = as.character(MSSubClass)) %>%
  
  # Manejo de NA estructurales 
  step_mutate_at(all_of(special_cat), 
                 fn = ~ifelse(is.na(.), "NA", as.character(.))) %>%
  step_mutate_at(all_of(dependent_num), 
                 fn = ~ifelse(is.na(.), 0, as.numeric(.))) %>%
  
  step_string2factor(all_nominal_predictors()) %>%
  
  # Transformación de categóricas a ordinales
  step_mutate_at(
    c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC"), 
    fn = ~map_qual[as.character(.)]
  ) %>%
  step_mutate(BsmtExposure = map_exposure[BsmtExposure]) %>%
  step_mutate(Fence = map_fence[Fence]) %>%
  step_mutate(LotShape = map_shape[LotShape]) %>% 
  step_mutate_at(
    c("BsmtFinType1", "BsmtFinType2"), 
    fn = ~map_types[as.character(.)]
  ) %>%
  step_mutate(GarageFinish = map_finish[GarageFinish]) %>%
  
  # Imputación
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  
  # Codificación dummy
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%

  # Corrección de SalePrice
  step_log(SalePrice, base = 10) %>%
  
  # Estandarización y eliminación de varianza cero
  step_normalize(all_numeric_predictors()) # %>%
  #step_nzv(all_predictors())
  
# Preparación de la receta
prep <- prep(recipe, training = df_train)

# Aplicación de la receta a los tres conjuntos
df_train_pr <- bake(prep, new_data = df_train)
df_val_pr <- bake(prep, new_data = df_val)
df_test_pr <- bake(prep, new_data = df_test)

cat("\nNA en train:", sum(is.na(df_train_pr)), "\n")
cat("NA en val:", sum(is.na(df_val_pr)), "\n")
cat("NA en test:", sum(is.na(df_test_pr)), "\n")

# ------------------------------------------------------------------------------
# Fase 3: Expoertación de conjuntos ----
# ------------------------------------------------------------------------------
dir.create(file.path("data", "preprocessed"), showWarnings = FALSE, recursive = TRUE)

write.csv(df_train_pr,
          file = file.path("data", "preprocessed", "train.csv"),
          row.names = FALSE)
write.csv(df_valid_pr,
          file = file.path("data", "preprocessed", "valid.csv"),
          row.names = FALSE)
write.csv(df_test_pr,
          file = file.path("data", "preprocessed", "test.csv"),
          row.names = FALSE)

cat("\n Datos preprocesados guardados en: data/preprocessed/\n")
cat(" - train.csv", nrow(df_train_pr), "filas\n")
cat(" - val.csv", nrow(df_val_pr), "filas\n")
cat(" - test.csv", nrow(df_test_pr), "filas\n")