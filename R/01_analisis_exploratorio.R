#setwd("Trabajo1")
#rm(list=ls())

library(tidyverse) # Manipulación y gráficas
library(corrplot) # Matrices de correlación
library(e1071) # Asimetría y curtosis

dir.create(file.path("resultados", "EDA"), recursive = TRUE, showWarnings = FALSE)

data <- read.csv(file.path("data", "train.csv"))

# ------------------------------------------------------------------------------
# Fase 1: Análisis de estructura ----
# ------------------------------------------------------------------------------
dim(data)
str(data)
summary(data)

# ------------------------------------------------------------------------------
# Fase 2: Variable objetivo "SalePrice" ----
# ------------------------------------------------------------------------------
# Histograma
ggplot(data, aes(x = SalePrice)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.5) +
  geom_density(col = "red") + 
  theme_minimal() +
  labs(title = "Distribución de SalePrice", x = "Precio", y = "Densidad")

# Asimetría
skewness(data$SalePrice)

# ------------------------------------------------------------------------------
# Fase 3: Análisis de variables numéricas ----
# ------------------------------------------------------------------------------
nums <- unlist(lapply(data, is.numeric))
data_nums <- data[, nums]

# Matriz de correlación de todas las numéricas
cor_matrix <- cor(data_nums, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.6, tl.col = "black")

# Variables más correlacionadas con SalePrice
cor_sorted <- sort(cor_matrix[,"SalePrice"], decreasing = TRUE)
top5_cor <- names(head(cor_sorted, 5))
top10_cor <- names(head(cor_sorted, 10))
head(cor_sorted, 10)

# Como son muchas variables, se muestran solo las 10 más correlacionadas
cort_top_matrix <- cor(data_nums[top10_cor], use = "complete.obs")
corrplot(cort_top_matrix, method = "color", type = "upper", tl.cex = 0.6, tl.col = "black")

data %>%
  select(SalePrice, all_of(top5_cor)) %>%
  pivot_longer(cols = -SalePrice, names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Valor, y = SalePrice)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Línea de tendencia
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Relación de variables Top vs SalePrice", 
       subtitle = "Atención a los valores atípicos (puntos alejados de la tendencia)")

pdf(file.path("resultados", "EDA", "EDA_boxplots_numericas.pdf"), width=12, height=8)
for (var in names(data_nums)) {
  p <- ggplot(data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "steelblue", outlier.color = "red", outlier.alpha = 0.6) +
    theme_minimal() +
    labs(title = paste("Distribución de", var), y = var, x = "")
  print(p)
}
dev.off()

# ------------------------------------------------------------------------------
# Fase 4: Análisis de variables categóricas y valores faltantes ----
# ------------------------------------------------------------------------------
data_cat <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
resumen <- data.frame() # Tabla de categóricas

pdf(file.path("resultados", "EDA", "EDA_boxplots_categoricas.pdf"), width=12, height=8)

for (var in data_cat) {
  n_catergories <- length(unique(data[[var]]))
  n_nas <- sum(is.na(data[[var]]))
  pct_nas <- round(n_nas / nrow(data) * 100, 2)
  
  # Categoría más frecuente (Moda)
  tabla_freq <- table(data[[var]])
  moda <- names(tabla_freq)[which.max(tabla_freq)]
  freq_moda <- max(tabla_freq)
  pct_moda <- round(freq_moda / nrow(data) * 100, 2)
  
  resumen <- rbind(resumen, data.frame(
    Variable = var,
    N_Catergorias = n_catergories,
    Moda = moda,
    Freq_Moda = freq_moda,
    Pct_Moda = pct_moda,
    N_NAs = n_nas,
    pct_nas = pct_nas
  ))
  
  # Outliers
  p <- ggplot(data, aes(x = .data[[var]], y = SalePrice)) +
    geom_boxplot(fill = "steelblue", outlier.color = "red", outlier.alpha = 0.6) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Distribución de SalePrice por", var),
         x = var, y = "Precio de Venta")
  print(p)
}
write.csv(resumen, file.path("resultados", "EDA", "resumen_categoricas.csv"), row.names = FALSE)
dev.off()


pdf(file.path("resultados", "EDA", "EDA_barras_categoricas.pdf"), width=12, height=8)

for (var in data_cat) {
  p <- ggplot(data, aes(x = .data[[var]])) +
    geom_bar(fill = "steelblue", alpha = 0.7) +
    geom_text(stat = "count", aes(label = ..count..), 
              vjust = -0.5, size = 3, color = "black") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Frecuencia de categorías en", var),
         x = var, y = "Frecuencia")
  print(p)
}

dev.off()

