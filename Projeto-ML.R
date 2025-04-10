# Instalar pacotes se ainda não tiver
install.packages("dplyr")
install.packages("ggplot2")
install.packages("Metrics")

# Carregar pacotes
library(dplyr)
library(ggplot2)
library(Metrics)

# ETAPA 1 — Preparar os dados

dados_filmes <- dados_filmes %>%
  filter(!is.na(duration), !is.na(rating)) %>%
  mutate(
    duration_num = as.numeric(stringr::str_extract(duration, "\\d+")),
    rating_num = as.numeric(factor(rating))  # codifica o rating
  )

# ETAPA 2 — Treinar o modelo de regressão linear
modelo_lm <- lm(duration_num ~ rating_num, data = dados_filmes)

# Ver resumo do modelo
summary(modelo_lm)

# ETAPA 3 — Prever valores
previsoes <- predict(modelo_lm, newdata = dados_filmes)

# ETAPA 4 — Avaliar o modelo
avaliacao <- data.frame(
  real = dados_filmes$duration_num,
  previsto = previsoes
)

avaliacao <- na.omit(avaliacao)

# Calcular métricas
r2 <- summary(modelo_lm)$r.squared
mae_val <- mae(avaliacao$real, avaliacao$previsto)
rmse_val <- rmse(avaliacao$real, avaliacao$previsto)

# Mostrar métricas
cat("R²:", round(r2, 4), "\n")
cat("MAE:", round(mae_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")

# ETAPA 5 — Visualizar o modelo com gráfico
ggplot(dados_filmes, aes(x = rating_num, y = duration_num)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Regressão Linear: Rating x Duração",
    x = "Rating (codificado)",
    y = "Duração (minutos)"
  ) +
  theme_minimal()

# ETAPA 6 — Salvar o modelo para usar na API
saveRDS(modelo_lm, "modelo_lm.rds")

