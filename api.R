# api.R

library(plumber)

# Carregar os modelos
modelo_lm <- readRDS("modelo_lm.rds")
modelo_log <- readRDS("modelo_log.rds")

#* @apiTitle API Machine Learning Netflix
#* @apiDescription Esta API possui dois endpoints:
#* `/predicao` (regressão linear) e `/classificacao` (regressão logística)

#* Predição da variável contínua (duração prevista)
#* @param x valor numérico (ex: rating codificado)
#* @get /predicao
function(x) {
  entrada <- data.frame(x = as.numeric(x))
  names(entrada) <- names(modelo_lm$model)[2]  # pega o nome da variável usada no modelo
  pred <- predict(modelo_lm, newdata = entrada)
  list(valor_entrada = x, predicao = round(pred, 2))
}

#* Classificação binária (tipo previsto: Filme ou Série)
#* @param duration_num duração em minutos
#* @get /classificacao
function(duration_num) {
  entrada <- data.frame(duration_num = as.numeric(duration_num))
  prob <- predict(modelo_log, newdata = entrada, type = "response")
  classe <- ifelse(prob >= 0.5, "Filme", "Série")
  
  list(
    duracao = duration_num,
    probabilidade_filme = round(prob, 3),
    tipo_previsto = classe
  )
}
