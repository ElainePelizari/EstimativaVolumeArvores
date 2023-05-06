# Carregar o arquivo Volumes.csv
dados <- read.csv2("/Users/adm/Documents/Repositorios/Pós graduação/Volumes.csv")

# Eliminar a coluna NR
dados <- dados[-1,]

# Criar partição de dados: treinamento 80%, teste 20%
library(caret)
set.seed(123)
particao <- createDataPartition(dados$VOL, p=0.8, list=FALSE)
treino <- dados[particao,]
teste <- dados[-particao,]

# Treinar os modelos com o pacote "caret"
modelo_rf <- train(VOL ~ ., data=treino, method="rf")
modelo_svm <- train(VOL ~ ., data=treino, method="svmRadial")
modelo_nn <- train(VOL ~ ., data=treino, method="neuralnet")

# Modelo alométrico de SPURR
alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, dados, start=list(b0=0.5, b1=0.5))

# Efetuar as predições nos dados de teste
pred_rf <- predict(modelo_rf, newdata=teste)
pred_svm <- predict(modelo_svm, newdata=teste)
pred_nn <- predict(modelo_nn, newdata=teste)
pred_alom <- predict(alom, newdata=teste)

# Coeficiente de determinação (R2)
R2_rf <- cor(teste$VOL, pred_rf)^2
R2_svm <- cor(teste$VOL, pred_svm)^2
R2_nn <- cor(teste$VOL, pred_nn)^2
R2_alom <- cor(teste$VOL, pred_alom)^2

# Função para calcular as métricas (R2, RMSE e MAE)
calcular_metricas <- function(obs, pred){
  r2 <- cor(obs, pred)^2
  rmse <- sqrt(mean((obs - pred)^2))
  mae <- mean(abs(obs - pred))
  return(c(R2=r2, RMSE=rmse, MAE=mae))
}

# Métricas para os modelos
metricas_rf <- calcular_metricas(teste$Volume, pred_rf)
metricas_svm <- calcular_metricas(teste$Volume, pred_svm)
metricas_nn <- calcular_metricas(teste$Volume, pred_nn)
metricas_alom <- calcular_metricas(teste$Volume, pred_alom)
