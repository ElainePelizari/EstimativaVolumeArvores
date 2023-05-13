install.packages("mlbench")
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
library("mlbench")
library("e1071")
library("randomForest")
library("kernlab")
library("caret")
library("neuralnet")

volumes <- read.csv("/Users/adm/Documents/Repositorios/Pós graduação/Volumes.csv", sep=";", dec=",")
head(volumes)

#2 - Eliminar a coluna NR, que só apresenta um número sequencial
volumes <- volumes[,-1]
head(volumes)

#3 - Criar particao de dados: treinamento 80%, teste 20%
indices <- createDataPartition(volumes$VOL, p=0.8, list=FALSE)
treino <- volumes[indices,]
teste <- volumes[-indices,]

#4 - Usando o pacote "caret", treinar os modelos: Random Forest (rf), SVM (svmRadial),
# Redes Neurais (neuralnet) e o modelo alométrico de SPURR
set.seed(7)
rf <- train(VOL~., data=treino, method="rf")
svm <- train(VOL~., data=treino, method="svmRadial")
rna <- train(VOL~., data = treino, method = "nnet", trace = FALSE, linout = TRUE)

#5 - O modelo alometrico ? dado por: Volume = b0 + b1 * dap^2 * Ht
alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, treino, start=list(b0=0.5, b1=0.5))

#6 - Efetue as predicoes nos dados de teste
predict.rf <- predict(rf, teste)
predict.svm <- predict(svm, teste)
predict.rna <- predict(rna, teste)
predict.alom <- predict(alom, teste)

summary(predict.rf)
summary(predict.svm)
summary(predict.rna)
summary(predict.alom)

#7 - Crie funções e calcule as seguintes m?tricas entre a predição e os dados observados

r_squared <- function(modelo, dados, var_resp){
  y = dados[,var_resp]
  numerador <- sum((y - predict(modelo, dados))^2)
  denominador <- sum((y - mean(y))^2)
  return(1 - (numerador/denominador)) 
}

s_xy <- function(modelo, dados, var_resp){
  y = dados[,var_resp]
  numerador <- sum((y - predict(modelo, dados))^2)
  denominador <- length(y) - 2
  return(sqrt(numerador/denominador))
}

s_xy_perc <- function(modelo, dados, var_resp){
  numerador = s_xy(modelo, dados, var_resp)
  denominador = mean(dados[,var_resp])
  return(numerador/denominador * 100)
}

resumo <- function(modelo, dados, var_resp){
  result <- c()
  result <- append(result, r_squared(modelo,dados,var_resp), after=length(result))
  result <- append(result, s_xy(modelo,dados,var_resp), after=length(result))
  result <- append(result, s_xy_perc(modelo,dados,var_resp), after=length(result))
  df <- data.frame(t(result), row.names = modelo$"method")
  colnames(df) <- c("R2", "Sxy", "Sxyp")
  return(df)
}

resultados <- resumo(rf, teste, "VOL")
resultados <- rbind(resultados, resumo(svm, teste, "VOL"))
resultados <- rbind(resultados, resumo(rna, teste, "VOL"))
resultados <- rbind(resultados, resumo(alom, teste, "VOL"))
row.names(resultados)[4] <- "alom"
resultados

# 8. Escolha o melhor modelo

# alom foi o melhor modelo