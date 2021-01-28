###########
### FAZENDO AJUSTES NECESSARIOS


### Carregando os pacotes
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(readr)
library(caret)
library(nnet)
library(rpart)       # for fitting decision trees
library(ipred)       # for fitting bagged decision trees
library(pROC)
library(NeuralNetTools)

######### carregando base de dados necess?rias
setwd("C:\\Users\\igorl\\Documents\\UFPB\\Igor\\P10\\TCC 2\\Dados\\Script e dados")


### TREINO


abev3_treino <- read.csv("abev3_treinoFinal.csv", sep = ",")
b3sa3_treino <- read.csv("b3sa3_treinoFinal.csv", sep = ",")
bbas3_treino <- read.csv("bbas3_treinoFinal.csv", sep = ",")
bbdc4_treino <- read.csv("bbdc4_treinoFinal.csv", sep = ",")
ibov_treino <- read.csv("ibov_treinoFinal.csv", sep = ",")
itsa4_treino <- read.csv("itsa4_treinoFinal.csv", sep = ",")
itub4_treino <- read.csv("itub4_treinoFinal.csv", sep = ",")
petr3_treino <- read.csv("petr3_treinoFinal.csv", sep = ",")
petr4_treino <- read.csv("petr4_treinoFinal.csv", sep = ",")
vale3_treino <- read.csv("vale3_treinoFinal.csv", sep = ",")
glimpse(b3sa3_treino)

###### TESTE

abev3_teste <- read.csv("abev3_testFinal.csv", sep = ",")
b3sa3_teste <- read.csv("b3sa3_testeFinal.csv", sep = ",")
bbas3_teste <- read.csv("bbas3_testeFinal.csv", sep = ",")
bbdc4_teste <- read.csv("bbdc4_testeFinal.csv", sep = ",")
ibov_teste <- read.csv("ibov_testeFinal.csv", sep = ",")
itsa4_teste <- read.csv("itsa4_testeFinal.csv", sep = ",")
itub4_teste <- read.csv("itub4_testeFinal.csv", sep = ",")
petr3_teste <- read.csv("petr3_testeFinal.csv", sep = ",")
petr4_teste <- read.csv("petr4_testeFinal.csv", sep = ",")
vale3_teste <- read.csv("vale3_testeFinal.csv", sep = ",")
glimpse(b3sa3_teste)
glimpse(teste_b3sa3)
#### Removendo vari?veis desnecess?rias 


glimpse(abev3_treino)
glimpse(b3sa3_teste)
abev3_treino <- abev3_treino[,c(-1:-2)]
abev3_teste <- abev3_teste[,c(-1:-2)]

b3sa3_treino <- b3sa3_treino[,c(-1:-2)]
b3sa3_teste <- b3sa3_teste[,c(-1:-2)]



bbas3_treino <- bbas3_treino[,c(-1:-2)]
bbas3_teste <- bbas3_teste[,c(-1:-2)]

bbdc4_treino <- bbdc4_treino[,c(-1:-2)]
bbdc4_teste <- bbdc4_teste[,c(-1:-2)]

ibov_treino <- ibov_treino[,c(-1:-2)]
ibov_teste <- ibov_teste[,c(-1:-2)]

itsa4_treino <- itsa4_treino[,c(-1:-2)]
itsa4_teste <- itsa4_teste[,c(-1:-2)]

itub4_treino <- itub4_treino[,c(-1:-2)]
itub4_teste <- itub4_teste[,c(-1:-2)]

petr3_treino <- petr3_treino[,c(-1:-2)]
petr3_teste <- petr3_teste[,c(-1:-2)]

petr4_treino <- petr4_treino[,c(-1:-2)]
petr4_teste <- petr4_teste[,c(-1:-2)]

vale3_treino <- vale3_treino[,c(-1:-2)]
vale3_teste <- vale3_teste[,c(-1:-2)]


glimpse(b3sa3_treino)
glimpse(b3sa3_teste)
### Transformando sentido em fac

abev3_treino$sentido_ABEV3 <- as.factor(abev3_treino$sentido_ABEV3)
abev3_teste$sentido_ABEV3 <- as.factor(abev3_teste$sentido_ABEV3)

b3sa3_treino$sentido_B3SA3 <- as.factor(b3sa3_treino$sentido_B3SA3)
b3sa3_teste$sentido_B3SA3 <- as.factor(b3sa3_teste$sentido_B3SA3)

bbas3_treino$sentido_BBAS3 <- as.factor(bbas3_treino$sentido_BBAS3)
bbas3_teste$sentido_BBAS3 <- as.factor(bbas3_teste$sentido_BBAS3)

bbdc4_treino$sentido_BBDC4 <- as.factor(bbdc4_treino$sentido_bbdc4)
bbdc4_teste$sentido_BBDC4 <- as.factor(bbdc4_teste$sentido_BBDC4)

ibov_treino$sentido_IBOV <- as.factor(ibov_treino$sentido_IBOV)
ibov_teste$sentido_IBOV <- as.factor(ibov_teste$sentido_IBOV)

itsa4_treino$sentido_ITSA4 <- as.factor(itsa4_treino$sentido_ITSA4)
itsa4_teste$sentido_ITSA4 <- as.factor(itsa4_teste$sentido_ITSA4)

itub4_treino$sentido_ITUB4 <- as.factor(itub4_treino$sentido_ITUB4)
itub4_teste$sentido_ITUB4 <- as.factor(itub4_teste$sentido_ITUB4)

petr3_treino$sentido_PETR3 <- as.factor(petr3_treino$sentido_PETR3)
petr3_teste$sentido_PETR3 <- as.factor(petr3_teste$sentido_PETR3)

petr4_treino$sentido_PETR4 <- as.factor(petr4_treino$sentido_PETR4)
petr4_teste$sentido_PETR4 <- as.factor(petr4_teste$sentido_PETR4)

vale3_treino$sentido_VALE3 <- as.factor(vale3_treino$sentido_VALE3)
vale3_teste$sentido_VALE3 <- as.factor(vale3_teste$sentido_VALE3)


glimpse(b3sa3_treino)
glimpse(b3sa3_teste)



## Finalizando de ajustar as bases de dados


#### Log abertura e fechamento IBOV



##### Teste
ibov_teste$lag_abertura_IBOV <- log(ibov_teste$lag_abertura_IBOV)
ibov_teste$lag_fechamento_IBOV <- log(ibov_teste$lag_fechamento_IBOV)

##### Treino

ibov_treino$lag_abertura_IBOV <- log(ibov_treino$lag_abertura_IBOV)
ibov_treino$lag_fechamento_IBOV <- log(ibov_treino$lag_fechamento_IBOV)

##### Calculando o DIFF da abertura e fechamento das empresas

#### ABEV3

abev3_teste$lag_diff <- abev3_teste$lag_fechamento_ABEV3 - abev3_teste$lag_abertura_ABEV3
abev3_treino$lag_diff <- abev3_treino$lag_fechamento_ABEV3 - abev3_treino$lag_abertura_ABEV3

abev3_teste$lag_diff_ABEV3 <- NULL
abev3_treino$lag_diff_ABEV3 <- NULL

#### B3SA3
b3sa3_teste$lag_diff <- b3sa3_teste$lag_fechamento_B3SA3 - b3sa3_teste$lag_abertura_B3SA3
b3sa3_treino$lag_diff <- b3sa3_treino$lag_fechamento_B3SA3 - b3sa3_treino$lag_abertura_B3SA3


#### BBAS3

bbas3_teste$lag_diff <- bbas3_teste$lag_fechamento_BBAS3 - bbas3_teste$lag_abertura_BBAS3
bbas3_treino$lag_diff <- bbas3_treino$lag_fechamento_BBAS3 - bbas3_treino$lag_abertura_BBAS3

#### BBDC4

bbdc4_teste$lag_diff <- bbdc4_teste$lag_fechamento_BBDC4 - bbdc4_teste$lag_abertura_BBDC4
bbdc4_treino$lag_diff <- bbdc4_treino$lag_fechamento_BBDC4 - bbdc4_treino$lag_abertura_BBDC4

#### IBOV

ibov_teste$lag_diff <- ibov_teste$lag_fechamento_IBOV - ibov_teste$lag_abertura_IBOV
ibov_treino$lag_diff <- ibov_treino$lag_fechamento_IBOV - ibov_treino$lag_abertura_IBOV

#### ITSA4

itsa4_teste$lag_diff <- itsa4_teste$lag_fechamento_ITSA4 - itsa4_teste$lag_abertura_ITSA4
itsa4_treino$lag_diff <- itsa4_treino$lag_fechamento_ITSA4 - itsa4_treino$lag_abertura_ITSA4

#### ITUB4

itub4_teste$lag_diff <- itub4_teste$lag_fechamento_ITUB4 - itub4_teste$lag_abertura_ITUB4
itub4_treino$lag_diff <- itub4_treino$lag_fechamento_ITUB4 - itub4_treino$lag_abertura_ITUB4

#### PETR3

petr3_teste$lag_diff <- petr3_teste$lag_fechamento_PETR3 - petr3_teste$lag_abertura_PETR3
petr3_treino$lag_diff <- petr3_treino$lag_fechamento_PETR3 - petr3_treino$lag_abertura_PETR3


#### PETR4

petr4_teste$lag_diff <- petr4_teste$lag_fechamento_PETR4 - petr4_teste$lag_abertura_PETR4
petr4_treino$lag_diff <- petr4_treino$lag_fechamento_PETR4 - petr4_treino$lag_abertura_PETR4

#### VALE3


vale3_teste$lag_diff <- vale3_teste$lag_fechamento_VALE3 - vale3_teste$lag_abertura_VALE3
vale3_treino$lag_diff <- vale3_treino$lag_fechamento_VALE3 - vale3_treino$lag_abertura_VALE3

##### MODELANDO LOG?STICA

## ABEV3
set.seed(9)
ctrl <- trainControl(method="repeatedcv", c(repeats = 5), number = 10)
trainControl(seeds = 1)
abev3_treino <- treino
abev3_teste <- teste


logit_abev3 <- train(sentido_ABEV3 ~ lag_diff_ABEV3 + lag_value_ABEV3 + lag_volume_ABEV3, method = "glm",trControl = ctrl, data = abev3_treino)

# visualizar resultado
logit_abev3
summary(logit_abev3)
# previsao
logit_pred_abev3 <-  predict(logit_abev3, newdata = abev3_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_abev3, abev3_teste$sentido_ABEV3)


write.csv(logit_pred_abev3,"logit_pred_abev3.csv")






## B3SA3
## regress?o log?stica pelo caret

set.seed(9)

# validação cruzada 30x10-folds
ctrl <- trainControl(method="repeatedcv", c(repeats = 5), number = 10)

# treinamento/validacao a partir da grid

logit_b3sa3 <- train(sentido_B3SA3 ~., method = "glm",trControl = ctrl, data = b3sa3_treino)

# visualizar resultado
logit_b3sa3
summary(logit_b3sa3)
# previsao
logit_pred_b3sa3 <-  predict(logit_b3sa3, newdata = b3sa3_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_b3sa3, teste_b3sa3$sentido_B3SA3)

#### BBAS3

bbas3_treino <- read.csv("bbas3_treinoFinal.csv", sep = ",")
bbas3_teste <- read.csv("bbas3_testeFinal.csv", sep = ",")

glimpse(bbas3_treino)
glimpse(bbas3_teste)

bbas3_treino <- bbas3_treino[c(-1:-2)]
bbas3_teste <- bbas3_teste[c(-1:-2)]

glimpse(bbas3_treino)

bbas3_treino$sentido_BBAS3 <- as.factor(bbas3_treino$sentido_BBAS3)
bbas3_teste$sentido_BBAS3 <- as.factor(bbas3_teste$sentido_BBAS3)

bbas3_treino$lag_diff <- bbas3_treino$lag_fechamento_BBAS3 - bbas3_treino$lag_abertura_BBAS3
bbas3_teste$lag_diff <- bbas3_teste$lag_fechamento_BBAS3 - bbas3_teste$lag_abertura_BBAS3



bbas3_treino$lag_value_BBAS3
bbas3_teste$lag_value_BBAS3

glimpse(bbas3_treino)
glimpse(bbas3_teste)
## Logistica BBAS3
#################

set.seed(9)

# validação cruzada 30x10-folds
ctrl <- trainControl(method="repeatedcv", c(repeats = 5), number = 10)

# treinamento/validacao a partir da grid

logit_bbas3 <- train(sentido_BBAS3 ~ lag_diff + lag_value_BBAS3 + lag_volume_BBAS3, method = "glm",trControl = ctrl, data = bbas3_treino)

# visualizar resultado
logit_bbas3
summary(logit_bbas3)
# previsao
logit_pred_bbas3 <-  predict(logit_bbas3, newdata = bbas3_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_bbas3, bbas3_teste$sentido_BBAS3)





#### BBDC4

bbdc4_teste <- bbdc4_teste[,-1]
bbdc4_treino <- bbdc4_treino[,-1]

# treinamento/validacao a partir da grid

logit_bbdc4 <- train(sentido_BBDC4 ~ lag_diff + lag_value_BBDC4 + lag_volume_BBDC4, method = "glm",trControl = ctrl, data = bbdc4_treino)

# visualizar resultado
logit_bbdc4
summary(logit_bbdc4)
# previsao
logit_pred_bbdc4 <-  predict(logit_bbdc4, newdata = bbdc4_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_bbdc4, bbdc4_teste$sentido_BBDC4)


#### ITSA4

itsa4_treino <- itsa4_treino[,-1]
glimpse(itsa4_treino)
itsa4_teste <- itsa4_teste[,-1]

# treinamento/validacao a partir da grid

logit_itsa4 <- train(sentido_ITSA4 ~ lag_diff + lag_value_ITSA4 + lag_volume_ITSA4, method = "glm",trControl = ctrl, data = itsa4_treino)

# visualizar resultado
logit_itsa4
summary(logit_itsa4)
# previsao
logit_pred_itsa4 <-  predict(logit_itsa4, newdata = itsa4_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_itsa4, itsa4_teste$sentido_ITSA4)







#### ITUB4




itub4_treino <- itub4_treino[,-1]
itub4_teste <- itub4_teste[,-1]

# treinamento/validacao a partir da grid

logit_itub4 <- train(sentido_ITUB4 ~ lag_diff + lag_value_ITUB4 + lag_volume_ITUB4, method = "glm",trControl = ctrl, data = itub4_treino_test)


# visualizar resultado
logit_itub4
summary(logit_itub4)
# previsao
logit_pred_itub4 <-  predict(logit_itub4, newdata = itub4_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_itub4, itub4_teste$sentido_ITUB4)




#### PETR3




petr3_teste <- petr3_teste[,-1]
petr3_treino <- petr3_treino[,-1]
#################

logit_petr3 <- train(sentido_PETR3 ~ lag_diff + lag_value_PETR3 + lag_volume_PETR3, method = "glm",trControl = ctrl, data = petr3_treino)


# visualizar resultado
logit_petr3
summary(logit_petr3)
# previsao
logit_pred_petr3 <-  predict(logit_petr3, newdata = petr3_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_petr3, petr3_teste$sentido_PETR3)



#### PETR4

petr4_teste <- petr4_teste[,-1]
petr4_treino <- petr4_treino[,-1]


logit_petr4 <- train(sentido_PETR4 ~ lag_diff + lag_value_PETR4 + lag_volume_PETR4, method = "glm",trControl = ctrl, data = petr4_treino)


# visualizar resultado
logit_petr4
summary(logit_petr4)
# previsao
logit_pred_petr4 <-  predict(logit_petr4, newdata = petr4_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_petr4, petr4_teste$sentido_PETR4)




#### VALE3


vale3_treino <- vale3_treino[,-1]
vale3_teste <- vale3_teste[,-1]

logit_vale3 <- train(sentido_VALE3 ~ lag_diff + lag_value_VALE3 + lag_volume_VALE3, method = "glm",trControl = ctrl, data = vale3_treino)


# visualizar resultado
logit_vale3
summary(logit_vale3)
# previsao
logit_pred_vale3 <-  predict(logit_vale3, newdata = vale3_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_vale3, vale3_teste$sentido_VALE3)




#### IBOV


ibov_treino <- ibov_treino[,-1]
ibov_teste <- ibov_teste[,-1]

logit_ibov <- train(sentido_IBOV ~ lag_diff + lag_value_IBOV + lag_volume_IBOV, method = "glm",trControl = ctrl, data = ibov_treino)


# visualizar resultado
logit_ibov
summary(logit_ibov)
# previsao
logit_pred_ibov <-  predict(logit_ibov, newdata = ibov_teste)

# avaliar a matrix de confusão 

confusionMatrix(logit_pred_ibov, ibov_teste$sentido_IBOV)








#### Resultados salvar_logit

confusionMatrix(logit_pred_b3sa3, b3sa3_teste$sentido_B3SA3)

write.csv(logit_pred_b3sa3, "logit_pred_b3sa3.csv")

confusionMatrix(logit_pred_bbas3, bbas3_teste$sentido_BBAS3)
write.csv(logit_pred_bbas3, "logit_pred_bbas3.csv")

confusionMatrix(logit_pred_bbdc4, bbdc4_teste$sentido_BBDC4)
write.csv(logit_pred_bbdc4, "logit_pred_bbdc4.csv")

confusionMatrix(logit_pred_ibov, ibov_teste$sentido_IBOV)
write.csv(logit_pred_ibov, "logit_pred_ibov.csv")

confusionMatrix(logit_pred_itsa4, itsa4_teste$sentido_ITSA4)
write.csv(logit_pred_itsa4, "logit_pred_itsa4.csv")

confusionMatrix(logit_pred_itub4, itub4_teste$sentido_ITUB4)
write.csv(logit_pred_itub4, "logit_pred_itub4.csv")

confusionMatrix(logit_pred_petr3, petr3_teste$sentido_PETR3)
write.csv(logit_pred_petr3, "logit_pred_petr3.csv")

confusionMatrix(logit_pred_petr4, petr4_teste$sentido_PETR4)
write.csv(logit_pred_petr4, "logit_pred_petr4.csv")

confusionMatrix(logit_pred_vale3, vale3_teste$sentido_VALE3)
write.csv(logit_pred_vale3, "logit_pred_vale3.csv")

############## LOGIT_FEITO


#############
#### KNN para todas as empresas
## grid do knn
set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))



# validação cruzada
knn_abev3 <-  train(
  sentido_ABEV3 ~ .,
  data = abev3_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_abev3
knn_abev3$bestTune
plot(knn_abev3)

# previsão
knn_pred_abev3 <- predict(knn_abev3, newdata = abev3_teste)
# matriz de confusão
confusionMatrix(knn_pred_abev3, abev3_teste$sentido_ABEV3)

## salvando os predicted

write.csv(knn_pred_abev3, "knn_pred_abev3.csv")

#### B3SA3

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))



# validação cruzada
knn_b3sa3 <-  train(
  sentido_B3SA3 ~ .,
  data = b3sa3_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_b3sa3
knn_b3sa3$bestTune
plot(knn_b3sa3)

# previsão
knn_pred_b3sa3 <- predict(knn_b3sa3, newdata = b3sa3_teste)
# matriz de confusão
confusionMatrix(knn_pred_b3sa3, b3sa3_teste$sentido_B3SA3)

## salvando os predicted

write.csv(knn_pred_b3sa3, "knn_pred_b3sa3.csv")



#### BBAS3

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))



# validação cruzada
knn_bbas3 <-  train(
  sentido_BBAS3 ~ .,
  data = bbas3_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_bbas3
knn_bbas3$bestTune
plot(knn_bbas3)

# previsão
knn_pred_bbas3 <- predict(knn_bbas3, newdata = bbas3_teste)
# matriz de confusão
confusionMatrix(knn_pred_bbas3, bbas3_teste$sentido_BBAS3)

## salvando os predicted

write.csv(knn_pred_bbas3, "knn_pred_bbas3.csv")








#### BBDC4

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))



# validação cruzada
knn_bbdc4 <-  train(
  sentido_BBDC4 ~ .,
  data = bbdc4_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_bbdc4
knn_bbdc4$bestTune
plot(knn_bbdc4)

# previsão
knn_pred_bbdc4 <- predict(knn_bbdc4, newdata = bbdc4_teste)
# matriz de confusão
confusionMatrix(knn_pred_bbdc4, bbdc4_teste$sentido_BBDC4)

## salvando os predicted

write.csv(knn_pred_bbdc4, "knn_pred_bbdc4.csv")







#### IBOV

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))



# validação cruzada
knn_ibov <-  train(
  sentido_IBOV ~ .,
  data = ibov_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_ibov
knn_ibov$bestTune
plot(knn_ibov)

# previsão
knn_pred_ibov <- predict(knn_ibov, newdata = ibov_teste)
# matriz de confusão
confusionMatrix(knn_pred_ibov, ibov_teste$sentido_IBOV)

## salvando os predicted

write.csv(knn_pred_ibov, "knn_pred_ibov.csv")


#### IBOV

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))



# validação cruzada
knn_itsa4 <-  train(
  sentido_ITSA4 ~ .,
  data = itsa4_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_itsa4
knn_itsa4$bestTune
plot(knn_itsa4)

# previsão
knn_pred_itsa4 <- predict(knn_itsa4, newdata = itsa4_teste)
# matriz de confusão
confusionMatrix(knn_pred_itsa4, itsa4_teste$sentido_ITSA4)

## salvando os predicted

write.csv(knn_pred_itsa4, "knn_pred_itsa4.csv")


#### ITUB4

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))



# validação cruzada
knn_itub4 <-  train(
  sentido_ITUB4 ~ .,
  data = itub4_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_itub4
knn_itub4$bestTune
plot(knn_itub4)

# previsão
knn_pred_itub4 <- predict(knn_itub4, newdata = itub4_teste)
# matriz de confusão
confusionMatrix(knn_pred_itub4, itub4_teste$sentido_ITUB4)

## salvando os predicted

write.csv(knn_pred_itub4, "knn_pred_itub4.csv")


#### PETR3

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))

# validação cruzada
knn_petr3 <-  train(
  sentido_PETR3 ~ .,
  data = petr3_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_petr3
knn_petr3$bestTune
plot(knn_petr3)

# previsão
knn_pred_petr3 <- predict(knn_petr3, newdata = petr3_teste)
# matriz de confusão
confusionMatrix(knn_pred_petr3, petr3_teste$sentido_PETR3)

## salvando os predicted

write.csv(knn_pred_petr3, "knn_pred_petr3.csv")




#### PETR4

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))

# validação cruzada
knn_petr4 <-  train(
  sentido_PETR4 ~ .,
  data = petr4_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_petr4
knn_petr4$bestTune
plot(knn_petr4)

# previsão
knn_pred_petr4 <- predict(knn_petr4, newdata = petr4_teste)
# matriz de confusão
confusionMatrix(knn_pred_petr4, petr4_teste$sentido_PETR4)

## salvando os predicted

write.csv(knn_pred_petr4, "knn_pred_petr4.csv")



#### VALE3

set.seed(9)
knn_grid <- expand.grid(k = seq(1, 301, by = 2))

# validação cruzada
knn_vale3 <-  train(
  sentido_VALE3 ~ .,
  data = vale3_treino,
  method = "knn",
  trControl = ctrl,
  tuneGrid = knn_grid)

# visualizar os resultados
knn_vale3
knn_vale3$bestTune
plot(knn_vale3)

# previsão
knn_pred_vale3 <- predict(knn_vale3, newdata = vale3_teste)
# matriz de confusão
confusionMatrix(knn_pred_vale3, vale3_teste$sentido_VALE3)

## salvando os predicted

write.csv(knn_pred_vale3, "knn_pred_vale3.csv")



plot(knn_itsa4)
plot(knn_bbdc4)








#### KNN_FEITO

### SVM_LINEAR
############

#### svm_ABEV3


# SVM linear
# grid
grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_abev3 <- train(sentido_ABEV3 ~., data = abev3_treino, method = "svmLinear",
                          trControl=ctrl,
                          tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_abev3
svmLin_abev3$results
svmLin_abev3$bestTune
plot(svmLin_abev3)

# previsao
svmLin_pred_abev3 <- predict(svmLin_abev3, newdata = abev3_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_abev3, abev3_teste$sentido_ABEV3)

### Salvando os pred

write.csv(svmLin_pred_abev3, "svmLin_pred_abev3.csv")





#### svm_B3SA3


# SVM linear
# grid
grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_b3sa3 <- train(sentido_B3SA3 ~., data = b3sa3_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_b3sa3
svmLin_b3sa3$results
svmLin_b3sa3$bestTune
plot(svmLin_b3sa3)

# previsao
svmLin_pred_b3sa3 <- predict(svmLin_b3sa3, newdata = b3sa3_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_b3sa3, b3sa3_teste$sentido_B3SA3)

### Salvando os pred

write.csv(svmLin_pred_b3sa3, "svmLin_pred_b3sa3.csv")


#### svm_BBAS3


# SVM linear
# grid
grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_bbas3 <- train(sentido_BBAS3 ~., data = bbas3_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_bbas3
svmLin_bbas3$results
svmLin_bbas3$bestTune
plot(svmLin_bbas3)

# previsao
svmLin_pred_bbas3 <- predict(svmLin_bbas3, newdata = bbas3_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_bbas3, bbas3_teste$sentido_BBAS3)

### Salvando os pred

write.csv(svmLin_pred_bbas3, "svmLin_pred_bbas3.csv")

#### svm_BBDC4


# SVM linear
# grid


grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
grid_svmLin

# validacao cruzada

svmLin_bbdc4 <- train(sentido_BBDC4 ~., data = bbdc4_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_bbdc4
svmLin_bbdc4$results
svmLin_bbdc4$bestTune
plot(svmLin_bbdc4)

# previsao
svmLin_pred_bbdc4 <- predict(svmLin_bbdc4, newdata = bbdc4_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_bbdc4, bbdc4_teste$sentido_BBDC4)

### Salvando os pred

write.csv(svmLin_pred_bbdc4, "svmLin_pred_bbdc4.csv")


#### svm_IBOV


# SVM linear
# grid


grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_ibov <- train(sentido_IBOV ~., data = ibov_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_ibov
svmLin_ibov$results
svmLin_ibov$bestTune
plot(svmLin_ibov)

# previsao
svmLin_pred_ibov <- predict(svmLin_ibov, newdata = ibov_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_ibov, ibov_teste$sentido_IBOV)

### Salvando os pred

write.csv(svmLin_pred_ibov, "svmLin_pred_ibov.csv")



# SVM linear_ITSA4
# grid


grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_itsa4 <- train(sentido_ITSA4 ~., data = itsa4_treino, method = "svmLinear",
                     trControl=ctrl,
                     tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_itsa4
svmLin_itsa4$results
svmLin_itsa4$bestTune
plot(svmLin_itsa4)




# previsao
svmLin_pred_itsa4 <- predict(svmLin_itsa4, newdata = itsa4_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_itsa4, itsa4_teste$sentido_ITSA4)

### Salvando os pred

write.csv(svmLin_pred_itsa4, "svmLin_pred_itsa4.csv")



# SVM linear_ITUB4
# grid


grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_itub4 <- train(sentido_ITUB4 ~., data = itub4_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_itub4
svmLin_itub4$results
svmLin_itub4$bestTune
plot(svmLin_itub4)




# previsao
svmLin_pred_itub4 <- predict(svmLin_itub4, newdata = itub4_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_itub4, itub4_teste$sentido_ITUB4)

### Salvando os pred

write.csv(svmLin_pred_itub4, "svmLin_pred_itub4.csv")



# SVM linear_PETR3
# grid


grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_petr3 <- train(sentido_PETR3 ~., data = petr3_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_petr3
svmLin_petr3$results
svmLin_petr3$bestTune
plot(svmLin_petr3)




# previsao
svmLin_pred_petr3 <- predict(svmLin_petr3, newdata = petr3_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_petr3, petr3_teste$sentido_PETR3)

### Salvando os pred

write.csv(svmLin_pred_petr3, "svmLin_pred_petr3.csv")



# SVM linear_PETR4
# grid


grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_petr4 <- train(sentido_PETR4 ~., data = petr4_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_petr4
svmLin_petr4$results
svmLin_petr4$bestTune
plot(svmLin_petr4)




# previsao
svmLin_pred_petr4 <- predict(svmLin_petr4, newdata = petr4_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_petr4, petr4_teste$sentido_PETR4)

### Salvando os pred

write.csv(svmLin_pred_petr4, "svmLin_pred_petr4.csv")



# SVM linear_VALE3
# grid


grid_svmLin <- expand.grid(C = c(0.001, 0.005, 0.009, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))

# validacao cruzada

svmLin_vale3 <- train(sentido_VALE3 ~., data = vale3_treino, method = "svmLinear",
                      trControl=ctrl,
                      tuneGrid = grid_svmLin)
# visualizar resultados
svmLin_vale3
svmLin_vale3$results
svmLin_vale3$bestTune
plot(svmLin_vale3)




# previsao
svmLin_pred_vale3 <- predict(svmLin_vale3, newdata = vale3_teste)

# matriz de confusao
confusionMatrix(svmLin_pred_vale3, vale3_teste$sentido_VALE3)

### Salvando os pred

write.csv(svmLin_pred_vale3, "svmLin_pred_vale3.csv")


###### Rodando SVM RADIAL

#########

## ABEV3

# SVM Radial
# grid
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1))
# validacao cruzada


svmRad_abev3 <- train(sentido_ABEV3 ~., 
                    data = abev3_treino, 
                    method = "svmRadial",
                    trControl=ctrl,
                    tuneGrid = grid_radial)

# visualizar resultados
svmRad_abev3
svmRad_abev3$results
svmRad_abev3$bestTune
plot(svmRad_abev3)

# prever
svmRad_pred_abev3 <- predict(svmRad_abev3, newdata = abev3_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_abev3, abev3_teste$sentido_ABEV3)


# Salvando o pred

write.csv(svmRad_pred_abev3, "svmRad_pred_abev3.csv")

### B3SA3

# validacao cruzada


svmRad_b3sa3 <- train(sentido_B3SA3 ~., 
                      data = b3sa3_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_b3sa3
svmRad_b3sa3$results
svmRad_b3sa3$bestTune
plot(svmRad_b3sa3)

# prever
svmRad_pred_b3sa3 <- predict(svmRad_b3sa3, newdata = b3sa3_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_b3sa3, b3sa3_teste$sentido_B3SA3)


# Salvando o pred

write.csv(svmRad_pred_b3sa3, "svmRad_pred_b3sa3.csv")


### BBAS3

# validacao cruzada


svmRad_bbas3 <- train(sentido_BBAS3 ~., 
                      data = bbas3_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_bbas3
svmRad_bbas3$results
svmRad_bbas3$bestTune
plot(svmRad_bbas3)

# prever
svmRad_pred_bbas3 <- predict(svmRad_bbas3, newdata = bbas3_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_bbas3, bbas3_teste$sentido_BBAS3)


# Salvando o pred

write.csv(svmRad_pred_bbas3, "svmRad_pred_bbas3.csv")



### BBDC4

svmRad_bbdc4 <- train(sentido_BBDC4 ~., 
                      data = bbdc4_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_bbdc4
svmRad_bbdc4$results
svmRad_bbdc4$bestTune
plot(svmRad_bbdc4)

# prever
svmRad_pred_bbdc4 <- predict(svmRad_bbdc4, newdata = bbdc4_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_bbdc4, bbdc4_teste$sentido_BBDC4)


# Salvando o pred
#SALVAR O CONFUSION MATRIX

write.csv(svmRad_pred_bbdc4, "svmRad_pred_bbdc4.csv")




### IBOV

svmRad_ibov <- train(sentido_IBOV ~., 
                      data = ibov_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_ibov
svmRad_ibov$results
svmRad_ibov$bestTune
plot(svmRad_ibov)

# prever
svmRad_pred_ibov <- predict(svmRad_ibov, newdata = ibov_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_ibov, ibov_teste$sentido_IBOV)


# Salvando o pred

write.csv(svmRad_pred_ibov, "svmRad_pred_ibov.csv")


### ITSA4

svmRad_itsa4 <- train(sentido_ITSA4 ~., 
                     data = itsa4_treino, 
                     method = "svmRadial",
                     trControl=ctrl,
                     tuneGrid = grid_radial)

# visualizar resultados
svmRad_itsa4
svmRad_itsa4$results
svmRad_itsa4$bestTune
plot(svmRad_itsa4)

# prever
svmRad_pred_itsa4 <- predict(svmRad_itsa4, newdata = itsa4_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_itsa4, itsa4_teste$sentido_ITSA4)


# Salvando o pred

write.csv(svmRad_pred_itsa4, "svmRad_pred_itsa4.csv")

### ITUB4

svmRad_itub4 <- train(sentido_ITUB4 ~., 
                      data = itub4_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_itub4
svmRad_itub4$results
svmRad_itub4$bestTune
plot(svmRad_itub4)

# prever
svmRad_pred_itub4 <- predict(svmRad_itub4, newdata = itub4_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_itub4, itub4_teste$sentido_ITUB4)


# Salvando o pred

write.csv(svmRad_pred_itub4, "svmRad_pred_itub4.csv")

### PETR3

svmRad_petr3 <- train(sentido_PETR3 ~., 
                      data = petr3_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_petr3
svmRad_petr3$results
svmRad_petr3$bestTune
plot(svmRad_petr3)

# prever
svmRad_pred_petr3 <- predict(svmRad_petr3, newdata = petr3_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_petr3, petr3_teste$sentido_PETR3)


# Salvando o pred

write.csv(svmRad_pred_petr3, "svmRad_pred_petr3.csv")

### PETR4

svmRad_petr4 <- train(sentido_PETR4 ~., 
                      data = petr4_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_petr4
svmRad_petr4$results
svmRad_petr4$bestTune
plot(svmRad_petr4)

# prever
svmRad_pred_petr4 <- predict(svmRad_petr4, newdata = petr4_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_petr4, petr4_teste$sentido_PETR4)


# Salvando o pred

write.csv(svmRad_pred_petr4, "svmRad_pred_petr4.csv")

### VALE3

svmRad_vale3 <- train(sentido_VALE3 ~., 
                      data = vale3_treino, 
                      method = "svmRadial",
                      trControl=ctrl,
                      tuneGrid = grid_radial)

# visualizar resultados
svmRad_vale3
svmRad_vale3$results
svmRad_vale3$bestTune
plot(svmRad_vale3)

# prever
svmRad_pred_vale3 <- predict(svmRad_vale3, newdata = vale3_teste)

# matriz de confusao
confusionMatrix(svmRad_pred_vale3, vale3_teste$sentido_VALE3)


# Salvando o pred

write.csv(svmRad_pred_vale3, "svmRad_pred_vale3.csv")





## Random Forest

### ABEV3

rf_metric <- "Accuracy"






rf_train <- train(Despesa_Operadora~., data=data_train, method="ranger", num.trees = 500, importance = "permutation")



warnings()
print(rf_abev3)
plot(rf_abev3)

rf_pred_abev3 <- predict(rf_abev3, abev3_teste)

confusionMatrix(rf_pred_abev3, abev3_teste$sentido_ABEV3)



# Salvando pred

write.csv(rf_pred_abev3,"rf_pred_abev3.csv")


####  B3SA3


rf_b3sa3 <- train(sentido_B3SA3~., data=b3sa3_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_b3sa3)
plot(rf_b3sa3)

rf_pred_b3sa3 <- predict(rf_b3sa3, b3sa3_teste)

confusionMatrix(rf_pred_b3sa3, b3sa3_teste$sentido_B3SA3)



# Salvando pred

write.csv(rf_pred_b3sa3,"rf_pred_b3sa3.csv")

####  BBAS3


rf_bbas3 <- train(sentido_BBAS3~., data=bbas3_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_bbas3)
plot(rf_bbas3)

rf_pred_bbas3 <- predict(rf_bbas3, bbas3_teste)

confusionMatrix(rf_pred_bbas3, bbas3_teste$sentido_BBAS3)



# Salvando pred

write.csv(rf_pred_bbas3,"rf_pred_bbas3.csv")

####  BBDC4


rf_bbdc4 <- train(sentido_BBDC4~., data=bbdc4_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_bbdc4)
plot(rf_bbdc4)

rf_pred_bbdc4 <- predict(rf_bbdc4, bbdc4_teste)

confusionMatrix(rf_pred_bbdc4, bbdc4_teste$sentido_BBDC4)



# Salvando pred

write.csv(rf_pred_bbdc4,"rf_pred_bbdc4.csv")

####  IBOV


rf_ibov <- train(sentido_IBOV~., data=ibov_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_ibov)
plot(rf_ibov)

rf_pred_ibov <- predict(rf_ibov, ibov_teste)

confusionMatrix(rf_pred_ibov, ibov_teste$sentido_IBOV)



# Salvando pred

write.csv(rf_pred_ibov,"rf_pred_ibov.csv")


####  ITSA4


rf_itsa4 <- train(sentido_ITSA4~., data=itsa4_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_itsa4)
plot(rf_itsa4)

rf_pred_itsa4 <- predict(rf_itsa4, itsa4_teste)

confusionMatrix(rf_pred_itsa4, itsa4_teste$sentido_ITSA4)



# Salvando pred

write.csv(rf_pred_itsa4,"rf_pred_itsa4.csv")



####  ITUB4


rf_itub4 <- train(sentido_ITUB4~., data=itub4_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_itub4)
plot(rf_itub4)

rf_pred_itub4 <- predict(rf_itub4, itub4_teste)

confusionMatrix(rf_pred_itub4, itub4_teste$sentido_ITUB4)



# Salvando pred

write.csv(rf_pred_itub4,"rf_pred_itub4.csv")


####  PETR3


rf_petr3 <- train(sentido_PETR3~., data=petr3_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_petr3)
plot(rf_petr3)

rf_pred_petr3 <- predict(rf_petr3, petr3_teste)

confusionMatrix(rf_pred_petr3, petr3_teste$sentido_PETR3)



# Salvando pred

write.csv(rf_pred_petr3,"rf_pred_petr3.csv")



####  PETR4


rf_petr4 <- train(sentido_PETR4~., data=petr4_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_petr4)
plot(rf_petr4)

rf_pred_petr4 <- predict(rf_petr4, petr4_teste)

confusionMatrix(rf_pred_petr4, petr4_teste$sentido_PETR4)



# Salvando pred

write.csv(rf_pred_petr4,"rf_pred_petr4.csv")

####  VALE3


rf_vale3 <- train(sentido_VALE3~., data=vale3_treino, method="rf", metric=metric, tuneGrid=rf_grid, trControl=ctrl)
print(rf_vale3)
plot(rf_vale3)

rf_pred_vale3 <- predict(rf_vale3, vale3_teste)

confusionMatrix(rf_pred_vale3, vale3_teste$sentido_VALE3)



# Salvando pred

write.csv(rf_pred_vale3,"rf_pred_vale3.csv")





## Bagging

## ABEV3


bag_abev3 <- train(sentido_ABEV3 ~.,data = abev3_treino, method = "treebag", trControl = ctrl,
                   metric = "Accuracy" )
bag_abev3


bag_pred_abev3 <- predict(bag_abev3, abev3_teste)
confusionMatrix(bag_pred_abev3, abev3_teste$sentido_ABEV3)


### Salvando pred

write.csv(bag_pred_abev3,"bag_pred_abev3.csv")


## B3SA3


bag_b3sa3 <- train(sentido_B3SA3 ~.,data = b3sa3_treino, method = "treebag", trControl = ctrl,
                   metric = "Accuracy" )
bag_b3sa3


bag_pred_b3sa3 <- predict(bag_b3sa3, b3sa3_teste)
confusionMatrix(bag_pred_b3sa3, b3sa3_teste$sentido_B3SA3)


### Salvando pred

write.csv(bag_pred_b3sa3,"bag_pred_b3sa3.csv")


## BBAS3


bag_bbas3 <- train(sentido_BBAS3 ~.,data = bbas3_treino, method = "treebag", trControl = ctrl,
                   metric = "Accuracy" )
bag_bbas3


bag_pred_bbas3 <- predict(bag_bbas3, bbas3_teste)
confusionMatrix(bag_pred_bbas3, bbas3_teste$sentido_BBAS3)


### Salvando pred

write.csv(bag_pred_bbas3,"bag_pred_bbas3.csv")


## BBDC4

bag_bbdc4 <- train(sentido_BBDC4 ~.,data = bbdc4_treino, method = "treebag", trControl = ctrl,
                   metric = "Accuracy" )
bag_bbdc4


bag_pred_bbdc4 <- predict(bag_bbdc4, bbdc4_teste)
confusionMatrix(bag_pred_bbdc4, bbdc4_teste$sentido_BBDC4)


### Salvando pred

write.csv(bag_pred_bbdc4,"bag_pred_bbdc4.csv")



## BBDC4


bag_bbdc4 <- train(sentido_BBDC4 ~.,data = bbdc4_treino, method = "treebag", trControl = ctrl,
                   metric = "Accuracy" )
bag_bbdc4


bag_pred_bbdc4 <- predict(bag_bbdc4, bbdc4_teste)
confusionMatrix(bag_pred_bbdc4, bbdc4_teste$sentido_BBDC4)


### Salvando pred

write.csv(bag_pred_bbdc4,"bag_pred_bbdc4.csv")



## IBOV


bag_ibov<- train(sentido_IBOV ~.,data = ibov_treino, method = "treebag", trControl = ctrl,
                   metric = "Accuracy" )
bag_ibov


bag_pred_ibov <- predict(bag_ibov, ibov_teste)
confusionMatrix(bag_pred_ibov, ibov_teste$sentido_IBOV)


### Salvando pred

write.csv(bag_pred_ibov,"bag_pred_ibov.csv")



## ITSA4


bag_itsa4<- train(sentido_ITSA4 ~.,data = itsa4_treino, method = "treebag", trControl = ctrl,
                 metric = "Accuracy" )
bag_itsa4


bag_pred_itsa4 <- predict(bag_itsa4, itsa4_teste)
confusionMatrix(bag_pred_itsa4, itsa4_teste$sentido_ITSA4)


### Salvando pred

write.csv(bag_pred_itsa4,"bag_pred_itsa4.csv")



## ITUB4


bag_itub4<- train(sentido_ITUB4 ~.,data = itub4_treino, method = "treebag", trControl = ctrl,
                  metric = "Accuracy" )
bag_itub4


bag_pred_itub4 <- predict(bag_itub4, itub4_teste)
confusionMatrix(bag_pred_itub4, itub4_teste$sentido_ITUB4)


### Salvando pred

write.csv(bag_pred_itub4,"bag_pred_itub4.csv")




## PETR3


bag_petr3<- train(sentido_PETR3 ~.,data = petr3_treino, method = "treebag", trControl = ctrl,
                  metric = "Accuracy" )
bag_petr3


bag_pred_petr3 <- predict(bag_petr3, petr3_teste)
confusionMatrix(bag_pred_petr3, petr3_teste$sentido_PETR3)


### Salvando pred

write.csv(bag_pred_petr3,"bag_pred_petr3.csv")


## PETR4


bag_petr4<- train(sentido_PETR4 ~.,data = petr4_treino, method = "treebag", trControl = ctrl,
                  metric = "Accuracy" )
bag_petr4


bag_pred_petr4 <- predict(bag_petr4, petr4_teste)
confusionMatrix(bag_pred_petr4, petr4_teste$sentido_PETR4)


### Salvando pred

write.csv(bag_pred_petr4,"bag_pred_petr4.csv")



## VALE3


bag_vale3<- train(sentido_VALE3 ~.,data = vale3_treino, method = "treebag", trControl = ctrl,
                  metric = "Accuracy" )
bag_vale3


bag_pred_vale3 <- predict(bag_vale3, vale3_teste)
confusionMatrix(bag_pred_vale3, vale3_teste$sentido_VALE3)


### Salvando pred

write.csv(bag_pred_vale3,"bag_pred_vale3.csv")


#### AdaBoost

## ABEV3

library(fastAdaboost)
library(ada)

grid_ada <- expand.grid(nIter = 2, method = "Binary")

ada_abev3 <- train(sentido_ABEV3~., data = abev3_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_abev3

ada_pred_abev3 <- predict(ada_abev3, abev3_teste)
confusionMatrix(ada_pred_abev3, abev3_teste$sentido_ABEV3)


### Salvando pred

write.csv(ada_pred_abev3,"ada_pred_abev3.csv")


#### B3SA3

ada_b3sa3 <- train(sentido_B3SA3~., data = b3sa3_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_b3sa3

ada_pred_b3sa3 <- predict(ada_b3sa3, b3sa3_teste)
confusionMatrix(ada_pred_b3sa3, b3sa3_teste$sentido_B3SA3)


### Salvando pred

write.csv(ada_pred_b3sa3,"ada_pred_b3sa3.csv")

#### BBAS3

ada_bbas3 <- train(sentido_BBAS3~., data = bbas3_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_bbas3

ada_pred_bbas3 <- predict(ada_bbas3, bbas3_teste)
confusionMatrix(ada_pred_bbas3, bbas3_teste$sentido_BBAS3)


### Salvando pred

write.csv(ada_pred_bbas3,"ada_pred_bbas3.csv")

#### BBDC4

ada_bbdc4 <- train(sentido_BBDC4~., data = bbdc4_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_bbdc4

ada_pred_bbdc4 <- predict(ada_bbdc4, bbdc4_teste)
confusionMatrix(ada_pred_bbdc4, bbdc4_teste$sentido_BBDC4)


### Salvando pred

write.csv(ada_pred_bbdc4,"ada_pred_bbdc4.csv")




#### IBOV

ada_ibov <- train(sentido_IBOV~., data = ibov_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_ibov

ada_pred_ibov <- predict(ada_ibov, ibov_teste)
confusionMatrix(ada_pred_ibov, ibov_teste$sentido_IBOV)


### Salvando pred

write.csv(ada_pred_ibov,"ada_pred_ibov.csv")



#### ITSA4

ada_itsa4 <- train(sentido_ITSA4~., data = itsa4_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_itsa4

ada_pred_itsa4 <- predict(ada_itsa4, itsa4_teste)
confusionMatrix(ada_pred_itsa4, itsa4_teste$sentido_ITSA4)


### Salvando pred

write.csv(ada_pred_itsa4,"ada_pred_itsa4.csv")



#### ITUB4

ada_itub4 <- train(sentido_ITUB4~., data = itub4_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_itub4

ada_pred_itub4 <- predict(ada_itub4, itub4_teste)
confusionMatrix(ada_pred_itub4, itub4_teste$sentido_ITUB4)


### Salvando pred

write.csv(ada_pred_itub4,"ada_pred_itub4.csv")



#### PETR3

ada_petr3 <- train(sentido_PETR3~., data = petr3_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_petr3

ada_pred_petr3 <- predict(ada_petr3, petr3_teste)
confusionMatrix(ada_pred_petr3, petr3_teste$sentido_PETR3)


### Salvando pred

write.csv(ada_pred_petr3,"ada_pred_petr3.csv")



#### PETR4

ada_petr4 <- train(sentido_PETR4~., data = petr4_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_petr4

ada_pred_petr4 <- predict(ada_petr4, petr4_teste)
confusionMatrix(ada_pred_petr4, petr4_teste$sentido_PETR4)


### Salvando pred

write.csv(ada_pred_petr4,"ada_pred_petr4.csv")



#### VALE3

ada_vale3 <- train(sentido_VALE3~., data = vale3_treino, metric = "Accuracy" , method = "adaboost", tuneGrid = grid_ada, trControl = ctrl)
ada_vale3

ada_pred_vale3 <- predict(ada_vale3, vale3_teste)
confusionMatrix(ada_pred_vale3, vale3_teste$sentido_VALE3)


### Salvando pred

write.csv(ada_pred_vale3,"ada_pred_vale3.csv")




#########

## XGBoost

library(doParallel)
cores <- 7
registerDoParallel(cores = cores)
library(xgboost)

set.seed(9)
nrounds <- 1000


tuneGrid_xgb_1 <- expand.grid(nrounds = 100,
                         eta = 0.025,
                         max_depth = 10,
                         gamma = 10,
                         colsample_bytree = 1,
                         min_child_weight = 10,
                         subsample = 0.5)

tuneGrid_xgb_2 <- expand.grid(nrounds = c(100,150),
                              eta = c(0.025,0.05),
                              max_depth = c(10,20),
                              gamma = c(10,20),
                              colsample_bytree = 1,
                              min_child_weight = c(10,20),
                              subsample = 0.5)

tuneGrid_xgb_3 <- expand.grid(nrounds = c(100,150,200,250),
                              eta = c(0.025,0.05,0.1,0.2),
                              max_depth = c(10,20,30),
                              gamma = c(10,20,30),
                              colsample_bytree = 1,
                              min_child_weight = c(10,20,30),
                              subsample = 0.5)

#### ABEV3

mat_abev3_treino <- as.matrix(abev3_treino[,-1])
mat_abev3_classe <- as.factor(as.matrix(abev3_treino[,1]))


set.seed(9)

xgb_abev3 <- train(x = mat_abev3_treino, y = mat_abev3_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_abev3

plot(xgb_abev3)



abev3_teste$lag_diff_ABEV3 <- abev3_teste$lag_fechamento_ABEV3 - abev3_teste$lag_abertura_ABEV3
names(abev3_teste) <- names(abev3_treino)

xgb_pred_abev3_3 <- predict(xgb_abev3, abev3_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_abev3_2, abev3_teste$sentido_ABEV3)


# SALVANDO XGB
write.csv(xgb_pred_abev3_2,"xgb_pred_abev3.csv")

###########


#### B3SA3

mat_b3sa3_treino <- as.matrix(b3sa3_treino[,-1])
mat_b3sa3_classe <- as.factor(as.matrix(b3sa3_treino[,1]))




xgb_b3sa3 <- train(x = mat_b3sa3_treino, y = mat_b3sa3_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_b3sa3

plot(xgb_b3sa3)


xgb_pred_b3sa3 <- predict(xgb_b3sa3, b3sa3_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_b3sa3, b3sa3_teste$sentido_B3SA3)


# SALVANDO XGB
write.csv(xgb_pred_b3sa3,"xgb_pred_b3sa3.csv")




#### BBASA3

mat_bbas3_treino <- as.matrix(bbas3_treino[,-1])
mat_bbas3_classe <- as.factor(as.matrix(bbas3_treino[,1]))




xgb_bbas3 <- train(x = mat_bbas3_treino, y = mat_bbas3_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_bbas3

plot(xgb_bbas3)


xgb_pred_bbas3 <- predict(xgb_bbas3, bbas3_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_bbas3, bbas3_teste$sentido_BBAS3)


# SALVANDO XGB
write.csv(xgb_pred_bbas3,"xgb_pred_bbas3.csv")




#### BBDC4

mat_bbdc4_treino <- as.matrix(bbdc4_treino[,-1])
mat_bbdc4_classe <- as.factor(as.matrix(bbdc4_treino[,1]))




xgb_bbdc4 <- train(x = mat_bbdc4_treino, y = mat_bbdc4_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_bbdc4

plot(xgb_bbdc4)


xgb_pred_bbdc4 <- predict(xgb_bbdc4, bbdc4_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_bbdc4, bbdc4_teste$sentido_BBDC4)


# SALVANDO XGB
write.csv(xgb_pred_bbdc4,"xgb_pred_bbdc4.csv")




#### IBOV

mat_ibov_treino <- as.matrix(ibov_treino[,-1])
mat_ibov_classe <- as.factor(as.matrix(ibov_treino[,1]))




xgb_ibov <- train(x = mat_ibov_treino, y = mat_ibov_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_ibov

plot(xgb_ibov)


xgb_pred_ibov <- predict(xgb_ibov, ibov_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_ibov, ibov_teste$sentido_IBOV)


# SALVANDO XGB
write.csv(xgb_pred_ibov,"xgb_pred_ibov.csv")


#### ITSA4

mat_itsa4_treino <- as.matrix(itsa4_treino[,-1])
mat_itsa4_classe <- as.factor(as.matrix(itsa4_treino[,1]))




xgb_itsa4 <- train(x = mat_itsa4_treino, y = mat_itsa4_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_itsa4

plot(xgb_itsa4)


xgb_pred_itsa4 <- predict(xgb_itsa4, itsa4_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_itsa4, itsa4_teste$sentido_ITSA4)


# SALVANDO XGB
write.csv(xgb_pred_itsa4,"xgb_pred_itsa4.csv")



#### ITUB4

mat_itub4_treino <- as.matrix(itub4_treino[,-1])
mat_itub4_classe <- as.factor(as.matrix(itub4_treino[,1]))




xgb_itub4 <- train(x = mat_itub4_treino, y = mat_itub4_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_itub4

plot(xgb_itub4)


xgb_pred_itub4 <- predict(xgb_itub4, itub4_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_itub4, itub4_teste$sentido_ITUB4)


# SALVANDO XGB
write.csv(xgb_pred_itub4,"xgb_pred_itub4.csv")


#### PETR3

mat_petr3_treino <- as.matrix(petr3_treino[,-1])
mat_petr3_classe <- as.factor(as.matrix(petr3_treino[,1]))




xgb_petr3 <- train(x = mat_petr3_treino, y = mat_petr3_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_petr3

plot(xgb_petr3)


xgb_pred_petr3 <- predict(xgb_petr3, petr3_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_petr3, petr3_teste$sentido_PETR3)


# SALVANDO XGB
write.csv(xgb_pred_petr3,"xgb_pred_petr3.csv")




#### PETR4

mat_petr4_treino <- as.matrix(petr4_treino[,-1])
mat_petr4_classe <- as.factor(as.matrix(petr4_treino[,1]))




xgb_petr4 <- train(x = mat_petr4_treino, y = mat_petr4_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_petr4

plot(xgb_petr4)
logit_abev3

xgb_pred_petr4 <- predict(xgb_petr4, petr4_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_petr4, petr4_teste$sentido_PETR4)


# SALVANDO XGB
write.csv(xgb_pred_petr4,"xgb_pred_petr4.csv")

#### VALE3

mat_vale3_treino <- as.matrix(vale3_treino[,-1])
mat_vale3_classe <- as.factor(as.matrix(vale3_treino[,1]))




xgb_vale3 <- train(x = mat_vale3_treino, y = mat_vale3_classe, trControl = ctrl,tuneGrid = tuneGrid_xgb_2, method = "xgbTree")


xgb_vale3

plot(xgb_vale3)


xgb_pred_vale3 <- predict(xgb_vale3, vale3_teste)


### Melhor modelo 

confusionMatrix(xgb_pred_vale3, vale3_teste$sentido_VALE3)


# SALVANDO XGB
write.csv(xgb_pred_itub4,"xgb_pred_itub4.csv")
############## MODELOS RODADOS




############## ENSEMBLE

library(nnet)


df_nnet <- data.frame(abev3_treino[,-1], b3sa3_treino[,-1], bbdc4_treino[,-1],
                      bbas3_treino[,-1], itsa4_treino[,-1], itub4_treino[,-1],
                      petr3_treino[,-1], petr4_treino[,-1], vale3_treino[,-1], ibov_treino[,-1], y =  ibov_treino[,1])



df_nnet_test <- data.frame(abev3_teste[,-1], b3sa3_teste[,-1], bbdc4_teste[,-1],
                           bbas3_teste[,-1], itsa4_teste[,-1], itub4_teste[,-1],
                           petr3_teste[,-1], petr4_teste[,-1], vale3_teste[,-1], ibov_teste[,-1], y =  ibov_teste[,1])



glimpse(df_nnet)

set.seed(10)

fit_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5,
                        summaryFunction = twoClassSummary, classProbs = TRUE)

nnetGrid <- expand.grid(size = seq(from = 1, to = 100, by = 1), 
                        decay = seq(from = 0.01, to =1, by = 0.01))


neural_net <- train(y ~., data = df_nnet, method = "nnet", trainControl = fit_ctrl,
                    tuneGrid = nnetGrid, metric = "Accuracy")

names(df_nnet_test) <- names(df_nnet)



length(c(df_nnet_test))
neural_net  


names(df_nnet)






nnet_pred_ibov <- predict(neural_net, df_nnet_test)

confusionMatrix(nnet_pred_ibov, df_nnet_test$y)




########### APLICaR OS MELHORES MODELOS PARA CADA EMPRESA AFIM DE VERIFICAR COMO IR? AFETAR A NNET


### ABEV3
xgb_abev3_pred <- predict(xgb_abev3, abev3_treino)

xgb_abev3_prob <- predict(xgb_abev3, abev3_teste, type = "prob")
xgb_abev3_prob

roc_xgb <- data.frame(abev3_teste, xgb_abev3_prob)

roc_xgb_abev3 <- roc(data = roc_xgb, predictor =  "X1", response =  "sentido_ABEV3")

roc_xgb_abev3

### PLOTANDO O GR?FICO 
roc_ggplot_abev3 <- ggroc(roc_xgb_abev3, legacy.axes = TRUE)

roc_ggplot_abev3
roc_ggplot_abev3 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC XGB ABEV3") + labs(subtitle = c("AUC: 0.4983" ))
auc(roc_xgb_abev3)



#ROC_B3SA3
knn_b3sa3_pred <- predict(knn_b3sa3, b3sa3_treino)

knn_b3sa3_prob <- predict(knn_b3sa3, b3sa3_teste, type = "prob")

b3sa3_teste

roc_knn <- data.frame(b3sa3_teste, knn_b3sa3_prob)
roc_knn$X0


roc_knn_b3sa3 <- roc(data = roc_knn, predictor =  "X0", response =  "sentido_B3SA3")


roc_ggplot_b3sa3 <- ggroc(roc_knn_b3sa3, legacy.axes = TRUE)

roc_ggplot_b3sa3
roc_ggplot_b3sa3 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC KNN B3SA3") + labs(subtitle = c("AUC: 0.5752 " ))
auc(roc_knn_b3sa3)




### BBAS3

knn_bbas3_pred <- predict(knn_bbas3, bbas3_treino)


knn_bbas3_prob <- predict(knn_bbas3, bbas3_teste, type = "prob")
knn_bbas3_prob

roc_knnBBAS3 <- data.frame(bbas3_teste, knn_bbas3_prob)

roc_knn_bbas3 <- roc(data = roc_knnBBAS3, predictor =  "X1", response =  "sentido_BBAS3")

### PLOTANDO O GR?FICO 
roc_ggplot_bbas3 <- ggroc(roc_knn_bbas3, legacy.axes = TRUE)

roc_ggplot_bbas3 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC KNN BBAS3") + labs(subtitle = c("AUC: 0.5176" ))
auc(roc_knn_bbas3)





#### BBDC4

bag_bbdc4_pred <- predict(bag_bbdc4, bbdc4_treino)


bag_bbdc4_prob <- predict(bag_bbdc4, bbdc4_teste, type = "prob")

roc_bag <- data.frame(bbdc4_teste, bag_bbdc4_prob)

roc_bag_bbdc4 <- roc(data = roc_bag, predictor =  "X1", response =  "sentido_BBDC4")

roc_bag_bbdc4

### PLOTANDO O GR?FICO 
roc_ggplot_bbdc4 <- ggroc(roc_bag_bbdc4, legacy.axes = TRUE)

roc_ggplot_bbdc4 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC BAGGING BBDC4") + labs(subtitle = c("AUC: 0.5273" ))
auc(roc_bag_bbdc4)




ada_itsa4_pred <- predict(ada_itsa4, itsa4_treino)

ada_itsa4_prob <- predict(ada_itsa4, itsa4_teste, type = "prob")

roc_ada <- data.frame(itsa4_teste, ada_itsa4_prob)

roc_ada_itsa4 <- roc(data = roc_ada, predictor =  "X1", response =  "sentido_ITSA4")

roc_ada_itsa4

### PLOTANDO O GR?FICO 
roc_ggplot_itsa4 <- ggroc(roc_ada_itsa4, legacy.axes = TRUE)

roc_ggplot_itsa4 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC ADA ITSA4") + labs(subtitle = c("AUC: 0.5337" ))








knn_itub4_pred <- predict(knn_itub4, itub4_treino)

knn_itub4_prob <- predict(knn_itub4, itub4_teste, type = "prob")

roc_knnItub4 <- data.frame(itub4_teste, knn_itub4_prob)

roc_knn_itub4 <- roc(data = roc_knnItub4, predictor =  "X1", response =  "sentido_ITUB4")

roc_knn_itub4

### PLOTANDO O GR?FICO 
roc_ggplot_itub4 <- ggroc(roc_knn_itub4, legacy.axes = TRUE)

roc_ggplot_itub4 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC KNN ITUB4") + labs(subtitle = c("AUC: 0.51" ))







logit_petr3_pred <- predict(logit_petr3, petr3_treino)


logit_petr3_prob <- predict(logit_petr3, petr3_teste, type = "prob")

roc_logit <- data.frame(petr3_teste, logit_petr3_prob)

roc_logit_petr3 <- roc(data = roc_logit, predictor =  "X1", response =  "sentido_PETR3")

roc_logit_petr3

### PLOTANDO O GR?FICO 
roc_ggplot_petr3 <- ggroc(roc_logit_petr3, legacy.axes = TRUE)

roc_ggplot_petr3 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC REG. LOG?STICA PETR3") + labs(subtitle = c("AUC: 0.5776" ))



svmRad_petr4_pred <- predict(svmRad_petr4, petr4_treino)

svmRad_petr4_prob <- predict(svmRad_petr4, petr4_teste, type = "prob")

roc_svmRadPetr4 <- data.frame(petr4_teste, svmRad_petr4_prob)

roc_logit_petr3 <- roc(data = roc_logit, predictor =  "X1", response =  "sentido_PETR3")

roc_logit_petr3

### PLOTANDO O GR?FICO 
roc_ggplot_petr3 <- ggroc(roc_logit_petr3, legacy.axes = TRUE)

roc_ggplot_petr3 + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed") + ggtitle("ROC REG. LOG?STICA PETR3") + labs(subtitle = c("AUC: 0.5776" ))










svmRad_vale3_pred <- predict(svmRad_vale3, vale3_treino)







#### AGRUPAR OS MELHORES MODELOS NA NNET

df_nnet_models <- data.frame(df_nnet, xgb_abev3_pred, knn_b3sa3_pred,
                             knn_bbas3_pred, bag_bbdc4_pred,
                             ada_itsa4_pred, knn_itub4_pred, logit_petr3_pred, 
                             svmRad_petr4_pred, svmRad_vale3_pred)



df_nnet_testMod <- data.frame(df_nnet_test, xgb_pred_abev3_3, knn_pred_b3sa3,
                              knn_pred_bbas3, bag_pred_bbdc4, ada_pred_itsa4, 
                              knn_pred_itub4, logit_pred_petr3,
                              svmRad_pred_petr4, svmRad_pred_vale3)




glimpse(df_nnet_testMod)
glimpse(df_nnet_models)


names(df_nnet_testMod) <- names(df_nnet_models)

neural_net_models <- train(y ~., data = df_nnet_models, method = "nnet", metric = "Accuracy", trainControl = fit_ctrl,
                           tuneGrid = nnetGrid)


#neural_net <- train(y ~., data = df_nnet, method = "nnet", trainControl = fit_ctrl,
#                    tuneGrid = nnetGrid, metric = "Accuracy")


neural_net_models



plotnet(neural_net_models, y_names = "Sentido_IBOV")

glimpse(df_nnet_testMod)

nnet_pred_ibovMod <- predict(neural_net_models, df_nnet_testMod)

confusionMatrix(nnet_pred_ibovMod, df_nnet_testMod$y)





### Salvar a base de dados de teste e treino

write.csv(df_nnet_models, "df_nnet_models.csv")
write.csv(df_nnet_testMod, "df_nnet_testMod.csv")




############ TESTANDO XGB COMO ENSEMBLE
matXgb_Esemb_train <- as.matrix(data_train[,-4])


matXgb_Esemb_y <- as.matrix((data_train[,4]))
tuneGrid_xgb_3 <- expand.grid(nrounds = c(100,150,200,250),
                              eta = c(0.025,0.05,0.1,0.2),
                              max_depth = c(10,20,30),
                              gamma = c(10,20,30),
                              colsample_bytree = 1,
                              min_child_weight = c(10,20,30),
                              subsample = 0.5)
set.seed(10)

xgb_model <- train(x = data_train, y = data_train$Despesa_Operadora, trControl = ctrl,tuneGrid = tuneGrid_xgb_3, method = "xgbTree", verbose = TRUE)



xgb_model

xgb_pred <- read.csv("xgb_pred.csv")
xgb_pred <- xgb_pred$x

predict_xgb <- predict(xgb_model, data_test)

library(caret)



RMSE(xgb_pred, data_test$Despesa_Operadora)

write.csv(data_train, "data_train.csv")
write.csv(data_test, "data_test.csv")


xgb_ensemb_1

xgb_pred_ibov <- predict(xgb_ensemb_1, df_nnet_test)

confusionMatrix(xgb_pred_ibov, df_nnet_test$y)



########### Testando XGB com modelos

#TRANSFORMANDO FACTOR EM NUMERIC
df_nnet_models1$xgb_abev3_pred <- as.numeric(df_nnet_models1$xgb_abev3_pred) - 1
df_nnet_models1$knn_b3sa3_pred <- as.numeric(df_nnet_models1$knn_b3sa3_pred) - 1
df_nnet_models1$knn_bbas3_pred <- as.numeric(df_nnet_models1$knn_bbas3_pred) - 1
df_nnet_models1$bag_bbdc4_pred <- as.numeric(df_nnet_models1$bag_bbdc4_pred) - 1
df_nnet_models1$ada_itsa4_pred <- as.numeric(df_nnet_models1$ada_itsa4_pred) - 1
df_nnet_models1$knn_itub4_pred <- as.numeric(df_nnet_models1$knn_itub4_pred) - 1
df_nnet_models1$logit_petr3_pred <- as.numeric(df_nnet_models1$logit_petr3_pred) - 1
df_nnet_models1$svmRad_petr4_pred <- as.numeric(df_nnet_models1$svmRad_petr4_pred) - 1
df_nnet_models1$svmRad_vale3_pred <- as.numeric(df_nnet_models1$svmRad_vale3_pred) - 1



matXgb_Esemb_trainMod <- as.matrix(df_nnet_models1[,-61])
  


matXgb_Esemb_factorMod <- as.factor(as.matrix((df_nnet_models1[,61])))


xgb_ensemb_mod <- train(x = matXgb_Esemb_trainMod, y = matXgb_Esemb_factorMod, trControl = ctrl,tuneGrid = tuneGrid_xgb_3, method = "xgbTree")

xgb_df_testMod <- df_nnet_testMod
############### AJUSTANDO A BASE DE TESTE

xgb_df_testMod$xgb_abev3_pred <- as.numeric(xgb_df_testMod$xgb_abev3_pred) - 1
xgb_df_testMod$knn_b3sa3_pred <- as.numeric(xgb_df_testMod$knn_b3sa3_pred) - 1
xgb_df_testMod$knn_bbas3_pred <- as.numeric(xgb_df_testMod$knn_bbas3_pred) - 1
xgb_df_testMod$bag_bbdc4_pred <- as.numeric(xgb_df_testMod$bag_bbdc4_pred) - 1
xgb_df_testMod$ada_itsa4_pred <- as.numeric(xgb_df_testMod$ada_itsa4_pred) - 1
xgb_df_testMod$knn_itub4_pred <- as.numeric(xgb_df_testMod$knn_itub4_pred) - 1
xgb_df_testMod$logit_petr3_pred <- as.numeric(xgb_df_testMod$logit_petr3_pred) - 1
xgb_df_testMod$svmRad_petr4_pred <- as.numeric(xgb_df_testMod$svmRad_petr4_pred) - 1
xgb_df_testMod$svmRad_vale3_pred <- as.numeric(xgb_df_testMod$svmRad_vale3_pred) - 1





xgb_pred_ibovMod <- predict(xgb_ensemb_mod, xgb_df_testMod)









confusionMatrix(xgb_pred_ibov, xgb_df_testMod$y)

