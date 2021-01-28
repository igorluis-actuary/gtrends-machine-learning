#### XGBOOST

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
library(xgboost)
library(doParallel)
#Definindo Diret?rio

setwd("C:\\Users\\igorl\\Documents\\UFPB\\Igor\\P10\\TCC 2\\Dados\\Script e dados")

#Coletar somente o treino da abev3

treino <- read.csv("abev3_treinoFinal.csv")
glimpse(treino)
teste <- read_csv('abev3_testFinal.csv')
glimpse(teste)

# selecionando as variÃ¡veis
treino <- treino %>% 
  select(-X, -date)
teste <- teste %>% 
  select(-X1, -date)

# transformando sentido em factor
treino$sentido_ABEV3 <- as.factor(treino$sentido_ABEV3)
teste$sentido_ABEV3 <- as.factor(teste$sentido_ABEV3)

# visualizar
glimpse(treino)
glimpse(teste)

# Ajustar o lag_diff para treino e teste

treino$lag_diff_ABEV3 <- treino$lag_fechamento_ABEV3 - treino$lag_abertura_ABEV3
head(treino$lag_diff_ABEV3)
teste$lag_diff_ABEV3 <- teste$lag_fechamento_ABEV3 - teste$lag_abertura_ABEV3
teste




# Setar a semente

set.seed(9)

# Configurar o processamento paralelo
# Somente se o modelo for demorar demais, se não não utilizar
#cores <- 7
#registerDoParallel(cores = cores)




####### Random forest











######## xgBoost

## Preparando a base de dados:
# Convertendo o sentido para numérico
mat.classe<-as.matrix(as.integer(treino$sentido_ABEV3)-1)
mat.classe

## convertendo ambas as bases para a forma matricial
#convertendo
mat_training <- data.matrix(treino)

mat_test <- data.matrix(teste)
mat_test
mat_test[,1] <- ifelse(mat_test[,1] == 2,1,0)
mat_training[,1] <- ifelse(mat_training[,1] == 2,1,0)

class(mat_training)
str(mat_training)
# Outra maneira de converter para matrix
mat_training <- apply(as.matrix.noquote(treino),2,as.numeric)
mat_test <- apply(as.matrix.noquote(teste), 2, as.numeric)
str(mat_training)


## Rodando o XGBOOST

hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  accuracy = 0,          # a place to dump accuracy results
  trees = 0,          # a place to dump required number of trees
  recall = 0
)
for(i in seq_len(nrow(hyper_grid))) {
  set.seed(9)
  m <- xgb.cv(
    data = mat_training,
    label = mat.classe,
    nrounds = 4000,
    objective = "reg:logistic",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ),metrics = list("error", "auc") 
  )
  }








