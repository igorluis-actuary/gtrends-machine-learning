
### Carregando os pacotes
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(readr)
library(caret)
library(nnet)
#Definindo Diretório

setwd("C:\\Users\\igorl\\Documents\\UFPB\\Igor\\P10\\TCC 2\\Dados\\Script e dados")


#Coletar somente o treino da abev3

abev3 <- read.csv("abev3_treino.csv")

abev3$X <- NULL


#Criando função para normalizar

normalize_fun <- function(x){
  (x-min(x))/(max(x)-min(x))
}


abev3$lag_fechamento_ABEV3 <- normalize_fun(abev3$lag_fechamento_ABEV3)
abev3$lag_abertura_ABEV3 <- normalize_fun(abev3$lag_abertura_ABEV3)
abev3$lag_volume_ABEV3 <- normalize_fun(abev3$lag_volume_ABEV3)
abev3$lag_retorno_ABEV3 <- normalize_fun(abev3$lag_retorno_ABEV3)
abev3$lag_value_ABEV3 <- normalize_fun(abev3$lag_value_ABEV3)
#modulo diff
abev3$lag_diff <- abs(abev3$lag_diff)
abev3$lag_diff <- normalize_fun(abev3$lag_diff)
abev3$lag_diff


## regressão logística pelo caret

ctrl <- trainControl(method="repeatedcv", c(repeats = 3), number = 10)
logit.abev3Car <- train(sentido_ABEV3 ~ lag_retorno_ABEV3 + lag_value_ABEV3 + lag_diff + lag_volume_ABEV3
                        , data = abev3, method = "glm", trControl = ctrl)
summary(logit.abev3Car)
## regressão logistica pelo nnet

logit.abev3 <- multinom(sentido_ABEV3 ~ lag_retorno_ABEV3 + lag_value_ABEV3 + lag_diff + lag_volume_ABEV3
                        , data = abev3)
summary(logit.abev3)
