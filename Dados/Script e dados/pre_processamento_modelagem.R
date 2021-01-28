
### Carregando os pacotes
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(readr)
library(caret)

#Definindo Diret?rio

setwd("C:\\Users\\igorl\\Documents\\UFPB\\Igor\\P10\\TCC 2\\Dados\\Script e dados")

### Importando os dados

dados <- read_csv("dados_todos.csv")

# visualizando a estrutura dos dados
str(dados)

# transformando em data.table
df <- data.table(dados)

# removendo a coluna X1
df <- df[,X1:= NULL]

str(df)

# transformando as colunas 'ativo', 'sentido_ret' e 'Dia' para factor
names_factor <- c('ativo', 'sentido_ret','Dia')
for (col in names_factor){
  e = substitute(X := as.factor(X), list(X = as.symbol(col)))
  df[, eval(e)]
}

str(df)

# transformando a variável volume em ln(volume)
df <- df[,volume := log(volume)]

head(df)

# criar variavel lag_fechamento by ativo/time
df <- df[, lag_fechamento:=c(NA, fechamento[-.N]), by = ativo]
df <- df[, lag_volume:=c(NA,volume[-.N]), by = ativo]
df <- df[, lag_value:=c(NA, value[-.N]), by = ativo]
df <- df[, lag_abertura:=c(NA, abertura[-.N]), by = ativo]
df <- df[, lag_retorno:=c(NA, retorno[-.N]), by = ativo]








#retirar o ibov do df


                          #teste_final <- pivot_wider(teste, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value","lag_diff"))



str(df)
# se fechamento > lag_fechamento, sentido == 1; se fechamento == lag_fechamento, sentido == 0; se fechamento < lag_fechamento, sentido == -1

df <- df[, sentido := ifelse(fechamento > lag_fechamento, 1, 0)]
str(df)
# excluindo NA
df <- na.omit(df)



# transformando sentido em factor
df <- df[, sentido := as.factor(sentido)]

# excluindo sentido_ret
df <- df[, sentido_ret := NULL]

# visualizando as estat??sticas por ano e ativo
estatisticas_ano_ativo <- df[,list(ret_medio=mean(retorno), ret_mediano = median(retorno), ret_sd = sd(retorno), ret_min = min(retorno), ret_max = max(retorno),
                                   volume_medio=mean(volume), volume_mediano = median(volume), volume_sd = sd(volume), volume_min = min(volume), volume_max = max(volume),
                                   value_medio=mean(value), value_mediano = median(value), value_sd = sd(value), value_min = min(value), value_max = max(value)),
                             by=list(Ano,ativo)]
# salvando em .csv
write_csv(x = estatisticas_ano_ativo, path = 'estatisticas_ano_ativo.csv')

# visualizando as estatisticas por ativo
estatisticas_ativo <- df[,list(ret_medio=mean(retorno), ret_mediano = median(retorno), ret_sd = sd(retorno), ret_min = min(retorno), ret_max = max(retorno),
                               volume_medio=mean(volume), volume_mediano = median(volume), volume_sd = sd(volume), volume_min = min(volume), volume_max = max(volume),
                               value_medio=mean(value), value_mediano = median(value), value_sd = sd(value), value_min = min(value), value_max = max(value)),
                         by=list(ativo)]

# salvando em .csv
write_csv(x = estatisticas_ativo, path = 'estatisticas_ativo.csv')

# Contar a quantidade de 1, 0 e -1 por ativo
sentido_count_ativo <- df %>% 
  group_by(ativo) %>%
  count(sentido)

# salvar 
write_csv(sentido_count_ativo, path= 'sentido_count_ativo.csv')

# Contar a quantidade de 1, 0 e -1 por Ano/ativo
sentido_count_ano_ativo <- df %>% 
  group_by(Ano, ativo) %>%
  count(sentido)

write_csv(sentido_count_ano_ativo, path= 'sentido_count_ano_ativo.csv')

# Transformando painel longo em largo

df_wider <- pivot_wider(df, id_cols = 'date', names_from = 'ativo', values_from = c('retorno', 'fechamento', 'abertura', 'volume', 'value', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno" ))

glimpse(df_wider)
str(df)
head(df_wider$retorno_IBOV)
dados %>% filter(ativo =="IBOV")

#Separando dados em treino e em teste

#TESTE
# excluindo as colunas Dia, Ano, Semana e lag_fechamento
teste <- df[,(c(7:9)):= NULL]


df$sentido
# selecionando o ano de 2018
teste_pre <- df[year(date) == 2018]
teste_pre$sentido
treino_pre <- df[year(date) !=2018]
str(teste_pre)
teste_pre$sentido
glimpse(teste_pre)

#teste_final <- pivot_wider(teste, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value","lag_diff"))


#write.csv(treino_final,"treino_final_trab.csv")
#write.csv(teste_final, "teste_final_trab.csv")

#Selecionando a base de treino para cada ativo

train_abev3 <- treino[ativo == "ABEV3"]
test_abev3 <- teste[ativo == "ABEV3"]

train_b3sa3 <- treino_pre[ativo == "B3SA3"]
test_b3sa3 <- teste_pre[ativo == "B3SA3"]

train_bbdc4 <- treino_pre[ativo == "BBDC4"]
test_bbdc4 <- teste_pre[ativo == "BBDC4"]

train_bbas3 <- treino_pre[ativo == "BBAS3"]
test_bbas3 <- teste_pre[ativo == "BBAS3"]

train_itsa4 <- treino_pre[ativo == "ITSA4"]
test_itsa4 <- teste_pre[ativo == "ITSA4"]

train_itub4 <- treino_pre[ativo == "ITUB4"]
test_itub4 <- teste_pre[ativo == "ITUB4"]

train_petr3 <- treino_pre[ativo == "PETR3"]
test_petr3 <- teste_pre[ativo == "PETR3"]

train_petr4 <- treino_pre[ativo == "PETR4"]
test_petr4 <- teste_pre[ativo == "PETR4"]

train_vale3 <- treino_pre[ativo == "VALE3"]
test_vale3 <- teste_pre[ativo == "VALE3"]

train_ibov <- treino_pre[ativo == "IBOV"]
test_ibov <- teste_pre[ativo == "IBOV"]

# Transformando os DF de treino em painel largo


abev3_treino <- pivot_wider(train_abev3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value", "lag_diff"))
b3sa3_treino <- pivot_wider(train_b3sa3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
bbdc4_treino <- pivot_wider(train_bbdc4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
bbas3_treino <- pivot_wider(train_bbas3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
itsa4_treino <- pivot_wider(train_itsa4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
itub4_treino <- pivot_wider(train_itub4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
petr3_treino <- pivot_wider(train_petr3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
petr4_treino <- pivot_wider(train_petr4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
vale3_treino <- pivot_wider(train_vale3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value")) 
ibov_treino <- pivot_wider(train_ibov, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))

#### teste

abev3_test <- pivot_wider(test_abev3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value", "lag_diff"))

b3sa3_teste <- pivot_wider(test_b3sa3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
bbdc4_teste <- pivot_wider(test_bbdc4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
bbas3_teste <- pivot_wider(test_bbas3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
itsa4_teste <- pivot_wider(test_itsa4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
itub4_teste <- pivot_wider(test_itub4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
petr3_teste <- pivot_wider(test_petr3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
petr4_teste <- pivot_wider(test_petr4, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))
vale3_teste <- pivot_wider(test_vale3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value")) 
ibov_teste <- pivot_wider(test_ibov, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))


# Criando o DIFF







# Treino
write.csv(abev3_treino, "abev3_treinoFinal.csv")
write.csv(b3sa3_treino, "b3sa3_treinoFinal.csv")
write.csv(bbas3_treino, "bbas3_treinoFinal.csv")
write.csv(bbdc4_treino, "bbdc4_treinoFinal.csv")
write.csv(ibov_treino, "ibov_treinoFinal.csv")
write.csv(itsa4_treino, "itsa4_treinoFinal.csv")
write.csv(itub4_treino, "itub4_treinoFinal.csv")
write.csv(petr3_treino, "petr3_treinoFinal.csv")
write.csv(petr4_treino, "petr4_treinoFinal.csv")
write.csv(vale3_treino, "vale3_treinoFinal.csv")

# Teste
write.csv(abev3_test, "abev3_testFinal.csv")
write.csv(b3sa3_teste, "b3sa3_testeFinal.csv")
write.csv(bbas3_teste, "bbas3_testeFinal.csv")
write.csv(bbdc4_teste, "bbdc4_testeFinal.csv")
write.csv(ibov_teste, "ibov_testeFinal.csv")
write.csv(itsa4_teste, "itsa4_testeFinal.csv")
write.csv(itub4_teste, "itub4_testeFinal.csv")
write.csv(petr3_teste, "petr3_testeFinal.csv")
write.csv(petr4_teste, "petr4_testeFinal.csv")
write.csv(vale3_teste, "vale3_testeFinal.csv")


abev3_treino
abev3_test
glimpse(abev3_widertrain)

#Criando o DIFF


# using glm() function 
formula = sentido_ABEV3 ~ lag_retorno_ABEV3 + lag_value_ABEV3 + lag_diff + lag_volume_ABEV3

modelLookup("glm")

logit_abev3 <- train(method = "glm",sentido_ABEV3 ~., data = abev3_widertrain)


summary(logit_abev3)









write.csv(abev3_widertrain, "abev3_treino.csv")
write.csv(treino, "treino.csv")

#Coletar somente o treino da abev3

abev3 <- read.csv("abev3_treino.csv")

abev3$X <- NULL


#Criando fun??o para normalizar

normalize_fun <- function(x){
  (x-min(x))/(max(x)-min(x))
}


abev3$lag_fechamento_ABEV3 <- normalize_fun(abev3$lag_fechamento_ABEV3)
abev3$lag_abertura_ABEV3 <- normalize_fun(abev3$lag_abertura_ABEV3)
abev3$lag_volume_ABEV3 <- normalize_fun(abev3$lag_volume_ABEV3)
abev3$lag_retorno_ABEV3 <- normalize_fun(abev3$lag_retorno_ABEV3)
abev3$lag_value_ABEV3 <- normalize_fun(abev3$lag_value_ABEV3)
abev3$lag_diff <- abs(abev3$lag_diff) #Modulo diff
abev3$lag_diff <- normalize_fun(abev3$lag_diff)
abev3$lag_diff


# Transformar factor -1 em zero


a <- abev3 %>% mutate(sentido_ABEV3 == "-1" & sentido_ABEV3 == "0", "0","1")
require(data.table)

abev3DT <- data.table(abev3)

abev3DT[, sentido_ABEV3 := ifelse(sentido_ABEV3 == -1 | sentido_ABEV3 == 0, 0, 1)]
ifelse

summary(abev3DT$sentido_ABEV3)

abev3DT$sentido_ABEV3 <- as.factor(abev3DT$sentido_ABEV3)
abev3DT$date <- as.Date(abev3DT$date)
glimpse(abev3DT)


# Modelar logistica


logit_abev3 <- train(method = "glm", sentido_ABEV3 ~ lag_retorno_ABEV3 + lag_value_ABEV3 + lag_diff + lag_volume_ABEV3
, data = abev3, family = binomial)
summary(logit_abev3)






##########
##library("nnet")

logit_abev3.1 <- multinom(sentido_ABEV3 ~ lag_retorno_ABEV3 + lag_value_ABEV3 + lag_diff + lag_volume_ABEV3,
                  data = abev3[,-1], contrasts = abev3$sentido_ABEV3 == "1")

summary(logit_abev3.1)






# KNN

ctrl <- trainControl(method="repeatedcv", c(repeats = 5), number = 10)

knnFitAbev3DT <- train(method = "knn", sentido_ABEV3 ~ lag_retorno_ABEV3 + lag_value_ABEV3 + lag_diff + lag_volume_ABEV3
                , data = abev3DT,trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)



knnFitAbev3DT


plot(knnFitAbev3DT)
