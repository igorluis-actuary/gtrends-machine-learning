
### Carregando os pacotes
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(readr)
library(caret)

#Definindo Diretório

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

# transformando a variÃ¡vel volume em ln(volume)
df <- df[,volume := log(volume)]

# criar variavel lag_fechamento by ativo/time
df <- df[, lag_fechamento:=c(NA, fechamento[-.N]), by = ativo]
df <- df[, lag_volume:=c(NA,volume[-.N]), by = ativo]
df <- df[, lag_value:=c(NA, value[-.N]), by = ativo]
df <- df[, lag_abertura:=c(NA, abertura[-.N]), by = ativo]
df <- df[, lag_retorno:=c(NA, retorno[-.N]), by = ativo]
# se fechamento > lag_fechamento, sentido == 1; se fechamento == lag_fechamento, sentido == 0; se fechamento < lag_fechamento, sentido == -1
df <- df[, sentido := ifelse(fechamento > lag_fechamento, 1, 
                               ifelse(fechamento == lag_fechamento, 0, -1)), by = ativo]

# excluindo NA
df <- na.omit(df)

# transformando sentido em factor
df <- df[, sentido := as.factor(sentido)]

# excluindo sentido_ret
df <- df[, sentido_ret := NULL]

# visualizando as estatÃ?sticas por ano e ativo
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
df_wider <- pivot_wider(df, id_cols = 'date', names_from = 'ativo', values_from = c('retorno', 'fechamento', 'abertura', 'volume', 'value', 'sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno" ))

glimpse(df_wider)

str(df)
#Separando dados em treino e em teste

#TESTE
# excluindo as colunas Dia, Ano, Semana e lag_fechamento
teste <- df[,(c(7:9,11)):= NULL]



# selecionando o ano de 2018
teste <- df[year(date) == 2018]
str(teste)

#TREINO
treino <- df[year(date) != 2018]
str(treino)


#Selecionando a base de treino para cada ativo

train.abev3 <- treino[ativo == "ABEV3"]
train.b3sa3 <- treino[ativo == "B3SA3"]
train.bbdc4 <- treino[ativo == "BBDC4"]
train.bbas3 <- treino[ativo == "BBAS3"]
train.itsa4 <- treino[ativo == "ITSA4"]
train.itub4 <- treino[ativo == "ITUB4"]
train.petr3 <- treino[ativo == "PETR3"]
train.petr4 <- treino[ativo == "PETR4"]
train.vale3 <- treino[ativo == "VALE3"]

# Transformando os DF em painel largo


abev3_widertrain <- pivot_wider(train.abev3, id_cols = 'date', names_from = 'ativo', values_from = c('sentido', "lag_fechamento", "lag_abertura", "lag_volume", "lag_retorno", "lag_value"))




glimpse(abev3_widertrain)

#Criando o DIFF
abev3_widertrain$lag_diff <- abev3_widertrain$lag_fechamento_ABEV3 - abev3_widertrain$lag_abertura_ABEV3


# using glm() function 
formula = sentido_ABEV3 ~ lag_retorno_ABEV3 + lag_value_ABEV3 + lag_diff + lag_volume_ABEV3

modelLookup("glm")

logit_abev3 <- train(method = "glm",sentido_ABEV3 ~., data = abev3_widertrain)


summary(logit_abev3)





a




write.csv(abev3_widertrain, "abev3_treino.csv")
write.csv(treino, "treino.csv")












