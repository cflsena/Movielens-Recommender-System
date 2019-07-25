## Set diretorio de trabalho
## Alterar para o diretorio que estiver os arquivos .r
setwd("/home/cflsena/Documentos/Github/R/Movielens Recommender System")

## Lista de pacotes utilizados
## NLP: para tarefas de NLP
## stringr: manipulacao de strings
## SnowballC: algoritmo de stemming
## data.frame: manipular grandes massas de dados
## Pacote recommenderlab: para tarefas de recomendacao: ...
list_of_packages <- c("NLP", "stringr", "SnowballC", "data.table", "recommenderlab")

## Retorna uma lista de pacotes para NLP nao instalados
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

## Instala os novos pacotes para tarefas de NLP
if(length(new_packages)) 
  install.packages(new_packages)

## Reinicia a sessao do R (necessario para o pacote rJava)
## apos reiniciar a sessao, comendar a linha abaixo e continuar
## e continuar a partir do carregamento dos pacotes 
##.rs.restartR()

## Carrega os pacotes necessarios
library(rJava)
library(NLP)
library(stringr)
library(SnowballC)
library(data.table)
library(recommenderlab)

## Le os datasets de filmes como linhas inteiras e altera o caracter separador para '\t'
movies_1m <- readLines("datasets/1m/movies.dat")
movies_1m <- gsub("::", "\t", movies_1m)

ratings_1m <- readLines("datasets/1m/ratings.dat")
ratings_1m <- gsub("::", "\t", ratings_1m)

users_1m <- readLines("datasets/1m/users.dat")
users_1m <- gsub("::", "\t", users_1m)

## Carrega os datasets movielens 1m e transforma em um data.frame
movies_1m <- data.frame(fread(as.String(movies_1m), sep = "\t", col.names = c("MovieID", "Title", "Genres")))
ratings_1m <- data.frame(fread(as.String(ratings_1m), sep = "\t", col.names = c("UserID", "MovieID", 
                                                                     "Rating","Timestamp")))
users_1m <- data.frame(fread(as.String(users_1m), sep = "\t", col.names = c("UserID", "Gender", "Age", 
                                                                 "Occupation", "Zip-code")))

## Analise Exploratoria

## Caracteristicas dataset movies_1m

## Verifica os 6 primeiros registros do dataset
## Visualmente percebe-se que o ano do lancamento filme faz parte do titulo
## deveria ser outro atributo, possibilitaria verificar se o ano do filme 
## influencia na recomendacao

## atributo Genres indica que o filme possui varios generos
## conforme arquivo readme do proprio dataset
## esse valores serao transformados em atributos binarios (0,1)
## 1 - indica que pertence aquele genero; 0 - nao pertence

head(movies_1m)

## Novos atributos do tipo genero

## seta em uma variavel todos os generos dos filmes
genres <- movies_1m$Genres

## quebra a lista de generos separados por pipe em generos individualmente
## e transforma a lista de generos individuais em uma array
genres_filtered <- unlist(strsplit(genres,"|", fixed = T))

## quantidade de classificacoes de generos
qt_classification <- sort(table(genres_filtered), decreasing = T)
qt_classification

## remove as repeticoes do array
genres_filtered <- sort(genres_filtered[!duplicated(genres_filtered)])

## descricao de todos os generos filtrados que coincidem com os generos descritos no readme
genres_filtered

## Adiciona os novos atributos ao data.frame inicializados com 'NA'
movies_1m[genres_filtered] <-NA

## Seta 1 para filmes que possuem o genero X e 0 para os que nao tem
movies_1m$Action <- ifelse(str_detect("Action", genres), 1, 0)
movies_1m$Animation <- ifelse(str_detect("Animation", genres), 1, 0)
movies_1m$Adventure <- ifelse(str_detect("Adventure", genres), 1, 0)
movies_1m$`Children's` <- ifelse(str_detect("`Children's`", genres), 1, 0)
movies_1m$Comedy <- ifelse(str_detect("Comedy", genres), 1, 0)
movies_1m$Crime <- ifelse(str_detect("Crime", genres), 1, 0)
movies_1m$Documentary <- ifelse(str_detect("Documentary", genres), 1, 0)
movies_1m$Drama <- ifelse(str_detect("Drama", genres), 1, 0)
movies_1m$Fantasy <- ifelse(str_detect("Fantasy", genres), 1, 0)
movies_1m$`Film-Noir` <- ifelse(str_detect("`Film-Noir`", genres), 1, 0)
movies_1m$Horror <- ifelse(str_detect("Horror", genres), 1, 0)
movies_1m$Musical <- ifelse(str_detect("Musical", genres), 1, 0)
movies_1m$Mystery <- ifelse(str_detect("Mystery", genres), 1, 0)
movies_1m$Romance <- ifelse(str_detect("Romance", genres), 1, 0)
movies_1m$`Sci-Fi` <- ifelse(str_detect("`Sci-Fi`", genres), 1, 0)
movies_1m$Thriller <- ifelse(str_detect("Thriller", genres), 1, 0)
movies_1m$War <- ifelse(str_detect("War", genres), 1, 0)
movies_1m$Western <- ifelse(str_detect("Western", genres), 1, 0)

## remove a coluna Genres que nao eh mais necessaria
movies_1m <- subset(movies_1m, select = -Genres)

## exibe o dataset com os novos atributos referentes a genero
View(movies_1m)

# Novo atributo ano - on doing