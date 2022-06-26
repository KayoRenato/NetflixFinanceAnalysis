# Problema de Negocio - Analistar os dados financeiro globais da Netflix e identificar padroes.

install.packages("readxl")

#Carregar os pacotes - Caso nao tenha o pacote instalado na maquina, sera necessario a instalacao antes da carga.
library(dplyr)
library(tidyr)
library(readxl)
library(readr)

getwd()

#Dados do faturamento da netflix
df_netflix <- read.csv("dados_netflix_Dec_2021.csv", stringsAsFactors = TRUE)
str(df_netflix)
summary(df_netflix)
View(df_netflix)

#Dados do World Bank
df_pib <- read.csv("dados_world_bank.csv", skip = 3 , stringsAsFactors = TRUE)
str(df_pib)
summary(df_pib)
View(df_pib)

#Dados de desiguldade salarial
df_salario <- read.csv("dados_desigualdade_salarial_harvard.csv", stringsAsFactors = TRUE)
str(df_salario)
summary(df_salario)
View(df_salario)


# brazil_salario <- subset(df_salario, country == 'Brazil' )
# View(brazil_salario)
# plot(x=brazil_salario$year, y=brazil_salario$gini_disp)

# US_salario <- subset(df_salario, country == 'United States' )
# View(US_salario)
# plot(x=US_salario$year, y=US_salario$gini_disp)

# country_salario <- plot(x = df_salario$country, )
# country_salario


#Dados dos titulos no IMDB
df_IMDB <- read_tsv("title.basics.tsv")
str(df_IMDB)
summary(df_IMDB)
View(head(df_IMDB))



#Dados dos Top 10 shows da Netflix por pais
df_top10 <- read_excel("top_10_shows_netflix.xlsx")
str(df_top10)
summary(df_top10)
View(df_top10)


#Dados de assinantes da Netfliz em Jul/2021
df_sub <- read.csv("assinantes_netflix_jul_2021.csv", stringsAsFactors = TRUE)
str(df_sub)
summary(df_sub)
View(df_sub)

#Codigos dos paises
df_countryCod <- read.csv("wikipedia-iso-country-codes.csv", stringsAsFactors = TRUE)
str(df_countryCod)
summary(df_countryCod)
View(df_countryCod)


#-------------------- Limpeza dos Dados ------------------

#-------------------- dataset 01

#Coluna com a diferenca de planos standard - basico
df_netflix$basic_standard_diff = (df_netflix$Cost.Per.Month...Standard.... - df_netflix$Cost.Per.Month...Basic....) 


#Coluna com a diferenca de planos premium ~ standard
df_netflix$standard_premiuma_diff = (df_netflix$Cost.Per.Month...Premium.... - df_netflix$Cost.Per.Month...Standard....)

View(df_netflix)

names(df_pib)[names(df_pib) == "Country.Name"] <- "Country"
View(df_pib)

#Intersecao de dados entre netflix e pib por paises (inner join)  netflix[7](58)[208]pib - rows
df_netflix_pib <- merge(df_netflix, df_pib, by="Country")
str(df_netflix_pib)
summary(df_netflix_pib)
View(df_netflix_pib)

#Extrair o PIB de 2020
df_netflix_pib_2020 <- df_netflix_pib[-c(11:73, 75)]
str(df_netflix_pib_2020)
View(df_netflix_pib_2020)
names(df_netflix_pib_2020)[names(df_netflix_pib_2020)== 'X2020'] <- "2020 GDP (World Bank)"
View(df_netflix_pib_2020)

#Limpeza do df de desigualdade social
df_salario <- df_salario[, c(1:3)]
str(df_salario)
summary(df_salario)
View(df_salario)

#Selecionar o ano em que o pais apresentou o maior valor de salario
df_salario_ano <- df_salario %>% group_by(country) %>% summarise(max = max(year, na.rm = TRUE))
str(df_salario_ano)
summary(df_salario_ano)
View(df_salario_ano)

#Extraindo apenas o df que apresenta o maior valor de salario para cada pais
df_salario <- merge(df_salario, df_salario_ano, by.x=c("country", "year"), by.y = c("country", "max"))
str(df_salario)
summary(df_salario)
View(df_salario)

anos<-factor(df_salario$year)
str(anos)
plot(anos)

#Combinacao de df (?????)
df_netflix_pib_salario2020 <- merge(df_netflix_pib_2020, df_salario, by.x = c("Country"), by.y = c("country"))
str(df_netflix_pib_salario2020)
summary(df_netflix_pib_salario2020)
View(df_netflix_pib_salario2020)

#Limpeza do df_sub para combinacao com o df anterior
df_sub <- df_sub[,c(1, 23, 24)]
names(df_sub)[names(df_sub)== 'X..of.Subscribers.Q4.2021..Estimate.'] <- "Subscribers.Q4.2021"
names(df_sub)[names(df_sub)== 'Q4.2021.Revenue....Estimate.'] <- "Revenue.Q4.2021"
View(df_sub)

df_01 <- merge(df_netflix_pib_salario2020, df_sub, by.y = c("Country"))
str(df_01)
summary(df_01)
View(df_01)


# Merge do df de codigo pais para o choropleth map
df_countryCod <- df_countryCod[, c(1,3)]
df_01 <- merge(df_01, df_countryCod, by.x = c("Country"), by.y = c("English.short.name.lower.case"))
str(df_01)
summary(df_01)
View(df_01)

#Salvar df limpo
write.csv(df_01, "dataset01_clear.csv", row.names = FALSE)


#-------------------- dataset 02

df_genero <- df_IMDB[, -c(1, 4:8)]
names(df_genero)[names(df_genero) == 'primaryTitle'] <- 'show_title'
str(df_genero)
summary(df_genero)
View(df_genero)


# Associar o genero com os tops 10 shows
df_top_genero <- merge(df_top10, df_genero, by = "show_title")
str(df_top_genero)
summary(df_top_genero)
View(df_top_genero)

# Manter apenas 1 entrada para cada top 10
df_top_genero <- df_top_genero[(df_top_genero$category == 'Films' & df_top_genero$titleType == 'movie') | (df_top_genero$category == 'TV' & df_top_genero$titleType == 'tvSeries'), ]
str(df_top_genero)
summary(df_top_genero)
View(df_top_genero)


df_top_genero <- distinct(df_top_genero, df_top_genero$show_title, df_top_genero$week, df_top_genero$country_name, df_top_genero$category, df_top_genero$titleType, df_top_genero$cumulative_weeks_in_top_10, .keep_all = TRUE) 
str(df_top_genero)
summary(df_top_genero)
View(df_top_genero)

# Manter apenas a informacai de genero de filme por pais
df_top_genero_pais <- df_top_genero[, -c(1, 3:9)]
str(df_top_genero_pais)
summary(df_top_genero_pais)
View(df_top_genero_pais)

#Pivot do df

df_top_genero_pais <- separate(df_top_genero_pais, c("genres"), c("genres1", "genres2", "genres3"), sep = ",")
str(df_top_genero_pais)
summary(df_top_genero_pais)
View(df_top_genero_pais)

df_top_genero_pais <- pivot_longer(df_top_genero_pais, c("genres1", "genres2", "genres3"), names_to = "genres123", values_to = "genres")
str(df_top_genero_pais)
summary(df_top_genero_pais)
View(df_top_genero_pais)

df_count_genres <- count(df_top_genero_pais, df_top_genero_pais$country_name, df_top_genero_pais$genres)
View(df_count_genres)

df_count_genres <- na.omit(df_count_genres)
View(df_count_genres)


df_count_genres <- subset(df_count_genres, df_count_genres$`df_top_genero_pais$genres` != "\\N")
df_count_genres$n <- as.numeric(df_count_genres$n)
str(df_count_genres)
View(df_count_genres)

#Salvar df limpo
write.csv(df_count_genres, "dataset02_clear.csv", row.names = FALSE)


#-------------------- dataset 03


# Renomer df anterior
df_sun_burst <- rename(df_count_genres, label = country_name)
names(df_sun_burst) <- c("label", "genres", "n")
View(df_sun_burst)

# Removendo o '-' dos generos
df_sun_burst$genres <- sub("-", " ", df_sun_burst$genres)
View(df_sun_burst)

# Ajuste de nome

df_sun_burst$parent = c("Total - ")
df_sun_burst$parent <- paste(df_sun_burst$parant, df_sun_burst$genres)
df_sun_burst$id = c(" - ")
df_sun_burst$id <- paste(df_sun_burst$parent, df_sun_burst$id)
df_sun_burst$id <- paste(df_sun_burst$id, df_sun_burst$label)
str(df_sun_burst)
View(df_sun_burst)


# Agregacao 

df_agg <- aggregate(df_sun_burst$n, list(df_sun_burst$genres), FUN=sum)
View(df_agg)

names(df_agg) <- c("label", "n")
str(df_agg)
View(df_agg)

df_agg$genres <- c(NA)
df_agg$parent <- c("Total")
df_agg$id <- c(" - ")
df_agg$id <- paste(df_agg$parent, df_agg$id)
df_agg$id <- paste(df_agg$id, df_agg$label)
str(df_agg)
View(df_agg)

# Calculo Soma

total = sum(df_agg$n)
total

# Combinando em um df final
df_sun_burst <- rbind(df_agg, df_sun_burst)
View(df_sun_burst)

df_sun_burst <- rbind(c("Total", total, NA, NA, "Total"), df_sun_burst)
View(df_sun_burst)

df_sun_burst <- df_sun_burst[,-c(3)]
View(df_sun_burst)
df_sun_burst$n <- as.numeric(df_sun_burst$n)
str(df_sun_burst)
View(df_sun_burst)

#Salvar df limpo
write.csv(df_count_genres, "dataset03_clear.csv", row.names = FALSE)
