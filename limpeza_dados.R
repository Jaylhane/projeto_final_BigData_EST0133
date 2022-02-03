# Tratando os dados 
# Data: 27/01/2022

#####Bibliotecas#####

library(tidyverse)
#library(gender)
#library(genderdata)
#check_genderdata_package()
#install_genderdata_package()
library(lubridate)


#####Carregando os dados#####

livros <- read.csv("./Conjunto de Dados/books.csv",
                   encoding = "UTF-8",
                   header = TRUE) %>% 
  select(-bookID, 
         -title, 
         -isbn, 
         -isbn13) %>% 
  na.omit()

#####Tratando os dados#####

summary(as.factor(livros$language_code))

linhas_invalidas_de_language_code<- livros %>% 
  filter(language_code=="9780674842113"|
           language_code=="9780851742717"|
           language_code=="9781563841552"|
           language_code=="9781593600112"|
           language_code=="")

livros <- livros %>% 
  filter(!(language_code%in%linhas_invalidas_de_language_code$language_code))

rm(linhas_invalidas_de_language_code)

# autores <- livros %>% 
#  select(authors) %>% 
#  unique()

# genero <- gender(autores$authors,
#                 countries = c("United States",
#                               "Canada",
#                               "United Kingdom")) %>% 
#  select(name, gender) %>% 
#  distinct()

#rm(autores,genero)
# Não funcionou, pois há mais de 6k autores e ele só identificou 4,
# dessa forma, iremos seguir coma análise sem a variável gênero. 

summary(as.factor(livros$publisher))

# tem alguns nomes de editoras em chines, iremos evitar transformar esses dados em
# fator para não prejudicar essa informação

## Atualização quanto ao `publisher`, é melhor remover essa variável pois haverá
## muitas categorias, exigindo muito processamento computacional na árvore de decisão

#####Verificando a lingua#####

livros %>% 
  ggplot(aes(language_code))+
  geom_bar()+
  coord_flip()

## Como há muito pouca variação linguistica comparado ao grupo inglês, 
## dividiremos a categoria de `language_code` em duas: inglês e não inglês


livros <- livros %>% 
  mutate(publication_date = mdy(publication_date),
         average_rating = as.double(average_rating),
         num_pages = as.integer(num_pages),
         book_age = year(today())-year(publication_date),
         month_publication = as.factor(month(publication_date)),
         year_publication = as.factor(year(publication_date)),
         language_code = factor(
           ifelse(language_code %in% c("enm",
                                       "eng",
                                       "en-US",
                                       "en-GB",
                                       "en-CA"),
                  "English","Other")
           )
         ) %>% 
  select(-authors, -publication_date, -publisher) %>% 
  na.omit()

summary(livros)

# 50% das observações estão entre [0,3] e o 1º Q também é iqual a 3, verificando o histograma dessa variável temos:

livros %>% 
  ggplot(aes(x=average_rating, after_stat(scaled)))+
  geom_histogram(aes(y=..density..),
                 bins = 15)+
  geom_density()+
  geom_vline(xintercept = c(3.5,4), color = "green", lty=2)

# e conferindo a quantidade de observações menores de 3 temos: 

livros %>% 
  filter(average_rating<3.5) %>% 
  count()

summary(livros$average_rating)

quantile(livros$average_rating,.67)

livros %>% 
  filter(average_rating>4) %>% 
  count()

# dessa forma, trabalharemos apenas com duas categorias, sendo elas regular no intervalo de [0,3] 
# e bom no intervalo de (3,5], uma vez que pelo histograma é percebido ter poucas notas acima de 4 também. 

# Dessa forma, nosso conjunto de dados final é composto por duas categorias: regular e bom. Aplicando no conjunto de dados:

livros <- livros %>% 
  mutate(
    book_rating =
      case_when(average_rating<3.5 ~ "Ruim",
                average_rating<=4 ~ "Bom",
                TRUE ~ "Ótimo")
  ) %>% 
  select(-average_rating)

livros %>% 
  group_by(book_rating) %>% 
  count()

#####Salvando os dados atuais#####

write.csv(livros, 
          "./Conjunto de Dados/books_t.csv",
          fileEncoding = "UTF-8",
          row.names = FALSE)
