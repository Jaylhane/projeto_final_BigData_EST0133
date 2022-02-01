# Análise Exploratória
# Data 01/02/2022

library(tidyverse)
library(tidymodels)

theme_set(theme_light())

#####Carregando os Dados#####

livros <- read.csv("./Conjunto de Dados/books_t.csv",
                   encoding = "UTF-8") %>% 
  mutate(language_code=factor(language_code),
         month_publication=factor(month_publication),
         year_publication=factor(year_publication),
         book_rating=factor(book_rating,
                            levels = c("Bom","Regular")))

#####Separando em Treino e Teste#####

set.seed(1904, kind = "Mersenne-Twister", normal.kind = "Inversion")

livros_split <- initial_split(livros, prop = .75, strata = book_rating)

livros_treino <- training(livros_split)

livros_teste <- testing(livros_split)

#####Verificando a correlação das variáveis quantitativas#####

library(GGally)

livros_treino %>% 
  select(where(is.numeric)) %>% 
  ggpairs(upper = list(continuous = wrap("cor", method = "spearman")))

# há uma correlação bem forte entre a variável `text_reviews_count` e `ratings_count`
# o que era esperado já que quem deixa uma nota de avaliação (`rating_count`) 
# não necessariamente deixa uma resenha (`text_reviews_count`)
# mas se a pessoa deixou uma resenha ela também deixou uma nota de avaliação. 
# Há indicios de multicolineariedade entre essas duas variáveis. 
# Nesse caso remove? 

livros_treino %>% 
  select(where(is.numeric),book_rating) %>% 
  pivot_longer(-book_rating) %>% 
  ggplot(.,aes(fill = book_rating)) +
  geom_boxplot(aes(y=value)) +
  facet_wrap(~ name, scales = "free") +
  labs(x="",
       y="Valor",
       fill = "Classificação\ndo Livro",
       title = "Boxplot das variáveis por classificação do livro")+
  scale_fill_viridis_d()

# As variáveis não se diferenciam quanto suas médias entre as classificações do livro

grafico_bons_mes_ano <- livros_treino %>% 
  mutate(book_rating = book_rating == "Bom") %>% 
  group_by(
    mes = month_publication,
    ano = year_publication
  ) %>% 
  summarise(book_rating = mean(book_rating)) %>% 
  ggplot(aes(mes,ano, fill = book_rating)) + 
  geom_tile(alpha = .75) + 
  scale_fill_viridis_c(labels = scales::percent) + 
  labs(fill = "% livros bons" , x="Mês", y="Ano",
       title = "Composição dos livros avaliados como: BOM")+
  theme(legend.position = "right");grafico_bons_mes_ano
  
#grafico_regulares_mes_ano <- livros_treino %>% 
#  mutate(book_rating = book_rating == "Regular") %>% 
#  group_by(
#    mes = month_publication,
#    ano = year_publication
#  ) %>% 
#  summarise(book_rating = mean(book_rating)) %>% 
#  ggplot(aes(mes,ano, fill = book_rating)) + 
#  geom_tile(alpha = .75) + 
#  scale_fill_viridis_c(labels = scales::percent) + 
#  labs(fill = "% livros regulares", x="Mês", y="",
#       title = "Composição dos livros avaliados como: REGULAR")
#
#gridExtra::grid.arrange(grafico_bons_mes_ano,grafico_regulares_mes_ano,
#                        ncol = 2)

# Há uma distinção entre a avaliação do livro para publicações em anos mais antigos ou mais recentes,
# enquanto que no intervalo de 1986 a 2008 é bem misturado. 

# Verificando a distribuição de livros publicados ao longo dos anos temos:

livros_treino %>%  
  group_by(
    mes = month_publication,
    ano = year_publication
  ) %>% 
  count() %>% 
  ggplot(aes(n,ano, fill=mes))+
  geom_col()+
  geom_hline(yintercept = "1986", color = "blue", lty=2)+
  geom_hline(yintercept = "2008", color = "blue", lty=2)+
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Quantidade Publicações",
       y = "Ano",
       fill = "Mês",
       title = "Quantidade de Publicações por Ano")+
  scale_fill_viridis_d()
  
# a partir de 1986 há um crescimento exponencial de publições por ano nos dados, 
# de forma que é compreensível que para os intervalos de anos de 1986 a 2008 
# está bem mesclado a % de livros bons publicados

