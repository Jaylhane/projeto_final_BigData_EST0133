## Modelagem
## Data: 05/02/2022

library(tidyverse)
library(tidymodels)

theme_set(theme_light(base_family = "IBMPlexSans"))

livros <- read.csv("./Conjunto de Dados/books_t.csv",
                   encoding = "UTF-8") %>% 
  mutate(publication_date=as.Date(publication_date),  
         prop_text_reviews = text_reviews_count / ratings_count,
         prop_text_reviews = ifelse(prop_text_reviews %in% c(NaN,Inf),
                                    0, prop_text_reviews),
         book_rating=factor(book_rating,
                            levels = c("Ótimo","Bom","Ruim"))) %>% 
  select(-month_publication, -year_publication, -text_reviews_count) %>% 
  filter(book_age<40) %>% 
  filter(num_pages<1000) %>% 
  filter(ratings_count<1000)

#####Separando Treino e Teste#####

set.seed(1904, kind = "Mersenne-Twister", normal.kind = "Inversion")

livros_split <- initial_split(livros, prop = .75, strata = book_rating)

livros_treino <- training(livros_split)

livros_teste <- testing(livros_split)

#####Criando Métricas#####

livros_metricas <- metric_set(accuracy, roc_auc, mn_log_loss)

#####Criando Folds#####

set.seed(1989)
livros_folds <- vfold_cv(livros_treino, strata = book_rating, v=10)
livros_folds

#####Criando a receita - Pre-processamento de Dados#####

livros_rec <- recipe(book_rating ~ ., data = livros_treino) %>%
  themis::step_downsample(book_rating) %>% 
  step_date(publication_date, features = c("month"), 
            keep_original_cols = FALSE) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>% 
  #step_pca(all_predictors(),threshold = .80) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  prep()

prep(livros_rec) %>% 
  bake(new_data =NULL)
# 19 colunas
# 2 colunas com PCA

#####Aplicando a receita nos conj de treino e teste#####

# livros_treino_t <- juice(livros_rec)
# 
# livros_teste_t <- bake(livros_rec,
#                        new_data = livros_teste)

 #####Grid de Procura e Parada antecipada#####

stopping_spec <-
  boost_tree(
    trees = 500,
    mtry = tune(),
    learn_rate = tune(),
    stop_iter = tune()
  ) %>%
  set_engine("xgboost", validation = 0.2) %>%
  set_mode("classification")

stopping_grid <-
  grid_latin_hypercube(
    mtry(range = c(5L, 18L)),
    learn_rate(range = c(-5, -1)), 
    stop_iter(range = c(10L, 50L)), 
    size = 10
  )

early_stop_wf <- workflow(livros_rec, stopping_spec)

doParallel::registerDoParallel()
set.seed(2022)
stopping_rs <- tune_grid(
  early_stop_wf,
  livros_folds,
  grid = stopping_grid,
  metrics = livros_metricas
)

#####Avaliação do Modelo#####

autoplot(stopping_rs)

show_best(stopping_rs, metric = "mn_log_loss")

stopping_fit <- early_stop_wf %>%
  finalize_workflow(select_best(stopping_rs, "mn_log_loss")) %>%
  last_fit(livros_split)

stopping_fit

collect_metrics(stopping_fit)

library(vip)

extract_workflow(stopping_fit) %>%
  extract_fit_parsnip() %>%
  vip(num_features = 15, geom = "point")+
  ggtitle("Variáveis mais importantes no modelo")

collect_predictions(stopping_fit) %>%
  conf_mat(book_rating, .pred_class) %>%
  autoplot(type = "heatmap")+
  ggtitle("Mapa de Calor das Predições")

collect_predictions(stopping_fit, summarize = FALSE) %>%
  roc_curve(book_rating, .pred_class:.pred_Ruim) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(alpha = 0.8, size = 1) +
  coord_equal() +
  labs(color = NULL,
       title =  "Curva ROC Modelo Final",
       x="1 - especificidade",
       y="Sensibilidade")

## Verdadeiro positivo
collect_predictions(stopping_fit) %>% 
  sens(book_rating, .pred_class)

## Verdadeiro negativo
collect_predictions(stopping_fit) %>% 
  spec(book_rating, .pred_class)

collect_predictions(stopping_fit, summarize = FALSE) %>%
  roc_curve(book_rating, .pred_class:.pred_Ruim) %>% 
  group_by(.level) %>% 
  summarise(mean_sens=mean(sensitivity),
            mean_spec=mean(specificity))
