# Modelagem



livros <- read.csv("./Conjunto de Dados/books_t.csv",
                   encoding = "UTF-8") %>% 
  mutate(publication_date=as.Date(publication_date),  
         prop_text_reviews = text_reviews_count / ratings_count,
         book_rating=factor(book_rating,
                            levels = c("Ótimo","Bom","Ruim"))) %>% 
  select(-month_publication, -year_publication, -text_reviews_count)


#####Separando em Treino e Teste#####

set.seed(1904, kind = "Mersenne-Twister", normal.kind = "Inversion")

livros_split <- initial_split(livros, prop = .75, strata = book_rating)

livros_treino <- training(livros_split)

livros_teste <- testing(livros_split)
livros_metrics <- metric_set(accuracy, roc_auc, mn_log_loss)

set.seed(234, kind = "Mersenne-Twister", normal.kind = "Inversion")

livros_folds <- vfold_cv(livros_treino, strata = book_rating, v = 5)
livros_folds

livros_rec <- recipe(book_rating ~ ., data = livros_treino) %>%
  themis::step_downsample(book_rating) %>% 
  step_date(publication_date, features = c("year", "month"), 
            keep_original_cols = FALSE) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>% 
  prep()


livros_treino_t <- juice(livros_rec)
# preparar o conjunto de teste
livros_teste_t <- bake(livros_rec,
                        new_data = livros_teste)
## we can `prep()` just to check that it works
prep(livros_rec)

livros_treino_cv <- vfold_cv(livros_treino_t, v = 6)

livros_xgboost_tune <-
  boost_tree(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("xgboost", validation = 0.2)

# grid de procura
livros_xgboost_grid <- grid_regular(mtry(range(1, 19)),
                                trees(range(500, 1000)),
                                min_n(range(10, 100)),
                                levels = c(10, 2, 10))

livros_xgboost_tune_wflow <-
  workflow() %>%
  add_model(livros_xgboost_tune) %>%
  add_formula(book_rating ~ .)

doParallel::registerDoParallel()

# avaliacao do modelo
livros_xgboost_fit_tune <-
  livros_xgboost_tune_wflow %>%
  tune_grid(
    resamples = livros_treino_cv,
    grid = livros_xgboost_grid
  )


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
    mtry(range = c(1L, 7L)), 
    learn_rate(range = c(-5, -1)), 
    stop_iter(range = c(10L, 50L)), 
    size = 10
  )

early_stop_wf <- workflow(livros_rec, stopping_spec)

doParallel::registerDoParallel()

set.seed(345, kind = "Mersenne-Twister", normal.kind = "Inversion")

stopping_rs <- tune_grid(
  early_stop_wf,
  livros_folds,
  grid = stopping_grid,
  metrics = livros_metrics
)

show_best(stopping_rs, metric = "mn_log_loss")
