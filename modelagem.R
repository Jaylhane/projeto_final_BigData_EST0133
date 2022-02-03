# Modelagem

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")

livros_split <- livros %>%
  initial_split(strata = book_rating)

livros_train <- training(livros_split)
livros_test <- testing(livros_split)
livros_metrics <- metric_set(accuracy, roc_auc, mn_log_loss)

set.seed(234, kind = "Mersenne-Twister", normal.kind = "Inversion")

livros_folds <- vfold_cv(livros_train, strata = book_rating)
livros_folds

livros_rec <- recipe(book_rating ~ .,
                      data = livros_train) 

## we can `prep()` just to check that it works
prep(livros_rec)


stopping_spec <-
  boost_tree(
    trees = (range(500,1000)),
    mtry = tune(),
    learn_rate = tune(),
    stop_iter = tune()
  ) %>%
  set_engine("xgboost", validation = 0.2) %>%
  set_mode("classification")

stopping_grid <-
  grid_latin_hypercube(
    mtry(range = c(1, 8)), 
    learn_rate(range = c(-5, -1)), 
    stop_iter(range = c(10, 50)), 
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
