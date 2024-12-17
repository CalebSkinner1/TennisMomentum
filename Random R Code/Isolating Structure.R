# Isolating Structure

library("arrow")
library("here")
library("scales")
library("flextable")
library("tidyverse"); theme_set(theme_minimal())
library("tidymodels")
library("discrim")
library("fpp3")
library("recipes")
library("patchwork")
library("fable")

# data
gs <- read_parquet(here("grandslams2.parquet"))
cov <- read_parquet(here("covariates.parquet"))

structure_analysis <- gs %>%
  group_by(match_id) %>%
  mutate(
    serve_impact = sum(point_victor == server)/n() - sum(point_victor != server)/n()) %>%
  ungroup() %>%
  mutate(
    ranking1 = replace_na(ranking1, 1000),
    ranking2 = replace_na(ranking2, 1000),
    match_id = factor(match_id),
    point_victor = factor(if_else(point_victor == 2, 0, point_victor)),
    rank_diff = ranking1 - ranking2,
    ps_1_prob = (1/ps_1)*100,
    ps_2_prob = (1/ps_2)*100,
    b365_1_prob = (1/b365_1)*100,
    b365_2_prob = (1/b365_2)*100,
    ps_sum = ps_1_prob + ps_2_prob,
    b365_sum = b365_1_prob + b365_2_prob,
    ps_1_prob = (ps_1_prob * 100)/ps_sum,
    ps_2_prob = (ps_2_prob * 100)/ps_sum,
    b365_1_prob = (b365_1_prob * 100)/b365_sum,
    b365_2_prob = (b365_2_prob * 100)/b365_sum) %>%
  filter(!is.na(ps_1_prob), !is.na(b365_1_prob), !is.na(rank_diff)) %>%
  select(match_id, point_no, point_victor, ranking1, ranking2, ps_1_prob, ps_2_prob,
         b365_1_prob, b365_2_prob, serve_impact, server)

structure_analysis %>% glimpse()
# use transformed probability?

set.seed(1128)
structure_split <- structure_analysis %>% initial_split(prop = .75, strata = point_victor)
structure_train <- training(structure_split)
structure_test <- testing(structure_split)
structure_folds <- vfold_cv(structure_train, v = 5, strata = point_victor)

structure_rec <- recipe(point_victor ~ ranking1 + ranking2 + ps_1_prob +
                          b365_1_prob + serve_impact + server, data = structure_train) %>%
  step_interact(terms = ~server:serve_impact) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Logistic Regression -----------------------------------------------------

lr_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

lr_wf <- workflow() %>%
  add_recipe(structure_rec) %>%
  add_model(lr_spec)

lr_fit <- lr_wf %>%
  fit(data = structure_train)

lr_train_results <- augment(lr_fit, new_data = structure_train)

lr_train_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

lr_test_results <- augment(lr_fit, new_data = structure_test)

lr_test_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

# gini index
lr_test_results %>% gain_capture(point_victor, .pred_0)

lr_test_results %>%
  filter(match_id == "2023-wimbledon-1701")

tidy(lr_fit)

# LDA ---------------------------------------------------------------------

lda_spec <- discrim_linear() %>%
  set_mode("classification") %>%
  set_engine("MASS")

lda_wf <- workflow() %>%
  add_recipe(structure_rec) %>%
  add_model(lda_spec)

lda_fit <- lda_wf %>%
  fit(data = structure_train)

lda_train_results <- augment(lda_fit, new_data = structure_train)

lda_train_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

lda_test_results <- augment(lda_fit, new_data = structure_test)

lda_test_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

lda_test_results %>% gain_capture(point_victor, .pred_0)


# Random Forest -----------------------------------------------------------

rf_spec <- rand_forest(trees = 1,
                       mtry = tune(),
                       min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("classification")

rf_rec <- recipe(point_victor ~ ranking1 + ranking2 + ps_1_prob +
                   b365_1_prob + serve_impact + server, data = structure_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

rf_tuning_grid <- grid_regular(
  mtry(range = c(3, 6)),
  min_n(range = c(40, 50)),
  levels = 10)

rf_tune_results <- tune_grid(
  object = workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec),
  resamples = structure_folds,
  grid = rf_tuning_grid,
  metrics = metric_set(accuracy))

rf_best_params <- select_best(rf_tune_results, "accuracy")
rf_best_params

rf_fit <- finalize_workflow(
  workflow() %>%
    add_recipe(structure_rec) %>%
    add_model(rf_spec),
  rf_best_params) %>%
  fit(data = structure_train)

rf_train_results <- augment(rf_fit, new_data = structure_train)

rf_train_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

rf_test_results <- augment(rf_fit, new_data = structure_test)

rf_test_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

rf_test_results %>% gain_capture(point_victor, .pred_0)

# Support Vector Machine -------------------------------------------------------------------------

svm_linear_spec <- svm_poly(degree = 1) %>%
  set_mode("classification") %>%
  set_engine("kernlab", scaled = FALSE)

# no cross val
svm_linear_fit <- workflow() %>%
  add_recipe(structure_rec) %>%
  add_model(svm_linear_spec %>% set_args(cost = 1)) %>%
  fit(data = structure_train)

svm_linear_fit

svm_linear_wf <- workflow() %>%
  add_recipe(structure_rec) %>%
  add_model(svm_linear_spec %>% set_args(cost = tune()))

svm_linear_tuning_grid <- grid_regular(cost(), levels = 10)

svm_linear_tune_res <- tune_grid(
  svm_linear_wf, 
  resamples = structure_folds, 
  grid = svm_linear_tuning_grid)

best_cost <- select_best(svm_linear_tune_res, metric = "accuracy")

svm_linear_fit <- finalize_workflow(
  workflow() %>%
    add_recipe(structure_rec) %>%
    add_model(svm_linear_spec),
  best_cost) %>%
  fit(data = structure_train)

svm_linear_train_results <- augment(svm_linear_fit, new_data = structure_train)

svm_linear_train_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

svm_linear_test_results <- augment(svm_linear_fit, new_data = structure_test)

svm_linear_test_results %>%
  metrics(truth = point_victor, estimate = .pred_class)

svm_linear_test_results %>% gain_capture(point_victor, .pred_0)

# Plot --------------------------------------------------------------------

augment(lr_fit, new_data = structure_analysis) %>%
  select(match_id, point_no, .pred_1) %>%
  right_join(gs, by = join_by(match_id, point_no)) %>%
  rename("pred_point_victor" = ".pred_1") %>%
  relocate(pred_point_victor, .before = point_victor) %>%
  filter(match_id == "2023-wimbledon-1701") %>%
  mutate(
    point_victor = as.numeric(if_else(point_victor == "1", 1, 0)),
    adj_point_victor = (point_victor - pred_point_victor)*2,
    adj_point_margin = cumsum(adj_point_victor)) %>%
  ggplot(aes(x = point_no, y = adj_point_margin)) +
  geom_line()

# Function ----------------------------------------------------------------

grad_update <- function(F, order){
  n <- length(F)
  x <- seq(1, n, length.out = n)
  g <- numeric(n)
  if(order == 1){
    g[1] <- NA
    if (n > 2)
      g[2:n] <- (F[2:n] - F[1:(n - 1)])/(x[2:n] - x[1:(n - 1)])
  }
  if(order == 2){
    g[1] <- NA
    g[2] <- (F[2] - F[1])/(x[2] - x[1])
    # g[n] <- (3*F[n] - 4*F[n - 1] + F[n-2])/(2*(x[n] - x[n - 1]))
    if (n > 3)
      g[3:n] <- (3*F[3:n] - 4*F[2:(n-1)] + F[1:(n-2)])/(2*(x[3:n] - x[2:(n - 1)]))
  }
  if(order == 3){
    g[1] <- NA
    g[2] <- g[2] <- (F[2] - F[1])/(x[2] - x[1])
    g[3] <- (3*F[3] - 4*F[2] + F[1])/(2*(x[3] - x[2]))
    if (n > 3)
      g[4:n] <- (F[4:n] - 3*F[3:(n - 1)] + 3*F[2:(n - 2)] - F[1:(n-3)])/((x[4:n] - x[3:(n - 1)]))
  }
  return(g)
}

cum_points <- function(df, match_code, type) {
  df <- df %>% filter(match_id == match_code) %>%
    mutate(
      diff = p1_points_won - p2_points_won)
  vlines <- df %>% filter(set_victor != 0) %>% dplyr::select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  service <- df %>%
    summarise(
      exp_server = sum(point_victor == server)/n() - sum(point_victor != server)/n()) %>% pull()
  
  p1 <- df$player1[1]
  p2 <- df$player2[1]
  tourney <- df$tournament[1]
  gender <- df$sex[1]
  
  match <- df %>%
    mutate(
      point_margin = diff,
      serve_adj = if_else(point_victor == 1, 1, -1) + if_else(server == 1, -service, service),
      adj_point_margin = cumsum(serve_adj)) %>%
    as_tsibble(index = point_no)
  
  ses <- match %>%
    model(
      model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = .1) + season("N")))
  
  match_flip <- df %>%
    mutate(
      point_margin = diff,
      serve_adj = if_else(point_victor == 1, 1, -1) + if_else(server == 1, -service, service),
      adj_point_margin = cumsum(serve_adj)) %>%
    as_tsibble(index = point_no) %>%
    mutate(flip = n() - point_no + 1) %>%
    arrange(flip) %>%
    as_tsibble(index = flip)
  
  ses_flip <- match_flip %>%
    model(
      model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = .1) + season("N")))
  
  if(type == "both"){
    p <- df %>% ggplot() +
      geom_line(aes(point_no, p1_points_won, color = 'p1')) +
      geom_line(aes(point_no, p2_points_won, color = 'p2')) +
      scale_color_manual(name = 'Player',
                         breaks = c('p1', 'p2'),
                         labels = c(p1, p2),
                         values = c('p1' = 'cadetblue4', 'p2' = 'indianred3')) +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      labs(y = "Points Won", x = "Point Number", title = str_c(p1, " vs. ", p2)) +
      scale_color_discrete(name = "Player", labels = c(str_extract_part(p1, " ", before = FALSE),
                                                       str_extract_part(p2, " ", before = FALSE))) +
      theme(legend.position = c(.9, .2))
    print(p)
  }
  if(type == "difference"){
    p <- df %>% ggplot(aes(point_no, diff, color = "indianred3")) +
      geom_line() +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      labs(y = "Point Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
      theme(legend.position = "none")
    print(p)
  }
  if(type == "color serve"){
    p <- df %>% mutate(
      server = factor(if_else(server == 1, p1, p2))) %>%
      ggplot(aes(point_no, diff)) +
      geom_point(aes(color = server), size = .4) +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      labs(y = "Point Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
      scale_color_discrete(name = "Server",
                           labels = c(str_extract_part(p2, " ", before = FALSE),
                                      str_extract_part(p1, " ", before = FALSE))) +
      theme(legend.position = c(.28, .2))
    print(p)
  }
  if(type == "adjusted"){
    p <- match %>%
      ggplot(aes(point_no, adj_point_margin, color = "indianred3")) +
      geom_line() +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      labs(y = "Adjusted Point Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
      theme(legend.position = "none")
    print(p)
  }
  if(type == "structural adjusted"){
    p <- df %>%
      filter(match_id == match_code) %>%
      arrange(point_no) %>%
      mutate(
        point_victor = as.numeric(if_else(point_victor == "1", 1, 0)),
        adj_point_victor = (point_victor - pred_point_victor)*2,
        adj_point_margin = cumsum(adj_point_victor)) %>%
      ggplot(aes(x = point_no, y = adj_point_margin, color = "indianred3")) +
      geom_line() +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      labs(y = "Structurally Adjusted Point Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
      theme(legend.position = "none")
    print(p)
  }
  
  if(type == "smoothed"){
    p_back <- components(ses) %>%
      left_join(fitted(ses), by = c(".model", "point_no")) %>%
      mutate(adj_point_margin = replace_na(adj_point_margin, 0)) %>%
      slice(-1) %>%
      ggplot() +
      geom_line(aes(point_no, adj_point_margin), color = "cadetblue4") +
      geom_line(aes(point_no, level), color = "indianred3") +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      labs(x = "", y = "Point Margin", title = "Backward Smoothing")
    
    p_forward <- components(ses_flip) %>%
      left_join(fitted(ses_flip), by = c(".model", "flip")) %>%
      mutate(
        adj_point_margin = replace_na(adj_point_margin, 0),
        point_no = n() - flip) %>%
      slice(-1) %>%
      ggplot() +
      geom_line(aes(point_no, adj_point_margin), color = "cadetblue4") +
      geom_line(aes(point_no, level), color = "indianred3") +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      labs(x = "Point Number", y = "Point Margin", title = "Forward Smoothing")
    
    print(p_back/p_forward)
  }
  if(type == "compare orders"){
    smoothed <- components(ses) %>% slice(-1) %>%
      dplyr::select(point_no, adj_point_margin, level) %>%
      # add_row(point_no = 0, point_margin = 0, level = 0) %>%
      arrange(point_no) %>%
      mutate(
        gradient_o1 = grad_update(level, order = 1),
        gradient_o2 = grad_update(level, order = 2),
        point_victor = match$point_victor) %>%
      relocate(point_victor, .before = adj_point_margin)
    
    p1 <- ggplot(smoothed) +
      geom_line(aes(point_no, gradient_o1), color = "forestgreen") +
      labs(x = "", y = "Backward Momentum", title = "1st Order Approximation") +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(breaks = c(-.6, -.3, 0, .3, .6), limits = c(-.65, .65))
    # scale_color_manual(
    #   values = c(gradient_o1 = "forestgreen", gradient_o2 = "limegreen"),
    #   breaks = c("First Order", "Second Order"),
    #   labels = c(gradient_o1 = "First Order", gradient_o2 = "Second Order"),
    #   name = "")
    
    p2 <- ggplot(smoothed) +
      geom_line(aes(point_no, gradient_o2), color = "limegreen") +
      labs(x = "Point Number", y = "Backward Momentum", title = "2nd Order Approximation") +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(breaks = c(-.6, -.3, 0, .3, .6), limits = c(-.65, .65))
    # scale_color_manual(
    #   values = c(gradient_o1 = "forestgreen", gradient_o2 = "limegreen"),
    #   breaks = c("First Order", "Second Order"),
    #   labels = c(gradient_o1 = "First Order", gradient_o2 = "Second Order"),
    #   name = "")
    
    print(p1/p2)
  }
  if(type == "full viz"){
    smoothed <- components(ses) %>% slice(-1) %>%
      dplyr::select(point_no, adj_point_margin, level) %>%
      # add_row(point_no = 0, point_margin = 0, level = 0) %>%
      arrange(point_no) %>%
      mutate(
        gradient_b1 = grad_update(level, order = 1),
        point_victor = match$point_victor) %>%
      relocate(point_victor, .before = adj_point_margin)
    
    smoothed_flip <- components(ses_flip) %>%
      slice(-1) %>%
      select(flip, adj_point_margin, level) %>%
      # add_row(point_no = 0, adj_point_margin = 0, level = 0) %>%
      arrange(flip) %>%
      mutate(
        gradient_f1 = -grad_update(level, order = 1)) %>%
      as_tibble() %>%
      mutate(
        point_no = n() - flip + 1) %>%
      arrange(point_no) %>%
      select(-flip, -adj_point_margin)
    
    total <- smoothed %>% left_join(smoothed_flip, join_by(point_no))
    
    p_back <- ggplot(total) +
      geom_line(aes(point_no, gradient_b1), color = "forestgreen") +
      labs(x = "", y = "", title = "Backward Momentum") +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(breaks = c(-.6, -.3, 0, .3, .6))
    
    p_future <- ggplot(total) +
      geom_line(aes(point_no, gradient_f1), color = "limegreen") +
      labs(x = "", y = "", title = "Future Momentum") +
      geom_vline(xintercept = vlines, color = "darkgrey") +
      geom_hline(yintercept = 0) +
      scale_y_continuous(breaks = c(-.6, -.3, 0, .3, .6))
    
    p_adj_margin <- ggplot(total) +
      geom_line(aes(point_no, adj_point_margin), color = "indianred3") +
      labs(x = "", y = "", title = "Adjusted Point Margin") +
      geom_vline(xintercept = vlines, color = "darkgrey")
    
    print(p_back/p_adj_margin/p_future)
  }
}

# structurally adjusted
augment(lr_fit, new_data = structure_analysis) %>%
  select(match_id, point_no, .pred_1) %>%
  right_join(gs, by = join_by(match_id, point_no)) %>%
  rename("pred_point_victor" = ".pred_1") %>%
  relocate(pred_point_victor, .before = point_victor) %>%
  cum_points("2023-wimbledon-1701", "structural adjusted")

# serve adjusted only
augment(lr_fit, new_data = structure_analysis) %>%
  select(match_id, point_no, .pred_1) %>%
  right_join(gs, by = join_by(match_id, point_no)) %>%
  rename("pred_point_victor" = ".pred_1") %>%
  relocate(pred_point_victor, .before = point_victor) %>%
  cum_points("2023-wimbledon-1701", "adjusted")

# Edit Data Set -----------------------------------------------------------

gs_new <- augment(lr_fit, new_data = structure_analysis) %>%
  select(match_id, point_no, .pred_1) %>%
  right_join(gs, by = join_by(match_id, point_no)) %>%
  rename("pred_point_victor" = ".pred_1") %>%
  relocate(pred_point_victor, .before = point_victor) %>%
  group_by(match_id) %>%
  mutate(
    point_victor = as.numeric(if_else(point_victor == 1, 1, 0)),
    adj_point_victor = (point_victor - pred_point_victor)*2,
    adj_point_margin = cumsum(adj_point_victor),
    temp = if_else(point_victor == 1, 1, -1),
    point_margin = cumsum(temp)) %>%
  ungroup() %>%
  mutate(
    point_victor = as.factor(point_victor),
    game_victor = as.factor(if_else(game_victor == 1, 1, 0)),
    set_victor = as.factor(if_else(set_victor == 1, 1, 0)),
    match_victor = as.factor(if_else(match_victor == 1, 1, 0)),
    across(p1_ace:p2_break_pt_missed, ~factor(.x)),
    tiebreak = as.factor(tiebreak),
    across(first_pt_set:match_pt, ~factor(.x)),
    across(sex:change_ends, ~factor(.x)),
    ps_1_prob = (1/ps_1)*100,
    ps_2_prob = (1/ps_2)*100,
    b365_1_prob = (1/b365_1)*100,
    b365_2_prob = (1/b365_2)*100,
    ps_sum = ps_1_prob + ps_2_prob,
    b365_sum = b365_1_prob + b365_2_prob,
    ps_1_prob = ps_1_prob/ps_sum,
    b365_1_prob = b365_1_prob/b365_sum,
    total_distance_run = p1_distance_run + p2_distance_run,
    long_point = factor(case_when(
      total_distance_run > 60 ~ 1,
      rally_count > 6 ~ 1,
      .default = 0))) %>%
  rename(p1_break_saved = p2_break_pt_missed,
         p2_break_saved = p1_break_pt_missed,
         p1_break_converted = p1_break_pt_won,
         p2_break_converted = p2_break_pt_won) %>%
  select(-temp, -level_backward, -level_forward, -gradient_backward, -spline_backward,
         -gradient_forward, -spline_forward, -winner_shot_type, -serve_width, -serve_depth, -return_depth,
         -ht1, -hand1, -age1, -ht2, -hand2, -age2, -retired, -delay, -run, -ps_1, -ps_2, -b365_1, -b365_2,
         -ps_sum, -b365_sum, -ps_2_prob, -b365_2_prob, -status, -speed_mph, -rally_count, -total_distance_run,
         -p1_distance_run, -p2_distance_run, -contains("break_pt"), -contains("net_pt")) %>%
  relocate(point_margin, .before = adj_point_margin) %>%
  relocate(ps_1_prob, .before = tiebreak) %>%
  relocate(b365_1_prob, .before = ps_1_prob) %>%
  relocate(adj_point_victor, .after = point_victor) %>%
  relocate(tiebreak_victory, .after = tiebreak) %>%
  relocate(sex, .after = match) %>%
  relocate(interruption, .after = match_victor) %>%
  relocate(change_ends, .after = interruption) %>%
  relocate(long_point, .after = p2_break_saved) %>%
  relocate(p1_break_saved, .after = p2_break_saved) %>%
  filter(!is.na(adj_point_margin)) # removes values without betting odds or rank

# check for NAs
gs_new %>% #filter(year == 2023, tournament == "wimbledon") %>%
  summarise(
    across(match_id:match_pt, ~sum(is.na(.x)))
  ) %>% glimpse()

# Computing Derivatives on Residuals --------------------------------------

# gradient function rework
grad_update <- function(F, order){
  n <- length(F)
  x <- seq(1, n, length.out = n)
  g <- numeric(n)
  if(order == 1){
    g[1] <- NA
    if (n > 2)
      g[2:n] <- (F[2:n] - F[1:(n - 1)])/(x[2:n] - x[1:(n - 1)])
  }
  if(order == 2){
    g[1] <- NA
    g[2] <- (F[2] - F[1])/(x[2] - x[1])
    # g[n] <- (3*F[n] - 4*F[n - 1] + F[n-2])/(2*(x[n] - x[n - 1]))
    if (n > 3)
      g[3:n] <- (3*F[3:n] - 4*F[2:(n-1)] + F[1:(n-2)])/(2*(x[3:n] - x[2:(n - 1)]))
  }
  if(order == 3){
    g[1] <- NA
    g[2] <- g[2] <- (F[2] - F[1])/(x[2] - x[1])
    g[3] <- (3*F[3] - 4*F[2] + F[1])/(2*(x[3] - x[2]))
    if (n > 3)
      g[4:n] <- (F[4:n] - 3*F[3:(n - 1)] + 3*F[2:(n - 2)] - F[1:(n-3)])/((x[4:n] - x[3:(n - 1)]))
  }
  return(g)
}

# adding derivatives!
derivative_tibble <- function(df, alpha, order){
  
  tp <- df %>%
    select(match_id, point_no, adj_point_margin)
  
  matches <- tp %>% select(match_id) %>% distinct() %>% as_vector()
  
  mast_deriv <- tibble()
  
  for(i in 1:length(matches)){
    # filtering for each match
    this_match <- tp %>% filter(match_id == matches[i])
    
    # smoothing from backward
    ses <- this_match %>%
      as_tsibble(index = point_no) %>%
      model(
        model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = alpha) + season("N")))
    
    # backward gradient
    deriv_backward <- components(ses) %>%
      slice(-1) %>%
      select(point_no, adj_point_margin, level) %>%
      # add_row(point_no = 0, adj_point_margin = 0, level = 0) %>%
      arrange(point_no) %>%
      mutate(
        gradient_backward = grad_update(level, order = order)) %>%
      rename(level_backward = "level") %>%
      as_tibble() %>%
      mutate(match_id = matches[i])
    
    # smoothing from forward
    ses_flip <- this_match %>%
      mutate(
        flip = n() - point_no + 1) %>%
      arrange(flip) %>%
      as_tsibble(index = flip) %>%
      model(
        model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = alpha) + season("N")))
    
    # forward gradient
    deriv_forward <- components(ses_flip) %>%
      slice(-1) %>%
      select(flip, adj_point_margin, level) %>%
      # add_row(point_no = 0, adj_point_margin = 0, level = 0) %>%
      arrange(flip) %>%
      mutate(
        gradient_forward = -grad_update(level, order = order)) %>%
      rename(level_forward = "level") %>%
      as_tibble() %>%
      mutate(
        match_id = matches[i],
        point_no = n() - flip + 1) %>%
      arrange(point_no) %>%
      select(-flip, -adj_point_margin)
    
    new_deriv <- left_join(deriv_backward, deriv_forward, by = join_by("match_id", "point_no")) %>%
      relocate(match_id, .before = point_no)
    
    mast_deriv <- bind_rows(mast_deriv, new_deriv)
  }
  mast_deriv
}

wimbledon_derivatives <- gs_new %>% filter(year == 2023, tournament == "wimbledon") %>%
  derivative_tibble(alpha = .10, order = 1)

derivatives_alpha10 <- gs_new %>%
  derivative_tibble(alpha = .10, order = 1) %>%
  select(-contains("level")) %>%
  rename(gradient_backward_a10 = gradient_backward,
         gradient_forward_a10 = gradient_forward)

derivatives_alpha15 <- gs_new %>%
  derivative_tibble(alpha = .15, order = 1) %>%
  select(-contains("level")) %>%
  rename(gradient_backward_a15 = gradient_backward,
         gradient_forward_a15 = gradient_forward)

derivatives_alpha20 <- gs_new %>%
  derivative_tibble(alpha = .2, order = 1) %>%
  select(-contains("level")) %>%
  rename(gradient_backward_a20 = gradient_backward,
         gradient_forward_a20 = gradient_forward)

together <- gs_new %>%
  full_join(derivatives_alpha10, by = join_by(match_id, point_no, adj_point_margin)) %>%
  relocate(gradient_backward_a10, .after = adj_point_margin) %>%
  relocate(gradient_forward_a10, .after = gradient_backward_a10) %>%
  full_join(derivatives_alpha15, by = join_by(match_id, point_no, adj_point_margin)) %>%
  relocate(gradient_backward_a15, .after = gradient_forward_a10) %>%
  relocate(gradient_forward_a15, .after = gradient_backward_a15) %>%
  full_join(derivatives_alpha20, by = join_by(match_id, point_no, adj_point_margin)) %>%
  relocate(gradient_backward_a20, .after = gradient_forward_a15) %>%
  relocate(gradient_forward_a20, .after = gradient_backward_a20)

together %>% filter(match_id == "2023-wimbledon-1701") %>%
  ggplot() +
  geom_line(aes(x = point_no, y = gradient_backward_a20), color = "forestgreen") +
  geom_line(aes(x = point_no, y = gradient_backward_a15), color = "cadetblue") +
  geom_line(aes(x = point_no, y = gradient_backward_a10), color = "indianred3") +
  labs(x = "Point", y = "Backward Derivative")

write_parquet(together, here("Data/grandslams3.parquet"))
  