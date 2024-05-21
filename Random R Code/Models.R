# Models
# Author: Caleb Skinner

# libraries
library("tidyverse")
library("magrittr")
library("ggpubr")
library("fields")
library("zoo")
library("lubridate")
library("tidymodels")
library("here")
library("arrow")
library("performance")
library("car")

# reading in parquet
# gs <- read_parquet(here("grandslams2.parquet"))
# cov <- read_parquet(here("covariates.parquet"))

# match_code <- "2023-wimbledon-1701"

# covariate functions - color covariates

# covariate ideas - serve, rally length, break saved, break converted, interruption/change ends, tiebreak victory, pressure point
# we need to find a way to adjust for player rank - betting odds would be ideal.
# way to determine which players are more volatile?

# https://theanalyst.com/na/2022/03/capturing-momentum-in-tennis/ - leverage points, pressure points

# Visualizing Covariates --------------------------------------------------

covariates <- function(df, match_code, covariate){
  df <- df %>% filter(match_id == match_code) %>%
    mutate(
      diff = p1_points_won - p2_points_won)
  vlines <- df %>% filter(set_victor != 0) %>% dplyr::select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  p1 <- df$player1[1]
  p2 <- df$player2[1]
  tourney <- df$tournament[1]
  gender <- df$sex[1]
  
  df0 <- df %>% mutate(
    server_name = if_else(server == 1, player1, player2),
    rally_count = rally_count,
    break_saved = case_when(
      p1_break_pt_missed == 1 ~ player2,
      p2_break_pt_missed == 1 ~ player1,
      .default = NA),
    break_converted = case_when(
      p1_break_pt_won == 1 ~ player1,
      p2_break_pt_won == 1 ~ player2,
      .default = NA),
    time_diff = minute(elapsed_time) - lag(minute(elapsed_time) + second(elapsed_time)/60 + lag(second(elapsed_time)/60)),
    interruption = case_when(
      game_no == 4 & lag(game_no) == 3 ~ 90,
      game_no == 6 & lag(game_no) == 5 ~ 90,
      game_no == 8 & lag(game_no) == 7 ~ 90,
      game_no == 10 & lag(game_no) == 9 ~ 90,
      game_no == 12 & lag(game_no) == 11 ~ 90,
      game_no == 1 & lag(game_no) > 1 & set_no > 1 ~ 120,
      time_diff > 5 ~ 300,
      .default = NA),
    p1_tiebreak_score = if_else(tiebreak == 1, p1_score, "0"),
    p1_tiebreak_score = as.integer(p1_tiebreak_score),
    p2_tiebreak_score = if_else(tiebreak == 1, p2_score, "0"),
    p2_tiebreak_score = as.integer(p2_tiebreak_score),
    change_ends = case_when(
      interruption > 0 ~ "change",
      game_no == 2 & lag(game_no) == 1 ~ "change",
      tiebreak == 1 & (p1_tiebreak_score + p2_tiebreak_score) %% 6 == 0 ~ "change",
      .default = NA),
    tiebreak_victory = case_when(
      tiebreak == 1 & lead(tiebreak) == 0 & point_victor == 1 ~ player1,
      tiebreak == 1 & lead(tiebreak) == 0 & point_victor == 2 ~ player2,
      .default = NA),
    pressure_point = case_when(
      # potentially replace with leverage metric...
      p1_games == 5 ~ "pressure",
      p1_games == 6 ~ "pressure",
      p2_games == 5 ~ "pressure",
      p2_games == 6 ~ "pressure",
      p1_break_pt == 1 ~ "pressure",
      p2_break_pt == 1 ~ "pressure",
      .default = NA)) %>% dplyr::select(-contains("tiebreak_score"))
  
    break_hold <- df %>%
    group_by(game_no, set_no) %>%
    summarise(victor = sum(game_victor),
              server = mean(server)) %>%
      mutate(
        hold = if_else(server == victor, 1, 0),
        brk = if_else(server != victor, 1, 0)
      ) %>% dplyr::select(-server, -victor)
  
    df <- df0 %>% left_join(break_hold, by = join_by(game_no, set_no)) %>%
      mutate(break_saved = if_else(hold == 1, break_saved, NA))
    
  p <- df %>% ggplot(aes(point_no, diff)) +
    geom_vline(xintercept = vlines) +
    labs(y = "Points Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)"))
  
  # case_when(
  #   covariate == "server"
  # )
  
  
  if(covariate == "server"){
    p <- p + geom_point(aes(color = factor(server_name)))}
  else{
    if(covariate == "rally_length"){
      p <- p + geom_point(aes(color = rally_count))}
    else{
      if(covariate == "break_saved"){
        p <- p + geom_point(aes(color = factor(break_saved)))}
      else{
        if(covariate == "break_converted"){
          p <- p + geom_point(aes(color = factor(break_converted)))}
        else{
          if(covariate == "interruption"){
            p <- p + geom_point(aes(color = factor(interruption)))}
          else{
            if(covariate == "change_ends"){
              p <- p + geom_point(aes(color = factor(change_ends)))}
            else{
              if(covariate == "tiebreak_victory"){
                p <- p + geom_point(aes(color = factor(tiebreak_victory)))}
              else{
                if(covariate == "pressure_point"){
                  p <- p + geom_point(aes(color = factor(pressure_point)))}
              }
            }
          }
        }
      }
    }
  }
  p <- p + labs(color = covariate)
  print(p)
}

# gs %>% filter(match_id == match_code) %>% view()

# covariates
gs %>% covariates(match_code, "server")
gs %>% covariates(match_code, "rally_length")
gs %>% covariates(match_code, "break_saved")
gs %>% covariates(match_code, "break_converted")
gs %>% covariates(match_code, "interruption")
gs %>% covariates(match_code, "change_ends")
gs %>% covariates(match_code, "tiebreak_victory")
gs %>% covariates(match_code, "pressure_point")
# length of game

# Transforming data --------------------------------

# deriv <- derivative_basis(gs, match_code = "2023-wimbledon-1601", l = 20, print = "tibble")

# selecting appropriate covariates
# filtered for ease
{testing0 <- gs %>%
  group_by(match_id) %>%
  mutate(
    point_victor = factor(if_else(point_victor == 2, 0, 1)),
    break_saved = case_when(
      p1_break_pt_missed == 1 ~ 2,
      p2_break_pt_missed == 1 ~ 1,
      .default = 0),
    break_converted = factor(case_when(
      p1_break_pt_won == 1 ~ 1,
      p2_break_pt_won == 1 ~ 2,
      .default = 0)),
    pressure_point = factor(case_when(
      # potentially replace with leverage metric...
      p1_games == 5 ~ 1,
      p1_games == 6 ~ 1,
      p2_games == 5 ~ 1,
      p2_games == 6 ~ 1,
      p1_break_pt == 1 ~ 1,
      p2_break_pt == 1 ~ 1,
      .default = 0)))

break_hold <- testing0 %>%
  group_by(match_id, set_no, game_no) %>%
  summarise(
    victor = sum(game_victor),
    server = mean(server),
    total_breaks_saved = sum(break_saved)) %>%
  mutate(
    total_breaks_saved = total_breaks_saved/server,
    hold = if_else(server == victor, 1, 0),
    brk = if_else(server != victor, 1, 0)) %>%
  select(-server, -victor)

tiebreak_info <- gs %>%
  filter(tiebreak_victory == 1) %>%
  mutate(
    tiebreak_close_victory = factor(if_else(abs(as.integer(p1_score) - as.integer(p2_score)) == 1, 1, 0)),
    tiebreak_very_close_victory = factor(if_else(tiebreak_close_victory == 1 & (as.integer(p1_score) + as.integer(p2_score)) > 12, 1, 0))) %>%
  select(match_id, point_no, tiebreak_close_victory, tiebreak_very_close_victory)

testing <- testing0 %>%
  left_join(break_hold, by = join_by(match_id, set_no, game_no)) %>%
  mutate(
    total_breaks_saved = if_else(hold == 1, total_breaks_saved, 0),
    server = factor(server),
    tbs = factor(total_breaks_saved),
    recover_to_hold = if_else(total_breaks_saved > 0, game_victor, 0)) %>%
  left_join(tiebreak_info, by = join_by(match_id, point_no)) %>%
  mutate(across(contains("close_victory"), ~replace_na(., "0")))

# creating and isolating covariates and predicted
cov <- testing %>%
  group_by(match_id) %>%
  mutate(
    last_pt_game = if_else(game_victor != 0, 1, 0),
    break_saved = factor(break_saved),
    p1_winner = factor(p1_winner),
    p2_winner = factor(p2_winner),
    p1_ace = factor(p1_ace),
    p2_ace = factor(p2_ace),
    p1_double_fault = factor(p1_double_fault),
    p2_double_fault = factor(p2_double_fault),
    p1_unf_err = factor(p1_unf_err),
    p2_unf_err = factor(p2_unf_err),
    lag1_gradient_backward = lag(gradient_backward),
    lag2_gradient_backward = lag(gradient_backward, 2),
    lag3_gradient_backward = lag(gradient_backward, 3),
    lag4_gradient_backward = lag(gradient_backward, 4),
    lag1_gradient_forward = lag(gradient_forward),
    lag1_point_victor = lag(point_victor, 1),
    lag2_point_victor = lag(point_victor, 2),
    lag3_point_victor = lag(point_victor, 3),
    lag4_point_victor = lag(point_victor, 4),
    lag5_point_victor = lag(point_victor, 5),
    lag1_server = lag(server, 1),
    lag2_server = lag(server, 2),
    lag3_server = lag(server, 3),
    lag4_server = lag(server, 4),
    lag5_server = lag(server, 5),
    lag1_break_saved = lag(break_saved, 1),
    lag2_break_saved = lag(break_saved, 2),
    lag3_break_saved = lag(break_saved, 3),
    lag4_break_saved = lag(break_saved, 4),
    lag5_break_saved = lag(break_saved, 5),
    lag1_break_converted = lag(break_converted, 1),
    lag2_break_converted = lag(break_converted, 2),
    lag3_break_converted = lag(break_converted, 3),
    lag4_break_converted = lag(break_converted, 4),
    lag5_break_converted = lag(break_converted, 5),
    lag1_interruption = lag(interruption, 1),
    lag2_interruption = lag(interruption, 2),
    lag3_interruption = lag(interruption, 3),
    lag4_interruption = lag(interruption, 4),
    lag5_interruption = lag(interruption, 5),
    lag1_change_ends = lag(change_ends, 1),
    lag2_change_ends = lag(change_ends, 2),
    lag3_change_ends = lag(change_ends, 3),
    lag4_change_ends = lag(change_ends, 4),
    lag5_change_ends = lag(change_ends, 5),
    point_margin = p1_points_won - p2_points_won,
    lag_point_margin = lag(point_margin),
    sex = factor(if_else(sex == 1, "men", "women")),
    server = factor(if_else(server == 1, 1, 0)),
    last_pt_set = factor(last_pt_set),
    bet_odds_a = (1/ps_1)*100,
    bet_odds_b = (1/ps_2)*100,
    bet_odds = bet_odds_a*100/(bet_odds_a + bet_odds_b),
    total_distance_run = p1_distance_run + p2_distance_run,
    recover_to_hold = factor(recover_to_hold),
    long_point = factor(case_when(
      total_distance_run > 60 ~ 1,
      rally_count > 6 ~ 1,
      .default = 0))) %>%
  select(year, tournament, match_id, point_no, contains("point_margin"), bet_odds, contains("point_victor"),
         server, break_saved, break_converted, recover_to_hold, game_victor,
         interruption, change_ends, sex, contains("backward"), last_pt_game,
         contains("forward"), long_point, tiebreak_victory, tiebreak_close_victory,
         tiebreak_very_close_victory, last_pt_set, contains("_winner"), contains("ace"),
         contains("double_fault"), contains("unf_err")) %>%
  na.omit() %>%
  ungroup()

# write_parquet(cov, here("covariates.parquet"))

cov_shortened <- cov %>%
  filter(year %in% c(2022, 2023)) #%>%
  #select(-year, -tournament)

rm(break_hold, testing0, tiebreak_info)
}

# cov %>% glimpse()

# Visualizing Derivatives -------------------------------------------------

# ok, this section can be deleted at some point. But right now I am not sure what is happening with derivatives.
# I need to see if derivatives go up after winning the previous point. Or what the derivative measures
# derivative is supposed to be the derivative AFTER the point, because it takes it into account

# change in derivative after points
# cov %>% mutate(
#   change3_derivative = lag2_derivative - lag3_derivative) %>%
#   group_by(lag4_point_victor, lag3_point_victor, lag2_point_victor, lag1_point_victor, point_victor) %>%
#   summarise(
#     deriv = mean(lag2_derivative),
#     change_deriv = mean(change3_derivative)) %>%
#   rename(
#     "2prev_point" = "lag4_point_victor",
#     "prev_point" = "lag3_point_victor",
#     "this_point" = "lag2_point_victor",
#     "next_point" = "lag1_point_victor",
#     "2next_point" = "point_victor") %>% print(n = 32)

# change in derivative after several points
cov_shortened %>%
  mutate(change1_derivative = derivative - lag1_derivative,
         change2_derivative = lag1_derivative - lag2_derivative,
         change3_derivative = lag2_derivative - lag3_derivative) %>%
  group_by(lag2_point_victor, lag1_point_victor, point_victor) %>%
  summarise(
    lag3_deriv = mean(lag3_derivative),
    change3_deriv = mean(change3_derivative),
    lag2_deriv = mean(lag2_derivative),
    change2_deriv = mean(change2_derivative),
    lag1_deriv = mean(lag1_derivative),
    change1_deriv = mean(change1_derivative),
    deriv = mean(derivative)) %>%
  relocate(lag2_point_victor, .after = lag3_deriv) %>%
  relocate(lag1_point_victor, .after = lag2_deriv) %>%
  relocate(point_victor, .after = lag1_deriv) # %>% dplyr::select(-lag3_deriv, -lag2_deriv, -lag1_deriv, -deriv)
  
# visualizing one match - trying to find trends
cov_shortened %>% filter(match_id == "2023-wimbledon-1701") %>%
  mutate(derivative = round(derivative, digits = 3),
         change_derivative = round(derivative - lag1_derivative, digits = 3)) %>%
  dplyr::select(point_no, point_victor, point_margin, derivative, change_derivative)

# derivative by lag_derivative
cov_shortened %>%
  ggplot() +
  geom_point(aes(x = lag1_derivative, y = derivative, color = point_victor))

# derivative by lag1_point_victor
cov_shortened %>%
  ggplot() +
  geom_jitter(aes(x = lag1_point_victor, y = derivative, color = point_victor))

# change derivative by server
cov_shortened %>%
  mutate(change_derivative = derivative - lag1_derivative) %>%
  ggplot() +
  geom_jitter(aes(x = server, y = change_derivative), alpha = .1)

# change derivative by lag1_derivative - this is good news, i think
cov_shortened %>%
  mutate(change_derivative = derivative - lag1_derivative) %>%
  ggplot() +
  geom_point(aes(x = lag1_derivative, y = change_derivative, color = point_victor))

# change in derivative after converting a break point
cov_shortened %>%
  mutate(change_derivative = derivative - lag1_derivative) %>%
  # filter(lag1_break_converted != 0) %>%
  group_by(lag1_break_converted) %>%
  summarise(
    mean_derivative_change = mean(change_derivative),
    p1_won = sum(point_victor == 1),
    p2_won = sum(point_victor == 2)
    # points_won = sum(point_victor)/n()
  )

# change in derivative after saving a break point
cov_shortened %>%
  mutate(change_derivative = derivative - lag1_derivative) %>%
  # filter(lag1_break_converted != 0) %>%
  group_by(lag1_break_saved, lag1_server) %>%
  summarise(
    mean_derivative_change = mean(change_derivative),
    p1_won = sum(point_victor == 1),
    p2_won = sum(point_victor == 2)
    # points_won = sum(point_victor)/n()
  )

# cov %>% mutate(change_derivative = derivative - lag_derivative) %>%
#   filter(lag1_break_converted != 0) %>%
#   dplyr::select(lag1_break_converted, point_victor, contains("derivative"))

# derivative by break_point
cov_shortened %>%
  mutate(change_derivative = derivative - lag1_derivative) %>%
  filter(lag1_break_converted != 0) %>%
  ggplot() +
  geom_point(aes(x = lag1_break_converted, y = change_derivative))

# derivative
lm_b_deriv <- lm_spec %>%
  fit(derivative ~ server + lag1_derivative + factor(lag1_break_converted), data = cov)

lm_b_deriv %>% 
  pluck("fit") %>%
  summary()

lm_derivative <- lm_spec %>%
  fit(derivative ~ server + lag1_server:lag1_point_victor + lag1_derivative +
        lag1_server:factor(lag1_break_saved) + factor(lag1_break_converted) +
        lag1_derivative:factor(lag1_interruption) + lag1_derivative:factor(lag1_change_ends), data = cov)

lm_derivative %>%
  pluck("fit") %>%
  summary()

tidy(lm_derivative)

glance(lm_derivative)

prev_point <- lm_spec %>%
  fit(derivative ~ lag1_derivative + point_victor, data = cov)

prev_point %>% 
  pluck("fit") %>%
  summary()

# structure
lm_structure <- lm_spec %>%
  fit(point_margin ~ lag_point_margin + server + ps_1 + b365_1, data = cov)

lm_structure %>% 
  pluck("fit") %>%
  summary()

lm_bet <- lm_spec %>%
  fit(point_margin ~ lag_point_margin + b365_1 + ps_1 + server, data = cov)

lm_bet %>%
  pluck("fit") %>%
  summary()

plot(x = lm_bet$fit$residuals, y = cov$ps_1)

ggplot(cov) +
  geom_point(aes(x = ps_1, y = point_margin))

resid <- lm_structure$fit$residuals

plot(resid)

# Point Margin and Point Victor ------------------------------------------------------------

# lm_margin <- lm_spec %>%
#   fit(point_margin ~ server + lag1_server:lag1_point_victor + lag_point_margin +
#         lag1_server:factor(lag1_break_saved) + factor(lag1_break_converted) +
#         lag_point_margin:factor(lag1_interruption) + lag_point_margin:factor(lag1_change_ends), data = cov)
# 
# lm_margin %>% 
#   pluck("fit") %>%
#   summary()
# 
# tidy(lm_margin)
# 
# glance(lm_margin)


# Does Momentum Exist? ----------------------------------------------------

lr_mod <- logistic_reg() %>% 
  set_engine("glm")

exist_momentum <- lr_mod %>%
  fit(point_victor ~ tournament:sex:server + lag1_gradient_backward + bet_odds, data = cov)

exist_momentum %>%
  pluck("fit") %>%
  summary()

# To do: make table of results and begin to note the interpretations
# make confidence intervals and convert to odds ratios
# RoC curve
# hold sex, server, betting odds, and tournament constant for probability odds

# TABLE
original_em <- exist_momentum %>%
  tidy() %>%
  select(-statistic) %>%
  rename(`log-odds` = estimate)

# one unit change in momentum
odr_em <- original_em %>%
  mutate(
    `odds-ratio` = exp(`log-odds`),
    `2.5% OR` = exp(`log-odds` - 1.96 * std.error),
    `97.5% OR` = exp(`log-odds` + 1.96 * std.error)) %>%
  select(-std.error, -`log-odds`, -p.value) %>% 
  relocate(`2.5% OR`, .before = `odds-ratio`)

# .5 unit change in momentum
half_odr_em <- original_em %>% filter(term == "lag1_gradient_backward") %>%
  mutate(
    `odds-ratio` = exp(.5 * `log-odds`),
    `2.5% OR` = exp(.5 * `log-odds` - 1.96 * .5 * std.error),
    `97.5% OR` = exp(.5 * `log-odds` + 1.96 * .5 * std.error)) %>%
  select(-std.error, -`log-odds`, -p.value) %>% 
  relocate(`2.5% OR`, .before = `odds-ratio`)

# ROC Curve
full <- cov %>%
  select(point_victor, tournament, sex, server, bet_odds, lag1_gradient_backward) %>%
  augment(exist_momentum, .)

full %>%
  roc_curve(truth = point_victor, .pred_0) %>%
  autoplot()

cov %>%
  select(point_victor, tournament, sex, server, bet_odds, lag1_gradient_backward) %>%
  augment(exist_momentum, .) %>%
  accuracy(truth = point_victor, estimate = .pred_class)

# Why is this flipped?

full %>% roc_auc(truth = point_victor, .pred_0)

# What causes momentum (point level)? ------------------------------------------------------

# gradient forward - momentum AFTER point ends, does not include current point result
# gradient backward - momentum up to current point, includes current point result!

# go in appendix

# with rally count
gs %>%
  summarise(
    q1 = quantile(rally_count, .25, na.rm = TRUE),
    mean = mean(rally_count, na.rm = TRUE),
    q3 = quantile(rally_count, .75, na.rm = TRUE),
    `85th` = sum(rally_count > 5, na.rm = TRUE)/sum(!is.na(rally_count)))
# use rally count > 5 (18.7% of points longer)

# with distance run
gs %>%
  summarise(
    q1 = quantile(p1_distance_run, .25, na.rm = TRUE),
    mean = mean(p1_distance_run, na.rm = TRUE),
    q3 = quantile(p1_distance_run, .75, na.rm = TRUE),
    `85th` = sum(p1_distance_run > 25, na.rm = TRUE)/sum(!is.na(p1_distance_run)))

# use distance run > 25 (18.1% of points were longer)

lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# interesting and all good on the point level
cov_model <- lm_spec %>%
  fit(gradient_forward ~ server:sex + server + bet_odds + break_converted + break_saved +
        point_victor + recover_to_hold + point_victor:long_point + p1_winner + p2_winner +
        p1_ace + p2_ace + p1_double_fault + p2_double_fault, data = cov) # cov

cov_model %>%
  pluck("fit") %>%
  summary()

# cumulative model for running

# fix recover to hold, check long point, and winners/double faults

cov_model %>% check_collinearity()

# with long list of point results, need rally count and distance run
# cov_model <- lm_spec %>%
#   fit(gradient_forward ~ server:sex + recover_to_hold + bet_odds + break_converted +
#         break_saved + point_victor + tiebreak_victory + lag1_point_victor + lag2_point_victor + lag3_point_victor +
#         lag4_point_victor + lag5_point_victor + point_victor:last_pt_set, data = cov)

# thoughts: good with short term, breaks, point winner, controls, but weak with long term impact like
# new set, interruptions, change_ends, tiebreak_victory are struggling - need to find a way to identify

# multicollinear (vif, w/ and w/o), interpret, predict

# what causes momentum? limited data --------------------------------------

# in appendix

# with betting odds
cov %>%
  summarise(
    q1 = quantile(bet_odds, .25, na.rm = TRUE),
    mean = mean(bet_odds, na.rm = TRUE),
    median = median(bet_odds, na.rm = TRUE),
    q3 = quantile(bet_odds, .75, na.rm = TRUE),
    mid_twentieth = sum(between(bet_odds, 1.6, 2.5))/sum(!is.na(bet_odds)))

# reducing data
cov_close <- cov %>% filter(between(bet_odds, 40, 60))

# more point level, but with matches with close betting odds (implied probability between 33.33% and 66.67%)
cov_close_model <- lm_spec %>%
  fit(gradient_forward ~ sex:server + server + bet_odds + break_converted + break_saved +
        point_victor + recover_to_hold + point_victor:long_point + p1_winner + p2_winner +
        p1_ace + p2_ace + p1_double_fault + p2_double_fault, data = cov_close)

cov_close_model %>%
  pluck("fit") %>%
  summary()

# What causes momentum? game level ----------------------------------------

# new set, interruptions, change_ends, tiebreak_victory are struggling

game_cov <- cov %>%
  group_by(match_id) %>%
  mutate(
    interruption = factor(lead(interruption)),
    change_ends = factor(lead(change_ends))) %>%
  filter(last_pt_game == 1) %>%
  mutate(
    lead1_gradient_forward = lead(gradient_forward),
    game_victor = factor(if_else(game_victor == 2, 0, 1))) %>%
  ungroup()

game_cov_model <- lm_spec %>%
  fit(gradient_forward ~ game_victor + server + bet_odds + interruption:game_victor +
        change_ends:game_victor + tiebreak_victory + last_pt_set:game_victor, data = game_cov)

game_cov_model %>%
  pluck("fit") %>%
  summary()

game_cov_model %>% check_collinearity()

# game_cov_model_1game <- lm_spec %>%
#   fit(lead1_gradient_forward ~ game_victor + server + bet_odds + interruption:game_victor +
#         change_ends:game_victor + tiebreak_victory + last_pt_set:game_victor, data = game_cov)
# 
# game_cov_model_1game %>%
#   pluck("fit") %>%
#   summary()

# what causes momentum? game and close ------------------------------------

game_cov_close <- game_cov %>%
  filter(between(bet_odds, 40, 60))

game_cov_model_close <- lm_spec %>%
  fit(gradient_forward ~ game_victor + server + bet_odds + interruption:game_victor +
        change_ends:game_victor + tiebreak_victory + last_pt_set:game_victor, data = game_cov_close)

game_cov_model_close %>%
  pluck("fit") %>%
  summary()

game_cov_model_close_1game <- lm_spec %>%
  fit(lead1_gradient_forward ~ game_victor + server + bet_odds + interruption:game_victor +
        change_ends:game_victor + tiebreak_victory + last_pt_set:game_victor, data = game_cov_close)

game_cov_model_close_1game %>%
  pluck("fit") %>%
  summary()
