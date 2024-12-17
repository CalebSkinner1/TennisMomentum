# Randomizing players

library("here")
library("tidyverse")
library("arrow")

gs <- read_parquet(here("grandslams.parquet"))

gs %>% glimpse()

player1_attributes <- c(
  "player1", "ranking1", "p1_sets", "p1_games", "p1_score", "p1_points_won", "p1_ace", "p1_winner",
  "p1_double_fault", "p1_unf_error","p1_net_pt", "p1_net_pt_won", "p1_break_pt", "p1_break_pt_won",
  "p1_break_pt_missed", "p1_distance_run", "ht1", "hand1", "age1", "b365_1", "ps_1")

player2_attributes <- c(
  "player2", "ranking2", "p2_sets", "p2_games", "p2_score", "p2_points_won", "p2_ace", "p2_winner",
  "p2_double_fault", "p2_unf_error","p2_net_pt", "p2_net_pt_won", "p2_break_pt", "p2_break_pt_won",
  "p2_break_pt_missed", "p2_distance_run", "ht2", "hand2", "age2", "b365_2", "ps_2")

flipped_attributes <- c(
  "server", "status", "point_victor", "game_victor", "set_victor", "match_victor", "tiebreak_victory")

negated_attributes <- c(
  "adj_point_margin", "level_backward", "level_forward", "gradient_backward",
  "spline_backward", "gradient_forward", "spline_forward", "run")

temp <- gs %>% distinct(match_id) %>%
  mutate(player = if_else(runif(nrow(.), 0,1) > .5, "switch", "keep")) %>%
  left_join(gs, by = "match_id") %>%
  mutate(
    # player 1 attributes
    n_player1 = if_else(player == "switch", player2, player1),
    n_ranking1 = if_else(player == "switch", ranking2, ranking1),
    n_p1_sets = if_else(player == "switch", p2_sets, p1_sets),
    n_p1_games = if_else(player == "switch", p2_games, p1_games),
    n_p1_score = if_else(player == "switch", p2_score, p1_score),
    n_p1_points_won = if_else(player == "switch", p2_points_won, p1_points_won),
    n_p1_ace = if_else(player == "switch", p2_ace, p1_ace),
    n_p1_winner = if_else(player == "switch", p2_winner, p1_winner),
    n_p1_double_fault = if_else(player == "switch", p2_double_fault, p1_double_fault),
    n_p1_unf_err = if_else(player == "switch", p2_unf_err, p1_unf_err),
    n_p1_net_pt = if_else(player == "switch", p2_net_pt, p1_net_pt),
    n_p1_net_pt_won = if_else(player == "switch", p2_net_pt_won, p1_net_pt_won),
    n_p1_break_pt = if_else(player == "switch", p2_break_pt, p1_break_pt),
    n_p1_break_pt_won = if_else(player == "switch", p2_break_pt_won, p1_break_pt_won),
    n_p1_break_pt_missed = if_else(player == "switch", p2_break_pt_missed, p1_break_pt_missed),
    n_p1_distance_run = if_else(player == "switch", p2_distance_run, p1_distance_run),
    n_ht1 = if_else(player == "switch", ht2, ht1),
    n_hand1 = if_else(player == "switch", hand2, hand1),
    n_age1 = if_else(player == "switch", age2, age1),
    n_b365_1 = if_else(player == "switch", b365_2, b365_1),
    n_ps_1 = if_else(player == "switch", ps_2, ps_1),
    
    # player 2 attributes
    player2 = if_else(player == "switch", player1, player2),
    ranking2 = if_else(player == "switch", ranking1, ranking2),
    p2_sets = if_else(player == "switch", p1_sets, p2_sets),
    p2_games = if_else(player == "switch", p1_games, p2_games),
    p2_score = if_else(player == "switch", p1_score, p2_score),
    p2_points_won = if_else(player == "switch", p1_points_won, p2_points_won),
    p2_ace = if_else(player == "switch", p1_ace, p2_ace),
    p2_winner = if_else(player == "switch", p1_winner, p2_winner),
    p2_double_fault = if_else(player == "switch", p1_double_fault, p2_double_fault),
    p2_unf_err = if_else(player == "switch", p1_unf_err, p2_unf_err),
    p2_net_pt = if_else(player == "switch", p1_net_pt, p2_net_pt),
    p2_net_pt_won = if_else(player == "switch", p1_net_pt_won, p2_net_pt_won),
    p2_break_pt = if_else(player == "switch", p1_break_pt, p2_break_pt),
    p2_break_pt_won = if_else(player == "switch", p1_break_pt_won, p2_break_pt_won),
    p2_break_pt_missed = if_else(player == "switch", p1_break_pt_missed, p2_break_pt_missed),
    p2_distance_run = if_else(player == "switch", p1_distance_run, p2_distance_run),
    ht2 = if_else(player == "switch", ht1, ht2),
    hand2 = if_else(player == "switch", hand1, hand2),
    age2 = if_else(player == "switch", age1, age2),
    b365_2 = if_else(player == "switch", b365_1, b365_2),
    ps_2 = if_else(player == "switch", ps_1, ps_2),
    
    # flipped attributes
    server = case_when(
      player == "switch" & server == 1 ~ 2,
      player == "switch" & server == 2 ~ 1,
      .default = server),
    status = case_when(
      player == "switch" & status == "p1_break" ~ "p2_break",
      player == "switch" & status == "p2_break" ~ "p1_break",
      .default = status),
    point_victor = case_when(
      player == "switch" & point_victor == 1 ~ 2,
      player == "switch" & point_victor == 2 ~ 1,
      .default = point_victor),
    game_victor = case_when(
      player == "switch" & game_victor == 1 ~ 2,
      player == "switch" & game_victor == 2 ~ 1,
      .default = game_victor),
    set_victor = case_when(
      player == "switch" & set_victor == 1 ~ 2,
      player == "switch" & set_victor == 2 ~ 1,
      .default = set_victor),
    match_victor = case_when(
      player == "switch" & match_victor == 1 ~ 2,
      player == "switch" & match_victor == 2 ~ 1,
      .default = match_victor),
    tiebreak_victory = case_when(
      player == "switch" & tiebreak_victory == "1" ~ "2",
      player == "switch" & tiebreak_victory == "2" ~ "1",
      .default = tiebreak_victory),
    
    # negated attributes
    adj_point_margin = if_else(player == "switch", -adj_point_margin, adj_point_margin),
    level_backward = if_else(player == "switch", -level_backward, level_backward),
    level_forward = if_else(player == "switch", -level_forward, level_forward),
    gradient_backward = if_else(player == "switch", -gradient_backward, gradient_backward),
    gradient_forward = if_else(player == "switch", -gradient_forward, gradient_forward),
    spline_backward = if_else(player == "switch", -spline_backward, spline_backward),
    spline_forward = if_else(player == "switch", -spline_forward, spline_forward),
    run = if_else(player == "switch", -run, run))
  

temp2 <- temp %>% select(-c("player1", "ranking1", "p1_sets", "p1_games", "p1_score", "p1_points_won", "p1_ace", "p1_winner",
  "p1_double_fault", "p1_unf_err","p1_net_pt", "p1_net_pt_won", "p1_break_pt", "p1_break_pt_won",
  "p1_break_pt_missed", "p1_distance_run", "ht1", "hand1", "age1", "b365_1", "ps_1")) %>%
  rename_with(~str_sub(.x, 3, -1L), starts_with("n_")) %>%
  select(
    match_id, player1, ranking1, player2, ranking2, elapsed_time, time_diff, set_no, game_no,
    point_no, server, serve_no, status, p1_sets, p2_sets, p1_games, p2_games, p1_score, p2_score, point_victor,
    p1_points_won, p2_points_won, adj_point_margin, level_backward, level_forward, gradient_backward,
    spline_backward, gradient_forward, spline_forward, game_victor, set_victor, match_victor, p1_ace,
    p2_ace, p1_winner, p2_winner, winner_shot_type, p1_double_fault, p2_double_fault,
    p1_unf_err, p2_unf_err, p1_net_pt, p2_net_pt, p1_net_pt_won, p2_net_pt_won, p1_break_pt,
    p2_break_pt, p1_break_pt_won, p2_break_pt_won, p1_break_pt_missed, p2_break_pt_missed, p1_distance_run,
    p2_distance_run, rally_count, speed_mph, serve_width, serve_depth, return_depth, ht1, hand1, age1, ht2, hand2,
    age2, b365_1, b365_2, ps_1, ps_2, year, tournament, match, tiebreak, retired, delay, first_pt_set, last_pt_set,
    match_pt, run, sex, interruption, change_ends, tiebreak_victory) %>%
  mutate(
    tiebreak_victory = factor(tiebreak_victory))

write_parquet(temp2, here("grandslams2.parquet"))

# gs %>% filter(match_id == "2011-ausopen-1113") %>% glimpse()
# temp2 %>% filter(match_id == "2011-ausopen-1113") %>% select(p1_break_pt_missed:tiebreak_victory) %>% view()
\
# gs %>% glimpse()
# temp2 %>% glimpse()

# temp2 %>% distinct(match_id, match_victor) %>% count(match_victor)

