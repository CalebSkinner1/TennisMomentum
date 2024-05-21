# Author: Caleb Skinner
# Last Modified: July 6
# Title: Grand Slam tennis descriptive statistics

# load in libraries
library("tidyverse"); theme_set(theme_bw())
library("roll")
library("janitor")
library("arrow")

# load in data
gs <- read_parquet("grandslams2.parquet")
# gs2 <- read_parquet("grandslams2.parquet")


# sliced view of first 1000 points
# sliced <- gs %>% 
#   slice(1:1000)

# number of sets in match
gs %>%
  filter(last_pt_set == 1) %>%
  group_by(match_id) %>%
  summarise(
    sets = n()) %>%
  group_by(sets) %>%
  summarise(
    count = n())

tiebreaks <- gs %>%
  filter(tiebreak == 1) %>%
  select(match_id, last_pt_set, set_no, set_victor, game_victor, p1_score, p2_score)

# Any bias over who is player 1 and player 2? By set.
gs %>%
  filter(last_pt_set == 1) %>%
  group_by(set_victor) %>% 
  summarise(
    count = n()) %>%
  filter(set_victor != 0) %>%
  mutate(
    prop = count/sum(count))

# Any bias over who is player 1 and player 2? By match
gs %>%
  distinct(match_id, match_victor) %>%
  count(match_victor) %>%
  mutate(
    prop = n/sum(n))

# Any bias over who is player 1 and player 2? By match
gs %>%
  count(point_victor) %>%
  mutate(
    prop = n/sum(n))

# percentage of points won by server; the two are basically identical but opposite halves
gs %>%
  filter(server==1) %>%
  group_by(point_victor) %>%
  summarise(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))
# Player 1 won .615 of serves

gs %>%
  filter(server==2) %>%
  group_by(point_victor) %>%
  summarise(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))
# Player 2 won .613 of serves

# For Descriptive Data (only), I will only apply Player 1 as server.
# Player 2 as server will be removed for ease.
# Reminder: Player 1 won .623 of serves and .377 of returns

# PPW after... ------------------------------------------------------------

# PPW after a break (.619)
gs %>%
  mutate(break_lag = lag(p1_break_pt_won)) %>% slice(-1) %>%
  filter(break_lag == 1) %>%
  group_by(point_victor) %>% 
  summarise(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))

# First point of game (.616)
gs %>%
  filter(tiebreak == 0) %>% 
  mutate(fp_match = if_else(elapsed_time == 0, 1, 0),
         fp_game = abs(server - lag(server)),
         fp_all = if_else(fp_match == 1 | fp_game == 1, 1, 0)) %>%
  filter(fp_all == 1 & server == 1) %>%
  group_by(point_victor) %>% 
  summarise(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))

# First point of set (.606)
gs %>%
  filter(tiebreak == 0) %>% 
  mutate(fp_match = if_else(elapsed_time==0, 1, 0),
         fp_set = abs(set_no - lag(set_no)),
         fp_all = if_else(fp_match == 1 | fp_set == 1, 1, 0)) %>%
  filter(fp_all == 1 & server == 1) %>%
  group_by(point_victor) %>% 
  summarise(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))

# Ace (.655 when serving, .382 when returning)
gs %>%
  mutate(lag_ace = lag(p1_ace)) %>% 
  filter(lag_ace == 1) %>%
  group_by(server, point_victor) %>% 
  summarise(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))

# Double Fault (.605 when serving, .372 when returning)
gs %>%
  mutate(lag_df = lag(p1_double_fault)) %>% 
  filter(lag_df == 1) %>%
  group_by(server, point_victor) %>% 
  summarise(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))

# PPW after saved break point
# server wins .592 of points after saving a break point
gs %>%
  mutate(lag_p1_break_pt_missed = lag(p1_break_pt_missed)) %>% 
  filter(lag_p1_break_pt_missed == 1) %>%
  group_by(point_victor) %>%
  summarize(
    count = n()) %>% 
  mutate(
    "percent" = count / sum(count))

# PPW break points
# server wins .582 of break points
gs %>%
  filter(p1_break_pt == 1) %>%
  group_by(point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW serving for set; server .645
gs %>%
  filter(p1_games == 5 | p1_games == 6,
         p2_games < p1_games,
         server == 1) %>%
  group_by(point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW serving for set- close set (1 break); server .631
gs %>%
  filter(p1_games == 5 | p1_games == 6,
         between(p1_games - p2_games, 0, 2.1),
         server == 1) %>%
  group_by(point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW serving to stay in set; server .594
gs %>%
  filter(p2_games == 5 | p2_games == 6,
         p2_games > p1_games,
         server == 1) %>%
  group_by(point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW serving to stay in set- close set (1 break); server .615
gs %>%
  filter(p2_games == 5 | p2_games == 6,
         between(p2_games - p1_games, 0, 2.1),
         server == 1) %>%
  group_by(point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW after winner; server .633 and returner .398
gs %>%
  mutate(lag_p1_winner = lag(p1_winner)) %>% 
  filter(lag_p1_winner == 1) %>%
  group_by(server, point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW after unforced error; server .604 and returner .392
gs %>%
  mutate(lag_p1_unf_err = lag(p1_unf_err)) %>% 
  filter(lag_p1_unf_err == 1) %>%
  group_by(server, point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW by point; big interesting table
gs %>%
  filter(server == 1,
         tiebreak == 0,
         !is.na(point_victor)) %>%
  group_by(p1_score, p2_score, point_victor) %>% 
  summarize(
    count = n()) %>%
  mutate(
    "P1Win" = count / sum(count)) %>%
  filter(point_victor == 1) %>%
  arrange(p1_score)

# PPW up a break; .700 serving, .300 returning
gs %>%
  filter(status == "p1_break") %>%
  group_by(point_victor, server) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW tiebreak; .632 serving
gs %>%
  filter(tiebreak == 1, server == 1) %>%
  group_by(point_victor) %>%
  summarize(
    count = n()) %>%
  mutate(
    "percent" = count / sum(count))

# PPW First Serve; .683 on 1st serve
gs %>%
  filter(server == 1, !is.na(serve_no)) %>%
  group_by(serve_no, point_victor) %>%
  summarize(
    count = n(),
    speed_mph = mean(speed_mph)) %>%
  mutate(
    "percent" = count / sum(count))

df <- gs
match_code <- "2022-wimbledon-1403"
type <- "difference"

# plot functions ----------------------------------------------------------

# plot runs
runs <- function(df, match_code){
  df <- df %>% filter(match_id == match_code) %>%
    mutate(p1_run = if_else(run > 0, run, NA),
           p2_run = if_else(run < 0, run, NA))
  
  vlines <- df %>% filter(set_victor != 0) %>% select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  p1 <- df$player1[1]
  p2 <- df$player2[1]
  
  df %>% ggplot() +
    geom_point(aes(point_no, p1_run, color = 'p1')) +
    geom_point(aes(point_no, p2_run, color = 'p2')) +
    scale_color_manual(name = 'Player',
                       breaks = c('p1', 'p2'),
                       labels = c(p1, p2),
                       values = c('p1' = 'cadetblue4', 'p2' = 'indianred3')) +
    geom_vline(xintercept = vlines) +
    labs(y = "Consecutive Points Won", x = "Point Number", title = str_c(p1, " vs. ", p2))
}

gs %>% runs("2011-ausopen-1101")
gs %>% runs("2023-wimbledon-1701")
gs %>% runs("2023-wimbledon-2503")
gs %>% runs("2013-frenchopen-1601")
gs %>% runs("2019-wimbledon-1701")
gs %>% runs("2012-ausopen-1701")
gs %>% runs("2018-wimbledon-1601")
gs %>% runs("2012-wimbledon-2101")

# plot points won cumulatively for any match
cum_points <- function(df, match_code, type) {
  df <- df %>% filter(match_id == match_code) %>%
    mutate(
      diff = p1_points_won - p2_points_won)
  vlines <- df %>% filter(set_victor != 0) %>% select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  p1 <- df$player1[1]
  p2 <- df$player2[1]
  
  if(type == "both"){
    p <- df %>% ggplot() +
      geom_point(aes(point_no, p1_points_won, color = 'p1')) +
      geom_point(aes(point_no, p2_points_won, color = 'p2')) +
      scale_color_manual(name = 'Player',
                         breaks = c('p1', 'p2'),
                         labels = c(p1, p2),
                         values = c('p1' = 'cadetblue4', 'p2' = 'indianred3')) +
      geom_vline(xintercept = vlines) +
      labs(y = "Points Won", x = "Point Number", title = str_c(p1, " vs. ", p2))
    print(p)
  }
  if(type == "difference"){
    p <- df %>% ggplot() +
      geom_point(aes(point_no, diff, color = "indianred3")) +
      geom_vline(xintercept = vlines) +
      labs(y = "Points Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
      theme(legend.position = "none")
    print(p)
  }
}

cum_points(gs, "2013-wimbledon-1504", "difference")
cum_points(gs, "2013-wimbledon-1504", "both")

cum_points(gs, "2012-frenchopen-1502", "difference")
cum_points(gs, "2012-frenchopen-1502", "both")

cum_points(gs, "2019-usopen-1701", "difference")
cum_points(gs, "2019-usopen-1701", "both")

cum_points(gs, "2018-wimbledon-1601", "difference")
cum_points(gs, "2018-wimbledon-1601", "both")

cum_points(gs, "2023-wimbledon-2503", "difference")

cum_points(gs, "2023-wimbledon-2503", "both")

gs %>% cum_points("2022-wimbledon-1403", "both")

# plot percentage of points won cumulatively for any match
cum_perc <- function(df, match_code) {
  vlines <- df %>% filter(str_detect(match_id, match_code), set_victor != 0) %>% select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  df %>% filter(str_detect(match_id, match_code)) %>%
    mutate(w_perc_pts_won = if_else(match_victor == 1, p1_points_won, p2_points_won)/point_no,
           l_perc_pts_won = if_else(match_victor == 1, p2_points_won, p1_points_won)/point_no) %>%
    ggplot() +
    geom_point(aes(point_no, w_perc_pts_won, color = 'Winner')) +
    geom_point(aes(point_no, l_perc_pts_won, color = 'Loser')) +
    scale_color_manual(name = 'Player',
                       breaks = c('Winner', 'Loser'),
                       values = c('Winner' = 'cadetblue4', 'Loser' = 'indianred3')) +
    geom_vline(xintercept = vlines) +
    scale_y_continuous(name = "Percentage of Points Won", limits = c(.4, .6))
}

gs %>% cum_perc("2023-wimbledon-1701")

# plot percentage of points won over last n points for any match. f is the frequency of rolling sum
cum_roll_perc <- function(df, match_code, n, f) {
  vlines <- df %>% filter(str_detect(match_id, match_code), set_victor != 0) %>% select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  df %>% filter(str_detect(match_id, match_code)) %>%
    mutate(
      p1 = if_else(point_victor == 1, 1, 0),
      p2 = if_else(point_victor == 2, 1, 0),
      p1_roll = roll_sum(p1, width = n, min_obs = 1),
      p2_roll = roll_sum(p2, width = n, min_obs = 1),
      p1_roll_perc = p1_roll/(p1_roll + p2_roll),
      p2_roll_perc = p2_roll/(p1_roll + p2_roll),
      w_roll_perc = if_else(match_victor == 1, p1_roll_perc, p2_roll_perc),
      l_roll_perc = if_else(match_victor == 2, p1_roll_perc, p2_roll_perc)) %>%
    filter(point_no %% f == 0) %>%
    ggplot() +
    geom_line(aes(point_no, w_roll_perc, color = 'Winner')) +
    geom_line(aes(point_no, l_roll_perc, color = 'Loser')) +
    scale_color_manual(name = 'Player',
                       breaks = c('Winner', 'Loser'),
                       values = c('Winner' = 'cadetblue4', 'Loser' = 'indianred3')) +
    geom_vline(xintercept = vlines) +
    scale_y_continuous(name = str_c("Last ", n, " Points"))
}

gs %>% cum_roll_perc("2023-wimbledon-1701", 25, 10)

fit_plot <- function(df_edit, match_code, df_log) {
  vlines_0 <- df_edit %>% filter(match_id == match_code, set_victor != 0) %>% select(point_no)
  vlines <- vlines_0 %>% slice(1:(n()-1)) %>% pull() + .5
  vlines_0 <- vlines_0 %>% pull()
  set_scores <- df_edit %>%
    filter(match_id == match_code, set_victor != 0) %>%
    mutate(p1_games = p1_games + if_else(point_victor == 1, 1, 0),
           p2_games = p2_games + if_else(point_victor == 2, 1, 0)) %>% select(p1_games, p2_games)
  player1 <- df_edit %>% filter(match_id == match_code) %>% select(player1) %>% distinct() %>% pull()
  player2 <- df_edit %>% filter(match_id == match_code) %>% select(player2) %>% distinct() %>% pull()
  
  remove <- df_log$na.action
  bad <- df_edit %>%
    mutate(
      row = row_number()) %>%
    filter(row %in% remove) %>%
    select(match_id, point_no)
  
  df_edit <- df_edit %>% anti_join(bad)
  
  df <- tibble(fitted = df_log$fitted.values,
               point = df_edit$point_no,
               match_id = df_edit$match_id,
               player1 = df_edit$player1,
               player2 = df_edit$player2,
               winner = factor(if_else(df_edit$point_victor == 1, player1, player2)),
               residual = df_log$residuals,
               break_pt = factor(df_edit$p1_break_pt_won + df_edit$p2_break_pt_won)) %>%
    filter(match_id == match_code)
  
  midpt <- (max(df$fitted) + min(df$fitted))/2
  
  df %>% ggplot(aes(point, fitted)) +
    geom_point(aes(color = winner, shape = break_pt, size = break_pt)) +
    geom_vline(xintercept = vlines) +
    scale_size_discrete(name = "Break", range = c(3,5)) +
    scale_shape_manual(name = "Break", values = c(16,8)) +
    scale_color_discrete(name = "Point Winner") +
    labs(y = str_c(player1), x = "Point", title = str_c(player1, " vs. ", player2)) +
    scale_y_continuous(breaks = seq(.2, .8, .05), sec.axis = sec_axis(name = str_c(player2), ~ 1 - ., breaks = seq(.2, .8, .05))) +
    annotate("text", x = (vlines_0[1] - .5)/2, y = midpt, label = str_c(set_scores[1,1],"-",set_scores[1,2]), color = "darkgrey", size = 7.5) +
    annotate("text", x = (vlines_0[2] - vlines_0[1] - .5)/2 + vlines_0[1], y = midpt, label = str_c(set_scores[2,1],"-",set_scores[2,2]), color = "darkgrey", size = 7.5) +
    annotate("text", x = (vlines_0[3] - vlines_0[2] - .5)/2 + vlines_0[2], y = midpt, label = str_c(set_scores[3,1],"-",set_scores[3,2]), color = "darkgrey", size = 7.5) +
    annotate("text", x = (vlines_0[4] - vlines_0[3] -.5)/2 + vlines_0[3], y = midpt, label = str_c(set_scores[4,1],"-",set_scores[4,2]), color = "darkgrey", size = 7.5) +
    annotate("text", x = (vlines_0[5] - vlines_0[4] -.5)/2 + vlines_0[4], y = midpt, label = str_c(set_scores[5,1],"-",set_scores[5,2]), color = "darkgrey", size = 7.5)
}

# brainstorming -----------------------------------------------------------

# "Hawkes process hawkesbow" - situational variables and momentum variables
# https://hawkeslib.readthedocs.io/en/latest/tutorial.html
# meet with Dr. Hering - indicators of a change
# meet with Dr. Stamey - bayesian

# Ideas
# PPW After missing several first serves
# maybe exclude dominant sets
# PPW distance run
# Three types of sets: 1. domination, 2. early break but close, 3. very close
# PPW following after each of those sets AND variation
# Need to adjust for change of ends
# effectiveness of serve end?
# maybe PPW at four points
# indicator variable for how well the players are doing


# for initial visualization, player 1 serves and not serves
# standardize points by who serves (or maybe just a 0,1 variable)
# autoregressive model -> 1 pt back 2 pt back 3 pt back

# Big Methodology Ideas
# * self exciting model, poisson, residuals would identify momentum
# plot some runs, how often do players go on runs
# * time series analysis
# change point analysis? --> Dr. Herring
# try to identify change points
# * bayesian logistic regression


# logistic regression -----------------------------------------------------

logistic_gs <- gs %>%
  mutate(
    server = if_else(server == 2, 0, server),
    point_victor = if_else(point_victor == 2, 0, point_victor),
    game_victor = if_else(game_victor == 2, 0, game_victor),
    set_victor = if_else(set_victor == 2, 0, set_victor),
    # lag_p1_distance = if_else(first_pt_set == 1, NA, lag(p1_distance_run)),
    # lag_p2_distance = if_else(first_pt_set == 1, NA, lag(p2_distance_run)),
    last_point_victor = if_else(point_no == 1, 0, lag(point_victor)), # technically this 0 should be an NA
    set_diff = p1_sets - p2_sets,
    # lag_p1_ace = if_else(lag(match_pt) == 0, lag(p1_ace), NA),
    # lag_p2_ace = if_else(lag(match_pt) == 0, lag(p2_ace), NA),
    # lag_p1_double_fault = if_else(lag(match_pt) == 0, lag(p1_double_fault), NA),
    # lag_p2_double_fault = if_else(lag(match_pt) == 0, lag(p2_double_fault), NA),
    pt_win_perc = if_else(set_no == 1 & game_no < 4, .5, lag(p1_points_won/(p2_points_won + p1_points_won))) * 100,
    rank_diff = ranking2 - ranking1,
    lastpt_p1_winner = if_else(point_no == 1, 0, lag(p1_winner)), # technically this 0 should be an NA
    lastpt_p2_winner = if_else(point_no == 1, 0, lag(p2_winner)), # technically this 0 should be an NA
    p1_serving_for_set = if_else(server == 1,
                                 if_else((p1_games == 5 & (p2_games < p1_games)) |
                                           (p1_games == 6 & (p2_games < p1_games)), 1, 0), 0),
    p2_serving_for_set = if_else(server == 0,
                                 if_else((p2_games == 5 & (p1_games < p2_games)) |
                                           (p2_games == 6 & (p1_games < p2_games)), 1, 0), 0),
    p1_serving_to_stay = if_else(server == 1,
                                 if_else((p2_games == 5 & (p1_games < p2_games)) |
                                           (p2_games == 6 & (p1_games < p2_games)), 1, 0), 0),
    p2_serving_to_stay = if_else(server == 0,
                                 if_else((p1_games == 5 & (p2_games < p1_games)) |
                                           (p1_games == 6 & (p2_games < p1_games)), 1, 0), 0)) %>%
  group_by(match_id) %>%
  mutate(
    p1_serves = cumsum(server),
    p2_serves = point_no - p1_serves,
    p1_serves_won = cumsum(point_victor*server),
    p2_serves_won = cumsum((point_victor - 1)*(server - 1)),
    p1_srv_win_perc = if_else(set_no == 1 & game_no < 7, .625, lag(p1_serves_won)/lag(p1_serves)) * 100,
    p2_srv_win_perc = if_else(set_no == 1 & game_no < 7, .625, lag(p2_serves_won)/lag(p2_serves)) * 100,
    p1_point = if_else(last_point_victor == 1, 1, 0),
    p2_point = if_else(last_point_victor == 0, 1, 0),
    p1_point_roll = roll_sum(p1_point, width = 25, min_obs = 1),
    p2_point_roll = roll_sum(p2_point, width = 25, min_obs = 1),
    roll_win_perc = if_else(point_no < 10, .5, p1_point_roll/(p1_point_roll + p2_point_roll)) * 100) %>%
  ungroup() %>%
  select(-contains("p1_point"), -contains("p2_point"))

large_glm <- glm(point_victor ~ server + ranking1 + ranking2 + pt_win_perc +
                   roll_win_perc + server:sex + status + set_diff + server:tiebreak +
                   p1_break_pt + p2_break_pt + p1_serving_for_set +
                   p2_serving_for_set + p1_serving_to_stay + p2_serving_to_stay +
                   p1_srv_win_perc:server + p2_srv_win_perc:server + factor(tournament):server,
                 family = binomial(link = "logit"), data = logistic_gs)

summary(large_glm)

gs %>% fit_plot("2022-wimbledon-1403", large_glm)

# Set by Set --------------------------------------------------------------

sets <- gs %>% filter(sex == 1, last_pt_set == 1) %>%
  mutate(
    set1 = if_else(set_no == 1, point_victor, 0),
    set2 = if_else(set_no == 2, point_victor, 0),
    set3 = if_else(set_no == 3, point_victor, 0),
    set4 = if_else(set_no == 4, point_victor, 0),
    set5 = if_else(set_no == 5, point_victor, 0)) %>%
  group_by(match_id, match_victor) %>%
  summarise(
    set1 = sum(set1),
    set2 = sum(set2),
    set3 = sum(set3),
    set4 = sum(set4),
    set5 = sum(set5)) %>%
  mutate(
      across(match_victor:set5, ~case_when(
        match_victor == 2 & .x == 1 ~ 2,
        match_victor == 2 & .x == 2 ~ 1,
        .default = .x))) %>%
  select(-match_victor)

# winning percentage if...
sets %>%
  group_by() %>%
  summarise(
    W = sum(set1 == 1)/sum(set1 %in% c(1,2)),
    L = sum(set1 == 2)/sum(set1 %in% c(1,2)),
    WW = sum(str_c(set1, set2) == 11)/sum(str_c(set1, set2) %in% c(22,11)),
    LL = sum(str_c(set1, set2) == 22)/sum(str_c(set1, set2) %in% c(22,11)),
    WL = sum(str_c(set1, set2) == 12)/sum(str_c(set1, set2) %in% c(12,21)),
    LW = sum(str_c(set1, set2) == 21)/sum(str_c(set1, set2) %in% c(12,21)),
    WWL = sum(str_c(set1, set2, set3) == 112)/sum(str_c(set1, set2, set3) %in% c(112,221)),
    LLW = sum(str_c(set1, set2, set3) == 221)/sum(str_c(set1, set2, set3) %in% c(112,221)),
    WLW = sum(str_c(set1, set2, set3) == 121)/sum(str_c(set1, set2, set3) %in% c(121,212)),
    LWL = sum(str_c(set1, set2, set3) == 212)/sum(str_c(set1, set2, set3) %in% c(121,212)),
    WLL = sum(str_c(set1, set2, set3) == 122)/sum(str_c(set1, set2, set3) %in% c(122,211)),
    LWW = sum(str_c(set1, set2, set3) == 211)/sum(str_c(set1, set2, set3) %in% c(122,211)),
    WWLL = sum(str_c(set1, set2, set3, set4) == 1122)/sum(str_c(set1, set2, set3, set4) %in% c(1122,2211)),
    LLWW = sum(str_c(set1, set2, set3, set4) == 2211)/sum(str_c(set1, set2, set3, set4) %in% c(1122,2211)),
    WLWL = sum(str_c(set1, set2, set3, set4) == 1212)/sum(str_c(set1, set2, set3, set4) %in% c(1212,2121)),
    LWLW = sum(str_c(set1, set2, set3, set4) == 2121)/sum(str_c(set1, set2, set3, set4) %in% c(1212,2121)),
    WLLW = sum(str_c(set1, set2, set3, set4) == 1221)/sum(str_c(set1, set2, set3, set4) %in% c(1221,2112)),
    LWWL = sum(str_c(set1, set2, set3, set4) == 2112)/sum(str_c(set1, set2, set3, set4) %in% c(1221,2112)))

# most popular paths...
sets %>% mutate(path = str_c(set1, set2, set3, set4, set5)) %>%
  distinct(path, .keep_all = TRUE) %>%
  group_by(path) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% view()


# Fatigue Over Sets -----------------------------------------------

five_sets <- gs %>% group_by(match_id) %>%
  mutate(
    total_sets = max(set_no)) %>%
  ungroup() %>%
  filter(total_sets == 5)

five_sets %>%
  group_by(set_no) %>%
  summarise(
    points = n()/(five_sets %>% distinct(match_id) %>% count()),
    winners = mean(p1_winner + p2_winner),
    ace = mean(p1_ace + p2_ace),
    unforced_errors = mean(p1_unf_err + p2_unf_err),
    double_fault = mean(p1_double_fault + p2_double_fault),
    server_win = mean(point_victor == server),
    serve_in = mean(serve_no == 1, na.rm = TRUE),
    distance_run = mean(p1_distance_run + p2_distance_run, na.rm = TRUE),
    serve_speed = mean(speed_mph, na.rm = TRUE),
    rally_count = mean(rally_count, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))


# Change Overs and reaction to break ------------------------------------------

gs %>% glimpse()

gs %>% filter(game_victor != 0) %>%
  group_by(match_id) %>%
  mutate(
    p1_prev_break = if_else(set_no == 1 & game_no == 1, 0, lag(p1_break_pt_won)),
    p2_prev_break = if_else(set_no == 1 & game_no == 1, 0, lag(p2_break_pt_won)),
    change_ends = if_else(game_no %in% c(2, 4, 6, 8, 10), 1, 0),
    set_change = if_else(game_no == 1 & set_no > 1, 1, 0),
    server_wins = if_else(server == game_victor, 1, 0),
    prev_break = p1_prev_break + p2_prev_break) %>%
  filter(tiebreak == 0, !is.na(prev_break)) %>%
  ungroup() %>%
  select(server_wins, prev_break, change_ends, set_change) %>%
  group_by(prev_break, change_ends, set_change) %>%
  summarise(hold_rate = mean(server_wins))


# Distance Run ------------------------------------------------------------

# distance run by serve
gs %>% filter(match_id == "2023-wimbledon-1701") %>%
  group_by(server) %>%
  summarise(p1_distance_run = round(mean(p1_distance_run, na.rm = TRUE), digits = 2),
            p2_distance_run = round(mean(p2_distance_run, na.rm = TRUE), digits = 2)) %>% view()

# distance run visualization comparing server and returner
gs %>% filter(str_detect(match_id, "2023-wimbledon-11"), server == 1, !is.na(p1_distance_run), !is.na(p2_distance_run)) %>%
  ggplot(aes(p1_distance_run, p2_distance_run)) +
  geom_point(aes(color = factor(point_victor)), alpha = .3) +
  labs(x = "Server", y = "Returner", title = "Distance Run")

# distance run by first and second serve
gs %>% filter(year == 2023) %>%
  group_by(match_id, set_no, game_no) %>%
  mutate(p1_distance_lag = lag(p1_distance_run),
         p2_distance_lag = lag(p2_distance_run)) %>%
  ungroup() %>%
  filter(!is.na(p1_distance_lag)) %>%
  group_by(server, serve_no) %>%
  summarise(
    p1_distance_lag = mean(p1_distance_lag),
    p2_distance_lag = mean(p2_distance_lag))

# Rain Delay --------------------------------------------------------------

gs %>%
  filter(delay == 1)


# Tiebreaks ---------------------------------------------------------------

# a 3% drop in winning serve once players enter a tiebreak...
gs %>% group_by(match_id, set_no) %>%
  mutate(has_tiebreak = if_else(sum(tiebreak)>0, 1, 0)) %>%
  ungroup() %>%
  filter(has_tiebreak == 1) %>%
  mutate(first_two_points = case_when(
    p1_score == "0" & p2_score == "0" & tiebreak == 0 ~ 1,
    p1_score == "15" & p2_score == "0" ~ 1,
    p1_score == "0" & p2_score == "15" ~ 1,
    .default = 0)) %>%
  filter(server == 1) %>%
  group_by(tiebreak, first_two_points) %>%
  summarise(
    p1_points_won = sum(point_victor == 1),
    p2_points_won = sum(point_victor == 2)) %>%
  mutate(
    p1_perc = p1_points_won/(p1_points_won + p2_points_won))

# after a tiebreak

gs %>% group_by(match_id, set_no) %>%
  mutate(has_tiebreak = if_else(sum(tiebreak)>0, 1, 0),
         has_tiebreak = replace_na(has_tiebreak, 0)) %>%
  ungroup() %>%
  group_by(match_id) %>%
  mutate(last_set_tiebreak = lag(tiebreak),
         last_set_tiebreak = replace_na(last_set_tiebreak, 0)) %>%
  ungroup() %>%
  filter(first_pt_set == 1) %>%
  distinct(match_id, set_no, .keep_all = TRUE) %>%
  select(match_id, set_no, has_tiebreak, last_set_tiebreak) %>%
  right_join(gs, by = join_by(match_id, set_no)) %>%
  filter(!is.na(has_tiebreak), server == 1) %>%
  group_by(has_tiebreak, last_set_tiebreak) %>%
  summarise(
    p1_points_won = sum(point_victor == 1),
    p2_points_won = sum(point_victor == 2)) %>%
  mutate(
    p1_perc = p1_points_won/(p1_points_won + p2_points_won))


# Visualizations ----------------------------------------------------------

# functions
cum_points <- function(df, match_code, type) {
  df <- df %>% filter(match_id == match_code) %>%
    mutate(
      diff = p1_points_won - p2_points_won)
  vlines <- df %>% filter(set_victor != 0) %>% dplyr::select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  p1 <- df$player1[1]
  p2 <- df$player2[1]
  tourney <- df$tournament[1]
  gender <- df$sex[1]
  
  if(type == "both"){
    p <- df %>% ggplot() +
      geom_point(aes(point_no, p1_points_won, color = 'p1')) +
      geom_point(aes(point_no, p2_points_won, color = 'p2')) +
      scale_color_manual(name = 'Player',
                         breaks = c('p1', 'p2'),
                         labels = c(p1, p2),
                         values = c('p1' = 'cadetblue4', 'p2' = 'indianred3')) +
      geom_vline(xintercept = vlines) +
      labs(y = "Points Won", x = "Point Number", title = str_c(p1, " vs. ", p2))
    print(p)
  }
  if(type == "difference"){
    p <- df %>% ggplot(aes(point_no, diff, color = "indianred3")) +
      geom_point() +
      geom_vline(xintercept = vlines) +
      labs(y = "Points Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
      theme(legend.position = "none")
    print(p)
  }
  
  if(type == "adjusted"){
    service <- df %>% filter(tournament == tourney, sex == gender) %>%
      summarise(
        s1_won = sum(point_victor == 1 & server == 1),
        s2_won = sum(point_victor == 2 & server == 2),
        total = n()) %>%
      mutate(wp = (s1_won + s2_won)/total) %>%
      dplyr::select(wp) %>% pull()
    
    df <- df %>%
      mutate(
        serve_temp = case_when(
          server == 1 & point_victor == 1 ~ 2*(1 - service),
          server == 2 & point_victor == 1 ~ 2*service,
          server == 1 & point_victor == 2 ~ -2*service,
          server == 2 & point_victor == 2 ~ -2*(1 - service)
        ),
        cum_adj_serve = cumsum(serve_temp))
    
    p <- df %>% ggplot(aes(point_no, cum_adj_serve, color = "indianred3")) +
      geom_point() +
      geom_vline(xintercept = vlines) +
      labs(y = "Points Margin (Adjusted for Serve)", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
      theme(legend.position = "none")
    print(p)
  }
}

points_vector <- function(df, match_code, type){
  
  tourney <- df %>% filter(match_id == match_code) %>% dplyr::select(tournament) %>% slice(1) %>% pull()
  gender <- df %>% filter(match_id == match_code) %>% dplyr::select(sex) %>% slice(1) %>% pull()
  
  service <- df %>% filter(tournament == tourney, sex == gender) %>%
    summarise(
      s1_won = sum(point_victor == 1 & server == 1),
      s2_won = sum(point_victor == 2 & server == 2),
      total = n()) %>%
    mutate(wp = (s1_won + s2_won)/total) %>%
    dplyr::select(wp) %>% pull()
  
  df <- df %>% filter(match_id == match_code) %>% 
    mutate(
      adj_p1 = case_when(server == 1 & point_victor == 1 ~ service,
                         server == 2 & point_victor == 1 ~ 2 - service,
                         .default = 0),
      adj_p2 = case_when(server == 2 & point_victor == 2 ~ service,
                         server == 1 & point_victor == 2 ~ 2 - service,
                         .default = 0),
      adj_p1_sum = cumsum(adj_p1),
      adj_p2_sum = cumsum(adj_p2),
      adj_points_margin = adj_p1_sum - adj_p2_sum,
      points_margin = p1_points_won - p2_points_won)
  
  if(type == "adjusted"){
    points <- df %>% dplyr::select(adj_points_margin) %>% as_vector() %>% as.double()
    points
  }
  else{
    if(type == "server"){
      tib <- df %>% dplyr::select(point_no, points_margin, server)
      tib
    }
    else{
      points <- df %>% dplyr::select(points_margin) %>% as_vector() %>% as.double()
      points
    }
  }
}

cum_points(gs, match_code, "adjusted") +
  geom_smooth(span = .1, color = "cadetblue3", se = F)

points <- gs %>% points_vector(match_code, "normal")
