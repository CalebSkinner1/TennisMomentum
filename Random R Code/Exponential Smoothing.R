# Exponential Smoothing
# Author: Caleb Skinner

library("tidyverse")
library("fpp3")
library("recipes")
library("patchwork")
library("arrow")
library("here")
library("lubridate")
library("fable")

gs <- read_parquet(here("grandslams2.parquet"))

# match codes
match_code <- "2011-ausopen-1101" # Nadal-Daniel
match_code <- "2018-wimbledon-1601" # Isner-Anderson
match_code <- "2018-wimbledon-1602" # Djokovic-Nadal
match_code <- "2017-ausopen-1701" # Federer-Nadal
match_code <- "2018-usopen-1501" # Thiem-Nadal
match_code <- "2023-wimbledon-1701" # Djokovic-Alcaraz

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

# create match

match0 <- gs %>% filter(match_id == match_code)

service <- match0 %>%
  summarise(
    exp_server = sum(point_victor == server)/n() - sum(point_victor != server)/n()) %>% pull()

# match0 %>% summarise(
#     exp_server = sum(point_victor == server)/n(),
#     exp_returner = sum(point_victor != server)/n())

match <- match0 %>%
  mutate(
    point_margin = p1_points_won - p2_points_won,
    serve_adj = if_else(point_victor == 1, 1, -1) + if_else(server == 1, -service, service),
    adj_point_margin = cumsum(serve_adj)) %>%
  as_tsibble(index = point_no)

match %>% 
  mutate(point_victor = if_else(point_victor == 1, 1, -1)) %>%
  group_by(server) %>%
  summarise(s = sum(serve_adj),
            p = sum(point_victor))

ses <- match %>%
  model(
    model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = .1) + season("N")))

# components(ses) %>%
#   left_join(fitted(ses), by = c(".model", "point_no")) %>%
#   mutate(adj_point_margin = replace_na(adj_point_margin, 0))

match_flip <- match %>%
  as_tibble() %>%
  mutate(flip = n() - point_no + 1) %>%
  arrange(flip) %>%
  as_tsibble(index = flip)

ses_flip <- match_flip %>%
  model(
    model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = .1) + season("N")))

components(ses_flip) %>%
  left_join(fitted(ses_flip), by = c(".model", "flip")) %>%
  mutate(adj_point_margin = replace_na(adj_point_margin, 0),
         point_no = n() - flip) %>%
  slice(-1) %>%
  ggplot() +
  geom_line(aes(point_no, adj_point_margin), color = "cadetblue4") +
  geom_line(aes(point_no, level), color = "indianred3") +
  # geom_vline(xintercept = vlines) +
  labs(x = "", y = "Point Margin", title = str_c(match$player1[1], " vs. ", match$player2[1]))

# Simple Exponential Smoothing --------------------------------------------

# function to visualize the smoothing
ses_visualize <- function(match, alpha, order){
  ses <- match %>%
    model(
      ANN = ETS(adj_point_margin ~ error("A") + trend("N", alpha = alpha) + season("N")))
  
  vlines <- match %>% filter(set_victor != 0) %>% dplyr::select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  p <- components(ses) %>%
    left_join(fitted(ses), by = c(".model", "point_no")) %>%
    mutate(adj_point_margin = replace_na(adj_point_margin, 0)) %>%
    slice(-1) %>%
    ggplot() +
    geom_line(aes(point_no, adj_point_margin), color = "cadetblue4") +
    geom_line(aes(point_no, level), color = "indianred3") +
    geom_vline(xintercept = vlines) +
    labs(x = "", y = "Point Margin", title = str_c(match$player1[1], " vs. ", match$player2[1]))
  
  smoothed <- components(ses) %>% slice(-1) %>%
    dplyr::select(point_no, adj_point_margin, level) %>%
    # add_row(point_no = 0, point_margin = 0, level = 0) %>%
    arrange(point_no) %>%
    mutate(
      gradient_o1 = grad_update(level, order = 1),
      gradient_o2 = grad_update(level, order = 2),
      gradient_o3 = grad_update(level, order = 3),
      point_victor = match$point_victor) %>%
    relocate(point_victor, .before = adj_point_margin)
  
  g1 <- ggplot(smoothed) +
    geom_line(aes(point_no, gradient_o1), color = "forestgreen") +
    labs(x = "points", y = "derivative", title = "first order") +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0)
  
  g2 <- ggplot(smoothed) +
    geom_line(aes(point_no, gradient_o2), color = "limegreen") +
    labs(x = "points", y = "", title = "second order") +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0)
  
  if(order == "both"){
    return(p / (g1 + g2))
  }
  if(order == 1){
    return(p / (g1 + labs(title = "")))
  }
  if(order == 2){
    return(p / (g2 + labs(title = "", y = "derivative")))
  }
}

smoothed %>% as_tibble() %>% 
  filter(!is.na(gradient_o1)) %>%
  group_by() %>%
  summarise(
    var_g1 = var(gradient_o1),
    var_g2 = var(gradient_o2))

service <- match0 %>% filter(tournament == "Wimbledon", sex == "Male") %>%
  summarise(
    s1_won = sum(point_victor == 1 & server == 1),
    s2_won = sum(point_victor == 2 & server == 2),
    total = n()) %>%
  mutate(wp = (s1_won + s2_won)/total) %>%
  dplyr::select(wp) %>% pull()

# examples
# both
ses_visualize(match, .1, "both")

# first order
ses_visualize(match, .1, 1)

# second order
ses_visualize(match, .2, 2)

derivative_tibble <- function(df, alpha, order){
  
  tp <- df %>%
    mutate(point_margin = p1_points_won - p2_points_won) %>%
    select(match_id, point_no, point_margin, server, point_victor)
  
  matches <- tp %>% select(match_id) %>% distinct() %>% as_vector()
  
  mast_deriv <- tibble()
  
  for(i in 1:length(matches)){
    # filtering for each match
    this_match <- tp %>% filter(match_id == matches[i])
    
    # creating service adjustment manually
    service <- this_match %>%
      summarise(
        exp_server = sum(point_victor == server)/n() - sum(point_victor != server)/n()) %>% pull()
    
    # smoothing from backward
    ses <- this_match %>%
      mutate(
        # point_margin = p1_points_won - p2_points_won,
        serve_adj = if_else(point_victor == 1, 1, -1) + if_else(server == 1, -service, service),
        adj_point_margin = cumsum(serve_adj)) %>%
      as_tsibble(index = point_no) %>%
      model(
        model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = alpha) + season("N")))
    
    # calculating spline from backward smooth
    temp <- components(ses) %>% select(level)
    f <- splinefun(temp$point_no, temp$level)
    splinecoef <- get("z", envir = environment(f))
    
    # backward gradient
    deriv_backward <- components(ses) %>%
      slice(-1) %>%
      select(point_no, adj_point_margin, level) %>%
      # add_row(point_no = 0, adj_point_margin = 0, level = 0) %>%
      arrange(point_no) %>%
      mutate(
        gradient_backward = grad_update(level, order = order),
        spline_backward = (splinecoef$b[-1] %>% as_vector())) %>%
      rename(level_backward = "level") %>%
      as_tibble() %>%
      mutate(match_id = matches[i])
    
    # smoothing from forward
    ses_flip <- this_match %>%
      mutate(
        # point_margin = p1_points_won - p2_points_won,
        serve_adj = if_else(point_victor == 1, 1, -1) + if_else(server == 1, -service, service),
        adj_point_margin = cumsum(serve_adj),
        flip = n() - point_no + 1) %>%
      arrange(flip) %>%
      as_tsibble(index = flip) %>%
      model(
        model = ETS(adj_point_margin ~ error("A") + trend("N", alpha = alpha) + season("N")))
    
    # calculating spline from forward smooth
    temp_flip <- components(ses_flip) %>% select(level)
    f_flip <- splinefun(temp$point_no, temp$level)
    splinecoef_flip <- get("z", envir = environment(f_flip))
    
    # forward gradient
    deriv_forward <- components(ses_flip) %>%
      slice(-1) %>%
      select(flip, adj_point_margin, level) %>%
      # add_row(point_no = 0, adj_point_margin = 0, level = 0) %>%
      arrange(flip) %>%
      mutate(
        gradient_forward = -grad_update(level, order = order),
        spline_forward = -(splinecoef_flip$b[-1] %>% as_vector())) %>%
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

wimbledon_derivatives <- gs %>% filter(str_detect(match_id, "2023-wimbledon-16")) %>% 
  derivative_tibble(alpha = .1, order = 1)

wimbledon_derivatives %>%
  filter(match_id == "2023-wimbledon-1601") %>%
  ggplot() +
  geom_line(aes(x = point_no, y = level_backward), color = "indianred3") +
  geom_line(aes(x = point_no, y = level_forward), color = "cadetblue4") +
  geom_point(aes(x = point_no, y = adj_point_margin), size = .5)

wimbledon_derivatives %>%
  filter(match_id == "2023-wimbledon-1601") %>%
  ggplot() +
  geom_line(aes(x = point_no, y = spline_backward), color = "indianred3") +
  geom_line(aes(x = point_no, y = spline_forward), color = "cadetblue4")

full_derivatives <- gs %>% #filter(year == 2023) %>%
  derivative_tibble(alpha = .10, order = 1)

together <- gs %>%
  left_join(full_derivatives, by = join_by(match_id, point_no)) %>%
  relocate(adj_point_margin, .after = p2_points_won) %>%
  relocate(level_backward, .after = adj_point_margin) %>%
  relocate(level_forward, .after = level_backward) %>%
  relocate(gradient_backward, .after = level_forward) %>%
  relocate(spline_backward, .after = gradient_backward) %>%
  relocate(gradient_forward, .after = spline_backward) %>%
  relocate(spline_forward, .after = gradient_forward) %>%
  select(-missing_data)

# write_parquet(together, here("grandslams.parquet"))

# report(ses)
# 
# components(ses) %>% autoplot()
# 
# components(ses) %>%
#   left_join(fitted(ses), by = c(".model", "point_no"))

# ses %>% forecast(h = 10) %>%
#   autoplot(match) +
#   labs(y = "point_margin", x = "point_no", title = "Alcaraz vs. Djokovic")

# Exponential Smoothing with trend ----------------------------------------

holt <- match %>%
  model(
    AAN = ETS(point_margin ~ error("A") + trend("A") + season("N")))

report(holt)

components(holt) %>% autoplot()

components(holt) %>%
  left_join(fitted(holt), by = c(".model", "point_no"))

holt %>% forecast(h = 10) %>%
  autoplot(match) +
  labs(y = "point_margin", x = "point_no", title = "Alcaraz vs. Djokovic")

# Dampened Trend ----------------------------------------------------------

dampen <- match %>%
  model(
    AAdN = ETS(point_margin ~ error("A") + trend("Ad") + season("N")))

report(dampen)

components(dampen) %>% autoplot()

components(dampen) %>%
  left_join(fitted(dampen), by = c(".model", "point_no"))

dampen %>% forecast(h = 10) %>%
  autoplot(match) +
  labs(y = "point_margin", x = "point_no", title = "Alcaraz vs. Djokovic")


# Altogether --------------------------------------------------------------

all <- match %>%
  model(
    ses = ETS(point_margin ~ error("A") + trend("N") + season("N")),
    holt = ETS(point_margin ~ error("A") + trend("A") + season("N")),
    damped = ETS(point_margin ~ error("A") + trend("Ad") + season("N"))
    )

tidy(all)
accuracy(all)


# Seasonal ----------------------------------------------------------------

# match_season <- match %>%
#   filter(Purpose == "server")

?ETS

season <- match %>%
  model(
    additive = ETS(point_margin ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(point_margin ~ error("M") + trend("A") + season("M")))
