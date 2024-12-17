# Old Derivative Functions and Smoothing Functions

library("fda")
library("ggpubr")

gs <- read_csv("Data Creation/grandslams.csv")

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

# smoothing functions
smooth <- function(df, match_code, type, order = 6, l = 1){
  # creating match df
  temp <- df %>% points_vector(match_code, type) %>%
    as_tibble() %>%
    mutate(point_no = row_number()) %>%
    rename(margin = value)
  
  # filtering out data to creat match df with details
  df0 <- df %>% filter(match_id == match_code)
  
  # creating details for ggplot
  vlines <- df0 %>% filter(set_victor != 0) %>% dplyr::select(point_no) %>% slice(1:(n()-1)) %>% pull()
  p1 <- df0$player1[1]
  p2 <- df0$player2[1]
  tourney <- df0$tournament[1]
  derivative <- order - 2
  
  #(1) Creating the saturated basis functions.
  gr_basis <- create.bspline.basis(norder = order, breaks = temp$point_no)
  
  #(2) Defining the roughness penalty.
  rougness_penalty <- fdPar(gr_basis, Lfdobj = derivative, lambda = l)
  
  #(3) Smoothing the data.
  smoothed <- smooth.basis(temp$point_no, temp$margin, rougness_penalty)
  
  # creating important objects from smoothed list
  smooth_size <- smoothed[["fd"]][["basis"]][["nbasis"]]
  match_length <- count(temp) %>% pull()
  complete_margin <- append(temp$margin, rep(NA, smooth_size - match_length), after = match_length)
  
  # creating tibble
  sm_t <- tibble(point_no = c(1:smooth_size),
                 smooth = smoothed$fd$coefs,
                 point_margin = complete_margin)
  
  # plotting data
  ggplot(sm_t) +
    geom_line(aes(point_no, smooth), color = "cadetblue3") +
    geom_point(aes(point_no, point_margin), color = "indianred3", size = .8) +
    geom_vline(xintercept = vlines) +
    labs(y = "Points Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
    theme(legend.position = "none")
}

# example
gs %>% smooth(match_code, "normal", order = 6, l = 3)

# derivative functions

derivative_basis <- function(df, match_code, type = "normal", order = 6, l = 1, derivative = 1, print = "both"){
  
  # creating match df "temp"
  temp <- df %>% points_vector(match_code, type) %>%
    as_tibble() %>%
    mutate(point_no = row_number()) %>%
    rename(margin = value)
  
  # filtering out data to creat match df with details
  df0 <- gs %>% filter(match_id == match_code)
  
  # creating details for ggplot
  vlines <- df0 %>% filter(set_victor != 0) %>% dplyr::select(point_no) %>% slice(1:(n()-1)) %>% pull()
  p1 <- df0$player1[1]
  p2 <- df0$player2[1]
  tourney <- df0$tournament[1]
  
  #(1) Creating the saturated basis functions.
  gr_basis <- create.bspline.basis(norder = order, breaks = temp$point_no)
  
  #(2) Defining the roughness penalty.
  rougness_penalty <- fdPar(gr_basis, Lfdobj = derivative, lambda = l)
  
  #(3) Smoothing the data.
  smoothed <- smooth.basis(temp$point_no, temp$margin, rougness_penalty)
  
  # creating important objects from smoothed list
  match_length <- count(temp) %>% pull()
  
  # Creates a fine mesh between max and min points
  point_mesh <- seq(1, match_length, length=1000)
  
  # creates nth derivative tibble
  f_deriv <- predict(smoothed$fd, point_mesh, Lfdobj = 1) %>%
    as_tibble() %>%
    mutate(point = point_mesh)
  
  # graphs nth derivative
  plot1 <- ggplot(f_deriv) +
    geom_line(aes(point, V1), color = "indianred3") +
    geom_vline(xintercept = vlines) +
    labs(y = "Derivative of Points Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 0)
  
  # creating important objects from smoothed list
  smooth_size <- smoothed[["fd"]][["basis"]][["nbasis"]]
  complete_margin <- append(temp$margin, rep(NA, smooth_size - match_length), after = match_length)
  
  # creating tibble
  sm_t <- tibble(point_no = c(1:smooth_size),
                 smooth = smoothed$fd$coefs,
                 point_margin = complete_margin)
  
  # plotting data
  plot2 <- ggplot(sm_t) +
    geom_line(aes(point_no, smooth), color = "indianred3") +
    geom_point(aes(point_no, point_margin), color = "cadetblue4", size = .4) +
    geom_vline(xintercept = vlines) +
    labs(y = "Points Margin", x = "Point Number", title = str_c(p1, " (+) vs. ", p2, " (-)")) +
    theme(legend.position = "none")
  
  figure <- ggarrange(plot2 + labs(x = ""),
                      plot1 + labs(title = ""),
                      nrow = 2)
  
  if(print == "both"){
    print(figure)
  }
  else{
    if(print == "derivative"){
      print(plot1)
    }
    else{
      print(f_deriv)
    }
  }
}

# example
derivative_basis(gs, match_code = match_code, l = 20, print = "both")

derivative_loess <- function(df, match_code, type){
  df <- df %>% filter(match_id == match_code)
  
  vlines <- df %>%
    filter(set_victor != 0) %>%
    dplyr::select(point_no) %>%
    slice(1:(n()-1)) %>% pull()
  
  p1 <- df$player1[1]
  p2 <- df$player2[1]
  
  points <- points_vector(df, match_code, type) %>%
    as_tibble() %>%
    mutate(point_no = row_number()) %>%
    rename(margin = value)
  
  lv <- lowess(points$point_no, points$margin, f = .1) %>%
    as_tibble() %>%
    rename(point_no = x,
           margin = y) %>%
    mutate(
      margin_delay = lag(margin, 1),
      dydx = (margin - margin_delay)) %>%
    dplyr::select(-margin_delay, -margin) %>%
    left_join(points, join_by(point_no))
  
  plot1 <- lv %>%
    ggplot(aes(point_no, margin, color = "indianred3")) +
    geom_point() +
    geom_smooth(span = .10, color = "cadetblue4", se = F) +
    geom_vline(xintercept = vlines) +
    labs(y = "Points Margin (Adjusted for Serve)", x = "",
         title = str_c(p1, " (+) vs. ", p2, " (-)")) +
    theme(legend.position = "none")
  
  plot2 <- lv %>%
    ggplot(aes(point_no, dydx)) +
    geom_point(color = "cadetblue4") +
    geom_smooth(span = .10, color = "indianred3", se = F) +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0) +
    labs(y = "dydx", x = "",
         title = "")
  
  figure <- ggarrange(plot1, plot2,
                      nrow = 2)
  print(figure)
}

derivative_loess(gs, match_code, "difference")

derivative_old <- function(df, match_code, type, n = 20){
  df <- df %>% filter(match_id == match_code)
  
  vlines <- df %>%
    filter(set_victor != 0) %>%
    dplyr::select(point_no) %>%
    slice(1:(n()-1)) %>% pull()
  
  p1 <- df$player1[1]
  p2 <- df$player2[1]
  points <- points_vector(df, match_code, "adjusted")
  
  df <- points %>% as_tibble() %>%
    rename(margin = value) %>%
    mutate(
      point_no = row_number(),
      margin_delay = lag(margin, n = n),
      dydx = (margin - margin_delay)/n,
      dydx_delay = lag(dydx, n = n),
      dy2dx2 = (dydx - dydx_delay)/n)
  
  plot1 <- df %>%
    ggplot(aes(point_no, margin, color = "indianred3")) +
    geom_point() +
    geom_smooth(span = .2, color = "cadetblue4", se = F) +
    geom_vline(xintercept = vlines) +
    labs(y = "Points Margin (Adjusted for Serve)", x = "",
         title = str_c(p1, " (+) vs. ", p2, " (-)")) +
    theme(legend.position = "none")
  
  plot2 <- df %>%
    ggplot(aes(point_no, dydx)) +
    geom_point(color = "cadetblue4") +
    geom_smooth(span = .20, color = "indianred3", se = F) +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0)
  labs(y = "dydx", x = "",
       title = "")
  
  # plot3 <- df %>%
  #   ggplot(aes(point_no, dy2dx2)) +
  #   geom_point(color = "indianred3") +
  #   geom_smooth(span = .1, color = "cadetblue4", se = F) +
  #   geom_vline(xintercept = vlines) +
  #   geom_hline(yintercept = 0)
  # labs(y = "dy2dx2", x = "Point Number",
  #      title = "")
  
  figure <- ggarrange(plot1, plot2,
                      nrow = 2)
  print(figure)
}