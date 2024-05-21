# Visualizing Derivatives

# Caleb
# 12/17/23

library("tidyverse")
library("patchwork")

gs <- read_parquet(here("grandslams.parquet"))

gs %>% glimpse()

# comparing the forward and backward levels
gs %>% filter(year == 2023) %>%
  ggplot() +
  geom_point(aes(x = level_backward, y = level_forward), alpha = .01)

# gradient backward
gs %>% filter(year == 2023) %>%
  ggplot() +
  geom_point(aes(x = lag(gradient_backward), y = gradient_backward, color = factor(point_victor)))

# gradient forward
gs %>% filter(year == 2023) %>%
  ggplot() +
  geom_point(aes(x = lag(gradient_forward), y = gradient_forward, color = factor(lag(point_victor))))

# gradient backward on gradient forward
gs %>% filter(year == 2023) %>%
  ggplot() +
  geom_point(aes(x = gradient_backward, y = gradient_forward), alpha = .1)

# adjusted point margin
gs %>% filter(str_detect(match_id, "2023-wimbledon-1701")) %>%
  ggplot() +
  geom_line(aes(x = point_no, y = adj_point_margin))

gs %>% filter(str_detect(match_id, "2023-wimbledon-1701")) %>%
  match_visualize("forward", "both")

gs %>% filter(str_detect(match_id, "2023-wimbledon-1701")) %>%
  select(point_no, point_victor, contains("gradient"), contains("level"), contains("spline"))

match_visualize <- function(match, method, type){
  
  # vertical lines
  vlines <- match %>% filter(set_victor != 0) %>% select(point_no) %>% slice(1:(n()-1)) %>% pull()
  
  # plot backward
  pb <- match %>%
    ggplot() +
    geom_line(aes(point_no, adj_point_margin), color = "cadetblue4") +
    geom_line(aes(point_no, level_backward), color = "indianred3") +
    geom_vline(xintercept = vlines) +
    labs(x = "", y = "Point Margin", title = str_c(match$player1[1], " vs. ", match$player2[1]))
  
  # plot forward
  pf <- match %>%
    ggplot() +
    geom_line(aes(point_no, adj_point_margin), color = "cadetblue4") +
    geom_line(aes(point_no, level_forward), color = "indianred3") +
    geom_vline(xintercept = vlines) +
    labs(x = "", y = "Point Margin", title = str_c(match$player1[1], " vs. ", match$player2[1]))
  
  # backward gradient
  gb <- match %>% ggplot() +
    geom_line(aes(point_no, gradient_backward), color = "forestgreen") +
    labs(x = "points", y = "derivative", title = "backward gradient") +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0)
  
  sb <- match %>% ggplot() +
    geom_line(aes(point_no, spline_backward), color = "limegreen") +
    labs(x = "points", y = "derivative", title = "backward spline") +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0)
  
  # forward gradient
  gf <- match %>% ggplot() +
    geom_line(aes(point_no, gradient_forward), color = "forestgreen") +
    labs(x = "points", y = "", title = "forward gradient") +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0)
  
  sf <- match %>% ggplot() +
    geom_line(aes(point_no, spline_forward), color = "limegreen") +
    labs(x = "points", y = "derivative", title = "forward spline") +
    geom_vline(xintercept = vlines) +
    geom_hline(yintercept = 0)
  
  if(method == "backward"){
    if(type == "both"){
      return(pb / (gb + sb))
    }
    if(type == "gradient"){
      return(pb / gb)
    }
    else{
      return(pb / sb)
    }
  }
  if(method == "forward"){
    if(type == "both"){
      return(pf / (gf + sf))
    }
    if(type == "gradient"){
      return(pf / gf)
    }
    else{
      return(pf / sf)
    }
  }
}
