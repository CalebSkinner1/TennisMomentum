# Outline of Journal

library("tidyverse")
library("arrow")
library("flextable")
library("here")
library("tidymodels")
library("janitor")

gs <- read_parquet(here("Data/grandslams3.parquet"))

set_flextable_defaults(
  font.size = 10, theme_fun = theme_apa,
  padding = 6,
  background.color = "#EFEFEF",
  text.align = "center")

lr_mod <- logistic_reg() %>% 
  set_engine("glm")


# Literature Review -------------------------------------------------------

# - research that finds momentum does not exist

# - 3 momentum frameworks
# - momentum findings in sports (easier to isolate in simpler matches)
# - tennis's repetition is suited for study
# - some important findings in tennis


# Modeling Trend in a Match -----------------------------------------------
# first model, plot residuals
# smooth
# compute backward derivative

# Results -----------------------------------------------------------------

gs2 <- gs %>%
  mutate(
    lag_gradient_backward_a10 = lag(gradient_backward_a10),
    lag_gradient_backward_a15 = lag(gradient_backward_a15),
    lag_gradient_backward_a20 = lag(gradient_backward_a20)) %>%
  select(-gradient_backward_a10, -gradient_backward_a15, -gradient_backward_a20) %>%
  filter(!is.na(lag_gradient_backward_a10), point_no > 5)

# exist_momentum_men <- lr_mod %>%
#   fit(point_victor ~ tournament:server:sex + lag_gradient_backward_a10:sex + lag_gradient_backward_a10 + b365_1_prob*sex, data = gs2)
# 
# exist_momentum_women <- lr_mod %>%
#   fit(point_victor ~ tournament:server:sex + lag_gradient_backward_a10:sex + lag_gradient_backward_a10 + b365_1_prob*sex, data = gs2)
# 
# exist_momentum_combined <- lr_mod %>%
#   fit(point_victor ~ tournament:server + lag_gradient_backward_a10 + b365_1_prob, data = gs2)

exist_momentum_men <- lr_mod %>%
  fit(point_victor ~ tournament:server:sex + lag_gradient_backward_a10:sex + lag_gradient_backward_a10 + b365_1_prob, data = gs2)

gs3 <- gs2 %>% mutate(sex = factor(if_else(sex == "1", 0, 1)))

exist_momentum_women <- lr_mod %>%
  fit(point_victor ~ tournament:server:sex + lag_gradient_backward_a10:sex + lag_gradient_backward_a10 + b365_1_prob, data = gs3)

exist_momentum_combined <- lr_mod %>%
  fit(point_victor ~ tournament:server + lag_gradient_backward_a10 + b365_1_prob, data = gs2)

prep_table <- function(fit, name, multiplier){
  fit %>%
    tidy() %>%
    clean_names() %>%
    select(-statistic) %>%
    filter(!str_detect(term, "tournament")) %>%
    mutate(
      `2.5%` = exp(estimate * multiplier - 1.96*std_error*multiplier),
      odds_ratio = exp(estimate * multiplier),
      `97.5%` = exp(estimate * multiplier + 1.96*std_error*multiplier),
      across(where(is.numeric), ~round(.x, digits = 4))) %>%
    slice(1:3) %>%
    mutate(term = c("Intercept", "backward trend", "bet_odds")) %>%
    select(-estimate, -std_error, -p_value) %>%
    rename_with(~paste0(name, "_", .x), .cols = everything())
}

prep_log_table <- function(men_fit, women_fit, combined_fit, multiplier){
  em_men <- men_fit %>%
    prep_table("men", multiplier) %>%
    select(-contains("term"))
  
  em_women <- women_fit %>%
    prep_table("women", multiplier) %>%
    select(-contains("term"))
  
  combined_fit %>%
    prep_table("combined", multiplier) %>%
    bind_cols(em_men) %>%
    bind_cols(em_women) %>%
    rename("term" = "combined_term") %>%
    mutate(
      term = str_replace_all(term, "_", " "),
      term = str_to_title(term),
      term = recode(term,
                    "Bet Odds" = "Pre-Match Win Probability")) %>%
    rename_with(str_to_title)
}

prep_log_table(exist_momentum_men, exist_momentum_women, exist_momentum_combined, 1) %>%
  flextable() %>%
  add_header_row(
    values = c("", "Combined", "Men", "Women"),
    colwidths = c(1, 3, 3, 3), top = TRUE) %>%
  set_header_labels(values = c("Term", "2.5%", "Odds Ratio", "97.5%", "2.5%", "Odds Ratio", "97.5%", "2.5%", "Odds Ratio", "97.5%")) %>%
  align(align = "center", part = "all")

# logistic regression
# men and women

# Discussion --------------------------------------------------------------
# talk about results



