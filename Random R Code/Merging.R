# Merging all the datasets
# Author: Caleb Skinner
# Date: October 23

# loading libraries
library("arrow")
library("tidyverse"); theme_set(theme_bw())
library("roll")
library("janitor")
library("here")
`%!in%` <- Negate(`%in%`)

setwd("Data Creation")

# reading in observations by match for player data
u2023_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2023-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2023_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2023-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)

u2022_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2022-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2022_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2022-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)

u2021_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2021-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2021_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2021-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)

u2020_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2020-usopen-matches.csv") %>%
  select(match_id, player1, player2)

u2019_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2019-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2019_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2019-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)

u2018_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2018-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2018_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2018-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)

u2017_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2017-usopen-matches.csv") %>%
  select(match_id, player1, player2) %>%
  mutate(
    player1 = case_when(match_id == "2017-usopen-2218" ~ "Carla Suarez Navarro",
                        match_id == "2017-usopen-2405" ~ "Carla Suarez Navarro",
                        .default = player1),
    player2 = case_when(match_id == "2017-usopen-2148" ~ "Garbine Muguruza",
                        match_id == "2017-usopen-2221" ~ "Alize Cornet",
                        match_id == "2017-usopen-2224" ~ "Garbine Muguruza",
                        match_id == "2017-usopen-2309" ~ "Carla Suarez Navarro",
                        match_id == "2017-usopen-2312" ~ "Garbine Muguruza",
                        match_id == "2017-usopen-2406" ~ "Garbine Muguruza",
                        .default = player2))
w2017_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2017-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)
f2017_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2017-frenchopen-matches.csv") %>%
  select(match_id, player1, player2) %>%
  mutate(
    player1 = case_when(match_id == "2017-frenchopen-2117" ~ "Garbine Muguruza",
                        match_id == "2017-frenchopen-2135" ~ "Alize Lim",
                        match_id == "2017-frenchopen-2154" ~ "Alize Cornet",
                        match_id == "2017-frenchopen-2209" ~ "Garbine Muguruza",
                        match_id == "2017-frenchopen-2305" ~ "Garbine Muguruza",
                        match_id == "2017-frenchopen-2314" ~ "Alize Cornet",
                        match_id == "2017-frenchopen-2403" ~ "Garbine Muguruza",
                        .default = player1),
    player2 = case_when(match_id == "2017-frenchopen-2227" ~ "Alize Cornet",
                        match_id == "2017-frenchopen-2407" ~ "Alize Cornet",
                        .default = player2))
a2017_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2017-ausopen-matches.csv") %>%
  select(match_id, player1, player2) %>%
  mutate(
    player1 = case_when(match_id == "2017-ausopen-2109" ~ "Carla Suarez Navarro",
                        match_id == "2017-ausopen-2132" ~ "Mariana Duque Marino",
                        match_id == "2017-ausopen-2145" ~ "Alize Cornet",
                        match_id == "2017-ausopen-2205" ~ "Carla Suarez Navarro",
                        match_id == "2017-ausopen-2223" ~ "Alize Cornet",
                        .default = player1),
    player2 = case_when(match_id == "2017-ausopen-2116" ~ "Garbine Muguruza",
                        match_id == "2017-ausopen-2208" ~ "Garbine Muguruza",
                        match_id == "2017-ausopen-2304" ~ "Garbine Muguruza",
                        match_id == "2017-ausopen-2402" ~ "Garbine Muguruza",
                        match_id == "2017-ausopen-2501" ~ "Garbine Muguruza",
                        .default = player2))

u2016_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2016-usopen-matches.csv") %>%
  select(match_id, player1, player2) %>%
  mutate(
    player1 = case_when(match_id == "2016-usopen-2163" ~ "Alize Cornet",
                        match_id == "2016-usopen-2303" ~ "Carla Suarez Navarro",
                        match_id == "2016-usopen-2402" ~ "Carla Suarez Navarro",
                        .default = player1),
    player2 = case_when(match_id == "2016-usopen-2148" ~ "Garbine Muguruza",
                        match_id == "2016-usopen-2224" ~ "Garbine Muguruza",
                        .default = player2))
w2016_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2016-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)
f2016_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2016-frenchopen-matches.csv") %>%
  select(match_id, player1, player2)
a2016_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2016-ausopen-matches.csv") %>%
  select(match_id, player1, player2)

u2015_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2015-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2015_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2015-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)
f2015_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2015-frenchopen-matches.csv") %>%
  select(match_id, player1, player2)
a2015_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2015-ausopen-matches.csv") %>%
  select(match_id, player1, player2)

u2014_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2014-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2014_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2014-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)
f2014_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2014-frenchopen-matches.csv") %>%
  select(match_id, player1, player2)
a2014_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2014-ausopen-matches.csv") %>%
  select(match_id, player1, player2)

u2013_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2013-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2013_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2013-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)
f2013_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2013-frenchopen-matches.csv") %>%
  select(match_id, player1, player2)
a2013_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2013-ausopen-matches.csv") %>%
  select(match_id, player1, player2)

u2012_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2012-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2012_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2012-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)
f2012_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2012-frenchopen-matches.csv") %>%
  select(match_id, player1, player2)
a2012_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2012-ausopen-matches.csv") %>%
  select(match_id, player1, player2)

u2011_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2011-usopen-matches.csv") %>%
  select(match_id, player1, player2)
w2011_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2011-wimbledon-matches.csv") %>%
  select(match_id, player1, player2)
f2011_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2011-frenchopen-matches.csv") %>%
  select(match_id, player1, player2)
a2011_matches_0 <- read_csv("tennis_slam_pointbypoint-master/2011-ausopen-matches.csv") %>%
  select(match_id, player1, player2)

# reading in rankings
grand_slams <- c("Wimbledon", "Roland Garros", "Australian Open", "Us Open", "US Open")

recoding_names <- function(df){
  df %>% mutate(player = recode(player,
                                "Christopher Oconnell" = "Christopher O'Connell",
                                "J J Wolf" = "J.J. Wolf",
                                "Albert Ramos" = "Albert Ramos-Vinolas",
                                "Storm Sanders" = "Storm Hunter",
                                "Xin Yu Wang" = "Xinyu Wang",
                                "Mackenzie Mcdonald" = "Mackenzie McDonald",
                                "Caty Mcnally" = "Caty McNally",
                                "Viktoria Kuzmova" = "Viktoria Hruncakova",
                                "Cori Gauff" = "Coco Gauff",
                                "Margarita Gasparyan" = "Margarita Betova",
                                "Soon Woo Kwon" = "Soonwoo Kwon",
                                "Christina Mchale" = "Christina McHale",
                                "Alex De Minaur" = "Alex de Minaur",
                                "Botic Van De Zandschulp" = "Botic van De Zandschulp",
                                "Tim Van Rijthoven" = "Tim van Rijthoven",
                                "Chun Hsin Tseng" = "Chun hsin Tseng",
                                "Alizé Cornet" = "Alize Cornet",
                                "CoCo Vandeweghe" = "Coco Vandeweghe",
                                "Jaqueline Adina Cristian" = "Jaqueline Cristian",
                                "Juan Martin Del Potro" = "Juan Martin del Potro",
                                "Kate Makarova"  = "Ekaterina Makarova",
                                "Garbiñe Muguruza" = "Garbine Muguruza",
                                "Carla Suárez Navarro" = "Carla Suarez Navarro",
                                "Daria Gavrilova" = "Daria Saville",
                                "Christian Garin" = "Cristian Garin",
                                "Kateryna Kozlova" = "Kateryna Baindl",
                                "Alison Riske Amritraj" = "Alison Riske",
                                "Jc Aragone" = "JC Aragone",
                                "Catherine Cartan Bellis" = "Catherine Bellis",
                                "Mirjana Lucic Baroni" = "Mirjana Lucic",
                                "Jo Wilfried Tsonga" = "Jo-Wilfried Tsonga",
                                "Victor Estrella" = "Victor Estrella Burgos",
                                "Inigo Cervantes Huegun" = "Inigo Cervantes",
                                "Guillermo Garcia Lopez" = "Guillermo Garcia-Lopez",
                                "Albert Ramos Vinolas" = "Albert Ramos-Vinolas",
                                "Daniel Gimeno Traver" = "Daniel Gimeno-Traver",
                                "D. Muñoz de la Nava" = "Daniel Munoz de la Nava",
                                "Pierre Hugues Herbert" = "Pierre-Hugues Herbert",
                                "Su Wei Hsieh" = "Su-Wei Hsieh",
                                "Mariana Duque-Mariño" = "Mariana Duque-Marino",
                                "Mariana Duque Marino" = "Mariana Duque-Marino",
                                "L. Domínguez Lino" = "Lourdes Dominguez Lino",
                                "Bethanie Mattek Sands" = "Bethanie Mattek-Sands",
                                "Anna Lena Friedsam" = "Anna-Lena Friedsam",
                                "Cristina Andreea Mitu" = "Andreea Mitu",
                                "Jan Lennard Struff" = "Jan-Lennard Struff",
                                "Yen Hsun Lu" = "Yen-Hsun Lu",
                                "Edouard Roger Vasselin" = "Edouard Roger-Vasselin",
                                "Kimiko Date Krumm" = "Kimiko Date-Krumm",
                                "B.Zahlavova Strycova" = "Barbora Strycova",
                                "Maria Teresa Torro Flor" = "Maria Torro-Flor",
                                "An-Sophie Mestach" = "An Sophie Mestach",
                                "Silvia Soler-Espinosa" = "Silvia Soler Espinosa",
                                "Irina-Camelia Begu" = "Irina Camelia Begu",
                                "James Mcgee" = "James McGee",
                                "Danielle Rose Collins" = "Danielle Collins",
                                "Mirjana Lucic-Baroni" = "Mirjana Lucic",
                                "Andreas Haider Maurer" = "Andreas Haider-Maurer",
                                "Samuel Groth" = "Sam Groth",
                                "A.Schmiedlova" = "Anna Schmiedlova",
                                "A.Pavlyuchenkova" = "Anastasia Pavlyuchenkova",
                                "Alexandra Cadantu Ignatik" = "Alexandra Cadantu",
                                "M.Torro-Flor" = "Maria Torro-Flor",
                                "M.Larcher De Brito" = "Michelle Larcher De Brito",
                                "Paul Henri Mathieu" = "Paul-Henri Mathieu",
                                "Miloslav Mecir Jr" = "Miloslav Mecir",
                                "Ds.Schwartzman" = "Diego Schwartzman",
                                "Anna Karolina Schmiedlova" = "Anna Schmiedlova",
                                "Iveta Melzer" = "Iveta Benesova",
                                "E.Cabeza Candela" = "Estrella Cabeza Candela",
                                "Mt.Torro-Flor" = "Maria Torro-Flor",
                                "Stanislas Wawrinka" = "Stan Wawrinka",
                                "Alex Bogomolov Jr." = "Alex Bogomolov",
                                "Alex Bogomolov Jr" = "Alex Bogomolov",
                                "Klara Zakopalova" = "Klara Koukalova",
                                "Pablo Carreno Busta" = "Pablo Carreno-Busta",
                                "Kai Chen Chang" = "Kai-Chen Chang",
                                "Edina Gallovits Hall" = "Edina Gallovits-Hall",
                                "Jamie Lee Hampton" = "Jamie Hampton",
                                "Laura Pous Tio" = "Laura Pous-Tio",
                                "Cedrik Marcel Stebe" = "Cedrik-Marcel Stebe",
                                "Grigor DiMitrov" = "Grigor Dimitrov",
                                "A. Medina Garrigues" = "Anabel Medina Garrigues",
                                "Barbora Zahlavova Strycova" = "Barbora Strycova",
                                "Yung Jan Chan" = "Yung-Jan Chan",
                                "Rogerio Dutra Da Silva" = "Rogerio Dutra Silva",
                                "Jean Rene Lisnard" = "Jean-Rene Lisnard",
                                "Reka Luca Jani" = "Reka-Luca Jani",
                                "Vesna Dolonts" = "Vesna Dolonc",
                                "Anastasia Yakimova" = "Anastasiya Yakimova",
                                "Patricia Mayr Achleitner" = "Patricia Mayr-Achleitner",
                                "Richard Berankis" = "Ricardas Berankis",
                                "Thiemo De Bakker" = "Thiemo de Bakker",
                                "Vesna Manasieva" = "Vesna Dolonc",
                                "Lesya Tsurenko" = "Lesia Tsurenko",
                                "A. Schmiedlova" = "Anna Schmiedlova",
                                "Jarmila Gajdosova" = "Jarmila Wolfe",
                                "P.Mayr-Achleitner" = "Patricia Mayr-Achleitner",
                                "A.Medina Garrigues" = "Anabel Medina Garrigues",
                                "Ying Ying Duan" = "Ying-Ying Duan",
                                "Daniel Munoz-De La Nava" = "Daniel Munoz de la Nava",
                                "Tatjana Malek" = "Tatjana Maria",
                                "Lara Arruabarrena-Vecino" = "Lara Arruabarrena",
                                "Jarmila Groth" = "Jarmila Wolfe",
                                "Lesley Pattinama Kerkhove" = "Lesley Kerkhove",
                                "Catherine McNally" = "Caty McNally",
                                "Marcelo Tomas Barrios Vera" = "Tomas Barrios Vera",
                                "Maria Camila Osorio Serrano" = "Camila Osorio",
                                "Holger Vitus Nodskov Rune" = "Holger Rune",
                                "Samantha Murray Sharan" = "Samantha Murray"
  ))
}

rank_wrangle <- function(df){
  df %>%
    filter(tourney_name %in% grand_slams) %>%
    pivot_longer(c(winner_name, loser_name), values_to = "player", names_to = "winner") %>%
    mutate(
      ht = if_else(winner == "winner_name", winner_ht, loser_ht),
      hand = if_else(winner == "winner_name", winner_hand, loser_hand),
      age = if_else(winner == "winner_name", winner_age, loser_age),
      rank = if_else(winner == "winner_name", winner_rank, loser_rank),
      tourney_name = recode(tourney_name, "Us Open" = "US Open")) %>%
    select(tourney_name, player, rank, ht, hand, age) %>%
    distinct() %>%
    recoding_names()
}

atp_rankings_2023 <- read_csv("tennis_atp-master/atp_matches_2023.csv") %>%
  rank_wrangle()
atp_rankings_2022 <- read_csv("tennis_atp-master/atp_matches_2022.csv") %>%
  rank_wrangle()
atp_rankings_2021 <- read_csv("tennis_atp-master/atp_matches_2021.csv") %>%
  rank_wrangle()
atp_rankings_2020 <- read_csv("tennis_atp-master/atp_matches_2020.csv") %>%
  rank_wrangle()
atp_rankings_2019 <- read_csv("tennis_atp-master/atp_matches_2019.csv") %>%
  rank_wrangle()
atp_rankings_2018 <- read_csv("tennis_atp-master/atp_matches_2018.csv") %>%
  rank_wrangle()
atp_rankings_2017 <- read_csv("tennis_atp-master/atp_matches_2017.csv") %>%
  rank_wrangle()
atp_rankings_2016 <- read_csv("tennis_atp-master/atp_matches_2016.csv") %>%
  rank_wrangle()
atp_rankings_2015 <- read_csv("tennis_atp-master/atp_matches_2015.csv") %>%
  rank_wrangle()
atp_rankings_2014 <- read_csv("tennis_atp-master/atp_matches_2014.csv") %>%
  rank_wrangle()
atp_rankings_2013 <- read_csv("tennis_atp-master/atp_matches_2013.csv") %>%
  rank_wrangle()
atp_rankings_2012 <- read_csv("tennis_atp-master/atp_matches_2012.csv") %>%
  rank_wrangle()
atp_rankings_2011 <- read_csv("tennis_atp-master/atp_matches_2011.csv") %>%
  rank_wrangle()

wta_rankings_2023 <- read_csv("tennis_wta-master/wta_matches_2023.csv") %>%
  rank_wrangle()
wta_rankings_2022 <- read_csv("tennis_wta-master/wta_matches_2022.csv") %>%
  rank_wrangle()
wta_rankings_2021 <- read_csv("tennis_wta-master/wta_matches_2021.csv") %>%
  rank_wrangle()
wta_rankings_2020 <- read_csv("tennis_wta-master/wta_matches_2020.csv") %>%
  rank_wrangle()
wta_rankings_2019 <- read_csv("tennis_wta-master/wta_matches_2019.csv") %>%
  rank_wrangle()
wta_rankings_2018 <- read_csv("tennis_wta-master/wta_matches_2018.csv") %>%
  rank_wrangle()
wta_rankings_2017 <- read_csv("tennis_wta-master/wta_matches_2017.csv") %>%
  rank_wrangle()
wta_rankings_2016 <- read_csv("tennis_wta-master/wta_matches_2016.csv") %>%
  rank_wrangle()
wta_rankings_2015 <- read_csv("tennis_wta-master/wta_matches_2015.csv") %>%
  rank_wrangle()
wta_rankings_2014 <- read_csv("tennis_wta-master/wta_matches_2014.csv") %>%
  rank_wrangle()
wta_rankings_2013 <- read_csv("tennis_wta-master/wta_matches_2013.csv") %>%
  rank_wrangle()
wta_rankings_2012 <- read_csv("tennis_wta-master/wta_matches_2012.csv") %>%
  rank_wrangle()
wta_rankings_2011 <- read_csv("tennis_wta-master/wta_matches_2011.csv") %>%
  rank_wrangle()

rankings_2023 <- bind_rows(atp_rankings_2023, wta_rankings_2023)
rankings_2022 <- bind_rows(atp_rankings_2022, wta_rankings_2022)
rankings_2021 <- bind_rows(atp_rankings_2021, wta_rankings_2021)
rankings_2020 <- bind_rows(atp_rankings_2020, wta_rankings_2020)
rankings_2019 <- bind_rows(atp_rankings_2019, wta_rankings_2019)
rankings_2018 <- bind_rows(atp_rankings_2018, wta_rankings_2018)
rankings_2017 <- bind_rows(atp_rankings_2017, wta_rankings_2017)
rankings_2016 <- bind_rows(atp_rankings_2016, wta_rankings_2016)
rankings_2015 <- bind_rows(atp_rankings_2015, wta_rankings_2015)
rankings_2014 <- bind_rows(atp_rankings_2014, wta_rankings_2014)
rankings_2013 <- bind_rows(atp_rankings_2013, wta_rankings_2013)
rankings_2012 <- bind_rows(atp_rankings_2012, wta_rankings_2012)
rankings_2011 <- bind_rows(atp_rankings_2011, wta_rankings_2011)

combine_match_rank <- function(matches, rankings, tournament){
  matches %>%
    rename(player = player1) %>%
    recoding_names() %>%
    left_join(rankings %>% filter(tourney_name == tournament), by = "player") %>%
    rename(player1 = player,
           player = player2,
           ranking1 = rank,
           ht1 = ht,
           hand1 = hand,
           age1 = age) %>%
    recoding_names() %>%
    left_join(rankings %>% filter(tourney_name == tournament), by = "player") %>%
    rename(player2 = player,
           ranking2 = rank,
           ht2 = ht,
           hand2 = hand,
           age2 = age) %>%
    select(-contains("tourney_name"))
}

u2023_matches <- combine_match_rank(u2023_matches_0, rankings_2023, "US Open")
w2023_matches <- combine_match_rank(w2023_matches_0, rankings_2023, "Wimbledon")

u2022_matches <- combine_match_rank(u2022_matches_0, rankings_2022, "US Open")
w2022_matches <- combine_match_rank(w2022_matches_0, rankings_2022, "Wimbledon")

u2021_matches <- combine_match_rank(u2021_matches_0, rankings_2021, "US Open")
w2021_matches <- combine_match_rank(w2021_matches_0, rankings_2021, "Wimbledon")

u2020_matches <- combine_match_rank(u2020_matches_0, rankings_2020, "US Open")

u2019_matches <- combine_match_rank(u2019_matches_0, rankings_2019, "US Open")
w2019_matches <- combine_match_rank(w2019_matches_0, rankings_2019, "Wimbledon")

u2018_matches <- combine_match_rank(u2018_matches_0, rankings_2018, "US Open")
w2018_matches <- combine_match_rank(w2018_matches_0, rankings_2018, "Wimbledon")

u2017_matches <- combine_match_rank(u2017_matches_0, rankings_2017, "US Open")
w2017_matches <- combine_match_rank(w2017_matches_0, rankings_2017, "Wimbledon")
f2017_matches <- combine_match_rank(f2017_matches_0, rankings_2017, "Roland Garros")
a2017_matches <- combine_match_rank(a2017_matches_0, rankings_2017, "Australian Open")

u2016_matches <- combine_match_rank(u2016_matches_0, rankings_2016, "US Open")
w2016_matches <- combine_match_rank(w2016_matches_0, rankings_2016, "Wimbledon")
f2016_matches <- combine_match_rank(f2016_matches_0, rankings_2016, "Roland Garros")
a2016_matches <- combine_match_rank(a2016_matches_0, rankings_2016, "Australian Open")

u2015_matches <- combine_match_rank(u2015_matches_0, rankings_2015, "US Open")
w2015_matches <- combine_match_rank(w2015_matches_0, rankings_2015, "Wimbledon")
f2015_matches <- combine_match_rank(f2015_matches_0, rankings_2015, "Roland Garros")
a2015_matches <- combine_match_rank(a2015_matches_0, rankings_2015, "Australian Open")

u2014_matches <- combine_match_rank(u2014_matches_0, rankings_2014, "US Open")
w2014_matches <- combine_match_rank(w2014_matches_0, rankings_2014, "Wimbledon")
f2014_matches <- combine_match_rank(f2014_matches_0, rankings_2014, "Roland Garros")
a2014_matches <- combine_match_rank(a2014_matches_0, rankings_2014, "Australian Open")

u2013_matches <- combine_match_rank(u2013_matches_0, rankings_2013, "US Open")
w2013_matches <- combine_match_rank(w2013_matches_0, rankings_2013, "Wimbledon")
f2013_matches <- combine_match_rank(f2013_matches_0, rankings_2013, "Roland Garros")
a2013_matches <- combine_match_rank(a2013_matches_0, rankings_2013, "Australian Open")

u2012_matches <- combine_match_rank(u2012_matches_0, rankings_2012, "US Open")
w2012_matches <- combine_match_rank(w2012_matches_0, rankings_2012, "Wimbledon")
f2012_matches <- combine_match_rank(f2012_matches_0, rankings_2012, "Roland Garros")
a2012_matches <- combine_match_rank(a2012_matches_0, rankings_2012, "Australian Open")

u2011_matches <- combine_match_rank(u2011_matches_0, rankings_2011, "US Open")
w2011_matches <- combine_match_rank(w2011_matches_0, rankings_2011, "Wimbledon")
f2011_matches <- combine_match_rank(f2011_matches_0, rankings_2011, "Roland Garros")
a2011_matches <- combine_match_rank(a2011_matches_0, rankings_2011, "Australian Open")

# betting odds and merge
reading_betting <- function(path_to_file){
  read_csv(path_to_file) %>%
    select(Round, Winner, Loser, WRank, LRank, B365W, B365L, PSW, PSL, `Best of`) %>%
    mutate(rank_1 = str_c(WRank, "and", LRank),
           rank_2 = str_c(LRank, "and", WRank),
           sex = if_else(`Best of` == 3, "women", "men")) %>%
    pivot_longer(cols = c(rank_1, rank_2), names_to = "order", values_to = "ranking_c") %>%
    select(-WRank, -LRank, -order, -`Best of`)
}

merge_matches_betting <- function(bet1, bet2, matches){
  bets <- reading_betting(bet1) %>% bind_rows(reading_betting(bet2))
  matches %>% mutate(
    sex = if_else(str_detect(match_id, "-1"), "men", "women"),
    ranking_c = str_c(ranking1, "and", ranking2)) %>%
    left_join(bets, by = join_by(ranking_c, sex)) %>%
    select(-Winner, -Loser, -Round, -ranking_c, -sex)
}

u2023_full <- merge_matches_betting("betting odds/usopen2023m.csv", "betting odds/usopen2023f.csv", u2023_matches)
w2023_full <- merge_matches_betting("betting odds/wimbledon2023m.csv", "betting odds/wimbledon2023f.csv", w2023_matches)

u2022_full <- merge_matches_betting("betting odds/usopen2022m.csv", "betting odds/usopen2022f.csv", u2022_matches)
w2022_full <- merge_matches_betting("betting odds/wimbledon2022m.csv", "betting odds/wimbledon2022f.csv", w2022_matches)

u2021_full <- merge_matches_betting("betting odds/usopen2021m.csv", "betting odds/usopen2021f.csv", u2021_matches)
w2021_full <- merge_matches_betting("betting odds/wimbledon2021m.csv", "betting odds/wimbledon2021f.csv", w2021_matches)

u2020_full <- merge_matches_betting("betting odds/usopen2020m.csv", "betting odds/usopen2020f.csv", u2020_matches)

u2019_full <- merge_matches_betting("betting odds/usopen2019m.csv", "betting odds/usopen2019f.csv", u2019_matches)
w2019_full <- merge_matches_betting("betting odds/wimbledon2019m.csv", "betting odds/wimbledon2019f.csv", w2019_matches)

u2018_full <- merge_matches_betting("betting odds/usopen2018m.csv", "betting odds/usopen2018f.csv", u2018_matches)
w2018_full <- merge_matches_betting("betting odds/wimbledon2018m.csv", "betting odds/wimbledon2018f.csv", w2018_matches)

u2017_full <- merge_matches_betting("betting odds/usopen2017m.csv", "betting odds/usopen2017f.csv", u2017_matches)
w2017_full <- merge_matches_betting("betting odds/wimbledon2017m.csv", "betting odds/wimbledon2017f.csv", w2017_matches)
f2017_full <- merge_matches_betting("betting odds/frenchopen2017m.csv", "betting odds/frenchopen2017f.csv", f2017_matches)
a2017_full <- merge_matches_betting("betting odds/ausopen2017m.csv", "betting odds/ausopen2017f.csv", a2017_matches)

u2016_full <- merge_matches_betting("betting odds/usopen2016m.csv", "betting odds/usopen2016f.csv", u2016_matches)
w2016_full <- merge_matches_betting("betting odds/wimbledon2016m.csv", "betting odds/wimbledon2016f.csv", w2016_matches)
f2016_full <- merge_matches_betting("betting odds/frenchopen2016m.csv", "betting odds/frenchopen2016f.csv", f2016_matches)
a2016_full <- merge_matches_betting("betting odds/ausopen2016m.csv", "betting odds/ausopen2016f.csv", a2016_matches)

u2015_full <- merge_matches_betting("betting odds/usopen2015m.csv", "betting odds/usopen2015f.csv", u2015_matches)
w2015_full <- merge_matches_betting("betting odds/wimbledon2015m.csv", "betting odds/wimbledon2015f.csv", w2015_matches)
f2015_full <- merge_matches_betting("betting odds/frenchopen2015m.csv", "betting odds/frenchopen2015f.csv", f2015_matches)
a2015_full <- merge_matches_betting("betting odds/ausopen2015m.csv", "betting odds/ausopen2015f.csv", a2015_matches)

u2014_full <- merge_matches_betting("betting odds/usopen2014m.csv", "betting odds/usopen2014f.csv", u2014_matches)
w2014_full <- merge_matches_betting("betting odds/wimbledon2014m.csv", "betting odds/wimbledon2014f.csv", w2014_matches)
f2014_full <- merge_matches_betting("betting odds/frenchopen2014m.csv", "betting odds/frenchopen2014f.csv", f2014_matches)
a2014_full <- merge_matches_betting("betting odds/ausopen2014m.csv", "betting odds/ausopen2014f.csv", a2014_matches)

u2013_full <- merge_matches_betting("betting odds/usopen2013m.csv", "betting odds/usopen2013f.csv", u2013_matches)
w2013_full <- merge_matches_betting("betting odds/wimbledon2013m.csv", "betting odds/wimbledon2013f.csv", w2013_matches)
f2013_full <- merge_matches_betting("betting odds/frenchopen2013m.csv", "betting odds/frenchopen2013f.csv", f2013_matches)
a2013_full <- merge_matches_betting("betting odds/ausopen2013m.csv", "betting odds/ausopen2013f.csv", a2013_matches)

u2012_full <- merge_matches_betting("betting odds/usopen2012m.csv", "betting odds/usopen2012f.csv", u2012_matches)
w2012_full <- merge_matches_betting("betting odds/wimbledon2012m.csv", "betting odds/wimbledon2012f.csv", w2012_matches)
f2012_full <- merge_matches_betting("betting odds/frenchopen2012m.csv", "betting odds/frenchopen2012f.csv", f2012_matches)
a2012_full <- merge_matches_betting("betting odds/ausopen2012m.csv", "betting odds/ausopen2012f.csv", a2012_matches)

u2011_full <- merge_matches_betting("betting odds/usopen2011m.csv", "betting odds/usopen2011f.csv", u2011_matches)
w2011_full <- merge_matches_betting("betting odds/wimbledon2011m.csv", "betting odds/wimbledon2011f.csv", w2011_matches)
f2011_full <- merge_matches_betting("betting odds/frenchopen2011m.csv", "betting odds/frenchopen2011f.csv", f2011_matches)
a2011_full <- merge_matches_betting("betting odds/ausopen2011m.csv", "betting odds/ausopen2011f.csv", a2011_matches)

matches <- bind_rows(u2023_full, w2023_full, u2022_full, w2022_full,
                     u2021_full, w2021_full, u2020_full, 
                     u2019_full, w2019_full, u2018_full, w2018_full,
                     u2017_full, w2017_full, f2017_full, a2017_full,
                     u2016_full, w2016_full, f2016_full, a2016_full,
                     u2015_full, w2015_full, f2015_full, a2015_full,
                     u2014_full, w2014_full, f2014_full, a2014_full,
                     u2013_full, w2013_full, f2013_full, a2013_full,
                     u2012_full, w2012_full, f2012_full, a2012_full,
                     u2011_full, w2011_full, f2011_full, a2011_full)

reading_points <- function(path_to_file){
  read_csv(path_to_file,
           col_types = cols(PointNumber = col_double(), ElapsedTime = col_time(),
                            History = col_double(), P1TurningPoint = col_double(),
                            P2TurningPoint = col_double()))
}

u2023_0 <- reading_points("tennis_slam_pointbypoint-master/2023-usopen-points.csv")
w2023_0 <- reading_points("tennis_slam_pointbypoint-master/2023-wimbledon-points.csv")

u2022_0 <- reading_points("tennis_slam_pointbypoint-master/2022-usopen-points.csv")
w2022_0 <- reading_points("tennis_slam_pointbypoint-master/2022-wimbledon-points.csv")

u2021_0 <- reading_points("tennis_slam_pointbypoint-master/2021-usopen-points.csv")
w2021_0 <- reading_points("tennis_slam_pointbypoint-master/2021-wimbledon-points.csv")

u2020_0 <- reading_points("tennis_slam_pointbypoint-master/2020-usopen-points.csv")

u2019_0 <- reading_points("tennis_slam_pointbypoint-master/2019-usopen-points.csv")
w2019_0 <- reading_points("tennis_slam_pointbypoint-master/2019-wimbledon-points.csv")

u2018_0 <- reading_points("tennis_slam_pointbypoint-master/2018-usopen-points.csv")
w2018_0 <- reading_points("tennis_slam_pointbypoint-master/2018-wimbledon-points.csv")

u2017_0 <- reading_points("tennis_slam_pointbypoint-master/2017-usopen-points.csv")
w2017_0 <- reading_points("tennis_slam_pointbypoint-master/2017-wimbledon-points.csv")
f2017_0 <- reading_points("tennis_slam_pointbypoint-master/2017-frenchopen-points.csv")
a2017_0 <- reading_points("tennis_slam_pointbypoint-master/2017-ausopen-points.csv")

u2016_0 <- reading_points("tennis_slam_pointbypoint-master/2016-usopen-points.csv")
w2016_0 <- reading_points("tennis_slam_pointbypoint-master/2016-wimbledon-points.csv")
f2016_0 <- reading_points("tennis_slam_pointbypoint-master/2016-frenchopen-points.csv")
a2016_0 <- reading_points("tennis_slam_pointbypoint-master/2016-ausopen-points.csv")

u2015_0 <- reading_points("tennis_slam_pointbypoint-master/2015-usopen-points.csv")
w2015_0 <- reading_points("tennis_slam_pointbypoint-master/2015-wimbledon-points.csv")
f2015_0 <- reading_points("tennis_slam_pointbypoint-master/2015-frenchopen-points.csv")
a2015_0 <- reading_points("tennis_slam_pointbypoint-master/2015-ausopen-points.csv")

u2014_0 <- reading_points("tennis_slam_pointbypoint-master/2014-usopen-points.csv")
w2014_0 <- reading_points("tennis_slam_pointbypoint-master/2014-wimbledon-points.csv")
f2014_0 <- reading_points("tennis_slam_pointbypoint-master/2014-frenchopen-points.csv")
a2014_0 <- reading_points("tennis_slam_pointbypoint-master/2014-ausopen-points.csv")

u2013_0 <- reading_points("tennis_slam_pointbypoint-master/2013-usopen-points.csv")
w2013_0 <- reading_points("tennis_slam_pointbypoint-master/2013-wimbledon-points.csv")
f2013_0 <- reading_points("tennis_slam_pointbypoint-master/2013-frenchopen-points.csv")
a2013_0 <- reading_points("tennis_slam_pointbypoint-master/2013-ausopen-points.csv")

u2012_0 <- reading_points("tennis_slam_pointbypoint-master/2012-usopen-points.csv")
w2012_0 <- reading_points("tennis_slam_pointbypoint-master/2012-wimbledon-points.csv")
f2012_0 <- reading_points("tennis_slam_pointbypoint-master/2012-frenchopen-points.csv")
a2012_0 <- reading_points("tennis_slam_pointbypoint-master/2012-ausopen-points.csv")

u2011_0 <- reading_points("tennis_slam_pointbypoint-master/2011-usopen-points.csv")
w2011_0 <- reading_points("tennis_slam_pointbypoint-master/2011-wimbledon-points.csv")
f2011_0 <- reading_points("tennis_slam_pointbypoint-master/2011-frenchopen-points.csv")
a2011_0 <- reading_points("tennis_slam_pointbypoint-master/2011-ausopen-points.csv")

gs_0 <- bind_rows(u2023_0, w2023_0, u2022_0, w2022_0,
                  u2021_0, w2021_0, u2020_0,
                  u2019_0, w2019_0, u2018_0, w2018_0,
                  u2017_0, w2017_0, f2017_0, a2017_0,
                  u2016_0, w2016_0, f2016_0, a2016_0,
                  u2015_0, w2015_0, f2015_0, a2015_0,
                  u2014_0, w2014_0, f2014_0, a2014_0,
                  u2013_0, w2013_0, f2013_0, a2013_0,
                  u2012_0, w2012_0, f2012_0, a2012_0,
                  u2011_0, w2011_0, f2011_0, a2011_0) %>%
  left_join(matches, by = "match_id")

# function that calculates the mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}

char_int <- c(1:14, 16:29) %>% as.character()

# filtering out some data and removing excess variables
gs_1 <- gs_0 %>%
  filter((P1PointsWon + P2PointsWon) != 0) %>%
  mutate(
    RallyCount = case_when(
      is.na(RallyCount) ~ Rally,
      .default = RallyCount),
    ServeNumber = case_when(
      is.na(ServeNumber) & (P1FirstSrvIn + P2FirstSrvIn == 0) ~ 2,
      is.na(ServeNumber) & (P1FirstSrvIn + P2FirstSrvIn == 1) ~ 1,
      .default = ServeNumber)) %>%
  select(
    -Rally, -P1FirstSrvIn, -P1FirstSrvWon, -P2FirstSrvIn, -P2FirstSrvWon, -ServeIndicator,
    -P1SecondSrvIn, -P1SecondSrvWon, -P2SecondSrvIn, -P2SecondSrvWon,
    -P1ForcedError, -P2ForcedError, -Serve_Direction, -Winner_FH, -Winner_BH,
    -ServingTo, -P1TurningPoint, -P2TurningPoint, -WinnerType, -History, -P1Momentum, -P2Momentum,
    -contains("BreakPoint")) %>%
  rename(
    server = PointServer,
    point_victor = PointWinner,
    set_victor = SetWinner,
    point_no = PointNumber,
    elapsed_time = ElapsedTime,
    set_no = SetNo,
    game_no = GameNo,
    serve_no = ServeNumber,
    game_victor = GameWinner,
    p1_points_won = P1PointsWon,
    p2_points_won = P2PointsWon,
    p1_ace = P1Ace,
    p2_ace = P2Ace,
    p1_winner = P1Winner,
    p2_winner = P2Winner,
    winner_shot_type = WinnerShotType,
    p1_double_fault = P1DoubleFault,
    p2_double_fault = P2DoubleFault,
    p1_unf_err = P1UnfErr,
    p2_unf_err = P2UnfErr,
    p1_net_pt = P1NetPoint,
    p2_net_pt = P2NetPoint,
    p1_net_pt_won = P1NetPointWon,
    p2_net_pt_won = P2NetPointWon,
    p1_distance_run = P1DistanceRun,
    p2_distance_run = P2DistanceRun,
    rally_count = RallyCount,
    speed_mph = Speed_MPH,
    speed_kmh = Speed_KMH,
    serve_width = ServeWidth,
    serve_depth = ServeDepth,
    return_depth = ReturnDepth,
    p1_games = P1GamesWon,
    p2_games = P2GamesWon,
    p1_score = P1Score,
    p2_score = P2Score) %>%
  mutate(
    serve_no = case_when(
      p1_double_fault == 1 ~ 2,
      p2_double_fault == 1 ~ 2,
      .default = serve_no),
    year = str_split(match_id, "-", simplify = TRUE)[,1],
    tournament = str_split(match_id, "-", simplify = TRUE)[,2],
    match = str_split(match_id, "-", simplify = TRUE)[,3],
    p1_games = lag(p1_games),
    p2_games = lag(p2_games),
    p1_score = lag(p1_score),
    p2_score = lag(p2_score),
    tiebreak = case_when(
      p1_score %in% char_int ~ 1,
      p2_score %in% char_int ~ 1,
      lead(p1_score) %in% char_int ~ 1,
      lead(p2_score) %in% char_int ~ 1,
      .default = 0),
    set_victor = case_when(
      tiebreak == 1 ~ game_victor,
      (p1_games > p2_games) & p1_games > 4 & game_victor == 1 ~ 1,
      (p2_games > p1_games) & p2_games > 4 & game_victor == 2 ~ 2,
      .default = 0),
    p1_games = if_else(lag(set_victor) > 0, 0, p1_games),
    p2_games = if_else(lag(set_victor) > 0, 0, p2_games),
    retired = if_else(set_victor == 0 & match_id != lead(match_id), 1, 0), # cannot tell which player retired
    first_pt_set = if_else(game_no == 1 & p1_score == 0 & p2_score == 0, 1, 0),
    last_pt_set = if_else(set_victor != 0 | retired != 0, 1, 0),
    match_pt = if_else(match_id != lead(match_id), 1, 0),
    match_pt = if_else(match_id == "2011-ausopen-2701" & point_no == 182, 1, match_pt),
    p1_break_pt = (if_else(server == 2 & p1_score == "AD", 1, 0) +
                         if_else(server == 2 & p1_score == 40 & p2_score != 40 & p2_score != "AD", 1, 0)),
    p2_break_pt = (if_else(server == 1 & p2_score == "AD", 1, 0) +
                         if_else(server == 1 & p2_score == 40 & p1_score != 40 & p1_score != "AD", 1, 0)),
    p1_break_pt_won = if_else(p1_break_pt == 1 & point_victor == 1, 1, 0),
    p2_break_pt_won = if_else(p2_break_pt == 1 & point_victor == 2, 1, 0),
    p1_break_pt_missed = p1_break_pt - p1_break_pt_won,
    p2_break_pt_missed = p2_break_pt - p2_break_pt_won,
    pt_server = server*2 - 3,
    p1_games = replace_na(p1_games, 0),
    p2_games = replace_na(p2_games, 0),
    status = case_when(
      p1_games > p2_games + 1 ~ "p1_break",
      p2_games > p1_games + 1 ~ "p2_break",
      p1_games > p2_games + 1 + pt_server ~ "p1_break",
      p2_games > p1_games + 1 + pt_server*-1 ~ "p2_break",
      .default = "no_break"),
    status = if_else(lag(set_victor) > 0, "no_break", status),
    status = replace_na(status, "no_break"),
    p1_score = replace_na(p1_score, "0"),
    p2_score = replace_na(p2_score, "0"),
    p1_win_set = if_else(set_victor == 1, 1, 0),
    p2_win_set = if_else(set_victor == 2, 1, 0),
    speed_mph = case_when(
      speed_mph > 0 ~ speed_mph,
      speed_kmh > 0 ~ speed_kmh * 0.621371,
      .default = 0)) %>%
  group_by(match_id) %>%
  mutate(
    p1_sets = cumsum(p1_win_set),
    p2_sets = cumsum(p2_win_set),
    p1_sets = lag(p1_sets),
    p2_sets = lag(p2_sets),
    p1_sets = if_else(lag(match_pt) == 1, 0, p1_sets),
    p2_sets = if_else(lag(match_pt) == 1, 0, p2_sets),
    p1_sets = replace_na(p1_sets, 0),
    p2_sets = replace_na(p2_sets, 0),
    
    # add runs
    point_winner = if_else(point_victor == 2, -1, point_victor),
    run = sequence(rle(as.character(point_winner))$lengths) * point_winner) %>%
  ungroup() %>%
  mutate(
    # divides matches up between men's and women's
    sex = if_else(str_detect(match_id, "-1"), 1, 0),
    
    # removes NA from first line
    across(contains("break_pt"), ~replace_na(.,0))) %>%
  select(-pt_server, -contains("win_set"), -point_winner, -speed_kmh) %>%
  relocate(player1, .after = elapsed_time) %>%
  relocate(player2, .after = player1)

# match_ids where the match was labeled as a retirement, but it actually is not a retirement
retired_false <- c("2011-frenchopen-1104", "2011-frenchopen-1105", "2011-frenchopen-1218",
                   "2011-frenchopen-1313", "2011-frenchopen-2303","2012-frenchopen-1129",
                   "2012-frenchopen-2102", "2012-frenchopen-2111", "2012-frenchopen-2118",
                   "2012-usopen-1108", "2012-wimbledon-2149", "2012-wimbledon-2209", 
                   "2013-frenchopen-1202", "2013-frenchopen-1501", "2013-usopen-2502",
                   "2013-wimbledon-1147", "2013-wimbledon-2135", "2013-wimbledon-2146",
                   "2013-wimbledon-2163", "2014-ausopen-1211", "2014-frenchopen-1213",
                   "2014-frenchopen-1216", "2014-frenchopen-2205", "2014-frenchopen-2206", 
                   "2014-frenchopen-2208", "2014-frenchopen-2211", "2014-frenchopen-2215",
                   "2014-frenchopen-2217", "2014-frenchopen-2229", "2014-wimbledon-1146",
                   "2014-wimbledon-2115", "2015-frenchopen-2127", "2015-frenchopen-2206",
                   "2015-usopen-2307", "2015-wimbledon-1107", "2015-wimbledon-1123",
                   "2015-wimbledon-1124", "2015-wimbledon-1133", "2015-wimbledon-2106",
                   "2015-wimbledon-2136", "2016-frenchopen-1137", "2016-frenchopen-1303",
                   "2016-frenchopen-2121", "2016-frenchopen-2221", "2016-frenchopen-2307",
                   "2016-frenchopen-2407", "2017-frenchopen-1120", "2017-frenchopen-1127",
                   "2017-frenchopen-1131", "2017-frenchopen-1161", "2017-frenchopen-1163",
                   "2017-frenchopen-2134", "2017-wimbledon-2401", "2017-wimbledon-2403",
                   "2017-wimbledon-2404", "2017-wimbledon-2405", "2017-wimbledon-2406",
                   "2020-usopen-1225", "2020-usopen-2211", "2020-usopen-2221",
                   "2021-wimbledon-1122", "2021-wimbledon-1150", "2021-wimbledon-1206",
                   "2021-wimbledon-2137", "2021-wimbledon-2221", "2022-usopen-1103",
                   "2022-usopen-1114", "2022-usopen-1116", "2022-usopen-1149",
                   "2022-usopen-1150", "2022-usopen-1151", "2022-usopen-1504",
                   "2022-usopen-2121", "2022-usopen-2123", "2022-usopen-2139",
                   "2022-usopen-2140", "2022-usopen-2150", "2022-usopen-2152",
                   "2022-usopen-2154", "2022-usopen-2155", "2022-usopen-2160")

wacko <- c("2012-usopen-1108", "2015-frenchopen-1222", "2017-wimbledon-1120", "2011-frenchopen-1203", "2013-frenchopen-1501", # these are messed up
           "2011-frenchopen-1218", "2012-frenchopen-1129", "2013-frenchopen-1226", "2014-frenchopen-1230", # scoring got barely messed up 
           "2015-frenchopen-1126", "2022-usopen-1504", # scoring got barely messed up 
           "2011-frenchopen-1212", "2011-frenchopen-1225", "2012-frenchopen-1104", "2013-frenchopen-1217", # missing a few points at end of one of the sets
           "2013-frenchopen-1231", "2013-frenchopen-1402", "2014-frenchopen-1142", "2014-frenchopen-1220", # missing a few points at end of one of the sets
           "2015-frenchopen-1128", "2018-wimbledon-1228", # missing a few points at end of one of the sets
           "2022-usopen-2160", # only tracked 2 points
           "2011-ausopen-2305", # retired after 7 points
           "2011-ausopen-1140", "2011-frenchopen-2312", "2012-frenchopen-1503", "2013-frenchopen-1109", # missing points in the middle
           "2014-frenchopen-1206", "2014-frenchopen-1208", "2014-frenchopen-2203", "2014-frenchopen-2221", # missing points in the middle
           "2014-frenchopen-2225", "2014-frenchopen-2227", "2014-frenchopen-2230", "2015-frenchopen-1101", # missing points in the middle
           "2015-frenchopen-1109", "2015-frenchopen-2225", "2014-frenchopen-1216", "2014-frenchopen-2217", # missing points in the middle
           "2014-frenchopen-2218", "2015-frenchopen-1123", "2012-usopen-1403", "2014-frenchopen-1211", # missing points in the middle
           "2014-frenchopen-1213", "2014-frenchopen-2212", "2015-frenchopen-1224", "2015-frenchopen-2113", # missing points in the middle
           "2015-frenchopen-2127", "2013-frenchopen-1116", "2013-frenchopen-1219", "2014-frenchopen-2204", # missing points in the middle
           "2013-frenchopen-1221", "2014-frenchopen-1203", "2014-frenchopen-2210", "2013-frenchopen-2406", # missing points in the middle
           "2014-frenchopen-2205", "2014-frenchopen-1214", "2014-frenchopen-2208", "2015-wimbledon-1229", # missing points in the middle
           "2014-frenchopen-2211", "2014-frenchopen-2206", "2014-frenchopen-2215", "2012-frenchopen-2111", # missing points in the middle
           "2012-frenchopen-2118" # missing points in the middle)
)

gs_2 <- gs_1 %>%
  filter(last_pt_set == 1) %>%
  group_by(match_id) %>%
  summarise(match_victor = get_mode(set_victor)) %>%
  distinct() %>%
  # matches that were not full in data and had incorrect match victors
  mutate(
    match_victor = case_when(
      match_id == "2011-ausopen-2305" ~ 2,
      match_id == "2011-frenchopen-2303" ~ 1,
      match_id == "2011-usopen-1144" ~ 2,
      match_id == "2011-wimbledon-1202" ~ 1,
      match_id == "2012-ausopen-2304" ~ 2,
      match_id == "2012-frenchopen-1124" ~ 1,
      match_id == "2012-frenchopen-2102" ~ 2,
      match_id == "2012-frenchopen-2111" ~ 1,
      match_id == "2012-usopen-1108" ~ 2,
      match_id == "2012-wimbledon-2149" ~ 2,
      match_id == "2013-ausopen-1203" ~ 1,
      match_id == "2013-usopen-2502" ~ 1,
      match_id == "2013-wimbledon-1124" ~ 1,
      match_id == "2013-wimbledon-1219" ~ 2,
      match_id == "2014-frenchopen-1105" ~ 2,
      match_id == "2014-frenchopen-1108" ~ 1,
      match_id == "2014-frenchopen-1138" ~ 2,
      match_id == "2014-frenchopen-1154" ~ 2,
      match_id == "2014-frenchopen-2139" ~ 1,
      match_id == "2014-frenchopen-2146" ~ 1,
      match_id == "2014-frenchopen-2206" ~ 2,
      match_id == "2014-frenchopen-2230" ~ 1,
      match_id == "2014-usopen-1137" ~ 1,
      match_id == "2014-usopen-1138" ~ 2,
      match_id == "2014-wimbledon-1159" ~ 1,
      match_id == "2014-wimbledon-2115" ~ 1,
      match_id == "2015-ausopen-1205" ~ 1,
      match_id == "2015-frenchopen-2127" ~ 2,
      match_id == "2015-usopen-1124" ~ 1,
      match_id == "2015-usopen-1156" ~ 2,
      match_id == "2015-usopen-1218" ~ 1,
      match_id == "2015-usopen-1302" ~ 1,
      match_id == "2015-wimbledon-1123" ~ 1,
      match_id == "2015-wimbledon-2163" ~ 2,
      match_id == "2016-ausopen-1159" ~  1,
      match_id == "2016-frenchopen-1308" ~ 1,
      match_id == "2016-frenchopen-2221" ~ 1,
      match_id == "2016-frenchopen-2307" ~ 1,
      match_id == "2016-usopen-1157" ~ 1,
      match_id == "2016-usopen-1301" ~ 1,
      match_id == "2016-wimbledon-1407" ~ 2,
      match_id == "2017-frenchopen-1127" ~ 1,
      match_id == "2017-frenchopen-1161" ~ 2,
      match_id == "2017-frenchopen-1163" ~ 1,
      match_id == "2017-frenchopen-1314" ~ 1,
      match_id == "2017-wimbledon-1151" ~ 2,
      match_id == "2017-wimbledon-1227" ~ 1,
      match_id == "2017-wimbledon-2203" ~ 1,
      match_id == "2017-wimbledon-2401" ~ 2,
      match_id == "2019-wimbledon-1108" ~ 1,
      match_id == "2019-wimbledon-2114" ~ 2,
      match_id == "2019-wimbledon-2157" ~ 1,
      match_id == "2019-wimbledon-2217" ~ 1,
      match_id == "2020-usopen-1126" ~ 2,
      match_id == "2020-usopen-1401" ~ 2,
      match_id == "2020-usopen-2211" ~ 1,
      match_id == "2020-usopen-2221" ~ 1,
      match_id == "2021-usopen-1161" ~ 2,
      match_id == "2021-wimbledon-2132" ~ 1,
      match_id == "2021-wimbledon-2137" ~ 1,
      match_id == "2022-usopen-1120" ~ 1,
      match_id == "2022-usopen-1150" ~ 1,
      match_id == "2022-usopen-1151" ~ 1,
      match_id == "2022-usopen-1157" ~ 1,
      match_id == "2022-usopen-2103" ~ 1,
      match_id == "2022-usopen-2139" ~ 1,
      match_id == "2022-usopen-2150" ~ 1,
      match_id == "2022-usopen-2152" ~ 2,
      match_id == "2022-usopen-2160" ~ 2,
      match_id == "2022-wimbledon-1128" ~ 1,
      match_id == "2022-wimbledon-1207" ~ 1,
      match_id == "2022-wimbledon-2144" ~ 2,
      match_id == "2022-wimbledon-2146" ~ 2,
      match_id == "2023-wimbledon-2406" ~ 2,
      match_id == "2023-usopen-1129" ~ 2,
      .default = match_victor)) %>%
  right_join(gs_1, by = "match_id") %>%
  mutate(
    retired = case_when(
      match_id %in% retired_false ~ 0,
      .default = retired),
    missing_data = if_else(match_id %in% retired_false, 1, 0)) %>%
  relocate(contains("player"), .after = "match_id") %>%
  relocate("ranking1", .after = "player1") %>%
  relocate("ranking2", .after = "player2") %>%
  relocate("game_no", .after = "set_no") %>%
  relocate("server", .after = "game_no") %>%
  relocate("point_no", .after = "server") %>%
  relocate("status", .after = "point_no") %>% 
  relocate(contains("sets"), .after = "status") %>% 
  relocate(contains("games"), .after = "p2_sets") %>%
  relocate(contains("score"), .after = "p2_games") %>%
  relocate("point_victor", .after = "p2_score") %>%
  relocate(contains("points_won"), .after = "point_victor") %>%
  relocate("game_victor", .after = "p2_points_won") %>%
  relocate("set_victor", .after = "game_victor") %>%
  relocate("match_victor", .after = "set_victor") %>%
  relocate("serve_no", .after = "server") %>%
  relocate("winner_shot_type", .after = "p2_winner") %>%
  relocate("speed_mph", .before = "serve_width") %>%
  relocate(contains("break_pt"), .after = "p2_net_pt_won") %>%
  # these matches are wacko messed up :(
  filter(match_id %!in% wacko,
         point_victor != 0) %>%
  mutate(speed_mph = na_if(speed_mph, 0),
         b365_1 = if_else(match_victor == 1, B365W, B365L),
         b365_2 = if_else(match_victor == 2, B365W, B365L),
         ps_1 = if_else(match_victor == 1, PSW, PSL),
         ps_2 = if_else(match_victor == 2, PSW, PSL)) %>%
  relocate(b365_1, .after = "age2") %>%
  relocate(b365_2, .after = "b365_1") %>%
  relocate(ps_1, .after = "b365_2") %>%
  relocate(ps_2, .after = "ps_1") %>%
  select(-B365W, -B365L, -PSW, -PSL)

# adding delays
gs <- gs_2 %>%
  group_by(match_id) %>%
  mutate(
    seconds = second(elapsed_time) + 60 * minute(elapsed_time) + 3600 * hour(elapsed_time) + 86400 * day(elapsed_time),
    lag_seconds = lag(seconds),
    time_diff = if_else(point_no == 1, 0, seconds - lag_seconds)) %>%
  ungroup() %>%
  mutate(delay = if_else(time_diff > 900, 1, 0)) %>%
  relocate(delay, .after = retired) %>%
  relocate(point_no, .after = game_no) %>%
  relocate(time_diff, .after = elapsed_time) %>%
  mutate(delay = replace_na(delay, 0)) %>%
  arrange(match_id, point_no) %>%
  select(-seconds, -lag_seconds) %>%
  mutate(
    # lots of the covariates
    delay = case_when(
      match_id == "2015-usopen-2108" ~ 0,
      match_id == "2016-ausopen-1222" ~ 0,
      match_id == "2021-usopen-1115" ~ 0,
      match_id == "2022-usopen-2104" ~ 0,
      match_id == "2022-usopen-2104" & point_no == 151 ~ 0, # researched match - https://thedailytexan.com/2022/09/02/former-longhorn-peyton-stearns-makes-us-open-debut/
      match_id == "2023-wimbledon-1127" & point_no < 91 ~ 0,
      match_id == "2023-wimbledon-1140" & point_no != 262 ~ 0,
      .default = delay),
    interruption = case_when(
      game_no == 4 & lag(game_no) == 3 ~ 1,
      game_no == 6 & lag(game_no) == 5 ~ 1,
      game_no == 8 & lag(game_no) == 7 ~ 1,
      game_no == 10 & lag(game_no) == 9 ~ 1,
      game_no == 12 & lag(game_no) == 11 ~ 1,
      game_no == 1 & lag(game_no) > 1 & set_no > 1 ~ 1,
      .default = 0),
    p1_tiebreak_score = if_else(tiebreak == 1, p1_score, "0"),
    p1_tiebreak_score = as.integer(p1_tiebreak_score),
    p2_tiebreak_score = if_else(tiebreak == 1, p2_score, "0"),
    p2_tiebreak_score = as.integer(p2_tiebreak_score),
    change_ends = case_when(
      interruption == 1 ~ 1,
      game_no == 2 & lag(game_no) == 1 ~ 1,
      tiebreak == 1 & (p1_tiebreak_score + p2_tiebreak_score) %% 6 == 0 ~ 1,
      .default = 0),
    tiebreak_victory = factor(case_when(
      tiebreak == 1 & lead(tiebreak) == 0 & point_victor == 1 ~ 1,
      tiebreak == 1 & lead(tiebreak) == 0 & point_victor == 2 ~ 2,
      .default = 0))
    ) %>%
  select(-p1_tiebreak_score, -p2_tiebreak_score) %>%
  
  # replacing 0 distance_run with NA
  mutate(
    p1_distance_run = case_when(
      p1_distance_run == 0 ~ NA,
      .default = p1_distance_run),
    p2_distance_run = case_when(
      p2_distance_run == 0 ~ NA,
      .default = p2_distance_run),
    
    # replace 0 rally_count with NA
    rally_count = case_when(
      rally_count == 0 & p1_double_fault == 0 & p2_double_fault == 0 ~ NA,
      .default = rally_count),
    rally_count = case_when(
      is.na(lag(rally_count)) & is.na(lead(rally_count)) & rally_count == 0 ~ NA,
      .default = rally_count),
    
    # correcting distance run in 2016 US Open
    p1_distance_run = if_else(year == 2016 & tournament == "usopen", p1_distance_run/3.28084, p1_distance_run),
    p2_distance_run = if_else(year == 2016 & tournament == "usopen", p2_distance_run/3.28084, p2_distance_run),
    )

# adding derivatives!
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

wimbledon_derivatives <- gs %>% filter(year == 2023, tournament == "wimbledon") %>%
  derivative_tibble(alpha = .10, order = 1)

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

# For Dr. Sturdivant ------------------------------------------------------

wim_men_final <- gs %>%
  filter(str_detect(match_id, "2023-wimbledon-13") | str_detect(match_id, "2023-wimbledon-14") | str_detect(match_id, "2023-wimbledon-15") | str_detect(match_id, "2023-wimbledon-16") | str_detect(match_id, "2023-wimbledon-17")) %>%
  select(match_id, player1, player2, elapsed_time:point_no, p1_sets:p2_score, server, serve_no, point_victor:set_victor, p1_ace:return_depth)

# wim_men_final %>% glimpse()

# linker <- build_linker(wim_men_final, variable_description = var_desc, variable_type = var_type)

# write_csv(wim_men_final, "wimbledon_featured_matches.csv")

data_dict <- tibble(
  variables = wim_men_final %>% colnames(),
  explanation = c(
    "match identification",
    "first and last name of the first player",
    "first and last name of the second player",
    "time elapsed since first point",
    "set number in match",
    "game number in set",
    "point number in game",
    "sets won by player 1",
    "sets won by player 2",
    "games won by player 1 in current set",
    "games won by player 2 in current set",
    "player 1's score within current game",
    "player 2's score within current game",
    "server of the point",
    "first or second serve",
    "winner of the point",
    "number of points won by player 1 in match",
    "number of points won by player 2 in match",
    "a player won a game this point",
    "a player won a set this point",
    "player 1 hit an untouchable winning serve",
    "player 2 hit an untouchable winning serve",
    "player 1 hit an untouchable winning shot",
    "player 2 hit an untouchable winning shot",
    "category of untouchable shot",
    "player 1 missed both serves and lost the point",
    "player 2 missed both serves and lost the point",
    "player 1 made an unforced error",
    "player 2 made an unforced error",
    "player 1 made it to the net",
    "player 2 made it to the net",
    "player 1 won the point while at the net",
    "player 2 won the point while at the net",
    "player 1 has an opportunity to win a game player 2 is serving",
    "player 2 has an opportunity to win a game player 1 is serving",
    "player 1 won the game player 2 is serving",
    "player 2 won the game player 1 is serving",
    "player 1 missed an opportunity to win a game player 2 is serving",
    "player 2 missed an opportunity to win a game player 1 is serving",
    "player 1's distance ran in meters during point",
    "player 2's distance ran in meters during point",
    "number of shots during the point",
    "speed of serve in mph",
    "direction of serve",
    "depth of serve",
    "depth of return"),
  example = c(
    "2023-wimbledon-1701",
    "Carlos Alcaraz",
    "Novak Djokovic",
    "00:10:27",
    "1, 2, 3, 4, or 5",
    "1, 2, ..., 13",
    "1, 2, 3... etc.",
    "0, 1, or 2",
    "0, 1, or 2",
    "0, 1,...,6",
    "0, 1,...,6",
    "0 (love), 15, 30, 40, AD (advantage)",
    "0 (love), 15, 30, 40, AD (advantage)",
    "1: player 1, 2: player 2",
    "1: first serve, 2: second serve",
    "1 if player 1 wins, 0 if player 2 wins",
    "0, 1, 2... etc.",
    "0, 1, 2... etc.",
    "0: no one, 1: player 1, 2: player 2",
    "0: no one, 1: player 1, 2: player 2",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "F: Forehand, B: Backhand",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "0 or 1",
    "5.376, 21.384, etc.",
    "6.485, 12.473, etc.",
    "1, 2, 4, etc. (includes serve)",
    "81, 124, etc.",
    "B: Body, BC: Body/Center, BW: Body/Wide, C: Center, W: Wide",
    "CTL: Close To Line, NCTL: Not Close To Line",
    "D: Deep, ND: Not Deep"))

# write_csv(data_dict, "data_dictionary.csv")
  
  
  
  
  
