library(tidyverse)
library(chessR)
library(gganimate)

# get data via {chessR}
greg <- get_game_data("fatgreggy")

# blitz after 4/15/22
greg %>%
  filter(time_class == "blitz" & Date > as.Date("2022-04-15")) %>%
  group_by(Date) %>%
  summarise(highest_elo = max(UserELO)) %>%
  ggplot(aes(Date, highest_elo)) +
  geom_line()

# blitz over the years
greg %>%
  filter(time_class == "blitz") %>%
  group_by(Date) %>%
  summarise(highest_elo = max(UserELO)) %>%
  ggplot(aes(Date, highest_elo)) +
  geom_line() +
  transition_reveal(Date)

# blitz/bullet/rapid/daily over the years
greg %>% 
  group_by(Date, time_class) %>% 
  summarise(highest_elo = max(UserELO)) %>% 
  ggplot(aes(Date, highest_elo, color = time_class)) +
  geom_line() +
  scale_color_manual(values= c("red", "midnightblue", "green", "darkgray"))
