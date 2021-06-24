library(tidyverse)
library(gganimate)
library(countrycode)
library(ggflags)
theme_set(theme_minimal())

events <- c(
  "POR vs. FRA\nGER vs. HUN\n---\nAfter Matchday 2\n",
  "POR 0-0 FRA\nGER 0-0 HUN\n---\nKickoff\n",
  "POR 0-0 FRA\nGER 0-1 HUN\n---\n(HUN) Szalai 11'\n",
  "POR 1-0 FRA\nGER 0-1 HUN\n---\n(POR) Ronaldo 30' (P)\n",
  "POR 1-1 FRA\nGER 0-1 HUN\n---\n(FRA) Benzema 45+2' (P)\n",
  "POR 1-2 FRA\nGER 0-1 HUN\n---\n(FRA) Benzema 47'\n",
  "POR 2-2 FRA\nGER 0-1 HUN\n---\n(POR) Ronaldo 60' (P)\n",
  "POR 2-2 FRA\nGER 1-1 HUN\n---\n(GER) Havertz 66'\n",
  "POR 2-2 FRA\nGER 1-2 HUN\n---\n(HUN) Schafer 68'\n",
  "POR 2-2 FRA\nGER 2-2 HUN\n---\n(GER) Goretzka 84'\n",
  "POR 2-2 FRA\nGER 2-2 HUN\n---\nFull-time. End of Group Stage\n"
)

f <- tribble(
  ~Frame, ~Team, ~Points, ~Rank, ~GD,
  events[1], "France", 4, 1, 1,
  events[1], "Germany", 3, 2, 1,
  events[1], "Portugal", 3, 3, 1,
  events[1], "Hungary", 1, 4, -3,
  
  events[2], "France", 5, 1, 1,
  events[2], "Germany", 4, 2, 1,
  events[2], "Portugal", 4, 3, 1,
  events[2], "Hungary", 2, 4, -3,
  
  events[3], "France", 5, 1, 1,
  events[3], "Germany", 3, 4, 0,
  events[3], "Portugal", 4, 2, 1,
  events[3], "Hungary", 4, 3, -2,
  
  events[4], "France", 4, 2, 0,
  events[4], "Germany", 3, 4, 0,
  events[4], "Portugal", 6, 1, 2,
  events[4], "Hungary", 4, 3, -2,
  
  events[5], "France", 5, 1, 1,
  events[5], "Germany", 3, 4, 0,
  events[5], "Portugal", 4, 2, 1,
  events[5], "Hungary", 4, 3, -2,
  
  events[6], "France", 7, 1, 2,
  events[6], "Germany", 3, 3, 0,
  events[6], "Portugal", 3, 4, 0,
  events[6], "Hungary", 4, 2, -2,
  
  events[7], "France", 5, 1, 1,
  events[7], "Germany", 3, 4, 0,
  events[7], "Portugal", 4, 2, 1,
  events[7], "Hungary", 4, 3, -2,
  
  events[8], "France", 5, 1, 1,
  events[8], "Germany", 4, 2, 1,
  events[8], "Portugal", 4, 3, 1,
  events[8], "Hungary", 2, 4, -3,
  
  events[9], "France", 5, 1, 1,
  events[9], "Germany", 3, 4, 0,
  events[9], "Portugal", 4, 2, 1,
  events[9], "Hungary", 4, 3, -2,
  
  events[10], "France", 5, 1, 1,
  events[10], "Germany", 4, 2, 1,
  events[10], "Portugal", 4, 3, 1,
  events[10], "Hungary", 2, 4, -3,
  
  events[11], "France", 5, 1, 1,
  events[11], "Germany", 4, 2, 1,
  events[11], "Portugal", 4, 3, 1,
  events[11], "Hungary", 2, 4, -3
)

a <- f %>% 
  mutate(Frame = factor(Frame, levels = events),
         Code = str_to_lower(countrycode(Team, "country.name", "iso2c"))) %>% 
  ggplot() +
  geom_col(aes(x = Rank, y = Points, group = Team, fill = Team), 
           show.legend = FALSE, width = 0.5) +
  geom_text(aes(x = Rank, y = 0, label = paste(Team, " ", " ", " ", " ", sep = " "),
                group = Team), 
            hjust = 1, size = 4, vjust = 0.35) +
  geom_text(aes(x = Rank, y = Points, label = as.character(Points)), 
            hjust = -0.3, size = 4) +
  geom_flag(aes(x = Rank, y = -0.3, country = Code), size = 6) +
  coord_flip(clip = "off") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 0:9) + 
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 2.7, "cm"),
    panel.spacing.x = unit(5.5, "lines"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#00c1d5", color = "#00c1d5"),
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("#052789", "#231f20", "#0b663a", "#e42518")) +
  transition_states(Frame, wrap = FALSE) +
  ease_aes("quadratic-in-out") +
  labs(
    title = "UEFA Euro 2020 Group F Standings - Matchday 3",
    subtitle = "{closest_state}"
  )

animate(a, nframes = 500, fps = 50, height = 480, width = 600, res = 95, duration = 15, end_pause = 100)
