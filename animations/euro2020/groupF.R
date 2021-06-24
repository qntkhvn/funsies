library(tidyverse)
library(gganimate)
library(countrycode)
library(ggflags)
theme_set(theme_minimal())

f <- tribble(
  ~Frame, ~Team, ~Points, ~Rank, ~GD,
  "f0", "France", 4, 1, 1,
  "f0", "Germany", 3, 2, 1,
  "f0", "Portugal", 3, 3, 1,
  "f0", "Hungary", 1, 4, -3,
  
  "f1", "France", 5, 1, 1,
  "f1", "Germany", 4, 2, 1,
  "f1", "Portugal", 4, 3, 1,
  "f1", "Hungary", 2, 4, -3,
  
  "f2", "France", 5, 1, 1,
  "f2", "Germany", 3, 4, 0,
  "f2", "Portugal", 4, 2, 1,
  "f2", "Hungary", 4, 3, -2,
  
  "f3", "France", 4, 2, 0,
  "f3", "Germany", 3, 4, 0,
  "f3", "Portugal", 6, 1, 2,
  "f3", "Hungary", 4, 3, -2,
  
  "f4", "France", 5, 1, 1,
  "f4", "Germany", 3, 4, 0,
  "f4", "Portugal", 4, 2, 1,
  "f4", "Hungary", 4, 3, -2,
  
  "f5", "France", 7, 1, 2,
  "f5", "Germany", 3, 3, 0,
  "f5", "Portugal", 3, 4, 0,
  "f5", "Hungary", 4, 2, -2,
  
  "f6", "France", 5, 1, 1,
  "f6", "Germany", 3, 4, 0,
  "f6", "Portugal", 4, 2, 1,
  "f6", "Hungary", 4, 3, -2,
  
  "f7", "France", 5, 1, 1,
  "f7", "Germany", 4, 2, 1,
  "f7", "Portugal", 4, 3, 1,
  "f7", "Hungary", 2, 4, -3,
  
  "f8", "France", 5, 1, 1,
  "f8", "Germany", 3, 4, 0,
  "f8", "Portugal", 4, 2, 1,
  "f8", "Hungary", 4, 3, -2,
  
  "f9", "France", 5, 1, 1,
  "f9", "Germany", 4, 2, 1,
  "f9", "Portugal", 4, 3, 1,
  "f9", "Hungary", 2, 4, -3,
  
  "f10", "France", 5, 1, 1,
  "f10", "Germany", 4, 2, 1,
  "f10", "Portugal", 4, 3, 1,
  "f10", "Hungary", 2, 4, -3
)

a <- f %>% 
  mutate(
    Frame = case_when(
      Frame == "f0" ~ "POR vs. FRA\nGER vs. HUN\n---\nAfter Matchday 2\n",
      Frame == "f1" ~ "POR 0-0 FRA\nGER 0-0 HUN\n---\nKickoff\n",
      Frame == "f2" ~ "POR 0-0 FRA\nGER 0-1 HUN\n---\n(HUN) Szalai 11'\n",
      Frame == "f3" ~ "POR 1-0 FRA\nGER 0-1 HUN\n---\n(POR) Ronaldo 30' (P)\n",
      Frame == "f4" ~ "POR 1-1 FRA\nGER 0-1 HUN\n---\n(FRA) Benzema 45+2' (P)\n",
      Frame == "f5" ~ "POR 1-2 FRA\nGER 0-1 HUN\n---\n(FRA) Benzema 47'\n",
      Frame == "f6" ~ "POR 2-2 FRA\nGER 0-1 HUN\n---\n(POR) Ronaldo 60' (P)\n",
      Frame == "f7" ~ "POR 2-2 FRA\nGER 1-1 HUN\n---\n(GER) Havertz 66'\n",
      Frame == "f8" ~ "POR 2-2 FRA\nGER 1-2 HUN\n---\n(HUN) Schafer 68'\n",
      Frame == "f9" ~ "POR 2-2 FRA\nGER 2-2 HUN\n---\n(GER) Goretzka 84'\n",
      Frame == "f10" ~ "POR 2-2 FRA\nGER 2-2 HUN\n---\nFull-time. End of Group Stage\n",
      TRUE ~ as.character(Frame)
    ),
    Frame = factor(Frame, levels = c(
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
    )),
    Code = str_to_lower(countrycode(Team, "country.name", "iso2c"))
  ) %>% 
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


  