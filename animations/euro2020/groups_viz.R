library(tidyverse)
library(gganimate)
theme_set(theme_minimal())

euro_groups <- read_csv("https://raw.githubusercontent.com/qntkhvn/funsies/main/animations/euro2020/euro_groups.csv")

colors <- c("#ED2939", "#eb192b", "gray22", "#ed1b2c",
            "#e32219","#00007e", "#002F6C", "#052789",
            "#231f20", "#0b663a", "#0A36AF", "#FF4F00",
            "#D20000", "#DC143C", "#e42518", "#e40303",
            "#05005b", "#034da3", "#880f14", "#ffec00",
            "#e4000f", "#E30A17", "#ffe000", "#e11a22")

a <- euro_groups %>% 
  ggplot() +
  geom_col(aes(x = Rank, y = Points, group = Team, fill = Team), 
           show.legend = FALSE) +
  geom_text(aes(x = Rank, y = 0, label = paste(Team, " ", sep = " "),
                group = Team), 
            hjust = 1, size = 3, vjust = 0.35) +
  geom_text(aes(x = Rank, y = Points, label = as.character(Points)), 
            hjust = -0.3, size = 3) +
  facet_wrap(~ Group, scales = "free", ncol = 2) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  ylim(c(0, 9.5)) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 2.7, "cm"),
    panel.spacing.x = unit(5.5, "lines"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#00c1d5", color = "#00c1d5"),
    plot.title = element_text(size = 10)
  ) +
  scale_fill_manual(values = colors) +
  transition_states(Date, transition_length = 2, state_length = 1, wrap = FALSE) +
  ggtitle("UEFA Euro 2020 Group Standings\nAs of {closest_state}")

animate(a, nframes = 500, fps = 50, height = 480, width = 600, res = 95, end_pause = 200, duration = 12)
