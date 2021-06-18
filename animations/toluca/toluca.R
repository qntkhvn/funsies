library(tidyverse)
library(gganimate)
library(broom)
library(ALSM)

y <- TolucaCompany$y
x <- TolucaCompany$x

a <- tibble(span = seq(0.2, 1.5, by = 0.05)) %>%
  group_by(span) %>%
  do(augment(loess(y ~ x, degree = 2, span = .$span))) %>% 
  ggplot(aes(x, y)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = .fitted), color = "chocolate", size = 2) +
  theme_bw() +
  transition_states(span) +
  ggtitle("25 points tolucat \n Span = {closest_state}")

animate(a, height = 450, width = 500, fps = 10)
