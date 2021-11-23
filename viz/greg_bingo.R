library(tidyverse)

greg <- c("It depends",
          "You always try to do\nwhat you're trying to do",
          "Takes shot at\ncomputer science boy",
          "Forgets to unmute\non Zoom",
          "Joins Zoom late",
          "Drinks water and\nsays gotta stay hydrated",
          "Makes a base R plot",
          "It's all connected",
          "Yawns",
          "Mentions loss function",
          "What time does\nthis class end",
          "Talks about his\ngrad school days",
          "Says deets\ninstead of details",
          "I don't just teach stats,\nI teach life",
          "Mentions Quang",
          "Answers scam call",
          "Mentions Perry",
          "The baby bear",
          "Wears a blazer",
          "Wears a hat",
          "Have I adequately\naddressed your question?",
          "I have 3 kids",
          "The answer is\ncross-validation",
          "It's in your heart",
          "Picks on Akhil",
          "Mentions stitch fix",
          "Bottle service",
          "It's sorta like",
          "Is that what\nthe kids say",
          "Somebody look it up",
          "Am I boring you?")

set.seed(1)
tibble(what = sample(greg, 25),
       row = rep(1:5, 5),
       col = rep(1:5, each = 5)) %>% 
  mutate(what = ifelse(row == 3 & col == 3, 
                       "Free space\nHas 10+ tabs opened", what)) %>% 
  ggplot(aes(row, col)) +
  geom_tile(color = "black", fill = "white") +
  geom_text(aes(label = what)) +
  theme_void()
