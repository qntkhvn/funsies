library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/wiki/2021_NCAA_Division_I_FBS_football_rankings"

raw_tbl <- url %>% 
  read_html() %>% 
  html_element(xpath = '//*[@id="mw-content-text"]/div[1]/div[2]/table') %>% 
  html_table()

names(raw_tbl) <- c("Rank", str_c("Week", 0:15), "Rank2")

cleaned_tbl <- raw_tbl %>% 
  filter(!is.na(Rank)) %>% 
  select(-Week15, -Rank2) %>% 
  pivot_longer(!Rank,
               names_to = "Week",
               names_prefix = "Week",
               names_transform = list("Week" = as.double),
               values_to = "School") %>% 
  mutate(School = if_else(Week == 0, str_c(School, " (0-0)"), School),
         School = str_sub(School, 1, str_locate(School, "\\(\\d")[, "start"], - 2))

final25 <- cleaned_tbl %>% 
  filter(Week == 14)

cfp <- cleaned_tbl %>% 
  filter(School %in% final25$School)

final_tbl <- cfp %>% 
  full_join(expand(cfp, Week, School)) %>% 
  mutate(Rank = if_else(is.na(Rank), 26, Rank),
         School = fct_rev(factor(School, levels = final25$School)))

final_tbl %>% 
  ggplot(aes(Week, School, fill = Rank)) +
  geom_tile(color = "white") +
  scale_x_continuous(breaks = 0:14, 
                     position = "top") +
  scale_fill_gradient2(high = "white", 
                       low = "midnightblue", 
                       mid = "salmon", 
                       midpoint = 13) +
  labs(x = NULL,
       y = NULL,
       title = "\n2021 AP Top 25 Weekly College Football Poll",
       subtitle = "(For Top 25 Teams At the End of Regular Season)\n") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(margin = margin(0, -12, 0, 0)))
