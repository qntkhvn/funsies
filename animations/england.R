library(tidyverse)
library(rvest)
library(janitor)
library(gganimate)

eng_wiki <- "https://en.wikipedia.org/wiki/England_national_football_team"
eng_html <- read_html(eng_wiki)
eng_tables <- html_table(eng_html)

rounds <- c("Absent", "Group stage", "Round of 16", "Quarterfinal", "Semifinal", "Champions")
wc_years <- c("1930", "1934", "1938", seq(1950, 2018, 4))
euro_years <- as.character(seq(1960, 2016, 4))
years <- sort(c(wc_years, euro_years))

eng_wc <- eng_tables[[37]] %>% 
  row_to_names(row_number = 1) %>% 
  select(Year, Round) %>% 
  filter(Year %in% wc_years) %>% 
  mutate(Round = if_else(str_detect(Round, "(?i)not") == TRUE, "Absent", Round),
         Round = str_remove(Round, "-"),
         Round = str_replace(Round, "finals", "final"),
         Round = if_else(Round %in% c("Third place", "Fourth place"), "Semifinal",
                         if_else(Round == "Second group stage", "Quarterfinal", Round)),
         Round = factor(Round, levels = rounds)) %>% 
  add_count(Year, Round, name = "Count") %>% 
  mutate(Year = factor(Year, levels = years))

eng_wc <- full_join(eng_wc, expand(eng_wc, Year, Round)) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count)) %>% 
  arrange(Year, Round) %>% 
  group_by(Round) %>% 
  mutate(Count = cumsum(Count),
         Tournament = "World Cup")

eng_euro <- eng_tables[[39]]  %>% 
  row_to_names(row_number = 1) %>% 
  select(Year, Round) %>% 
  filter(Year %in% euro_years) %>% 
  mutate(Round = if_else(str_detect(Round, "(?i)not") == TRUE, "Absent", Round),
         Round = str_remove(Round, "-"),
         Round = str_replace(Round, "finals", "final"),
         Round = if_else(Round %in% c("Third place", "Fourth place"), "Semifinal",
                         if_else(Round == "Second group stage", "Quarterfinal", Round)),
         Round = factor(Round, levels = rounds)) %>% 
  add_count(Year, Round, name = "Count") %>% 
  mutate(Year = factor(Year, levels = years))

eng_euro <- full_join(eng_euro, expand(eng_euro, Year, Round)) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count)) %>% 
  arrange(Year, Round) %>% 
  group_by(Round) %>% 
  mutate(Count = cumsum(Count),
         Tournament = "Euro")

eng_results <- eng_wc %>% 
  bind_rows(eng_euro) %>% 
  arrange(Year) %>% 
  mutate(Year = if_else(Year %in% wc_years,
                        paste(Year, "World Cup"),
                        paste(Year, "Euro")))

a <- eng_results %>% 
  ggplot(aes(x = Count, y = Round, fill = Tournament)) + 
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = as.character(Count)), size = 3, hjust = -0.3) + 
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~ Tournament) +
  scale_fill_manual(values = c("#00c1d5", "gold")) + 
  theme(strip.background = element_rect(fill = "#00007e"),
        panel.grid.minor.x = element_blank(),
        plot.caption=element_text(hjust = 0, face = "italic")) +
  transition_states(Year, wrap = FALSE) +
  labs(caption = "\n*Absent: Did not qualify/ Did not enter qualification/ Not a FIFA member at the time",
       title = "England's Performance \n {closest_state}")

animate(a, nframes = 432, fps = 24, height = 500, width = 700, res = 100, end_pause = 60)
