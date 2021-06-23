library(tidyverse)

groupA <- c("Turkey", "Italy", "Wales", "Switzerland")
groupB <- c("Denmark", "Finland", "Belgium", "Russia")
groupC <- c("Netherlands", "Ukraine", "Austria", "North Macedonia")
groupD <- c("England", "Croatia", "Scotland", "Czech Republic")
groupE <- c("Spain", "Sweden", "Poland", "Slovakia")
groupF <- c("Hungary", "Portugal", "France", "Germany")

teams <- c(groupA, groupB, groupC, groupD, groupE, groupF)

groups <- tibble(groupA, groupB, groupC, groupD, groupE, groupF) %>% 
  pivot_longer(
    cols = starts_with("group"),
    names_to = "Group",
    names_prefix = "group",
    values_to = "Team"
  )

matches <- tribble(
  ~Date, ~Match, ~Score,
  "11 June 2021", "Turkey-Italy", "0-3",
  "12 June 2021", "Wales-Switzerland", "1-1",
  "12 June 2021", "Denmark-Finland", "0-1",
  "12 June 2021", "Belgium-Russia", "3-0",
  "13 June 2021", "England-Croatia", "1-0",
  "13 June 2021", "Austria-North Macedonia", "3-1",
  "13 June 2021", "Netherlands-Ukraine", "3-2",
  "14 June 2021", "Scotland-Czech Republic", "0-2",
  "14 June 2021", "Poland-Slovakia", "1-2",
  "14 June 2021", "Spain-Sweden", "0-0",
  "15 June 2021", "Hungary-Portugal", "0-3",
  "15 June 2021", "France-Germany", "1-0",
  "16 June 2021", "Finland-Russia", "0-1",
  "16 June 2021", "Turkey-Wales", "0-2",
  "16 June 2021", "Italy-Switzerland", "3-0",
  "17 June 2021", "Ukraine-North Macedonia", "2-1",
  "17 June 2021", "Denmark-Belgium", "1-2",
  "17 June 2021", "Netherlands-Austria", "2-0",
  "18 June 2021", "Sweden-Slovakia", "1-0",
  "18 June 2021", "Croatia-Czech Republic", "1-1",
  "18 June 2021", "England-Scotland", "0-0",
  "19 June 2021", "Hungary-France", "1-1",
  "19 June 2021", "Portugal-Germany", "2-4",
  "19 June 2021", "Spain-Poland", "1-1",
  "20 June 2021", "Italy-Wales", "1-0",
  "20 June 2021", "Switzerland-Turkey", "3-1",
  "21 June 2021", "North Macedonia-Netherlands", "0-3",
  "21 June 2021", "Ukraine-Austria", "0-1",
  "21 June 2021", "Russia-Denmark", "1-4",
  "21 June 2021", "Finland-Belgium", "0-2",
  "22 June 2021", "Czech Republic-England", "0-1",
  "22 June 2021", "Croatia-Scotland", "3-1",
  "23 June 2021", "Slovakia-Spain", "0-5",
  "23 June 2021", "Sweden-Poland", "3-2",
  "23 June 2021", "Germany-Hungary", "2-2",
  "23 June 2021", "Portugal-France", "2-2"
)     

# for factor reordering
dates <- c("10 June 2021", unique(matches$Date))

euro_groups <- matches %>% 
  separate(Match, into = c("t1", "t2"), sep = "-") %>% 
  separate(Score, into = c("s1", "s2"), sep = "-") %>% 
  mutate(s1 = as.numeric(s1), 
         s2 = as.numeric(s2),
         p1 = if_else(s1 > s2, 3,
                           if_else(s1 == s2, 1, 0)),
         p2 = if_else(p1 == 3, 0,
                           if_else(p1 == 1, 1, 3)),
         d1 = s1 - s2,
         d2 = s2 - s1) %>% 
  pivot_longer(
    !Date,
    names_to = c(".value", "obs"),
    names_pattern = "(.)(.)"
    ) %>% 
  select(Date, Team = t, Score = s, Points = p, GD = d) %>% 
  mutate(Date = factor(Date, levels = dates),
         Team = factor(Team, levels = teams))

euro_groups <- full_join(euro_groups, expand(euro_groups, Date, Team)) %>% 
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  arrange(Date) %>% 
  group_by(Team) %>%
  mutate(Points = cumsum(Points),
         GD = cumsum(GD)) %>% 
  left_join(groups, by = "Team") %>% 
  arrange(Date, -Points, -GD) %>% 
  group_by(Date, Group) %>% 
  mutate(Rank = 1:4)

# euro_groups %>% write_csv("euro_groups.csv")
