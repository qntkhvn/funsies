library(tidyverse)
library(viridis)

Team <- c(
  "England", "Netherlands", "Scotland", "Switzerland",
  "France", "Spain", "Bulgaria", "Romania",
  "Germany", "CzechRepublic", "Italy", "Russia",
  "Portugal", "Croatia", "Denmark", "Turkey",
  
  "Portugal", "Romania", "England", "Germany",
  "Italy", "Turkey", "Belgium", "Sweden",
  "Spain", "Yugoslavia", "Norway", "Slovenia",
  "Netherlands", "France", "CzechRepublic", "Denmark",
  
  "Portugal", "Greece", "Spain", "Russia",
  "France", "England", "Croatia", "Switzerland",
  "Sweden", "Denmark", "Italy", "Bulgaria",
  "CzechRepublic", "Netherlands", "Germany", "Latvia",
  
  "Portugal", "Turkey", "CzechRepublic", "Switzerland",
  "Croatia", "Germany", "Austria", "Poland",
  "Netherlands", "Italy", "Romania", "France",
  "Spain", "Russia", "Sweden", "Greece",
  
  "CzechRepublic", "Greece", "Russia", "Poland",
  "Germany", "Portugal", "Denmark", "Netherlands",
  "Spain", "Italy", "Croatia", "Ireland",
  "England", "France", "Ukraine", "Sweden",
  
  "France", "Switzerland", "Albania", "Romania",
  "Wales", "England", "Slovakia", "Russia",
  "Germany", "Poland", "NorthernIreland", "Ukraine",
  "Croatia", "Spain", "Turkey", "CzechRepublic",
  "Italy", "Belgium", "Ireland", "Sweden",
  "Hungary", "Iceland", "Portugal", "Austria"
)

Outcome <- c(
  "SF", "QF", "G", "G", "SF", "QF", "G", "G", "W", "F", "G", "G", "QF",
  "QF", "G", "G", "SF", "QF", "G", "G", "F", "QF", "G", "G", "QF", "QF",
  "G", "G", "SF", "W", "G", "G", "F", "W", "G", "G", "QF", "QF", "G",
  "G", "QF", "QF", "G", "G", "SF", "SF", "G", "G", "QF", "SF", "G", "G",
  "QF", "F", "G", "G", "QF", "QF", "G", "G", "W", "SF", "G", "G", "QF",
  "QF", "G", "G", "SF", "SF", "G", "G", "W", "F", "G", "G", "QF", "QF",
  "G", "G", "F", "R16", "G", "G", "SF", "R16", "R16", "G", "SF", "QF", "R16",
  "G", "R16", "R16", "G", "G", "QF", "QF", "R16", "G", "R16", "QF", "W", "G"
)


euro <- tibble(
  Year = c(rep(seq(1996, 2012, 4), each = 16), rep(2016, 24)),
  Team,
  Group = c(rep(rep(c("A", "B", "C", "D"), each = 4), 6), 
            rep(c("E", "F"), each = 4)),
  Place = rep(1:4, 26),
  Outcome
) %>% 
  mutate(Outcome = factor(Outcome, 
                          levels = c("G", "R16", "QF", "SF", "F", "W")))
euro %>% 
  mutate(Group = fct_rev(Group)) %>% 
  ggplot(aes(x = Place, y = Group, color = Outcome)) +
  geom_point(shape = 16, size = 8) +
  facet_wrap(~ Year, ncol = 2, scales = "free") +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  labs(
    title = "Euro Performances and Group Positions"
  ) +
  theme(
    axis.title = element_blank(),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#00c1d5", color = "#00c1d5"),
    plot.title = element_text(hjust = 0.5),
  )
