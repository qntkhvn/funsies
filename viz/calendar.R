library(tidyverse)
library(lubridate)

plot_calendar <- function(year) {
  
  first <- str_c(year, "-01-01")
  last <- str_c(year, "-12-31")
  
  p <- seq(as.Date(first), as.Date(last), 1) |>
    stringi::stri_datetime_fields() |>
    ggplot(aes(DayOfWeek, WeekOfMonth)) +
    geom_tile(fill = "white", color = "gray") +
    scale_y_reverse(breaks = NULL) +
    scale_x_continuous(breaks = 1:7, labels = wday(1:7, label = TRUE)) +
    geom_text(aes(label = Day)) +
    facet_wrap(~ month(Month, label = TRUE), scales = "free") +
    theme_light() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank())
  
  return(p)
}

plot_calendar(2021)