library(tidyverse)
library(rStrava)
library(lubridate)

healthygreg <- athl_fun(34707096)

healthygreg |> 
  pluck("34707096") |> 
  pluck("monthly") |> 
  mutate(month = month(month, label = TRUE)) |> 
  ggplot(aes(month, miles, group = 1)) +
  geom_point(aes(size = miles), show.legend = FALSE) +
  geom_line() +
  theme_bw()
