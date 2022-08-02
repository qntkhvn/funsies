library(tidyverse)
library(rvest)

e <- read_lines("https://apod.nasa.gov/htmltest/gifcity/e.1mil") |> 
  str_c(collapse = "") |> 
  str_remove_all("\\.")
e <- str_sub(e, str_locate(e, "e\\s=\\s\\s")[2] + 1, str_locate(e, "e\\s=\\s\\s")[2] + 1e6)

str_length(e)

e_digits <- tibble(digit = e) |> 
  mutate(digit = str_split(digit, "")) |> 
  unnest_longer(digit) |> 
  mutate(row_index = rep(1:1000, each = 1000),
         col_index = rep(1:1000, 1000))

e_digits |>
  ggplot(aes(row_index, col_index, fill = factor(digit))) +
  geom_tile(show.legend = FALSE) +
  scale_fill_manual(values = rainbow(10)) +
  scale_y_reverse() + 
  coord_equal() +
  theme_void()
