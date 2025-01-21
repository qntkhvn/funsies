library(tidyverse)
library(rvest)

# pi ----------------------------------------------------------------------

pi <- read_html("https://www.angio.net/pi/digits/100000.txt") |> 
  html_text() |> 
  str_remove("\\.") 

pi_digit <- tibble(digit = pi) |> 
  mutate(digit = str_split(digit, "")) |> 
  unnest_longer(digit) |> 
  mutate(row_index = rep(1:400, 250),
         col_index = rep(1:250, each = 400))

pi_digit |>
  ggplot(aes(row_index, col_index, fill = factor(digit))) +
  geom_tile() +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", n = 10, type = "continuous")) +
  scale_y_reverse() + 
  guides(fill = guide_legend(nrow = 1, title = NULL)) + 
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")

# e -----------------------------------------------------------------------

e <- read_html("http://boston.conman.org/2004/11/12/e100k.txt") |> 
  html_text() |> 
  str_remove("^[^\n]*\n") |>  # remove first line 
  str_remove_all("\\s")       # remove all whitespace

# append 2 to the string of first 99999 digits
# since these are digits after the decimal
e_digit <- tibble(digit = str_c(2, str_sub(e, 1, 99999))) |> 
  mutate(digit = str_split(digit, "")) |> 
  unnest_longer(digit) |> 
  mutate(row_index = rep(1:400, each = 250),
         col_index = rep(1:250, 400))

e_digit |>
  ggplot(aes(row_index, col_index, fill = factor(digit))) +
  geom_tile() +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", n = 10, type = "continuous")) +
  scale_y_reverse() + 
  guides(fill = guide_legend(nrow = 1, title = NULL))+ 
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

# key ---------------------------------------------------------------------

tibble(x = 0:9,
       v = as.character(MetBrewer::met.brewer("Hiroshige", n = 10, type = "continuous"))) |> 
  ggplot(aes(x, 1)) +
  geom_tile(aes(fill = I(v))) +
  geom_text(aes(label = x), color = "white") +
  coord_equal() +
  theme_void()
