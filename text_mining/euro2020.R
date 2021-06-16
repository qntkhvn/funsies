library(tidyverse)
library(rtweet)
library(tidytext)
library(wordcloud)
library(reshape2)
theme_set(theme_light())

euro_tweets <- search_tweets(
  q = "#euro2020",
  n = 10000,
  lang = "en",
  include_rts = FALSE
)

eu_words <- euro_tweets %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!(word %in% c("https", "t.co", "euro2020")))

eu_words %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       subtitle = "Unique words found in 10000 #euro2020 tweets during Wales vs Turkey")

eu_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment,sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue","purple"),
                   max.words = 150)
