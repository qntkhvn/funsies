library(tidyverse)
library(rtweet)
library(tidytext)
library(wordcloud)
library(reshape2)
theme_set(theme_light())

euro_tweets <- search_tweets(
  q = "#engsco",
  n = 10000,
  lang = "en",
  include_rts = FALSE
)

word_counts <- euro_tweets %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!(word %in% c("https", "t.co"))) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", y = "")

word_counts %>% 
  filter(!(word %in% c("fucking", "fuck", "shit"))) %>% # remove curse words
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","brown"),
                   max.words = 200)
