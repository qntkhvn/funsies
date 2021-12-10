
library(tidyverse)
library(rtweet)
library(tidytext)
library(wordcloud)
library(reshape2)
theme_set(theme_light())

greg_tweets <- get_timeline(user = "statsinthewild", n = 5000)

words <- greg_tweets %>%
  select(text) %>%
  mutate(text = str_replace_all(text, "@[A-Za-z0-9_]+", "")) %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!(word %in% c("https", "t.co", as.character(1:9))))

words %>% 
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique words found in @statsinthewild's last 5000 tweets/replies")

words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(sentiment = if_else(word == "trump", "negative", sentiment)) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("red", "brown"),
    max.words = 200,
    title.size = 1.5
  )
