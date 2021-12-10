

words <- c(
  "deets",
  "kids",
  "children",
  "heart",
  "depends",
  "phd",
  "bottleservice",
  "brofessor",
  "tenured",
  "quang",
  "base R",
  "sorta like",
  "interesting",
  "zoomies",
  "mtcars",
  "dat",
  "steve",
  "a <-",
  "fred",
  "greg",
  "perry",
  "666666",
  "stats boy",
  "stitch fix",
  "drip",
  "meme",
  "rap",
  "improv",
  "data art",
  "nft",
  "mess",
  "twitch",
  "twitter",
  "menaretrash",
  "dad jokes",
  "chitty chat",
  "wife",
  "paper",
  "research",
  "life",
  "scam",
  "adequately",
  "baby bear",
  "unmute",
  "late",
  "boring",
  "connected",
  "hydrated",
  "set.seed(1234)",
  "uconn",
  "wpi",
  "413",
  "wrestling",
  "nuts"
)


set.seed(66666666666)
freq <- ceiling(100 * runif(length(words)))


library(wordcloud)

wordcloud(
  words = words,
  freq = freq,
  max.words = length(freq),
  random.order = FALSE,
  colors = sample(colors(), length(words)),
  min.freq = 1
)
