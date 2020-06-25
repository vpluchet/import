library(tidyverse)
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
head(brexit_polls)

brexit_polls %>% filter(month(startdate) == 4) %>% nrow()
sum(month(brexit_polls$startdate) == 4) #equivalent

end <- round_date(ymd("2016-06-12"), unit = "week")
end
brexit_polls %>% filter(round_date(enddate, unit = "week") == end) %>% nrow()
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12") #equivalent

brexit_polls %>% mutate(day = weekdays(enddate)) %>% count(day) %>%
  arrange(desc(n))
table(weekdays(brexit_polls$enddate))

data(movielens)
head(movielens)
movies <- movielens %>% mutate(timestamp = as_datetime(timestamp)) %>% 
  mutate(rev_year = year(timestamp), rev_h = hour(timestamp))
head(movies, n=10L)

# Year and hour arranged by counts
movies %>% count(rev_year) %>% arrange(desc(n))
movies %>% count(rev_h) %>% arrange(desc(n))

# Alternative code
dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
names(which.max(reviews_by_year))    # name of year with most reviews

reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews


library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

head(gutenberg_metadata)

pat_pride <- ".*[pP]ride.*[pP]rejudice.*"
ind <- str_detect(gutenberg_metadata$title, pattern = pat_pride)
head(ind)
gutenberg_metadata %>% filter(ind)

# Alternative code
gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

# Finding the original version
gutenberg_works(title == "Pride and Prejudice", languages = "en")
gutenberg_works(title == "Pride and Prejudice")$gutenberg_id
gutenberg_works(title == "Pride and Prejudice") %>% .$gutenberg_id

pride <- gutenberg_download(1342)
words <- unnest_tokens(tbl = pride, output = word, input = text) %>%
  select(word)
head(words, n=20L)
nrow(words)

head(stop_words)

words_filt <- words %>% filter(!word %in% stop_words$word)
nrow(words_filt)
# alternative code
nrow(words %>% anti_join(stop_words))

words_tidy <- words_filt %>% filter(!str_detect(word, "\\d+"))
nrow(words_tidy)

q1 <- words_tidy %>% count(word) %>% filter(n > 100)
nrow(q1)
q1 %>% arrange(desc(n))

words_tidy %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(word)

words_tidy %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(n)

afinn <- get_sentiments("afinn")
str(afinn)
afinn_sentiments <- words_tidy %>% inner_join(afinn)
nrow(afinn_sentiments)

m <- matrix(1:4, 2)
m
proportions(m, margin = NULL)
proportions(m, margin = 1)
proportions(m, margin = 2)
proportions(m, margin = c(1,2))

t <- afinn_sentiments %>% mutate(test = case_when(
  value > 0 ~"positive",
  value == 0 ~"neutral",
  value < 0 ~ "negative"
))
proportions(table(t$test))
mean(afinn_sentiments$value > 0)

sum(afinn_sentiments$value == 4)

