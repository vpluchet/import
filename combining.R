# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

tab %>% mutate(rate = total / population * 10^5, trump_lead = trump > clinton) %>%
  filter(!(abb %in%"DC")) %>%
  arrange(rate) %>%
  ggplot(aes(y = rate, x = trump, label = abb, color = trump_lead)) +
  geom_point() +
  geom_text_repel()

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
full_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

# Binding
bind_cols(a = 1:3, b = 4:6)

tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
str(tab)


tab1 <- tab[, 1:3]
head(tab1)
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab1
tab2 <- tab[3:4,]
tab2
bind_rows(tab1, tab2)

