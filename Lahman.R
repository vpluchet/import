library(Lahman)

# The Batting data frame contains the offensive statistics for all 
# baseball players over several seasons.
head(Batting)

top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

# Master data frame has demographic information for all players:
Master %>% as_tibble()

top_names <- top %>% left_join(Master, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

# salary data
head(Salaries)

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary %>% arrange(desc(salary))

# Awards players
head(AwardsPlayers)



