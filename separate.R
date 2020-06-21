library(tidyverse)
library(dslabs)

# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data using the unite function
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

# Read Marathon times.csv
d <- read_csv("times.csv")
marathon <- d %>% gather(year, time, -age_group)
head(marathon)
# converting back
marathon %>% spread(year, time)

# Read Diseases.txt
d <- read_delim("Diseases.txt", delim = " ")
d

#Read Diseases.csv
d <- read_csv("Diseases.csv")
d
tidy <- d %>% gather(key = disease, value = count, -state, -year, -population)
head(tidy)

# Read murders_tot
d <- read_csv("murders_tot.csv")
d
tidy <- d %>% spread(key = var, value = people)
tidy

# Read 2015-2016 races
d <- read_csv("2015-2016 races.csv")
d
gath <- d %>% gather(key = "key", value = "value", -age_group)
head(gath)
sep <- gath %>% separate(col = key, into = c("year", "variable_name"), sep = "_")
head(sep)
tidy <- sep %>% spread(key = variable_name, value = value)
head(tidy)

# Read basket stats
stats <- read_csv("basket.csv")
stats
tidy <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)
tidy

# Read CO2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)

co2_tidy <- co2_wide %>% gather(key = month, value = co2, -year)
head(co2_tidy)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# Admissions
data(admissions)
head(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

# to get three columns major, men, women
dat_tidy <- dat %>% spread(key = gender, value = admitted)
head(dat_tidy)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- tmp %>% unite(col = column_name, c(key, gender), sep = "_")
tmp2  

# Reshape tmp2 to a table with six rows and five columns named major, admitted_men, 
# admitted_women, applicants_men and applicants_women

tmp3 <- tmp2 %>% spread(key = column_name, value = value)
tmp3

# Simplest case where column names are character data
head(relig_income)
tidy <- relig_income %>%
  pivot_longer(-religion, names_to = "income", values_to = "count")
head(tidy)

# Slightly more complex case where columns have common prefix,
# and missings are structural so should be dropped.
head(billboard)
tidy <- billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )
head(tidy)

# Multiple variables stored in column names
head(who)
tidy <- who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)
head(tidy)

# Multiple observations per row
head(anscombe)
tidy <- anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )
head(tidy)

# Example with pivot_wider
head(fish_encounters)
tidy <- fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)
head(tidy)
# Fill in missing values
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)

# Generate column names from multiple variables
us_rent_income
us_rent_income %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

# When there are multiple `names_from` or `values_from`, you can use
# use `names_sep` or `names_glue` to control the output variable names
us_rent_income %>%
  pivot_wider(
    names_from = variable,
    names_sep = ".",
    values_from = c(estimate, moe)
  )
us_rent_income %>%
  pivot_wider(
    names_from = variable,
    names_glue = "{variable}_{.value}",
    values_from = c(estimate, moe)
  )

# Can perform aggregation with values_fn
warpbreaks <- as_tibble(warpbreaks[c("wool", "tension", "breaks")])
warpbreaks
warpbreaks %>%
  pivot_wider(
    names_from = wool,
    values_from = breaks,
    values_fn = mean
  )



