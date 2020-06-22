# import a webpage into R
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab

tab <- tab %>% html_table
class(tab)
head(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

# Guacamole
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

# Creating a function
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

# Payroll
library(rvest)
url <- "http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[1]])
html_text(nodes[[2]])
html_text(nodes[[3]])
html_text(nodes[[4]])
html_text(nodes[[5]])
html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])
html_table(nodes[[5]])

sapply(nodes[1:4], html_table)    # 2, 3, 4 give tables with payroll info

tab1 <- html_table(nodes[[11]])
tab2 <- html_table(nodes[[20]])
tab1_ren <- tab1 %>% select(X2, X3, X4) %>% setNames(c("Team", "Payroll", "Average"))
tab2_ren <- tab2 %>% setNames(c("Team", "Payroll", "Average"))
tab1_f <- tab1_ren[-1,]
tab2_f <- tab2_ren[-1,]
full_join(tab1_f, tab2_f, by=c("Team"))

length(nodes)
tab20 <- html_table(nodes[[20]])
tab21 <- html_table(nodes[[21]])
tab22 <- html_table(nodes[[22]])

# Brexit
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
br <- read_html(url)
nodes_br <- html_nodes(br, "table")
tab <- read_html(url) %>% html_nodes("table")
length(tab)



str(html_table(tab[[1]], fill = TRUE))
str(html_table(tab[[2]], fill = TRUE))
str(html_table(tab[[3]], fill = TRUE))
str(html_table(tab[[4]], fill = TRUE))
str(html_table(tab[[5]], fill = TRUE))
str(html_table(tab[[6]], fill = TRUE))
str(html_table(tab[[7]], fill = TRUE))
str(html_table(tab[[8]], fill = TRUE))
str(html_table(tab[[9]], fill = TRUE))
