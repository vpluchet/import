library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)

txt <- pdf_text(fn)
str(txt)
head(txt)
txt[9]
x <- str_split(txt[9], "\n")
class(x)
length(x)
nrow(x)

s <- x[[1]]
class(s)
length(s)

s<- str_trim(s)
s[1] # print string, visually inspect last character

header_index <- str_which(s, "2015")[1]
header_index
header <- s[header_index]
header

tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
month
class(header)
header


tail_index <- str_which(s, "Total")
tail_index

n <- str_count(s, "\\d+")
sum(n == 1)

# Removing undesired rows. The entry header_index and everything before 
# it should be removed. Entries for which n is 1 should also be removed,
# and the entry tail_index and everything that comes after it should be removed as well.
out <- c(1:header_index, which(n==1), tail_index:length(s))
out
s <- s[-out]
length(s)

# Now we are ready to remove all text that is not a digit or space
s <- str_remove_all(s, "[^\\d\\s]")
s

# Removing the figures on the right coming from the graph
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s

# cbind function
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
m


col_names <- c("day", header)
col_names
class(s)
colnames(s) <- col_names
head(s)
tab <- s %>% cbind(month = 9)
head(tab)
tabn <- matrix(parse_number(tab), ncol = 6, nrow = nrow(tab))
colnames(tabn) <- c(col_names, "month")
head(tabn)
nrow(tabn)

mean(tabn[,2])
mean(tabn[,3])
mean(tabn[1:19,4])
mean(tabn[20:30,4])

tab <- s %>% 
  as_tibble() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)
mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2017"[20:30])

# Making tab a tidy data frame
tab
tab %>% pivot_longer(cols = 2:5, names_to = "year", values_to = "deaths")

# Alternative code 
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))

library(plotly) # to make interactive graphs

p <- tab %>% filter(!year == 2018) %>% ggplot(aes(x = day, y = deaths, color = year)) +
  geom_line() +
  geom_vline(aes(xintercept = 20), color = "purple") +
  geom_text(aes(x = 13, y = 130, label = "Hurricane Maria 2017-9-20 ->"), color = "purple")

ggplotly(p)

