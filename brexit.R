library(rvest)
library(tidyverse)
library(stringr)
library(rvest)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls_raw <- tab[[5]] %>% html_table(fill = TRUE)

# Some rows in this table do not contain polls. You can identify these by
# the lack of the percent sign (%) in the Remain column.
ind <- str_detect(polls_raw$Remain, "%")
sum(ind)
polls_renamed <- polls_raw %>% filter(ind) %>% setNames(
  c("dates", "remain", "leave", "undecided", "lead",
    "samplesize", "pollster", "poll_type", "notes"))

nrow(polls_renamed)

# the following commands convert remain to proportions between 0-1
as.numeric(str_replace(polls_renamed$remain, "%", ""))/100
parse_number(polls_renamed$remain)/100

str_replace(polls_renamed$undecided, "N/A", "0%")

# The dates column contains the range of dates over which the poll was conducted.
# The format is "8-10 Jan" where the poll had a start date of 2016-01-08 and end
# date of 2016-01-10. Some polls go across month boundaries (16 May-12 June).
# The end date of the poll will always be one or two digits, followed by a space,
# followed by the month as one or more letters (either capital or lowercase).
# In these data, all month abbreviations or names have 3, 4 or 5 letters.
# Write a regular expression to extract the end day and month from dates
date_pattern <- "^\\d{1,2}-(\\d{1,2})\\s(.{3-5})"
temp <- str_extract_all(polls$dates, date_pattern )
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)

head(str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+"))

# preparing polls
polls <- polls_renamed %>% 
        mutate(end_date = str_extract(dates, "\\d{1,2}\\s[a-zA-Z]+")) %>%
        mutate(undecided = str_replace_all(undecided, pattern =  "N/A", replacement = "0%")) %>%
        mutate(lead = str_replace_all(lead, pattern =  "N/A", replacement = "0%")) %>%
        mutate(samplesize = str_replace_all(samplesize, pattern =  "N/A", replacement = "0")) %>%
        mutate_at(2:5, parse_number)
        



