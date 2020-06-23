library(dslabs)
data("research_funding_rates")
research_funding_rates

library(pdftools)
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

# If we examine the object text we notice that it is a character vector with an entry for each page
# So we keep the page we want using the following code:
txt
str(txt)

# Getting the table we want
raw_data_research_funding_rates <- txt[2]
raw_data_research_funding_rates %>% head
# Each line on the page, including the table rows, is separated by the symbol for newline: \n.
# We can therefore can create a list with the lines of the text as elements:
tab <- str_split(raw_data_research_funding_rates, "\n")
str(tab) # tab is a list of 1
tab <- tab[[1]]
str(tab)
tab %>% head
# we see that the information for the column names is the third and fourth entries:
the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1
# We want to remove the leading space and everything following the comma.
# We can use regex for the latter. Then we can obtain the elements by splitting
# using the space. We want to split only when there are 2 or more spaces to avoid
# splitting success rate. So we use the regex \\s{2,} as follows:

the_names_1 %>% str_view_all(",\\s.")

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1


the_names_2
# Here we want to trim the leading space and then split by space as we did for the first line:
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# Now we can join these to generate one name for each column:
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
tmp_names
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

# Now we are ready to get the actual data. By examining the tab object, we notice that the information
# is in lines 6 through 14. We can use str_split() again to achieve our goal:
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number) # converts all columns to numbers except first
new_research_funding_rates %>% head()

# We can see that the objects are identical:
identical(research_funding_rates, new_research_funding_rates)





