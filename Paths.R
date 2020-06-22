# see working directory
getwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)

# Read first lines
read_lines("murders.csv", n_max = 5)

# read file in CSV format
dat <- read_csv("murders.csv")
head(dat)
class(dat)
class(dat$abb)

# using read.csv
data2 <- read.csv("murders.csv")
head(data2)
class(data2)
class(data2$abb)

# Load olive
file.exists("olive.csv")
library(readxl)
d <- read.csv("olive.csv")
head(d)

# Download from internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat3 <- read_csv(url)
download.file(url, "murders_downloaded.csv")

# Download from internet, give a temorary name, push to dat4 and then remove temp file
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat4 <- read_csv(tmp_filename)
file.remove(tmp_filename)
head(dat4)

# Copy file from other folder
file.location <- file.path(system.file("extdata", package = "dslabs"), "2010_bigfive_regents.xls")
file.destination <- file.path(getwd())
file.copy(file.location, file.destination) 
exc_name <- "2010_bigfive_regents.xls"
exc <- read_excel(exc_name, "Sheet1")

# Data import assessment
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat5 <- read_csv(url, col_names = FALSE)

# Sales
sales <- read_csv("sales.csv")
str(sales)
sales
sales %>% mutate_at(2:3, parse_number)
# sales %>% mutate_at(2:3, as.numeric) will not work
# sales %>% mutate_all(parse_number) will not work because will apply to first col too
sales %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
  mutate_at(2:3, as.numeric)
