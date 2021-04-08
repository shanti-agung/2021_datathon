# this file looks at alice and eamon functions

# load libraries
library(readr)
library(dplyr)
library(stringr)


# load aw functions
source(file.path("analyses", "team1", "aw_functions.r"))

# load offenses_dispositions dataset
# Read in the data from internet - define readr cols for parsing failures ----
odcols <- cols(
  docket_id = col_double(),
  description = col_character(),
  statute_description = col_character(),
  sequence_number = col_double(),
  grade = col_character(),
  disposition = col_character(),
  disposing_authority__first_name = col_character(),
  disposing_authority__middle_name = col_character(),
  disposing_authority__last_name = col_character(),
  disposing_authority__title = col_character(),
  disposing_authority__document_name = col_character(),
  disposition_method = col_character(),
  min_period = col_character(),
  max_period = col_character(),
  period = col_character(),
  sentence_type = col_character()
)
od <- readr::read_csv('https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions.csv',
                      col_types = odcols)

# ----------- ec code: ec_sentence_period.rmd -----------
od %>%
  summarize(across(c(min_period, max_period, period), n_distinct))

od %>%
  count(period) %>%
  arrange(desc(n)) %>%
  head(10)

od %>%
  select(ends_with("period"))

od %>%
  filter(!is.na(period),
         is.na(min_period),
         is.na(max_period)) %>%
  count(period) %>%
  arrange(desc(n)) %>%
  head(20)

# en and em dash
str_subset(od$period,
           "(–|—)")

clean_periods <- od$period %>%
  tolower() %>%
  str_replace_all("[[:blank:]]+", " ") %>%
  str_replace("^ ", "") %>%
  str_replace_all(" $", "")

# Deal with the many ways of writing ".5"
clean_periods <- clean_periods %>%
  str_replace_all("([[:digit:]]+) - 1/2", "\\1.5") %>%
  str_replace_all("([[:digit:]]+) 1/2", "\\1.5") %>%
  str_replace_all("([[:digit:]]+) and a half", "\\1.5") 

# Tokenize "time in"/"time-in", "time served" and some typos
clean_periods <- clean_periods %>%
  str_replace("tiime", "time") %>%
  str_replace("tim ", "time") %>%
  str_replace("time[ -]?(served|in)", "time_served")

# Standardize units
clean_periods <- clean_periods %>%
  str_replace_all("hrs", "hours") %>%
  str_replace_all("hour(?!s)",  "hours") %>%
  str_replace_all("day(?!s)",   "days") %>%
  str_replace_all("month(?!s)", "months") %>%
  str_replace_all("year(?!s)",  "years")

# Standardize the "time arithmetic"
clean_periods <- clean_periods %>%
  str_replace_all(" and ", " plus ") %>%
  str_replace_all(" less ", " minus ")
# This one's tricky, in part because str_replace_all is greedier than it should be
operator_insertion_regexp <- "^([[:digit:]]+) ([[:alpha:]]+) ([[:digit:]]+) ([[:alpha:]]+)$"
needs_operator_inserted <- !is.na(clean_periods) &
  str_detect(clean_periods, operator_insertion_regexp) & 
  (str_replace(clean_periods, operator_insertion_regexp, "\\2") != "to")
clean_periods[needs_operator_inserted] <- str_replace_all(clean_periods[needs_operator_inserted],
                                                          operator_insertion_regexp,
                                                          "\\1 \\2 plus \\3 \\4")

# Split the periods using "-" or "to"
clean_periods_split <- clean_periods %>%
  str_split_fixed("\\s?((\\bto\\b)|-)\\s?", 2)
#str_split_fixed("\\s?((\\bto\\b)|(-(?!.*\\bto\\b)))\\s?", 2)

# If the beginning period is missing a time unit (days, months, years), grab it from the end period
is_missing_units <- str_detect(clean_periods_split[, 1], "^[[:digit:]]+(\\.5)?$")
# Pulling out the first units word is a bit tricky
missing_units <- clean_periods_split[is_missing_units, 2] %>%
  str_extract("^[[:digit:]]+(\\.5)? [[:alpha:]]+") %>%
  str_extract("[[:alpha:]]+$")
clean_periods_split[is_missing_units, 1] <- paste(clean_periods_split[is_missing_units, 1],
                                                  missing_units, sep = " ")

clean_periods_split %>%
  as.data.frame() %>%
  distinct() %>%
  arrange(desc(V1)) %>%
  print.AsIs()


# ------- look up at od_v2 and od_v3 ----------

od_v2_1819 <- readr::read_csv('https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_v2_2018_2019.csv',
                              col_types = odcols)

glimpse(od_v2_1819)
dim(od_v2_1819)
dim(od)
glimpse(od)

od_v3_1819 <- readr::read_csv('https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_v3_2018_2019.csv',
                              col_types = odcols)
glimpse(od_v3_1819)
head(od_v3_1819$credit)