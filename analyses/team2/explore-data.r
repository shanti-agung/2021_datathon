# ******************
#
# Explore Data
#
# ******************

library(tidyverse)
library(lubridate)

# ----- load datasets -------

# defendants
defendant_ids <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/defendant_docket_ids.csv")

# offenses & dispositions (2010 2011)
offense_disposition_2010_2011 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_2010_2011.csv")
offense_disposition_2012_2013 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_2012_2013.csv")
offense_disposition_2014_2015 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_2014_2015.csv")
offense_disposition_2016_2017 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_2016_2017.csv")
offense_disposition_2018_2019 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_2018_2019.csv",
                                          guess_max = 238753)

offense_disposition <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions.csv")

# defendant & docket details
details_docket <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/defendant_docket_details.csv",
                           guess_max = 20000)

# bail (2010 2011)
bail_2010 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/bail_2010.csv")
bail_2011 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/bail_2011.csv", guess_max = 2000) 

# defendant id & docket id
ids_defendant_docket <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/defendant_docket_ids.csv")

test <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_v2.csv")
# ------ explore -------

head(defendant_ids)

glimpse(offense_disposition)
glimpse(defendant_docket)

defendant_docket %>% 
  count(status_name)

table(defendant_docket$current_processing_status__processing_status, defendant_docket$status_name)

# -- description or statute_description as the unit of analysis
offense_disposition %>% 
  count(statute_description)

offense_disposition %>% 
  filter(statute_description != description) %>% 
  count() # 116,631

offense_disposition %>% 
  filter(statute_description == description) %>% 
  count() #1,348,482
# -- 

table(offense_disposition$disposition)
table(offense_disposition$sentence_type)
table(offense_disposition$disposition, offense_disposition$sentence_type)

offense_disposition %>% 
  filter(!is.na(disposition)) %>% 
  count(disposition, sentence_type) %>% 
  view()

table(offense_disposition$disposition_method)

offense_disposition %>% 
  select(docket_id, description, grade, disposition, disposition_method, min_period, max_period, period, sentence_type) %>% 
  filter(!is.na(disposition)) %>% 
  count(sentence_type)

# ------- what's the min, max, and avg number of offenses in a docket? ------

# offense_disposition %>% 
#   group_by(docket_id) %>% 
#   summarise(docket_id, max_num_offense = max(sequence_number)) # why is there repeated docket_id?

offense_disposition %>% 
  filter(docket_id == 1) %>% 
  view()
  # description = "DUI: Gen Imp/Inc of Driving Safely - 1st Off" appears 3 times
  # sequence_number: 1,1, 2, 4
  # the two entries with similar sequence_number (1) have different sentence types though

offense_disposition %>% 
  filter(docket_id == 4) %>% 
  view()
  # how should we interpret list of offenses, in a docket, that has NAs in columns disposition and beyond?

offenses_per_docket <- offense_disposition %>% 
  filter(!(sequence_number %in% c(99999, 9999, 9998, 9997, 999, 998))) %>% 
  group_by(docket_id) %>% 
  summarise(max_num_offense = max(sequence_number))

offenses_per_docket %>% 
  arrange(desc(max_num_offense))

offense_disposition %>% 
  filter(docket_id == 54436) %>% 
  arrange(desc(sequence_number)) %>% 
  view() 
  # all offenses entries have NAs in column disposition and beyond
  # repeated content of "description" and "statue_description"

offense_disposition %>% 
  filter(docket_id == 66941) %>% 
  arrange(desc(sequence_number)) %>% 
  view() 
  # sequence_number starts at 364

offense_disposition %>% 
  filter(docket_id == 67457) %>% 
  arrange(desc(sequence_number)) %>% 
  view() 
  # sequence_number jumps from 1, 2, 3, to 278, 279...422

offense_disposition %>% 
  filter(docket_id == 5) %>% 
  arrange(desc(sequence_number)) %>% 
  view() 

offenses_per_docket <- offense_disposition %>% 
  group_by(docket_id) %>% 
  summarise(num_offense = n()) 
  # issues that remain: all offenses entries have NAs in column disposition and beyond
                  # repeated content of "description" and "statue_description"

offenses_per_docket %>% 
  arrange(desc(num_offense))

offense_disposition %>% 
  filter(docket_id == 223627) %>% 
  arrange(desc(sequence_number)) %>% 
  view() 

offenses_per_docket <- offense_disposition %>% 
  distinct(docket_id, description, statute_description, sentence_type, .keep_all = TRUE) %>% 
  group_by(docket_id) %>% 
  summarise(num_offense = n()) 
  # eliminate entries where all offenses entries have NAs in column disposition and beyond,
  # repeated content of "description" and "statute_description"
  # include sequence_number entry of "9999" et al. 

offenses_per_docket %>% 
  arrange(desc(num_offense))

offense_disposition %>% 
  filter(docket_id == 75305) %>% 
  arrange(desc(sequence_number)) %>% 
  view() 

summary(offenses_per_docket$num_offense)

offenses_per_docket %>% 
  ggplot(aes(num_offense)) +
  geom_histogram(fill = "#7fbf7b") +
  labs(title = "Offenses per Docket",
       subtitle = "Year 2010 - 2020",
       x = "Number of offenses",
       y = "Number of dockets",
       caption = "Data from offenses_dispositions.csv \nOnly offenses unique in docket_id, description, statute_description, and sentence_type variables are included\nInclude offenses where values of sequence_number variable are 99999, 9999, 9998, 9997, 999, and 998")

# the distribution of number of offense per docket is right skewed, with centered at median of 3 offenses per case.
# with most of the data between 1 to 7 offenses per case, a range of roughly six offenses. 
# outliers are present in the higher end of the distribution.

offenses_per_docket %>% 
  ggplot(aes(num_offense)) +
  geom_histogram(fill = "#7fbf7b", binwidth = 1) +
  coord_cartesian(ylim = c(0,100)) +
  labs(title = "Offenses per Docket: Zoomed",
       subtitle = "Year 2010 - 2020",
       x = "Number of offenses",
       y = "Number of dockets",
       caption = "Data from offenses_dispositions.csv")



# ---------- what's distribution of docket per person? ----------

glimpse(ids_defendant_docket) 

dockets_per_defendant <- ids_defendant_docket %>% 
  group_by(defendant_id) %>% 
  summarise(n_dockets = n())

dockets_per_defendant %>% 
  arrange(desc(n_dockets))

ids_defendant_docket %>% 
  filter(defendant_id == "135924") %>% 
  view()

summary(dockets_per_defendant$n_dockets)

dockets_per_defendant  %>% 
  ggplot(aes(n_dockets)) +
  geom_histogram(fill = "#7fbf7b", binwidth = 0.9) +
  labs(title = "Dockets per Defendant",
       subtitle = "Year 2010 - 2020",
       x = "Number of dockets",
       y = "Number of defendants",
       caption = "Data from offenses_dispositions.csv")

# the distribution of number of offense per docket is right skewed, with centered at median of 3 dockets per defendant.
# with most of the data between 1 to 36 dockets per defendant, a range of roughly thirty five dockets. 
# outliers are present in the higher end of the distribution.

dockets_per_defendant  %>% 
  ggplot(aes(n_dockets)) +
  geom_histogram(fill = "#7fbf7b", binwidth = 0.9) +
  coord_cartesian(ylim = c(0,100)) +
  labs(title = "Dockets per Defendant: Zoomed",
       subtitle = "Year 2010 - 2020",
       x = "Number of dockets",
       y = "Number of defendants",
       caption = "Data from offenses_dispositions.csv")

# -------- how many are repeat offenders (multi dockets)? -------

dockets_per_defendant %>% 
  mutate(repeat_defendant = if_else(n_dockets == 1, FALSE, TRUE)) %>% 
  ggplot(aes(repeat_defendant)) +
  geom_bar(aes(y = stat(prop), group = 1), fill = "#7fbf7b") +
  labs(title = "Repeat Defendants",
       subtitle = "Year 2010 - 2020",
       x = "Repeat Defendant",
       y = "Proportion",
       caption = "Data from defendant_docket_ids.csv")

# -------- how many judges are there in the dataset? -----------

# overall 
judge_names <- offense_disposition %>% 
  distinct(disposing_authority__first_name, disposing_authority__middle_name, 
           disposing_authority__last_name) %>% 
  filter(!is.na(disposing_authority__last_name))

count(judge_names)

docket_judge <- offense_disposition %>% 
  distinct(docket_id, disposing_authority__first_name, disposing_authority__middle_name, 
           disposing_authority__last_name) %>% 
  filter(!is.na(disposing_authority__last_name))

# 
# %>% 
#   group_by(disposing_authority__last_name, disposing_authority__first_name, 
#            disposing_authority__middle_name) %>% 
#   summarise(n_dockets = n()) %>% 

# -- what's the number of cases that a judge typically handle, per year?
glimpse(details_docket)
glimpse(offense_disposition)
  
  # check for multiple appearance of a docket_id in this dataset
details_docket %>% 
  count(docket_id) %>% 
  filter(n>1)

  # merge docket year-month with judge names
judge_docket_monthly <- details_docket %>% 
  mutate(docket_year = year(filing_date), docket_month = month(filing_date, label = TRUE)) %>% 
  select(docket_id, docket_year, docket_month) %>%
  left_join(docket_judge, by = "docket_id") %>% 
  filter(!is.na(disposing_authority__last_name)) %>% 
  group_by(disposing_authority__first_name, disposing_authority__middle_name,
           disposing_authority__last_name, docket_year, docket_month) %>% 
  summarise(n_docket = n())

min(judge_docket_monthly$docket_year)
max(judge_docket_monthly$docket_year)

judge_docket_monthly %>% 
  ggplot(aes(n_docket)) +
  geom_histogram(fill = "#7fbf7b", binwidth = 1.5) +
  labs(title = "Monthly dockets per judge",
       subtitle = "Year 2010 - 2020",
       x = "Number of dockets",
       y = "Number of judges",
       caption = "Data from offenses_dispositions.csv and defendant_docket_details.csv")

judge_docket_monthly %>% 
  ggplot(aes(n_docket)) +
  geom_histogram(fill = "#7fbf7b", binwidth = 1.5) +
  coord_cartesian(ylim = c(0,300)) +
  labs(title = "Monthly dockets per judge: zoomed",
       subtitle = "Year 2010 - 2020",
       x = "Number of dockets",
       y = "Number of judges",
       caption = "Data from offenses_dispositions.csv and defendant_docket_details.csv")

# -- how many cases are "adjudicated" and "close"?
details_docket %>% 
  filter(!is.na(disposition_date)) %>% 
  count(current_processing_status__processing_status) %>% 
  view()

# -- how often does bail happen? -- what's the population?

# -- distribution of sentence type -- across offenses vs. across docket?

# -- how to parse "period" var
