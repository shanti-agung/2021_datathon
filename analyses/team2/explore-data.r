# ******************
#
# Explore Data
#
# ******************

library(tidyverse)

# ----- load dataset -------

# defendants
defendant_ids <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/defendant_docket_ids.csv")

# offenses & dispositions (2010 2011)
offense_disposition <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/offenses_dispositions_2010_2011.csv")

# defendant & docket (2010 2011)
defendant_docket <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/defendant_docket_details_2010_2011.csv")

# bail (2010 2011)
bail_2010 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/bail_2010.csv")
bail_2011 <- read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/bail_2011.csv", guess_max = 2000) 

# ------ explore -------

head(defendant_ids)

glimpse(offense_disposition)
glimpse(defendant_docket)

defendant_docket %>% 
  count(status_name)

table(defendant_docket$current_processing_status__processing_status, defendant_docket$status_name)

offense_disposition %>% 
  count(statute_description)

offense_disposition %>% 
  filter(statute_description != description) %>% 
  head()

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
