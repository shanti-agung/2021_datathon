# this file looks at alice and eamon functions

# load libraries
library(readr)
library(dplyr)


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

