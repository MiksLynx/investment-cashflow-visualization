library(tidyverse)
library(readxl)
library(lubridate)






twinoData <- read_excel(
  "twino-export/twino-data.xlsx", 
  col_types = c(
    "skip", "date", "text", 
    "text", "skip", "numeric", "skip", 
    "skip", "skip"
  ), skip = 2
) %>%
  setNames(c('bookingDate', 'type', 'desciption', 'amount_eur')) %>%
  mutate(
    bookingDate = as_date(bookingDate),
    platform = 'twino'
  )






mintosData <- read_excel(
  "mintos-export/20181104-account-statement.xlsx", 
  col_types = c(
    "skip", "text", "text", 
    "numeric", "skip", "skip"
  )
) %>%
  setNames(c('bookingDate', 'details', 'amount_eur')) %>% 
  mutate(
    bookingDate = as_date(bookingDate),
    details = str_replace(details, 'Loan ID.*', ''),
    platform = 'mintos'
  )







envestioData <- read_excel(
  "envestio-export/Envestio-data.xlsx", 
  col_types = c(
    "text", "text", "skip", 
    "skip", "text"
  )
) %>%
  mutate(
    bookingDate = dmy(Date)
  ) %>% View()







