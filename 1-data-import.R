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
  ) %>%
  arrange(bookingDate)



mintosData <- read_excel(
  "mintos-export/mintos-data.xlsx", 
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
  ) %>%
  arrange(bookingDate)




envestioData <- read_excel(
  "envestio-export/Envestio-data.xlsx", 
  col_types = c(
    "text", "text", "skip", 
    "skip", "text"
  )
) %>%
  mutate(
    bookingDate = dmy(Date),
    amount_eur = str_sub(Amount, 2, nchar(Amount)) %>% # removing mystical first symbol
      str_replace_all(., '—', '-') %>% # replacing mystical minus with correct minus
      as.numeric(),
    platform = 'envestio'
  ) %>%
  select(-Date, -Amount) %>%
  arrange(bookingDate)


allData <- bind_rows(twinoData, mintosData, envestioData) %>%
  arrange(bookingDate)

