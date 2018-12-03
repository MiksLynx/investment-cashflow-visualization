# Invested money
investment <- allData %>%
  filter(type == 'FUNDING' | details == 'Incoming client payment' | Name == 'Investor account top-up') %>%
  mutate(ym = substr(bookingDate, 1, 7)) %>%
  group_by(ym) %>%
  summarise(invested = sum(amount_eur)) %>%
  ungroup() %>%
  mutate(cumInvested = cumsum(invested))

# Interest
interest <- allData %>%
  filter(
    desciption == 'INTEREST' |
    str_detect(details, 'Interest income') |
    str_detect(Name, 'bonus') |
    str_detect(Name, 'Interest')
  ) %>%
  mutate(ym = substr(bookingDate, 1, 7)) %>%
  group_by(ym) %>%
  summarise(interestGained = sum(amount_eur)) %>%
  ungroup() %>%
  mutate(cumInterestGained = cumsum(interestGained))

cashflow <- allData %>%
  mutate(
    transactionType = case_when(
      .$type == 'FUNDING' |
        .$details == 'Incoming client payment' |
        .$Name == 'Investor account top-up' ~ 'Investment',
      .$desciption == 'INTEREST' |
        str_detect(.$details, 'Interest income') |
        str_detect(.$Name, 'bonus') |
        str_detect(.$Name, 'Interest') ~ 'Interest',
      TRUE ~ 'Other'
    )
  ) %>%
  filter(transactionType %in% c('Interest', 'Investment')) %>%
  group_by(bookingDate, transactionType) %>%
  summarise(amount_eur = sum(amount_eur, na.rm = T)) %>%
  spread(transactionType, amount_eur) %>%
  # populate data with empty dates
  right_join(data.frame(bookingDate = seq.Date(as.Date('2018-02-01'), Sys.Date(), by = 'days')),
             by = 'bookingDate') %>%
  # replace NAs with 0
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = Interest + Investment) %>%
  ungroup() %>%
  # calculate cumulative measures
  mutate(
    Interest = cumsum(Interest),
    Investment = cumsum(Investment),
    Total = cumsum(Total)
  ) %>%
  # gather everything back so that ggplot can work with this
  gather(key = transactionType, value = amount_eur,
         c(Interest, Investment, Total)) %>%
  mutate(transactionType = factor(transactionType, levels = c('Interest', 'Investment', 'Total'),
                                  labels = c('Interest gained', 'Money invested', 'Total saved')))



colour <- c("#999999", "#E69F00", "#56B4E9")

ggplot(aes(
  y = amount_eur,
  x = bookingDate,
  group = transactionType,
  color = transactionType
), data = cashflow) +
  geom_line(size = 0.75) +
  scale_x_date(date_breaks = '1 month', date_labels = '%Y-%m') +
  scale_y_continuous(breaks = seq(0, 5000, 500), minor_breaks = seq(0, 5000, 250)) +
  labs(title = "Summary of investments' cashflow",
       subtitle = "Results on daily basis") +
  labs(x = 'Booking date', y = 'EUR') +
  scale_colour_manual(values = colour) +
  theme_bw() +
  theme(
    legend.position = 'right',
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = 0)
