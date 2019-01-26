import::from(magrittr, "%>%", "%$%", "%<>%")
import::from(dplyr,
             "mutate", "group_by", "summarize", "rename_all", "rename",
             "arrange")
import::from(readr, "read_csv", "col_date", "col_character", "col_double")
library(ggplot2)
source("paths.R")

#' @param file the path to a csv file with the columns
#' Date, Transaction, Name, Memo, Amount.
#' Date should be in m/d/Y format, e.g. 12/13/2018.
#' @return a dataframe with the csv's column names lowercased, the rows ordered
#' by data and memo text, and some additional derived columns added.
read_transactions <- function(file) {
  file %>%
    read_csv(col_types = list(Date = col_date("%m/%d/%Y"),
                              Transaction = col_character(),
                              Name = col_character(),
                              Memo = col_character(),
                              Amount = col_double())) %>%
    rename_all(tolower) %>%
    rename(type = transaction) %>%
    arrange(date, memo) %>%
    mutate(balance = cumsum(amount))
}

#' Groups a dataframe of transactions to the day level.
#' @return a dataframe
to_daily <- function(dat) {
  dat %>%    
    group_by(date) %>%
    summarize(amount = sum(amount)) %>%
    arrange(date) %>%
    mutate(balance = cumsum(amount))
}

plot_daily_timeseries <- function(dat) {
  dat %>%
    ggplot(aes(x = date, y = balance)) +
    geom_line() +
    theme_bw() +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::dollar) +
    theme(axis.text.x = element_text(angle = 90))
}

checking_balance_timeseries <- read_transactions(CHECKING_FILE) %>%
  to_daily() %>%
  plot_daily_timeseries()

credit_balance_timeseries <- read_transactions(CREDIT_FILE) %>%
  to_daily() %>%
  plot_daily_timeseries()

