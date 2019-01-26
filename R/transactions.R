import::from(magrittr, "%>%", "%$%", "%<>%")
import::from(dplyr,
             "mutate", "group_by", "summarize", "rename_all", "rename",
             "arrange")
             
DATA = "../transactions.csv"

#' @param dat a dataframe with the columns
#' Date, Transaction, Name, Memo, Amount
#' @return a dataframe with the column names lowercased, the rows  ordered
#' by data and memo text, and some additional derived columns added.
prepare_transactions <- function(dat) {
  dat %>%
    rename_all(tolower) %>%
    rename(type = transaction) %>%
    arrange(date, memo) %>%
    mutate(balance = cumsum(amount))
}

transactions <- readr::read_csv(DATA) %>%
  prepare_transactions()
  

