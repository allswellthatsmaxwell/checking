import::from(magrittr, "%>%", "%$%", "%<>%")
import::from(dplyr,
             "mutate", "group_by", "summarize", "rename_all", "rename",
             "arrange", "tibble")
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

#' Increments ss's count in the env counts, adding ss if it does not exist.
#' Returns nothing interesting.
add_to_env <- function(ss, counts) {
  if (!exists(ss, envir = counts, inherits = FALSE)) {
    counts[[ss]] = 1
  } else {
    counts[[ss]] %<>% {. + 1}
  }
}

#' Counts all the prefixes of s and puts those counts in the env counts,
#' inserting any that don't exist.
#' Returns nothing interesting.
count_prefixes <- function(s, counts) {
  for (i in 1:nchar(s)) {
    add_to_env(substr(s, 1, i), counts)
  }
}

#' counts all the prefixes of all lengths in a list of strings.
#' @param strings an iterable of strings
#' @return a dataframe with all the prefixes and their counts.
count_all_prefixes <- function(strings) {
  counts <- new.env()
  for (s in strings) {    
    count_substrings(s, counts)
  }

  prefixes <- ls(counts)
  prefixes_counts <- prefixes %>%
    sapply(function(prefix) counts[[prefix]]) %>%
    as.numeric()
  tibble(prefix = prefixes, count = prefixes_counts) %>%
    arrange(desc(count)) 
}

collapse_whitespace <- function(s) gsub("[ ]+", " ", s)

#' Strictly speaking, this function removes substrings, not just prefixes.
remove_prefixes_from_string <- function(string, prefixes) {
  Reduce(function(s, prefix) gsub(prefix, "", s, ignore.case = TRUE),
         x = prefixes, init = string) %>%
    trimws() %>%
    collapse_whitespace()
}

#' Removes prefixes (in a file) from the input strings.
#' @param strings an iterable of strings to remove prefixes from
#' @param prefix_file a csv with the single column 'prefix', containing
#' all the substrings slated for removal.
#' Strictly speaking, this function removes substrings, not just prefixes.
remove_prefixes_from_strings <- function(strings, prefix_file) {
  prefixes <-
    readr::read_csv("../intermediate/prefixes_to_remove.csv",
                    col_types = list(prefix = col_character())) %$%
    prefix  
  sapply(strings, function(s) remove_prefixes_from_string(s, prefixes))
}


checking <- read_transactions(CHECKING_FILE)
credit   <- read_transactions(CREDIT_FILE)

checking_balance_timeseries <- checking %>%
  to_daily() %>%
  plot_daily_timeseries()

credit_balance_timeseries <- credit %>%
  to_daily() %>%
  plot_daily_timeseries()

## I looked at the common prefixes to find the prefixes I wanted to remove.
## Things like "DEBIT PURCHASE -VISA".
counts <- count_all_prefixes(checking[["name"]])
readr::write_csv(counts, '../intermediate/prefix_counts.csv')

result <- remove_prefixes_from_strings(checking[["name"]], PREFIXES_TO_REMOVE_FILE)

checking[["name_post_prefix_removal"]] <-
  remove_prefixes_from_strings(checking[["name"]], PREFIXES_TO_REMOVE_FILE)
