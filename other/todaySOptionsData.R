library(quantmod)
library(lubridate)
library(tidyr)


stock_symbol <- "GOOG"
string_list <- list("2023-10-20", "2023-11-17", "2023-12-15")

today_date <- format(Sys.Date(), "%Y-%m-%d")


# Create a folder to store the CSV file
folder_name <- "options_data"
if (!dir.exists(folder_name)) {
  dir.create(folder_name, showWarnings = FALSE)
}

folder_today <- file.path(folder_name, today_date)
dir.create(folder_today, showWarnings = FALSE)

for (string in string_list) {
  
  options_data <- getOptionChain(stock_symbol, string)

  calls_data <- options_data$calls
  puts_data <- options_data$puts
  
  calls_file_name <- paste0(folder_today, "/calls_", string, ".csv")
  puts_file_name <- paste0(folder_today, "/puts_", string, ".csv")
  
  write.csv(calls_data, calls_file_name, row.names = FALSE)
  write.csv(puts_data, puts_file_name, row.names = FALSE)
}