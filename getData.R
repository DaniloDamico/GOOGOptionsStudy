library(quantmod)

stock <- "GOOG"
start_date <- as.Date("2021-01-01")
end_date <- as.Date("2023-08-10") # the last day is excluded

# Download stock data
getSymbols(stock, from = start_date, to = end_date)

adjusted_prices <- Ad(get(stock))
opening_prices <- Op(get(stock))
high_prices <- Hi(get(stock))
low_prices <- Lo(get(stock))
closing_prices <- Cl(get(stock))
volume <- Vo(get(stock))

# Create dataframe
stock_data <- data.frame(Date = index(closing_prices),
                         Adjusted = as.vector(adjusted_prices),
                         Opening = as.vector(opening_prices),
                         High = as.vector(high_prices),
                         Low = as.vector(low_prices),
                         Close = as.vector(closing_prices),
                         Volume = as.vector(volume))

write.csv(stock_data, file = "GOOG_data.csv", row.names = FALSE)