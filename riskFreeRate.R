library(quantmod)
library(ggplot2)
library(lubridate)
library(zoo)
library(readr)
library(dplyr)

# The goal of this script is to compute the risk free interest rate using 10 year US Bonds as a proxy.
# The risk free rate is computed by subtracting the expected inflation rate from the yield of the Treasury bond.

today_date <- as.Date(format(Sys.Date(), "%Y-%m-%d"))

# 10 Years Treasury Constant Maturity Rate
treasury_ticker <- "DGS10"
getSymbols(treasury_ticker, src = "FRED", from = "2020-02-01", to = today_date)
bond_yield <- get(treasury_ticker)
bond_yield <- diff(bond_yield) / bond_yield[-length(bond_yield)]
bond_yield <- na.omit(bond_yield)

#inflation_ticker <- "CPIAUCNS"  # Consumer Price Index (CPI) for All Urban Consumers
inflation_ticker <- "EXPINF10YR"
getSymbols(inflation_ticker, src = "FRED", from = "2020-01-01")



#mean value of [S(n) - S(n-1)]/S(n-1)
inflation_rate <- get(inflation_ticker)

inflation_rate <- diff(inflation_rate) / lag(inflation_rate)
inflation_rate <- na.omit(inflation_rate)

#print(inflation_rate)

# Calculate the real risk-free rate by adjusting for inflation
# the bond_yield data is daily (with gaps) while the inflation data is monthly

index_names <- index(bond_yield)

risk_free_rate <- data.frame(rate = numeric(0))

for(index in index_names){
  date <- as.Date(index)
  first_day <- as.Date(format(date, "%Y-%m-01"))
  
  b <- as.numeric(bond_yield[date, treasury_ticker])
  i <- as.numeric(inflation_rate[first_day, inflation_ticker])
  
  risk_free_rate[as.character(date), "rate"] <- b-i
}


risk_free_date <- as.Date(rownames(risk_free_rate))

print(ggplot(data = risk_free_rate, aes(x = risk_free_date)) +
        geom_line(aes(y = rate, color = "rate")) +
        labs(title = "Risk-free rate",
             x = "Date",
             y = "Rate",
             color = "Metric") +
        scale_color_manual(values = c("rate" = "blue")) +
        theme_minimal())

# Create dataframe
risk_free_rate <- data.frame(Date = risk_free_date,
                         Rate = as.vector(risk_free_rate$rate))

write.csv(risk_free_rate, file = "risk_free_rate.csv", row.names = FALSE)
write.csv(bond_yield, file = "bond_yield.csv", row.names = FALSE)

print(mean(risk_free_rate$Rate))
print(mean(bond_yield$DGS10))

      
#stock_data <- read.csv("GOOG_data.csv")

# # Create dataframe
# stock_data <- data.frame(Date = as.Date(stock_data$Date), Close = as.vector(stock_data$Close))
# 
# # Spot-Futures Parity: K = (1+r)S(0)
# strike_price <- data.frame(price = numeric(0))
# stock_dates <- stock_data$Date
# 
# str(risk_free_rate)
# 
# for(d in stock_dates){
#   close_value <- as.numeric(stock_data$Close[stock_data$Date == d])
#   risk_free <- as.numeric(risk_free_rate[d, "rate"])
#   
#   #print(paste(close_value, " ", risk_free))
#   
#   strike_price[as.character(d), "price"] <- (1+risk_free)*close_value
# }
# 
# 
# 
# # Put-Call Parity: C(0) - P(0) = S(0) - K/(1+r)
# 
# # S(0) = Close on that day
# # r using bond yield on that date
