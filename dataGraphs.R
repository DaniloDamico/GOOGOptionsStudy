library(ggplot2)
library(readr)

stock_data <- read.csv("GOOG_data.csv")

# Create dataframe
stock_data <- data.frame(Date = as.Date(stock_data$Date),
                         Adjusted = as.vector(stock_data$Adjusted),
                         Opening = as.vector(stock_data$Opening),
                         High = as.vector(stock_data$High),
                         Low = as.vector(stock_data$Low),
                         Close = as.vector(stock_data$Close),
                         Volume = as.vector(stock_data$Volume))

# Stock Price
print(ggplot(data = stock_data, aes(x = Date)) +
        geom_line(aes(y = Adjusted, color = "Adjusted")) +
        #geom_line(aes(y = Opening, color = "Open")) +
        #geom_line(aes(y = High, color = "High")) +
        #geom_line(aes(y = Low, color = "Low")) +
        #geom_line(aes(y = Close, color = "Close")) +
        labs(title = "Stock Price",
             x = "Date",
             y = "Price",
             color = "Metric") +
        scale_color_manual(values = c("Adjusted" = "blue"))+
                                      #"Open" = "red",
                                      #"High" = "green",
                                      #"Low" = "purple",
                                      #"Close" = "orange")) +
        theme_minimal())


# Returns dataframe
dates <- stock_data$Date[-1] # remove first date
returns_data <- data.frame(Date = as.Date(dates),
                         Adjusted = as.vector(diff(log(stock_data$Adjusted))),
                         Opening = as.vector(diff(log(stock_data$Opening))),
                         High = as.vector(diff(log(stock_data$High))),
                         Low = as.vector(diff(log(stock_data$Low))),
                         Close = as.vector(diff(log(stock_data$Close))))

# Logarithmic Returns log(S(n+1)/S(n))
print(ggplot(data = returns_data, aes(x = Date)) +
        geom_line(aes(y = Adjusted, color = "Adjusted")) +
        #geom_line(aes(y = Opening, color = "Open")) +
        #geom_line(aes(y = High, color = "High")) +
        #geom_line(aes(y = Low, color = "Low")) +
        #geom_line(aes(y = Close, color = "Close")) +
        labs(title = "Logarithmic returns",
             x = "Date",
             y = "Returns",
             color = "Metric") +
        scale_color_manual(values = c("Adjusted" = "blue",
                                      "Open" = "red",
                                      "High" = "green",
                                      "Low" = "purple",
                                      "Close" = "orange")) +
        theme_minimal())

# Volatility (Standard Deviation of returns)
volatility <- sd(returns_data$Adjusted, na.rm = TRUE)
print(paste("Volatility: ", volatility))