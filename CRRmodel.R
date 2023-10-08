# Cox Ross Rubinstein Model (Multi-Period Multiplicative Binomial Model)

# at the end of every one of the negotiation periods, we can have one of two outcomes: 
#   - positive (prob p)
#   - negative (prob q = 1-p)
# every result is independent from every other
# => P(w) = p^k q^(n-k)

library(ggplot2)
library(fs)
library(reshape2)
library(tidyr)
library(quantmod)
library(dplyr)

#### 0. RETRIEVE DATA AND INITIALIZE CONSTANTS ####

  # stock data until 2023-08-10
  stock_data <- read.csv("output/GOOG_data.csv")
  stock_data <- data.frame(Date = as.Date(stock_data$Date),
                           Adjusted = as.vector(stock_data$Adjusted))
  
  # stock_data from 2023-08-10 until 2023-09-08
  getSymbols("GOOG", from = "2023-08-10", to = "2023-09-09")
  real_prices <- Ad(get("GOOG"))
  real_prices <- data.frame(Adjusted = as.vector(real_prices))
  
  # risk-free rate
  #r <- read.csv("output/risk_free_rate.txt")
  #r <- read.csv("output/annualized_risk_free_rate.txt")
  r <- read.csv("output/10_year_risk_free_rate.txt")
  r <- as.numeric(r$x)
  
  
  # volatility
  #sigma <- read.csv("output/annualized_volatility.txt")
  sigma <- read.csv("output/volatility.txt")
  sigma <- as.numeric(sigma$x)
  
  S0 <- real_prices$Adjusted[1]
  
  N <- 20 # trading days from 2023-08-10 to 2023-09-08
  delta_t <- 1 # one day in a trading year
  
  u <- exp(sigma*sqrt(delta_t))
  d <- 1/u
  
  # risk-neutral probability
  p_tilde <- (1+r-d)/(u-d)
  q_tilde <- 1 - p_tilde
  
  cat("\tStarting Stock Price:\t", S0, "\n")
  cat("\tVolatility:\t\t", sigma, "\n")
  cat("\tRisk-free rate:\t\t", r, "\n")
  cat("\tUp:\t\t\t", u, "\n")
  cat("\tDown:\t\t\t", d, "\n")
  cat("\tP tilde:\t\t", p_tilde, "\n")
  cat("\tQ tilde:\t\t", q_tilde, "\n")
  
#### 1. RETRIEVE AND PRINT OPTION DATA ####
  
  # setup
  calls_file_name <- "calls_2023-09-08.csv"
  puts_file_name <- "puts_2023-09-08.csv"
  parent_folder <- paste0(getwd(),"/2023-09-08")
  observation_dates <- list.files(parent_folder)
  
  last_call_observation <- read.csv(paste0(parent_folder, "/2023-09-08/", calls_file_name))
  last_call_data <- data.frame(Strike = as.vector(last_call_observation$Strike),
                               Last = as.vector(last_call_observation$Last))
  last_put_observation <- read.csv(paste0(parent_folder, "/2023-09-08/", puts_file_name))
  last_put_data <- data.frame(Strike = as.vector(last_put_observation$Strike),
                              Last = as.vector(last_put_observation$Last))
  
  # Create a new dataframe with Strike values as columns
  call_dataframe <- data.frame(t(last_call_data$Last))
  colnames(call_dataframe) <- last_call_data$Strike
  
  put_dataframe <- data.frame(t(last_put_data$Last))
  colnames(put_dataframe) <- last_put_data$Strike
  
  # set the date in a new column
  call_dataframe$Date <- as.Date("2023-09-08")
  call_column_names <- colnames(call_dataframe)
  
  put_dataframe$Date <- as.Date("2023-09-08")
  put_column_names <- colnames(put_dataframe)
  
  for (folder in observation_dates[-length(observation_dates)]) {
    
    # CALL
    call_observation_data <- read.csv(paste0(parent_folder, "/", folder, "/", calls_file_name))
    call_observation_dataframe <- data.frame(Strike = as.vector(call_observation_data$Strike),
                                             Last = as.vector(call_observation_data$Last))
    
    # Add a new row with NA values to the original dataframe
    new_row <- data.frame(matrix(NA, ncol = length(call_column_names)))
    colnames(new_row) <- call_column_names
    call_dataframe <- rbind(call_dataframe, new_row)
    
    # update Date
    call_dataframe[nrow(call_dataframe), "Date"] <- as.Date(folder)
    
    # retrieve the data from the current observation in the correct format
    curr_call_row <- data.frame(t(call_observation_dataframe$Last))
    colnames(curr_call_row) <- call_observation_dataframe$Strike
    
    # update last row
    call_dataframe[nrow(call_dataframe), names(curr_call_row)] <- unlist(curr_call_row)
    
    # PUT
    
    put_observation_data <- read.csv(paste0(parent_folder, "/", folder, "/", puts_file_name))
    put_observation_dataframe <- data.frame(Strike = as.vector(put_observation_data$Strike),
                                            Last = as.vector(put_observation_data$Last))
    
    # Add a new row with NA values to the original dataframe
    new_row <- data.frame(matrix(NA, ncol = length(put_column_names)))
    colnames(new_row) <- put_column_names
    put_dataframe <- rbind(put_dataframe, new_row)
    
    # update Date
    put_dataframe[nrow(put_dataframe), "Date"] <- as.Date(folder)
    
    # retrieve the data from the current observation in the correct format
    curr_put_row <- data.frame(t(put_observation_dataframe$Last))
    colnames(curr_put_row) <- put_observation_dataframe$Strike
    
    # update last row
    put_dataframe[nrow(put_dataframe), names(curr_put_row)] <- unlist(curr_put_row)
  }
  
  call_dataframe <- call_dataframe[order(call_dataframe$Date), ]
  write.csv(call_dataframe, file = "output/call.csv", row.names = FALSE)
  
  melted_call_dataframe <- melt(call_dataframe ,  id.vars = 'Date', variable.name = 'strike')
  call_price_graph <- ggplot(melted_call_dataframe, aes(Date,value)) + 
                              geom_line(aes(colour = strike)) + 
                              theme_minimal() +
                              labs(title = "Call Options Price History")
  
  print(call_price_graph)
  ggsave("output/call_price_graph.png", plot = call_price_graph, width = 10, height = 6)
  
  put_dataframe <- put_dataframe[order(put_dataframe$Date), ]
  
  write.csv(put_dataframe, file = "output/put.csv", row.names = FALSE)
  
  melted_put_dataframe <- melt(put_dataframe ,  id.vars = 'Date', variable.name = 'strike')
  put_price_graph <- ggplot(melted_put_dataframe, aes(Date,value)) + 
                            geom_line(aes(colour = strike)) + 
                            theme_minimal() +
                            labs(title = "Put Options Price History")
  
  print(put_price_graph)
  ggsave("output/put_price_graph.png", plot = put_price_graph, width = 10, height = 6)

#### 2. THE PRICE TREE ####
  
  create_binomial_tree <- function(StartingValue, treeHeight){
                            #binomial tree
                            binomial_tree <- matrix(NA, nrow=treeHeight+1, ncol = treeHeight+1)
                            binomial_tree[1,1] <- StartingValue
                            
                            # fill the tree
                            for (n in 0:treeHeight) {
                              for (k in 0:n) {
                                S <- round(StartingValue * (u ^ k) * (d ^ (n - k)),2)
                                binomial_tree[n + 1, k + 1] <- S
                              }
                            }
                            return(binomial_tree)
  }

  price_tree <- create_binomial_tree(S0, N)
  
  # convert the matrix to a dataframe for easier manipulation
  price_tree <- as.data.frame(price_tree)
  price_tree$index <- 1:nrow(price_tree)
  price_tree <- gather(price_tree, key = "Column", value = "Value", -index)
  
  tree_graph <- ggplot(data = real_prices, aes(x = seq_along(Adjusted), y = Adjusted)) +
                        geom_line() +
                        geom_point(data = price_tree, aes(x = index, y = Value)) +
                        labs(
                          title = "GOOG Adjusted Prices (Aug 10, 2023 - Sep 9, 2023)",
                          x = "Number of Rows",
                          y = "Adjusted Price"
                        ) +
                        theme_minimal()
  
  print(tree_graph)
  ggsave("output/tree_graph.png", plot = tree_graph, width = 10, height = 6)

  # compute the difference between the real value and the closest predicted value
  price_tree <- na.omit(price_tree)
  price_tree <- price_tree[order(price_tree$index), ]
  price_tree <- subset(price_tree, select = -Column)
  
  difference <- list()
  for(i in 1:nrow(real_prices)){
    curr_predicted_values <- subset(price_tree, index == i)
    curr_predicted_values$Value <- abs(curr_predicted_values$Value - real_prices$Adjusted[i])
    min <- min(curr_predicted_values$Value)
    difference <- c(difference, min)
  }
  difference <- data.frame(Value = unlist(difference))
  

  difference_graph <- ggplot(difference, aes(x = seq_along(Value), y = Value)) +
                              geom_line() +
                              labs(
                                title = "Difference between actual stock value and closest predicted value",
                                x = "",
                                y = "Values") +
                              theme_minimal()
  
  print(difference_graph)
  ggsave("output/difference_graph.png", plot = difference_graph, width = 10, height = 6)
  
  min_difference <- min(difference$Value[2:length(difference$Value)])
  max_difference <- max(difference$Value[2:length(difference$Value)])
  avg_difference <- mean(difference$Value[2:length(difference$Value)])
  
  cat("\n\tDifference between predicted and actual stock value:\n")
  cat("\tMin:\t\t", min_difference, "\n")
  cat("\tMax:\t\t", max_difference, "\n")
  cat("\tAverage:\t", avg_difference, "\n")

#### 3. CALL AND PUT OPTION VALUES ####
  
  call_value <- function(StartingValue, treeHeight, Strike) {

    binomial_tree <- create_binomial_tree(StartingValue, treeHeight)
    call_tree <- matrix(NA, nrow=treeHeight+1, ncol = treeHeight+1)
    
    # values at the final node
    for(i in 1:(treeHeight+1)){
      # call: Max [ (Sn − K), 0 ]
      call_tree[treeHeight+1, i] <- max(binomial_tree[treeHeight+1, i] - Strike, 0)
    }
    
    # backwards induction
    for (i in treeHeight:1) {
      for (j in 1:i) {
        call_tree[i, j] <- round(exp(-r * delta_t) * (p_tilde * call_tree[i + 1, j + 1] + q_tilde * call_tree[i+1, j]), 2)
      }
    }

    return(call_tree[1,1])
  }
  
  put_value <- function(StartingValue, treeHeight, Strike) {

    binomial_tree <- create_binomial_tree(StartingValue, treeHeight)
    put_tree <- matrix(NA, nrow=treeHeight+1, ncol = treeHeight+1)
    
    # values at the final node
    for(i in 1:(treeHeight+1)){
      # put: Max [ (K − Sn), 0 ]
      put_tree[treeHeight+1, i] <- max(Strike - binomial_tree[treeHeight+1, i], 0)
    }
    
    # backwards induction
    for (i in treeHeight:1) {
      for (j in 1:i) {
        put_tree[i, j] <- round(exp(-r * delta_t) * (p_tilde * put_tree[i + 1, j + 1] + q_tilde * put_tree[i+1, j]), 2)
      }
    }
    
    return(put_tree[1,1])
  }
  
  # DO THE FOLLOWING FOR ALL VALUES BETWEEN 125 AND 135

  columns_to_keep <- names(call_dataframe)[names(call_dataframe) >= '125' & names(call_dataframe) <= '135']
  call_dataframe <- call_dataframe[, columns_to_keep]
  put_dataframe <- put_dataframe[, columns_to_keep]
  
  for(s in columns_to_keep){
    # arrays to store the values predicted by the CRR model
    predicted_call <- data.frame(CallValue = numeric(0))
    predicted_put <- data.frame(PutValue = numeric(0))
    
    # fill the arrays
    for(i in (N):1){
      StartingValue <- real_prices$Adjusted[N+2-i]
      treeHeight <- i
      
      predicted_call <- predicted_call %>% add_row(CallValue = call_value(StartingValue, treeHeight, as.numeric(s)))
      predicted_put <- predicted_put %>% add_row(PutValue = put_value(StartingValue, treeHeight, as.numeric(s)))
    }
    
    call_prediction <- ggplot(data = predicted_call, aes(x = seq_along(CallValue))) +
                              geom_line(aes(y = CallValue, color = "Estimate")) +
                              geom_line(aes(y = call_dataframe[[s]][-1], color = "Real")) +
                              labs(title = paste("Call Value for Strike ", s),
                                   x = "Date",
                                   y = "Price",
                                   color = "Metric") +
                              scale_color_manual(values = c("Estimate" = "blue",
                                                            "Real" = "orange")) +
                              theme_minimal()
    
    print(call_prediction)
    ggsave(paste("output/call_prediction_strike_", s, ".png"), plot = call_prediction, width = 10, height = 4)
    
    put_prediction <- ggplot(data = predicted_put, aes(x = seq_along(PutValue))) +
                              geom_line(aes(y = PutValue, color = "Estimate")) +
                              geom_line(aes(y = put_dataframe[[s]][-1], color = "Real")) +
                              labs(title = paste("Put Value for Strike ", s),
                                   x = "Date",
                                   y = "Price",
                                   color = "Metric") +
                              scale_color_manual(values = c("Estimate" = "blue",
                                                            "Real" = "orange")) +
                              theme_minimal()
    
    print(put_prediction)
    ggsave(paste("output/put_prediction_strike_", s, ".png"), plot = put_prediction, width = 10, height = 4)
    
  }
  


  
  