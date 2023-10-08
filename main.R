# Get the path of the currently running script
scriptPath <- sys.frame(1)$ofile

# Set the working directory to the directory containing the script
setwd(dirname(scriptPath))

# get GOOG data from Yahoo Finance
cat("Executing getData.R\n")
source("getData.R")

# Compute Stock Prices, Volatility and Returns on previous data
cat("Executing dataGraphs.R\n")
source("dataGraphs.R")

# Compute an approximation of risk free rate both inflation adjusted and not
cat("Executing riskFreeRate.R\n")
source("riskFreeRate.R")

# Use the CRR model to estimate the value of Put and Call options, compare price data to the CRR binomial tree 
cat("Executing CRRmodel.R\n")
source("CRRmodel.R")
