library(quantmod)

# Morningstar expectation for 2023 US inflation
expected_inflation <- 0.037 

# Market Yield on U.S. Treasury Securities at 1-Month Constant Maturity
treasury_ticker <- "DGS1MO"
getSymbols(treasury_ticker, src = "FRED", from = "2023-08-08", to = "2023-08-08")

bond_yield <- get(treasury_ticker)  # percentage
bond_yield <- as.numeric(bond_yield)/100        # decimal
bond_yield <- bond_yield - expected_inflation

cat("\tRisk-free rate based on 1-Month T Bill value:\t", bond_yield, "\n")
write.csv(bond_yield, file = "output/risk_free_rate.txt", row.names = FALSE)

# Value of 10 Years Treasury Constant Maturity Rate
treasury_ticker <- "DGS10"
getSymbols(treasury_ticker, src = "FRED", from = "2023-07-10", to = "2023-08-10")
bond_yield <- get(treasury_ticker)
bond_yield <- na.omit(bond_yield)
bond_yield <- as.numeric(mean(bond_yield))
bond_yield <- bond_yield/100 # decimal

bond_yield <- bond_yield - expected_inflation

cat("\tRisk-free rate based on 10-Year T Bill value:\t", bond_yield, "\n")
write.csv(bond_yield, file = "output/10_year_risk_free_rate.txt", row.names = FALSE)