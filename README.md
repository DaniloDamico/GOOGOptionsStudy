# GOOG Options Study

This is a term paper for the course "Metodi Probabilistici e Statistici per i Mercati Finanziari." The scripts in this repository are designed to work in RStudio, and they assume that the repository's folder is set as the working directory.

## Overview

This project analyzes Alphabet Inc.'s stock options data from Yahoo Finance, focusing on stock value, volatility, and expected returns compared to real market data. The predictions made in this study are validated against actual data obtained from Yahoo Finance. The computations for Stock Value (S), Call Value (C), and Put Value (P) are based on the Cox-Ross-Rubinstein (CRR) model, with a fixed strike price (K = 130).

## Predictions

Predictions in this study begin on August 10, 2023, and target options with a maturity date of September 8, 2023. The risk-free rate (r) is approximated using the 10 Years Treasury Constant Maturity Rate.

## CRR Model Calibration

The CRR model is calibrated as follows:

- Number of Trading Days (N): 20 (representing the trading days from August 10, 2023, to September 8, 2023)
- Time Step ($\Delta t$): 1 (indicating one day in a trading year)
- Upward Movement (u): $e^{\sigma \sqrt{\Delta t}}\$
- Downward Movement (d): $\frac{1}{u}$

## Risk-Neutral Probabilities

The risk-neutral probabilities are computed as follows:

- $\tilde{p}$: $\frac{1 + r - d}{u - d}$
- $\tilde{q}$: $1 - \tilde{p}$

Where:
- S: Stock Value
- C: Call Value
- P: Put Value
- K: Strike Price
- $\sigma$: Volatility
- r: Risk-Free Rate

