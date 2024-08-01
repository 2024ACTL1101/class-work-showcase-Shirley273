
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# Calculate daily returns for AMD based on AMD.Close 
df$amd_returns <- c(NA, diff(df$AMD))/ lag(df$AMD)
df$GSPC_returns <- c(NA, diff(df$GSPC))/ lag(df$GSPC)
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#Calculate daily risk free rate 
df$Daily_RF <- (1+df$RF/100)^(1/360)-1

```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
# Calculate Excess Returns for AMD and GSPC by subtracting amd_returns from daily risk free rate. 
df$amd_excess_returns <- df$amd_returns - df$Daily_RF
df$GSPC_excess_returns <- df$GSPC_returns - df$Daily_RF

```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r

# Perform Linear Regression to estimate Beta (AMD vs GSPC)
model <- lm(amd_excess_returns ~ GSPC_excess_returns, data = df)

# Summary of the regression model
summary(model)
beta_value <- summary(model)$coefficients["GSPC_excess_returns", "Estimate"]
print(paste("Beta value of AMD:", beta_value))

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?
My beta value is approximately 1.5699987, this means it is more volatile than the market. This suggests it has a more systematic risk, as it experiences larger fluctuations compared to the overall market. For example, if the markets moves up or down, AMD is more likely to experience a more significant price change in the same direction.  Despite this, the beta value suggests a potentially higher return compensated for the increased volatility, and investors would need to consider their risk tolerance and investment strategy. 

**Answer:**


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# Perform the regression analysis
model <- lm(amd_excess_returns ~ GSPC_excess_returns, data = df)


# Plot the relationship
ggplot(df, aes(x = GSPC_excess_returns, y = amd_excess_returns)) +
  geom_point(color = "blue") +  # Plot the points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add the regression line
  labs(
    title = "CAPM Regression: AMD vs. S&P 500 Excess Returns",
    x = "S&P 500 Excess Returns",
    y = "AMD Excess Returns"
  ) +
  theme_minimal()
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

```current_rf_rate <- 0.05  # Current risk-free rate
annual_sp500_return <- 0.133  # Annual expected return for S&P 500
trading_days <- 252  # Number of trading days in a year

# Calculate daily risk-free rate
daily_rf_rate <- (1 + current_rf_rate/100)^(1/360) - 1

#Calculate x_f 
x_f <- annual_sp500_return/trading_days - daily_rf_rate

# Estimate AMD's expected excess return
Y_f <- current_rf_rate + beta_value*(annual_sp500_return - current_rf_rate)

# Calculate number of observations (1259 - 1)  since first return in GSPC = n/a
n <- 1258
# mean residual
mean <- mean(df$GSPC_excess_returns, na.rm = TRUE)

#Calculate standard error of estimate (n-1-1) is the degrees of freedom 
se <- sqrt(sum(residuals(model)^2) / (n-1 -1))


# Calculate the mean and sum of squares, excluding the first value since first excess return = 0
returns_to_use <- df$GSPC_excess_returns[-1]
SSX <- sum((returns_to_use - mean)^2)


# Calculate standard error of forecast (s_f)
sf <- se*sqrt(1 + 1/n + (x_f - mean)^2/ SSX)


# Calculate the 90% prediction interval for AMD's annual expected return
alpha <- 0.1  # 90% confidence level
t_value <- qt(1 - alpha/2, df = n - 2)  # t-value for 90% confidence with n-2 degrees of freedom

# Calculate annual standard error for prediction
annual_sf <- sf*sqrt(252)

# Calculate lower and upper bounds of the prediction interval
lower_bound <- Y_f - t_value * annual_sf
upper_bound <- Y_f + t_value * annual_sf

print(upper_bound)
print(lower_bound)
```



```r
Therefore, the 90% prediction interval for AMD's annual expected return is -0.4907239,0.8513437. This range suggests the potential variability in predicting future performance. Thereby, investors are recommended to consider their investment strategy before investing. 
```
