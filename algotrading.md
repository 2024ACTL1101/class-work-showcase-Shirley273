
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Deletes bottom 3 rows since they are NA 
for (i in 1:3) {
amd_df <- amd_df[-c(nrow(amd_df)),]
}



# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Loop based on trading period (between 2019/05/20 to 2024/05/17)
# Fill your code here 
for (i in 1:nrow(amd_df)) {
  # If the previous price = 0, set 'trade_type' to 'buy', update 'costs_proceeds', and 'accumulated_shares'
  if (previous_price == 0 || is.na(previous_price)) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
  # If current price < previous day, we set the 'trade_type' to 'buy' and update our cost_proceeds and accumulated_shares correspondingly
  else if (!is.na(amd_df$close[i]) && amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
  else
  {
    amd_df$accumulated_shares[i] <- accumulated_shares
    
  }
    
  
  # If it's the last day of trading, set 'trade_type' to 'sell' and sell all accumulated shares
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
  }
  
  # Update previous_price for the next iteration
  previous_price <- amd_df$close[i]
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Fill your code here
# Choose the data range from the trading period (1st of January 2022 - 31st of December 2022) 
# Define dates
start_date <- as.Date('2022-01-01')  # E.g.default start date 
end_date <- as.Date('2022-12-31')  # E.g.default end date 

# Filter the data to include only the trading period within the specified range
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Loop based on trading period (between 2022/01/01 to 2022/12/31)
for (i in 1:nrow(amd_df)) {
  # If the previous price = 0, set 'trade_type' to 'buy', update 'costs_proceeds', and 'accumulated_shares'
  if (is.na(previous_price) || previous_price == 0) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
  # If the price of the current day is lower than the previous day, we set the 'trade_type' to 'buy' and update our cost_proceeds and accumulated_shares correspondingly
  else if (!is.na(amd_df$close[i]) && amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
 
  # If it's the last day of trading, set 'trade_type' to 'sell' and sell all accumulated shares
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
  }
  
  # Update previous_price for the next iteration
  previous_price <- amd_df$close[i]

}
  
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
#Total profit/Loss (P/L) We calculate this by summing all entries in the "cost_proceeds" section and add it by the total capital we got after selling it on the last day (31/12/2022). The total cost is summing up all the entries excluding the last value since we are selling all the shares, which becomes a capital. Note the 



# Find the days when we bought.
buy_df <-amd_df[amd_df$trade_type=="buy",]
# Find the total capital invested by adding the costs proceeds on the days when we bought
total_capital_invested_1 <- abs(sum(buy_df$costs_proceeds[1:(nrow(buy_df))], na.rm = TRUE))
# Find the total profit or loss by adding the total costs proceeds on both the days when we bought and sold.
total_profit_or_loss_1<-sum(amd_df$costs_proceeds[1:(nrow(amd_df))], na.rm = TRUE) 
# Find ROI by dividing profit or loss by total capital invested, multiplying by 100 for a percentage.
ROI_1 <- (total_profit_or_loss_1 / total_capital_invested_1) * 100

print(total_profit_or_loss_1)
print(ROI_1)
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Choose the data range from the trading period (1st of January 2022 - 31st of December 2022) 
# Define dates
start_date <- as.Date('2022-01-01')  # Eg.default start date 
end_date <- as.Date('2022-12-31')  # Eg.default end date 

# Filter the data to include only the trading period within the specified range
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$average_purchase_price <- NA

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
average_purchase_price <- amd_df$close[1]

# buy on day 1
amd_df$trade_type[1] <- 'buy'
amd_df$costs_proceeds[1] <- -amd_df$close[1] * share_size
accumulated_shares <- accumulated_shares + share_size
amd_df$accumulated_shares[1] <- accumulated_shares[1]
amd_df$average_purchase_price[1] <- amd_df$close[1]  # First purchase, average price equals the closing price


# Loop based on trading period (between 2022/01/01 to 2022/12/31)
# When odd number of shares, round up number to sell and round down number to keep.




for (i in 2:nrow(amd_df)) {
if (amd_df$close[i] >= amd_df$average_purchase_price[i-1] * 1.2) {
    # Sell half of the holding shares if the current day's closing price is 20% increase or over the average purchase price
    amd_df$trade_type[i] <- 'sell'
    # Calculate cost proceeds
    cost_proceeds <- ceiling(amd_df$accumulated_shares[i-1] / 2) * amd_df$close[i]
    # Update costs/proceeds and accumulated shares
    amd_df$costs_proceeds[i] <- cost_proceeds
    amd_df$accumulated_shares[i] <- floor(amd_df$accumulated_shares[i-1] / 2)
    amd_df$average_purchase_price[i] <- amd_df$average_purchase_price[i-1]
    
  } else if (amd_df$close[i] < amd_df$close[i-1] ) {
    # Buy if the current day's closing price is less than the previous day's closing price
    amd_df$trade_type[i] <- 'buy'
    # Calculate costs
    buy_cost <- -amd_df$close[i] * share_size
    # Update costs/proceeds and accumulated shares
    amd_df$costs_proceeds[i] <- -(share_size * amd_df$close[i])
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    # Update average purchase price depending on whether shares were sold in the previous day 
    amd_df$average_purchase_price[i] <- ((amd_df$average_purchase_price[i-1] * (amd_df$accumulated_shares[i-1])) + (share_size * amd_df$close[i])) / amd_df$accumulated_shares[i]
    
    
    
    
  }
    else {
    # Hold if the current day's closing price is greater than the previous day's closing price but increases by less than 20%
    amd_df$trade_type[i] <- 'hold'
    # Update cost proceeds
    cost_proceeds <- 0 * amd_df$close[i]
    # Update accumulated shares column
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
    # Update average purchase price 
    amd_df$average_purchase_price[i] <- amd_df$average_purchase_price[i-1]
        } 
  
#On the last day, sell all the accumulated shares by the closing price 
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1] * amd_df$close[i]
  }
  
}




# Check ROI

# Find the days when we bought.
buy_df <-amd_df[amd_df$trade_type=="buy",]
# Find the total capital invested by adding the costs proceeds on the days when we bought
total_capital_invested_2 <- abs(sum(buy_df$costs_proceeds[1:(nrow(buy_df))], na.rm = TRUE))
# Find the total profit or loss by adding the total costs proceeds on both the days when we bought and sold.
total_profit_or_loss_2<-sum(amd_df$costs_proceeds[1:(nrow(amd_df))], na.rm = TRUE) 
# Find ROI by dividing profit or loss by total capital invested, multiplying by 100 for a percentage.
ROI_2 <- (total_profit_or_loss_2 / total_capital_invested_2) * 100

print(total_profit_or_loss_2)
print(ROI_2)

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
Based on our results, there was a profit loss and negative ROI between the trading period of 01/01/2022 to 31/12/2022. Despite implementing the profit taking strategy, the profit and loss did not improve. This is indicated in the P/L in step 2 which was -$335456 which was respective to step 5, -$341933, where the profit taking strategy has been implemented. Correspondingly, the ROI had also worsened slightly from -27.43% to -27.96%. The cause of a profit loss was likely due to the "PC market weakened significantly in the quarter" addressed by Lisa Su, AMD's chair and chief executive. As the macroeconomic conditions continue to worsen during 2022, this also 'drove lower-than-expected PC demand and a significant inventory correction across the PC supply chain.' This included the aftermath of the pandemic, as the demand and supply chain experienced major disturbance.

Furthermore, in terms of the profit-taking strategy, it was targeted at selling half of the holding shares with the conditions of a 20% increase in the price by the average purchase price. However, as the prices of the shares progressively decreased during the year, from 150.24 to 64.77 as its closing of the year ended. This meant the strategy did not have an effective impact in improving the profit and ROI. This is as there were not many opportunities for the prices to go beyond 20% of the purchasing price due to the deteriorating macroeconomic conditions. Therefore, the shares declined, and shaped negative sloping, and thereby reduced an opportunity for it to sell. Ultimately, this leads to the profit making strategy as an ineffective way to improve its ROI and profits. 
```

Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.




