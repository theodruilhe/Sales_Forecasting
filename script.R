# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

data <- read_csv("Documents/M2_D3S/big_data/Part_FDA/Sales_Forecasting/data/walmart_cleaned_subset.csv")


# Descriptive Statistics
# Summary statistics for the entire dataset
summary(data)

# Group by store and department for more granular statistics
desc_stats <- data %>%
  group_by(Store, Dept) %>%
  summarise(
    Min_Sales = min(Weekly_Sales, na.rm = TRUE),
    Max_Sales = max(Weekly_Sales, na.rm = TRUE),
    Mean_Sales = mean(Weekly_Sales, na.rm = TRUE),
    Median_Sales = median(Weekly_Sales, na.rm = TRUE),
    StdDev_Sales = sd(Weekly_Sales, na.rm = TRUE)
  )
print(desc_stats)

# Time Series Plot
# Aggregate weekly sales by date
time_series <- data %>%
  group_by(Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# Plot the time series
ggplot(time_series, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(color = "blue") +
  labs(
    title = "Weekly Sales Over Time",
    x = "Date",
    y = "Total Weekly Sales"
  ) +
  theme_minimal()

# Optional: Boxplot of Weekly Sales by Store
ggplot(data, aes(x = as.factor(Store), y = Weekly_Sales)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Weekly Sales by Store",
    x = "Store",
    y = "Weekly Sales"
  ) +
  theme_minimal()

# Optional: Seasonal Analysis
# Add a column for month and year
data <- data %>%
  mutate(Month = format(Date, "%Y-%m"))

# Aggregate sales by month
monthly_sales <- data %>%
  group_by(Month) %>%
  summarise(Total_Monthly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# Plot monthly sales trend
ggplot(monthly_sales, aes(x = as.Date(paste0(Month, "-01")), y = Total_Monthly_Sales)) +
  geom_line(color = "green") +
  labs(
    title = "Monthly Sales Trend",
    x = "Month",
    y = "Total Monthly Sales"
  ) +
  theme_minimal()


install.packages("sf", configure.args = "--with-proj-lib=/usr/local/lib/")
