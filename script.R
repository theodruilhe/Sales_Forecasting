########## FINAL PROJECT - FDA ##########

# Authors: Anaïs BOUGHANEM, Théo DRUILHE and Sigurd SAUE


library(dplyr)
library(ggplot2)
library(readr)

###### Descriptive Statistics #####

data <- read_csv("data/walmart_cleaned_subset.csv")

# AJOUTE CE QUE T'AS FAIT ICI ANAIS

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

# Aggregate weekly sales by date
agg_data <- data %>%
  group_by(Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# Plot the aggregated weekly sales over time
ggplot(time_series, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_point(color = "blue") +
  labs(
    title = "Weekly Sales Over Time",
    x = "Date",
    y = "Total Weekly Sales"
  ) +
  theme_minimal()

# Boxplot of Weekly Sales by Store
ggplot(data, aes(x = as.factor(Store), y = Weekly_Sales)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Weekly Sales by Store",
    x = "Store",
    y = "Weekly Sales"
  ) +
  theme_minimal()

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


# install.packages("sf", configure.args = "--with-proj-lib=/usr/local/lib/")


##### Data smoothing #####

# Choosing a store to isolate

# I choose to isolate store 4 bc it seems representative
# of the dataset looking the boxplot

store_4 <- data %>%
  filter(Store == 4)

# Aggregate weekly sales by date
agg_store_4 <- store_4 %>%
  group_by(Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# We plot
ggplot(agg_store_4, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_point(color = "deeppink") +
  labs(
    title = "Aggregate Sales for Store 4",
    x = "Date",
    y = "Weekly Sales"
  ) +
  theme_minimal()
# We have the same shape that for the global dataset

# B-splines

# We create a time vector t
time <- 1:143

# We fisrt do a Spline with 50 knots uniformly distributed
Spline_b <- create.bspline.basis(c(0,143), nbasis = 50, norder = 3)
plot(Spline_b)

funList = smooth.basis(time, agg_store_4$Total_Weekly_Sales, Spline_b)
my_smooth_func = funList$fd
plot(my_smooth_func)

y_pred = eval.fd(time, my_smooth_func)
agg_store_4$y_pred = y_pred

# We plot the raw data and the spline
colors <- c("Values" = "red",
            "First regression spline" = "#d2d9ff")

ggplot() +
  geom_point(data = agg_store_4, aes(x = time, y = Total_Weekly_Sales, color = "Values")) + 
  geom_line(data = agg_store_4, aes(x = time, y = y_pred, color = "First regression spline"), size = 1) + 
  theme_bw() +
  xlab("Time") + 
  ylab("Values") +
  scale_color_manual(values = colors) +
  labs(color = NULL)

# COMMENTS

# We now do a Spline with less knots but with some specific knots around Christmas

# Uniformly distributed knots over the year
knots_regular <- seq(1, 143, length.out = 20)

# Manually added knots around Christmas
knots_christmas_2010 <- c(43, 45, 47, 49)
knots_christmas_2011 <- knots_christmas_2010 + 52

# We sort the knots from both vectors
knots_positions <- sort(unique(c(knots_regular, knots_christmas_2010, knots_christmas_2011)))

# We create the spline
Spline_b_bis <- create.bspline.basis(rangeval = c(0, 143), breaks = knots_positions, norder = 3)
plot(Spline_b_bis)
funList_bis = smooth.basis(time, agg_store_4$Total_Weekly_Sales, Spline_b_bis)
my_smooth_func_bis = funList_bis$fd
plot(my_smooth_func_bis)

y_pred_bis = eval.fd(time, my_smooth_func_bis)
agg_store_4$y_pred_bis = y_pred_bis

# We plot the raw data and the spline
ggplot() +
  geom_point(data = agg_store_4, aes(x = time, y = Total_Weekly_Sales, color = "Values")) + 
  geom_line(data = agg_store_4, aes(x = time, y = y_pred_bis, color = "First regression spline"), size = 1) + 
  theme_bw() +
  xlab("Time") + 
  ylab("Values") +
  scale_color_manual(values = colors) +
  labs(color = NULL)

# COMMENTS


# GENERALIZATION

# We first do a Spline with 50 knots uniformly distributed
Spline_b_gen <- create.bspline.basis(c(0,143), nbasis = 50, norder = 3)
plot(Spline_b_gen)

funList_gen = smooth.basis(time, agg_data$Total_Weekly_Sales, Spline_b_gen)
my_smooth_func_gen = funList_gen$fd
plot(my_smooth_func_gen)

y_pred_gen = eval.fd(time, my_smooth_func_gen)
agg_data$y_pred_gen = y_pred_gen

# We plot the raw data and the spline
ggplot() +
  geom_point(data = agg_data, aes(x = time, y = Total_Weekly_Sales, color = "Values")) + 
  geom_line(data = agg_data, aes(x = time, y = y_pred_gen, color = "First regression spline"), size = 1) + 
  theme_bw() +
  xlab("Time") + 
  ylab("Values") +
  scale_color_manual(values = colors) +
  labs(color = NULL)

# COMMENTS

# We now do a Spline with fewer knots but with some specific knots around Christmas

# We create the spline
Spline_b_bis_gen <- create.bspline.basis(rangeval = c(0, 143), breaks = knots_positions, norder = 3)
plot(Spline_b_bis_gen)

funList_bis_gen = smooth.basis(time, agg_data$Total_Weekly_Sales, Spline_b_bis_gen)
my_smooth_func_bis_gen = funList_bis_gen$fd
plot(my_smooth_func_bis_gen)

y_pred_bis_gen = eval.fd(time, my_smooth_func_bis_gen)
agg_data$y_pred_bis_gen = y_pred_bis_gen

# We plot the raw data and the spline
ggplot() +
  geom_point(data = agg_data, aes(x = time, y = Total_Weekly_Sales, color = "Values")) + 
  geom_line(data = agg_data, aes(x = time, y = y_pred_bis_gen, color = "First regression spline"), size = 1) + 
  theme_bw() +
  xlab("Time") + 
  ylab("Values") +
  scale_color_manual(values = colors) +
  labs(color = NULL)


