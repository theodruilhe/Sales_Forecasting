########## FINAL PROJECT - FDA ##########

# Authors: Anaïs BOUGHANEM, Théo DRUILHE and Sigurd SAUE

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(plotly)
library(purrr)
library(cluster)   # For silhouette analysis
library(factoextra) # For k-means visualization

###### Descriptive Statistics #####

data <- read_csv("data/walmart_cleaned_subset.csv")

# Convert the date column to date format
data <- data %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Dataset overview 
cat("Total Records:", nrow(data), "\n")  
cat("Unique Stores:", length(unique(data$Store)), "\n") 
cat("Unique Departments:", length(unique(data$Dept)), "\n")  

# Check for missing values 
missing_values <- colSums(is.na(data))
print(missing_values) 

# Descriptive statistics
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

# Time series analysis
time_series <- data %>%
  group_by(Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))  # Aggregate weekly sales by date

# Define holiday dates
holidays <- data.frame(
  Date = as.Date(c("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08",
                   "2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06",
                   "2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29",
                   "2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27")),
  Event = c(rep("Super Bowl", 4), rep("Labour Day", 4), rep("Thanksgiving", 4), rep("Christmas", 4))
)

# Create plotly graph for weekly sales
fig <- plot_ly(time_series, 
               x = ~Date, 
               y = ~Total_Weekly_Sales, 
               type = 'scatter', 
               mode = 'lines', 
               name = 'Weekly Sales',
               line = list(color = 'blue', width = 2))  
fig

# Graph of weekly sales with event lines
ggplot(time_series, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(color = "blue", size = 1) + 
  geom_vline(data = holidays, aes(xintercept = as.numeric(Date), color = Event), 
             linetype = "dashed", size = 1) +  
  scale_color_manual(values = c("Super Bowl" = "blue", 
                                "Labour Day" = "green", 
                                "Thanksgiving" = "orange", 
                                "Christmas" = "red")) + 
  labs(title = "Weekly Sales with Holiday Events",
       x = "Date",
       y = "Total Weekly Sales",
       color = "Event") +
  theme_minimal()

data <- data %>% mutate(Year = year(Date))  # Extract the year from the Date column
# Aggregate sales by store and year
store_year_sales <- data %>%
  group_by(Store, Year) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE)) %>%
  ungroup()  

# bar plot for weekly sales by store and year 
ggplot(store_year_sales, aes(x = as.factor(Store), y = Total_Weekly_Sales, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Total Weekly Sales by Store and Year", 
    x = "Store",  
    y = "Total Weekly Sales",  
    fill = "Year" 
  ) +
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graph of monthly sales
data <- data %>% mutate(Month = format(Date, "%Y-%m"))

monthly_sales <- data %>%
  group_by(Month) %>%
  summarise(Total_Monthly_Sales = sum(Weekly_Sales, na.rm = TRUE)) 

# Plotly graph for monthly sales
monthly_sales_fig <- plot_ly(monthly_sales, 
                             x = ~as.Date(paste0(Month, "-01")),  
                             y = ~Total_Monthly_Sales, 
                             type = 'scatter', 
                             mode = 'lines', 
                             name = 'Monthly Sales') %>%
  layout(title = "Monthly Sales Trend",  
         xaxis = list(title = "Month"),
         yaxis = list(title = "Total Monthly Sales"))

# Display the graph for monthly sales
monthly_sales_fig

data <- data %>% mutate(IsHoliday = ifelse(Date %in% holidays$Date, TRUE, FALSE))

average_sales <- data %>%
  group_by(IsHoliday) %>%
  summarise(Average_Sales = mean(Weekly_Sales, na.rm = TRUE))

ggplot(average_sales, aes(x = as.factor(IsHoliday), y = Average_Sales, fill = as.factor(IsHoliday))) +
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) + 
  labs(title = "Average Sales: Holiday vs Non-Holiday", x = "Holiday", y = "Avg Weekly Sales") + 
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
            "First regression spline" = "blue")

ggplot() +
  geom_point(data = agg_store_4, aes(x = time, y = Total_Weekly_Sales, color = "Values")) + 
  geom_line(data = agg_store_4, aes(x = time, y = y_pred, color = "First regression spline"), size = 1) + 
  theme_bw() +
  xlab("Time") + 
  ylab("Values") +
  scale_color_manual(values = colors) +
  labs(color = NULL)

# We have a pretty good fit but we have to add 50 knots to the spline for 
# permits to have a good fit durind the christmas period, which is a lot 
# and can lead to overfitting

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

# With manually added knots, we can have a better fits with less knots


# GENERALIZATION

agg_data <- time_series

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

# As for the store 4, we have a better fit with fewer knots and some
# specific knots around Christmas

# We now apply the spline to all stores

df_total_sales <- data %>%
  group_by(Store, Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE), .groups = "drop")

df_total_sales <- df_total_sales %>%
  group_by(Store) %>%
  arrange(Date) %>%
  mutate(Week = row_number()) %>% 
  ungroup()

# Create a B-spline basis
Spline_b_all_store <- create.bspline.basis(c(0, max(df_total_sales$Week)), breaks = knots_positions, norder = 3)

# We create a function to smooth the data
smooth_store <- function(df) {
  funList <- smooth.basis(df$Week, df$Total_Weekly_Sales, Spline_b_all_store)
  df$y_pred <- eval.fd(df$Week, funList$fd)  # Spline predictions
  return(df)
}

# We apply the spline to all stores
df_smoothed_sales <- df_total_sales %>%
  group_by(Store) %>%
  nest() %>%
  mutate(data = map(data, smooth_store)) %>%
  unnest(cols = data)

# We plot the 45 smoothed curves
ggplot(df_smoothed_sales, aes(x = Week, y = y_pred, color = as.factor(Store))) +
  geom_line() +
  labs(title = "Spline Smoothing of Weekly Sales",
       x = "Week",
       y = "Smoothed Sales") +  # Removed the color label
  theme_minimal() +
  theme(legend.position = "none")  # Hides the legend


###### REGISTRATION ######

# Aggreagate the weekly sales by store
agg_all_stores <- data %>%
  group_by(Store, Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE), .groups = "drop")

ggplot(agg_all_stores, aes(x = Date, y = Total_Weekly_Sales, color = as.factor(Store))) +
  geom_line() +
  labs(title = "Ventes Hebdomadaires par Magasin",
       x = "Date",
       y = "Total des Ventes Hebdomadaires",
       color = "Magasin") +
  theme_minimal()
# We have amplitude variation, but not phase variation, so registration
# could be use to align the data yearly

# We create a dataset oh the store 4 for inly dpt 1

store_4_dpt_1 <- data %>%
  filter(Store == 4, Dept == 1)

data_reg <- store_4_dpt_1 %>%
  mutate(
    year = year(Date),      # Extraire l'année
    week = week(Date)     # Extraire la semaine de l'année
  )

# 🔹 Tracer les ventes hebdomadaires superposées par année
ggplot(data_reg, aes(x = week, y = Weekly_Sales, color = as.factor(year))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolution of weekly sales by year (store 4, dpt 1)",
    x = "Weeks",
    y = "Sales",
    color = "Year"
  ) +
  theme_minimal()

# We have a phase problem here, we can use landmark registration

# Split the data for each year
data_2010 <- data_reg %>% filter(year == 2010)
data_2011 <- data_reg %>% filter(year == 2011)
data_2012 <- data_reg %>% filter(year == 2012)

# We apply splines for each year

time_2010 <- data_2010$week
Spline_b_2010 <- create.bspline.basis(range(time_2010), nbasis = 25, norder = 3)
funList_2010 = smooth.basis(time_2010, data_2010$Weekly_Sales, Spline_b_2010)
my_smooth_func_2010 = funList_2010$fd


time_2011 <- data_2011$week
Spline_b_2011 <- create.bspline.basis(range(time_2011), nbasis = 25, norder = 3)
funList_2011 <- smooth.basis(time_2011, data_2011$Weekly_Sales, Spline_b_2011)
my_smooth_func_2011 <- funList_2011$fd

time_2012 <- data_2012$week
Spline_b_2012 <- create.bspline.basis(range(time_2012), nbasis = 25, norder = 3)
funList_2012 <- smooth.basis(time_2012, data_2012$Weekly_Sales, Spline_b_2012)
my_smooth_func_2012 <- funList_2012$fd


# We plot the 3 splines
plot(my_smooth_func_2010, col = "blue", lwd = 2, ylim = c(15000, 100000),
     main = "Ventes hebdomadaires lissées (2010, 2011, 2012)", xlab = "Semaine", ylab = "Ventes")
lines(my_smooth_func_2011, col = "red", lwd = 2)  # Ajouter la courbe 2011
lines(my_smooth_func_2012, col = "green", lwd = 2)  # Ajouter la courbe 2012
legend("top", legend = c("2010", "2011", "2012"), col = c("blue", "red", "green"), lwd = 2)

# Landmark registration

# We create landmark when IsHolyday is equal to 1

landmark_2010 <- data_2010 %>%
  filter(IsHoliday == 1) %>%
  pull(week)

landmark_2011 <- data_2011 %>%
  filter(IsHoliday == 1) %>%
  pull(week)

landmark_2012 <- data_2012 %>%
  filter(IsHoliday == 1) %>%
  pull(week)

# Plot the splines with the landmarks

plot(my_smooth_func_2010, col = "blue", lwd = 2, ylim = c(15000, 100000),
     main = "Ventes hebdomadaires lissées (2010, 2011, 2012)", xlab = "Semaine", ylab = "Ventes")
lines(my_smooth_func_2011, col = "red", lwd = 2)  # Ajouter la courbe 2011
lines(my_smooth_func_2012, col = "green", lwd = 2)  # Ajouter la courbe 2012
points(landmark_2010, rep(91000, length(landmark_2010)), col = "blue", pch = 19)  # Ajouter les points de repère 2010
points(landmark_2011, rep(95000, length(landmark_2011)), col = "red", pch = 19)  # Ajouter les points de repère 2011
points(landmark_2012, rep(100000, length(landmark_2012)), col = "green", pch = 19)  # Ajouter les points de repère 2012
legend("top", legend = c("2010", "2011", "2012"), col = c("blue", "red", "green"), lwd = 2)


# We add 1 to the landmarks
landmark_2010 <- c(1, landmark_2010)
landmark_2011 <- c(1, landmark_2011)
landmark_2012 <- c(1, landmark_2012)

# Deterline the maximum number of landmarks
max_landmarks <- max(length(landmark_2010), length(landmark_2011), length(landmark_2012))

# Complete with NA the missing landmarks
landmark_2010 <- c(landmark_2010, rep(NA, max_landmarks - length(landmark_2010)))
landmark_2011 <- c(landmark_2011, rep(NA, max_landmarks - length(landmark_2011)))
landmark_2012 <- c(landmark_2012, rep(NA, max_landmarks - length(landmark_2012)))

# Create the landmark dataframe
landmark_df <- data.frame(
  id = rep(c("2010", "2011", "2012"), each = max_landmarks),
  position = c(landmark_2010, landmark_2011, landmark_2012),
  landmark = rep(1:max_landmarks, times = 3)
)

# Calculate the mean of the landmarks (ignoring the NA)
Mean_landmark <- aggregate(landmark_df$position, 
                           by = list(landmark_df$landmark), 
                           FUN = mean, 
                           na.rm = TRUE)$x

# We add thge means in the landmark dataframe
landmark_df$Mean_landmark = rep(Mean_landmark, 3)

# We plot it
ggplot(data = landmark_df,
       aes(as.factor(landmark), position, color=as.factor(id) )) +
  geom_point() +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Landmark number") + ylab("Time")

landmark_plot <- ggplot(landmark_df,
                     aes(x = Mean_landmark, y = position,
                         group = id, color=as.factor(id))) + geom_point() +
  geom_line() +
  theme_classic() +
  theme(legend.title=element_blank())+ 
  xlab("Time") + ylab("Time")+ 
  scale_x_continuous(limits = c(1, 53))+ 
  scale_y_continuous(limits = c(0, 53))
landmark_plot

# Since using IsHoliday == 1 does not seem relevant, we manually select the landmarks
# We observe peaks in the curves for each year

# Function to find the weeks with the maximum sales within defined ranges
find_max_week <- function(data, year, ranges) {
  max_weeks <- c()
  
  for (range in ranges) {
    subset_data <- data %>%
      filter(annee == year, semaine >= range[1], semaine <= range[2])
    
    if (nrow(subset_data) > 0) {
      max_week <- subset_data$semaine[which.max(subset_data$Weekly_Sales)]
      max_weeks <- c(max_weeks, max_week)
    } else {
      max_weeks <- c(max_weeks, NA)
    }
  }
  return(max_weeks)
}

# We choose intervals in which we have one peak
ranges_2010 <- list(c(6,10), c(20,30), c(30,48), c(48,53))
ranges_2011 <- list(c(6,10), c(20,30), c(30,48), c(48,53))
ranges_2012 <- list(c(6,10), c(20,30), c(30,43)) 

# We compute the new landmarks
landmark_2010 <- find_max_week(data_test, 2010, ranges_2010)
landmark_2011 <- find_max_week(data_test, 2011, ranges_2011)
landmark_2012 <- find_max_week(data_test, 2012, ranges_2012)

# We add 1
landmark_2010 <- c(1, landmark_2010)
landmark_2011 <- c(1, landmark_2011)
landmark_2012 <- c(1, landmark_2012, NA)


landmark_df_2 <- data.frame(
  id = rep(c("2010", "2011", "2012"), each = max_landmarks),  
  position = c(landmark_2010, landmark_2011, landmark_2012),  
  landmark = rep(1:max_landmarks, times = 3)
)

# Compute the average of landmarks by group while ignoring NA values
Mean_landmark_2 <- aggregate(landmark_df_2$position, 
                           by = list(landmark_df_2$landmark), 
                           FUN = mean, 
                           na.rm = TRUE)$x

# And we add it to the dataframe
landmark_df_2$Mean_landmark = rep(Mean_landmark_2, 3)

# We plot it
ggplot(data = landmark_df_2,
       aes(as.factor(landmark), position, color=as.factor(id) )) +
  geom_point() +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Landmark number") + ylab("Time")

landmark_df_2 <- ggplot(landmark_df_2,
                     aes(x = Mean_landmark, y = position,
                         group = id, color=as.factor(id))) + geom_point() +
  geom_line() +
  theme_classic() +
  theme(legend.title=element_blank())+ 
  xlab("Time") + ylab("Time")+ 
  scale_x_continuous(limits = c(1, 53))+ 
  scale_y_continuous(limits = c(1, 53))

landmark_df_2
# With the new landmarks, it seems more interestning to do a registration

# We use the Hermite spline to register the curves
Hermite_2010 = cm.spline(Mean_landmark, c(landmark_2010), n=53)
Hermite_2011 = cm.spline(Mean_landmark, c(landmark_2011), n=53)
Hermite_2012 = cm.spline(Mean_landmark, c(landmark_2012), n=53)

# We create a dataframe with the Hermites
landmark_df_hermite = data.frame(id = rep(c("A","B","C"),
                                          each = length(Hermite_2010$x)),
                                 position = c(Hermite_2010$x, Hermite_2011$x, Hermite_2012$x),
                                 landmark = c(Hermite_2010$y, Hermite_2011$y, Hermite_2012$y))

# We plot it
H_plot <- ggplot(landmark_df_hermite,
                 aes(x = position, y = landmark,
                     group = id, color=as.factor(id))) +
  geom_line() +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Time") + ylab("Time")+ scale_x_continuous(limits = c(1, 53))+ scale_y_continuous(limits = c(1, 53))
H_plot

# We register the sales curves
gam_2010_inv = approx(Hermite_2010$y, Hermite_2010$x, xout = data_2010$week)
gam_2011_inv = approx(Hermite_2011$y, Hermite_2011$x, xout = data_2011$week)
gam_2012_inv = approx(Hermite_2012$y, Hermite_2012$x, xout = data_2012$week)

# We compute the new sales curves
f_2010_regis_H = approx(gam_2010_inv$y, data_2010$Weekly_Sales, xout = data_2010$week)$y
f_2011_regis_H = approx(gam_2011_inv$y, data_2011$Weekly_Sales, xout = data_2011$week)$y
f_2012_regis_H = approx(gam_2012_inv$y, data_2012$Weekly_Sales, xout = data_2012$week)$y

Functions_regis = data.frame(
  t = c(data_2010$week, data_2011$week, data_2012$week),
  new_f = c(f_2010_regis_H, f_2011_regis_H, f_2012_regis_H),
  id = rep(c("A", "B", "C"), times = c(length(data_2010$week), length(data_2011$week), length(data_2012$week))),
  type = rep(c("Hermite", "Hermite", "Hermite"), times = c(length(data_2010$week), length(data_2011$week), length(data_2012$week)))
)
# We plot the new curves
F_fin <- ggplot(Functions_regis,
                aes(x = t, y = new_f,
                    color=as.factor(id))) +
  geom_line(size=1) +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Time") + ylab("Curve values")
suppressWarnings(print(F_fin))
# We can observe that it doesn't work. Due to the fact that 2010 and 2012
# are not complete in the dataset, and that we have multiple landmarks,
# we did not manage to to thes registration




#### FPCA ####
# Define the range of weeks
week_range <- c(0, max(df_smoothed_sales$Week))

# Create a B-spline basis for FPCA with corrected order
fpca_basis <- create.bspline.basis(week_range, breaks = knots_positions, norder = 4)

# Function to convert data into a functional data object
convert_to_fd <- function(df) {
  fdPar_obj <- fdPar(fpca_basis, Lfdobj = 2, lambda = 1e-2)  # Regularization
  smooth_fd <- smooth.basis(df$Week, df$y_pred, fdPar_obj)
  return(smooth_fd$fd)
}

# Apply functional transformation to all stores
fd_objects <- df_smoothed_sales %>%
  group_by(Store) %>%
  nest() %>%
  mutate(fd = map(data, convert_to_fd))

# Extract functional data objects
fd_list <- fd_objects$fd

# Manually combine multiple functional data objects into a single fd object
if (length(fd_list) > 1) {
  coef_matrix <- do.call(cbind, lapply(fd_list, function(fd) fd$coefs))  # Combine coefficient matrices
  fd_data <- fd(coef_matrix, fpca_basis)  # Create new fd object
} else {
  fd_data <- fd_list[[1]]
}

# Perform FPCA
fpca_result <- pca.fd(fd_data, nharm = 3)  # Extract 3 principal components

# Print FPCA summary
summary(fpca_result)

# Plot eigenfunctions (Principal Component Functions)
plot(fpca_result$harmonics, main = "Eigenfunctions (Principal Component Functions)", xlab = "Weeks", ylab = "Eigenfunctions")

# Plot variance explained
plot(cumsum(fpca_result$values) / sum(fpca_result$values), type = "b", 
     xlab = "Number of Components", ylab = "Cumulative Variance Explained", 
     main = "Scree Plot for FPCA")

# Scores for each store
fpca_scores <- fpca_result$scores
print(fpca_scores)


#### CLUSTERING ####

# Extract FPCA scores (each row represents a store)
fpca_scores <- as.data.frame(fpca_result$scores)

# Standardize the scores to prevent scale dominance
fpca_scores_scaled <- scale(fpca_scores)

# Determine optimal number of clusters using the Elbow method
fviz_nbclust(fpca_scores_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Clusters")

# Apply k-means clustering with optimal k 
set.seed(123)  # For reproducibility
k <- 4  # Chosen by the elbow method
kmeans_result <- kmeans(fpca_scores_scaled, centers = k, nstart = 25)

# Add cluster labels to data
fpca_scores$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters in 2D using the first two principal components
ggplot(fpca_scores, aes(x = V1, y = V2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Cluster Visualization using FPCA Scores", x = "FPCA 1", y = "FPCA 2")

# Perform hierarchical clustering
hc_result <- hclust(dist(fpca_scores_scaled), method = "ward.D2")

# Dendrogram visualization
plot(hc_result, labels = FALSE, main = "Hierarchical Clustering Dendrogram")

# Cut tree into k clusters and assign labels
hc_clusters <- cutree(hc_result, k)
fpca_scores$HC_Cluster <- as.factor(hc_clusters)

# Silhouette analysis to validate clustering quality
silhouette_score <- silhouette(kmeans_result$cluster, dist(fpca_scores_scaled))
fviz_silhouette(silhouette_score) + ggtitle("Silhouette Plot for Clustering")
