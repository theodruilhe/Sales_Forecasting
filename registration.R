########## FINAL PROJECT - FDA ##########

# Authors: Anaïs BOUGHANEM, Théo DRUILHE and Sigurd SAUE


library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(fda)
###### Descriptive Statistics #####

data <- read_csv("data/walmart_cleaned_subset.csv")

# Convert Date column to Date format
data <- data %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# ---- Dataset Overview ----
cat("Total Records:", nrow(data), "\n")
cat("Unique Stores:", length(unique(data$Store)), "\n")
cat("Unique Departments:", length(unique(data$Dept)), "\n")

# ---- Check for Missing Values ----
missing_values <- colSums(is.na(data))
print(missing_values)

# ---- Descriptive Statistics ----
summary(data)

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

# ---- Time Series Analysis ----
time_series <- data %>%
  group_by(Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# Define holiday dates and labels
holidays <- data.frame(
  Date = as.Date(c("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08",
                   "2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06",
                   "2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29",
                   "2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27")),
  Event = c(rep("Super Bowl", 4), rep("Labour Day", 4), rep("Thanksgiving", 4), rep("Christmas", 4))
)

# ---- Plot the Time Series with Holidays ----
ggplot(time_series, aes(x = Date, y = Total_Weekly_Sales)) +
  geom_line(color = "blue") +
  geom_vline(data = holidays, aes(xintercept = as.numeric(Date), color = Event), linetype = "dashed") +
  scale_color_manual(values = c("Super Bowl" = "red", "Labour Day" = "green", "Thanksgiving" = "purple", "Christmas" = "orange")) +
  labs(
    title = "Weekly Sales Over Time with Holiday Markers",
    x = "Date",
    y = "Total Weekly Sales",
    color = "Holiday"
  ) +
  theme_minimal()

# ---- Boxplot of Weekly Sales by Store ----
ggplot(data, aes(x = as.factor(Store), y = Weekly_Sales)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Distribution of Weekly Sales by Store",
    x = "Store",
    y = "Weekly Sales"
  ) +
  theme_minimal()

# ---- Seasonal Analysis: Aggregate sales by month ----
data <- data %>% mutate(Month = format(Date, "%Y-%m"))
monthly_sales <- data %>%
  group_by(Month) %>%
  summarise(Total_Monthly_Sales = sum(Weekly_Sales, na.rm = TRUE))

ggplot(monthly_sales, aes(x = as.Date(paste0(Month, "-01")), y = Total_Monthly_Sales)) +
  geom_line(color = "green") +
  labs(
    title = "Monthly Sales Trend",
    x = "Month",
    y = "Total Monthly Sales"
  ) +
  theme_minimal()

# ---- Holiday Impact Analysis ----
data <- data %>% mutate(IsHoliday = ifelse(Date %in% holidays$Date, 1, 0))

holiday_sales <- data %>%
  group_by(IsHoliday) %>%
  summarise(Average_Sales = mean(Weekly_Sales, na.rm = TRUE))
print(holiday_sales)

ggplot(holiday_sales, aes(x = as.factor(IsHoliday), y = Average_Sales, fill = as.factor(IsHoliday))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Impact of Holidays on Sales",
    x = "Holiday (1=Yes, 0=No)",
    y = "Average Weekly Sales"
  ) +
  theme_minimal()

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


library(dplyr)


###### REGISTRATION ######

# Agréger les ventes hebdomadaires pour tous les magasins
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
# We have amplitude variation, but not phase variation

# We create a dataset oh the store 4 for inly dpt 1

store_4_dpt_1 <- data %>%
  filter(Store == 4, Dept == 1)

data_test <- store_4_dpt_1 %>%
  mutate(
    annee = year(Date),      # Extraire l'année
    semaine = week(Date)     # Extraire la semaine de l'année
  )

# 🔹 Tracer les ventes hebdomadaires superposées par année
ggplot(data_test, aes(x = semaine, y = Weekly_Sales, color = as.factor(annee))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Évolution des ventes hebdomadaires par année",
    x = "Semaine de l'année",
    y = "Ventes",
    color = "Année"
  ) +
  theme_minimal()

# We have a phase problem here, we can use landmark registration

# Séparer les données par année
data_2010 <- data_test %>% filter(annee == 2010)
data_2011 <- data_test %>% filter(annee == 2011)
data_2012 <- data_test %>% filter(annee == 2012)

# On applique les splines pour chaque année

# 2010

# We create the spline

time_2010 <- data_2010$semaine
Spline_b_2010 <- create.bspline.basis(range(time_2010), nbasis = 25, norder = 3)

funList_2010 = smooth.basis(time_2010, data_2010$Weekly_Sales, Spline_b_2010)
my_smooth_func_2010 = funList_2010$fd
plot(my_smooth_func_2010)


# 🔹 Lissage des ventes pour 2011 (copie exacte du code 2010)
time_2011 <- data_2011$semaine
Spline_b_2011 <- create.bspline.basis(range(time_2011), nbasis = 25, norder = 3)
funList_2011 <- smooth.basis(time_2011, data_2011$Weekly_Sales, Spline_b_2011)
my_smooth_func_2011 <- funList_2011$fd

# 🔹 Lissage des ventes pour 2012 (copie exacte du code 2010)
time_2012 <- data_2012$semaine
Spline_b_2012 <- create.bspline.basis(range(time_2012), nbasis = 25, norder = 3)
funList_2012 <- smooth.basis(time_2012, data_2012$Weekly_Sales, Spline_b_2012)
my_smooth_func_2012 <- funList_2012$fd


# 🔹 Tracer les 3 courbes lissées ensemble
plot(my_smooth_func_2010, col = "blue", lwd = 2, ylim = c(15000, 100000),
     main = "Ventes hebdomadaires lissées (2010, 2011, 2012)", xlab = "Semaine", ylab = "Ventes")
lines(my_smooth_func_2011, col = "red", lwd = 2)  # Ajouter la courbe 2011
lines(my_smooth_func_2012, col = "green", lwd = 2)  # Ajouter la courbe 2012

# Ajouter une légende
legend("top", legend = c("2010", "2011", "2012"), col = c("blue", "red", "green"), lwd = 2)

# Landmark registration

# We create landmark when holyday is equal to 1

landmark_2010 <- data_2010 %>%
  filter(IsHoliday == 1) %>%
  pull(semaine)

landmark_2011 <- data_2011 %>%
  filter(IsHoliday == 1) %>%
  pull(semaine)

landmark_2012 <- data_2012 %>%
  filter(IsHoliday == 1) %>%
  pull(semaine)

# 🔹 Tracer les courbes lissées avec les points de repère

plot(my_smooth_func_2010, col = "blue", lwd = 2, ylim = c(15000, 100000),
     main = "Ventes hebdomadaires lissées (2010, 2011, 2012)", xlab = "Semaine", ylab = "Ventes")
lines(my_smooth_func_2011, col = "red", lwd = 2)  # Ajouter la courbe 2011
lines(my_smooth_func_2012, col = "green", lwd = 2)  # Ajouter la courbe 2012
points(landmark_2010, rep(91000, length(landmark_2010)), col = "blue", pch = 19)  # Ajouter les points de repère 2010
points(landmark_2011, rep(95000, length(landmark_2011)), col = "red", pch = 19)  # Ajouter les points de repère 2011
points(landmark_2012, rep(100000, length(landmark_2012)), col = "green", pch = 19)  # Ajouter les points de repère 2012
legend("top", legend = c("2010", "2011", "2012"), col = c("blue", "red", "green"), lwd = 2)


# Ajouter 1 (début) et 52 (fin) aux landmarks existants
landmark_2010 <- c(1, landmark_2010)
landmark_2011 <- c(1, landmark_2011)
landmark_2012 <- c(1, landmark_2012)

# Déterminer la longueur maximale des landmarks
max_landmarks <- max(length(landmark_2010), length(landmark_2011), length(landmark_2012))

# Compléter les listes avec NA pour uniformiser leur taille
landmark_2010 <- c(landmark_2010, rep(NA, max_landmarks - length(landmark_2010)))
landmark_2011 <- c(landmark_2011, rep(NA, max_landmarks - length(landmark_2011)))
landmark_2012 <- c(landmark_2012, rep(NA, max_landmarks - length(landmark_2012)))

# Créer le dataframe des landmarks
landmark_df <- data.frame(
  id = rep(c("2010", "2011", "2012"), each = max_landmarks),  # A = 2010, B = 2011, C = 2012
  position = c(landmark_2010, landmark_2011, landmark_2012),  # Positions des landmarks
  landmark = rep(1:max_landmarks, times = 3)  # Identifiant du landmark (1, 2, 3, ...)
)

# Vérifier la structure du dataframe
landmark_df

# Calculer la moyenne des landmarks par groupe en ignorant les NA
Mean_landmark <- aggregate(landmark_df$position, 
                           by = list(landmark_df$landmark), 
                           FUN = mean, 
                           na.rm = TRUE)$x

# Répéter la moyenne pour remplir le dataframe
landmark_df$Mean_landmark = rep(Mean_landmark, 3)
# Vérifier la structure du dataframe
print(landmark_df)

ggplot(data = landmark_df,
       aes(as.factor(landmark), position, color=as.factor(id) )) +
  geom_point() +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Landmark number") + ylab("Time")

thrid_plot <- ggplot(landmark_df,
                     aes(x = Mean_landmark, y = position,
                         group = id, color=as.factor(id))) + geom_point() +
  geom_line() +
  theme_classic() +
  theme(legend.title=element_blank())+ 
  xlab("Time") + ylab("Time")+ 
  scale_x_continuous(limits = c(1, 53))+ 
  scale_y_continuous(limits = c(0, 53))
thrid_plot

# Ca ne semble pas pertinent avec les IsHolyday égal 1, on choisit donc les points manuellement
# On remarque des pics sur les courbes dde chaque année

# Fonction pour trouver les semaines de max dans des plages définies
find_max_week <- function(data, year, ranges) {
  max_weeks <- c()  # Initialisation de la liste des semaines max
  
  for (range in ranges) {
    subset_data <- data %>%
      filter(annee == year, semaine >= range[1], semaine <= range[2])
    
    if (nrow(subset_data) > 0) {
      max_week <- subset_data$semaine[which.max(subset_data$Weekly_Sales)]
      max_weeks <- c(max_weeks, max_week)
    } else {
      max_weeks <- c(max_weeks, NA)  # Ajouter NA si aucune donnée dans la plage
    }
  }
  return(max_weeks)
}

# Définition des plages de semaines pour chaque année
ranges_2010 <- list(c(6,10), c(20,30), c(30,48), c(48,53))
ranges_2011 <- list(c(6,10), c(20,30), c(30,48), c(48,53))
ranges_2012 <- list(c(6,10), c(20,30), c(30,43))  # Jusqu'à semaine 43 pour 2012

# Calcul des landmarks par année
landmark_2010 <- find_max_week(data_test, 2010, ranges_2010)
landmark_2011 <- find_max_week(data_test, 2011, ranges_2011)
landmark_2012 <- find_max_week(data_test, 2012, ranges_2012)

# Ajouter 1 au début de chaque liste
landmark_2010 <- c(1, landmark_2010)
landmark_2011 <- c(1, landmark_2011)
landmark_2012 <- c(1, landmark_2012, NA)

# Vérifier les résultats
print(landmark_2010)
print(landmark_2011)
print(landmark_2012)


landmark_df <- data.frame(
  id = rep(c("2010", "2011", "2012"), each = max_landmarks),  # A = 2010, B = 2011, C = 2012
  position = c(landmark_2010, landmark_2011, landmark_2012),  # Positions des landmarks
  landmark = rep(1:max_landmarks, times = 3)  # Identifiant du landmark (1, 2, 3, ...)
)

# Vérifier la structure du dataframe
landmark_df

# Calculer la moyenne des landmarks par groupe en ignorant les NA
Mean_landmark <- aggregate(landmark_df$position, 
                           by = list(landmark_df$landmark), 
                           FUN = mean, 
                           na.rm = TRUE)$x

# Répéter la moyenne pour remplir le dataframe
landmark_df$Mean_landmark = rep(Mean_landmark, 3)
# Vérifier la structure du dataframe
print(landmark_df)



ggplot(data = landmark_df,
       aes(as.factor(landmark), position, color=as.factor(id) )) +
  geom_point() +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Landmark number") + ylab("Time")

thrid_plot <- ggplot(landmark_df,
                     aes(x = Mean_landmark, y = position,
                         group = id, color=as.factor(id))) + geom_point() +
  geom_line() +
  theme_classic() +
  theme(legend.title=element_blank())+ 
  xlab("Time") + ylab("Time")+ 
  scale_x_continuous(limits = c(1, 53))+ 
  scale_y_continuous(limits = c(1, 53))
thrid_plot

Hermite_2010 = cm.spline(Mean_landmark, c(landmark_2010), n=53)
Hermite_2011 = cm.spline(Mean_landmark, c(landmark_2011), n=53)
Hermite_2012 = cm.spline(Mean_landmark, c(landmark_2012), n=53)

landmark_df_hermite = data.frame(id = rep(c("A","B","C"),
                                          each = length(Hermite_2010$x)),
                                 position = c(Hermite_2010$x, Hermite_2011$x, Hermite_2012$x),
                                 landmark = c(Hermite_2010$y, Hermite_2011$y, Hermite_2012$y))
H_plot <- ggplot(landmark_df_hermite,
                 aes(x = position, y = landmark,
                     group = id, color=as.factor(id))) +
  geom_line() +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Time") + ylab("Time")+ scale_x_continuous(limits = c(1, 53))+ scale_y_continuous(limits = c(1, 53))
H_plot
#See slide 41 of the course. First, estimate inverse warping.




gam_A_inv = approx(Hermite_2010$y, Hermite_2010$x, xout = data_2010$semaine)
gam_B_inv = approx(Hermite_2011$y, Hermite_2011$x, xout = data_2011$semaine)
gam_C_inv = approx(Hermite_2012$y, Hermite_2012$x, xout = data_2012$semaine)

f_A_regis_H = approx(gam_A_inv$y, data_2010$Weekly_Sales, xout = data_2010$semaine)$y
f_B_regis_H = approx(gam_B_inv$y, data_2011$Weekly_Sales, xout = data_2011$semaine)$y
f_C_regis_H = approx(gam_C_inv$y, data_2012$Weekly_Sales, xout = data_2012$semaine)$y
time <- seq(1, 53, by = 1)
Functions_regis = data.frame(
  t = c(data_2010$semaine, data_2011$semaine, data_2012$semaine),
  new_f = c(f_A_regis_H, f_B_regis_H, f_C_regis_H),
  id = rep(c("A", "B", "C"), times = c(length(data_2010$semaine), length(data_2011$semaine), length(data_2012$semaine))),
  type = rep(c("Hermite", "Hermite", "Hermite"), times = c(length(data_2010$semaine), length(data_2011$semaine), length(data_2012$semaine)))
)

F_fin <- ggplot(Functions_regis,
                aes(x = t, y = new_f,
                    color=as.factor(id))) +
  geom_line(size=1) +
  theme_classic() + theme(legend.title=element_blank())+ xlab("Time") + ylab("Curve values")
suppressWarnings(print(F_fin))

