install.packages("fastICA")
library(fastICA)
library(fda)
library(ggplot2)
library(dplyr)
#load the dataframe
demand <- read.table('DemandBids.txt', header = TRUE)
# Remove negative data:
df <- demand[which(demand$Prezzo > 0), ] # 20355 were negative
# We notice that negative values are used to close hours
df$Data <- as.Date(df$Data)
summary(df)


# Plot a histogram of price distribution
ggplot(df, aes(x = PrezzoZonale)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "PrezzoZonale", y = "Frequency") +
  theme_minimal()

# Plot price evolution over 1 year (unreadabale)
ggplot(df, aes(x = Data, y = PrezzoZonale)) +
  geom_line() +
  labs(x = "Date", y = "PrezzoZonale") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme_minimal()

# Convert Date to Year and Month
df$Year <- as.numeric(format(df$Data, "%Y"))
df$Month <- format(df$Data, "%m")

# Calculate the average PrezzoZonale for each month
df_monthly_average <- df %>%
  group_by(Month) %>%
  summarize(Average_PrezzoZonale = mean(PrezzoZonale))

# Plot a bar chart for the average price per month
ggplot(df_monthly_average, aes(x = factor(Month), y = Average_PrezzoZonale, fill = Average_PrezzoZonale)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Month", y = "Average PrezzoZonale", title = "Average PrezzoZonale per Month") +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

# Subset data for the year 2022
# Calculate the average PrezzoZonale for each month in 2022
df_monthly_average_2022 <- df %>%
  filter(Year == 2022) %>%
  group_by(Month) %>%
  summarize(Average_PrezzoZonale = mean(PrezzoZonale))

# Calculate the average PrezzoZonale for each month in 2023
df_monthly_average_2023 <- df %>%
  filter(Year == 2023) %>%
  group_by(Month) %>%
  summarize(Average_PrezzoZonale = mean(PrezzoZonale))


# Plot for 2022
plot_2022 <- ggplot(df_monthly_average_2022, aes(x = factor(Month), y = Average_PrezzoZonale, fill = Average_PrezzoZonale)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Month", y = "Average PrezzoZonale 2022", title = "Average PrezzoZonale per Month (2022)") +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()


# Plot for 2023
plot_2023 <- ggplot(df_monthly_average_2023, aes(x = factor(Month), y = Average_PrezzoZonale, fill = Average_PrezzoZonale)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Month", y = "Average PrezzoZonale", title = "Average PrezzoZonale per Month (2023)") +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

# Display both plots
gridExtra::grid.arrange(plot_2022, plot_2023, ncol = 2)

# Convert Ora 
df$Ora <- factor(df$Ora, levels = 1:24)

# Calculate the average PrezzoZonale for each hour
df_hourly_average <- df %>%
  group_by(Ora) %>%
  summarize(Average_PrezzoZonale = mean(PrezzoZonale))

# Plot a bar chart for the average price per hour with a gradient color
ggplot(df_hourly_average, aes(x = Ora, y = Average_PrezzoZonale, fill = Average_PrezzoZonale)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Ora", y = "Average PrezzoZonale", title = "Average PrezzoZonale per Ora") +
  theme_minimal()

# Fonction de lissage des données
smoothed_data <- smooth.spline(x = as.numeric(df$Data), y = df$PrezzoZonale)

# Spécifier l'origine pour la conversion en date
origin_date <- as.Date("1970-01-01")

# Créer un nouveau dataframe avec les données lissées
df_smoothed <- data.frame(Data = as.Date(smoothed_data$x, origin = origin_date), PrezzoZonale = smoothed_data$y)

# Tracer le graphique avec les données originales et lissées
ggplot() +
  geom_line(data = df, aes(x = Data, y = PrezzoZonale), color = "blue", alpha = 0.5) +
  geom_line(data = df_smoothed, aes(x = Data, y = PrezzoZonale), color = "red", size = 1) +
  labs(x = "Date", y = "PrezzoZonale", title = "Smoothed Price Evolution over Time") +
  theme_minimal()

# Définir les points temporels uniformément espacés pour le lissage
time_points <- seq(min(df$Data), max(df$Data), length.out = nrow(df))

# Lissage des données de prix
smoothed_prices <- smooth.spline(x = as.numeric(df$Data), y = df$PrezzoZonale, spar = 0.8)

# Créer une fonction de prix lissée
smoothed_price_function <- approxfun(smoothed_prices, method = "constant", rule = 2)

# Créer un objet de matrice de design pour l'ICA
price_matrix <- t(sapply(time_points, function(t) smoothed_price_function(t)))

# Effectuer une ICA sur les données de prix
ica_result <- ica(price_matrix)

# Afficher les résultats
ica_result
