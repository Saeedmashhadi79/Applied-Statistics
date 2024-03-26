library(ggplot2)
library(dplyr)

#load the dataframe
file_name_csv="C:/Users/mvare/Desktop/POLIMI/POLIMI-S2/AS/Price_of_energy_Project/Price_of_energy_2023-01-01_to_2023-12-31.csv"
df=read.csv(file_name_csv)

df$Data <- as.Date(df$Data)
summary(df)


# Plot a histogram of price distribution
ggplot(df, aes(x = Prezzo)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "Prezzo", y = "Frequency") +
  theme_minimal()

# Plot price evolution over 1 year (unreadabale)
ggplot(df, aes(x = Data, y = Prezzo)) +
  geom_line() +
  labs(x = "Date", y = "Prezzo") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme_minimal()

# Extract the month from Date
df$Month <- format(df$Data, "%m")

# Calculate the average Prezzo for each month
df_monthly_average <- df %>%
  group_by(Month) %>%
  summarize(Average_Prezzo = mean(Prezzo))

# Plot a bar chart for the average price per month
ggplot(df_monthly_average, aes(x = factor(Month), y = Average_Prezzo, fill = Average_Prezzo)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Month", y = "Average Prezzo", title = "Average Prezzo per Month") +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

# Convert Ora 
df$Ora <- factor(df$Ora, levels = 1:24)

# Calculate the average Prezzo for each hour
df_hourly_average <- df %>%
  group_by(Ora) %>%
  summarize(Average_Prezzo = mean(Prezzo))

# Plot a bar chart for the average price per hour with a gradient color
ggplot(df_hourly_average, aes(x = Ora, y = Average_Prezzo, fill = Average_Prezzo)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Ora", y = "Average Prezzo", title = "Average Prezzo per Ora") +
  theme_minimal()
