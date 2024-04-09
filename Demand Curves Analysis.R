# Demand bids analysis:
# Demand bids are taken into account for each hour, using data coming from GME
# from 2022-01-01 to 2023-12-31 (YYYY-MM-DD) at each day and hour,
# analyzing these functions (knowing PrezzoZonale too, the intersection) without
# confrontation with Offer curves for now.
# Only the pool where all the zones are present is taken into account.

# Libraries and data cleaning ---------------------------------------------
library(dplyr)
library(lubridate)
library(fda)
library(glmnet)
library(Matrix)
library(monotone)
# Necessary to work with "month" and "day" functions

demand <- read.table('DemandBids.txt', header = TRUE)
# Remove negative data:
demand <- demand[which(demand$Prezzo > 0), ] # 20355 were negative
# We notice that negative values are used to close hours

# Working in hours, Prezzo is a function of volume; in days, months and years,
# required output is PrezzoZonale. Converting dates in a more suitable form:

demand$Anno <- year(demand$Data)
demand$Mese <- month(demand$Data)
demand$Giorno <- day(demand$Data)
demand$GiornoSettimana <- weekdays(as.Date(demand$Data))

demand$GiornoSettimana <- factor(demand$GiornoSettimana,
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                            "Saturday", "Sunday"), ordered = TRUE)

# Cumulative quantity (Called Volume):
demand <- demand %>% 
  group_by(Data, Ora) %>% 
  mutate(Volume = cumsum(Quantita)) %>% 
  ungroup()

# Visualization
head(demand)
tail(demand)
dim(demand)
dimnames(demand)

demand.2022 <- demand[which(demand$Anno==2022),]
demand.2023 <- demand[which(demand$Anno==2023),]
save(demand, file = 'demand.RData')
save(demand.2022, file = 'demand.2022.RData')
save(demand.2023, file = 'demand.2023.RData')

# PRELIMINARY EDA ---------------------------------------------------------
# Treating hours separately (and considering daily time series), we look at
# similarities within day type and month. So: hour/day/month (for two and a half
# year). We notice trends in the data.

load('demand.RData')
load('demand.2022.RData')
load('demand.2023.RData')

# Boxplots ----------------------------------------------------------------

# Boxplot of PrezzoZonale in hours:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$Ora, ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by hour in 2022',
        xlim = c(0,24), col = rainbow(24))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$Ora, ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by hour in 2023',
        xlim = c(0,24), col = rainbow(24))

# Re-scaling them:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$Ora, ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by hour in 2022',
        xlim = c(0,24), col = rainbow(24), ylim = range(demand$PrezzoZonale))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$Ora, ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by hour in 2023',
        xlim = c(0,24), col = rainbow(24), ylim = range(demand$PrezzoZonale))

# Clearing price is much higher in 2022 rather than in 2023, with an higher
# variability. In both cases there's a peak in late hours and in the morning
# (more or less at nine o'clock).

# Boxplot of PrezzoZonale in months:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$Mese, ylab = 'Clearing price',
        xlab = 'Month', main = 'Clearing price by month in 2022',
        xlim = c(0,12), col = rainbow(12))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$Mese, ylab = 'Clearing price',
        xlab = 'Month', main = 'Clearing price by month in 2023',
        xlim = c(0,12), col = rainbow(12))

# Re-scaling them:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$Mese, ylab = 'Clearing price',
        xlab = 'Month', main = 'Clearing price by month in 2022',
        xlim = c(0,12), col = rainbow(12), ylim = range(demand$PrezzoZonale))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$Mese, ylab = 'Clearing price',
        xlab = 'Month', main = 'Clearing price by month in 2023',
        xlim = c(0,12), col = rainbow(12), ylim = range(demand$PrezzoZonale))

# Again, clearing price and variability are much higher in 2022 (probably due
# to external causes). In 2022 there is an anomalous peak in Summer, and a local
# peak in March. In 2023 winter months have the highest spot price.

# Boxplot of PrezzoZonale in days:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$Giorno, ylab = 'Clearing price',
        xlab = 'Day', main = 'Clearing price by day in 2022',
        xlim = c(0,31), col = rainbow(31))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$Giorno, ylab = 'Clearing price',
        xlab = 'Day', main = 'Clearing price by day in 2023',
        xlim = c(0,31), col = rainbow(31))

# Re-scaling them:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$Giorno,
        ylab = 'Clearing price',
        xlab = 'Day', main = 'Clearing price by day in 2022',
        col = rainbow(31), ylim = range(demand$PrezzoZonale))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$Giorno,
        ylab = 'Clearing price',
        xlab = 'Day', main = 'Clearing price by day in 2023',
        col = rainbow(31), ylim = range(demand$PrezzoZonale))

# Boxplot of PrezzoZonale in weekdays:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$GiornoSettimana, ylab = 'Clearing price',
        xlab = 'WeekDay', main = 'Clearing price by weekday in 2022',
        col = rainbow(7))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$GiornoSettimana, ylab = 'Clearing price',
        xlab = 'WeekDay', main = 'Clearing price by weekday in 2023',
        col = rainbow(7))

# Re-scaling them:
par(mfrow=c(2,1))
boxplot(demand.2022$PrezzoZonale ~ demand.2022$GiornoSettimana,
        ylab = 'Clearing price',
        xlab = 'Day', main = 'Clearing price by weekday in 2022',
        col = rainbow(7), ylim = range(demand$PrezzoZonale))
boxplot(demand.2023$PrezzoZonale ~ demand.2023$GiornoSettimana,
        ylab = 'Clearing price',
        xlab = 'Day', main = 'Clearing price by weekday in 2023',
        col = rainbow(7), ylim = range(demand$PrezzoZonale))

# Evaluation of statistical parameters (clearing price) ----------------

PrezzoZonale.2022_mean = mean(demand.2022$PrezzoZonale)
PrezzoZonale.2022_sd = sd(demand.2022$PrezzoZonale)
PrezzoZonale.2023_mean = mean(demand.2023$PrezzoZonale)
PrezzoZonale.2023_sd = sd(demand.2023$PrezzoZonale)

# Clearing price by months - 2022
PrezzoZonale.2022.months.mean = NULL
PrezzoZonale.2022.months.var = NULL
PrezzoZonale.2022.months.sd = NULL
for (i in 1:12){
  PrezzoZonale.2022.months.mean[i] = mean(demand.2022$PrezzoZonale
                                          [which(demand.2022$Mese==i)])
  PrezzoZonale.2022.months.var[i] = var(demand.2022$PrezzoZonale
                                        [which(demand.2022$Mese==i)])
  PrezzoZonale.2022.months.sd[i] = sd(demand.2022$PrezzoZonale
                                      [which(demand.2022$Mese==i)])
}
# Clearing price by hours - 2022
PrezzoZonale.2022.hours.mean = NULL
PrezzoZonale.2022.hours.var = NULL
PrezzoZonale.2022.hours.sd = NULL
for (i in 1:24){
  PrezzoZonale.2022.hours.mean[i] = mean(demand.2022$PrezzoZonale
                                         [which(demand.2022$Ora==i)])
  PrezzoZonale.2022.hours.var[i] = var(demand.2022$PrezzoZonale
                                       [which(demand.2022$Ora==i)])
  PrezzoZonale.2022.hours.sd[i] = sd(demand.2022$PrezzoZonale
                                     [which(demand.2022$Ora==i)])
}
# Clearing price by days - 2022
PrezzoZonale.2022.days.mean = NULL
PrezzoZonale.2022.days.var = NULL
PrezzoZonale.2022.days.sd = NULL
for (i in 1:31){
  PrezzoZonale.2022.days.mean[i] = mean(demand.2022$PrezzoZonale
                                        [which(demand.2022$Giorno==i)])
  PrezzoZonale.2022.days.var[i] = var(demand.2022$PrezzoZonale
                                      [which(demand.2022$Giorno==i)])
  PrezzoZonale.2022.days.sd[i] = sd(demand.2022$PrezzoZonale
                                    [which(demand.2022$Giorno==i)])
}
# Clearing price by weekdays - 2022
PrezzoZonale.2022.weekdays.mean = NULL
PrezzoZonale.2022.weekdays.var = NULL
PrezzoZonale.2022.weekdays.sd = NULL
find_weekday_position <- function(levels_vector, target_level) {
  return(match(target_level, levels_vector))
}

for (i in levels(demand$GiornoSettimana)){
  WD <- find_weekday_position(levels(demand$GiornoSettimana), i)
  PrezzoZonale.2022.weekdays.mean[WD] = mean(demand.2022$PrezzoZonale
                                             [which(demand.2022$GiornoSettimana==i)])
  PrezzoZonale.2022.weekdays.var[WD] = var(demand.2022$PrezzoZonale
                                           [which(demand.2022$GiornoSettimana==i)])
  PrezzoZonale.2022.weekdays.sd[WD] = sd(demand.2022$PrezzoZonale
                                         [which(demand.2022$GiornoSettimana==i)])
}

# Clearing price by months - 2023
PrezzoZonale.2023.months.mean = NULL
PrezzoZonale.2023.months.var = NULL
PrezzoZonale.2023.months.sd = NULL
for (i in 1:12){
  PrezzoZonale.2023.months.mean[i] = mean(demand.2023$PrezzoZonale
                                          [which(demand.2023$Mese==i)])
  PrezzoZonale.2023.months.var[i] = var(demand.2023$PrezzoZonale
                                        [which(demand.2023$Mese==i)])
  PrezzoZonale.2023.months.sd[i] = sd(demand.2023$PrezzoZonale
                                      [which(demand.2023$Mese==i)])
}
# Clearing price by hours - 2023
PrezzoZonale.2023.hours.mean = NULL
PrezzoZonale.2023.hours.var = NULL
PrezzoZonale.2023.hours.sd = NULL
for (i in 1:24){
  PrezzoZonale.2023.hours.mean[i] = mean(demand.2023$PrezzoZonale
                                         [which(demand.2023$Ora==i)])
  PrezzoZonale.2023.hours.var[i] = var(demand.2023$PrezzoZonale
                                       [which(demand.2023$Ora==i)])
  PrezzoZonale.2023.hours.sd[i] = sd(demand.2023$PrezzoZonale
                                     [which(demand.2023$Ora==i)])
}
# Clearing price by days - 2023
PrezzoZonale.2023.days.mean = NULL
PrezzoZonale.2023.days.var = NULL
PrezzoZonale.2023.days.sd = NULL
for (i in 1:31){
  PrezzoZonale.2023.days.mean[i] = mean(demand.2023$PrezzoZonale
                                        [which(demand.2023$Giorno==i)])
  PrezzoZonale.2023.days.var[i] = var(demand.2023$PrezzoZonale
                                      [which(demand.2023$Giorno==i)])
  PrezzoZonale.2023.days.sd[i] = sd(demand.2023$PrezzoZonale
                                    [which(demand.2023$Giorno==i)])
}
# Clearing price by weekdays - 2023
PrezzoZonale.2023.weekdays.mean = NULL
PrezzoZonale.2023.weekdays.var = NULL
PrezzoZonale.2023.weekdays.sd = NULL
find_weekday_position <- function(levels_vector, target_level) {
  return(match(target_level, levels_vector))
}

for (i in levels(demand$GiornoSettimana)){
  WD <- find_weekday_position(levels(demand$GiornoSettimana), i)
  PrezzoZonale.2023.weekdays.mean[WD] = mean(demand.2023$PrezzoZonale
                                             [which(demand.2023$GiornoSettimana==i)])
  PrezzoZonale.2023.weekdays.var[WD] = var(demand.2023$PrezzoZonale
                                           [which(demand.2023$GiornoSettimana==i)])
  PrezzoZonale.2023.weekdays.sd[WD] = sd(demand.2023$PrezzoZonale
                                         [which(demand.2023$GiornoSettimana==i)])
}


# Scatter plots -----------------------------------------------------------

# A) Means (months):
month_mean <- function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.months.mean, pch = 16, xlab = 'Months in 2022',
       ylab = 'Mean clearing price', col = 'blue',
       ylim = c(0,600))
  abline(h = PrezzoZonale.2022_mean, lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.months.mean, pch = 16, xlab = 'Months in 2023',
       ylab = 'Mean clearing price', col = 'red',
       ylim = c(0,600))
  abline(h = PrezzoZonale.2023_mean, lty = 2, lwd = 2)
}
month_mean()
#

# B) Means (hours):
hours_mean <- function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.hours.mean, pch = 16, xlab = 'Hours in 2022',
       ylab = 'Mean clearing price', col = 'blue',
       ylim = c(0,400))
  abline(h = PrezzoZonale.2022_mean, lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.hours.mean, pch = 16, xlab = 'Hours in 2023',
       ylab = 'Mean clearing price', col = 'red',
       ylim = c(0,400))
  abline(h = PrezzoZonale.2023_mean, lty = 2, lwd = 2)
}
hours_mean()

# C) Means (days):
day_mean <- function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.days.mean, pch = 16, xlab = 'Days in 2022',
       ylab = 'Mean clearing price', col = 'blue',
       ylim = c(0,600))
  abline(h = PrezzoZonale.2022_mean, lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.days.mean, pch = 16, xlab = 'Days in 2023',
       ylab = 'Mean clearing price', col = 'red',
       ylim = c(0,600))
  abline(h = PrezzoZonale.2023_mean, lty = 2, lwd = 2)
}
day_mean()
#

# D) Means (weekdays):
weekday_mean <- function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.weekdays.mean, pch = 16, xlab = 'Weekdays in 2022',
       ylab = 'Mean clearing price', col = 'blue',
       ylim = c(0,400))
  abline(h = PrezzoZonale.2022_mean, lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.weekdays.mean, pch = 16, xlab = 'Weekdays in 2023',
       ylab = 'Mean clearing price', col = 'red',
       ylim = c(0,400))
  abline(h = PrezzoZonale.2023_mean, lty = 2, lwd = 2)
}
weekday_mean()

# E) Standard deviation (months):
month_sd <- function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.months.sd, pch = 16, xlab = 'Months in 2022',
       ylab = 'Clearing price standard deviation', col = 'blue',
       ylim = c(10, 100))
  abline(h = mean(PrezzoZonale.2022.months.sd), lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.months.sd, pch = 16, xlab = 'Months in 2023',
       ylab = 'Clearing price standard deviation', col = 'red',
       ylim = c(10, 100))
  abline(h = mean(PrezzoZonale.2023.months.sd), lty = 2, lwd = 2)
}
month_sd()
# We don't compare here with total variability because it doesn't make sense,
# only with the mean of all the standard deviations

# F) Standard deviation (hours):
hours_sd = function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.hours.sd, pch = 16, xlab = 'Hours in 2022',
       ylab = 'Clearing price standard deviation', col = 'blue',
       ylim = c(10, 160))
  abline(h = mean(PrezzoZonale.2022.hours.sd), lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.hours.sd, pch = 16, xlab = 'Hours in 2023',
       ylab = 'Clearing price standard deviation', col = 'red',
       ylim = c(10, 160))
  abline(h = mean(PrezzoZonale.2023.hours.sd), lty = 2, lwd = 2)
}
hours_sd()

# G) sds (days):
day_sd <- function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.days.sd, pch = 16, xlab = 'Days in 2022',
       ylab = 'sd clearing price', col = 'blue',
       ylim = c(0,600))
  abline(h = PrezzoZonale.2022_sd, lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.days.sd, pch = 16, xlab = 'Days in 2023',
       ylab = 'sd clearing price', col = 'red',
       ylim = c(0,600))
  abline(h = PrezzoZonale.2023_sd, lty = 2, lwd = 2)
}
day_sd()
#

# H) sds (weekdays):
weekday_sd <- function(){
  par(mfrow = c(1,2))
  plot(PrezzoZonale.2022.weekdays.sd, pch = 16, xlab = 'Weekdays in 2022',
       ylab = 'sd clearing price', col = 'blue',
       ylim = c(0,400))
  abline(h = PrezzoZonale.2022_sd, lty = 2, lwd = 2)
  plot(PrezzoZonale.2023.weekdays.sd, pch = 16, xlab = 'Weekdays in 2023',
       ylab = 'sd clearing price', col = 'red',
       ylim = c(0,400))
  abline(h = PrezzoZonale.2023_sd, lty = 2, lwd = 2)
}
weekday_sd()


# Matplots ---------------------------------------------------------------
# BY MONTHS:
# In 2022:
PrezzoZonale.2022.OraMese <- matrix(rep(0, 24*12), 24, 12)
for (i in 1:12){
  for (j in 1:24){
    PrezzoZonale.2022.OraMese[j, i] <-
      mean(demand.2022$PrezzoZonale[which(demand.2022$Ora==j&demand.2022$Mese==i)])
  }
}
matplot(t(PrezzoZonale.2022.OraMese), type = 'l', ylab = 'Clearing price',
        xlab = 'Months', main = 'Clearing price by hour in 2022')

PrezzoZonale.2022.GiornoMese <- matrix(rep(0, 31*12), 31, 12)
for (i in 1:12){
  for (j in 1:31){
    PrezzoZonale.2022.GiornoMese[j, i] <-
      mean(demand.2022$PrezzoZonale[which(demand.2022$Giorno==j&demand.2022$Mese==i)])
  }
}
matplot(t(PrezzoZonale.2022.GiornoMese), type = 'l', ylab = 'Clearing price',
        xlab = 'Months', main = 'Clearing price by day in 2022')

PrezzoZonale.2022.GiornoSettimanaMese <- matrix(rep(0, 7*12), 7, 12)
for (i in 1:12){
  for (j in 1:7){
    PrezzoZonale.2022.GiornoSettimanaMese[j, i] <-
      mean(demand.2022$PrezzoZonale[which(demand.2022$GiornoSettimana
                                          ==levels(demand$GiornoSettimana)[j]
                                          &demand.2022$Mese==i)])
  }
}
matplot(t(PrezzoZonale.2022.GiornoSettimanaMese), type = 'l', ylab = 'Clearing price',
        xlab = 'Months', main = 'Clearing price by weekday in 2022')

# In 2023:
PrezzoZonale.2023.OraMese <- matrix(rep(0, 24*12), 24, 12)
for (i in 1:12){
  for (j in 1:24){
    PrezzoZonale.2023.OraMese[j, i] <-
      mean(demand.2023$PrezzoZonale[which(demand.2023$Ora==j&demand.2023$Mese==i)])
  }
}
matplot(t(PrezzoZonale.2023.OraMese), type = 'l', ylab = 'Clearing price',
        xlab = 'Months', main = 'Clearing price by hour in 2023')

PrezzoZonale.2023.GiornoMese <- matrix(rep(0, 31*12), 31, 12)
for (i in 1:12){
  for (j in 1:31){
    PrezzoZonale.2023.GiornoMese[j, i] <-
      mean(demand.2023$PrezzoZonale[which(demand.2023$Giorno==j&demand.2023$Mese==i)])
  }
}
matplot(t(PrezzoZonale.2023.GiornoMese), type = 'l', ylab = 'Clearing price',
        xlab = 'Months', main = 'Clearing price by day in 2023')

PrezzoZonale.2023.GiornoSettimanaMese <- matrix(rep(0, 7*12), 7, 12)
for (i in 1:12){
  for (j in 1:7){
    PrezzoZonale.2023.GiornoSettimanaMese[j, i] <-
      mean(demand.2023$PrezzoZonale[which(demand.2023$GiornoSettimana
                                          ==levels(demand$GiornoSettimana)[j]
                                          &demand.2023$Mese==i)])
  }
}
matplot(t(PrezzoZonale.2023.GiornoSettimanaMese), type = 'l', ylab = 'Clearing price',
        xlab = 'Months', main = 'Clearing price by weekday in 2023')

# BY HOURS:
# In 2022:
PrezzoZonale.2022.MeseOra <- matrix(rep(0, 24*12), 12, 24)
for (i in 1:24){
  for (j in 1:12){
    PrezzoZonale.2022.MeseOra[j, i] <-
      mean(demand.2022$PrezzoZonale[which(demand.2022$Mese==j&demand.2022$Ora==i)])
  }
}
matplot(t(PrezzoZonale.2022.MeseOra), type = 'l', ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by month in 2022')

PrezzoZonale.2022.GiornoOra <- matrix(rep(0, 31*24), 31, 24)
for (i in 1:24){
  for (j in 1:31){
    PrezzoZonale.2022.GiornoOra[j, i] <-
      mean(demand.2022$PrezzoZonale[which(demand.2022$Giorno==j&demand.2022$Ora==i)])
  }
}
matplot(t(PrezzoZonale.2022.GiornoOra), type = 'l', ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by day in 2022')

PrezzoZonale.2022.GiornoSettimanaOra <- matrix(rep(0, 7*12), 7, 24)
for (i in 1:24){
  for (j in 1:7){
    PrezzoZonale.2022.GiornoSettimanaOra[j, i] <-
      mean(demand.2022$PrezzoZonale[which(demand.2022$GiornoSettimana
                                          ==levels(demand$GiornoSettimana)[j]
                                          &demand.2022$Ora==i)])
  }
}
matplot(t(PrezzoZonale.2022.GiornoSettimanaOra), type = 'l', ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by weekday in 2022')

# In 2023:
PrezzoZonale.2023.MeseOra <- matrix(rep(0, 24*12), 12, 24)
for (i in 1:24){
  for (j in 1:12){
    PrezzoZonale.2023.MeseOra[j, i] <-
      mean(demand.2023$PrezzoZonale[which(demand.2023$Mese==j&demand.2023$Ora==i)])
  }
}
matplot(t(PrezzoZonale.2023.MeseOra), type = 'l', ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by month in 2023')

PrezzoZonale.2023.GiornoOra <- matrix(rep(0, 31*24), 31, 24)
for (i in 1:24){
  for (j in 1:31){
    PrezzoZonale.2023.GiornoOra[j, i] <-
      mean(demand.2023$PrezzoZonale[which(demand.2023$Giorno==j&demand.2023$Ora==i)])
  }
}


matplot(t(PrezzoZonale.2023.GiornoOra), type = 'l', ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by day in 2023')

PrezzoZonale.2023.GiornoSettimanaOra <- matrix(rep(0, 7*12), 7, 24)
for (i in 1:24){
  for (j in 1:7){
    PrezzoZonale.2023.GiornoSettimanaOra[j, i] <-
      mean(demand.2023$PrezzoZonale[which(demand.2023$GiornoSettimana
                                          ==levels(demand$GiornoSettimana)[j]
                                          &demand.2023$Ora==i)])
  }
}
matplot(t(PrezzoZonale.2023.GiornoSettimanaOra), type = 'l', ylab = 'Clearing price',
        xlab = 'Hours', main = 'Clearing price by weekday in 2023')


# FUNCTIONAL DATA ANALYSIS -------------------------------------

# FDA: Basis definition and regression model -----------------------------------------
initial_date <- as.Date("2022-01-01")
final_date <- as.Date("2023-12-31")
date_range <- seq(initial_date, final_date, by = "day")

# Since domains are different each time, we decide to normalize between 0 and 1
# and to insert in data.clear the minimum and maximum volumes.

# Create B-spline basis
norder <- 6
nbasis <- 18 # can be changed later
bspline.basis <- create.bspline.basis(rangeval = c(0,1),
                                     norder = norder,
                                     nbasis = nbasis)

# Initialize an empty dataframe
demand.clear <- data.frame(Date = character(),
                           Hour = numeric(),
                           Vmin = numeric(),
                           Vmax = numeric(),
                           NormV = I(list()),
                           Prezzi = I(list()),
                           DemandBidsCoef = I(list()),
                           Basismat = I(list())
)

# Definition of lambda_range:
lambda_range <- 10^seq(-4, -1 ,by = 0.25)

# Function to process data for each day and hour
process_data <- function(g, h, demand, bspline.basis) {
  vmin <- min(demand$Volume[which(demand$Data == g & demand$Ora == h)])
  vmax <- max(demand$Volume[which(demand$Data == g & demand$Ora == h)])
  v <- demand$Volume[which(demand$Data == g & demand$Ora == h)]
  
  # Normalization
  norm_v <- (v - vmin) / (vmax - vmin)
  
  pricemat <- demand$Prezzo[which(demand$Data == g & demand$Ora == h)]
  domain <- seq(0, 1, by = 0.01)
  interpolated <- approx(norm_v, pricemat, xout = domain, method = "linear")
  
  # Evaluate basis functions at domain points
  basismat <- eval.basis(evalarg = domain, basisobj = bspline.basis)
  
  best_lambda <- NULL
  min_gcv <- Inf
  
  for (lambda in lambda_range) {
    functionalPar <- fdPar(fdobj = bspline.basis, Lfdobj=3, lambda = lambda)
    gcv <- smooth.basis(domain, interpolated$y, functionalPar)$gcv
    
    # Check if current GCV is the smallest so far
    if (gcv < min_gcv & length(gcv)!= 0) {
      min_gcv <- gcv
      best_lambda <- lambda
    }
  }
  
  # Smoothed Values:
  functionalPar <- fdPar(fdobj = bspline.basis, Lfdobj=3, lambda = best_lambda)
  smoothed_fd <- smooth.basis(domain, interpolated$y, functionalPar)$fd
  smoothed <- eval.fd(domain, smoothed_fd, Lfd = 0)
  # Fit linear regression model
  pricecoef <- lsfit(x = basismat, y = smoothed, intercept = FALSE)$coef
  
  return(data.frame(Date = g,
                    Hour = h,
                    Vmin = vmin,
                    Vmax = vmax,
                    NormV = I(list(norm_v)),
                    Prezzi = I(list(pricemat)),
                    DemandBidsCoef = I(list(pricecoef)),
                    Basismat = I(list(basismat))))
}

# Use lapply to process data for each day and hour
processed_data <- lapply(as.character(date_range), function(g) {
  Ore <- unique(demand$Ora[which(demand$Data == g)])
  lapply(Ore, function(h) process_data(g, h, demand, bspline.basis))
})

# Combine the results into a single dataframe
demand.clear <- do.call(rbind, unlist(processed_data, recursive = FALSE))

# Add year, month, day and weekday:
demand.clear$Year <- year(demand.clear$Date)
demand.clear$Month <- month(demand.clear$Date)
demand.clear$Day <- day(demand.clear$Date)
demand.clear$WeekDay <- weekdays(as.Date(demand.clear$Date))
demand.clear$WeekDay <- factor(demand.clear$WeekDay,
          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                        "Saturday", "Sunday"), ordered = TRUE)
demand.clear.2022 <- demand.clear[which(demand.clear$Year==2022),]
demand.clear.2023 <- demand.clear[which(demand.clear$Year==2023),]

# Create prezzozonale per Date:
prezzozonale <- demand[, -c(4, 5, 10)]
prezzozonale <- unique(prezzozonale[, c("Data", "Ora", "PrezzoZonale")])
rownames(prezzozonale) <- NULL

# Save as RData:
save(demand.clear, file = 'demand.clear.RData')
save(demand.clear.2022, file = 'demand.clear.2022.RData')
save(demand.clear.2023, file = 'demand.clear.2023.RData')
save(prezzozonale, file = 'prezzozonale.RData')
save(bspline.basis, file = 'bspline.basis.RData')

# Now we have coefficients and Basismat for functional data objects in the
# dataframe, avoiding missing data. They're also indexed by date.

# FDA: smoothing: --------------------------------------------------------------

load("demand.clear.RData")
load("demand.clear.2022.RData")
load("demand.clear.2023.RData")
load("prezzozonale.RData")

par(mfrow = c(1, 1))
plot (bspline.basis)

# Define domain to plot:
domain <- seq(0, 1, by = 0.01)
basismat_domain <- eval.basis(evalarg = domain, basisobj = bspline.basis)

# Definition of useful indexes:
index_day <- function(y, m, d){ # all hours in a day
  which(demand.clear$Year == y &
      demand.clear$Month == m &
      demand.clear$Day == d)
}

index_hour <- function(y, m, h){ # all days in a month, one hour
  which(demand.clear$Year == y &
      demand.clear$Month == m &
      demand.clear$Hour == h)
}

index_yearmonth <- function(d, h){ # all years, all months, for an hour in a day
  which(demand.clear$Hour == h &
          demand.clear$Day == d)
}

index_month <- function(y, d, h){ # all months, for an hour in a day in a year
  which(demand.clear$Hour == h &
          demand.clear$Day == d &
          demand.clear$Year == y)
}

par(mfrow = c(1, 2))
y = 2023
m = 8
d = 15
plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Smoothed Prices',
     main = paste('y', y, ', m', m, ', d', d))
colors <- rainbow(length(index_day(y, m, d)))
j = 1
for (i in index_day(y, m ,d)){
  coefficients <- unlist(demand.clear$DemandBidsCoef[i])
  smoothed <- basismat_domain %*% coefficients
  lines(domain, smoothed, col = colors[j])
  j = j + 1
}

plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Real prices',
     main = paste('y', y, ', m', m, ', d', d))
j = 1
for (i in index_day(y, m ,d)){
  norm_v <- demand.clear$NormV[[i]]
  prezzi <- demand.clear$Prezzi[[i]]
  lines(norm_v, prezzi, col = colors[j])
  j = j + 1
}

par(mfrow = c(1, 2))
y = 2022
m = 1
h = 23
plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Smoothed Prices',
     main = paste('y', y, ', m', m, ', h', h))
colors <- rainbow(length(index_hour(y, m, h)))
j = 1
for (i in index_hour(y, m ,h)){
  coefficients <- unlist(demand.clear$DemandBidsCoef[i])
  smoothed <- basismat_domain %*% coefficients
  lines(domain, smoothed, col = colors[j])
  j = j + 1
}

plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Real Prices',
     main = paste('y', y, ', m', m, ', h', h))
j = 1
for (i in index_hour(y, m ,h)){
  norm_v <- demand.clear$NormV[[i]]
  prezzi <- demand.clear$Prezzi[[i]]
  lines(norm_v, prezzi, col = colors[j])
  j = j + 1
}

par(mfrow = c(1, 2))
h = 19
d = 1
plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Smoothed Prices',
     main = paste('h', h, ', d', d))
colors <- rainbow(length(index_yearmonth(d, h)))
j = 1
for (i in index_yearmonth(d, h)){
  coefficients <- unlist(demand.clear$DemandBidsCoef[i])
  smoothed <- basismat_domain %*% coefficients
  lines(domain, smoothed, col = colors[j])
  j = j + 1
}

plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Real prices',
     main = paste('h', h, ', d', d))
j = 1
for (i in index_yearmonth(d, h)){
  norm_v <- demand.clear$NormV[[i]]
  prezzi <- demand.clear$Prezzi[[i]]
  lines(norm_v, prezzi, col = colors[j])
  j = j + 1
}

par(mfrow = c(1, 2))
y = 2022
h = 19
d = 1
plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Smoothed Prices',
     main = paste('y', y, ', h', h, ', d', d))
colors <- rainbow(length(index_month(2023, d, h)))
j = 1
for (i in index_month(y, d, h)){
  coefficients <- unlist(demand.clear$DemandBidsCoef[i])
  smoothed <- basismat_domain %*% coefficients
  lines(domain, smoothed, col = colors[j])
  j = j + 1
}

plot(domain, rep(0, length(domain)), type = 'n', ylim = c(0, 4000), 
     xlab = 'Volume fraction', ylab = 'Real prices',
     main = paste('y', y, ', h', h, ', d', d))
j = 1
for (i in index_month(y, d, h)){
  norm_v <- demand.clear$NormV[[i]]
  prezzi <- demand.clear$Prezzi[[i]]
  lines(norm_v, prezzi, col = colors[j])
  j = j + 1
}

