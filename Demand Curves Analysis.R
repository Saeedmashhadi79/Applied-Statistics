# Demand bids analysis:
# Supply bids are taken into account for each hour, using data coming from GME
# from 2022-01-01 to 2023-12-31 (YYYY-MM-DD) at each day and hour,
# analyzing these functions (knowing PrezzoZonale too, the intersection) without
# confrontation with Offer curves for now.
# Only the pool where all the zones are present is taken into account.

library(dplyr)
library(lubridate)
library(fda)
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

# Preliminary EDA ---------------------------------------------------------
# Treating hours separately (and considering daily time series), we look at
# similarities within day type and month. So: hour/day/month (for two and a half
# year). We notice trends in the data.

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


# Scatter plots -----------------------------------------------------------

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

# C) Standard deviation (months):
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

# D) Standard deviation (hours):
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

# There doesn't seem to be a trend in days and weekdays, so it makes no sense
# to create a scatterplot of mean or standard deviation

# Functional Data Analysis: Smoothing -------------------------------------

domain <- range(demand$Volume)
norder <- 4
nbasis <- 20
demand.basis <- create.bspline.basis(rangeval = domain, nbasis = nbasis, 
                               norder = norder)
prova <- data.frame(
   volume = demand$Volume[which(demand$Data=="2022-02-06"&demand$Ora==8)],
   prezzo = demand$Prezzo[which(demand$Data=="2022-02-06"&demand$Ora==8)]
)
Xobs0 <- prova$prezzo
abscissa <- prova$volume
NT <- length(abscissa) # number of locations of observations

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
plot(abscissa,Xobs0,xlab="t",ylab="observed data", type = "l")

data_to_smooth <- demand$Volume[which(demand$Data == "2022-02-06" & demand$Ora == 8)]
basismat <- eval.basis(data_to_smooth, demand.basis)
demandfd <- smooth.basis(data_to_smooth, basismat, demand.basis, method = "spline")
plot(demandfd)














# Functional Data Analysis: Functional PCA --------------------------------







