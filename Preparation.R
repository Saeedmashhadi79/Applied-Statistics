# PREPARATION OF SAVED RData
# Libraries:
library(dplyr)
library(lubridate)
library(fda)
library(glmnet)
library(Matrix)
library(monotone)
library(fastICA)
library(ggplot2)

# Loadings to do:
# Demand:
load("demand.RData")
load("demand.2022.RData")
load("demand.2023.RData")
load("demand.clear.RData")
load("demand.clear.2022.RData")
load("demand.clear.2023.RData")

# Offer:
load("offer.RData")
load("offer.2022.RData")
load("offer.2023.RData")
load("offer.clear.RData")
load("offer.clear.2022.RData")
load("offer.clear.2023.RData")

# Shared:
load("prezzozonale.RData")
load("bspline.basis.RData")
domain = seq(0, 1, by = 0.01)
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

index_moment <- function(y, m, d, h){
  which(demand.clear$Hour == h &
          demand.clear$Day == d &
          demand.clear$Year == y &
          demand.clear$Month == m)
}
