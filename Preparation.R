# PREPARATION OF SAVED RData
# Libraries:
library(dplyr)
library(lubridate)
library(fda)
library(glmnet)
library(Matrix)
library(monotone)

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
