## ---- pdpyearly
library(tidyr)
library(rlang)
library(tidyverse)
load("data/yearly/pdp_yearly/trendgrid.rda")
trendgrid$variable <- rep(1:1000, 20)
# removing outliers
load("data/yearly/pdp_yearly/ur_ppgrid_rmout.rda")
ur_ppgrid_rmout$variable <- rep(1:1000, 20)

## facet ur_pp
names(ur_ppgrid_rmout)[names(ur_ppgrid_rmout) == 'ARMA.AR.MA'] <- 'ARMA'
names(ur_ppgrid_rmout)[names(ur_ppgrid_rmout) == 'ETS.trend'] <- 'ETS-trend'
keepurppgrid <- c("ur_pp", "id", "ARIMA", "ARMA",
                   "wn", "rwd", "ETS-trend", "variable")
ur_ppgrid_rmout <- ur_ppgrid_rmout[ , (names(ur_ppgrid_rmout) %in% keepurppgrid)]
ur_ppgrid_rmout <- ur_ppgrid_rmout[c("ur_pp", "id", "variable", "rwd", "ETS-trend", "ARIMA", "ARMA", "wn")]
ur_ppgrid_long <- gather(ur_ppgrid_rmout, class, probability, rwd:wn, factor_key=TRUE)
urpp_yearly <- ggplot(data = ur_ppgrid_long, aes_string(x = ur_ppgrid_long$ur_pp, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ class)


## facet trend
names(trendgrid)[names(trendgrid) == 'ARMA.AR.MA'] <- 'ARMA'
names(trendgrid)[names(trendgrid) == 'ETS.trend'] <- 'ETS-trend'
keeptrendgrid <- c("trend", "id", "ARIMA", "ARMA",
                   "wn", "rwd", "ETS-trend", "variable")
trendgrid <- trendgrid[ , (names(trendgrid) %in% keeptrendgrid)]
trendgrid <- trendgrid[c("trend", "id", "variable", "rwd", "ETS-trend", "ARIMA", "ARMA", "wn")]
trendgrid_long <- gather(trendgrid, class, probability, rwd:wn, factor_key=TRUE)
trend_yearly <- ggplot(data = trendgrid_long, aes_string(x = trendgrid_long$trend, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90))+
  facet_grid(. ~ class)
  
urpp_yearly/trend_yearly
