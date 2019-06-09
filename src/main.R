##---- packages
library(ggplot2)
library(patchwork)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(png)
library(tsfeatures)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(iml) #machine learning interpretability package
library(ggcorrplot) # to draw  ggcorrplot



## ---- viyearly
# All variable scores into one dataframe
load("data/yearly/train_importance.rda")
load(file = "data/yearly/sd_pdf_df.rda")
load(file = "data/yearly/sd_ice_df.rda")
## Permutation based
# head(train_importance)
# class(train_importance) #matrix
train_imp_df <- data.frame(train_importance)
train_imp_df <- add_rownames(train_imp_df, "Feature")
# names(train_imp_df)
train_imp_df <- within(train_imp_df, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_imp <- train_imp_df %>% melt(id.vars = "Feature")
# head(permutation_imp)
# dim(permutation_imp) # 250 3
colnames(permutation_imp) <- c("feature", "class", "score")

## PDP-based
# head(sd_pdf_df)
sd_pdf_df <- add_rownames(sd_pdf_df, "class")
# head(sd_pdf_df) %>% data.frame()
pdp_imp <- sd_pdf_df %>% melt(id.vars = "class")
# head(pdp_imp)
colnames(pdp_imp) <- c("class", "feature", "score")
# dim(pdp_imp) # 250 3

## ICE-based
# head(sd_ice_df)
sd_ice_df <- add_rownames(sd_ice_df, "class")
# head(sd_ice_df) %>% data.frame()
ice_imp <- sd_ice_df %>% melt(id.vars = "class")
# head(ice_imp)
colnames(ice_imp) <- c("class", "feature", "score")
# dim(ice_imp) # 250 3

## Combine the data frames
importancescoreY <- bind_rows(permutation_imp, pdp_imp)
importancescoreY <- bind_rows(importancescoreY, ice_imp)
importancescoreY$VI <- rep(c("permutation", "PDP", "ICE"), each = 250)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreY$class <- factor(importancescoreY$class,
                                 levels = c("rw", "rwd", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn"),
                                 labels = c("rw", "rwd", "ETS.trend", "ETS.dampedtrend", "ETS.notrendnoseasonal", "ARIMA", "ARMA.AR.MA", "wn", "theta", "nn")
)
rank_vi_yearly_classes <- importancescoreY %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_viy_classes <- rank_vi_yearly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overal importance of features to the forest
train_impforest <- data.frame(train_importance)
train_impforest <- add_rownames(train_impforest, "Feature")
train_impforest <- train_impforest[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
# head(train_impforest)
train_impforest <- train_impforest %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforest$mean_rank <- (train_impforest$rank_permu + train_impforest$rank_gini) / 2
meanrank_viy_forest <- data.frame(
  feature = train_impforest$Feature,
  class = rep("overall", 25),
  rank = train_impforest$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_yearly <- dplyr::bind_rows(meanrank_viy_forest, meanrank_viy_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_yearly, class == "overall")
meanrank_yearly$feature <- factor(meanrank_yearly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_yearly$class <- factor(meanrank_yearly$class,
                                levels = c(
                                  "overall", "rw",
                                  "rwd",
                                  "ETS.trend",
                                  "ETS.dampedtrend",
                                  "ETS.notrendnoseasonal",
                                  "ARIMA",
                                  "ARMA.AR.MA",
                                  "wn",
                                  "theta",
                                  "nn" ))

meanrank_yearly <- meanrank_yearly %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))

meanrank_yearly$rn <- 1:275

top <- meanrank_yearly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)

meanrank_yearly$istop <- ifelse(meanrank_yearly$rn%in%top$rn, TRUE, FALSE)

feaImp_yearly <- ggplot(meanrank_yearly, aes(y = rank, x = feature, fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~ class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+theme(text=element_text(size = 10))+
  theme(strip.text.x = element_text(size = 10))
feaImp_yearly



## ---- pdpyearly
load("data/yearly/pdp_yearly/trendgrid.rda")
trendgrid$variable <- rep(1:1000, 20)
# removing outliers
load("data/yearly/pdp_yearly/ur_ppgrid_rmout.rda")
ur_ppgrid_rmout$variable <- rep(1:1000, 20)


## facet 
names(ur_ppgrid_rmout)[names(ur_ppgrid_rmout) == 'ARMA.AR.MA'] <- 'ARMA'
names(ur_ppgrid_rmout)[names(ur_ppgrid_rmout) == 'ETS.trend'] <- 'ETS-T'
names(ur_ppgrid_rmout)[names(ur_ppgrid_rmout) == 'ETS.notrendnoseasonal'] <- 'ETS-NTNS'
keepurppgrid <- c("ur_pp", "id", "ARIMA", "ARMA",
                  "wn", "ETS-T", "ETS-NTNS", "variable")
ur_ppgrid_rmout <- ur_ppgrid_rmout[ , (names(ur_ppgrid_rmout) %in% keepurppgrid)]
ur_ppgrid_rmout <- ur_ppgrid_rmout[c("ur_pp", "id", "variable", "ARIMA", "ETS-T", "ETS-NTNS", "ARMA", "wn")]
ur_ppgrid_long <- gather(ur_ppgrid_rmout, class, probability, ARIMA:wn, factor_key=TRUE)
names(trendgrid)[names(trendgrid) == 'ARMA.AR.MA'] <- 'ARMA'
names(trendgrid)[names(trendgrid) == 'ETS.trend'] <- 'ETS-T'
names(trendgrid)[names(trendgrid) == 'ETS.notrendnoseasonal'] <- 'ETS-NTNS'
keeptrendgrid <- c("trend", "id", "ARIMA", "ARMA",
                   "wn", "ETS-NTNS", "ETS-T", "variable")
urpp_yearly <- ggplot(data = ur_ppgrid_long, aes_string(x = ur_ppgrid_long$ur_pp, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)
## facet trend
names(trendgrid)[names(trendgrid) == 'ARMA.AR.MA'] <- 'ARMA'
names(trendgrid)[names(trendgrid) == 'ETS.trend'] <- 'ETS-T'
names(trendgrid)[names(trendgrid) == 'ETS.notrendnoseasonal'] <- 'ETS-NTNS'
keeptrendgrid <- c("trend", "id", "ARIMA", "ARMA",
                   "wn", "ETS-NTNS", "ETS-T", "variable")
trendgrid <- trendgrid[ , (names(trendgrid) %in% keeptrendgrid)]
trendgrid <- trendgrid[c("trend", "id", "variable", "ARIMA","ETS-T", "ETS-NTNS",  "ARMA", "wn")]
trendgrid_long <- gather(trendgrid, class, probability, ARIMA:wn, factor_key=TRUE)
trend_yearly <- ggplot(data = trendgrid_long, aes_string(x = trendgrid_long$trend, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)
urpp_yearly/trend_yearly


## ---- pdpquarterly
load("data/quarterly/pdp_quarterly/seasonalitygridQ.rda")
seasonalitygridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/trendgridQ.rda")
trendgridQ$variable <- rep(1:1700, 20)
## Monthly linearity
load("data/monthly/pdp_monthly/linearitygridM.rda")
linearitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/NgridM.rda")
NgridM$variable <- rep(1:1700, 20)


names(seasonalitygridQ)[names(seasonalitygridQ) == "ETS.notrendnoseasonal"] <- 'ETS-NTNS'
names(seasonalitygridQ)[names(seasonalitygridQ) == "ETS.dampedtrend"] <- 'ETS-DT'
names(seasonalitygridQ)[names(seasonalitygridQ) == "ETS.dampedtrendseasonal"] <- 'ETS-DTS'
names(seasonalitygridQ)[names(seasonalitygridQ) == "ETS.trend"] <- 'ETS-T'
names(seasonalitygridQ)[names(seasonalitygridQ) == "ETS.trendseasonal"] <- 'ETS-TS'
names(seasonalitygridQ)[names(seasonalitygridQ) == "ETS.seasonal"] <- 'ETS-S'
keepSgrid <- c("seasonality", "id", "ETS-NTNS", "ETS-DT",
                  "ETS-T", "ETS-DTS", "ETS-TS", "ETS-S","variable")
seasonalitygridQ <- seasonalitygridQ[ , (names(seasonalitygridQ) %in% keepSgrid)]
seasonalitygridQ <- seasonalitygridQ[c("seasonality", "id","variable", "ETS-NTNS", "ETS-DT",
                                     "ETS-T", "ETS-DTS", "ETS-TS", "ETS-S")]
seasonalitygridQ_long <- gather(seasonalitygridQ, class, probability, "ETS-NTNS":'ETS-S', factor_key=TRUE)

sea_quarterly <- ggplot(data = seasonalitygridQ_long, aes_string(x = seasonalitygridQ_long$seasonality, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)

## facet trend
names(trendgridQ)[names(trendgridQ) == 'ETS.notrendnoseasonal'] <- 'ETS-NTNS'
names(trendgridQ)[names(trendgridQ) == 'ETS.dampedtrend'] <- 'ETS-DT'
names(trendgridQ)[names(trendgridQ) == 'ETS.dampedtrendseasonal'] <- 'ETS-DTS'
names(trendgridQ)[names(trendgridQ) == 'ETS.trend'] <- 'ETS-T'
names(trendgridQ)[names(trendgridQ) == 'ETS.trendseasonal'] <- 'ETS-TS'
names(trendgridQ)[names(trendgridQ) == 'ETS.seasonal'] <- 'ETS-S'
keepTgrid <- c("trend", "id", "ETS-NTNS", "ETS-DT",
               "ETS-T", "ETS-DTS", "ETS-TS", "ETS-S","variable")
trendgridQ <- trendgridQ[ , (names(trendgridQ) %in% keepTgrid)]
trendgridQ <- trendgridQ[c("trend", "id","variable", "ETS-NTNS", "ETS-DT",
                                       "ETS-T", "ETS-DTS", "ETS-TS", "ETS-S")]
trendgridQ_long <- gather(trendgridQ, class, probability, "ETS-NTNS":"ETS-S", factor_key=TRUE)

trend_quarterly <- ggplot(data = trendgridQ_long, aes_string(x = trendgridQ_long$trend, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)

sea_quarterly/trend_quarterly

## ---- pdpquarterly2
load("data/quarterly/pdp_quarterly/seasonalitygridQ.rda")
seasonalitygridQ$variable <- rep(1:1700, 20)
load("data/quarterly/pdp_quarterly/trendgridQ.rda")
trendgridQ$variable <- rep(1:1700, 20)

names(seasonalitygridQ)[names(seasonalitygridQ) == "ARMA.AR.MA"] <- 'ARMA'

keepSgrid <- c("seasonality", "id", "ARMA", "stlar",
               "tbats", "theta", "nn", "variable")
seasonalitygridQ <- seasonalitygridQ[ , (names(seasonalitygridQ) %in% keepSgrid)]
seasonalitygridQ <- seasonalitygridQ[c("seasonality", "id","variable", "ARMA", "stlar",
                                       "tbats", "theta", "nn")]
seasonalitygridQ_long2 <- gather(seasonalitygridQ, class, probability, ARMA:nn, factor_key=TRUE)

sea_quarterly2 <- ggplot(data = seasonalitygridQ_long2, aes_string(x = seasonalitygridQ_long2$seasonality, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonality") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)

## facet trend
names(trendgridQ)[names(trendgridQ) == 'ARMA.AR.MA'] <- 'ARMA'
keepTgrid <- c("trend", "id","ARMA", "stlar",
               "tbats", "theta", "nn","variable")
trendgridQ <- trendgridQ[ , (names(trendgridQ) %in% keepTgrid)]
trendgridQ <- trendgridQ[c("trend", "id","variable", "ARMA", "stlar",
                           "tbats", "theta", "nn")]
trendgridQ_long2 <- gather(trendgridQ, class, probability, ARMA:nn, factor_key=TRUE)

trend_quarterly2 <- ggplot(data = trendgridQ_long2, aes_string(x = trendgridQ_long2$trend, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)

sea_quarterly2/trend_quarterly2


## ---- pdpmonthly
# pl1 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="rw")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ylab("")+ggtitle("rw")
# pl2 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.trend")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ylab("")+ggtitle("ETS.T")
# pl3 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.dampedtrendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ylab("")+ggtitle("ETS.DTS")
# 
# pl4 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="ETS.trendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ylab("")+ggtitle("ETS.TS")
# 
# pl5 <- ggplot(data=linearitygridM, aes_string(x=linearitygridM$linearity, y="nn")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("linearity") + 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+
#   theme(legend.position="none", text = element_text(size=10)) +ylab("")+ggtitle("nn")
# 
# 
# pn1 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="rw")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
#   theme(legend.position="none", text = element_text(size=10))+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3) +
#   theme(legend.position = "none")+
#   ylab("")
# pn2 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.trend")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
#   theme(legend.position="none", text = element_text(size=10))+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3) +
#   theme(legend.position = "none")+
#   ylab("")
# pn3 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.dampedtrendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
#   theme(legend.position="none", text = element_text(size=10))+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3) +
#   theme(legend.position = "none")+
#   ylab("")
# pn4 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="ETS.trendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
#   theme(legend.position="none", text = element_text(size=10))+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3) +
#   theme(legend.position = "none")+
#   ylab("")
# pn5 <- ggplot(data=NgridM, aes_string(x=NgridM$N, y="nn")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("N")+ 
#   theme(legend.position="none", text = element_text(size=10))+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3) +
#   theme(legend.position = "none")+
#   ylab("")
# 
# (pl1|pl2|pl3|pl4|pl5)/(pn1|pn2|pn3|pn4|pn5)

## Monthly linearity
load("data/monthly/pdp_monthly/linearitygridM.rda")
linearitygridM$variable <- rep(1:1700, 20)
load("data/monthly/pdp_monthly/NgridM.rda")
NgridM$variable <- rep(1:1700, 20)

names(linearitygridM)[names(linearitygridM) == 'ETS.notrendnoseasonal'] <- 'ETS-NTNS'
names(linearitygridM)[names(linearitygridM) == 'ETS.dampedtrend'] <- 'ETS-DT'
names(linearitygridM)[names(linearitygridM) == 'ETS.dampedtrendseasonal'] <- 'ETS-DTS'
names(linearitygridM)[names(linearitygridM) == 'ETS.trend'] <- 'ETS-T'
names(linearitygridM)[names(linearitygridM) == 'ETS.trendseasonal'] <- 'ETS-TS'
names(linearitygridM)[names(linearitygridM) == 'ETS.seasonal'] <- 'ETS-S'
keepSgrid <- c("linearity", "id", "ETS-NTNS", "ETS-DT",
               "ETS-T", "ETS-DTS", "ETS-TS","variable")
linearitygridM <- linearitygridM[ , (names(linearitygridM) %in% keepSgrid)]
linearitygridM <- linearitygridM[c("linearity", "id","variable", "ETS-NTNS", "ETS-DT",
                                   "ETS-T", "ETS-DTS", "ETS-TS")]
linearitygridM_long <- gather(linearitygridM, class, probability, "ETS-NTNS":"ETS-TS", factor_key=TRUE)

linearity_monthly <- ggplot(data = linearitygridM_long, aes_string(x = linearitygridM_long$linearity, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("linearity") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class, scales = "free")

## facet 
names(NgridM)[names(NgridM) == 'ETS.notrendnoseasonal'] <- 'ETS-NTNS'
names(NgridM)[names(NgridM) == 'ETS.dampedtrend'] <- 'ETS-DT'
names(NgridM)[names(NgridM) == 'ETS.dampedtrendseasonal'] <- 'ETS-DTS'
names(NgridM)[names(NgridM) == 'ETS.trend'] <- 'ETS-T'
names(NgridM)[names(NgridM) == 'ETS.trendseasonal'] <- 'ETS-TS'
names(NgridM)[names(NgridM) == 'ETS.seasonal'] <- 'ETS-S'

Ngrid <- c("N", "id", "ETS-NTNS", "ETS-DT",
           "ETS-T", "ETS-DTS", "ETS-TS","variable")
NgridM <- NgridM[ , (names(NgridM) %in% Ngrid)]
NgridM <- NgridM[c("N", "id","variable", "ETS-NTNS", "ETS-DT",
                   "ETS-T", "ETS-DTS", "ETS-TS")]
NgridM_long <- gather(NgridM, class, probability, "ETS-NTNS":"ETS-TS", factor_key=TRUE)

N_monthly <- ggplot(data = NgridM_long, aes_string(x = NgridM_long$N, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("N") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)

linearity_monthly/N_monthly


## ---- vihourly
# All variable scores into one dataframe
load("data/hourly/trainH_importance.rda")
load(file = "data/hourly/sd_pdf_dfH.rda")
load(file = "data/hourly/sd_ice_dfH.rda")
## Permutation based
train_imp_dfH <- data.frame(trainH_importance)
train_imp_dfH <- add_rownames(train_imp_dfH, "Feature")
train_imp_dfH <- within(train_imp_dfH, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impH <- train_imp_dfH %>% melt(id.vars = "Feature")
#dim(permutation_impD) # 260 3
colnames(permutation_impH) <- c("feature", "class", "score")

## PDP-based
sd_pdf_dfH <- add_rownames(sd_pdf_dfH, "class")
pdp_imp <- sd_pdf_dfH %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")

## ICE-based
sd_ice_dfH <- add_rownames(sd_ice_dfH, "class")
ice_imp <- sd_ice_dfH %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")

## Combine the data frames
importancescoreH <- bind_rows(permutation_impH, pdp_imp)
importancescoreH <- bind_rows(importancescoreH, ice_imp)
importancescoreH$VI <- rep(c("permutation", "PDP", "ICE"), each = 260)

## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreH$class <- factor(importancescoreH$class,
                                 levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                            "theta","nn","wn"),
                                 labels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                            "theta","nn","wn"))

rank_vi_hourly_classes <- importancescoreH %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))

## compute mean rank
meanrank_vih_classes <- rank_vi_hourly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))

## overall importance of features to the forest
train_impforestH <- data.frame(trainH_importance)
train_impforestH <- add_rownames(train_impforestH, "Feature")
train_impforestH <- train_impforestH[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestH <- train_impforestH %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestH$mean_rank <- (train_impforestH$rank_permu + train_impforestH$rank_gini) / 2
meanrank_vih_forest <- data.frame(
  feature = train_impforestH$Feature,
  class = rep("overall", 26),
  rank = train_impforestH$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_hourly <- dplyr::bind_rows(meanrank_vih_forest, meanrank_vih_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_hourly, class == "overall")
meanrank_hourly$feature <- factor(meanrank_hourly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_hourly$class <- factor(meanrank_hourly$class,
                                levels = c("overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                           "theta","nn","wn"),
                                labels = c("overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                           "theta","nn","wn"))

meanrank_hourly$rn <- 1:286
topq <- meanrank_hourly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_hourly$istop <- ifelse(meanrank_hourly$rn %in% topq$rn, TRUE, FALSE)

meanrank_hourly$feature <- plyr::revalue(meanrank_hourly$feature, 
                                         c("seasonal_strength1"="seasonal_D (24)",
                                           "seasonal_strength2"="seasonal_W (168)"))

feaImp_hourly <- ggplot(meanrank_hourly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity", width=0.3) +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+
  theme(text=element_text(size = 10))
feaImp_hourly


## ---- pdphourly
load("data/hourly/hiceout/entropygridH.rda")
entropygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality1gridH.rda")
seasonality1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality2gridH.rda")
seasonality2gridH$variable <- rep(1:1000, 20)


keepSgrid <- c("seasonal_strength1", "id", "snaive", "rw",
               "mstlarima", "tbats", "nn","variable")
seasonality1gridH <- seasonality1gridH[ , (names(seasonality1gridH) %in% keepSgrid)]
seasonality1gridH <- seasonality1gridH[c("seasonal_strength1", "id","variable","rw", "snaive",
                                         "mstlarima", "tbats", "nn")]
seasonality1gridH_long <- gather(seasonality1gridH, class, probability, "rw":"nn", factor_key=TRUE)

seas1_hourly <- ggplot(data = seasonality1gridH_long, aes_string(x = seasonality1gridH_long$seasonal_strength1, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_daily") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)


keepS2grid <- c("seasonal_strength2", "id", "snaive", "rw",
               "mstlarima", "tbats", "nn","variable")
seasonality2gridH <- seasonality2gridH[ , (names(seasonality2gridH) %in% keepS2grid)]
seasonality2gridH <- seasonality2gridH[c("seasonal_strength2", "id","variable","rw", "snaive",
                                         "mstlarima", "tbats", "nn")]
seasonality2gridH_long <- gather(seasonality2gridH, class, probability, "rw":"nn", factor_key=TRUE)

seas2_hourly <- ggplot(data = seasonality2gridH_long, aes_string(x = seasonality2gridH_long$seasonal_strength2, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("seasonal_weekly") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=10))+
  facet_grid(. ~ class)

seas1_hourly/seas2_hourly


## ---- seasonalityhourly
load("data/hourly/hiceout/seasonality1gridH.rda")
seasonality1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality2gridH.rda")
seasonality2gridH$variable <- rep(1:1000, 20)
## Arrange graphs for faceting
keep.modelnames <- c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                     "theta","nn","wn")
keeps1 <- c(keep.modelnames, "seasonal_strength1")
keeps2 <- c(keep.modelnames, "seasonal_strength2")
seasonal1 <- seasonality1gridH[, names(seasonality1gridH) %in% keeps1]
seasonal1 <- rename(seasonal1, seasonal = seasonal_strength1) 
seasonal2 <- seasonality2gridH[, names(seasonality2gridH) %in% keeps2]
seasonal2 <- rename(seasonal2, seasonal = seasonal_strength2) 
seasonal1_long <- gather(seasonal1, class, probability, "mstlarima":"wn", factor_key = TRUE)
seasonal2_long <- gather(seasonal2, class, probability, "mstlarima":"wn", factor_key = TRUE)
seasonal1_long_mean <- seasonal1_long %>%
  group_by(seasonal, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)
seasonal2_long_mean <- seasonal2_long %>%
  group_by(seasonal, class) %>%
  summarise(n=n(), mean=mean(probability), sd=sd(probability)) %>%
  mutate(sem = sd/sqrt(n-1),
         CI_lower = mean+qt((1-0.95)/2, n-1)*sem,
         CI_upper = mean - qt((1-0.95)/2, n-1)*sem)

seasonal_DW <- dplyr::bind_rows(seasonal1_long_mean, seasonal2_long_mean)
seasonal_DW$feature <- c(rep("seasonal_D (24)", 200), rep("seasonal_W (168)", 200))
seasonal_DW$class <- factor(seasonal_DW$class,
                            levels = c("snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                       "theta","nn","wn"))

plot_pdp_hourly_seasonal <- ggplot(seasonal_DW, aes(x=seasonal, y=mean, color=feature))+
  geom_line(aes(x=seasonal, y=mean, color=feature), size = 1)+
  geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=feature),alpha=0.4, colour = NA)+
  facet_wrap(. ~ class, ncol = 5, nrow = 2)+
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=10), axis.title = element_text(size = 10))+
  theme(strip.text.x = element_text(size = 10))+xlab("strength of seasonality")+
  ylab("probability of selecting forecast-models")+
  theme(legend.position="bottom", legend.title=element_blank())+
  scale_colour_manual("",values=c("red", "blue"))+
  scale_fill_manual("",values=c("red", "blue"))
plot_pdp_hourly_seasonal


## ---- htwopdp
load("data/hourly/linearity.sediff_seacf1.h.rda")
colNamesss <- colnames(linearity.sediff_seacf1.h)[28:37]
# snaive
int1 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[6], fill = colNamesss[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("snaive")+
  theme(aspect.ratio=1, text = element_text(size=20))

## rw
int2 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[4], fill = colNamesss[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rw")+
  theme(aspect.ratio=1, text = element_text(size=20))

## rwd
int3 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[5], fill = colNamesss[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rwd")+
  theme(aspect.ratio=1, text = element_text(size=20))

## mstlarima
int4 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[1], fill = colNamesss[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("mstlarima")+
  theme(aspect.ratio=1, text = element_text(size=20))

## mstlets
int5 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[2], fill = colNamesss[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("mstlets")+
  theme(aspect.ratio=1, text = element_text(size=20))

## tbats
int6 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[8], fill = colNamesss[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("tbats")+
  theme(aspect.ratio=1, text = element_text(size=20))

## stlar
int7 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[7], fill = colNamesss[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("stlar")+
  theme(aspect.ratio=1, text = element_text(size=20))

## theta
int8 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[9], fill = colNamesss[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("theta")+
  theme(aspect.ratio=1, text = element_text(size=20))

## nn
int9 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[3], fill = colNamesss[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.2), breaks = seq(0, 0.2, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("nn")+
  theme(aspect.ratio=1, text = element_text(size=20))

## wn
int10 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[10], fill = colNamesss[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ggtitle("wn")+theme(legend.title=element_blank())+
  theme(aspect.ratio=1, text = element_text(size=20))

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+plot_layout(ncol = 5, nrow = 2)



## ---- dtwopdp
load("data/daily/sediff_acf5.seasonal_strength2.d.rda")
colNamesss <- colnames(sediff_acf5.seasonal_strength2.d)[28:37]
# snaive
int1 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[6], fill = colNamesss[6]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("snaive")+theme(aspect.ratio=1, text = element_text(size=20))

## rw
int2 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[4], fill = colNamesss[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("rw")+theme(aspect.ratio=1, text = element_text(size=20))
## rwd
int3 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[5], fill = colNamesss[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("rwd")+theme(aspect.ratio=1, text = element_text(size=20))
## mstlarima
int4 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[1], fill = colNamesss[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("mstlarima")+theme(aspect.ratio=1, text = element_text(size=20))
## mstlets
int5 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[2], fill = colNamesss[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.1), breaks = seq(0, 0.1, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("mstlets")+theme(aspect.ratio=1, text = element_text(size=20))
## tbats
int6 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[8], fill = colNamesss[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("tbats")+theme(aspect.ratio=1, text = element_text(size=20))
## stlar
int7 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[7], fill = colNamesss[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("stlar")+theme(aspect.ratio=1, text = element_text(size=20))
## theta
int8 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[9], fill = colNamesss[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("theta")+theme(aspect.ratio=1, text = element_text(size=20))
## nn
int9 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[3], fill = colNamesss[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.3), breaks = seq(0, 0.3, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+ theme(legend.position="none")+
  ggtitle("nn")+theme(aspect.ratio=1, text = element_text(size=20))
## wn
int10 <- ggplot(
  data = sediff_acf5.seasonal_strength2.d,
  aes_string(
    x = sediff_acf5.seasonal_strength2.d$sediff_acf5,
    y = sediff_acf5.seasonal_strength2.d$seasonal_strength2, 
    z = colNamesss[10], fill = colNamesss[10]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.9), breaks = seq(0, 0.9, 100), option = "A", direction = -1) +
  xlab("sediff_acf5") + ylab("seasonal_strength2")+theme(legend.title=element_blank())+
  ggtitle("wn")+theme(aspect.ratio=1, text = element_text(size=20))

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+plot_layout(ncol = 5, nrow = 2)
