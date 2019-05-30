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

## ---- yearlyoob
load("data/yearly/yearly_training.rda") # random forest training set 
load("data/yearly/train_votes.rda") # oob votes from the random forest (see: yearly_cluster_results for more info)
load("data/yearly/train_predictions_oob.rda") # based on oob prediction (see: yearly_cluster_results for more info)
votes_oob <- data.frame(train_votes)
names(votes_oob) <- names(table(train_predictions_oob))
votes_oob$predicted <- train_predictions_oob
votes_oob$classlabel <- yearly_training$classlabels
votes_oob <- votes_oob %>%
  mutate(id = seq_len(n())) %>%
  melt(id.var = c("classlabel", "id", "predicted"), na.rm = T) %>%
  select(-id)
votes_oob <- votes_oob %>%
  mutate(classlabel = recode(classlabel, nn="nn",
                             theta = "theta", wn = "wn", "ARMA/AR/MA" = "ARMA", ARIMA = "ARIMA", "ETS-notrendnoseasonal" = "ETS_NTNS",
                             "ETS-dampedtrend" = "ETS_DT", "ETS-trend" = "ETS_T", "rwd" = "rwd", "rw" = "rw" ))

votes_oob <- votes_oob %>%
  mutate(predicted = recode(predicted, nn="nn", theta = "theta",
                            wn = "wn", "ARMA/AR/MA" = "ARMA", ARIMA = "ARIMA",
                            "ETS-notrendnoseasonal" = "ETS_NTNS", "ETS-dampedtrend" = "ETS_DT",
                            "ETS-trend" = "ETS_T","rwd" = "rwd", "rw" = "rw" ))

votes_oob <- votes_oob %>%
  mutate(variable = recode(variable, nn="nn", theta = "theta", wn = "wn", "ARMA/AR/MA" = "ARMA",
                           ARIMA = "ARIMA", "ETS-notrendnoseasonal" = "ETS_NTNS", "ETS-dampedtrend" = "ETS_DT",
                           "ETS-trend" = "ETS_T", "rwd" = "rwd", "rw" = "rw" ))
# arrange labels
votes_oob$variable <- factor(votes_oob$variable,
                             levels = rev(c(
                               "nn",
                               "theta",
                               "wn",
                               "ARMA",
                               "ARIMA",
                               "ETS_NTNS",
                               "ETS_DT",
                               "ETS_T",
                               "rwd",
                               "rw" )))

oob_boxplot_yearly <- ggplot(votes_oob, aes(x = classlabel, y = value, fill = classlabel)) +
  geom_boxplot(outlier.size = 0.2, outlier.alpha = 0.4) +
  ylab("Proportion") +
  xlab("") +
  theme(legend.position = "none", legend.title = element_blank(), 
        legend.text.align = 0, text = element_text(size = 25), axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_discrete(limits = c("nn", "theta", "wn", "ARMA", "ARIMA", "ETS_NTNS", "ETS_DT", "ETS_T", "rwd", "rw" )) +
  coord_flip() + facet_wrap(. ~ variable, ncol=5)
oob_boxplot_yearly



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
  scale_fill_manual(breaks=c("0","1"), values=c("#f1a340","#998ec3"), guide="none")+theme(text=element_text(size = 20))+
  theme(strip.text.x = element_text(size = 18))
feaImp_yearly



## ---- pdpyearly
load("data/yearly/pdp_yearly/trendgrid.rda")
trendgrid$variable <- rep(1:1000, 20)
# removing outliers
load("data/yearly/pdp_yearly/ur_ppgrid_rmout.rda")
ur_ppgrid_rmout$variable <- rep(1:1000, 20)


# ## rwd
# p1 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "rwd")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
#   stat_summary(fun.data = mean_cl_normal, fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
#   theme(legend.position = "none",text = element_text(size=10)) + ggtitle("rwd")+ylab("")
# p7 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ETS.trend")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
#   stat_summary(fun.data = mean_cl_normal, fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + 
#   theme(legend.position = "none", text = element_text(size=10)) + xlab("ur_pp") + ggtitle("ETS-trend")+ylab("")
# p16 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ARIMA")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
#   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
#   theme(legend.position = "none", text = element_text(size=10)) + ggtitle("ARIMA")+ylab("")
# p19 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "ARMA.AR.MA")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
#   stat_summary(fun.data = mean_cl_normal, fill="red",geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
#   theme(legend.position = "none", text = element_text(size=10)) + ggtitle("ARMA")+ylab("")
# p22 <- ggplot(data = ur_ppgrid_rmout, aes_string(x = ur_ppgrid_rmout$ur_pp, y = "wn")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("ur_pp") +
#   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
#   theme(legend.position = "none", text = element_text(size=10)) + ggtitle("wn")+ylab("")
# 
# # trend
# p2 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "rwd")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
#   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + 
#   theme(legend.position = "none",text = element_text(size=10)) + ylab("")+theme(axis.text.x = element_text(angle = 90))
# p8 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ETS.trend")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
#   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + 
#   theme(legend.position = "none", text = element_text(size=10)) + ylab("")+theme(axis.text.x = element_text(angle = 90))
# p17 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ARIMA")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
#   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
#   theme(legend.position = "none", text = element_text(size=10)) + ylab("")+theme(axis.text.x = element_text(angle = 90))
# p20 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "ARMA.AR.MA")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
#   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) + 
#   theme(legend.position = "none", text = element_text(size=10)) + ylab("")+theme(axis.text.x = element_text(angle = 90))
# p23 <- ggplot(data = trendgrid, aes_string(x = trendgrid$trend, y = "wn")) +
#   stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("trend") +
#   stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +
#   theme(legend.position = "none", text = element_text(size=10)) + ylab("")+theme(axis.text.x = element_text(angle = 90))
# 
# (p1|p7|p16|p19|p22)/(p2|p8|p17|p20|p23)

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


## ---- viquarterly
# All variable scores into one dataframe
load("data/quarterly/trainQ_importance.rda")
load(file = "data/quarterly/sd_pdf_dfQ.rda")
load(file = "data/quarterly/sd_ice_dfQ.rda")
## Permutation based
train_imp_dfQ <- data.frame(trainQ_importance)
train_imp_dfQ <- add_rownames(train_imp_dfQ, "Feature")
train_imp_dfQ <- within(train_imp_dfQ, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impQ <- train_imp_dfQ %>% melt(id.vars = "Feature")
# dim(permutation_impQ) # 510 3
colnames(permutation_impQ) <- c("feature", "class", "score")
## PDP-based
sd_pdf_dfQ <- add_rownames(sd_pdf_dfQ, "class")
pdp_imp <- sd_pdf_dfQ %>% melt(id.vars = "class")
colnames(pdp_imp) <- c("class", "feature", "score")
## ICE-based
sd_ice_dfQ <- add_rownames(sd_ice_dfQ, "class")
ice_imp <- sd_ice_dfQ %>% melt(id.vars = "class")
colnames(ice_imp) <- c("class", "feature", "score")
## Combine the data frames
importancescoreQ <- bind_rows(permutation_impQ, pdp_imp)
importancescoreQ <- bind_rows(importancescoreQ, ice_imp)
importancescoreQ$VI <- rep(c("permutation", "PDP", "ICE"), each = 510)
## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreQ$class <- factor(importancescoreQ$class,
                                 levels = c(
                                   "snaive", "rwd", "rw", "ETS.notrendnoseasonal", "ETS.dampedtrend", "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal", "ETS.seasonal", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 )
)
rank_vi_quarterly_classes <- importancescoreQ %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_viq_classes <- rank_vi_quarterly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))
## overall importance of features to the forest
train_impforestQ <- data.frame(trainQ_importance)
train_impforestQ <- add_rownames(train_impforestQ, "Feature")
train_impforestQ <- train_impforestQ[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestQ <- train_impforestQ %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestQ$mean_rank <- (train_impforestQ$rank_permu + train_impforestQ$rank_gini) / 2
meanrank_viq_forest <- data.frame(
  feature = train_impforestQ$Feature,
  class = rep("overall", 30),
  rank = train_impforestQ$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_quarterly <- dplyr::bind_rows(meanrank_viq_forest, meanrank_viq_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_quarterly, class == "overall")
meanrank_quarterly$feature <- factor(meanrank_quarterly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_quarterly$class <- factor(meanrank_quarterly$class,
                                   levels = c(
                                     "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                     "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                   ),
                                   labels = c(
                                     "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                     "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
                                   )
)
meanrank_quarterly$rn <- 1:540
topq <- meanrank_quarterly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_quarterly$istop <- ifelse(meanrank_quarterly$rn %in% topq$rn, TRUE, FALSE)
feaImp_quarterly <- ggplot(meanrank_quarterly, aes(y = rank, x = feature,fill=as.factor(istop)), width=0.1) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
  theme(text=element_text(size = 9))
feaImp_quarterly

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

# seasonality
# pq13 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.notrendnoseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ggtitle("ETS.NTNS")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq14 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.notrendnoseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq17 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.dampedtrend")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ggtitle("ETS.DT")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq18 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.dampedtrend")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq21 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.trend")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ggtitle("ETS.T")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq22 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.trend")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3) +
#   theme(legend.position = "none") + theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq25 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.dampedtrendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+
#   theme(legend.position="none", text = element_text(size=10))+ggtitle("ETS.DTS")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq26 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.dampedtrendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq29 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.trendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ggtitle("ETS.TS")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq30 <- ggplot(data=trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.trendseasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq33 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ETS.seasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ggtitle("ETS.S")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq34 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ETS.seasonal")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# 
# (pq13|pq17|pq25|pq21|pq29|pq33)/(pq14|pq18|pq26|pq22|pq30|pq34)

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



# pq45 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="ARMA.AR.MA")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ggtitle("ARMA.AR.MA")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq46 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="ARMA.AR.MA")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq49 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="stlar")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ggtitle("stlar")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq50 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="stlar")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3)+theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq53 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="tbats")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ggtitle("tbats")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq54 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="tbats")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq61 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="theta")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ggtitle("theta")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq62 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="theta")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1),fill="red", alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq65 <- ggplot(data=seasonalitygridQ, aes_string(x=seasonalitygridQ$seasonality, y="nn")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonality")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ggtitle("nn")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# pq66 <- ggplot(data= trendgridQ, aes_string(x=trendgridQ$trend, y="nn")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("trend")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), fill="red",alpha = 0.3)+theme(legend.position="none", text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# (pq45|pq49|pq53|pq61|pq65)/(pq46|pq50|pq54|pq62|pq66)

## ---- vimonthly
# monthly feature importance
load("data/monthly/trainM_importance.rda")
load(file="data/monthly/sd_pdf_dfM.rda")
load(file="data/monthly/sd_ice_dfM.rda")
## Permutation based
train_imp_dfM <- data.frame(trainM_importance)
train_imp_dfM <- add_rownames(train_imp_dfM, "Feature")
train_imp_dfM <- within(train_imp_dfM, rm("MeanDecreaseAccuracy", "MeanDecreaseGini"))
permutation_impM <- train_imp_dfM %>% melt(id.vars = "Feature")
colnames(permutation_impM) <- c("feature", "class", "score")
## PDP-based
sd_pdf_dfM <- add_rownames(sd_pdf_dfM, "class")
pdp_impM <- sd_pdf_dfM %>% melt(id.vars = "class")
colnames(pdp_impM) <- c("class", "feature", "score")
## ICE-based
sd_ice_dfM <- add_rownames(sd_ice_dfM, "class")
ice_impM <- sd_ice_dfM %>% melt(id.vars = "class")
colnames(ice_impM) <- c("class", "feature", "score")
## Combine the data frames
importancescoreM <- bind_rows(permutation_impM, pdp_impM)
importancescoreM <- bind_rows(importancescoreM, ice_impM)
importancescoreM$VI <- rep(c("permutation", "PDP", "ICE"), each = 510)
## rank permutation, sd_pdp, and sd_ice scores for each class
importancescoreM$class <- factor(importancescoreM$class,
                                 levels = c(
                                   "snaive", "rwd", "rw", "ETS.notrendnoseasonal", "ETS.dampedtrend", "ETS.trend", "ETS.dampedtrendseasonal", "ETS.trendseasonal", "ETS.seasonal", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 )
)
rank_vi_monthly_classes <- importancescoreM %>%
  group_by(VI, class) %>%
  mutate(rank = min_rank(score))
## compute mean rank
meanrank_vim_classes <- rank_vi_monthly_classes %>% group_by(feature, class) %>% summarise_at(vars(c(rank)), funs(mean))
## overall importance of features to the forest
train_impforestM <- data.frame(trainM_importance)
train_impforestM <- add_rownames(train_impforestM, "Feature")
train_impforestM <- train_impforestM[, c("Feature", "MeanDecreaseAccuracy", "MeanDecreaseGini")]
train_impforestM <- train_impforestM %>%
  mutate(rank_permu = min_rank(MeanDecreaseAccuracy)) %>%
  mutate(rank_gini = min_rank(MeanDecreaseGini))
train_impforestM$mean_rank <- (train_impforestM$rank_permu + train_impforestM$rank_gini) / 2
meanrank_vim_forest <- data.frame(
  feature = train_impforestM$Feature,
  class = rep("overall", 30),
  rank = train_impforestM$mean_rank
)
## combine mean ranks for overall forest and separate classes
meanrank_monthly <- dplyr::bind_rows(meanrank_vim_forest, meanrank_vim_classes)
## create horizontal bar chart for ranks
orderOverall <- filter(meanrank_monthly, class == "overall")
meanrank_monthly$feature <- factor(meanrank_monthly$feature, levels = orderOverall$feature[order(orderOverall$rank)])
meanrank_monthly$class <- factor(meanrank_monthly$class,
                                 levels = c(
                                   "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA.AR.MA", "stlar", "tbats", "wn", "theta", "nn"
                                 ),
                                 labels = c(
                                   "overall", "snaive", "rwd", "rw", "ETS.NTNS", "ETS.DT", "ETS.T", "ETS.DTS", "ETS.TS", "ETS.S", "SARIMA",
                                   "ARIMA", "ARMA", "stlar", "tbats", "wn", "theta", "nn"
                                 )
)
meanrank_monthly$rn <- 1:540
topq <- meanrank_monthly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_monthly$istop <- ifelse(meanrank_monthly$rn %in% topq$rn, TRUE, FALSE)
feaImp_monthly <- ggplot(meanrank_monthly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 9, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
  theme(text=element_text(size = 9))
feaImp_monthly

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

## ----vihourly
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
                                levels = c(
                                  "overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                  "theta","nn","wn"
                                ),
                                labels = c(
                                  "overall","snaive", "rw", "rwd", "mstlarima", "mstlets", "tbats","stlar",
                                  "theta","nn","wn"
                                )
)

meanrank_hourly$rn <- 1:286
topq <- meanrank_hourly %>%
  group_by(class) %>%
  top_n(n = 5, wt = rank)
meanrank_hourly$istop <- ifelse(meanrank_hourly$rn %in% topq$rn, TRUE, FALSE)

meanrank_hourly$feature <- plyr::revalue(meanrank_hourly$feature, c("seasonal_strength1"="seasonal_daily", "seasonal_strength2"="seasonal_weekly"))

feaImp_hourly <- ggplot(meanrank_hourly, aes(y = rank, x = feature,fill=as.factor(istop))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~class, ncol = 6, nrow = 2) +
  coord_flip() + ylab("Average rank")+ 
  scale_fill_manual(breaks=c("0","1"), values=c("black","red"), guide="none")+
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

# #snaive
# p1 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="snaive")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("seasonal_daily")+ 
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fill="red", fun.args = list(mult = 1), alpha = 0.3) + theme(legend.position="none",text = element_text(size=10))+ggtitle("snaive")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ps1 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="snaive")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fill="red", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_weekly")+ theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ## rw
# p4 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="rw")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fill="red",fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_daily")+ theme(legend.position="none",text = element_text(size=10))+ggtitle("rw")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ps2 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="rw")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fill="red", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_weekly")+ theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ## mstlarima
# p10 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="mstlarima")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_daily")+ theme(legend.position="none",text = element_text(size=10))+ggtitle("mstlarima")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ps4 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="mstlarima")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_weekly")+ theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ## tbats
# p16 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="tbats")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("daily")+ theme(legend.position="none",text = element_text(size=10))+ggtitle("tbats")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ps6 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="tbats")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_weekly")+ theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ## nn
# p25 <- ggplot(data=seasonality1gridH, aes_string(x=seasonality1gridH$seasonal_strength1, y="nn")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_daily")+ theme(legend.position="none",text = element_text(size=10))+ggtitle("nn")+ylab("")+theme(axis.text.x = element_text(angle = 90))
# ps9 <- ggplot(data=seasonality2gridH, aes_string(x=seasonality2gridH$seasonal_strength2, y="nn")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, fill="red",geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3) +xlab("seasonal_weekly")+ theme(legend.position="none",text = element_text(size=10))+ylab("")+theme(axis.text.x = element_text(angle = 90))
# (p1|p4|p10|p16|p25)/(ps1|ps2|ps4|ps6|ps9)

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


## ---- quarterlylime
#which.min(m4qPCAresults1$PC1)  #405
#which.min(m4qPCAresults1$PC2)  #653
#which.max(m4qPCAresults1$PC1)  #908
#which.max(m4qPCAresults1$PC2)  #405
# which(0.46 < m4qPCAresults1$PC2 < 0.4692441) ##277166
load("data/quarterly/quarterly_training.rda")
load("data/quarterly/trainQ_votes.rda")
pcaQvariables <- quarterly_training[, 1:30]
pcaM4Q <- prcomp(pcaQvariables, center = TRUE, scale = TRUE)
PC1m4q <- pcaM4Q$x[, 1]
PC2m4q <- pcaM4Q$x[, 2]
PC3m4q <- pcaM4Q$x[, 3]
m4qPCAresults <- data.frame(PC1 = PC1m4q, PC2 = PC2m4q, PC3 = PC3m4q, pcaQvariables)
quarterly_training$PC1 <- PC1m4q
quarterly_training$PC2 <- PC2m4q

pcaQ <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  )
ggsave("pcaQ.png")

pcaQ <- ggplot(m4qPCAresults, aes(x = PC1, y = PC2)) +
  geom_point(colour = "firebrick1") +
  theme(
    legend.position = "none",
    aspect.ratio = 1
  ) +
  geom_point(data =quarterly_training[c(25,178, 653, 182),], aes(x = PC1, y = PC2), color = "black", size=5) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"), text = element_text(size=20))+
  geom_text_repel(
    data = quarterly_training[c(25,178, 653, 182),],
    aes(label = c("1: SARIMA", "2: rwd", "3: ETS-trendseasonal", "4: ETS-seasonal")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  )+ggtitle("A")

load("data/quarterly/explanationq.rda")
load("data/ts_lime_qpca.rda")
p1 <- autoplot(ts_lime_qpca[[1]])+theme(legend.position="none")+
  xlab("")+theme(axis.title.x=element_blank(),
                 #                     axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 text = element_text(size=20))+ylab("")+labs(
                   title = paste("B"),
                   subtitle = "1: SARIMA")

p2 <- autoplot(ts_lime_qpca[[2]])+theme(legend.position="none")+
  ggtitle("2: rwd")+xlab("")+theme(axis.title.x=element_blank(),
                                   #       axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),
                                   text = element_text(size=20))+ylab("")
p3 <- autoplot(ts_lime_qpca[[3]])+theme(legend.position="none")+
  ggtitle("3: ETS-trendseasonal")+xlab("")+theme(axis.title.x=element_blank(),
                                                 # axis.text.x=element_blank(),
                                                 axis.text.y=element_blank(),
                                                 text = element_text(size=20))+ylab("")
p4 <- autoplot(ts_lime_qpca[[4]])+theme(legend.position="none")+
  ggtitle("4: ETS-seasonal")+xlab("")+theme(axis.title.x=element_blank(),
                                            # axis.text.x=element_blank(),
                                            axis.text.y=element_blank(),
                                            text = element_text(size=20))+ylab("")

pp <- p1 + p2 + p3 + p4 + plot_layout(ncol = 1)
pcaQ2 <- pcaQ+pp+plot_layout(ncol = 2)
ggsave("img/pcaQ2.png")

## ---- corY
source("src/corrplot.R")
load("data/yearly/classlabelM1Y.rda")
clm1y <- classlabelM1Y$accuracy
mase_m1y <- clm1y[seq(1, nrow(clm1y), by = 2), ] 
## M3 competition yearly series
load("data/yearly/classlabelM3Y.rda") # on monash cluster
clm3y <- classlabelM3Y$accuracy
mase_m3y <- clm3y[seq(1, nrow(clm3y), by = 2), ] 
## MAR-based simulated time series
load("data/yearly/yearly_MARaccuracy.rda") # on monash cluster
## preparation of "Y" matrix
m1m3_mase <- rbind(mase_m1y, mase_m3y)
Y <- rbind(m1m3_mase, yearly_MARaccuracy)
Y <- data.frame(Y)
ggpairs(Y, upper = list(continuous = corrplot),axisLabels="none")