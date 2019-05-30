## ---- pdphourly
load("data/hourly/hiceout/curvaturegridH.rda")
curvaturegridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff1y_acf1gridH.rda")
diff1y_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff1y_acf5gridH.rda")
diff1y_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff1y_pacf5gridH.rda")
diff1y_pacf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff2y_acf1gridH.rda")
diff2y_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff2y_acf5gridH.rda")
diff2y_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/diff2y_pacf5gridH.rda")
diff2y_pacf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/e_acf1gridH.rda")
e_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/entropygridH.rda")
entropygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/hurstgridH.rda")
hurstgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/linearitygridH.rda")
linearitygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/lumpinessgridH.rda")
lumpinessgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/NgridH.rda")
NgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/nonlinearitygridH.rda")
nonlinearitygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seas_pacfgridH.rda")
seas_pacfgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality1gridH.rda")
seasonality1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/seasonality2gridH.rda")
seasonality2gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/sediff_acf1gridH.rda")
sediff_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/sediff_acf5gridH.rda")
sediff_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/sediff_seacf1gridH.rda")
sediff_seacf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/spikinessgridH.rda")
spikinessgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/stabilitygridH.rda")
stabilitygridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/trendgridH.rda")
trendgridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/y_acf1gridH.rda")
y_acf1gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/y_acf5gridH.rda")
y_acf5gridH$variable <- rep(1:1000, 20)
load("data/hourly/hiceout/y_pacf5gridH.rda")
y_pacf5gridH$variable <- rep(1:1000, 20)


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
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("snaive")+
  theme(aspect.ratio=1, text = element_text(size=20))

## rw
int2 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[4], fill = colNamesss[4]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rw")+
  theme(aspect.ratio=1, text = element_text(size=20))

## rwd
int3 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[5], fill = colNamesss[5]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("rwd")+
  theme(aspect.ratio=1, text = element_text(size=20))

## mstlarima
int4 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[1], fill = colNamesss[1]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("mstlarima")+
  theme(aspect.ratio=1, text = element_text(size=20))

## mstlets
int5 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[2], fill = colNamesss[2]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("mstlets")+
  theme(aspect.ratio=1, text = element_text(size=20))

## tbats
int6 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[8], fill = colNamesss[8]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("tbats")+
  theme(aspect.ratio=1, text = element_text(size=20))

## stlar
int7 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[7], fill = colNamesss[7]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("stlar")+
  theme(aspect.ratio=1, text = element_text(size=20))

## theta
int8 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[9], fill = colNamesss[9]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
  xlab("linearity") + ylab("sediff_seacf1")+ theme(legend.position="none")+ggtitle("theta")+
  theme(aspect.ratio=1, text = element_text(size=20))

## nn
int9 <- ggplot(
  data = linearity.sediff_seacf1.h,
  aes_string(
    x = linearity.sediff_seacf1.h$linearity,
    y = linearity.sediff_seacf1.h$sediff_seacf1, z = colNamesss[3], fill = colNamesss[3]
  )) + geom_tile() +
  scale_fill_viridis_c(limits = c(0, 0.8), breaks = seq(0, 0.8, 100), option = "A", direction = -1) +
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
  xlab("linearity") + ylab("sediff_seacf1")+ggtitle("wn")+theme()+
  theme(aspect.ratio=1, text = element_text(size=20))

int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+plot_layout(ncol = 5, nrow = 2)


