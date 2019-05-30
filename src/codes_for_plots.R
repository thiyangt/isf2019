## packages
#load libraries into R session
library(tidyverse)
library(Hmisc)
library(lme4)
library(nlme)

## time series plot
p2 <- ggplot(Milk, aes(x=Time, y=protein))+ geom_line(aes(group=Cow))+lab(y="")

## Pdp and ICE

df <- data.frame(x1=as.factor(c(11,11,11,12,12,12)), f=c(29,22,36,35,30,40), case=c(1,2,3,1,2,3),curve=c("PDP", "ICE", "ICE","PDP", "ICE", "ICE"))
df
ggplot(data=df, aes(x=x1, y=f, group=case,  colour=curve)) +
  geom_line(size=1.5) +
  geom_point()+ theme(text = element_text(size=20))+scale_color_manual(values=c("black","red"))

## Multiple frequency
load("data/ts_lime_hpca.rda")
msts_ts <- ts_lime_hpca[[4]] %>% msts(seasonal.periods = c(24,168))
mstsplot <- msts_ts%>% mstl() %>% autoplot()  
ggsave("img/mstsplot.png")

## entropy
library(forecast)
library(tsfeatures)
library(ggplot2)
library(patchwork)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)
a_ts <-  hourly_M4[[390]]$x %>% msts(seasonal.periods = c(24,168))
entropy(a_ts)
p1 <- autoplot(a_ts)+ggtitle("entropy: 0.85")+theme(text = element_text(size=20), axis.text.x=element_blank(), axis.text.y=element_blank())+xlab("")+ylab("")

b <- hourly_M4[[242]]$x %>% msts(seasonal.periods = c(24,168))
p2 <- autoplot(b)+ggtitle("entropy: 0.44")+theme(text = element_text(size=20), axis.text.x=element_blank(), axis.text.y=element_blank())+xlab("")+ylab("")

entropy <- p1/p2
ggsave("img/entropy.png")

## entropypdp
## ----hourlypdp
load("data/hourly/hiceout/entropygridH.rda")
entropygridH$variable <- rep(1:1000, 20)

keepEgrid <- c("entropy", "id", "snaive", "wn","variable")
entropygridH <- entropygridH[ , (names(entropygridH) %in% keepEgrid)]
entropygridH <- entropygridH[c("entropy", "id","variable", "snaive",
                                         "wn")]
entropygridH_long <- gather(entropygridH, class, probability, "snaive":"wn", factor_key=TRUE)

en_pdp2 <- ggplot(data = entropygridH_long, aes_string(x = entropygridH_long$entropy, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("entropy") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=12))+scale_x_continuous(limits = c(NA, 0.85))+
  facet_grid(. ~ class)

load("data/hourly/hiceout/entropygridH.rda")
entropygridH$variable <- rep(1:1000, 20)
entropygridH <- entropygridH[c("entropy", "id","variable", "rwd",
                               "tbats")]
entropygridH_long <- gather(entropygridH, class, probability, "rwd":"tbats", factor_key=TRUE)

en_pdp3 <- ggplot(data = entropygridH_long, aes_string(x = entropygridH_long$entropy, y = "probability")) +
  stat_summary(fun.y = mean, geom = "line", col = "red", size = 1) + xlab("entropy") +
  stat_summary(fun.data = mean_cl_normal,fill="red", geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3)+ 
  theme(axis.text.x = element_text(angle = 90),text = element_text(size=12))+scale_x_continuous(limits = c(NA, 0.85))+
  facet_grid(. ~ class)

en_pdp4 <- en_pdp2/en_pdp3

ggsave("img/en_pdp4.png")

# 
# p1 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="snaive")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+xlab("entropy")+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3, fill="red") +
#   theme(aspect.ratio=1,legend.position="none",text = element_text(size=10))+ylab("")+ggtitle("snaive")+
#   scale_x_continuous(limits = c(NA, 0.85))
# p2 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="nn")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3, fill="red") +xlab("entropy")+ 
#   theme(aspect.ratio=1,legend.position="none",text = element_text(size=10))+ylab("")+ggtitle("nn")+
#   scale_x_continuous(limits = c(NA, 0.85))
# p3 <- ggplot(data=entropygridH, aes_string(x=entropygridH$entropy, y="rwd")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3, fill="red") +xlab("entropy")+ 
#   theme(aspect.ratio=1,legend.position="none",text = element_text(size=10))+ylab("")+ggtitle("rwd")+
#   scale_x_continuous(limits = c(NA, 0.85))
# p4 <- ggplot(data= entropygridH, aes_string(x=entropygridH$entropy, y="tbats")) +
#   stat_summary(fun.y = mean, geom = "line", col="red", size=1)+
#   stat_summary(fun.data = mean_cl_normal, geom = "ribbon", fun.args = list(mult = 1), alpha = 0.3, fill="red") +xlab("entropy")+ 
#   theme(aspect.ratio=1,legend.position="none",text = element_text(size=10))+ylab("")+ggtitle("tbats")+
#   scale_x_continuous(limits = c(NA, 0.85))
# en_pdp <-  (p1|p2)/(p3|p4)
# ggsave("img/en_pdp.png")
