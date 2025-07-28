# Figure 4. Discharge dynamics ----

# Author: Adam Rexroade
# Project: Education Creek Source and Sink
# Date Created: February 27, 2025
# Date Last Modified: March 11, 2025

# Description: Calculate CQ, MQ, FQ relationships
# Modified from E_CQ_CF_powerrelationships.R



library(tidyverse)
library(ggpubr)
library(patchwork)
library(segmented)# MASS is a dependent and MASS overrides "select" argument in dplyr, how fucking stupid is that ???
library(MASS, exclude = "select") 
library(gt)




#1. Load and inital formatting of data ----
timeseries <- read_csv("Education/Clean/PublishedData/timeseries.csv")

flux <-  read_csv("Education/Clean/PublishedData/CO2endmembers.csv") %>% 
  dplyr::select(Date, CO2.atm, CO2.met, CO2.lat)

# summarize at daily level
hf <- timeseries %>% 
  group_by(Date) %>%
  summarize(CO2M.up=mean(CO2M.up, na.rm = TRUE),
            Q.up=mean(Q.up, na.rm = TRUE),
            CO2M.dn=mean(CO2M.dn, na.rm = TRUE),
            Q.dn=mean(Q.dn, na.rm = TRUE)) %>% 
  mutate(Month=lubridate::month(Date))

#join data
cf <- full_join(flux, hf, by=c("Date")) %>% 
  mutate(Month=month(Date)) %>%
  mutate(Flux=(CO2.atm*-1)/3022.5,
         Met=CO2.met/3022.5,
         Lat=CO2.lat/1209)%>% 
  na.omit()


##1.1 Add season indicator ----

hf$Season=NA
for (i in 1:nrow(hf)) {
  if (hf$Month[i]>4 & hf$Month[i]<11){
    hf$Season[i]="Dry"
  } else{
    hf$Season[i]="Wet"
  }
}

hf$Month <- as.factor(hf$Month)


cf$Season=NA
for (i in 1:nrow(cf)) {
  if (cf$Month[i]>4 & cf$Month[i]<11){
    cf$Season[i]="Dry"
  } else{
    cf$Season[i]="Wet"
  }
}


#2 C-Q regressions ----
## 2.1 Upstream Models ----

# format data
up <- hf %>% 
  mutate(lgQ=log(Q.up),
         lgCO2=log(CO2M.up)) %>% 
  dplyr::select(Date, Month, Season, lgQ, lgCO2) %>% 
  na.omit()

# find starting breakpoint
up.start.bkpt <- median(up$lgQ, na.rm = TRUE)

#establish linear regression
linearCQ.up <- lm(lgCO2~lgQ, up)

#davies test
Cqdavies <- davies.test(linearCQ.up, seg.Z = ~lgQ, k=10, alternative=c("two.sided", "less", "greater"), values=NULL, dispersion = NULL)

# establish segmented regression and extract model info
cq.seg <- segmented(linearCQ.up,
                    seg.Z = ~lgQ,
                    psi=up.start.bkpt)
summary(cq.seg)

cq.seg.p <- summary.segmented(cq.seg)$psi[1,2]
cq.seg.p.se <- summary.segmented(cq.seg)$psi[1,3]

cq.seg.slope <- slope(cq.seg)$lgQ[1:2,1]
cq.seg.slope.se <- slope(cq.seg)$lgQ[1:2,2]

fitted.up.cq <- fitted(cq.seg)
fitted.up.cq.df <- data.frame(Q=up$lgQ, C=fitted.up.cq)

# Plot 
cq.seg.up.plot <- ggplot()+
  geom_point(data=up, aes(x=exp(lgQ), y=exp(lgCO2), colour=Season), alpha=.3)+
  geom_line(data=fitted.up.cq.df, aes(x=exp(Q), y=exp(C)), color="grey30",linewidth=1.5)+
  geom_vline(aes(xintercept = exp(cq.seg.p)), color="grey30", linetype="dashed", linewidth=1.3)+
  scale_y_log10(breaks=c(250, 100, 500), labels=c(250, 100, 500))+scale_x_log10()+
  xlab(expression(Q~(L~s^-1)))+
  ylab(expression(CO[2]~(mu*M)))+
  scale_colour_manual(values = c("goldenrod3", "deepskyblue3"))+
  theme_bw()


## 2.2 Downstream Models ----
# format data
dn <- hf %>% 
  mutate(lgQ=log(Q.dn),
         lgCO2=log(CO2M.dn)) %>% 
  dplyr::select(Date, Month, Season, lgQ, lgCO2) %>% 
  na.omit()

# find starting breakpoint
dn.start.bkpt <- median(dn$lgQ, na.rm = TRUE)

#establish linear regression
linearCQ.dn <- lm(lgCO2~lgQ, dn)

#davies test
Cq.dn.davies <- davies.test(linearCQ.dn, seg.Z = ~lgQ, k=10, alternative=c("two.sided", "less", "greater"), values=NULL, dispersion = NULL)

# establish segmented regression and extract model info
cq.seg.dn <- segmented(linearCQ.dn,
                       seg.Z = ~lgQ,
                       psi=dn.start.bkpt)
summary(cq.seg.dn)

cq.seg.dn.p <- summary.segmented(cq.seg.dn)$psi[1,2]
cq.seg.dn.p.se <- summary.segmented(cq.seg.dn)$psi[1,3]

cq.seg.dn.slope <- slope(cq.seg.dn)$lgQ[1:2,1]
cq.seg.dn.slope.se <- slope(cq.seg.dn)$lgQ[1:2,2]

fitted.dn.cq <- fitted(cq.seg.dn)
fitted.dn.cq.df <- data.frame(Q=dn$lgQ, C=fitted.dn.cq)

# calculate z score to test for significant differences between slopes 


# Plot 
cq.seg.dn.plot <- ggplot()+
  geom_point(data=dn, aes(x=exp(lgQ), y=exp(lgCO2), colour=Season), alpha=.3)+
  geom_line(data=fitted.dn.cq.df, aes(x=exp(Q), y=exp(C)), color="grey30",linewidth=1.5)+
  geom_vline(aes(xintercept = exp(cq.seg.dn.p)), color="grey30", linetype="dashed",linewidth=1.3)+
  scale_y_log10()+scale_x_log10()+
  xlab(expression(Q~(L~s^-1)))+
  ylab(expression(CO[2]~(mu*M)))+
  scale_colour_manual(values = c("goldenrod3", "deepskyblue3"))+
  theme_bw()

# 3 F-Q regressions ----
## single model based on downstream Q

atm.fq <- cf %>% 
  mutate(lgF=log(Flux),
         lgQ=log(Q.dn)) %>% 
  dplyr::select(Date, Month, Season, lgQ, lgF)  %>% 
  na.omit()

# find starting breakpoint
atm.starting.bkpt <- median(atm.fq$lgQ, na.rm = TRUE)

#establish linear regression
linearCF.dn <- lm(lgF~lgQ, atm.fq)

#davies test
CF.dn.davies <- davies.test(linearCF.dn, seg.Z = ~lgQ, k=10, alternative=c("two.sided", "less", "greater"), values=NULL, dispersion = NULL)


# establish segmented regression and extract model info
atm.cf.seg <- segmented(linearCF.dn,
                       seg.Z = ~lgQ,
                       psi=atm.starting.bkpt)
summary(atm.cf.seg)

atm.cf.seg.p <- summary.segmented(atm.cf.seg)$psi[1,2]
atm.cf.seg.p.se <- summary.segmented(atm.cf.seg)$psi[1,3]

atm.cf.seg.slope <- slope(atm.cf.seg)$lgQ[1:2,1]
atm.cf.seg.slope.se <- slope(atm.cf.seg)$lgQ[1:2,2]

fitted.dn.cf <- fitted(atm.cf.seg)
fitted.dn.cf.df <- data.frame(Q=atm.fq$lgQ, Fl=fitted.dn.cf)
# Plot 
atm.cf.seg.plot <- ggplot()+
  geom_point(data=atm.fq, aes(x=exp(lgQ), y=exp(lgF), colour=Season), alpha=.3)+
  geom_line(data=fitted.dn.cf.df, aes(x=exp(Q), y=exp(Fl)), color="grey30", linewidth=1.5)+
  geom_vline(aes(xintercept = exp(atm.cf.seg.p)), color="grey30", linetype="dashed",linewidth=1.3)+ 
  scale_y_log10()+scale_x_log10()+
  xlab(expression(Q~(L~s^-1)))+
  ylab(expression(emission~(g~C-CO[2]~m^-2~d^-1)))+
  scale_colour_manual(values = c("goldenrod3", "deepskyblue3"))+
  theme_bw()

#4.3 M-Q regressions ----
## single model based on downstream Q
met.mq <- cf %>% 
  mutate(lgM=log(Met),
         lgQ=log(Q.dn)) %>% 
  dplyr::select(Date, Month, Season, lgQ, lgM)  %>% 
  na.omit()

# find starting breakpoint
met.starting.bkpt <- median(met.mq$lgQ, na.rm = TRUE)

#establish linear regression
linearCM.dn <- lm(lgM~lgQ, met.mq)

#davies test
Cm.dn.davies <- davies.test(linearCM.dn, seg.Z = ~lgQ, k=10, alternative=c("two.sided", "less", "greater"), values=NULL, dispersion = NULL)


# establish segmented regression and extract model info
met.mq.seg <- segmented(linearCM.dn,
                       seg.Z = ~lgQ,
                       psi=met.starting.bkpt)
summary(met.mq.seg)

met.mq.seg.p <- summary.segmented(met.mq.seg)$psi[1,2]
met.mq.seg.p.se <- summary.segmented(met.mq.seg)$psi[1,3]

met.mq.seg.slope <- slope(met.mq.seg)$lgQ[1:2,1]
met.mq.seg.slope.se <- slope(met.mq.seg)$lgQ[1:2,2]


fitted.dn.cm <- fitted(met.mq.seg)
fitted.dn.cm.df <- data.frame(Q=met.mq$lgQ, M=fitted.dn.cm)
# Plot 
met.mq.seg.plot <- ggplot()+
  geom_point(data=met.mq, aes(x=exp(lgQ), y=exp(lgM), colour=Season), alpha=.3)+
  geom_line(data=fitted.dn.cm.df, aes(x=exp(Q), y=exp(M)), color="grey30" ,linewidth=1.5)+
  geom_vline(aes(xintercept = exp(met.mq.seg.p)), color="grey30", linetype="dashed" ,linewidth=1.3)+
  scale_y_log10()+scale_x_log10()+
  xlab(expression(Q~(L~s^-1)))+
  ylab(expression(NEP(g~C-CO[2]~m^-2~d^-1)))+
  scale_colour_manual(values = c("goldenrod3", "deepskyblue3"))+
  theme_bw()
#5 Lateral flux-Q ----


lat.fq <- cf %>% 
  mutate(lgL=log(Lat),
         lgQ=log(Q.dn)) %>% 
  dplyr::select(Date, Month, Season, lgL, lgQ)  %>% 
  na.omit()

# find starting breakpoint
lat.starting.bkpt <- median(lat.fq$lgQ, na.rm = TRUE)

#establish linear regression
linearLQ.dn <- lm(lgL~lgQ, lat.fq)

#davies test
LQ.dn.davies <- davies.test(linearLQ.dn, seg.Z = ~lgQ, k=10, alternative=c("two.sided", "less", "greater"), values=NULL, dispersion = NULL)


# establish segmented regression and extract model info
LQ.seg.dn <- segmented(linearLQ.dn,
                       seg.Z = ~lgQ,
                       psi=lat.starting.bkpt)
summary(LQ.seg.dn)

lg.seg.dn.p <- summary.segmented(LQ.seg.dn)$psi[1,2]
lg.seg.dn.p.se <- summary.segmented(LQ.seg.dn)$psi[1,3]

lg.seg.dn.slope <- slope(LQ.seg.dn)$lgQ[1:2,1]
lg.seg.dn.slope.se <- slope(LQ.seg.dn)$lgQ[1:2,2]

fitted.dn.lq <- fitted(LQ.seg.dn)
fitted.dn.lq.df <- data.frame(Q=lat.fq$lgQ, LQ=fitted.dn.lq)
# Plot 
lq.seg.dn.plot <- ggplot()+
  geom_point(data=lat.fq, aes(x=exp(lgQ), y=exp(lgL), colour=Season), alpha=.3)+
  geom_line(data=fitted.dn.lq.df, aes(x=exp(Q), y=exp(LQ)), color="grey30", linewidth=1.5)+
  geom_vline(aes(xintercept = exp(lg.seg.dn.p)), color="grey30", linetype="dashed",linewidth=1.3)+ 
  scale_y_log10()+scale_x_log10()+
  xlab(expression(Q~(L~s^-1)))+
  ylab(expression(lateral~flux~(g~C-CO[2]~m^-1~d^-1)))+
  scale_colour_manual(values = c("goldenrod3", "deepskyblue3"))+
  theme_bw()



#6 Composite plot ----

layout <- "AB
CC
DD
EE"

compositeSeg <- 
  cq.seg.up.plot+
  cq.seg.dn.plot+
  lq.seg.dn.plot+
  atm.cf.seg.plot+
  met.mq.seg.plot+
  plot_layout(design = layout, guides = "collect", axis="collect")+
  plot_annotation(tag_levels = "a")

ggsave("Education/Figures/PublishedFigures/Figure4.png", plot=compositeSeg,
       height = 8.5, width = 7, units="in", dpi=300)



