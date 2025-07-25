# Figure 2: Overview of data

# Author: Adam Rexroade
# Project: Education Creek Source and Sink
# Date Created: October 21, 2024
# Date Last Modified: March 14, 2025

# Description: Assemble figure 2, overview of data

#1. Load data ----

## 1.1 Packages and load functions ----
library(tidyverse)
library(patchwork)
library(scales)
library(grid)

source("Education/Code/F_LoadBOMData.R")
source("Education/Code/F_LoadCleanData.R")
source("Education/Code/F_LoadSynData.R")


## 1.2 Data loaded and filtered with water year ----
rain <-read_csv("Education/Raw/BOM/IDCJAC0009_014279_1800_Complete/IDCJAC0009_014279_1800_Data.csv") %>% 
  mutate(Date=ymd(paste(Year, Month, Day, sep="-"))) %>% 
  select(Date, `Rainfall amount (millimetres)`) %>% 
  rename(Precip= `Rainfall amount (millimetres)`) %>% 
  select(Date, Precip) %>% 
  filter(Date> "2023-11-01" & Date < "2024-10-31")

hf <- read_csv("Education/Clean/PublishedData/Timeseries.csv")

syn <- read_csv("Education/Clean/PublishedData/SynopticSamples.csv")

## 1.3 set wet season boundaries (ggobjects) ----

wetggdate <- geom_rect(aes(xmin= lubridate::ymd("2023-11-01"), xmax = lubridate::ymd("2024-04-01"), ymin=0, ymax=Inf),
                   alpha=.05, fill="slategray1")

wetggdatetime <- geom_rect(aes(xmin= lubridate::ymd_hms("2023-11-01 00:00:10"), xmax = lubridate::ymd_hms("2024-04-01 00:00:10"), ymin=0, ymax=Inf),
                           alpha=.05, fill="slategray1")

## 1.4 Set Saturation Values ----

satco2 <- 13.3
satch4 <- 0.0025
satdo <- 7.6
# 2. panel a Precip figure ----

precip <- 
  ggplot(rain)+
  wetggdate+
  geom_col(aes(x=Date, y=Precip))+
  ylab("Precipitaion (mm)")+
  labs(tag = 'a')+
  theme_bw()

# 3. panel b discharge

hf_q <- hf %>% 
  select(DateTime, Q.up, Q.dn) %>% 
  pivot_longer(cols = c("Q.up", "Q.dn"), names_prefix = "Q.") %>% 
  rename(Q=value,
         Location=name)

hf_q$Location <- factor(hf_q$Location,
                        levels=c("up", "dn"),
                        labels = c("Upstream", "Downstream"))
q <- 
  ggplot(hf_q)+
  wetggdatetime+
  geom_line(aes(x=DateTime, y=Q, colour=Location))+
  scale_colour_manual(values=c("darkseagreen4", "chocolate3"))+
  scale_y_log10(breaks =c(1,10,100,1000), 
                labels = trans_format("log10", math_format(10^.x)))+
  xlab("Date")+ylab(expression(Q~(L~s^-1)))+
  labs(tag = 'b')+
  theme_bw()


# 4. panels c-e temp, do, co2,  ----

## 4.1 temp ----
hf_temp <- hf %>% 
  select(DateTime, Temp.up, Temp.dn) %>% 
  pivot_longer(cols = c("Temp.up", "Temp.dn"), names_prefix = "Temp.") %>% 
  rename(Temp=value,
         Location=name)

hf_temp$Location <- factor(hf_temp$Location,
                        levels=c("up", "dn"),
                        labels = c("Upstream", "Downstream"))
temp <- 
  ggplot(hf_temp)+
  wetggdatetime+
  geom_line(aes(x=DateTime, y=Temp, colour=Location))+
  scale_colour_manual(values=c("darkseagreen4", "chocolate3"))+
  scale_y_log10()+
  xlab("Date")+ylab("Temp (C)")+
  labs(tag = 'c')+
  theme_bw()

## 4.2 DO ----
hf_do <- hf %>% 
  select(DateTime, DO.up, DO.dn) %>% 
  pivot_longer(cols = c("DO.up", "DO.dn"), names_prefix = "DO.") %>% 
  rename(DO=value,
         Location=name)

hf_do$Location <- factor(hf_do$Location,
                           levels=c("up", "dn"),
                           labels = c("Upstream", "Downstream"))


do <- 
  ggplot(hf_do)+
  wetggdatetime+
  geom_hline(yintercept = satdo, linetype="dashed", color = "grey40", linewidth=0.8)+
  geom_line(aes(x=DateTime, y=DO, colour=Location))+
  scale_colour_manual(values=c("darkseagreen4", "chocolate3"))+
  scale_y_log10()+
  xlab("Date")+ylab(expression(DO~(mg~L^-1)))+
  labs(tag = 'd')+
  theme_bw()

## 4.2 CO2 ----
hf_co2 <- hf %>% 
  select(DateTime, CO2M.up, CO2M.dn) %>% 
  pivot_longer(cols = c("CO2M.up", "CO2M.dn"), names_prefix = "CO2M.") %>% 
  rename(CO2=value,
         Location=name)

hf_co2$Location <- factor(hf_co2$Location,
                         levels=c("up", "dn"),
                         labels = c("Upstream", "Downstream"))

co2 <- 
  ggplot(hf_co2)+
  wetggdatetime+
  geom_hline(yintercept = satco2, linetype="dashed", color = "grey40", linewidth=0.8)+
  geom_line(aes(x=DateTime, y=CO2, colour=Location))+
  scale_colour_manual(values=c("darkseagreen4", "chocolate3"))+
  scale_y_log10()+
  xlab("Date")+ylab(expression(CO[2]~(mu*M)))+
  labs(tag = 'e')+
  theme_bw()

# 5. panels f-h ch4, piezo co2, piezo ch4 ----

## 5.1 ch4 ----
ck <- syn %>% 
  filter(ID=="EC" | ID=="EC.DS") %>% 
  rename(Location=ID)

ck$Location <- factor(ck$Location,
                      levels=c("EC", "EC.DS"),
                      labels = c("Upstream", "Downstream"))


ch4 <- 
  ggplot(ck[!is.na(ck$CH4),])+
  wetggdate+
  geom_hline(yintercept = satch4, linetype="dashed", color = "grey40", linewidth=0.8)+
  geom_point(aes(x=Date, y=CH4*1000000, colour=Location))+
  geom_line(aes(x=Date, y=CH4*1000000, colour=Location))+
  scale_colour_manual(values=c("darkseagreen4", "chocolate3"))+
#  scale_y_log10()+
  xlab("Date")+ylab(expression(CH[4]~(mu*M)))+
  labs(tag = 'f')+
  theme_bw()+
  theme(legend.position = "none")

## 5.2 piezo co2 and ch4----

piezo <- syn %>% 
  filter(System=="PZ")


gwco2 <- 
  ggplot(piezo)+
  wetggdate+
  geom_boxplot(aes(x=Date, y=CO2*1000000, group = Date), outlier.shape = NA)+
  geom_jitter(aes(x=Date, y=CO2*1000000, group = Date), shape=1)+
  ylab(expression(CO[2]~(mu*M)))+
  labs(tag = 'g')+
  theme_bw()

gwch4 <- 
  ggplot(piezo)+
  wetggdate+
  geom_boxplot(aes(x=Date, y=CH4*1000000, group = Date), , outlier.shape = NA)+
  geom_jitter(aes(x=Date, y=CH4*1000000, group = Date), shape=1)+
  ylab(expression(CH[4]~(mu*M)))+
  labs(tag = 'h')+
  theme_bw()



# 6. create figure----

design <- "
AA
BC
BC
BC
BC
DE
DE
DE
DE
FG
FG
FG
FG
HH
IJ
IJ
IJ
IJ"


mergedfig <-   
  patchwork::plot_spacer()+
  precip+q+
  temp+do+
  co2+ch4+
  patchwork::plot_spacer()+
  gwco2+gwch4+
  plot_layout(guides = 'collect', axis_titles = "collect_x",  design = design)&
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

# modify specific plot elements 
mergedfig[[7]] <- mergedfig[[7]]+theme(legend.position = "none")
mergedfig[[1]] <-  textGrob("Stream Water")
mergedfig[[8]] <- textGrob("Shallow Groundwater")


mergedfig