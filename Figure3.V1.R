# Mass Balance Function for Education Creek 


# Author: Adam Rexroade
# Project: Education Creek Source and Sink
# Date Created: Feburary 27, 2025
# Date Last Modified: July 25, 2025

# Description: Develop and plots mass balance for co2 (daily) ad ch4 (monthly)
# Modified form MassBalanceFunction.R

#1. Load Libnraries ----
library(tidyverse)
library(patchwork)

#2. Load Datasets ----

timeseries <- read_csv("Education/Clean/PublishedData/Timeseries.csv")
synoptic <- read_csv("Education/Clean/PublishedData/SynopticSamples.csv")
metabolism <- read_csv("Education/Clean/PublishedData/MetabolismOutput.csv")
source("Education/Code/F_BaseFlowSeparation.R")

#3 Methane -----

##3.1 Format time series data ----

hf <- timeseries %>% 
  group_by(Date) %>%
  summarize(DO.up=mean(DO.up, na.rm = TRUE),
            Temp.up=mean(Temp.up, na.rm = TRUE),
            WL.up=mean(WL.up, na.rm = TRUE),
            CO2M.up=mean(CO2M.up, na.rm = TRUE),
            PAR.up=mean(PAR.up, na.rm = TRUE),
            Q.up=mean(Q.up, na.rm = TRUE),
            
            DO.dn=mean(DO.dn, na.rm = TRUE),
            Temp.dn=mean(Temp.dn, na.rm = TRUE),
            WL.dn=mean(WL.dn, na.rm = TRUE),
            CO2M.dn=mean(CO2M.dn, na.rm = TRUE),
            PAR.dn=mean(PAR.dn, na.rm = TRUE),
            Q.dn=mean(Q.dn, na.rm = TRUE)) %>% 
  mutate(Month=lubridate::month(Date))

hf.monthly <- hf %>% 
  mutate(Q.in=Q.up * 86400,
         Q.in.err=Q.in*.1,
         Q.out=Q.dn *86400,
         Q.out.err=Q.out*.1,
         Q.gain=Q.out-Q.in,
         Q.gain.err=Q.out.err+Q.in.err) %>% 
  group_by(Month) %>% 
  summarize(Q.in=mean(Q.in, na.rm = TRUE),
            Q.in.err=mean(Q.in.err, na.rm = TRUE),
            Q.out=mean(Q.out, na.rm=TRUE),
            Q.out.err=mean(Q.out.err, na.rm = TRUE),
            Q.gain=mean(Q.gain, na.rm = TRUE),
            Q.gain.err=mean(Q.gain.err, na.rm = TRUE))

#3.2 Format upstream and downstream fluxes
inout <- synoptic %>% 
  filter(System=="CR",
         Date !="2024-02-28",
         Date <ymd( "2024-10-30")) %>% 
  mutate(Month=month(round_date(Date, "month"))) %>% 
  select(Month, ID, CH4) %>% 
  pivot_wider(values_from = CH4, names_from  = ID) %>% 
  mutate(CH4.in= EC*12.01, # converts M to g C-CH4
         CH4.out=EC.DS*12.01) %>% 
  select(Month, CH4.in, CH4.out)

# maual gap fil, linear  
inout$CH4.out <- replace(inout$CH4.out, inout$Month==5, as.numeric(paste(2.5*10^-7*12.01)))  
  
##3.3 Lateral Inputs

# get subserface runnoff concentration mean

subsurface <- synoptic %>% 
  filter(System=="SS") %>% 
  summarize(CH4.subsurf= mean(CH4, na.rm = TRUE) *12.01,
            CH4.subsurf.sd= sd(CH4, na.rm=TRUE *12.01) )#unit is M
  
# get groundwater concentration and isopote means
  
groundwater <- synoptic %>% 
  filter(System=="PZ") %>% 
  mutate(CH4.C=CH4*12.01) %>% 
  summarize(CH4.gw=mean(CH4.C, na.rm=TRUE),
            CH4.gw.sd=sd(CH4.C, na.rm=TRUE),
            CH4.13.gw = mean(CH4.13, na.rm=TRUE),
            CH4.13.gw.sd= sd(CH4.13, na.rm=TRUE))

# baseflow separation

baseflow.up <- hf %>% 
  filter(is.na(Q.up)==FALSE) %>% 
  mutate(Baseflow.up=BaseflowSeparation(Q.up, filter_parameter=.9)[,1], passes=5) %>% 
  select(Date, Baseflow.up)

baseflow.dn <- hf %>% 
  filter(is.na(Q.dn)==FALSE) %>% 
  mutate(Baseflow.dn=BaseflowSeparation(Q.dn, filter_parameter=.9, passes=5)[,1]) %>% 
  select(Date, Baseflow.dn)

baseflow.tmp <- full_join(baseflow.up, baseflow.dn, by=c("Date"))

df.base <- left_join(hf,baseflow.tmp, by="Date") %>% 
  mutate(BF.pct.up= (Baseflow.up/Q.up)*100,
         BF.pct.dn= (Baseflow.dn/Q.dn)*100,
         SF.pct.up=100-BF.pct.up,
         SF.pct.dn=100-BF.pct.dn) %>% 
  select(Date, 
         Q.up, Q.dn, 
         BF.pct.up, BF.pct.dn,
         SF.pct.up, SF.pct.dn) %>% 
  mutate(Month=month(Date)) %>% 
  select(Month, BF.pct.dn, SF.pct.dn) %>% 
  group_by(Month) %>% 
  summarise(b= mean(BF.pct.dn, na.rm=TRUE)/100,
            s= mean(SF.pct.dn, na.rm=TRUE)/100)

##3.4  Oxidation Component ----

stream <- synoptic %>% 
  mutate(Month=lubridate::month(Date)) %>%
  filter(System=="CR",
         ID=="EC.DS") %>% 
  summarize(CH4.13.strm= mean(CH4.13, na.rm=TRUE),
            CH4.13.strm.sd= sd(CH4.13, na.rm=TRUE))


##3.5 bind data and Calculate ----

ch4 <- left_join(hf.monthly, inout, by="Month") %>% 
  left_join(df.base, by="Month") %>% 
  mutate(CH4.subsurf= subsurface$CH4.subsurf,
         CH4.subsurf.err= subsurface$CH4.subsurf.sd,
         CH4.gw=groundwater$CH4.gw,
         CH4.gw.err=groundwater$CH4.gw.sd,
         CH4.13.stm= mean(stream$CH4.13.strm, na.rm = TRUE),
         CH4.13.stm.err= stream$CH4.13.strm.sd,
         CH4.13.gw= mean(groundwater$CH4.13.gw, na.rm = TRUE),
         CH4.13.gw.err= (groundwater$CH4.13.gw.sd))

alpha <- 1.020 # see Tailardat 2020, Thottahil 2018

ch4.calc<-  ch4 %>% 
  mutate(CH4.lat= (Q.gain*s*CH4.subsurf)+(Q.gain*b*CH4.gw), #eq S4
         CH4.lat.err=( ((Q.gain.err/Q.gain)+(CH4.subsurf.err/CH4.subsurf))*(Q.gain.err*CH4.subsurf)*s)+
           ( ((Q.gain.err/Q.gain)+(CH4.gw.err/CH4.gw))*(Q.gain.err*CH4.gw)*b), 
         CH4.input=Q.in*CH4.in, 
         CH4.in.err=Q.in.err*CH4.in,
         CH4.output=Q.out*CH4.out,
         CH4.out.err=Q.out.err*CH4.out,
         CH4.ox= (CH4.13.stm-CH4.13.gw)/ ((alpha-1)*1000 )* (CH4.lat+CH4.input), # eq S3
         CH4.ox.err= ((((CH4.13.gw.err+CH4.13.stm.err)/(abs(CH4.13.gw-CH4.13.stm)) )/ ((alpha-1)*1000 ))+((CH4.in.err+CH4.lat.err)/(CH4.input+CH4.lat)))*CH4.ox, # eq S3 error 
         CH4.atm=CH4.in+CH4.lat-CH4.out-CH4.ox, # eq.1 
         CH4.atm.err=CH4.in.err+CH4.lat.err+CH4.out.err+CH4.ox.err)

ch4.endmembers <- ch4.calc %>% 
  select(Month, CH4.input, CH4.output, CH4.lat, CH4.ox, CH4.atm,
         CH4.in.err, CH4.out.err, CH4.lat.err, CH4.ox.err, CH4.atm.err) %>% 
  rename(CH4.in=CH4.input,
         CH4.out=CH4.output) %>%
  mutate(CH4.out=CH4.out*-1,
         CH4.ox= CH4.ox*-1,
         CH4.atm= CH4.atm*-1)

write_csv(ch4.endmembers, "Education/Clean/PublishedData/CH4Endmembers.csv")

# 3.6 Plot CH4----

ch4em_long <- ch4.endmembers %>%
  select(Month, CH4.in, CH4.lat, CH4.ox, CH4.atm, CH4.out) %>% 
  pivot_longer(cols = c("CH4.in", "CH4.lat", "CH4.ox", "CH4.atm", "CH4.out")) %>%
  rename(Member = name, Cmass = value)
ch4em_long[nrow(ch4em_long)+1,] <- list(1,"CH4.met", 0)

ch4em_long$Member <- factor(
  ch4em_long$Member,
  levels = c("CH4.in", "CH4.lat","CH4.met", "CH4.ox", "CH4.atm", "CH4.out"),
  labels = c(
    "Upstream Import",
    "Lateral Input",
    "Metabolism",
    "Oxidation",
    "Atmospheric Flux",
    "Downstream Export"
  )
)

ch4em_long$Month <- factor(
  ch4em_long$Month,
  levels = c("11", "12", "1", "2", "3","4", "5", "6", "7", "8", "9", "10")
)


mb.fig.ch4 <- 
  ggplot(ch4em_long)+
  geom_col(aes(x = Month, y = Cmass, fill = Member),position = "stack") +
  scale_fill_manual(
    guide = guide_legend(direction = "horizontal", title.position = "left", nrow = 2),
    name = "Member",
    values = c("orange2", "burlywood4", "palegreen4", "orchid3", "midnightblue","darkred")) +
  ylab(expression(g~C-CH[4] ~  d ^ -1)) +
  geom_hline(aes(yintercept = 0), color="black", linewidth=.3)+
  geom_vline(aes(xintercept = 6.5), color="black", linetype="dashed", linewidth=.3)+
  annotate(geom = "text", x = 3.25, y = 1400, label = "wet season")+
  annotate(geom = "text", x = 9.25, y = 1400, label = "dry season")+
  theme_classic()



inputs.perc.fig.ch4 <- 
  ggplot(ch4em_long)+
  geom_col(aes(x = Month, y = Cmass, fill = Member),position = "fill") +
  scale_fill_manual(
    guide = guide_legend(direction = "horizontal", title.position = "left", nrow = 2),
    name = "Member",
    values = c("orange2", "burlywood4","palegreen4", "orchid3", "midnightblue","darkred"))+
  geom_hline(aes(yintercept = 0), color="black", linewidth=.3)+
  geom_vline(aes(xintercept = 6.5), color="black", linetype="dashed", linewidth=.3)+
  ylab("Percent of total C")+
  theme_classic()+
  theme(legend.position="none")

#4. CO2 ----


# Upstream and downstream fluxes

hf.daily <- hf%>% 
  mutate(Q.in=Q.up * 86400,
         Q.in.err=Q.in*.1,
         Q.out=Q.dn *86400,
         Q.out.err=Q.out*.1,
         Q.gain=Q.out-Q.in,
         Q.gain.err=Q.out.err+Q.in.err,
         CO2.in=CO2M.up*10^-6*12.01*Q.in,
         CO2.out=CO2M.dn*10^-6*12.01*-1*Q.out,
         CO2.in.err=CO2.in*.1,,
         CO2.out.err=CO2.out*.1)

# Lateral Inputs

groundwater.co2 <- synoptic %>% 
  filter(System=="PZ") %>% 
  summarize(CO2.gw=mean(CO2, na.rm=TRUE)*12.01,
            CO2.gw.sd=sd(CO2, na.rm=TRUE)*12.01)

subsurface.co2 <- synoptic %>% 
  filter(System=="SS") %>% 
  summarize(CO2.subsurf=mean(CO2, na.rm=TRUE)*12.01,
            CO2.subsurf.sd=sd(CO2, na.rm=TRUE)*12.01)

df.base.co2 <- left_join(hf,baseflow.tmp, by="Date") %>% 
  mutate(BF.pct.up= (Baseflow.up/Q.up)*100,
         BF.pct.dn= (Baseflow.dn/Q.dn)*100,
         SF.pct.up=100-BF.pct.up,
         SF.pct.dn=100-BF.pct.dn) %>% 
  select(Date, 
         Q.up, Q.dn, 
         BF.pct.up, BF.pct.dn,
         SF.pct.up, SF.pct.dn) %>% 
  select(Date, BF.pct.dn, SF.pct.dn) %>% 
  group_by(Date) %>% 
  summarise(b= mean(BF.pct.dn, na.rm=TRUE)/100,
            s= mean(SF.pct.dn, na.rm=TRUE)/100)

# Metabolism
# scale from g C m-2 d-1 to g C d-1
met <- metabolism %>% 
  mutate(met=NEP.daily.C*3022.5,
         met.err=NEP.daily.sd.C*3022.5) %>%
  rename(Date=date) %>% 
  select(Date, met, met.err)

# oxidation 
ox <- ch4.endmembers %>% 
  select(Month, CH4.ox, CH4.ox.err)


# Combine and calculate end members ----

co2 <- left_join(hf.daily, met, by="Date") %>% 
  left_join(ox, by="Month") %>% 
  left_join(df.base.co2, by="Date") %>% 
  mutate(co2.gw=groundwater.co2$CO2.gw,
         co2.gw.err=groundwater.co2$CO2.gw.sd,
         co2.subsurf=subsurface.co2$CO2.subsurf,
         co2.subsurf.err=subsurface.co2$CO2.subsurf.sd,
         co2.lat= (Q.gain*s*co2.subsurf)+(Q.gain*b*co2.gw), #eq S4
         co2.lat.err=( ((Q.gain.err/Q.gain)+(co2.subsurf.err/co2.subsurf))*(Q.gain.err*co2.subsurf)*s)+
           ( ((Q.gain.err/Q.gain)+(co2.gw.err/co2.gw))*(Q.gain.err*co2.gw)*b),
         co2.flux=(CO2.in+co2.lat+met+CO2.out+CH4.ox)*-1,
         co2.flux.err=(CO2.in.err+CO2.out.err+co2.lat.err+met.err+CH4.ox.err)) %>% 
  select(Date, CO2.in, CO2.out, co2.lat, met, CH4.ox, co2.flux,
         CO2.in.err, CO2.out.err, co2.lat.err, met.err, CH4.ox.err, co2.flux.err) %>% 
  rename(CO2.lat=co2.lat,
         CO2.met=met,
         CO2.ox=CH4.ox,
         CO2.atm=co2.flux,
         
         CO2.lat.err=co2.lat.err,
         CO2.met.err=met.err,
         CO2.ox.err=CH4.ox.err,
         CO2.atm.err=co2.flux.err)
  
write_csv(co2, "Education/Clean/PublishedData/CO2endmembers.csv") #g c-co2 d-1

##Plot CO2 ----

## 4.1 Raw Mass balance figure ----
co2_long <- co2 %>%
  pivot_longer(cols = c("CO2.in", "CO2.lat", "CO2.met", "CO2.out", "CO2.atm", "CO2.ox")) %>%
  rename(Member = name, Cmass = value)
#df_long[nrow(df_long)+1,36:37] <- list("C.ox", 10)
#df_long[nrow(df_long),1] <-  lubridate::ymd("2024-02-01")

co2_long$Member <- factor(
  co2_long$Member,
  levels = c("CO2.in", "CO2.lat", "CO2.met", "CO2.ox", "CO2.atm", "CO2.out", "CO2.ox"),
  labels = c(
    "Upstream Import",
    "Lateral Input",
    "Metabolism",
    "Methane Oxidation",
    "Atmospheric Flux",
    "Downstream Export",
    "Methane Oxidation"
  )
)



co2.mb.fig <- 
  ggplot(co2_long)+
  geom_area(aes(x = Date, y = Cmass*10^-3, fill = Member),position = "stack") +
  scale_fill_manual(
    guide = guide_legend(direction = "horizontal", title.position = "left", nrow = 2),
    name = "Member",
    values = c("orange2", "burlywood4","palegreen4", "orchid3", "midnightblue","darkred")) +
  ylab(expression(kg~C-CO[2] ~  d ^ -1)) +
  geom_hline(aes(yintercept = 0), color="black", linewidth=.3)+
  geom_vline(aes(xintercept = lubridate::ymd("2024-05-01")), color="black", linetype="dashed", linewidth=.3)+
  annotate(geom = "text", x = lubridate::ymd("2024-02-01"), y = 220, label = "wet season")+
  annotate(geom = "text", x = lubridate::ymd("2024-08-01"), y = 220, label = "dry season")+
  annotate( geom = "segment", 
            x = ymd("2023-11-01"),  xend = ymd("2023-11-01"),
            y = 0, yend = 150, 
            arrow = arrow(type = "open", angle = 15, length=unit(4, "mm"))) +
  annotate( geom = "segment", 
            x = ymd("2023-11-01"),  xend = ymd("2023-11-01"),
            y = 0, yend = -150, 
            arrow = arrow(type = "open", angle = 15, length=unit(4, "mm"))) +
  annotate(geom = "text", x = ymd("2023-11-01"), y = 75, label = "inputs",  angle=90, vjust=-.5)+
  annotate(geom = "text", x = ymd("2023-11-01"), y = -75, label = "outputs",  angle=90, vjust=-.5)+
  theme_classic()+
  theme(legend.position = "none")



co2.inputs.perc.fig <- 
  ggplot(co2_long)+
  geom_area(aes(x = Date, y = Cmass*10^-3, fill = Member),position = "fill") +
  scale_fill_manual(
    guide = guide_legend(direction = "horizontal", title.position = "left", nrow = 2),
    name = "Member",
    values = c("orange2", "burlywood4","palegreen4",  "orchid3","midnightblue", "darkred" ))+
  geom_hline(aes(yintercept = 0), color="black", linewidth=.3)+
  geom_vline(aes(xintercept = lubridate::ymd("2024-05-01")), color="black", linetype="dashed", linewidth=.3)+
  #  annotate(geom = "text", x = lubridate::ymd("2024-02-01"), y = .95, label = "wet season")+
  #  annotate(geom = "text", x = lubridate::ymd("2024-08-01"), y = .95, label = "dry season")+
  annotate( geom = "segment", 
            x = ymd("2023-11-01"),  xend = ymd("2023-11-01"),
            y = 0, yend = .75, 
            arrow = arrow(type = "open", angle = 15, length=unit(4, "mm"))) +
  annotate( geom = "segment", 
            x = ymd("2023-11-01"),  xend = ymd("2023-11-01"),
            y = 0, yend = -.75, 
            arrow = arrow(type = "open", angle = 15, length=unit(4, "mm"))) +
  annotate(geom = "text", x = ymd("2023-11-01"), y = .37, label = "inputs",  angle=90, vjust=-.5)+
  annotate(geom = "text", x = ymd("2023-11-01"), y = -.37, label = "outputs",  angle=90, vjust=-.5)+
  ylab("Percent of total C")+
  theme_classic()+
  theme(legend.position = "none")

#Combine figures

layout <-
  "AB
AB
AB
CD
CD
CD
EE"

combinedmb <- 
  co2.mb.fig+mb.fig.ch4+
  co2.inputs.perc.fig+inputs.perc.fig.ch4+
  guide_area()+
  plot_layout(guides = "collect", design = layout)+
  plot_annotation(tag_levels = "a")

combinedmb

ggsave("Education/Figures/PublishedFigures/Figure3.png",
       plot=combinedmb,
       height=8, width=11, units="in", dpi=300)

