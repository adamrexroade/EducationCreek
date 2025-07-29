#Figure S6 Mass Balance Uncertainty



# Author: Adam Rexroade
# Project: Education Creek Source and Sink
# Date Created: July 11, 2025
# Date Last Modified: July 29, 2025

# Description: Plot Uncertainties of each end member of the CO2 and CH4 Mass Balance


#1 Load Data and Libraries ----
library(tidyverse)
library(scales)
library(patchwork)
ch4 <- read_csv("Education/Clean/PublishedData/CH4endmembers.csv")
up <- read_csv("Education/Clean/PublishedData/Timeseries.csv") %>%  
  select(Date, MaxedOutTRUE)  %>% 
  group_by(Date) %>% 
  mutate(MaxedOutTRUE = any(MaxedOutTRUE == "TRUE")) %>% 
  distinct(Date, MaxedOutTRUE) 

#2 Format For plotting ----
## 2.1 CH4 ----
ch4_flux_long <- ch4 %>% 
  pivot_longer(cols=c(2:6),
               names_to="EndMember",
               values_to="Flux") %>% 
  select(Month, EndMember, Flux)

ch4_err_long <- ch4 %>% 
  pivot_longer(cols=c(7:11),
               names_to="EndMember",
               values_to="Error") %>% 
  select(Month, EndMember, Error)

ch4_err_long$EndMember = substr(ch4_err_long$EndMember,1,nchar(ch4_err_long$EndMember)-4)

ch4_long <- ch4_flux_long %>% 
  left_join(ch4_err_long, by=c("Month", "EndMember")) 

ch4_long$EndMember <- factor(
  ch4_long$EndMember,
  levels = c("CH4.in", "CH4.lat", "CH4.ox", "CH4.atm", "CH4.out"),
  labels = c(
    "Upstream Import",
    "Lateral Input",
    "Oxidation",
    "Atmospheric Flux",
    "Downstream Export"
  )
)


ch4_long$Month <- factor(
  ch4_long$Month,
  levels = c("11", "12", "1", "2", "3","4", "5", "6", "7", "8", "9", "10")
)

## 2.2 CO2 ----


upstat <- up %>% filter(Date >=ymd("2023-11-01") & Date <= ymd("2024-10-31")) %>% 
  count(MaxedOutTRUE) 


co2 <- read_csv("Education/Clean/PublishedData/CO2endmembers.csv") %>% 
  select(Date, CO2.in, CO2.lat, CO2.met, CO2.ox, CO2.atm, CO2.out, 
         CO2.in.err, CO2.lat.err, CO2.met.err, CO2.ox.err, CO2.atm.err, CO2.out.err)  
  # rename(C.met.err=met.err, C.ox.err=CO2.ox.err) %>%
  # mutate(met.err=NULL,
         # co2.ox.err=NULL)

co2_flux_long <- co2 %>%
  pivot_longer(cols=c(2:7),
               names_to="EndMember",
               values_to="Flux") %>% 
  select(Date, EndMember, Flux)

co2_err_long <- co2 %>%
  pivot_longer(cols=c(8:13),
               names_to="EndMember",
               values_to="Error") %>% 
  select(Date, EndMember, Error)

co2_err_long$EndMember = substr(co2_err_long$EndMember,1,nchar(co2_err_long$EndMember)-4)


co2_long <- co2_flux_long %>% 
  left_join(co2_err_long, by=c("Date", "EndMember")) %>% 
  filter(Date >=ymd("2023-11-01") & Date <= ymd("2024-10-31")) %>% 
  mutate(Flux=Flux*10^-3,
         Error=Error*10^-3)

co2_long$EndMember <- factor(
  co2_long$EndMember,
  levels = c("CO2.in", "CO2.lat", "CO2.met", "CO2.ox", "CO2.atm", "CO2.out"),
  labels = c(
    "Upstream Import",
    "Lateral Input",
    "Metabolism",
    "Oxidation",
    "Atmospheric Flux",
    "Downstream Export"
  )
)

#3 Plot CH4 Data ----

#3.1 CH4 Upstream ----

ch4_up<- ggplot(ch4_long %>% filter(EndMember=="Upstream Import"), aes(x=Month, y=Flux)) +
  geom_bar(stat="identity", position="dodge", fill="orange2") +
  geom_errorbar(aes(x=Month, ymin=Flux-Error, ymax=Flux+Error), 
                width=.5)+
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(g~C-CH[4] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
    strip.text=element_text(color="white"))

#3.2 CH4 Lateral ----

ch4_lat<- ggplot(ch4_long %>% filter(EndMember=="Lateral Input"), aes(x=Month, y=Flux)) +
  geom_bar(stat="identity", position="dodge", fill="burlywood4") +
  geom_errorbar(aes(x=Month, ymin=Flux-Error, ymax=Flux+Error), 
                width=.5)+
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(g~C-CH[4] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))
    
#3.3 CH4 Downstream ----

ch4_down<- ggplot(ch4_long %>% filter(EndMember=="Downstream Export"), aes(x=Month, y=Flux)) +
  geom_bar(stat="identity", position="dodge", fill="darkred") +
  geom_errorbar(aes(x=Month, ymin=Flux-Error, ymax=Flux+Error), 
                width=.5)+
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(g~C-CH[4] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))

#3.4 CH4 Emission ----

ch4_atm<- ggplot(ch4_long %>% filter(EndMember=="Atmospheric Flux"), aes(x=Month, y=Flux)) +
  geom_bar(stat="identity", position="dodge", fill="midnightblue") +
  geom_errorbar(aes(x=Month, ymin=Flux-Error, ymax=Flux+Error), 
                width=.5)+
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(g~C-CH[4] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))

#3.5 CH4 Oxidation ----

ch4_oxidation<- ggplot(ch4_long %>% filter(EndMember=="Oxidation"), aes(x=Month, y=Flux)) +
  geom_bar(stat="identity", position="dodge", fill="orchid3") +
  geom_errorbar(aes(x=Month, ymin=Flux-Error, ymax=Flux+Error), 
                width=.5)+
  facet_wrap(~ EndMember, scales="free_y",) +
  xlab("Month") + ylab(expression(g~C-CH[4] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))


# 3.6 CO2 Upstream ----

co2_updata <- co2_long %>% 
  filter(EndMember=="Upstream Import") %>% 
  left_join(up, by="Date") %>%
  mutate(
         Flux1=Flux) %>% 
  pivot_wider(
    names_from = MaxedOutTRUE,
    values_from = Flux1,
    values_fill = NA
  ) 


co2_up <- ggplot(co2_updata, aes(x=Date)) +
  geom_ribbon(aes(x=Date, ymin=Flux-Error, ymax=Flux+Error),
                fill="orange2", alpha=.4)+
  geom_line(aes(y=`FALSE`), color="orange2",  linewidth=.5) +
  geom_line(aes(y=`TRUE`), color="orange2",  linetype="dotted", linewidth=.5) +
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(kg~C-CO[2] ~  d ^ -1))+
  theme_classic()+
  theme( legend.position= "none",
         strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))

# 3.7 CO2 Lateral ----

co2_lat <- ggplot(co2_long %>% filter(EndMember=="Lateral Input"), aes(x=Date, y=Flux)) +
  geom_ribbon(aes(x=Date, ymin=Flux-Error, ymax=Flux+Error), 
                fill="burlywood4", alpha=.4)+
  geom_line(color="burlywood4", linewidth=.5) +
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(kg~C-CO[2] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))

# 3.8 CO2 Metabolism ----
co2_met <- ggplot(co2_long %>% filter(EndMember=="Metabolism"), aes(x=Date, y=Flux)) +
  geom_ribbon(aes(x=Date, ymin=Flux-Error, ymax=Flux+Error), 
                fill="darkgreen", alpha=.4)+
  geom_line(color="darkgreen", linewidth=.5) +
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(kg~C-CO[2] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))

#3.9 CO2 Oxidation ----
co2_ox <- ggplot(co2_long %>% filter(EndMember=="Oxidation"), aes(x=Date, y=Flux)) +
  geom_ribbon(aes(x=Date, ymin=Flux-Error, ymax=Flux+Error), 
                fill="orchid3", alpha=.4)+
  geom_line(color="orchid3", linewidth=.5) +
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(kg~C-CO[2] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))

##3.10 Co2 downstream ----
co2_down <- ggplot(co2_long %>% filter(EndMember=="Downstream Export"), aes(x=Date, y=Flux)) +
  geom_ribbon(aes(x=Date, ymin=Flux-Error, ymax=Flux+Error), 
                fill="darkred", alpha=.4)+
  geom_line(color="darkred", linewidth=.5) +
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(kg~C-CO[2] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))


##3.11 co2 atmospheric flux ----
co2_atm <- ggplot(co2_long %>% filter(EndMember=="Atmospheric Flux"), aes(x=Date, y=Flux)) +
  geom_ribbon(aes(x=Date, ymin=Flux-Error, ymax=Flux+Error), 
                fill="midnightblue", alpha=.4)+
  geom_line(color="midnightblue", linewidth=.5) +
  facet_wrap(~ EndMember, scales="free_y",)+
  xlab("Month") + ylab(expression(kg~C-CO[2] ~  d ^ -1))+
  theme_classic()+
  theme( strip.background = element_rect(fill="black"), 
         strip.text=element_text(color="white"))

#4. Patchwork Figures ----

combined <- 
  co2_up + ch4_up+
  co2_lat + ch4_lat +
  co2_met + plot_spacer()+
  co2_ox+ch4_oxidation +
  co2_down + ch4_down +
  co2_atm+ ch4_atm +
  plot_layout(ncol=2, nrow=6, axis_titles = "collect")

ggsave(combined, 
         filename="Education/Figures/ManuscriptFigures/SI/S6.ModelUncertainty.png",
         width=7, height=11,units = "in", dpi=300)

