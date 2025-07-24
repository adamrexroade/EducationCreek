# Calculate Co2 at saturation for Figure 2 panel e


## 1.1 Packages and load functions ----


co2sat <- function(){
 
  
  source("Education/Code/F_LoadCleanData.R")
  
  
  Data.start <- lubridate::ymd("2023-11-01")
  Data.end <- lubridate::ymd("2024-10-31")
  df <- loadEC() %>% 
    filter(Data.start<Date & Date <Data.end)
  
  ## calcuate input parameters
  
  temp <- mean(df$Temp.dn,na.rm=TRUE) #C
  pres <- mean(df$Pressure.up, na.rm=TRUE) /10  #mBar, /10 converts to kpa
  
  #3. Calculate Csat 
  #based on NEON DissGas package 
  # Assume that atmospheric co2 concentration is 420 ppm and pressure is constant at 1atm 
  
  ##3.1 Load Constants ----
  cGas<-8.3144598 #universal gas constant (J K-1 mol-1)
  cKelvin <- 273.15 #Conversion factor from Kelvin to Celsius
  cPresConv <- 0.000001 # Constant to convert mixing ratio from umol/mol (ppmv) to mol/mol. Unit conversions from kPa to Pa, m^3 to L, cancel out.
  cT0 <- 298.15#Henry's law constant T0
  cConcPerc <- 100 #Convert to percent
  #Henry's law constants and temperature dependence from Sander (2015) DOI: 10.5194/acp-15-4399-2015
  ckHCO2 <- 0.00033 #mol m-3 Pa, range: 0.00031 - 0.00045
  cdHdTCO2 <- 2400 #K, range: 2300 - 2600
  
  
  
  ##3.2 Calculate Csat ----
  
  co2sat=  (ckHCO2 * exp(cdHdTCO2*(1/(temp + cKelvin) - 1/cT0))) * 420 * pres * cPresConv 
  
  return(co2sat*10^6) # convert M to uM
}