#DCG zooops 2. Puttin git all together


require(tidyverse)
require(zooper)
require(lubridate)
require(ggplot2)
require(sf)
require(readxl)




#So, Will is going to need % change from baseline for the whole community by region.

#Steps in the process:
#1. Calculate average by month, region, and water year type for each zoop
#2. Adjust Suisun biomasse with salinity based on Sam's models.
#3. Adjust Yolo biomass based on NDFS conceptual model
#4. Adjust based on habitat suitablity from Eli's data.

#Use output of step 3 for IBMR.
#Use output of step 4 for zooplankton metric. 


#1. Step 1: averages by month, region, and water year type

#load zooplankton biomas data that Sam put together

Zoops = read_csv("Zoop_data_mass_Rosie.csv")
str(Zoops)

#Calculate water year function
wtr_yr <- function(dates, start_month=12) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon == start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}


#add water year and water year type
Zoops2 = mutate(Zoops,  Yearadj = wtr_yr(Date))

WYs = read_csv("yeartypes.csv")
WYs =  dplyr::rename(WYs, Yearadj = Year)
Zoops3 = left_join(Zoops2, WYs) %>%
  ungroup()
head(Zoops3)

#Total biomass by sample and IBMR group
Zoop_mesoTot = group_by(Zoops3, SampleID, Station, Date, Year, SUBREGION, doy,
                        Yr_type,  Yearadj, IBMR) %>%
  dplyr::summarise(BPUEt = sum(BPUE, na.rm = T)) %>%
  mutate(Month = month(Date))

#Average zoop biomass by station, month, and water year type
#remove stations in San Pablo (no subregion)
Zoop_mesoSta = group_by(Zoop_mesoTot,  SUBREGION, Station,
                        Yr_type, Month, IBMR) %>%
  dplyr::summarise(BPUEm = mean(BPUEt, na.rm = T)) %>%
  filter(!is.na(SUBREGION))

ggplot(Zoop_mesoSta, aes(x = Yr_type, y = BPUEm)) + geom_boxplot()+
  facet_grid(SUBREGION~IBMR)

###################################################################3
#Step 2 - correct for salinity (insert Sam's analysis here)\

###################################################################3
#Step 3 - Adjust based on NDFS

##########################################
#Step 4 - Adjust based on habitat index. 


#load habitat by station
load("HabitatbyStations.RData")

names(IEP_sf_HSI)
IEP_sf_HSI = dplyr::rename(IEP_sf_HSI, Station = StationCode, Source = Survey)
IEP_HSI_long = st_drop_geometry(IEP_sf_HSI) %>%
  pivot_longer(cols =  starts_with("mean"),
               names_to = "Scenario", values_to = "HSI")

#import scenario descriptions
scenarios = read_csv("ScenariosRMA.csv")
IEP_HSI_long = left_join(IEP_HSI_long, scenarios)

IEP_wzoop = left_join(Zoop_mesoSta, IEP_HSI_long)


#To look at what the zooplankton biomass in the "good" habitat is, 
#I first tried cutting off the "good" habitat at an HSI above 0.4

IEP_zoop_goodHSI = filter(IEP_wzoop, HSI > 0.4) %>%
  group_by(Scenario, IBMR, Yr_type, Month, Action, SUBREGION) %>%
  dplyr::summarize(BPUE = mean(BPUEm, na.rm = T))

ggplot(filter(IEP_zoop_goodHSI, Yr_type == "Below Normal"), aes(x = IBMR, y = BPUE, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~SUBREGION)

ggplot(IEP_zoop_goodHSI, aes(x = IBMR, y = BPUE, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~Yr_type)

#Hey, that's not bad!

#That didn't show a lot of difference bewteen scenarios. 
#Next I will try weighting the BPUE by the HSI, rather than using a cut-off. 

IEP_zoop_goodHSI2 =mutate(IEP_wzoop, foodHSI = BPUEm*HSI) %>%
  filter(!is.na(HSI)) %>%
  group_by(Scenario, IBMR, Yr_type, Month, Action, SUBREGION) %>%
  dplyr::summarize(BPUEweighted = mean(foodHSI, na.rm = T))

ggplot(IEP_zoop_goodHSI2, aes(x = IBMR, y = BPUEweighted, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~Yr_type)

ggplot(filter(IEP_zoop_goodHSI2, Yr_type == "Below Normal"), aes(x = IBMR, y = BPUEweighted, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~SUBREGION)

#Nice. Even better. 
#I should probalby use just the regional average with the salinity adjustment for the IBMR inputs.

#use the habitat weighting just for the overall zoop score. 

#Also need to add weighting for NDFS. 
