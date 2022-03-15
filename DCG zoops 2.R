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
#2. Adjust Suisun biomass with salinity based on Sam's models.
#3. Adjust Yolo biomass based on NDFS conceptual model
#4. Adjust based on habitat suitability from Eli's data.

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
Zoops2 = mutate(Zoops,  Yearadj = wtr_yr(Date)) %>%
  filter(Year > 2000)

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

ZoopsSusBN = filter(Zoop_mesoTot, Yr_type == "Below Normal", Month %in% c(6,7,8,9,10), 
                    SUBREGION == "Suisun Marsh")

ZoopSusBNave = group_by(ZoopsSusBN,  SUBREGION, Station,
                        Yr_type, Month, IBMR) %>%
  dplyr::summarise(BPUEm = mean(BPUEt, na.rm = T)) %>%
  filter(!is.na(SUBREGION))

save(ZoopsSusBN, ZoopSusBNave, file = "SuisunAveZoops_BN.RData")

ggplot(Zoop_mesoSta, aes(x = Yr_type, y = BPUEm)) + geom_boxplot()+
  facet_grid(SUBREGION~IBMR)

ggplot(Zoop_mesoSta, aes(x = Yr_type, y = BPUEm)) + geom_bar()+
  facet_grid(SUBREGION~IBMR)


ggplot(filter(Zoop_mesoSta, Yr_type == "Dry"), aes(x = SUBREGION, y = BPUEm)) + geom_col()+
  facet_wrap(~IBMR)
ggplot(filter(Zoop_mesoSta, Yr_type == "Dry"), aes(x = IBMR, y = BPUEm)) + geom_col()+
  facet_wrap(~SUBREGION)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(filter(Zoop_mesoSta, Yr_type == "Below Normal"), aes(x = IBMR, y = BPUEm)) + geom_col()+
  facet_wrap(~SUBREGION)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################################################3
#Step 2 - correct for salinity (insert Sam's analysis here)\

#upload tidally filtered salinity
# SCHISM Model Output at Beldons Landing,
# 2022 Summer-fall alternatives
# Tidally filtered with a squared cosine-Lanczos filter with 40-hour cutoff
# Units: psu
# Terms:
# bn: below normal year classification
# dry: dry year classification
# bdl: Beldons Landing
# smscg4: 4psu trigger at BDL
# smscg6: 6psu trigger at BDL

salinities = read_csv("bdl_summer_fall_2022_psu_filtered.csv")
salinitiesLong = pivot_longer(salinities, cols = starts_with("bdl"), names_to = "Scenario",
                              values_to = "Salinity")

ggplot(salinitiesLong, aes(x = datetime, y = Salinity, color = Scenario))+
  geom_line()

salinities2 = mutate(salinitiesLong, Month = month(datetime)) %>%
  group_by(Scenario, Month) %>%
  summarize(Salinity = mean(Salinity, na.rm = T))
###################################################################3
#Step 3 - Adjust based on NDFS

NDFS = read_excel("NDFS_zoop_adjustments.xlsx")
NDFSlong = pivot_longer(NDFS, cols = c(Jul, Aug, Sep, Oct), names_to = "Month",
                        values_to = "Zoop_adj") %>%
  mutate(Zoop_adj = case_when(Zoop_adj == 1 ~1,
                              TRUE ~ 1+Zoop_adj/100),
         Month = as.character(factor(Month, levels = c("Jul", "Aug", "Sep", "Oct"), 
                                     labels = c(7,8,9,10))),
         Region = as.character(factor(Region, levels = c("Lower Sacramento River",
                                            "Sacramento River","Yolo Bypass"),
                         labels = c("LowSac", "Sac", "Yolo"))))
                         
         


###############################################################
#Step 3.5 IBMR inputs
PD.array <- readRDS("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/DCG/DCGmodeling/PD.array.rds")
#PD.array is a 4-D array, with dimensions of [year, month, strata, prey type]. Year 1 is calendar year 1995. 
#Strata : Yolo, Sac, SDelta, EDelta, LowSac, LowSJ,  Conf, SESuisun, NESuisun, Marsh, SWSuisun, NWSuisun
#Prey types: limno, othcaljuv, pdiapjuv, othcalad, acartela, othclad, allcopnaup, daphnia, othcyc, other, eurytem, pdiapfor
attributes(PD.array)
test= list(Year = 1995:2015, Month = 1:12, 
                           Region = c("Yolo", "Sac", "SDelta", "EDelta", "LowSac", 
                                      "LowSJ",  "Conf", "SESuisun", "NESuisun", "Marsh", "SWSuisun", "NWSuisun"),
                          Prey = c("limno", "othcaljuv", "pdiapjuv", "othcalad", "acartela", "othclad",
                                   "allcopnaup", "daphnia", "othcyc", "other", "eurytem", "pdiapfor"
                                     ))
dimnames(PD.array) = test

#select 6-October of dry years (2001, 2002, 2007, 2009, 2013)
PD.array_dry = PD.array[c(7,8,15,15,19), c(6:10), ,]

#select 6-October of below normal years (2001, 2002, 2007, 2009, 2013)
PD.array_bn = PD.array[c(10,16,18), c(6:10), ,]


#calculate the average by month per year type
PD.array_BNtest = apply(PD.array_bn, c(2,3,4), mean)

#I can't deal with arrays so here it is as a data frame
test2 = as_tibble(PD.array_BNtest, rownames = "Month")  %>%
  pivot_longer(cols = !Month, names_to = "RegionPrey", values_to = "BPUE") %>%
  group_by(RegionPrey)%>%
  mutate(Region = str_split_fixed(RegionPrey, "[.]", n = 2)[1,1], 
         Prey = str_split_fixed(RegionPrey, "[.]", n = 2)[1,2]) %>%
  ungroup

#atach NDFS zooplankton adjustments and calculate
test3 = left_join(test2, filter(NDFSlong, Yr_type == "Below Normal")) %>%
  mutate(BPUE2 = case_when(is.na(Zoop_adj) ~ BPUE,
                           TRUE ~ Zoop_adj * BPUE))


#cNowdo it for the dry year
PD.array_drytest = apply(PD.array_dry, c(2,3,4), mean)

#I can't deal with arrays so here it is as a data frame
drytest2 = as_tibble(PD.array_drytest, rownames = "Month")  %>%
  pivot_longer(cols = !Month, names_to = "RegionPrey", values_to = "BPUE") %>%
  group_by(RegionPrey)%>%
  mutate(Region = str_split_fixed(RegionPrey, "[.]", n = 2)[1,1], 
         Prey = str_split_fixed(RegionPrey, "[.]", n = 2)[1,2]) %>%
  ungroup

#atach NDFS zooplankton adjustments and calculate
drytest3 = left_join(test2, filter(NDFSlong, Yr_type == "Dry")) %>%
  mutate(BPUE2 = case_when(is.na(Zoop_adj) ~ BPUE,
                           TRUE ~ Zoop_adj * BPUE))

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
