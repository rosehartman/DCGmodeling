#Zooplankton to put into the IBMR

#The IBMR doens't include mysids, but we probably want to use them anyway.

#load packages
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(plotrix)
library(purrr)
library(cowplot)
library(plyr)
library(readxl)
library(deltamapr)
library(spacetools)
library(sf)
library(exactextractr)
library(lubridate)

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


#Get biomass lookup tables in
biomass_meso<-read_csv("Zooplankton/Data/biomass_mesomicro.csv")
biomass_meso$Taxlifestage<-paste(biomass_meso$Taxname,biomass_meso$Lifestage,sep=" ")
biomass_meso<-dplyr::select(biomass_meso,Taxlifestage,Carbon_mass_micrograms)

biomass_macro<-read_excel("Zooplankton/Biomass conversions_CEB Updated.xlsx", 
                          sheet = "Macro-zooplankton")


biomass_macro =  dplyr::filter(biomass_macro, Weight_type == "Dry", Preservative == "Formalin") %>%
  dplyr::select(Taxname,a,b)

#load the zooplankton meso and micro count data (created in the zooper_cleaning script)
Zoop_data<-readRDS("Zooplankton/Data/Zooper_data.rds")

#set taxa of interest list
target_FLOAT_taxa<-c("Pseudodiaptomus Juvenile",
                     "Pseudodiaptomus forbesi Adult",
                     "Bosmina longirostris Adult",
                     "Limnoithona tetraspina Adult",
                     "Daphnia Adult",
                     "Eurytemora affinis Adult",
                     "Eurytemora affinis Juvenile",
                     "Hyperacanthomysis longirostris Adult",
                     "Neomysis kadiakensis Adult",
                     "Neomysis mercedis Adult")

#I think we need a lookup table for the IBMR taxa groups.
#i downloaded the zooper crosswalk and added a column
#remove "_UnID" from taxa names
Zoop_data$Taxlifestage<-Zoop_data$Taxlifestage%>%str_replace("_UnID","")
Zoop_data$Taxlifestage<-ifelse(Zoop_data$Taxlifestage=="Limnoithona Adult","Limnoithona tetraspina Adult",Zoop_data$Taxlifestage)
  



IBMRtaxa = read_csv("Zooplankton/zoopcrosswalk2.csv") %>%
  dplyr::select(Taxlifestage, IBMR) %>%
  distinct()
Zoop_datatest = left_join(Zoop_data, IBMRtaxa) %>%
dplyr::filter(!Undersampled) %>%
  mutate(Month = month(Date))
#########################################################
#bleh. Let's just use what artur already synthesized

zoopdata = read_csv("Zooplankton/Data/Drought_taxa_BPUEmatrix.csv")
#UGH just annual

zoopdata2 = read_csv("Zooplankton/Data/zoop_drought_lt_bpue_reg.csv")
#nope
##############################################################
#OK, I'll just leave out mysids for now.


#Meso-biomass calculations
#i'm going to just use post-2000 data)
t_size<-c("Meso","Micro") #set target sizes for "meso" (really meso+micro)
Zoop_meso<-Zoop_datatest%>%filter(SizeClass%in%t_size,Date>"1999-12-31")

test = filter(Zoop_meso2, Month == 8)

#change daphnia names so it will link with biomass
Zoop_meso$Taxlifestage[Zoop_meso$Taxlifestage=="Daphnia_UnID Adult"]<-"Daphnia Adult"
Zoop_meso<-Zoop_meso%>%left_join(biomass_meso)
Zoop_meso$BPUE<-Zoop_meso$CPUE*Zoop_meso$Carbon_mass_micrograms

#attach region assignments

regions = read_csv("Zooplankton/AllIEP_wRegions.csv")
regions =   dplyr::rename(regions, Source = Survey, Station = StationCode)

#add water year and water year type. And season.

Zoop_meso2 = mutate(Zoop_meso, Month = month(Date), Yearadj = wtr_yr(Date)) %>%
  left_join(regions) 
WYs = read_csv("yeartypes.csv")
WYs =  dplyr::rename(WYs, Yearadj = Year)
Zoop_meso = left_join(Zoop_meso2, WYs) %>%
  ungroup()

ggplot(Zoop_meso, aes(x = SalSurf, y = BPUE, color = IBMR)) + geom_point()+
  facet_grid(Yr_type~ Region)

ggplot(Zoop_meso, aes(x = Region, y = BPUE, color = IBMR)) + geom_boxplot()+
  facet_grid(Yr_type~ .)

#######################################################################
#Total BPUE per group by sample. Then average BPUE per region and month by water  year

Zoop_mesoTot = group_by(Zoop_meso, SampleID, Volume, Source, SizeClass, Region, 
                        Yr_type, Latitude, Longitude, Station, Year, Month, Yearadj, IBMR) %>%
  dplyr::summarise(BPUEt = sum(BPUE, na.rm = T))

Zoop_mesoMean = group_by(Zoop_mesoTot,  Region, 
                         Yr_type, Year, Month, Yearadj, IBMR) %>%
  dplyr::summarise(BPUEm = mean(BPUEt, na.rm = T))


ggplot(Zoop_mesoMean, aes(x = IBMR, y = BPUEm, color = IBMR)) + geom_boxplot()+
  facet_grid(Yr_type~ Region, scales = "free_y")

############################################################################
#How about averge monthly biomass per station per year type? THen overlay good habitat
#to find average biomass in good habitat?

#Two options:
# 1. Create a plygon of area with smelt habitat above a threshold for each action scenario. 
#    Calculte zoop biomass within that polygon (for a given wate ryear type).
# 2. Calculate average smelt habitat index within each area. Calculate average zoop biomass
#    within each region. Repeat for each scenario. 

#load habitat by station
load("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/SMSCG/HabitatbyStations.RData")

#Average zoop biomass by station, month, and water year type
Zoop_mesoSta = group_by(Zoop_mesoTot,  Region, Source, Station,
                         Yr_type, Month, IBMR) %>%
  dplyr::summarise(BPUEm = mean(BPUEt, na.rm = T))
test = filter(Zoop_mesoSta, Month == 8)


names(IEP_sf_HSI)
IEP_sf_HSI = dplyr::rename(IEP_sf_HSI, Station = StationCode, Source = Survey)
IEP_HSI_long = st_drop_geometry(IEP_sf_HSI) %>%
  pivot_longer(cols =  starts_with("mean"),
                            names_to = "Scenario", values_to = "HSI")

#import scenario descriptions
scenarios = read_csv("ScenariosRMA.csv")
IEP_HSI_long = left_join(IEP_HSI_long, scenarios)

IEP_wzoop = left_join(dplyr::select(ungroup(Zoop_mesoSta),-Region), IEP_HSI_long)  

IEP_zoop_goodHSI = filter(IEP_wzoop, HSI > 0.2) %>%
  group_by(Scenario, IBMR, Yr_type, Month, Action) %>%
  dplyr::summarize(BPUE = mean(BPUEm, na.rm = T))

ggplot(IEP_zoop_goodHSI, aes(x = IBMR, y = BPUE, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~Yr_type)

#Damn it. I don't know that this is actually going to work.

#Maybe I should weight the biomass by habitat suitability

IEP_zoop_goodHSI2 =mutate(IEP_wzoop, foodHSI = BPUEm*HSI) %>%
  filter(!is.na(HSI)) %>%
  group_by(Scenario, IBMR, Yr_type, Month, Action) %>%
  dplyr::summarize(BPUEweighted = mean(foodHSI, na.rm = T))

ggplot(IEP_zoop_goodHSI2, aes(x = IBMR, y = BPUEweighted, fill = Action)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~Yr_type)

IEP_zoop_goodHSI3 = IEP_zoop_goodHSI2 %>%
  pivot_wider(id_cols = c(IBMR, Yr_type, Month), names_from = Action, values_from = BPUEweighted) %>%
  mutate(BPUEdiff = SMSCG - NoAction, BPUEperdiff = 1-(NoAction/SMSCG))


ggplot(IEP_zoop_goodHSI3, aes(x = IBMR, y = BPUEdiff)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~Yr_type)

ggplot(IEP_zoop_goodHSI3, aes(x = IBMR, y = BPUEperdiff)) + geom_col(position = "dodge")+
  facet_grid(as.factor(Month)~Yr_type)

#OK! This is something I can work with!!!

#I need to look at the details, but I think this is something useful. HIgher values for SMSCG action anyway.

#####################################################################
#try using the IBMR regions instead.

subregions = read_sf("subregions/subregions.shp")
load("C:/Users/rhartman/OneDrive - California Department of Water Resources/salinity control gates/SMSCG/HabitatmapsRMA.RData")

ex <- exact_extract(s, subregions, fun='mean', force_df = TRUE, progress = TRUE)

meanSHIt = bind_cols(subregion = subregions$SUBREGION, ex)

meanLong = pivot_longer(meanSHIt, cols = 2:31, names_to = "RunName", values_to = "HSI_mean") %>%
  mutate(RunName = str_remove(RunName, "mean."),
         Year = case_when(
           str_detect(RunName, "1940") ~ "1940",
           str_detect(RunName, "1979") ~ "1979",
           str_detect(RunName, "1986") ~ "1986"
         ),
         Month = case_when(
           str_detect(RunName, ".08") ~ "August",
           str_detect(RunName, ".07") ~ "July",
           str_detect(RunName, ".06") ~ "June",
           str_detect(RunName, ".09") ~ "Sep",
           str_detect(RunName, ".10") ~ "Oct"
         ),
         Action = case_when(
           str_detect(RunName, "no.action") ~ "noAction",
           str_detect(RunName, "smscg") ~"GateAction"
         ))


runname = names(ex) %>%
  str_remove("mean.")

###################################################################
#Can I plot this??

library(deltamapr)

#just look at Pseudodiaptomus in bleow normal years
BN = filter(IEP_wzoop, Yr_type == "Below Normal", IBMR == "pdiapfor", Month == 7, !is.na(Action)) %>%
  left_join(dplyr::select(IEP_sf_HSI, Station, Source, geometry))
ggplot()+geom_sf(data = WW_Delta) + geom_sf(data = BN, aes(size = BPUEm, geometry = geometry, color = HSI)) +
  facet_wrap(~Action)

#just look at Pseudodiaptomus in bleow normal years
BNaug = filter(IEP_wzoop, Yr_type == "Below Normal", IBMR == "pdiapfor", Month == 8, !is.na(Action)) %>%
  left_join(dplyr::select(IEP_sf_HSI, Station, Source, geometry))
ggplot()+geom_sf(data = WW_Delta) + geom_sf(data = BNaug, aes(size = BPUEm, geometry = geometry, color = HSI)) +
  facet_wrap(~Action)

ggplot()+geom_sf(data = WW_Delta) + geom_sf(data = BN, aes(size = BPUEm, geometry = geometry, color = BPUEm))# +
 # facet_wrap(~Action)


#what about all the copepeods? all the summer months
BNcal = filter(IEP_wzoop, Yr_type == "Below Normal", IBMR %in% c("pdiapfor","acartela","eurytem" ,"othcalad", "othcyc" ,"othcaljuv",
                                                                 "limno"), Action == "SMSCG", Month %in% c(7,8,9)) %>%
  group_by(Station, Source, Month) %>%
  dplyr::summarize(BPUE = sum(BPUEm)) %>%
    group_by(Station) %>%
    dplyr::summarise(BPUEm = mean(BPUE)) %>%
  left_join(dplyr::select(IEP_sf_HSI, Station, Source, geometry))


ggplot()+geom_sf(data = WW_Delta) + geom_sf(data = BNcal, aes(size = BPUEm, geometry = geometry, color = BPUEm))# +
# facet_wrap(~Action)
