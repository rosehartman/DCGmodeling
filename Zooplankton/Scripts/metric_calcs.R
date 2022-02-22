#FLOAT-MAST And DROUGHT Synthesis metric calculations
#The prupose of this script is to make metrics based on the design outlined on the shared document:

#https://cawater.sharepoint.com/:w:/r/sites/dwr-str/FLOAT/_layouts/15/Doc.aspx?sourcedoc=%7B3BFD28BC-5F31-4A8B-BAE6-7379F37F931A%7D&file=FLOAT%20and%20Drought%20MAST%20analyses.docx&action=default&mobileredirect=true

#Arthur Barros 6/29/2021
#arthur.barros@wildlife.ca.gov

rm( list = ls()) #clear env
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

biomass_macro<-read_excel("Data/ZoopSynth_biomass.xlsx", 
                          sheet = "ZoopSynth Macro-zooplankton")
biomass_macro$Taxlifestage<-paste(biomass_macro$Taxname,biomass_macro$Lifestage,sep=" ")
biomass_macro<-dplyr::select(biomass_macro,Taxlifestage,a,b)

#load the zooplankton meso and micro count data (created in the zooper_cleaning script)
Zoop_data<-readRDS("Data/Zooper_data.rds")

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

target_DROUGHT_taxa<-c("Pseudodiaptomus Juvenile",
                     "Pseudodiaptomus forbesi Adult",
                     "Daphnia Adult",
                     "Acartiella sinensis Adult",
                     "Limnoithona tetraspina Adult",
                     "Eurytemora affinis Adult",
                     "Eurytemora affinis Juvenile",
                     "Hyperacanthomysis longirostris Adult",
                     "Neomysis kadiakensis Adult",
                     "Neomysis mercedis Adult",
                     "Daphnia Adult",
                     "Tortanus Adult",
                     "Acartia Adult",
                     "Diaphanosoma Adult"
                     )

#################################
#Prep data for all analysis
#################################

#remove "_UnID" from taxa names
Zoop_data$Taxlifestage<-Zoop_data$Taxlifestage%>%str_replace("_UnID","")
Zoop_data$Taxlifestage<-ifelse(Zoop_data$Taxlifestage=="Limnoithona Adult","Limnoithona tetraspina Adult",Zoop_data$Taxlifestage)

#Meso-biomass calculations
t_size<-c("Meso","Micro") #set target sizes for "meso" (really meso+micro)
Zoop_meso<-Zoop_data%>%filter(SizeClass%in%t_size,Date>"1974-12-31")
#change daphnia names so it will link with biomass
Zoop_meso$Taxlifestage[Zoop_meso$Taxlifestage=="Daphnia_UnID Adult"]<-"Daphnia Adult"
Zoop_meso<-Zoop_meso%>%left_join(biomass_meso)
Zoop_meso$BPUE<-Zoop_meso$CPUE*Zoop_meso$CarbonWeight_ug

#pull in mysid counts and lengths from EMP to get macro BPUE
#note, this data is not published publicly yet
#I could use the access calculated BPUE, but chose to do it here to double check the math
Macro_lengths<-read.delim("Data/MysidAmphipodLengths.txt")
Macro_pluscounts<-read.delim("Data/MysidAmphipodPlusCounts.txt")

Macro_lf<-Macro_lengths%>% #calculate length frequencies
  dplyr::group_by(SampleDate,Station,SpeciesCode,SizeGroup,Size)%>%
  tally()

Macro_lengths_counted<-Macro_lengths%>% #calculate number counted for each taxa
  group_by(SampleDate,Station,SpeciesCode,SizeGroup)%>%
  tally()

Macro_totals<-Macro_lengths_counted%>% #calculate totals between length measurements and pluscounts
  full_join(Macro_pluscounts)
Macro_totals$Total<-(Macro_totals$n+Macro_totals$PlusCount)/Macro_totals$LabSubsample
Macro_totals<-dplyr::rename(Macro_totals,NumberMeasured=n)

#calculate adj frequency by taking the length frequency for each length (n) divided by the number of that mysid measured in that sample, multiplied by the total number of that mysid counted in the sample
Macro_lf<-Macro_lf%>%inner_join(Macro_totals)
#fix dates
Macro_lf$SampleDate<-as.Date(Macro_lf$SampleDate,"%m/%d/%Y")
Macro_lf$adj_freq<-(Macro_lf$n/Macro_lf$NumberMeasured)*Macro_lf$Total

#join species lookup data with macro length frequencies to link with biomass lookup table
species_lu<- read_excel("Data/MysidAmphipodSpeciesLookUp.xlsx")
Macro_lf<-Macro_lf%>%inner_join(species_lu)
Macro_lf$Taxlifestage<-paste(Macro_lf$SpeciesName,"Adult",sep=" ")

#Now join Macro_lf with biomass_macro and calculate dry weight for each size
Macro_biomass<-Macro_lf%>%left_join(biomass_macro)
Macro_biomass$dry_weight<-(Macro_biomass$a)*(Macro_biomass$Size^Macro_biomass$b) #calculate length dry weights
Macro_biomass$Total_dw<-Macro_biomass$dry_weight*Macro_biomass$adj_freq #calculate total dry weight for that taxa in that sample based on adj length frequencies

#calc total carbon per sample and species sizegroup
Macro_biomass<-Macro_biomass%>%
  dplyr::group_by(SampleDate,Station,Gear,Taxlifestage,SizeGroup)%>%
  dplyr::summarise(Total_sample_dw=sum(Total_dw))
Macro_biomass$carbon<-Macro_biomass$Total_sample_dw*0.4 #I don't know where the 0.4 carbon conversion rate comes from

#bring in volume and calculate BPUE
volumes<-read_excel("Data/Mysid_Qry1 Volume.xlsx")
Macro_biomass$SampleDate<-as.Date(Macro_biomass$SampleDate,"%m/%d/%Y")

Macro_biomass<-Macro_biomass%>%inner_join(volumes)
Macro_biomass$BPUE<-Macro_biomass$carbon/Macro_biomass$Volume
Macro_biomass$BPUE<-Macro_biomass$BPUE*1e3 #change milligrams to micrograms to fit with meso units
  
#filter to be 1975 and after
Macro_biomass<-Macro_biomass%>%
  filter(SampleDate>"1974-12-31")

#join in station info
Stations<-read_excel("Data/StationLookUp.xlsx")
Zoop_macro<-Macro_biomass%>%left_join(Stations)
Zoop_macro$Station<-Zoop_macro$StationNZ
Zoop_macro$Longitude<--(Zoop_macro$longdec)
Zoop_macro$Latitude<-Zoop_macro$latdec

#join meso and macro together
Zoop_meso<-dplyr::select(Zoop_meso,Date,Station,Source,SizeClass,Taxlifestage,BPUE,Latitude,Longitude)
Zoop_macro$Date<-Zoop_macro$SampleDate
Zoop_macro$Source<-"EMP"
Zoop_macro$SizeClass<-"Macro"
Zoop_macro<-ungroup(Zoop_macro)
Zoop_macro<-(dplyr::select(Zoop_macro,Date,Station,Source,SizeClass,Taxlifestage,BPUE,Latitude,Longitude))

Zoop_BPUE<-Zoop_meso%>%rbind(Zoop_macro)
Zoop_BPUE$Date<-as.Date(Zoop_BPUE$Date,"%m/%d/%Y")

#Lets bring in the FMWT Mysid BPUE data, which will be used for the short-term metrics
FMWT_Macro <- read_excel("Data/FMWT_MysidBPUE_2007to2018_25Sept2020.xlsx", 
                         sheet = "FMWT Mysid BPUE Matrix")
FMWT_Stations<-read_excel("Data/FMWT_MysidBPUE_2007to2018_25Sept2020.xlsx", 
                          sheet = "Station Lookup")
FMWT_Stations$Latitude<-FMWT_Stations$LatDecDegrees
FMWT_Stations$Longitude<-FMWT_Stations$LongDecDegrees
FMWT_Stations<-dplyr::select(FMWT_Stations,Station,Longitude,Latitude)
FMWT_Stations$Source<-"FMWT"

FMWT_Macro<-FMWT_Macro%>%inner_join(dplyr::select(FMWT_Stations,Station,Longitude,Latitude))

#pivot longer
FMWT_Macro_long<-pivot_longer(FMWT_Macro,cols="Acanthomysis aspera":"Neomysis mercedis",names_to = "Taxlifestage",values_to = "BPUE")
FMWT_Macro_long$Taxlifestage<-paste(FMWT_Macro_long$Taxlifestage,"Adult")
FMWT_Macro_long<-dplyr::select(FMWT_Macro_long,Date,Station,Taxlifestage,BPUE,Longitude, Latitude)
FMWT_Macro_long$Source<-c("FMWT")
FMWT_Macro_long$SizeClass<-c("Macro")

FMWT_Macro_long$BPUE<-FMWT_Macro_long$BPUE*1e3 #change BPUE from milligrams to micrograms

#rbind in FMWT_Macro_long with Zoop_BPUE
Zoop_BPUE<-Zoop_BPUE%>%rbind(FMWT_Macro_long)
#Zoop_BPUE should now have all the BPUE data we need for both short and long-term metrics

###############################
#Determine SubRegion assignments
###############################

#fit into deltamapr subregions for Brians adjusted EDSM subregions

WaterMap<-readRDS("Data/WaterMap.rds") #load watermap
stratum<-(R_EDSM_Subregions_Mahardja)%>%
  st_transform(4326)

#run ez stations seperately because they are non-fixed
ez<-c("NZEZ6","NZEZ2","NZEZ2SJR","NZEZ6SJR")
ez_stations<-unique(dplyr::select(Zoop_BPUE,Source,Station,Longitude,Latitude,Date))%>%filter(Station%in%ez)
ez_stations<-ez_stations[complete.cases(ez_stations$Longitude),]
ez_stations_geom<-st_as_sf(ez_stations,coords = c("Longitude", "Latitude"),remove = TRUE, crs = 4326)
p<-ggplot()+
  geom_sf(data=stratum,aes(fill=SubRegion))+
  geom_sf(data=spacetools::Delta)+
  geom_sf(data = ez_stations_geom$geometry, size = 2, 
          shape = 23, fill = "green")+
  theme(legend.position = "none") 
p
ggsave("Figures/ez_station_subregions.png")
ez_subregions<-st_join(ez_stations_geom,stratum,join = st_within) #joing ez stations to subregions
st_geometry(ez_subregions)<-NULL
ez_subregions<-unique((dplyr::select(ez_subregions,Source,Date,Station,SubRegion)))

#regular stations
zoop_stations<-unique(dplyr::select(Zoop_BPUE,Source,Station,Longitude,Latitude))
zoop_stations<-zoop_stations[complete.cases(zoop_stations$Longitude),]
zoop_stations<-(zoop_stations%>%filter(!Station%in%ez))
zoop_stations<-unique(as.matrix(zoop_stations)) #for some reason have to do this to remove duplicates, wasn;t working on just the dataframe?
zoop_stations<-as.data.frame(zoop_stations)

zoop_stations_geom<-st_as_sf(zoop_stations,coords = c("Longitude", "Latitude"),remove = TRUE, crs = 4326)

p<-ggplot()+
  geom_sf(data=stratum,aes(fill=SubRegion))+
  geom_sf(data=spacetools::Delta)+
  geom_sf(data = zoop_stations_geom$geometry, size = 2, 
          shape = 23, fill = "green")+
  theme(legend.position = "none") 
p
ggsave("Figures/station_subregions.png")
station_subregions<-st_join(zoop_stations_geom,stratum,join = st_within)
st_geometry(station_subregions)<-NULL
station_subregions<-unique((dplyr::select(station_subregions,Source,Station,SubRegion)))

#join in new subregion station assignments to the Zoop_BPUE dataframe
Zoop_BPUE_regions<-Zoop_BPUE%>%inner_join(station_subregions)
Zoop_BPUE_ez_regions<-Zoop_BPUE%>%inner_join(ez_subregions)
Zoop_BPUE<-Zoop_BPUE_regions%>%rbind(Zoop_BPUE_ez_regions)

Zoop_BPUE$water_year<-wtr_yr(Zoop_BPUE$Date)

#assign month and years
Zoop_BPUE$month<-as.numeric(format(as.Date(Zoop_BPUE$Date), "%m"))
Zoop_BPUE$year<-as.numeric(format(as.Date(Zoop_BPUE$Date), "%Y"))

#################################
#FLOAT Long term analysis (1975-2010,EMP)
#################################

#filter for target metrics
Zoop_LT<-Zoop_BPUE%>%
  filter(Source=="EMP",water_year<2011,Taxlifestage%in%target_FLOAT_taxa)

#first calc total BPUE for each sample
Zoop_sample_totals<-Zoop_LT%>%group_by(Station,Date,SubRegion,water_year,month)%>%
  dplyr::summarise(totalBPUE=sum(BPUE))
#calculate monthly average totalBPUE
Zoop_BPUE_monthly_average<-Zoop_sample_totals%>%
  group_by(water_year,month,SubRegion)%>%
  dplyr::summarise(month_BPUE=mean(totalBPUE))

#calculate seasonal averages for each subregion
month<-c(1,2,3,4,5,6,7,8,9,10,11,12)
Season<-c("Winter","Winter","Spring","Spring","Spring","Summer","Summer","Summer","Fall","Fall","Fall","Winter")
szn_mnth<-data.frame(month,Season)
szn_mnth$Season<-factor(szn_mnth$Season,levels=c("Winter","Spring","Summer","Fall"))

Zoop_BPUE_monthly_average<-Zoop_BPUE_monthly_average%>%inner_join(szn_mnth)
Zoop_BPUE_szn_avg<-Zoop_BPUE_monthly_average%>%group_by(Season,water_year,SubRegion)%>%
  dplyr::summarise(szn_BPUE=mean(month_BPUE))

#drop regions not in fall low_sal zone or long-term analysis lists
sams_table<-read.csv("Data/Low_salinity_zone.csv")
sams_table$water_year<-sams_table$Year
Zoop_BPUE_szn_avg<-Zoop_BPUE_szn_avg%>%inner_join(sams_table)#inner join narrows to just low_sal_zone
Zoop_FLOAT_LT<-Zoop_BPUE_szn_avg%>%
  filter(Long_term=="TRUE")

#calculate year szn average across subregions
Zoop_FLOAT_LT<-Zoop_FLOAT_LT%>%
  group_by(water_year,Season)%>%
  dplyr::summarise(szn_BPUE=mean(szn_BPUE))

Zoop_FLOAT_LT<-dplyr::rename(Zoop_FLOAT_LT,BPUE_ug=szn_BPUE)

#Taxa break down
taxa<-Zoop_LT%>%group_by(Station,Date,SubRegion,water_year,month,Taxlifestage)%>%
  dplyr::summarise(totalBPUE=sum(BPUE))
#calculate monthly average totalBPUE
taxa<-taxa%>%
  group_by(water_year,month,SubRegion,Taxlifestage)%>%
  dplyr::summarise(month_BPUE=mean(totalBPUE))

taxa<-taxa%>%inner_join(szn_mnth)
taxa<-taxa%>%group_by(Season,water_year,SubRegion,Taxlifestage)%>%
  dplyr::summarise(szn_BPUE=mean(month_BPUE))

taxa<-taxa%>%inner_join(sams_table)#inner join narrows to just low_sal_zone
taxa<-taxa%>%
  filter(Long_term=="TRUE")
#calculate year szn average across subregions
taxa<-taxa%>%
  group_by(water_year,Season,Taxlifestage)%>%
  dplyr::summarise(szn_BPUE=mean(szn_BPUE))
taxa<-dplyr::rename(taxa,BPUE_ug=szn_BPUE)
p<-ggplot(taxa,aes(water_year,BPUE_ug,fill=Taxlifestage))+
  geom_bar(stat="identity")+
  facet_wrap(~Season)
p
ggsave("Figures/FLOAT_BPUE_Taxa.png")

##############################
#FLOAT Short-term analysis (2010-2020, EMP, FMWT, STN, 20mm)
##############################
Zoop_ST<-Zoop_BPUE%>%
  filter(water_year>2009,Taxlifestage%in%target_FLOAT_taxa)

Zoop_ST_totals<-Zoop_ST%>%group_by(Station,Date,SubRegion,water_year,month)%>%
  dplyr::summarise(totalBPUE=sum(BPUE))

#calc monthly subregion averages
Zoop_ST_m<-Zoop_ST_totals%>%
  group_by(SubRegion,water_year,month)%>%
  dplyr::summarise(month_BPUE=mean(totalBPUE))

#calc seasonal averages for each Subregion
Zoop_ST_s<-Zoop_ST_m%>%inner_join(szn_mnth)
Zoop_ST_s<-Zoop_ST_s%>%
  group_by(Season,water_year,SubRegion)%>%
  dplyr::summarise(season_BPUE=mean(month_BPUE))
#drop regions not in Sams table
Zoop_ST_s<-Zoop_ST_s%>%inner_join(sams_table)%>%
  filter(water_year>2009, Short_term==TRUE)
Zoop_FLOAT_ST<-Zoop_ST_s%>%
  group_by(water_year,Season)%>%
  dplyr::summarise(szn_BPUE=mean(season_BPUE))
Zoop_FLOAT_ST<-dplyr::rename(Zoop_FLOAT_ST,BPUE_ug=szn_BPUE)

#################################
#DROUGHT BPUE Long term analysis (1975-2021,EMP)
#################################
rosies_table<-read.csv("Data/Rosies_regions.csv",fileEncoding="UTF-8-BOM")
Drought_LT<-Zoop_BPUE%>%
  filter(Taxlifestage%in%target_DROUGHT_taxa)

Drought_LT<-Drought_LT%>%inner_join(rosies_table)

#first calc total BPUE for each sample
Drought_LT_t<-Drought_LT%>%group_by(Station,Date,Long_term,Region,water_year,month)%>%
  dplyr::summarise(totalBPUE=sum(BPUE))
Drought_LT_t<-Drought_LT_t%>%
  inner_join(szn_mnth)
Drought_LT_monthly_average<-Drought_LT_t%>%
  group_by(water_year,month,Long_term,Region,Season)%>%
  dplyr::summarise(month_BPUE=mean(totalBPUE))
Drought_LT_m<-Drought_LT_monthly_average%>%
  filter(Long_term==TRUE)
#calculate seasonal averages
Drought_LT_s<-Drought_LT_m%>%
  group_by(water_year,Long_term,Region,Season)%>%
  dplyr::summarise(s_BPUE=mean(month_BPUE))
Zoop_Drought_LT<-Drought_LT_s%>%
  group_by(Season,water_year)%>%
  dplyr::summarise(BPUE_ug=mean(s_BPUE))
Zoop_Drought_LTall<-dplyr::rename(Zoop_Drought_LT)
Zoop_Drought_LTall<-Zoop_Drought_LT%>%
  dplyr::filter(Season != "Winter") %>%
  group_by(water_year)%>%
  dplyr::summarise(BPUE_ug=mean(BPUE_ug))

#now do it by region instead
Zoop_Drought_LT_reg<-Drought_LT_s%>%
  filter(Season != "Winter") %>%
  group_by(Region,water_year)%>%
  dplyr::summarise(reg_BPUE=mean(s_BPUE)) %>%
  dplyr::rename(BPUE_ug=reg_BPUE)


#Taxa break down
taxa<-Drought_LT%>%group_by(Station,Date,Long_term,Region,water_year,month,Taxlifestage)%>%
  dplyr::summarise(totalBPUE=sum(BPUE))
#calculate monthly average totalBPUE
taxa<-taxa%>%
  group_by(water_year,month,Long_term,Region,Taxlifestage)%>%
  dplyr::summarise(month_BPUE=mean(totalBPUE))

taxa<-taxa%>%inner_join(szn_mnth)
taxa_reg<-taxa%>%group_by(Season,water_year,Long_term,Region,Taxlifestage)%>%
  dplyr::summarise(szn_BPUE=mean(month_BPUE))

taxa_reg<-taxa_reg%>%
  filter(Long_term=="TRUE")
#calculate year szn average across subregions
taxa_seas<-taxa_reg%>%
  group_by(water_year,Season,Taxlifestage)%>%
  dplyr::summarise(szn_BPUE=mean(szn_BPUE))
taxa_seas<-dplyr::rename(taxa_seas,BPUE_ug=szn_BPUE)
taxa_annual<-taxa_seas%>%
  filter(Season != "Winter") %>%
  group_by(water_year,Taxlifestage)%>%
  dplyr::summarise(BPUE_ug=mean(BPUE_ug))%>%filter(water_year<2021)

taxa_annual_reg<-taxa_reg%>%
  filter(Season != "Winter") %>%
  group_by(water_year,Taxlifestage, Region)%>%
  dplyr::summarise(BPUE_ug=mean(szn_BPUE))%>%filter(water_year<2021)

p<-ggplot(taxa_annual,aes(water_year,BPUE_ug,fill=Taxlifestage))+
  geom_bar(stat="identity")
p
ggsave("Figures/DROUGHT_BPUE_Taxa.png")

#create joint wide matrix
Drought_LT_matrix<-Zoop_Drought_LTall
Drought_LT_matrix$Taxlifestage<-"All taxa"
Drought_LT_matrix<-dplyr::select(Drought_LT_matrix,water_year,Taxlifestage,BPUE_ug)
Drought_LT_matrix<-Drought_LT_matrix%>%
  rbind(taxa_annual)
Drought_LT_matrix<-Drought_LT_matrix%>%filter(water_year<2021)
Drought_LT_matrix$BPUE_ug[Drought_LT_matrix$BPUE_ug == 0] <- NA

Drought_LT_matrix<-Drought_LT_matrix%>%pivot_wider(names_from=Taxlifestage,values_from=BPUE_ug)

write.csv(Drought_LT_matrix,"Outputs/Drought_taxa_BPUEmatrix.csv",row.names = F)
save(taxa_reg, taxa_seas, taxa_annual, taxa_annual_reg, file = "Taxon_drought.RData")

#################################
#DROUGHT CPUE Long term analysis (1975-2010,EMP)
#this is count and not biomass estimates
#################################
Drought_LT_CPUE<-Zoop_data%>%
  filter(Taxlifestage%in%target_DROUGHT_taxa)
Drought_LT_CPUE$month<-as.numeric(format(as.Date(Drought_LT_CPUE$Date), "%m"))
Drought_LT_CPUE$year<-as.numeric(format(as.Date(Drought_LT_CPUE$Date), "%Y"))
Drought_LT_CPUE<-Drought_LT_CPUE%>%inner_join(szn_mnth)
Drought_LT_CPUE$water_year<-wtr_yr(Drought_LT_CPUE$Date)

Drought_LT_CPUE_dist<-Drought_LT_CPUE #set up for distribution

Drought_LT_CPUE<-Drought_LT_CPUE%>% #join in subregions
  inner_join(station_subregions)

Drought_LT_CPUE<-Drought_LT_CPUE%>%inner_join(rosies_table)

Drought_LT_CPUE_grouped<-Drought_LT_CPUE%>%group_by(Station,Date,Long_term,Region,water_year,month,Season)%>%
  filter(water_year>1974)%>%
  dplyr::summarise(totalCPUE=sum(CPUE))
Drought_LT_CPUE2<-Drought_LT_CPUE_grouped%>%
  group_by(water_year,month,Long_term,Region,Season)%>%
  dplyr::summarise(month_CPUE=mean(totalCPUE))
Drought_LT_CPUE2<-Drought_LT_CPUE2%>%
  filter(Long_term==TRUE)
#calculate seasonal averages
Drought_LT_CPUE2<-Drought_LT_CPUE2%>%
  group_by(water_year,Long_term,Region,Season)%>%
  dplyr::summarise(s_CPUE=mean(month_CPUE))
Zoop_Drought_LT_CPUE2<-Drought_LT_CPUE2%>%
  group_by(Season,water_year)%>%
  dplyr::summarise(szn_CPUE=mean(s_CPUE))
Zoop_Drought_LT_CPUE2<-Zoop_Drought_LT_CPUE2%>%
  group_by(water_year)%>%
  dplyr::summarise(szn_CPUE=mean(szn_CPUE))

#calculate seasonal averages by taxa
Drought_LT_CPUE2tax<-Drought_LT_CPUE%>%
  group_by(water_year,Long_term,Region,Season, month, Taxlifestage)%>%
  dplyr::summarise(sCPUE=mean(CPUE))
tax_Drought_LT_CPUE2<-Drought_LT_CPUE2tax%>%
  group_by(Season,water_year, Taxlifestage)%>%
  dplyr::summarise(szn_CPUE=mean(sCPUE))
write.csv(tax_Drought_LT_CPUE2, file = "SeasonalCPUE_bytaxa.csv", row.names = F)

#Calculate regional averages by water year
Zoop_Drought_LT_REG<-Drought_LT_CPUE_grouped %>%
  group_by(water_year,month, Region,Season)%>%
  dplyr::summarise(month_CPUE=mean(totalCPUE)) %>%
  filter(Season != "Winter") %>%
  group_by(water_year,Region,Season)%>%
  dplyr::summarise(s_cpue = mean(month_CPUE))%>%
  group_by(Region, water_year)%>%
  dplyr::summarise(reg_CPUE=mean(s_cpue))

Zoop_Drought_LT_REG_season<-Drought_LT_CPUE_grouped %>%
  group_by(water_year,month, Region,Season)%>%
  dplyr::summarise(month_CPUE=mean(totalCPUE)) %>%
  filter(Season != "Winter") %>%
  group_by(water_year,Region,Season)%>%

#################################
#DROUGHT effort
#################################
Effort<-unique(dplyr::select(Drought_LT, Date,Station,Source,SizeClass,Long_term,Region,month,year))
Effort<-Effort%>%inner_join(szn_mnth)

Effort<-Effort%>%filter(Long_term==TRUE)
#Effort<-Effort%>%filter(Source=="EMP")
Effort$Station_Date_Source_SizeClass<-paste(Effort$Station,Effort$Date,Effort$Source,Effort$SizeClass,Effort$Region)
Effort<-Effort%>%
  group_by(year,Region,Source,SizeClass)%>%
  dplyr::summarize(tows=length(Station_Date_Source_SizeClass))

#################################
#Save all the outputs
#################################

write.csv(Zoop_FLOAT_LT,"Outputs/zoop_float_lt.csv",row.names = F)
write.csv(Zoop_FLOAT_ST,"Outputs/zoop_float_st.csv",row.names = F)
write.csv(Zoop_Drought_LTall,"Outputs/zoop_drought_lt.csv",row.names = F)
write.csv(Zoop_Drought_LT,"Outputs/zoop_drought_lt_seasonal.csv",row.names = F)
write.csv(Zoop_Drought_LT_CPUE2,"Outputs/zoop_drought_lt_cpue.csv",row.names = F)
write.csv(Zoop_Drought_LT_REG,"Outputs/zoop_drought_lt_REG.csv",row.names = F)
write.csv(Zoop_Drought_LT_reg,"Outputs/zoop_drought_lt_REGbpue.csv",row.names = F)

saveRDS(Zoop_FLOAT_LT,"Outputs/zoop_float_lt.rds")
saveRDS(Zoop_FLOAT_ST,"Outputs/zoop_float_st.rds")
saveRDS(Zoop_Drought_LT,"Outputs/zoop_drought_lt.rds")
saveRDS(Zoop_Drought_LT_CPUE2,"Outputs/zoop_drought_lt_cpue.rds")
saveRDS(Drought_LT_CPUE,"Outputs/drought_lt_cpue.rds")
saveRDS(Drought_LT_CPUE_dist,"Outputs/drought_lt_cpue_dist.rds")
saveRDS(Effort,"Outputs/effort.rds")
saveRDS(ez_stations,"Outputs/ez_stations.rds")
saveRDS(zoop_stations,"Outputs/zoopstations.rds")
 
