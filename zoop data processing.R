#Set up the zooplankton data


require(dplyr)
require(zooper)
require(lubridate)
require(readr)
require(tidyr)
require(ggplot2)
require(sf)
require(readxl)
require(stringr)
require(mgcv)
require(purrr)

#Load zoop data

zoop_data<-Zoopsynther(Data_type="Community", Sources=c("EMP", "STN", "20mm", "FMWT"), Time_consistency = TRUE)


#Read in zoop mass conversions

zoop_mass_conversions<-read_excel("Biomass conversions.xlsx", sheet="Micro and Meso-zooplankton")%>%
  mutate(Taxname=case_when(Taxname=="Sinocalanus"~"Sinocalanus doerrii",
                           TRUE ~ Taxname),
         Lifestage=recode(Lifestage, Larvae="Larva"),
         Taxlifestage=paste(Taxname, Lifestage))%>%
  select(Taxlifestage, CarbonWeight_ug)


#Read in zoop groupings
zoop_groups<-read_csv("Zooplankton/zoopcrosswalk2.csv", col_types=cols_only(Taxlifestage="c", IBMR="c"))%>%
  distinct()


#Load Mysid biomass data
zoop_mysid<-read_excel("1972-2020MysidBPUEMatrix.xlsx",
                       sheet="Mysid_BPUE_matrix_1972-2020", na = "NA",
                       col_types = c(rep("numeric", 4), "date", "text", "text", rep("text", 7), rep("numeric", 8)))%>%
  select(Date=SampleDate, Station=StationNZ, BPUE=`Hyperacanthomysis longirostris`)%>%
  mutate(Source="EMP")%>%
  bind_rows(read_csv("FMWT STN 2007to2019 Mysid BPUE.csv", 
                     col_types=cols_only(Station="c", SampleDate="c", Project="c", `Hyperacanthomysis longirostris`="d"))%>%
              rename(Date=SampleDate, Source=Project, BPUE=`Hyperacanthomysis longirostris`)%>%
              mutate(Date=mdy(Date),
                     Station=recode(Station, MONT="Mont", HONK="Honk")))%>%
  mutate(BPUE_mysid=BPUE*1000, # Convert to ug
         Taxlifestage="Hyperacanthomysis longirostris Adult",
         SampleID=paste(Source, Station, Date),
         SizeClass="Macro")%>%
  select(SampleID, Taxlifestage, SizeClass, BPUE_mysid)



#Start processing the zoop data
zoop_data_mass<-zoop_data%>%
  mutate(Taxlifestage=str_remove(Taxlifestage, fixed("_UnID")))%>%
  filter(
    !(SizeClass=="Meso" & #eliminating species which are counted in meso and micro and retained better in the micro net from the meso calcs
        
        Taxlifestage%in%c("Asplanchna Adult", "Copepoda Larva","Cyclopoida Juvenile", "Eurytemora Larva", "Harpacticoida Undifferentiated",
                          "Keratella Adult", "Limnoithona Adult", "Limnoithona Juvenile", "Limnoithona sinenesis Adult", "Limnoithona tetraspina
                                    Adult", "Oithona Adult", "Oithona Juvenile", "Oithona davisae Adult", "Polyarthra Adult","Pseudodiaptomus Larva", 
                          "Rotifera Adult", "Sinocalanus doerrii Larva", "Synchaeta Adult", "Synchaeta bicornis Adult", "Trichocerca Adult")) &
      
      !(SizeClass=="Micro" &Taxlifestage%in%c("Cirripedia Larva", "Cyclopoida Adult", "Oithona similis")) & #removing categories better retained in meso net from micro net matrix
      Order!="Amphipoda" &
      (Order!="Mysida" | Taxlifestage=="Hyperacanthomysis longirostris Adult"))%>%
  mutate(Taxlifestage=recode(Taxlifestage, `Synchaeta bicornis Adult`="Synchaeta Adult",
                             `Pseudodiaptomus Adult`="Pseudodiaptomus forbesi Adult",
                             `Acanthocyclops vernalis Adult`="Acanthocyclops Adult"))%>%
  left_join(zoop_mass_conversions, by="Taxlifestage")%>%
  left_join(zoop_mysid, by=c("SampleID", "Taxlifestage", "SizeClass"))%>%
  left_join(zoop_groups, by="Taxlifestage")%>%
  mutate(BPUE=if_else(Taxlifestage=="Hyperacanthomysis longirostris Adult", BPUE_mysid, CPUE*CarbonWeight_ug))%>%
  filter(!is.na(BPUE) & !is.na(Latitude) & !is.na(Longitude) & !is.na(SalSurf))%>% # Removes any data without BPUE, which is currently restricted to Decapod Larvae, and H. longirostris from STN. Also removes 20mm and EMP EZ stations without coordinates
  group_by(IBMR)%>%
  mutate(flag=if_else(all(c("Micro", "Meso")%in%SizeClass), "Remove", "Keep"))%>% # This and the next 2 lines are meant to ensure that all categories are consistent across the surveys. Since only EMP samples microzoops, only EMP data can be used for categories that include both micro and mesozoops.
  ungroup()%>%
  filter(!(flag=="Remove" & Source!="EMP"))%>%
  select(SampleID, Station, Latitude, Longitude, SalSurf, Date, Year, IBMR, BPUE)%>%
  group_by(across(-BPUE))%>%
  summarise(BPUE=sum(BPUE), .groups="drop")%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=st_crs(deltamapr::R_EDSM_Subregions_Mahardja))%>%
#  st_join(deltamapr::R_EDSM_Subregions_Mahardja%>%
#            select(SubRegion))%>%
#  st_drop_geometry()%>%
#  filter(SubRegion=="Suisun Marsh")%>%
  mutate(doy=yday(Date),
         Year_fac=factor(Year),
         Station_fac=factor(Station),
         BPUE_log1p=log(BPUE+1))

#load the IBMR subregions

IBMRregions = read_sf("subregions/subregions.shp")

zoop_data_mass2 = st_transform(zoop_data_mass, crs = st_crs(IBMRregions))%>%
  st_join(select(IBMRregions, SUBREGION)) %>%
  st_drop_geometry()

Zoop_stas = group_by(zoop_data_mass, Station) %>%
  summarise(n = n())

ggplot()+ geom_sf(data = IBMRregions, aes(fill = SUBREGION))+
  geom_sf(data = Zoop_stas)
