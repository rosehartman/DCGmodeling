#Zooper Cleaning
#last updated 2/23/2021
rm(list = ls()) #clear env

#Load packages:
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(lubridate)
library(measurements)
library(zooper)

#download zoop data
zoop_data<-Zoopdownloader(Data_sets=c("EMP_Meso","FMWT_Meso","STN_Meso","20mm_Meso","EMP_Macro", "FMWT_Macro","STN_Macro"))

Meso_data <- Zoopsynther(Data_type = "Community",
                         Sources = c("EMP",  "FMWT","STN","20mm"),
                         Size_class = "Meso",
                         Date_range = c("1972-01-01", "2020-12-31"))
Macro_data<-Zoopsynther(Data_type = "Community",
                        Sources = c("EMP",  "FMWT","STN"),
                        Size_class = "Macro",
                        Date_range = c("1972-01-01", "2020-12-31"))
Micro_data<-Zoopsynther(Data_type = "Community",
                        Sources = c("EMP"),
                        Size_class = "Micro",
                        Date_range = c("1972-01-01", "2020-12-31"))

Zooper_data<-Meso_data%>%rbind(Micro_data)%>%rbind(Macro_data)

saveRDS(Zooper_data,"Data/Zooper_data.rds")
