rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Post-storm Response"))


library(dplyr)
library(sp)
library(sf)
library(raster)
library(ncf) # for gcdist()
library(ggsn)
library(ggspatial)
library(ggrepel)
library(lubridate)
library(hms)
library(tidyr)

### read in track and damage data
track <- read.csv("Test_track.csv") 
dmg <- read.csv("DataEntryRapidDamageAssessment_Test.csv") 

#Clean up track file and convert date time columns
track <- track %>%
  dplyr::select(Latitude, Longitude, Time) %>%
  separate(Time,into=c("Date", "Time2"), 
           sep=" ", convert = TRUE, extra = "merge") %>%
  mutate(date = ymd(Date),
         datetime_utc = as_datetime(paste(Date, Time2)),
         datetime_hst = as_datetime(datetime_utc, tz = 'US/Hawaii'))%>%
  separate(datetime_hst,into=c("d", "Time"), 
           sep=" ", convert = TRUE, extra = "merge")%>%
  dplyr::select(Latitude, Longitude, Time) 
  
head(track)

#Clean up track file and convert date time columns
dmg$Time <- as.POSIXct(dmg$Time,format="%H:%M:%S")
dmg <- dmg %>%
separate(Time,into=c("d", "Time"), 
         sep=" ", convert = TRUE, extra = "merge")%>%
  dplyr::select(-c(Notes)) 

dmgmeta.cols<-colnames(dmg)

dmg.meta<-dmg %>% distinct(Dataset_Name,Organization,Topside_Data_Recorder,In.water_Observer,    
Island,Sub.Island_Region,Survey_Type,GPS_Unit,GPS_Start,GPS_End,Tow_Dive_Number, Date)
dmg.meta

#Change NAs to 0(areas were damage wasn't recorded)
test<-left_join(track,dmg) %>%
  replace(is.na(.),0)

head(test)

write.csv(test,"testmerge.csv")

