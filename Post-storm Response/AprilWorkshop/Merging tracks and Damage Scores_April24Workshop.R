rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Post-storm Response/AprilWorkshop"))


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

### read in tracks
dmg <- read.csv("DMGScores-OAHU-041724.csv") 
debris<-subset(dmg,Damage_Score =="D")
dmg<-subset(dmg,Damage_Score !="D")

g1<-read.csv("DMG-OAHU-GROUP1-041724.csv")
g2<-read.csv("DMG-OAHU-GROUP2-4.17.2024.csv")
g3<-read.csv("DMG-OAHU-GROUP3-041724.csv")
g4<-read.csv("DMG-OAHU-GROUP4-041724.csv")

tracks<-rbind(g1,g2,g3,g4)



#Clean up track file and convert date time columns
tracks <- tracks %>%
  dplyr::select(Latitude, Longitude, time) %>%
  separate(time,into=c("Date", "Time2"), 
           sep=" ", convert = TRUE, extra = "merge") %>%
  mutate(date = ymd(Date),
         datetime_utc = as_datetime(paste(Date, Time2)),
         datetime_hst = as_datetime(datetime_utc, tz = 'US/Hawaii'))%>%
  separate(datetime_hst,into=c("d", "Time"), 
           sep=" ", convert = TRUE, extra = "merge")%>%
  dplyr::select(Latitude, Longitude,Date, Time)

tracks$Date<-as.Date(tracks$Date)


# #tracks$Date<-dplyr::filter(Date =="2024-04-17")
# date.range <- interval(as.POSIXct("9:43:00"), 
#                        as.POSIXct("11:31:42")) 
# 
# tracks <- tracks[tracks$Time %within% date.range,]
# 
# head(tracks)

#Clean up track file and convert date time columns
dmg$Time <- as.POSIXct(dmg$Time,format="%H:%M:%S")
dmg <- dmg %>%
  separate(Time,into=c("d", "Time"), 
           sep=" ", convert = TRUE, extra = "merge")%>%
  dplyr::select(-c(Notes)) 


# Convert time strings to POSIXct datetime objects
tracks$Time <- as.POSIXct(paste("2024-04-17", tracks$Time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dmg$Time <- as.POSIXct(paste("2024-04-17", dmg$Time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

start_time<-min(dmg$Time)
end_time<-max(dmg$Time)

#Trim tracks to start and end 
tracks<-tracks %>%
  filter(between(Time, start_time, end_time))

# Determine the max time in the track dataset
max_time_track <- max(tracks$Time)

# Set end times to one second before the next damage score starts, except for the last damage score
dmg <- dmg %>%
  arrange(Time) %>%
  # mutate(End_Time = if_else(row_number() == n(), max_time_track, lead(Time) - seconds(1)))
  mutate(End_Time = lead(Time) - seconds(1))
  
# Create intervals
dmg$Interval <- interval(dmg$Time, dmg$End_Time)

dmg<-subset(dmg,Organization=="Group 1")

# Initialize the Damage_Score column in track
tracks$Damage_Score <- NA

# Iterate over each row in track to assign Damage_Score
for (i in 1:nrow(tracks)) {
  
  for (j in 1:nrow(dmg)) {
    
    if (tracks$Time[i] %within% dmg$Interval[j]) {
      
      tracks$Damage_Score[i] <- dmg$Damage_Score[j]
      
      break  # Stop checking once the correct interval is found
      
    }
    
  }
}

# View the updated track df
ggplot(tracks, aes(Longitude, Latitude, fill = factor(Damage_Score))) + 
  geom_point(shape = 21, size = 4) + 
  coord_fixed()

