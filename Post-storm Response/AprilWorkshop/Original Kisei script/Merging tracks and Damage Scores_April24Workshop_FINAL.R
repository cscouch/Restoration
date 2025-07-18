rm(list = ls())

dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Post-storm Response/AprilWorkshop"))

library(dplyr)
library(sp)
library(sf)
library(raster)
library(ncf) # for gcdist()
# library(ggsn)
library(ggspatial)
library(ggrepel)
library(lubridate)
library(hms)
library(tidyr)

select = dplyr::select

#
dmg <- read.csv("DMGScores-OAHU-041724.csv") %>% filter(Damage_Score != "D")

g1 <- read.csv("DMG-OAHU-GROUP1-041724.csv"); g1$Group = "Group1"
g2 <- read.csv("DMG-OAHU-GROUP2-4.17.2024.csv"); g2$Group = "Group2"
g3 <- read.csv("DMG-OAHU-GROUP3-041724.csv"); g3$Group = "Group3"
g4 <- read.csv("DMG-OAHU-GROUP4-041724.csv"); g4$Group = "Group4"

tracks <- rbind(g1, g2, g3, g4); rm(g1, g2, g3, g4)

#Clean up track file and convert date time columns
tracks <- tracks %>%
  # select(Latitude, Longitude, time) %>%
  separate(time, into = c("Date", "Time2"), 
           sep = " ", convert = TRUE, extra = "merge") %>%
  mutate(date = ymd(Date),
         datetime_utc = as_datetime(paste(Date, Time2)),
         datetime_hst = as_datetime(datetime_utc, tz = 'US/Hawaii')) %>%
  separate(datetime_hst, into = c("d", "Time"), 
           sep = " ", convert = TRUE, extra = "merge") %>%
  select(Latitude, Longitude, Group, Date, Time)

tracks$Date <- as.Date(tracks$Date)

# #tracks$Date<-dplyr::filter(Date =="2024-04-17")
# date.range <- interval(as.POSIXct("9:43:00"), 
#                        as.POSIXct("11:31:42")) 
# 
# tracks <- tracks[tracks$Time %within% date.range,]
# 
# head(tracks)

#Clean up damage file and convert date time columns
dmg$Time <- as.POSIXct(dmg$Time, format = "%H:%M:%S")

dmg <- dmg %>%
  separate(Time, into = c("d", "Time"), 
           sep = " ", convert = TRUE, extra = "merge") %>%
  select(-c(Notes)) 

# Convert time strings to POSIXct datetime objects
tracks$Time <- as.POSIXct(paste("2024-04-17", tracks$Time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dmg$Time <- as.POSIXct(paste("2024-04-17", dmg$Time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Identify GPS gaps in Tracks
# Convert Date and Time in dmg from character to date and datetime types
dmg <- dmg %>%
  mutate(
    Date = mdy(Date),  # Convert Date from character to Date assuming format is m/d/yyyy
    GPS_Start = ymd_hms(paste(Date, GPS_Start)),  # Combine and convert to datetime
    GPS_End = ymd_hms(paste(Date, GPS_End))       # Combine and convert to datetime
  )

# Combine Date and Time in tracks to form a datetime column for comparison
tracks <- tracks %>%
  mutate(datetime = ymd_hms(paste(Date, format(Time, "%H:%M:%S"))))  # Ensure Time is formatted correctly

# Determine if each track's datetime falls within any dmg time interval
tracks <- tracks %>%
  rowwise() %>%
  mutate(in_out = if_else(any(dmg$GPS_Start <= datetime & datetime <= dmg$GPS_End), "in", "out")) %>%
  select(-c(datetime)) 

dmg %>% 
  select(GPS_Start, GPS_End) %>% 
  distinct()

tracks %>% 
  # filter(Group == "Group2") %>% 
  ggplot(aes(Longitude, Latitude, color = in_out)) + 
  geom_point()  + 
  facet_wrap(~Group, scales = "free")

tracks %>% 
  # filter(Group == "Group3") %>%
  ggplot(aes(Longitude, Latitude, color = Time)) + 
  geom_point()  + 
  facet_grid(in_out~Group)

start_time <- min(dmg$Time)
end_time <- max(dmg$Time)

#Trim tracks to start and end 
tracks <- tracks %>%
  filter(between(Time, start_time, end_time))

#Trim tracks to GPS start and GPS end 
tracks <- tracks %>%
  filter(in_out != "out")

# Determine the max time in the track dataset. This should be identical to max(dmg$Time)
max_time_track <- max(tracks$Time)

# Set end times to one second before the next damage score starts, except for the last damage score
dmg <- dmg %>%
  arrange(Time) %>%
  mutate(End_Time = lead(Time) - seconds(1))

# for the first observation, assign the first time stamp from GPS
dmg <- dmg %>%
  arrange(Time) %>%
  mutate(Time = if_else(row_number() == 1, GPS_Start, Time))

# for the last observation, assign the last time stamp from GPS
dmg <- dmg %>%
  arrange(Time) %>%
  mutate(End_Time = if_else(row_number() == n(), GPS_End, End_Time))

# Create intervals
dmg$Interval <- interval(dmg$Time, dmg$End_Time)

# dmg <- subset(dmg,Organization=="Group 1")

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

tracks$Damage_Score = as.numeric(tracks$Damage_Score)
tracks$Damage_Score[is.na(tracks$Damage_Score)] <- NA

# View the updated track df
ggplot(tracks, aes(Longitude, Latitude, fill = Damage_Score, color = Damage_Score)) + 
  geom_point(shape = 21, alpha = 0.5) + 
  facet_wrap(~Group, scales = "free")
