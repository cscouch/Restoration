rm(list = ls())

dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Post-storm Response/AprilWorkshop"))

library(tidyverse)
library(lubridate)
library(sf)
library(data.table)
library(viridis) 



# Load data ---------------------------------------------------------------

# Load island shapefile for plotting in R
islands <- st_read("islands.shp")

dmg <- read.csv("DMGScores-OAHU-041724.csv") %>% filter(Damage_Score != "D")

g1 <- read.csv("DMG-OAHU-GROUP1-041724.csv"); g1$Group = "Group1"
g2 <- read.csv("DMG-OAHU-GROUP2-4.17.2024.csv"); g2$Group = "Group2"
g3 <- read.csv("DMG-OAHU-GROUP3-041724.csv"); g3$Group = "Group3"
g4 <- read.csv("DMG-OAHU-GROUP4-041724.csv"); g4$Group = "Group4"

tracks <- rbind(g1, g2, g3, g4); rm(g1, g2, g3, g4)


# Data clean up and dealing with date/time formats ------------------------

#Clean up track file and convert date time columns
tracks <- tracks %>%
  separate(time, into = c("Date", "Time2"), 
           sep = " ", convert = TRUE, extra = "merge") %>%
  mutate(date = ymd(Date),
         datetime_utc = as_datetime(paste(Date, Time2)),
         datetime_hst = as_datetime(datetime_utc, tz = 'US/Hawaii')) %>%
  separate(datetime_hst, into = c("d", "Time"), 
           sep = " ", convert = TRUE, extra = "merge") %>%
  select(Latitude, Longitude, Group, Date, Time)

tracks$Date <- as.Date(tracks$Date)


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

# Quick visual check- which time points are within the damge time  --------
tracks %>% 
  ggplot(aes(Longitude, Latitude, color = in_out)) + 
  geom_point()  + 
  facet_wrap(~Group, scales = "free")


# Trim tracks data and define start and end times for damage scores -------
#Trim tracks to start and end 
start_time <- min(dmg$Time)
end_time <- max(dmg$Time)

tracks <- tracks %>%
  filter(between(Time, start_time, end_time))

#Trim tracks to GPS start and GPS end 
tracks <- tracks %>%
  filter(in_out != "out")

# Determine the max time in the track dataset. This should be identical to max(dmg$Time)
max(tracks$Time)
max(dmg$Time)

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

#Quick check of both data frames
head(dmg)
head(tracks)

# Merge damage and tracks data --------------------------------------------

# Use data.table for fast interval matching
setDT(dmg)
setDT(tracks)

#Change Group to Organization in tracks
tracks <- tracks %>%
  rename(Organization = Group)

# Assign foverlaps columns
dmg[, `:=`(start = Time, end = End_Time)]
tracks[, `:=`(start = Time, end = Time)]

# Drop NA intervals
dmg <- dmg[!is.na(start) & !is.na(end)]

#Check to see if start time is > end, drop these rows of data
dmg <- dmg[!(start > end)]

# Set keys and do overlap join
setkey(dmg, start, end)
setkey(tracks, start, end)

#convert group to factor
dmg$Organization <- factor(dmg$Organization)  
tracks$Organization <- factor(tracks$Organization)

#Join by Organization
group_list <- unique(tracks$Organization)

tracks_matched <- rbindlist(lapply(group_list, function(g) {
  foverlaps(
    tracks[Organization == g],
    dmg[Organization == g, .(start = Time, end = End_Time, Organization, Damage_Score)],
    type = "within"
  )
}))

tracks_matched <- tracks_matched %>% 
  drop_na(Damage_Score)

# Perform interval join
tracks <- tracks_matched[, .(Latitude, Longitude, Organization, Date, Time, Damage_Score)]

# Assign matched damage scores
tracks$Damage_Score = as.numeric(tracks$Damage_Score)

#Export combined data as csv file
write.csv(tracks,"Damage_tracks_comb_Kiesi.csv")



# Plot tracks 3 ways ------------------------------------------------------

# Without any Island shape files (the surveys were conducted across different spatial scales, so this view allows you to see the individual tracks for each group more clearly)
ggplot(tracks, aes(Longitude, Latitude, color = Damage_Score)) + 
  geom_point(shape = 21, size = 2, alpha = 0.6) + 
  scale_color_viridis(option = "C", direction = -1, na.value = "grey90", name = "Damage Score") +
  facet_wrap(~Organization, scales = "free") +
  labs(title = "Survey Tracks by Organization", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

#Add island-level shape file and combine all groups together
#Note, you will need to adjust the map extent (xlim and ylim) based on your survey area
ggplot(islands) + 
  geom_sf(fill = "grey90", color = "black") +
  geom_point(data = tracks, aes(Longitude, Latitude, color = Damage_Score), 
             shape = 21, size = 2, alpha = 0.6) + 
  coord_sf(xlim = c(-157.94, -157.86), ylim = c(21.28, 21.33), expand = FALSE) +
  scale_color_viridis(option = "C", direction = -1, na.value = "grey90", name = "Damage Score") +
  labs(title = "All Tracks with Island Map", x = "Longitude", y = "Latitude") +
  theme_minimal()

#Add island-level shape file, but visualize each group in its own map. Note- group 4 is barely visible because they surveyed such a small area. 
ggplot(islands) + 
  geom_sf(fill = "grey90", color = "black") +
  geom_point(data = tracks, aes(Longitude, Latitude, color = Damage_Score), 
             shape = 21, size = 2, alpha = 0.6) + 
  coord_sf(xlim = c(-157.94, -157.86), ylim = c(21.28, 21.33), expand = FALSE) +
  scale_color_viridis(option = "C", direction = -1, na.value = "grey90", name = "Damage Score") +
  facet_wrap(~Organization) +
  labs(title = "Survey Tracks by Organization with Island Map", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

