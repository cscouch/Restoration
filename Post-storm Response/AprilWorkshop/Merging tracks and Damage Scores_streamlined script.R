
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Post-storm Response/AprilWorkshop"))


library(tidyverse)
library(lubridate)
library(sf)
library(data.table)

# Load shapefile
islands <- st_read("islands.shp")

# Load and clean damage data
dmg <- read.csv("DMGScores-OAHU-041724.csv") %>%
  filter(Damage_Score != "D") %>%
  mutate(
    Date = mdy(Date),  # Convert Date from "mm/dd/yyyy"
    Time = as.character(Time),  # Ensure it's character before paste
    Start_Time = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S", tz = "US/Hawaii"),
    GPS_Start = as.POSIXct(paste(Date, GPS_Start), format = "%Y-%m-%d %H:%M:%S", tz = "US/Hawaii"),
    GPS_End   = as.POSIXct(paste(Date, GPS_End),   format = "%Y-%m-%d %H:%M:%S", tz = "US/Hawaii")
  ) %>%
  arrange(Start_Time) %>%
  mutate(
    Start_Time = if_else(row_number() == 1, GPS_Start, Start_Time),
    End_Time = lead(Start_Time, default = max(GPS_End, na.rm = TRUE)) - seconds(1),
    End_Time = if_else(row_number() == n(), GPS_End, End_Time),
    End_Time = pmax(Start_Time, End_Time)
  )

# Load and combine track data
track_files <- list(
  Group1 = "DMG-OAHU-GROUP1-041724.csv",
  Group2 = "DMG-OAHU-GROUP2-4.17.2024.csv",
  Group3 = "DMG-OAHU-GROUP3-041724.csv",
  Group4 = "DMG-OAHU-GROUP4-041724.csv"
)

tracks <- bind_rows(lapply(names(track_files), function(g) {
  read.csv(track_files[[g]]) %>%
    mutate(Group = g)
}))

# Clean track data
tracks <- tracks %>%
  rename(Organization = Group) %>%
  separate(time, into = c("Date", "Time2"), sep = " ", convert = TRUE, extra = "merge") %>%
  mutate(date = ymd(Date),
         datetime_utc = as_datetime(paste(Date, Time2)),
         Time = as_datetime(datetime_utc, tz = 'US/Hawaii')) %>%
  select(Latitude, Longitude, Organization, Date, Time)

# Filter tracks by window of time that dmg data was collected
start_time <- min(dmg$Start_Time)
end_time <- max(dmg$End_Time)
tracks <- tracks %>%
  filter(Time >= start_time, Time <= end_time)

# Use data.table for fast interval matching
setDT(dmg)
setDT(tracks)

# Assign foverlaps columns
dmg[, `:=`(start = Start_Time, end = End_Time)]
tracks[, `:=`(start = Time, end = Time)]

# Drop NA intervals
dmg <- dmg[!is.na(start) & !is.na(end)]

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
    dmg[Organization == g, .(start = Start_Time, end = End_Time, Organization, Damage_Score)],
    type = "within"
  )
}))

# Perform interval join
tracks <- tracks_matched[, .(Latitude, Longitude, Organization, Date, Time, Damage_Score)]

# Assign matched damage scores
tracks$Damage_Score <- tracks_matched$Damage_Score


tracks$Damage_Score = as.numeric(tracks$Damage_Score)
tracks$Damage_Score[is.na(tracks$Damage_Score)] <- NA


##### Plot tracks 3 different ways.
library(viridis) 

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