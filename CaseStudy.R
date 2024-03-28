install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
data_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
data_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
colnames(data_2019)
colnames(data_2020)
(data_2019 <- rename(data_2019,ride_id = trip_id,
                     rideable_type = bikeid,
                     started_at = start_time,
                     ended_at = end_time,
                     start_station_name = from_station_name,
                     start_station_id = from_station_id,
                     end_station_name = to_station_name,
                     end_station_id = to_station_id,
                     member_casual = usertype ))
data_2019 <-  mutate(data_2019, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type))
str(data_2019)
all_trips <- bind_rows(data_2019,data_2020)
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,tripduration))
colnames(all_trips)

table(all_trips$member_casual)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
table(all_trips$member_casual)

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

colnames(all_trips)

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

all_trips_v2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length),.groups = "drop") %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

