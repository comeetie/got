
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
locations=st_read("./data/GoTRelease/Locations.shp",crs=4326)
lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326)
conts=st_read("./data/GoTRelease/Continents.shp",crs=4326)
land=st_read("./data/GoTRelease/Land.shp",crs=4326)
wall=st_read("./data/GoTRelease/Wall.shp",crs=4326)
islands=st_read("./data/GoTRelease/Islands.shp",crs=4326)
kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326)
landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326)
roads=st_read("./data/GoTRelease/Roads.shp",crs=4326)
rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326)



plot(kingdoms %>% select(ClaimedBy) %>% st_geometry())

locations

plot(locations %>% filter(type=="Castle") %>% st_geometry(),add=TRUE)

CastlesPerKingdoms= st_join(locations,kingdoms) %>% 
  filter(type=="Castle") %>% 
  st_drop_geometry() %>% 
  group_by(ClaimedBy) %>% 
  summarize(n=n())



st_covers(kingdoms, locations %>% filter(type=="Kingdoms"))


plot(landscapes %>% select(type) %>% filter(type=="forest") %>% st_geometry(),col="green") 
plot(kingdoms %>% st_geometry(),add=TRUE)

forest_kingdoms = st_intersection(landscapes %>% select(type) %>% filter(type=="forest"),kingdoms)

forest_kingdoms %>% 
  select(ClaimedBy) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_drop_geometry() %>%
  group_by(ClaimedBy) %>%
  summarize(area = sum(area)) %>% 
  arrange(desc(area))


df = data.frame(time=1:20)
df %>% mutate(previous_time = lag(time))

appearences = readr::read_csv("./data/appearances.csv")
scenes = readr::read_csv("./data/scenes.csv")

main_char = c("Jon Snow", "Tyrion Lannister", "Daenerys Targaryen", "Sansa Stark", "Cersei Lannister", "Arya Stark") 



char_app = appearences %>% 
  filter(name %in% main_char) %>% 
  left_join(scenes) %>% 
  select(name,location,sceneStart)



scenes_loc=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)


char_app_loc = char_app %>% group_by(name) %>% 
  mutate(previous_location=lag(location)) %>% 
  arrange(name,sceneStart) %>% 
  left_join(scenes_loc) %>% 
  left_join(scenes_loc,by=c("previous_location"="location"))

char_app_dist = char_app_loc %>% filter(location!=previous_location)  %>% 
  mutate(distance = st_distance(geometry.x,geometry.y,by_element = TRUE)) %>%
  select(name,distance) %>% group_by(name) %>% summarize(distance=sum(distance))

char_app_dist

# a different approach

char_app = appearences %>% 
  filter(name %in% main_char) %>% 
  left_join(scenes) %>% 
  select(name,location,sceneStart)

char_app %>% left_join(scenes_loc) %>% st_as_sf() %>% 
  group_by(name) %>% 
  summarize(n=n(),do_union=FALSE) %>% 
  st_cast("LINESTRING") %>% 
  mutate(distance = st_length(geometry)) %>% 
  select(name,n,distance) %>% st_drop_geometry()




