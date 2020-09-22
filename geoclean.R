library(sf)
library(ggplot2)
library(dplyr)
locations=st_read("./data/GoTRelease/Locations.shp")
geo_loc = locations %>% select(type,size,name)
lakes=st_read("./data/GoTRelease/Lakes.shp")
temp = lakes %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Lake") %>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
conts=st_read("./data/GoTRelease/Continents.shp")
temp = conts %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Continent")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
land=st_read("./data/GoTRelease/Land.shp")
wall=st_read("./data/GoTRelease/Wall.shp")
temp = wall %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Wall")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
islands=st_read("./data/GoTRelease/Islands.shp")
temp =islands %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Island")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
kingdoms=st_read("./data/GoTRelease/Political.shp")
temp = kingdoms %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Kingdom")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
landscapes=st_read("./data/GoTRelease/Landscape.shp")
temp = landscapes %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Landscape")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
roads=st_read("./data/GoTRelease/Roads.shp")
temp = roads %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Roads")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
rivers=st_read("./data/GoTRelease/Rivers.shp")
temp = rivers %>% select(name) %>% st_centroid() %>% mutate(size=0,type="River")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)
regions=st_read("./data/GoTRelease/Regions.shp")
temp = regions %>% select(name) %>% st_centroid() %>% mutate(size=0,type="Region")%>% filter(!is.na(name))
geo_loc = geo_loc %>% rbind(temp)




library(readr)

scenes = read_csv("./data/scenes.csv")

subLoc = scenes %>% count(subLocation,wt=duration) %>% arrange(desc(n))
Loc = scenes %>% count(location,wt=duration) %>% arrange(desc(n))

Locf = Loc %>% mutate(geoloc=location) %>%
  mutate(geoloc=case_when(geoloc=="The Crownlands" ~"Crownsland",
                          geoloc=="The Riverlands" ~"Riverlands",
                          geoloc=="North of the Wall"~"The Haunted Forest",
                          geoloc=="The Stormlands"~"Stormlands",
                          geoloc=="The Summer Sea"~"Summer Sea",
                   TRUE~geoloc,)) %>%
  left_join(geo_loc,by=c("geoloc"="name")) %>% 
  select(location,type,geometry) %>% 
  st_as_sf() %>%
  filter(!duplicated(location))


write_sf(Locf,"./data/GoTRelease/ScenesLocations.shp")

Locf = subLoc %>% mutate(geoloc=subLocation) %>%
  mutate(geoloc=gsub("Outside the","",geoloc)) %>%
  mutate(geoloc=gsub("Outside ","",geoloc)) %>%
  mutate(geoloc=gsub("To ","",geoloc)) %>%
  mutate(geoloc=gsub("South to ","",geoloc)) %>%
  mutate(geoloc=gsub("East to ","",geoloc)) %>%
  mutate(geoloc=case_when(geoloc=="The Crownlands" ~"Crownsland",
                          geoloc=="The Riverlands" ~"Riverlands",
                          geoloc=="North of the Wall"~"The Haunted Forest",
                          geoloc=="The Stormlands"~"Stormlands",
                          geoloc=="The Summer Sea"~"Summer Sea",
                          TRUE~geoloc,)) %>%
  left_join(geo_loc,by=c("geoloc"="name")) %>% 
  select(subLocation,geoloc,type,geometry) %>% 
  st_as_sf() %>%
  filter(!duplicated(subLocation))

Locf

gg=Locf %>% left_join(Loc)
colforest="#9bc19b"
colriver="#7ec9dc"
colland="ivory"
  
ggplot()+geom_sf(data=land,fill=colland,col="ivory3")+
  geom_sf(data=islands,fill=colland,col="ivory3")+
  geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)+
  geom_sf(data=rivers,col=colriver)+
  geom_sf(data=lakes,col=colriver,fill=colriver)+
  geom_sf(data=wall,col="black",size=1)+
  geom_sf(data=gg,aes(size=n),col="red")+
  geom_sf_text(data=gg %>% filter(location!="The crownlands",!grepl("Sea",location)),aes(label=location),size=2.5,vjust=-0.5)+
  theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
  theme(panel.background = element_rect(fill = "azure",color=NA)) +
  labs(title = "GoT",caption = "Etiennne Côme, 2020",x="",y="")


nolabels= c("The Westerlands","The Stormlands")
ggplot()+geom_sf(data=land,fill=colland,col="ivory3")+
  geom_sf(data=islands,fill=colland,col="ivory3")+
  geom_sf(data=wall,col="black",size=1)+
  geom_sf(data=gg,aes(size=n/3600),col="#5d361a")+
  geom_sf_text(data=gg %>% filter(!(location %in% nolabels),!grepl("Sea",location)),aes(label=location),size=2.7,vjust=-0.5)+
  theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
  theme(panel.background = element_rect(fill = "azure",color=NA),legend.position="bottom") +
  labs(title = "Les lieux de GoT",caption = "@comeetie, 2020",x="",y="")+
  guides(size = guide_legend(title = "Durée (heures)"))


