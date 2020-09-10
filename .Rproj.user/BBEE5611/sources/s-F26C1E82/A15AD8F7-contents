
library(dplyr)
library(purrr)

data.raw = jsonlite::fromJSON("episodes.json")
gender= jsonlite::fromJSON("characters-gender-all.json")
characters.list = jsonlite::fromJSON("characters.json")
characters_male = data.frame(name=gender$male,sex="male")
characters_female = data.frame(name=gender$female,sex="female")
characters.meta = data.frame(name=characters.list$characters$characterName,
                             house=sapply(characters.list$characters$houseName,function(houses){ifelse(length(houses)==0,NA,houses[1])}),
                             killedBy=sapply(characters.list$characters$killedBy,function(killers){ifelse(length(killers)==0,NA,paste(killers,collapse = "/"))}),
                             image=characters.list$characters$characterImageThumb
                             )
  
  
  
characters =  bind_rows(characters_male,characters_female) %>% left_join(characters.meta)

library(readr)
write_csv(characters,"data/characters.csv")

scenes.list = map2(data.raw$episodes$scenes,1:length(data.raw$episodes$scenes),function(sc,ep_id){sc$episodeId=ep_id;sc})
scenes = do.call(bind_rows,scenes.list) %>% 
  mutate(duration=as.POSIXct(scenes$sceneEnd,format="%H:%M:%S")-as.POSIXct(scenes$sceneStart,format="%H:%M:%S")) %>%
  mutate(nbc = sapply(scenes$character,nrow)) %>%
  filter(nbc>0) %>%
  mutate(sceneId=1:n())
scenes$nbdeath = sapply(scenes$characters,function(ch){ifelse("alive" %in% colnames(ch),sum(!ch$alive,na.rm = TRUE),0)})

scenes = scenes %>% select(sceneStart,sceneEnd,location,subLocation,episodeId,duration,nbc,sceneId,nbdeath)
write_csv(scenes,"scenes.csv")

episodes = data.raw$episodes %>% select(episodeTitle,episodeNum,seasonNum) %>% mutate(episodeId=1:n())
episodes
write_csv(episodes,"episodes.csv")



appearances.list = map2(scenes$characters,1:length(scenes$characters),function(sc,sc_id){data.frame(name=sc$name,sceneId=sc_id)})
appearances = do.call(bind_rows,appearances.list)
write_csv(appearances,"appearances.csv")

library(sf)
library(tidyr)
land = st_read("./lands-of-ice-and-fire.json","land")
places = st_read("./lands-of-ice-and-fire.json","places")
countries = st_read("./lands-of-ice-and-fire.json","countries")


loc=scenes %>% count(subLocation,wt=duration) 
pl=st_read("./data/GoTRelease/Locations.shp") %>% select(name,type,size) %>% filter(!is.na(name))
island = st_read("./data/GoTRelease/Islands.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Island",size=0)%>% filter(!is.na(name))
land=st_read("data/GoTRelease/Landscape.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Landscape",size=0)%>% filter(!is.na(name))
pol=st_read("data/GoTRelease/Political.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Political",size=0)%>% filter(!is.na(name))
ri=st_read("data/GoTRelease/Rivers.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Rivers",size=0)%>% filter(!is.na(name))
re=st_read("data/GoTRelease/Regions.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Region",size=0)%>% filter(!is.na(name))
wall=st_read("data/GoTRelease/Wall.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Wall",size=0)%>% filter(!is.na(name))
ro=st_read("data/GoTRelease/Roads.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Roads",size=0)%>% filter(!is.na(name))
la=st_read("data/GoTRelease/Lakes.shp") %>% st_centroid() %>% select(name) %>% mutate(type="Lake",size=0)%>% filter(!is.na(name))
geoloc = bind_rows(pl,island,land,pol,ri,re,wall,ro,la)
plloc=  loc %>% mutate(loc_name=gsub("South to","",gsub("Outside ","",gsub("To ","",subLocation)))) %>% 
    mutate(loc_name=case_when(loc_name=="Casterly Rock" ~ "Lannisport",
                              loc_name=="The Gift" ~ "Bran's Gift",
                              loc_name=="The Kingsroad"~"Kingsroad",
                              loc_name=="North to the Wall"~"Land of Always Winter",
                              TRUE ~ loc_name)) %>% left_join(geoloc,by=c("loc_name"="name")) %>% arrange(desc(n))

View(plloc)
sum(scenes$nbdeath)

# plus grand meurtrier
table(characters$killedBy)


# ? La duree de la scene la plus longue et le titre de l'episode
scenes %>% arrange(desc(duration)) %>% head(1)


# les personnages de la scenes la plus longue
scenes %>% arrange(desc(duration)) %>% head(1) %>% left_join(appearances)

# le lieux le plus visités en nombre de scenes
scenes %>% group_by(location) %>% summarise(nbsc = n()) %>% arrange(desc(nbsc))

# le nombre de scenes a port-real
scenes %>% group_by(location) %>% summarise(nbsc = n()) %>% filter(location=="Port Real")

# le lieux ou le plus de personage meurts ?
scenes %>% group_by(subLocation) %>% summarise(nbd=sum(nbdeath)) %>% arrange(desc(nbd))

# ? L'episode ou Jon Snow est le plus longtemps visible
appearances %>% left_join(scenes) %>% left_join(episodes) %>% group_by(name,episodeId,episodeTitle) %>% summarise(screenTime=sum(duration)) %>% 
  filter(name=="Jon Snow") %>%
  arrange(desc(screenTime)) %>% head(1)

# ? combien de characters passent plus de 20 minutes à l'ecran sur l'ensemble des saisons
appearances %>% left_join(scenes) %>% left_join(episodes) %>% group_by(name) %>% summarise(screenTime=sum(duration)) %>% 
  filter(screenTime>20*60) %>% nrow()




# les deux caractères qui passent le plus de scenes ensembles ?
appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% filter(name.x!=name.y) %>% group_by(name.x,name.y) %>% summarise(nbs=n()) %>% arrange(desc(nbs))
# les deux caractères qui passent le plus longtemps ensembles ?
appearances %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% filter(name.x!=name.y) %>% 
  left_join(scenes %>% select(sceneId,duration)) %>%
  group_by(name.x,name.y) %>% summarise(commonTime=sum(duration)) %>% arrange(desc(commonTime))

# gender balance of screen time per episode?

appearances %>% left_join(characters) %>% left_join(scenes) %>% left_join(episodes) %>% group_by(sex,episodeId,episodeNum,episodeTitle) %>% 
  summarise(duration = sum(duration)) %>% arrange(episodeId)

# temps / episodes / character
screenTimePerSeasons = appearances %>% left_join(scenes) %>% left_join(episodes) %>% group_by(name,seasonNum) %>% summarise(screenTime=sum(duration)) %>% arrange(desc(screenTime)) 
screenTimeTotal = screenTimePerSeasons %>% group_by(name) %>% summarise(screenTimeTotal=sum(screenTime))
mainCharacters = screenTimeTotal %>% filter(screenTimeTotal>60*60) %>% arrange(screenTimeTotal) %>% mutate(nameF=factor(name,levels = name))


# temps / lieux / character
screenTimePerLocations = appearances %>% left_join(scenes)  %>% group_by(name,subLocation) %>% summarise(screenTime=sum(duration)) %>% arrange(desc(screenTime)) 


library(ggplot2)
ggplot(screenTimePerEpisodes %>% left_join(mainCharacters) %>% filter(!is.na(nameF)))+geom_bar(aes(x=nameF,y=screenTime,fill=as.factor(seasonNum)),stat="identity")+coord_flip()

