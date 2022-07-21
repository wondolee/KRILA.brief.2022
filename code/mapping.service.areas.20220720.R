rm(list=ls())
setwd("d:/WORKSPACE/GIT/KRILA.brief.2022/code")
data.path<-"d:/WORKSPACE/GIT/KRILA.brief.2022/data/"

##service areas with admin boundary and motorway
require(sf)
p.service<-st_read(paste0(data.path,"service.station.geojson"))
p.service<-st_transform(p.service,5179)

a.admin.gu<-st_read(paste0(data.path,"simple.sigungu.admin.geojson"))
a.admin.gu<-st_transform(a.admin.gu,5179)

p.highpass.ic<-st_read((paste0(data.path,"highpass.ic.geojson")))
p.highpass.ic<-st_transform(p.highpass.ic,5179)

p.rail.station<-st_read((paste0(data.path,"rail.station.geojson")))
p.rail.station<-st_transform(p.rail.station,5179)

require(dplyr)
p.service<-p.service[-grep("졸음",p.service$serviceAre_1)]

require(leaflet)

map.service<-ggplot()+
  theme_minimal()+
  labs(x=NULL, y=NULL,caption = "자료: ex 고속도로 공공데이터 포털, KTDB")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption=element_text(hjust = rel(1),size=rel(1),family="notosanskr",colour="black"),
        legend.text=element_text(size=rel(1),family="notosanskr",colour="black"),
        legend.position="right",
        axis.title=element_text(size=rel(2),face="bold",family="notosanskr",colour="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  #geom_sf(data = l.motorway, fill="grey80", colour=NA) +
  geom_sf(data=p.service,fill="grey20",size=0.2)+
  geom_sf(data=p.highpass.ic,fill="red",size=0.5)+
  geom_sf(data=a.admin.gu,fill=NA,colour="black",size=0.5)

iconSet <- awesomeIconList(
  service = makeAwesomeIcon(
    icon = 'fa-duotone fa-utensils',
    library = 'fa',
    iconColor = 'gold',
  ),
  highpass = makeAwesomeIcon(
    icon = 'fa-thin fa-road-circle-check',
    library = 'ion',
    iconColor = '#000000',
    markerColor = 'blue',
    squareMarker = TRUE
  ),
  restaurant = makeAwesomeIcon(
    icon = 'cutlery',
    library = 'glyphicon',
    iconColor = 'rgb(192, 255, 0)',
    markerColor = 'darkpurple',
    spin = TRUE,
    squareMarker = FALSE
  )

leaflet() %>% addProviderTiles('CartoDB.Positron') %>%
  addPolygons(data=a.admin.gu,stroke = FALSE, color = "grey80", fillOpacity = 0.2) %>%
  addMarkers(data=p.service, icon=
  
               
               addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-07"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="7 June 2021 (Mon)") %>% 
  addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-08"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="8 June 2021 (Tue)") %>% 
  addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-09"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="9 June 2021 (Wed)") %>%
  addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-10"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="10 June 2021 (Thu)") %>%
  addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-11"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="11 June 2021 (Fri)") %>% 
  addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-12"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="12 June 2021 (Sat)") %>%
  addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-13"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="13 June 2021 (Sun)") %>% 
  addPolygons(data=subset(int.data.tracks.geohash,date=="2021-06-14"),fillColor = ~pal(NO2), fillOpacity = 0.1,
              weight = 0, opacity = 0.1,group="14 June 2021 (Mon)") %>%
  addLegend(data=int.data.tracks.geohash,pal = pal, values = ~NO2, opacity = 0.8, title = 'NO2 ppb', position = "bottomleft") %>%
  addLayersControl(
    overlayGroups = c("7 June 2021 (Mon)","8 June 2021 (Tue)","9 June 2021 (Wed)","10 June 2021 (Thu)",
                      "11 June 2021 (Fri)","12 June 2021 (Sat)","13 June 2021 (Sun)","14 June 2021 (Mon)"),
    options = layersControlOptions(collapsed = FALSE)
  )