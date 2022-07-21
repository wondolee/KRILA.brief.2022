rm(list=ls())
setwd("d:/WORKSPACE/GIT/KRILA.brief.2022/code")
data.path<-"d:/WORKSPACE/GIT/KRILA.brief.2022/data/"
Sys.setlocale(category="LC_ALL", locale = "Korean")

##service areas with admin boundary and motorway
require(sf)
p.service<-st_read(paste0(data.path,"service.station.geojson"),options="ENCODING=EUC-KR")
p.service<-st_transform(p.service,5179)

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> parent of bd9ce92 (Final)
a.admin.gu<-st_read(paste0(data.path,"admin.gu.geojson"),options="ENCODING=EUC-KR")
a.admin.gu<-st_transform(a.admin.gu,5179)

a.admin.si<-st_read(paste0(data.path,"admin.si.geojson"),options="ENCODING=EUC-KR")
a.admin.si<-st_transform(a.admin.si,5179)

p.highpass.ic<-st_read((paste0(data.path,"highpass.ic.geojson")),options="ENCODING=EUC-KR")
<<<<<<< HEAD
=======
a.admin.gu<-st_read(paste0(data.path,"simple.sigungu.admin.geojson"))
a.admin.gu<-st_transform(a.admin.gu,5179)

p.highpass.ic<-st_read((paste0(data.path,"highpass.ic.geojson")))
>>>>>>> parent of 2e0212e (Final update)
=======
>>>>>>> parent of bd9ce92 (Final)
p.highpass.ic<-st_transform(p.highpass.ic,5179)

p.rail.station<-st_read((paste0(data.path,"rail.station.geojson")),options="ENCODING=EUC-KR")
p.rail.station<-st_transform(p.rail.station,5179)

require(dplyr)
p.service<-p.service[-grep("졸음",p.service$serviceAre_1)]
require(rgeos)
near.rail.sta<-as.data.frame(gDistance(as(p.rail.station,"Spatial"), as(p.service,"Spatial"),byid=TRUE))
min.d <- as.data.frame(apply(near.rail.sta, 1, function(x) order(x, decreasing=F)[2]));colnames(min.d)<-"rail.sta.id"
p.service<-cbind(p.service,min.d)
p.rail.station$rail.sta.id<-rownames(p.rail.station)

service.sta.cen<-data.frame(as(p.service,"Spatial"))
service.sta.cen<-service.sta.cen[c("coords.x1","coords.x2")];colnames(service.sta.cen)<-c("from.x","from.y")

rail.sta.cen<-data.frame(as(p.rail.station,"Spatial"))
rail.sta.cen<-rail.sta.cen[c("rail.sta.id","coords.x1","coords.x2")];colnames(rail.sta.cen)<-c("rail.sta.id","to.x","to.y")
rail.sta.cen$rail.sta.id<-as.numeric(rail.sta.cen$rail.sta.id)

service.sta.cen<-cbind(service.sta.cen,min.d)
service.sta.cen<-left_join(service.sta.cen,rail.sta.cen,by=c("rail.sta.id"))
service.sta.cen<-service.sta.cen[c(-3)]

rows <- split(service.sta.cen, seq(nrow(service.sta.cen)))
lines <- lapply(rows, function(row) {
  lmat <- matrix(unlist(row[1:4]), ncol = 2, byrow = TRUE)
  st_linestring(lmat)
})
od.lines<-st_as_sf(st_sfc(lines,crs=5179),crs=5179)
od.lines$id<-rownames(od.lines);min.d$id<-rownames(min.d)
od.lines<-left_join(od.lines,min.d,by=c("id"))

require(stplanr)
require(osrm)
od.lines.lat<-st_transform(od.lines,4326)
od.lines.lat.coord=od_coords(od.lines.lat)
from=od.lines.lat.coord[, 1:2]
to=od.lines.lat.coord[, 3:4]
car.trans.network = route(from, to, route_fun = route_osrm, osrm.profile = "car")
car.trans.network<-st_transform(car.trans.network,5179)


require(ggplot2)
require(RColorBrewer)
require(classInt)
require(maptools)
require(ggrepel)
require(scales)
require(rgdal)
require(ggspatial)
require(showtext)
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()

map.service<-ggplot()+ geom_sf(data=p.rail.station,aes(colour="고속철도 역"),size=rel(0.3),show.legend="point")+
  geom_sf(data=a.admin.gu,fill=NA,color="grey40",size=rel(0.2),show.legend=FALSE)+
  geom_sf(data=a.admin.si,fill=NA,color="grey20",size=rel(0.5),show.legend=FALSE)+
  geom_sf(data=car.trans.network,fill=NA,aes(colour="환승구간"),size=rel(0.3),show.legend="point")+
  geom_sf(data=p.service,aes(colour="고속도로 휴게소"),size=rel(0.5),show.legend="line")+
  geom_sf(data=p.highpass.ic,aes(colour="하이패스 IC 휴게소"),size=rel(0.5),show.legend = "point")+
  labs(col="주요 교통시설 입지")+
  scale_colour_manual(values=c("하이패스 IC 휴게소"="red","고속도로 휴게소"="blue","고속철도 역"="#C77CFF","환승구간"="green3"))+
  guides(colour=guide_legend(override.aes = list(size=4,shape=20)))+
  theme_minimal()+
  #geom_text_repel(data=a.admin.si,aes(label = CTP_KOR_NM,geometry= geometry), colour = "black")+
  theme(legend.key.size = unit(0.5, 'cm'),
        text = element_text(size = rel(5),family="notosanskr"),
        legend.position = "top",plot.title = element_text(size =rel(2),family="notosanskr",face = "bold"),
        legend.title=element_text(size=rel(6),family="notosanskr",face="bold"), legend.text=element_text(size=rel(4),family="notosanskr"))+
  annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_scale(location = "br", height = unit(0.3, "cm"))+
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(0.75, "cm"),
                         width = unit(0.75, "cm"))
<<<<<<< HEAD
ggsave(map.service,"png",filename="location.map.png",scale=1,width=150,height=100,units=c("mm"),dpi=300,bg="white")
=======

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
>>>>>>> parent of 2e0212e (Final update)
=======
ggsave(map.service,"png",filename="location.map.png",scale=1,width=150,height=100,units=c("mm"),dpi=300,bg="white")
>>>>>>> parent of bd9ce92 (Final)
