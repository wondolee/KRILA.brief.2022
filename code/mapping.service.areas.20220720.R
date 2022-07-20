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

l.motorway<-st_read((paste0(data.path,"simple.sigungu.admin.geojson")))
l.motorway<-st_transform(l.motorway,5179)

require(dplyr)
p.service<-p.service[-grep("졸음",p.service$serviceAre_1)]
