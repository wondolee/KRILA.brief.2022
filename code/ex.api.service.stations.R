setwd("d:/WORKSPACE/GIT/KRILA.brief.2022/code/")
rm(list=ls())
require(httr)
require(jsonlite)

ex.api.key<-"6053949007"
ex.api<-"http://data.ex.co.kr/openapi/business/conveniServiceArea"
ex.url<-paste0(ex.api,"?key=",ex.api.key,"&type=json")

test<-fromJSON(ex.url)
test$count #203 service stations

#download lists of service stations by pages
raw.data.01<-fromJSON(paste0(ex.url,"&numOfRows=100&pageNo=1"))
raw.data.02<-fromJSON(paste0(ex.url,"&numOfRows=100&pageNo=2"))
raw.data.03<-fromJSON(paste0(ex.url,"&numOfRows=100&pageNo=3"))

data.01<-as.data.frame(raw.data.01$list)
data.02<-as.data.frame(raw.data.02$list)
data.03<-as.data.frame(raw.data.03$list)
data<-rbind(data.01,data.02,data.03)

require(dplyr)
data<-select(data,-c(pageNo,numOfRows))

write.csv(data,"tbl.service.stations.csv")

addresses<-data$svarAddr
addresses<-data$serviceAreaName

vworld.api.key<-"B171760D-757D-3817-A49B-F2C8CBCB4711"
vworld.api<-"http://api.vworld.kr/req/address?service=address&request=getCoord"

require(rvest)
require(XML)

geocoding.data<-list()
fin=list()
for(i in 1:nrow(data)){
  vworld.url<-paste0(vworld.api,"&version=2.0&crs=epsg:4326&address=",addresses[i],
                   "&refine=true&simple=false&format=xml&type=road","&key=",vworld.api.key)
  geocoding.data[[i]]<-xmlParse(vworld.url,useInternalNodes = TRUE,encoding="utf-8")
  rootNode=xmlRoot(geocoding.data[[i]])
  test=xmlSApply(rootNode,xmlValue)
  test_dt=data.table(input=test[3],
                     refined=test[4],
                     result=test[5])
  fin[[i]]<-test_dt
}

fin<-rbindlist(fin)