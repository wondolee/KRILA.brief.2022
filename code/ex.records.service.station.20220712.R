rm(list=ls())
setwd("d:/WORKSPACE/GIT/KRILA.brief.2022/")
data.path<-"d:/WORKSPACE/GIT/KRILA.brief.2022/data/ex/"
filenames<-dir(data.path,pattern="*.CSV")

Sys.setlocale("LC_ALL","Korean")
require(stringr)
tbl.data<-list()
for (i in 1:length(filenames)){
  tbl.data[[i]]<-read.csv(paste0(data.path,filenames[[i]]),encoding="utf=8")
  tbl.data[[i]]$date<-substr(filenames[[i]],8,15)
}

tbl.data<-plyr::ldply(tbl.data,data.frame)
colnames(tbl.data)<-c("sta.nm","v.type","hh","dsrc.flows","dsrc.sta.users","per.sta.users","link.flows",
                      "num.v.use.sta","dur.v.use.sta","date")
freq.sta<-plyr::count(tbl.data$sta.nm)
freq.sta<-read.csv("d:/WORKSPACE/GIT/KRILA.brief.2022/data/sta.lists.csv")

require(dplyr)
require(lubridate)
tbl.data<-left_join(tbl.data,freq.sta,by="sta.nm")
tbl.data$date<-ymd(tbl.data$date)
tbl.data$date.time<-paste0(tbl.data$date," ",tbl.data$hh,":00:00")
tbl.data$date.time<-as.POSIXct(tbl.data$date.time)
tbl.data$day<-wday(tbl.data$date,label=FALSE)
tbl.data$highpass.ic[is.na(tbl.data$highpass.ic)]<-0
tbl.data$highpass.ic<-as.factor(tbl.data$highpass.ic)
tbl.data$hh[tbl.data$hh==24]<-0
tbl.data$hh<-paste0(tbl.data$hh,"h")
tbl.data$hh<-as.factor(tbl.data$hh)
tbl.data$hh<-ordered(tbl.data$hh,levels=c("0h","1h","2h","3h","4h","5h","6h","7h","8h","9h","10h","11h","12h",
                                          "13h","14h","15h","16h","17h","18h","19h","20h","21h","22h","23h"))
tbl.data$v.type<-as.factor(tbl.data$v.type)
levels(tbl.data$v.type)<-c("car","bus","lorry")
colnames(tbl.data)[2]<-c("Vehicle.types")

require(ggplot2)
require(ggplot2) 
require(showtext)
require(ggrepel)
require(RColorBrewer)
require(scales)
require(tseries)
require(forecast)
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()

plot.users<-ggplot(tbl.data,aes(x=hh,y=num.v.use.sta,fill=Vehicle.types))+
  geom_bar(stat='identity',position="stack",width=0.75)+
  geom_smooth(aes(colour=Vehicle.types),method="loess",span=0.8,se=TRUE)+
  #facet_grid(~highpass.ic)+
    theme_minimal()+
    labs(x="Hour of the day",y="Number of mortorway service area users")+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.key.size = unit(0.2, 'cm'),
          legend.key.height = unit(0.2, 'cm'),
          legend.key.width = unit(0.2, 'cm'),
          legend.title=element_text(size=rel(1.5),face="bold",family="notosanskr",colour="black"),
          legend.text=element_text(size=rel(1),family="notosanskr",colour="black"),
          legend.position="top",
          legend.justification = c("right", "top"),
          axis.title=element_text(size=rel(2),face="bold",family="notosanskr",colour="black"),
          axis.text.x = element_text(size=rel(1.5),family="notosanskr",colour="black",vjust=rel(5)),
          axis.text.y = element_text(size=rel(1.5),face="bold",family="notosanskr",colour="black"))

file.name=paste0(path,"/output/gender.",urn.id,".2021.png")
ggsave(basic,"png",filename=file.name,scale=1,width=120,height=60,units=c("mm"),dpi=300,bg="white")

plot.users<-ggplot(tbl.data,aes(x=hh,y=mㅡper.sta.users,fill=Vehicle.types))+
  geom_bar(stat='identity',position="stack",width=0.75)+
  geom_smooth(aes(colour=Vehicle.types),method="loess",span=0.8,se=TRUE)+
  #facet_grid(~highpass.ic)+
  theme_minimal()+
  labs(x="Hour of the day",y="Number of mortorway service area users")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.2, 'cm'),
        legend.title=element_text(size=rel(1.5),face="bold",family="notosanskr",colour="black"),
        legend.text=element_text(size=rel(1),family="notosanskr",colour="black"),
        legend.position="top",
        legend.justification = c("right", "top"),
        axis.title=element_text(size=rel(2),face="bold",family="notosanskr",colour="black"),
        axis.text.x = element_text(size=rel(1.5),family="notosanskr",colour="black",vjust=rel(5)),
        axis.text.y = element_text(size=rel(1.5),face="bold",family="notosanskr",colour="black"))
