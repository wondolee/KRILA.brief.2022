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
tbl.data$day<-wday(tbl.data$date,label=TRUE)
tbl.data$highpass.ic[is.na(tbl.data$highpass.ic)]<-0
tbl.data$highpass.ic<-as.factor(tbl.data$highpass.ic)
tbl.data$hh[tbl.data$hh==24]<-0
tbl.data$hh<-paste0(tbl.data$hh,"시")
tbl.data$hh<-as.factor(tbl.data$hh)
tbl.data$hh<-ordered(tbl.data$hh,levels=c("0시","1시","2시","3시","4시","5시","6시","7시","8시","9시","10시","11시","12시",
                                          "13시","14시","15시","16시","17시","18시","19시","20시","21시","22시","23시"))
tbl.data$v.type<-as.factor(tbl.data$v.type)
levels(tbl.data$v.type)<-c("승용차", "버스", "화물차")
colnames(tbl.data)[2]<-c("Vehicle.types")

require(ggplot2)
require(showtext)
require(ggrepel)
require(RColorBrewer)
require(scales)
require(tseries)
require(forecast)
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()

plot.users<-ggplot(tbl.data,aes(x=hh,y=num.v.use.sta/3,fill=Vehicle.types))+
  geom_bar(stat='identity',position="stack",width=0.75)+
  #geom_smooth(aes(colour=Vehicle.types),method="loess",span=0.8,se=TRUE)+
  #facet_grid(~highpass.ic)+
    theme_minimal()+
    labs(x="시간대별",y="(평균) 휴게소 총 이용 차량수")+
  scale_fill_discrete(name = "차종")+
  scale_y_continuous(labels=comma)+
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
ggsave(plot.users,"png",filename="num.of.service.users.png",scale=1,width=120,height=80,units=c("mm"),dpi=300,bg="white")

mean.per.tbl.data<-tbl.data %>% dplyr::group_by (Vehicle.types,hh,day) %>% 
  dplyr::summarise(per.sta.users=mean(per.sta.users)) %>% ungroup()
mean.per.tbl.data$per.sta.users<-mean.per.tbl.data$per.sta.users/100

plot.per.users<-ggplot(mean.per.tbl.data,aes(x=hh,y=per.sta.users,fill=Vehicle.types))+
  geom_bar(stat='identity',position="dodge",width=0.75)+
  #geom_smooth(aes(colour=Vehicle.types),method="loess",span=0.8,se=TRUE)+
  #facet_wrap(~day)+
  theme_minimal()+
  labs(x="시간대별",y="(평균) 휴게소 이용비율(%)")+
  scale_fill_discrete(name = "차종")+
  scale_y_continuous(labels=scales::percent_format(accuracy = 5L,suffix="%"))+
  theme(strip.text = element_text(size=rel(3),family="notosanskr",colour="black",face="bold"),
        panel.grid.major.x = element_blank(),
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
        axis.text.x = element_text(size=rel(1.2),family="notosanskr",colour="black",vjust=rel(10)),
        axis.text.y = element_text(size=rel(1.5),face="bold",family="notosanskr",colour="black"))
ggsave(plot.per.users,"png",filename="per.of.service.users.dodge.png",scale=1,width=150,height=80,units=c("mm"),dpi=300,bg="white")

mean.dur.tbl.data<-tbl.data %>% dplyr::group_by (Vehicle.types,hh,day) %>% 
  dplyr::summarise(dur.v.use.sta=mean(dur.v.use.sta)) %>% ungroup()
mean.dur.tbl.data$dur.v.use.sta<-mean.dur.tbl.data$dur.v.use.sta/60

plot.dur.users<-ggplot(mean.dur.tbl.data,aes(x=hh,y=dur.v.use.sta,fill=Vehicle.types))+
  geom_bar(stat='identity',position="dodge",width=0.75)+
  #geom_smooth(aes(colour=Vehicle.types),method="loess",span=0.8,se=TRUE)+
  #facet_wrap(~day)+
  theme_minimal()+
  labs(x="시간대별",y="(평균) 휴게소 체류시간(분)")+
  scale_fill_discrete(name = "차종")+
  scale_y_continuous(labels=scales::label_number(suffix="분"))+
  theme(strip.text = element_text(size=rel(3),family="notosanskr",colour="black",face="bold"),
        panel.grid.major.x = element_blank(),
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
        axis.text.x = element_text(size=rel(1.2),family="notosanskr",colour="black",vjust=rel(10)),
        axis.text.y = element_text(size=rel(1.5),face="bold",family="notosanskr",colour="black"))
ggsave(plot.dur.users,"png",filename="dur.of.service.users.dodge.png",scale=1,width=150,height=80,units=c("mm"),dpi=300,bg="white")

tbl.mean.dur.data<-tbl.data %>% dplyr::group_by (sta.nm,Vehicle.types,hh,day) %>% 
  dplyr::summarise(dur.v.use.sta=mean(dur.v.use.sta,na.rm=TRUE)/60) %>% ungroup()

tbl.mean.dur.data$rank <-percent_rank(tbl.mean.dur.data$dur.v.use.sta)
tbl.mean.dur.data<-tbl.mean.dur.data %>% group_by (Vehicle.types,day) %>% mutate(rank=rank(dur.v.use.sta))

require(ggmap)
register_google(key='자기 인증키')
map <- get_map(location='south korea', zoom=7, maptype='roadmap', color='bw')
