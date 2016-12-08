library(tidyverse)
library(ggplot2)
setwd("C:/Users/Para2x/Downloads")
sss<-readRDS("Farmdata2016-12-06.frm")
sss<-sss[[1]]
weather<-sss$Weather
###############################
######## update weather function
##########################################


update.weather<-function(){
  
  lastdate<-max(sss$Weather$date)+1
  
  farmstate<-"IA"
  ## for retrving up date 
  day1<-strftime(lastdate,'%d')
  month1<-strftime(lastdate,'%m')
  year1<-strftime(lastdate,'%Y')
  
  
  ####################################### ASOS
  ## for retrving up date 
  day2<-strftime(as.Date(Sys.time())-1 ,'%d')
  month2<-strftime(as.Date(Sys.time())-1 ,'%m')
  year2<-strftime(as.Date(Sys.time())-1 ,'%Y')
  

  add<-paste0("http://mesonet.agron.iastate.edu/cgi-bin/request/daily.py?network=",
              farmstate,"_ASOS&stations=",sss$wasos[[3]],"&year1=",year1,"&month1=",month1,"&day1=",day1,"&year2=",year2,"&month2=",month2,"&day2=",day2)
  
  ASOSweather<-read.csv2(add,sep = ",",stringsAsFactors = F)[,c(3:4,7,8,10:12)]
  ASOSweather$avg_temp_f<-(as.numeric(ASOSweather$min_temp_f)+as.numeric(ASOSweather$max_temp_f))/2
 
  ####################################COOP for its solar
  day2<-strftime(as.Date(Sys.time())-2 ,'%d')
  month2<-strftime(as.Date(Sys.time())-2 ,'%m')
  year2<-strftime(as.Date(Sys.time())-2 ,'%Y')
  
  add<-paste0("http://mesonet.agron.iastate.edu/cgi-bin/request/coop.py?network=",
              farmstate,"CLIMATE&stations=",sss$wcoop[[3]],"&year1=",year1,"&month1=",month1,"&day1=",day1,"&year2=",year2,"&month2=",month2,"&day2=",day2,"&vars%5B%5D=apsim&what=view&delim=comma&gis=no&scenario_year=2013")
  coopweather<-read.table(add,sep = "",skip = 10,header = T)
  names(coopweather)<-c("year","day","radn","maxt","mint","rain")
  
  ################################# mergiing datasets
  if(nrow(ASOSweather)==nrow(coopweather)){
    ASOSweather$avg_wind_speed_kts<-as.numeric(as.character(ASOSweather$avg_wind_speed_kts))*1.15
    weatherup<-cbind(coopweather[1:3],ASOSweather)
    weatherup<-weatherup[,c(1:5,11,10,8,9,7,6)]
    weatherup<-as.data.frame(apply(weatherup, 2, as.numeric))
    weatherup$date<-as.Date(strptime(paste(as.character(weatherup$year), weatherup$day), format="%Y %j") ) ##
    ### putting back in obj
    sss$Weather<-rbind(sss$Weather,weatherup)
    Msgcenter("Weather data was updated successfully.")
    
  }else{
    Msgcenter("Something went wrong in updating the weather.")
  }
  
}


#####################################
############ Plots weather and soil
#################################################
monthslabs<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
              "Sep","Oct","Nov","Dec")
dd<-weather%>%mutate(month=format(date,format="%b"),Year=as.factor(year))%>%group_by(month,Year)%>%
  summarise(maxtavg=mean(max_temp_f),sdtmax=sd(max_temp_f),mintavg=mean(min_temp_f),sdtmin=sd(min_temp_f),SumP=sum(precip_in))

dd%>%
  ggplot(aes(x=month,y=SumP))+
  geom_bar(aes(fill=Year),color="black",stat = "identity")+
  theme_bw(base_size = 18)+
  scale_x_discrete(limits=monthslabs,labels=monthslabs)+
  scale_fill_brewer(palette = "Set1")+
  labs(y="Total rainfall (in)",x="Month")


dd%>%
  ggplot(aes(x=month))+
  geom_boxplot(aes(ymin=maxtavg-sdtmax,ymax=maxtavg+sdtmax,y=maxtavg),fill="firebrick2",
               color="black",lwd=1.02)+
  geom_boxplot(aes(ymin=mintavg-sdtmin,ymax=mintavg+sdtmin,y=mintavg),fill="deepskyblue2",
               size=1.02,color="black")+
  theme_bw(base_size = 18)+
  scale_x_discrete(limits=monthslabs,labels=monthslabs)+
  labs(x="Month",y="Temperature (F)")
#############################
curmonth<-format(Sys.time(),format="%B")
curyear<-format(Sys.time(),format="%Y")

dd2<-weather%>%filter(format(date,format="%B")==curmonth & year!=as.numeric(curyear))%>%mutate(Avgt=(min_temp_f+max_temp_f)/2)%>%group_by(day)%>%
          summarise(maxt=max(Avgt),Temperature=min(Avgt),maxp=max(precip_in),Precipitation=min(precip_in),pp=mean(precip_in),Avgt2=mean(Avgt))
dd2$day<-1:length(dd2$day)
  
dd3<-weather%>%filter(format(date,format="%B")==curmonth & year==as.numeric(curyear))%>%mutate(Avgt=(min_temp_f+max_temp_f)/2)%>%select(day,Avgt,year,precip_in)
if(nrow(dd3)>0){
dd3$day<-1:nrow(dd3)
dd3$Year<-as.factor(dd3$year)
}

###temperature
dd2%>%ggplot(aes(x=day))+
  geom_ribbon(aes(ymin = Temperature, ymax =maxt),color="black",fill="grey80",alpha=0.2,linetype=2) +
  geom_point(aes(y = Temperature,color=Temperature),size=3,show.legend = F) +
  geom_point(aes(y = maxt,color=maxt),size=3,show.legend = F) +
  # geom_line(aes(y = Avgt2,color=Avgt2),size=1.2)+
  geom_line(aes(y = Avgt,color=Avgt,linetype=Year),data = dd3,size=1.2)+
  geom_point(aes(y = Avgt,color=Avgt),data = dd3,size=3,show.legend = F)+
  scale_y_continuous(limits = c(min(dd2$Temperature),max(dd2$maxt)+10))+
  scale_x_continuous(breaks  = seq(1,32,2))+
  labs(x="Day",y="Temperature (F)")+
  geom_text(aes(x=5,y=max(maxt)+10),label =curmonth,size=10 )+
  scale_colour_gradient(low="dodgerblue",high = "firebrick1")+
  theme_bw(base_size = 18)+
  theme(legend.position=c(0.92,0.8),legend.background = element_rect(fill=alpha('white', 0.1)))



dd2%>%ggplot(aes(x=day))+
  geom_ribbon(aes(ymin = cumsum(Precipitation), ymax =cumsum(maxp)),color="black",fill="grey80",alpha=0.2,linetype=2) +
  geom_line(aes(y = cumsum(pp)),linetype=2,color="dodgerblue4",size=1.2)+
  geom_line(aes(y = cumsum(precip_in),linetype=Year),color="dodgerblue2",data = dd3,size=1.2)+
  geom_point(aes(y = cumsum(precip_in)),color="dodgerblue3",data = dd3,size=3,show.legend = F)+
  labs(x="Day",y="Cumulative Precipitation (in)")+
  geom_text(aes(x=3,y=max(cumsum(dd2$maxp))),label =curmonth,size=10 )+
  scale_x_continuous(breaks  = seq(1,32,2))+
  theme_bw(base_size = 18)+
  theme(legend.position=c(0.06,0.8),legend.background = element_rect(fill=alpha('white', 0.1)))
#########################################################
############################# Soil
##########################################################
library(d3heatmap)
Soil<-sss$Soil@data
col<-c("sandtotal_r","claytotal_r","om_r","AWC","ph1to1h2o_r","muareaacres","CSR")
heatmapdf<-sss$Soil@data[,c(col)]
row.names(heatmapdf)<-sss$Soil@data$mupolygonkey
names(heatmapdf)<-c("Sand","Clay","Organic Matter","AWC","Soil pH","Area (ac)","CSR")

d3heatmap(heatmapdf, scale = "column",show_grid=TRUE,yaxis_font_size = 16,
          colors = "RdYlGn",Colv=F,revC=T)




#Soil[,c("mupolygonkey",col)]%>%gather(Name,Value,-c(mupolygonkey))%>%
#ggplot(aes(x=as.factor(mupolygonkey),y=Value))+
#  geom_bar(aes(fill=Name),position="stack", stat="identity")+
#  theme_bw(base_size = 18)+
#  labs(y="Value",x="Polygonkey")+ 
#  scale_fill_brewer(palette="Set1")+
#  theme(legend.position="top", panel.background = element_blank(), 
#        axis.line = element_line(color='black'), panel.grid.minor = element_blank(),
#        axis.text.x=element_text(angle=90, hjust=1))


breaks<-as.data.frame(lapply(heatmapdf,function(x){
  x<-x[!is.na(x)]; hist(x,plot = F,breaks = seq(min(x,na.rm = T),max(x,na.rm = T), length.out=5))$breaks
}))

perc<-as.data.frame(lapply(heatmapdf,function(x){
  x<-x[!is.na(x)];  c(0,hist(x,plot = F,breaks = seq(min(x,na.rm = T),max(x,na.rm = T), length.out=5))$counts/sum(hist(x,plot = F,breaks = seq(min(x,na.rm = T),max(x,na.rm = T), length.out=5))$counts))
}))

p1<-perc%>%mutate(names=c("C1","D","E","F","G"))%>%gather(Param,Value,-c(names))%>%
  group_by(Param)%>%mutate(CValue=cumsum(Value))
p2<-breaks%>%mutate(names=c("C1","D","E","F","G"))%>%gather(Param,ValueL,-c(names))
total<-cbind(as.data.frame(p1),Label=p2[,3])



total%>%ggplot(aes(x=Param,group=names))+
  theme_bw(base_size = 18)+
  geom_bar(aes(fill=names,y=Value*100),position="stack", stat="identity",color="black",size=1.1)+
  #geom_text(aes(y=CValue*100,label=round(Label,1)),check_overlap = T,nudge_y=2.5,size=5.5 )+
  labs(y="Percentage in this class",x="")+ 
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_x_discrete(limits=c("AWC","Area..ac.","Clay","CSR","Organic.Matter","Sand","Soil.pH"))+
  theme(legend.position="top",panel.grid.major = element_line(colour = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1))

