library(SoilTemp)
library(BioCro)
#library(Rcpp)
library(lubridate)
library(tidyverse)
library(sirad)
setwd("C:/Users/Para2x/Dropbox/R projects/FarmAssist/TestCode/Tempmodel")
source('Soil_Temp_Func.R')
source('Weachsub.R')
###############################
setwd("C:/Users/Para2x/Downloads")
farm<-readRDS("Farmdata2016-12-21.frm")
farm<-farm[[1]]
weather10<-farm$Wnext10
weather<-farm$Weather
weather10$date<-strptime(paste(weather10$year, weather10$doy), format="%Y %j")
weather10$doy<-as.numeric(weather10$doy)


tryCatch({
  
  BCbc<-bcauto(farm$Centriod@coords[[2]],farm$Centriod@coords[[1]],weather$date,extraT=weather$radn,(weather$max_temp_f-32)*5/9,(weather$min_temp_f-32)*5/9,perce=0.8,tal =0.65 )
  
  weather10$solaR<-with(weather10,                                   
                       bc(days=date, lat=farm$Centriod@coords[[2]],BCb=BCbc , extraT=NULL, (high-32)*5/9, (low-32)*5/9,tal=0.65) ) #T in C

},error=function(cond) {
  return(0.02)
})

ww<-weach.sub(weather10[,1:11],lati=farm$Centriod@coords[[2]])
###############################################################################################
############################################################# Simulation
start.time<-1
end<-240
soilP<-farm$Soil@data[,c("Fc2","PWP1","om_r","AWC")]%>%summarise_all(funs(mean(., na.rm = TRUE)))



soilcont<-SoilParms(B3 = 2.8,B2=-3.006,SOLID  = 0.8,ZLENG = 1.2,ORGAN= soilP$om_r/100,t.fc = soilP$Fc2/100,t.s=soilP$Fc2*0.5/100,t.r=soilP$PWP1/100)
t<-proc.time()
Output.soil<-Soil.temp(ww,
                       s_type=1, # top boundry condition energy balance 
                       DELZ=0.1,
                       Temp.ini=ww[1,5],
                       swc.ini=0.3,
                       start.time=start.time,
                       end=240,
                       DELT=3600,
                       soilControl=soilcont,
                       pltpop=7, #plant/m2
                       Z0=0.01)

dates<-strptime(paste(ww[,1],"-",format(strptime(ww[,2], format="%j"), format="%m-%d"),ww[,3],":00"),"%Y - %m-%d %H :%M")

stemp<-as.data.frame(Output.soil[['SoilTemp']])%>%mutate(Depth=seq(0.0,1.2,0.1))%>%gather(Time,Value,-c(Depth))

stemp$Time=rep(dates[start.time:(end+1)],each=13)

stemp$Depth<-as.factor(round((stemp$Depth*100)/2.54))
  
swc<-as.data.frame(Output.soil[['SW']])%>%mutate(Depth=seq(0.0,1.2,0.1))%>%gather(Time,Value,-c(Depth))
swc$Time=rep(dates[start.time:(end+1)],each=13)
swc$Depth<-as.factor(round((swc$Depth*100)/2.54))

## I did conversion from C to F
stemp[stemp$Depth%in%c("0","4","12","20"),]%>%ggplot()+
  geom_line(aes(Time,(Value*1.8)+32, color=Depth),size=1.2)+
  theme_bw(base_size = 15)+
  labs(y="Soil Temperature (F)")+
  scale_color_brewer(palette = "Set1")


## I did conversion from C to F
swc[swc$Depth%in%c("0","4","12","20"),]%>%ggplot()+
  geom_line(aes(Time,(Value), color=Depth),size=1.2)+
  theme_bw(base_size = 15)+
  labs(y="Soil moisture (inch/inch soil)")+
  scale_color_brewer(palette = "Set1")

proc.time()-t



