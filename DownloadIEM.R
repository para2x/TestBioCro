library(tidyverse)
library(BioCro)

add<-"http://mesonet.agron.iastate.edu/cgi-bin/request/daily.py?network=IA_ASOS&stations=AMW&year1=2010&month1=1&day1=1&year2=2016&month2=1&day2=1"
ptm <- proc.time()
ASOSweather<-read.csv2(add,sep = ",",stringsAsFactors = F)[,c(3:4,7,8,10:12)]
ASOSweather$avg_temp_f<-(as.numeric(ASOSweather$min_temp_f)+as.numeric(ASOSweather$max_temp_f))/2
proc.time() - ptm


add<-"http://mesonet.agron.iastate.edu/cgi-bin/request/coop.py?network=IACLIMATE&stations=IA0112&year1=2010&month1=1&day1=1&year2=2015&month2=12&day2=31&vars%5B%5D=apsim&what=view&delim=comma&gis=no&scenario_year=2013"
ptm <- proc.time()
coopweather<-read.table(add,sep = "",skip = 10,header = T)
names(coopweather)<-c("year","day","radn","maxt","mint","rain")
proc.time() - ptm


## mergiing datasets
if(nrow(ASOSweather)==nrow(coopweather)){
  ASOSweather$avg_wind_speed_kts<-as.numeric(as.character(ASOSweather$avg_wind_speed_kts))*1.15
  weather<-cbind(coopweather[1:3],ASOSweather)
}
weather<-weather[,c(1:5,11,10,8,9,7,6)]
weather<-as.data.frame(apply(weather, 2, as.numeric))
weather$date<-as.Date(strptime(paste(as.character(weather$year), weather$day), format="%Y %j") ) ##
weather$avg_wind_speed_kts<-weather$avg_wind_speed_kts*1.15
## for retrving up date 
strftime(as.Date(Sys.time()) ,'%d')
strftime(as.Date(Sys.time()) ,'%m')
strftime(as.Date(Sys.time()) ,'%Y')



rm(weather,ASOSweather,coopweather)
## running model and merging the out puts
##############
library(tidyverse)
library(BioCro)
library(plyr)
setwd("C:/Users/Owner/Downloads")
sss<-readRDS("Farmdata2016-11-19 (1).frm")
sss<-sss[[1]]
weather<-sss$Weather
selected.years<-c(2015)
cropsim<-data.frame()
timestep<-12
aa<-lapply(unique(sss$Weather$year)[which(unique(sss$Weather$year)%in%selected.years)],
           function(x){
             weach((sss$Weather[,1:11])%>%filter(year==x),lat = sss$Centriod@coords[2],ts=timestep)
           })
weachdata <- ldply(aa, data.frame)

datatv<-sss$Management
datatv<-datatv%>%mutate(year=as.numeric(format(as.Date(start),format="%Y"))) 
t<-proc.time()
for(i in unique(sss$Soil$mupolygonkey)){
  for (yeari in selected.years){
    tempdata<-datatv%>%filter(year==yeari)
    #preparing dates
    plandate<-as.numeric(strftime(tempdata[tempdata$content=="Planting",3], format = "%j"))
    harvdate<-as.numeric(strftime(tempdata[tempdata$content=="Harvest",3], format = "%j"))
    ## subsetting weather
    ww<-as.data.frame(weachdata)%>%filter(year==yeari)
    ## running model
      s0 <- soilParms(FieldC=1, WiltP=,soilLayers=,soilDepth=1)
      phenolcont<-phenoParms(tp1 = 400, tp2 = 600,tp3 = 1000,tp4 = 1200,
                             tp5 = 1400,tp6 = 7500,
                             kStem6 =0.25 ,kLeaf6 =0.15 ,kRoot6 =0.0 ,kRhizome6 =0.05 ,
                             kGrain6 = 0.55)
      
      
     # ww<-weach((sss$Weather[,1:11])%>%filter(year==2016),lat = sss$Centriod@coords[2])
      #running model
      modelc<- BioGro(ww,day1 = plandate,dayn = harvdate,lat=sss$Centriod@coords[2],timestep = timestep,
                      soilControl=s0,
                      phenoControl=phenolcont)
      
      dfm<-do.call(cbind.data.frame, modelc[-c(27,28)]) ## add polygon ID and year at the end
      dfm<- dfm[,-c(24:26)]%>%group_by(DayofYear)%>%summarise_all(max)%>%mutate(Year=yeari,mupolygonkey=i)
      cropsim<-rbind(cropsim,dfm)
  } # loop year
} ##loop soil
proc.time()-t

plot(modelc)





ss<-cropsim%>%group_by(Year)%>%summarise_all(max)



modelc1<-BioGro(ww,day1 = 90,dayn = 280,soilControl = s3)

xyplot(modelc0$SoilWatCont +
         modelc1$SoilWatCont ~ modelc0$DayofYear,
       type="l",		 
       ylab="Soil water Content (fraction)",
       xlab="DOY")



plot(modelc)
pools<-modelc[c(27,28)]
pools<-do.call(cbind.data.frame, pools)
dfm<-do.call(cbind.data.frame, modelc[-c(27,28)]) ## add polygon ID and year at the end
dfm2<- dfm[,-c(24:26)]%>%group_by(DayofYear)%>%summarise_all(mean)

########### GDD
library(tidyverse)
library(broom)
ss<-weather%>%group_by(year)%>%do(GDD=cumsum(.$avg_temp_f[1:4]))

######################

if (interactive()) {
  
  ui <- fluidPage(
    sliderInput("obs", "Number of observations:",
                min =as.Date("2015-01-01"), max = as.Date("2015-09-01"),format = "%F",
                value = c(as.Date("2015-06-01"),as.Date("2015-07-01")),step=1
                ),    sliderInput("obs2", "Number of observations:",
                                  min =0, max = 0,format = "%F",
                                  value = 0
                ),
    plotOutput("distPlot")
  )
  
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      plot()
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}



########################### mean sd tidy
library(readr)
library(tidyverse)
crop <- read_csv("C:/Users/Para2x/Downloads/crop (1).csv")[,-c(1)]
crop<-crop[,c(27,1,28,3:4)]
crop2<-crop%>%group_by(Year,DayofYear)%>%summarise_all(funs(mean,sd))
crop2<-crop2[,-c(3,6)]
#
piece1<-crop2[,c(1:4)]%>%gather(Param,Mean,-c(Year,DayofYear))
piece2<-crop2[,c(1:2,5:6)]%>%gather(Param,sd,-c(Year,DayofYear))
bigpiece<-cbind(piece1,piece2)[,-c(5,6,7)]
bigpiece$sd<-rnorm(length(bigpiece$sd),0,0.1)

bigpiece%>%ggplot(aes(x=DayofYear,y=Mean))+
  geom_line(aes(color=Param,linetype=as.factor(Year)))+
  geom_errorbar(aes(ymin=Mean-sd,ymax=Mean+sd))
#######################################
######## Checking the weather
######################################################
library(tidyverse)
sim2015<-weather%>%filter(year<2015)%>%select(day,radn,max_temp_f,min_temp_f,precip_in,avg_wind_speed_kts,max_rh)%>% 
  group_by(day)%>%summarise_all(funs(mean(., na.rm = TRUE)))

ggplot()+
  geom_line(aes(x=day,y=cumsum(radn)),data =sim2015,color="red")+
  geom_line(aes(x=day,y=cumsum(radn)),data =weather%>%filter(year==2015),color="blue")

ggplot()+
  geom_line(aes(x=day,y=(max_temp_f)),data =sim2015,color="red")+
  geom_line(aes(x=day,y=(max_temp_f)),data =weather%>%filter(year==2015),color="blue")




ggplot()+
  geom_line(aes(x=day,y=cumsum(precip_in)),data =sim2015,color="red")+
  geom_line(aes(x=day,y=cumsum(precip_in)),data =weather%>%filter(year==2015),color="blue")

