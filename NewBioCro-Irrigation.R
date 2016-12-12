###########
library(BioCro)
library(tidyverse)
#############################
data("weather05")
s0 <- soilParms(FieldC=0.4, WiltP=0.15,soilLayers=1,
                wsFun="exp")

phenolcont<-phenoParms(tp1 = 400, tp2 = 600,tp3 = 1000,tp4 = 1200,
                       tp5 = 1400,tp6 = 7500,
                       kStem6 =0.25 ,kLeaf6 =0.15 ,kRoot6 =0.0 ,kRhizome6 =0.05 ,
                       kGrain6 = 0.55)

##### Soil Irrigation

Irrigcont<-list(c(100,200,250),c(50,50,50))

#################################### Running
modelNoI<- BioGro(weather05,timestep = 1,day1 = 90,dayn = 300,
                soilControl=s0,
                phenoControl=phenolcont)   # control 

modelI<- BioGro(weather05,timestep = 1,day1 = 90,dayn = 300,
                soilControl=s0,
                phenoControl=phenolcont,
                Irrigation =Irrigcont,
                IrrigationEfficiency = 0.95)   # control the Irrigation

############ gathering data
plotdata<-data.frame(Doy=modelI$DayofYear,Stem=modelI$Stem,
                     LeafP=modelI$LeafPsimVec,
                     Model="Irr",
                     SConduc=modelI$StomatalCondCoefs,
                     SWC=modelI$SoilWatCont)

plotdata<-rbind(plotdata,data.frame(Doy=modelN$DayofYear,
                                    Stem=modelNoI$Stem,
                                    LeafP=modelNoI$LeafPsimVec,
                                    Model="NoI",
                                    SConduc=modelNoI$StomatalCondCoefs,
                                    SWC=modelNoI$SoilWatCont))

plotdata<-plotdata%>%gather(Param,Value,-c(Doy,Model))

#plotting
plotdata%>%filter(Param!="SWC")%>%ggplot()+
  geom_line(aes(Doy,Value,color=Model),size=1.6)+
  theme_bw(base_size = 15)+
  facet_grid(.~Param)+
  scale_y_continuous(breaks = seq(0,max(plotdata$Value)+5,10))

#plotting
plotdata%>%filter(Param=="SWC")%>%ggplot()+
  geom_line(aes(Doy,Value,color=Model),size=1.6)+
  scale_x_continuous(breaks = seq(90,300,10))+
  theme_bw()
