###########
library(BioCro)
library(tidyverse)
#############################
data("weather05")
s0 <- soilParms(FieldC=1, WiltP=,soilLayers=,soilDepth=1)

phenolcont<-phenoParms(tp1 = 400, tp2 = 600,tp3 = 1000,tp4 = 1200,
                       tp5 = 1400,tp6 = 7500,
                       kStem6 =0.25 ,kLeaf6 =0.15 ,kRoot6 =0.0 ,kRhizome6 =0.05 ,
                       kGrain6 = 0.55)

##### Soil N paramaters
centuryContNofert<-centuryParms()
nitrocont2<-nitroParms()
##### Crop N paraneters
##Very very sensitive to alpha.b1
nitrocont<-nitroParms(Vmax.b1=-1,alpha.b1=-0.005, kLN = 0.5)
                                # Fertilization (g/m2)=10Kg/ha, # initial mineral N
centuryContfert<-centuryParms(iMinN = 0)

Fertilizationcont<-list(c(100,200),c(500,500))

#################################### Running
modelNoN<- BioGro(weather05,timestep = 1,day1 = 90,dayn = 300,
                soilControl=s0,
                phenoControl=phenolcont,
                centuryControl = centuryContNofert, ## ading the fert
                nitroControl = nitrocont)   # control the N effect on growth

modelN<- BioGro(weather05,timestep = 1,day1 = 90,dayn = 300,
                soilControl=s0,
                phenoControl=phenolcont,
                centuryControl = centuryContfert, ## ading the fert
                nitroControl = nitrocont,
                Fertilization =Fertilizationcont )   # control the N effect on growth

############ gathering data
plotdata<-data.frame(Doy=modelN$DayofYear,Stem=modelN$Stem,
                     LeafN=modelN$LeafNitrogen,
                     SoilMinN=modelN$MinNitroVec+10,
                     Model="Nfert",
                     Vmax=modelN$VmaxVec)
plotdata<-rbind(plotdata,data.frame(Doy=modelN$DayofYear,
                                    Stem=modelNoN$Stem,
                                    LeafN=modelNoN$LeafNitrogen,
                                    SoilMinN=modelNoN$MinNitroVec+10,
                                    Model="NoN",
                                    Vmax=modelNoN$VmaxVec))

plotdata<-plotdata%>%gather(Param,Value,-c(Doy,Model))

#plotting
plotdata%>%filter(Param!="SoilMinN")%>%ggplot()+
  geom_line(aes(Doy,Value,color=Model),size=1.6)+
  theme_bw()+
  facet_grid(.~Param)+
  scale_y_continuous(breaks = seq(0,max(plotdata$Value)+5,10))

#plotting
plotdata%>%filter(Param=="SoilMinN")%>%ggplot()+
  geom_line(aes(Doy,Value,color=Model),size=1.6)+
  scale_x_continuous(breaks = seq(90,300,10))+
  theme_bw()
