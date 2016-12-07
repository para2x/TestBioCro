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


                         # Fertilization, # initial mineral N
centuryContNofert<-centuryParms(Nfert = c(0,90),iMinN = 0)
centuryContfert<-centuryParms(Nfert = c(10000,90),iMinN = 100000)

nitrocont<-nitroParms(Vmax.b1=-1,alpha.b1=-1)
nitrocont2<-nitroParms()

modelNoN<- BioGro(weather05,timestep = 1,day1 = 90,dayn = 300,
                soilControl=s0,
                phenoControl=phenolcont,
                centuryControl = centuryContNofert, ## ading the fert
                nitroControl = nitrocont)   # control the N effect on growth


modelN<- BioGro(weather05,timestep = 1,day1 = 90,dayn = 300,
                soilControl=s0,
                phenoControl=phenolcont,
                centuryControl = centuryContfert, ## ading the fert
                nitroControl = nitrocont)   # control the N effect on growth


## Stem
xyplot(modelNoN$Stem+modelN$Stem~modelNoN$DayofYear)
##vmax
xyplot(modelNoN$VmaxVec+modelN$VmaxVec~modelNoN$DayofYear)
