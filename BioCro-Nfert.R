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



centuryCont<-centuryParms(Nfert = c(99,00))

modelc<- BioGro(weather05,timestep = 1,day1 = 90,dayn = 300,
                soilControl=s0,
                phenoControl=phenolcont,
                centuryControl = centuryCont)


#plot(modelc)

barplot(modelc$SNpools)
