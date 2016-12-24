##################################################################################
#
#----------------------------- SOIL TEMPERATURE MODULE --------------------------#
#------------------ This module has been developed by Hamze Dokoohaki -----------#
#---------------------------------------------------------------------- ---------#
#                   Under spuervision of :    Ferando Miguez, Brian Hornbuckle   #
##################################################################################
source('Aux_Soilfunc.R')
# weath <- weather dataframe 
# DELT  <- time step
#DELZ   <-space increment
#Z0     <-roughness legth
#Amp    <-Air temp amplitude for s_type=2
#Temp.ini <-Initial temperatuer
#soilControl<-soil hydrulic and thermal properties
#s_type     <-Surface boundry conidtion type 1=Energy balnace 2=Average Air Temp
#ntair      <- window size for taking the average of air temp in s_type=2
#swc.ini    <-Initial soil moisture
#start.time <-starting point at the weather dataframe
#len        <- Length of simulation based on weather time step (hr)
#LAI        <- Leaf Area Index
#pltpop         <-plant pop #plant/m2

Soil.temp<-function (weath, DELT=3600,DELZ=0.05,Z0=0.15,Amp=1,Temp.ini=24,
                      soilControl = list(), s_type=1,ntair=5,swc.ini=0.22,
                       start.time=2,end=240,LAI=0,pltpop=7,Lat=42,Long=-92) {
 
  #######################################  Definign the Variables
  THETAS = soilControl$t.s
  THETAR = soilControl$t.r
  thetafc= soilControl$t.fc
  B1= soilControl$B1
  B2= soilControl$B2
  B3 = soilControl$B3
  SOLID  =soilControl$SOLID
  ORGAN  = soilControl$ORGAN
  ZLENG=soilControl$ZLENG
  ####### Producing empty vectors for storing the results
  len=end
  N=floor((ZLENG+0.001)/DELZ)+1  # number of nodes in Z direction
  swc=rep(swc.ini,N);
  Temp.ini=rep(Temp.ini,13); # Inititial conditions
  Soil.temp<-matrix(NA,nrow=N,ncol=len+1)  # Final results matrix
  SWC.mat<-matrix(NA,nrow=N,ncol=len+1)  # Final results matrix
  tet1<-0.45;  # first layer water content
  Tbar=0;
  # Energy balnce vectors
  Net.rad<-rep(0,len);  
  L.heat<-rep(0,len);  
  S.heat<-rep(0,len);
  G.heat<-rep(0,len);      
  resi<-rep(0,len);
  canopy.temp<-rep(0,len);
  # Soil thermal prop
  CAPA=rep(0,N);  
  RAMDAS=rep(0,N);
  ALPA=rep(0,N);
  ### Temoprary variables
  swc.s<-rep(0,len);  
  time.stamp<-c();    
  Temp<-Temp.ini;  
  Soil.temp[,1]<-Temp.ini;
  SWC.mat[,1]<-swc.ini
  swc.s[1]<-swc.ini
  MAXLAI<-LAI # max leaf area index before the loop
  canopy.hei.pot<-1.6 # canopy height potential (m)
 
    ########################### Going thtough the loop for that period of time
  j=2
  for (ii in start.time:end) {
    ####################################################### providing the weather parameters
    Time<-weath[ii,3];  # Hour
    ws<-weath[ii,7];   # Wind Speed
    t.air<-weath[ii,5];   # Air Temp
    rad<-weath[ii,4]      # Radiation
    tt1<-Soil.temp[1,j-1]; ### soil temp at first node
    tt2<-Soil.temp[2,j-1]; ### soil temp at scond node
    rh=weath[ii,6];   #Relative humidity
    doy<-weath[ii,2] # day of year
    year<-weath[ii,1] # year
    #daylen<-daylength(doy,Lat,Long) ## calculating day length 
   # cat(ws,"-",ii,"\n")
    if (LAI>0){canp.temp<-canopy.temp[j-1]}else{canp.temp<-0}     
    if (ws==0){ws=0.01}
    ##################### surface boundry or 2 
    if (ii>ntair && s_type==2){
      Tbar=0
     for (p in 0:(ntair-1)){     Tbar=Tbar+weath[ii-p,5]    }
      Tbar=Tbar/ntair
     }else{
      Tbar=t.air
    }
    ########## putting the weather and crop parametrs in a list and passing it to function
    # manipulting Time
    if (Time==0){hr<-24}else{hr<-Time}
    # Finding zenith angle   
    temp.time<-strptime(paste(year,"-",format(strptime(doy, format="%j"), format="%m-%d"),hr,":00"),"%Y - %m-%d %H :%M")
    sza<-SZA(as.POSIXct(temp.time))
    if (sza>90) {sza<-90}
    ################################ Crop Height
    #https://github.com/DSSAT/dssat-csm/blob/develop/MZ_GROSUB.for
    # Line 1808-1815
    #! An empirical equation was added to calculate canopy height (CANHT). The denominator
    # (0.4238*PLTPOP + 0.3424) describes the relationship between potential maximum
    # LAI and plant population by fitting a straight line through the data of Jaya
    # et al. (http://www.regional.org.au/au/asa/2001/6/b/jaya.htm).
    # Potential canopy height (CANHT_POT) was set to 1.6 m but should be provided by
    # the user (added to cultivar file?); RS 26May04
    
    if (LAI >= MAXLAI) {    #keeps CANHT at the maximum value
      CANHT <- LAI / (0.4238 * pltpop + 0.3424) * canopy.hei.pot
      if (CANHT > canopy.hei.pot) {
        CANHT<-canopy.hei.pot
      }
    }
    
    MAXLAI = max(MAXLAI,LAI)      # Maximum XLAI season 
    ## check this conversion -> umol/m2sec --- > W/M2    1 W/m2 ??? 4.6 ??mole.m2/s.
    rad.conv<-(DELT/3600)/4.6
    we.Parm<-list("Time"=Time,"ws"=ws,"t.air"=t.air,"rad"=rad*rad.conv,"rh"=rh,"Tbar"=Tbar,"Amp"=Amp,"doy"=doy,"year"=year,"sza"=sza)
    crop.Parm<-list("LAI"=LAI,"CropH"=CANHT)
    #####################Soil Water content Simulation 
    sc <- soilML(precipt=weath[ii,8], CanopyT=canp.temp, cws = swc,
                 soilDepth=0.6, FieldC=thetafc, WiltP=THETAR, rootDB=0.5, soilLayers=N,
                 LAI=LAI, k=0.68, AirTemp=t.air,IRad=rad, winds=ws, RelH=rh, soilType=6,
                 hydrDist=1)
    tet1<-sc[1,1] 
 
    psim<-sc[1,12] 
    swc<-sc[,1]
    evap.ml<-sc[1,6]
    ################################ Preparing soil heat properties
    for (i in 1:N){
      CAPA[i]=CAPACT(SOLID,ORGAN,sc[N,1]) #
      RAMDAS[i]=CONDT(B1,B2,B3,sc[N,1])     # cat(RAMDAS[1],"\n")
    }
    ALPA=(RAMDAS/CAPA)
    ###################################################### Soil Temperature
    #Out.put<-SOILTEMPFUN(tet1,tt1,tt2,psim,s_type,DELT,Temp,N,CAPA,RAMDAS,ALPA,thetafc,
    #                     DELZ,Z0,we.Parm,crop.Parm)
    Out.put<-SOILTEMPFUN(tet1,tt1,tt2,psim,s_type,DELT,Temp,N,CAPA,RAMDAS,ALPA,thetafc,
                                        DELZ,Z0,we.Parm,crop.Parm)
    

    ## Storing the output
    Soil.temp[,j]=Out.put$Temp
    SWC.mat[,j]<-sc[,1]
    
    #########  Collecting energy balance components
    if (s_type==1){
      Net.rad[ii]<-Out.put$bal$Bal$RN
      L.heat[ii]<-Out.put$bal$Bal$ALES
      S.heat[ii]<-Out.put$bal$Bal$SH
      G.heat[ii]<-Out.put$bal$Bal$GS
      swc.s[ii]<-tet1
      resi[ii]<-Out.put$bal$Bal$RES
      canopy.temp<-Out.put$bal$Bal$Tcan
    }
    # putting the current soil temp as the initial soil temp for next step
    Temp<-Soil.temp[,j]  
 j<-j+1
  } # end of loop of time
  return(list("Year"=year,"DOY"=doy,"SoilTemp"=Soil.temp,"NetR"=Net.rad,"LH"=L.heat,"SH"=S.heat,"GH"=G.heat,
              "SW"=SWC.mat,"Res"=resi))

}
