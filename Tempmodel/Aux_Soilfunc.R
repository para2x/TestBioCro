############## Soil param function
#SOLID solid part
SoilParms<-function(t.s=0.44,t.r=0.1,t.fc=0.33,B1= 0.228, B2= -2.406,B3 = 4.909,
                    SOLID= 0.5600,ORGAN= 0.070,ZLENG=0.6){
  
   list("t.s"=t.s,"t.r"=t.r,"t.fc"=t.fc,"B1"= B1, "B2"= B2,"B3" =B3,
       "SOLID"= SOLID,"ORGAN"= ORGAN,"ZLENG"=ZLENG)
}

######## # ESTIMATING SOIL HEAT properties
CAPACT<-function(SOLID,ORGAN,WATER){
  return((2.39e6*SOLID)+(5e6*ORGAN)+(4.18e6*WATER))
}

CONDT<-function (B1,B2,B3,THTA){
  return(B1+(B2*THTA)+(B3*sqrt(THTA)))
}

### Albedo estimation
ALBEDO_FUNC<-function (SWC1){
  if(SWC1>0.25){
    ALBE=0.15
  }else if (SWC1<0.1){
    ALBE=0.25
    
  }else {
    ALBE=0.4-SWC1
    
  }
  return (ALBE)
}


#################### Zenith Angle estimation
####Adopted  from :
#Package: RAtmosphere #Version: 1.1
#Author: Gionata Biavati <gbiavati@bgc-jena.mpg.de>
############# Require insol package too for equation of time
SZA<-function (timein , Lat = 42.0347, Lon = -93.6200){
  sza <- vector("numeric", length = length(timein))
  for (i in 1:length(timein)) {
    time <- as.POSIXlt(timein[i], tz = 'GMT')
  #  print(timein)
  #  print(time)
    d2r = pi/180
    r2d = 1/d2r
    d <- 23.45 * d2r * sin(d2r * 360 * (284 + time$yday)/365)
    #E_qt <-  eqtime(time$yday)    
    if (time$yday <= 106) {
      E_qt <- -14.2 * sin(pi * (time$yday + 7)/111)
    }
    else {
      if (time$yday <= 166) {
        E_qt <- 4 * sin(pi * (time$yday - 106)/59)
      }
      else {
        if (time$yday <= 246) {
          E_qt <- -6.5 * sin(pi * (time$yday - 166)/80)
        }
        else {
          E_qt <- 16.4 * sin(pi * (time$yday - 247)/113)
        }
      }
    }
    
    T <- time$hour + time$min/60 + time$sec/3600
    Longitude <- Lon
    T_solar <- T + Longitude/15 + E_qt/60
    w <- pi * (12 - T_solar)/12
    l <- Lat * d2r
    sza[i] <- 90 - asin(sin(l) * sin(d) + cos(l) * cos(d) * 
                          cos(w)) * r2d
  }
  return(sza)
}
