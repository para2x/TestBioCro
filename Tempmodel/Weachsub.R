weach.sub <- function(X,lati,ts=1,temp.units=c("Fahrenheit","Celsius"),
                  rh.units=c("percent","fraction"),ws.units=c("mph","mps"),
                  pp.units=c("in","mm"),...){
MPHTOMPERSEC <- 0.447222222222222
temp.units <- match.arg(temp.units)
rh.units <- match.arg(rh.units)
ws.units <- match.arg(ws.units)
pp.units <- match.arg(pp.units)
ldays<-length(X[, 1]) # how many days
year <- X[, 1]
DOYm <- X[, 2]
solar <- X[, 3]
maxTemp <- X[, 4]
minTemp <- X[, 5]
avgTemp <- X[, 6]
maxRH <- X[, 7]
minRH <- X[, 8]
avgRH <- X[, 9]
WindSpeed <- X[, 10]
precip <- X[, 11]
tint <- 24/ts
tseq <- seq(0, 23, ts)
solarR <- (0.12 * solar) * 2.07 * 10^6/3600
solarR <- rep(solarR, each = tint)
ltseq <- length(tseq)
resC2 <- numeric(ltseq * ldays)

for (i in seq_along(DOYm)) {
  res <- lightME(DOY = DOYm[i], t.d = tseq, lat = lati)
  Itot <- res$I.dir + res$I.diff
  indx <- 1:ltseq + (i - 1) * ltseq
  resC2[indx] <- (Itot - min(Itot))/max(Itot)
}
SolarR <- solarR * resC2
if (temp.units == "Farenheit") {
  minTemp <- (minTemp - 32) * (5/9)
  minTemp <- rep(minTemp, each = tint)
  maxTemp <- (maxTemp - 32) * (5/9)
  maxTemp <- rep(maxTemp, each = tint)
  rangeTemp <- maxTemp - minTemp
}
else {
  minTemp <- rep(minTemp, each = tint)
  maxTemp <- rep(maxTemp, each = tint)
  rangeTemp <- maxTemp - minTemp
}
xx <- rep(tseq, ldays)
temp1 <- sin(2 * pi * (xx - 10)/tint)
temp1 <- (temp1 + 1)/2
Temp <- minTemp + temp1 * rangeTemp
minRH <- rep(minRH, each = tint)
maxRH <- rep(maxRH, each = tint)
temp2 <- cos(2 * pi * (xx - 10)/tint)
temp2 <- (temp2 + 1)/2
if (rh.units == "percent") {
  RH <- (minRH + temp2 * (maxRH - minRH))/100
}
else {
  RH <- (minRH + temp2 * (maxRH - minRH))
}
if (ws.units == "mph") {
  WS <- rep(WindSpeed, each = tint) * MPHTOMPERSEC
}
else {
  WS <- rep(WindSpeed, each = tint)
}
if (pp.units == "in") {
  precip <- rep(I((precip * 2.54 * 10)/tint), each = tint)
}
else {
  precip <- rep(I(precip/tint), each = tint)
}
hour <- rep(tseq, ldays)
DOY <- DOYm
doy <- rep(DOY, each = tint)
ans <- cbind(year, doy, hour, SolarR, Temp, RH, WS, precip)
return(as.data.frame(ans))
}