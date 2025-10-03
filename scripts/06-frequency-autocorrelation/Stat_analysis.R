#
#Test program to carry out various time series analyses
#
#load libraries needed
library(timeSeries)
library(Kendall)
library(Rwave)
library(TSA)
library(wmtsa)
source("wavelet.R")
#--Rayong-- Change station name

########################define file for input / read #############@###################
data <- read.table("Daten_Rayong.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
attach(data)

#####The stuff above needs to be processed only once.

########################define file for comparison ###################################
#Nino1/Nino4/Nino34 region (specify)
nfile="nina1.dat"
#nfile1="nina1.dat"
#nfile34="nina34.dat"
#nfile4="nina4.dat"

####################define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)
#--Rayong-- Change station name and folder name
station=Rain
station_name="Monthly rainfall in Rayong"
dir_name="Monthly rain in Rayong with Nino 1 1950"
#title for wavelet
twvlet = "b)"
telnino = nfile

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name) else dirfile=(dir_name)

#source file path of programms and data (change!!!!); 
#output goes to subdirectory with name of station

#--Rayong-- Change directory path
dir0=dirname(file.path("D:","Temp","dummy"))
dir1=dirname(file.path("D:","Temp",paste(dirfile),"dummy"))

months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#begin (-999) and end (999) of series
istart=which(station==-999)+1 
if(length(istart)==0) istart=1

ifinal=which(station==999)-1
nmax=ifinal-istart +1 
yearstart=year[istart]
monthstart=which(months==month[istart])
ts=station[istart:ifinal]
#this is a first approach to fix missing data, needs reconsideration
tsub=substituteNA(ts, type = "mean")
str(tsub)
tseries=ts(as.vector(tsub),start=c(yearstart,monthstart),frequency=12)
t=time(tseries)

#*************************************************************** time series *********************************
setwd(dir1)
pdf("tseries.pdf")
setwd(dir0)

plot(tseries,xlim=c(1970,2010),ylab="Temperature (C)",
main=paste("Station:",station_name),type="l")

#simple trend line regression model
trend_lm_fit <- lm(tseries ~ t)
trend_sum <- summary(trend_lm_fit)
a=trend_lm_fit$coefficients[1]
b=trend_lm_fit$coefficients[2]
x1=t[1]
x2=t[length(t)]
y1=a + b*x1
y2=a + b*x2
lines(c(x1,x2),c(y1,y2), col = "red")

#Polynomial trend 
d=data.frame(cbind(t,tseries))
lines(lowess(d,f=0.5), col = "blue")
ylab=max(tseries)
legend(1969,ylab, c(paste("", c("linear", "polynomial"))), lty = 1, col =c("red","blue"))
dev.off()

#*************************************************************** decomposition********************************
#seasonal decomposition 
setwd(dir1)
pdf("seasonal_decomp.pdf")
setwd(dir0)

sd=stl(tseries,s.window="periodic",t.window=7)
trend_series=sd$time.series[,2]-mean(sd$time.series[,2])
plot(sd,xlim=c(1970,2010),main=paste("Station:",station_name))
#ylab = expression(paste("Temperature [ ", degree, "C]")))

MannKenn=MannKendall(tseries)

#*************************************************************** cycle remove*********************************
# harmonic analysis to remove annual cycle
setwd(dir1)
pdf("tseries_harmonic.pdf")
setwd(dir0)

har=harmonic(tseries,1)
model=lm(tseries~har)
#summary(model)
fit=fitted(model)

op=par
par(mfrow=c(2,1))
plot(tseries,xlim=c(1970,2010),type="l",main=paste("Station:",station_name),
ylab = expression(paste("Temp-Anomaly [ ", degree, "C]")))
lines(as.vector(t),fit,type="l",col="blue")
legend(1969,29, c(paste("", c("Station", "Cycle"))), lty = 1, col =c("black","blue"))

tdiff=as.vector(tseries)-as.vector(fit)
tdiff=ts(tdiff,start=c(1970,1),frequency=12)
plot(tdiff,type="l",xlim=c(1970,2010),main="Cycle-removed series")
par(op)
dev.off()

#********************************************************* correlation anomaly *********************************
#New SST data from NCEP
# El Nino data & correlation
setwd(dir1)
pdf("nino_station.pdf")
setwd(dir0)

par(mfrow=c(2,1))

######################## read comparison file ########################################
#anomalies with respect to monthly means
SST = read.table(file=paste(nfile),header=FALSE,sep="",dec=".",na.strings="-99.99")
SST=SST[,-1]
zm<-apply(SST, MARGIN=2, FUN=mean)
z2<-sweep(SST, MARGIN=2, STATS=zm) #subtract monthly means
start0=1950
#startyear=1970
startyear=1950
rowstart = startyear-start0 
SST_red=z2[-c(1:rowstart),]
SST=matrix(t(SST_red),ncol=1,byrow=TRUE)
SST=as.vector(SST)
ninodat <-ts(SST,start=startyear,frequency=12)

#absolute values
#SST2 = read.table(file="nina1.dat",header=FALSE,sep="",dec=".",na.strings="-99.99")
#SST2=SST2[,-1]
#SST2_red=SST2[-c(1:rowstart),]
#SST2_red=matrix(t(SST2_red),ncol=1,byrow=TRUE)
#SST2_red=as.vector(SST2_red)
#ninodat <-ts(SST2_red,start=startyear,frequency=12)
#plot(ninodat2)

#SST = read.table(file="nina34.dat",header=FALSE,sep="",dec=".",na.strings="-99.99")
#startyear=1970
#rowstart=which(SST[,1]==startyear)-1
#SST=SST[-c(1:rowstart),]
#SST=SST[,-1]
#SST=matrix(t(SST),ncol=1,byrow=TRUE)
#SST=as.numeric(SST)
#ninodat <-ts(SST,start=startyear,frequency=12)
#nino34=ninodat-mean(ninodat,na.rm=TRUE)

#plot nino (region 1) and temperatures
#plot(tdiff,main=paste("El Nino + Station:",station_name))

######################## plot trend line as anomaly ####
plot(trend_series,ylim=c(-2,4),xlim=c(1970,2010),main=paste(station_name),
ylab = expression(paste("Temp-Anomaly [ ", degree, "C]")))
lines(ninodat,type="l",col="blue")
legend(1969,4, c(paste("", c("Station", "El Nino"))), lty = 1, col =c("black","blue"))

# correlation analysis El Nino-Station
# cut-down longer (Nino) timeseries to shorter station series.
max1=length(ninodat)
max2=length(trend_series)
maxtot=min(max1,max2)

tfinal=max(t)
ninoshort=window(ninodat,start=c(yearstart,monthstart),end=tfinal)
acf=ccf(ninoshort, trend_series,main=paste(dir_name))

#******************************************************* cross-correlation output********************************
#Correlation-Koeffizient(for lag=0)
corcoef=cor(ninoshort, trend_series)
par(op)
par(mfrow=c(2,1))

#max for lag -0

#find max and min coeff
nino_1=lag(ninoshort,0)
ccf=ccf(ninoshort, trend_series)
maxlag=which(ccf$acf==max(ccf$acf))
minlag=which(ccf$acf==min(ccf$acf))
lagmax=ccf$lag[maxlag]
lagmin=ccf$lag[minlag]
# regression analysis station_series=a + b *time + c*SST)
dat <-ts.union(t, trend_series, nino_1)
lm_fit <- lm(trend_series ~ t + nino_1,dat)
reg_sum <- summary(lm_fit)
# get regression coefficients
a <- coef(lm_fit)[1]
b <- coef(lm_fit)[2]
c <- coef(lm_fit)[3]
station_est = a + b*t + c*nino_1

plot(trend_series,xlim=c(1970,2010),main=paste("Regression ",station_name," = a + b*t + c*SST"), 
ylab = expression(paste("Temp-Anomaly [ ", degree, "C]")))
yleg=max(trend_series)-0.1
legend(1969,yleg, c(paste("", c("Observed", "SST-Estim."))), lty = 1, col =c("black","blue"))
points(station_est, type ="l", col = "blue")
par(op)

plot(nino_1,trend_series,xlab="SST",ylab="Station",
main=paste("Station",station_name,"/SST"))
dev.off()

##################################print output ############################################
#print out some variables to file
setwd(dir1)
sink("Output.txt")
setwd(dir0)

cat("Station:  "); station_name
cat("----------------------------");""
cat("Mann-Kenndall test for trend:  "); MannKenn 
cat("Correlation-coefficients Nino/Station(for lag=0):  ",corcoef); corcoef
cat("Regression-Summary:  "); reg_sum
cat("\n","Lag and cross-correlation","\n")
cbind(ccf$lag,ccf$acf)
cat("Maximum= ", max(ccf$acf), "at lag ", lagmax*12,"\n")
cat("Minimum= ", min(ccf$acf), "at lag ", lagmin*12,"\n")
sink()

#********************************************************* wavelet analysis *********************************

#wavelet analysis of El Nino and cycle-removed station data 
setwd(dir1)
pdf("wavelet_nino_station.pdf")
setwd(dir0)


##par(mfrow=c(2,1))
#--Rayong-- Change El Nino wavelet title
n3wsp <- wsp(ninodat,s0=2,noctave=3,nvoice=20,nreal=0,plottitle=telnino) 
#--Rayong-- Change station wavelet title
n3wsp <- wsp(tdiff,s0=2,noctave=3,nvoice=20,nreal=0,
plottitle=paste(twvlet,station_name))
##par(op) 
dev.off()

#multiresolution decomposition Nino
setwd(dir1)
pdf("multiresol_nino_station.pdf")
setwd(dir0)

x <- as.vector(ninodat)
z2 <- wavMRDSum(x, levels=5:5)
z3 <- wavMRDSum(x, levels=4:5)
z4 <- wavMRDSum(x, levels=3:5)
stackPlot(x=as.vector(time(ninodat)),
y=list("El Nino"=x,"D3+D4+D5"=z4,"D4+D5"=z3,"D5"=z2),ylim=range(x,z2,z3,z4),main="El Nino")

#multiresolution decomposition of station
x <- as.vector(trend_series)
z1 <- wavMRDSum(x, levels=6:6)
z2 <- wavMRDSum(x, levels=5:6)
z3 <- wavMRDSum(x, levels=4:6)
z4 <- wavMRDSum(x, levels=3:6)
stackPlot(x=as.vector(t),
y=list("T-series"=x,"D3+D4+D5+D6"=z4,"D4+D5+D6"=z3,"D5+D6"=z2,"D6"=z1),
ylim=range(x,z1,z2,z3,z4),main=paste("Station:",station_name))

dev.off()
