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
source("clear.R")
#clear()
#--Rayong-- Change station name

#####The stuff above needs to be processed only once.

########################define file for comparison ###################################
#Nino1/Nino4/Nino34 region (specify)
#nfile="monthly-obs_filled1971-2006(2100)+WetDay+WetRatio.csv"
nfile="ocean index 1971-2009.csv"

max.level.dcomp = 5
in.one.pdf = TRUE

trend.plot1 = FALSE
trend.plot2 = FALSE
trend.plot3 = FALSE
wavelet.plot = TRUE
dcompose.plot = FALSE


#######################
### 1) Directory Names

maindir = "Temp/frequency_autocorrelation"

#dir_name = "freq_analysis_v01_monthly-obs_filled1971-2005 only WAVELET"
dir_name = "freq_analysis_v01_ocean index 1971-2008 only WAVELET"

#dir_name = "freq_analysis_v01_monthly-obs_filled1971-2005"
#dir_name = "freq_analysis_v01_ocean index 1971-2008"




###########################
## DATA for Regional Index
############# spatial info of index #########################
start0.index=1971  #year start in table file
startyear.index=1971 #year to analysis begins
startmonth.index=1
startmonth0.index=1
#endyear.index = 2008 # if 12/2000 have to set to endyear= 2000
endyear.index = 2005 # if 12/2000 have to set to endyear= 2000
endmonth.index=12 # if 12/2000 have to set to endmonth= 12
unused.col = 1:2

dataindex <- read.table(nfile, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

ifinal=min(which(dataindex$year==endyear.index)) + endmonth.index
datafinal = length(dataindex$year)+1
subty = (startyear.index-start0.index)*12

#******************** remove unwanted column here ##################################
dataindex0 = dataindex[-c(0:subty,ifinal:datafinal),-unused.col]

#source file path of programms and data (change!!!!); 
#output goes to subdirectory with name of station
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

nmax = nrow(dataindex0)
coln = ncol(dataindex0)
#monthstart =which(months==month[istart])


####### Creat Directory ########
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
cat("\nCreating directory for conclusion :",dir_name)
L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name, recursive = TRUE)
dirfile=(dir_name)
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))


# Plot all in one PDF
if(in.one.pdf){
setwd(dir1)
pdf(paste("Freq Analysis - all.pdf"))
setwd(dir0)
}

##############################
####### main loop (dat.i)
##############################
for(dat.i in 1:coln){

cat("\n****\nStart analyzing",dat.i,colnames(dataindex0)[dat.i])




# Creat PDF for each file
if(!in.one.pdf){
cat("\nCreating PDF",paste("Freq Analysis - ",colnames(dataindex0)[dat.i],".pdf",sep=""))
setwd(dir1)
pdf(paste("Freq Analysis - ",colnames(dataindex0)[dat.i],".pdf",sep=""))
setwd(dir0)
}

yearstart = dataindex$year[1]
monthstart = which(months==dataindex$month[1])

ts=dataindex0[,dat.i]
tseries=ts(as.vector(ts),start=c(yearstart,monthstart),frequency=12)
t=time(tseries)

if(length(which(is.na(tseries)))>0){
						t.na = max(which(is.na(tseries)))
						t.start = time(tseries)[t.na+1]
						newmat2 = ts(ts[(t.na+1):length(tseries)], frequency=12,start=t.start)

						tseries = newmat2
						t=time(tseries)
						}else{
							t.start =start0.index
							}


#station=Rain
station_name = colnames(dataindex0)[dat.i]

#title for wavelet
twvlet = "b)"
telnino = nfile


#*************************************************************** time series *********************************
cat("\nWriting tseries")
#setwd(dir1)
#pdf("tseries.pdf")
#setwd(dir0)

if(trend.plot1){
plot(tseries,xlim=c(start0.index,endyear.index),ylab=station_name,
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
legend(start0.index,ylab, c(paste("", c("linear", "polynomial"))), lty = 1, col =c("red","blue"))
#dev.off()
}

#*************************************************************** decomposition********************************
#seasonal decomposition 
cat("\nWriting decomposition")
#setwd(dir1)
#pdf("seasonal_decomp.pdf")
#setwd(dir0)

sd=stl(tseries,s.window="periodic",t.window=7)
trend_series=sd$time.series[,2]-mean(sd$time.series[,2])
if(trend.plot2){
plot(sd,xlim=c(start0.index,endyear.index),main=paste("Station:",station_name))
#ylab = expression(paste("Temperature [ ", degree, "C]")))
}

MannKenn=MannKendall(tseries)

#*************************************************************** cycle remove*********************************
# harmonic analysis to remove annual cycle
cat("\nWriting harmnic analysis")
#setwd(dir1)
#pdf("tseries_harmonic.pdf")
#setwd(dir0)

har=harmonic(tseries,1)
model=lm(tseries~har)
#summary(model)
fit=fitted(model)

op=par
par(mfrow=c(2,1))
y.max = max(tseries,fit)
y.min = min(tseries,fit)
if(trend.plot3){
plot(tseries,xlim=c(start0.index,endyear.index),ylim = c(y.min,y.max),type="l",main=paste("Station:",station_name),
ylab = station_name)
lines(as.vector(t),fit,type="l",col="blue")
legend(start0.index, max(tseries)-0.5,c(paste("", c("Station", "Cycle"))), lty = 1, col =c("black","blue"))
}

tdiff=as.vector(tseries)-as.vector(fit)
tdiff=ts(tdiff,start=t.start,frequency=12)
#plot(tdiff,type="l",xlim=c(start0.index,endyear.index),main="Cycle-removed series")
#par(op)
#dev.off()


#********************************************************* correlation anomaly *********************************
#New SST data from NCEP
# El Nino data & correlation
cat("\nWriting correlation")
#setwd(dir1)
#pdf("nino_station.pdf")
#setwd(dir0)

#par(mfrow=c(2,1))

######################## plot trend line as anomaly ####
if(trend.plot3){
#plot(trend_series,ylim=c(-2,4),xlim=c(start0.index,endyear.index),main=paste(station_name),
y.max = max(trend_series,tdiff)
y.min = min(trend_series,tdiff)
plot(trend_series,ylim=c(y.min,y.max ),xlim=c(start0.index,endyear.index),main=paste(station_name),
ylab = expression(paste("Anomaly")))
lines(tdiff,type="l",col="blue",lty=2)
legend(start0.index,y.max-0.01, c(paste("", c("Trend line","Cycle removed"))), lty = c(1,2), col =c("black","blue"))
}
# correlation analysis El Nino-Station
# cut-down longer (Nino) timeseries to shorter station series.
#max1=length(ninodat)
#max2=length(trend_series)
#maxtot=min(max1,max2)

tfinal=max(t)

#wavelet analysis of El Nino and cycle-removed station data 
cat("\nWriting wavelet")
#setwd(dir1)
#pdf("wavelet_nino_station.pdf")
#setwd(dir0)

#--Rayong-- Change station wavelet title
if(wavelet.plot){
n3wsp1 <- wsp(tseries,s0=2,noctave=3,nvoice=20,nreal=0,
plottitle=paste("Wavelet (time-series)",station_name))
#n3wsp2 <- wsp(trend_series,s0=2,noctave=3,nvoice=20,nreal=0,
#plottitle=paste("Wavelet (trend line)",station_name))
#n3wsp3 <- wsp(tdiff,s0=2,noctave=3,nvoice=20,nreal=0,
#plottitle=paste("Wavelet (cycle removed)",station_name))
##par(op) 
#dev.off()
}

#multiresolution decomposition Nino
cat("\nWriting multiresolution decomposition")
#setwd(dir1)
#pdf("multiresol_nino_station.pdf")
#setwd(dir0)

#multiresolution decomposition of station
x <- as.vector(trend_series)
z1 <- wavMRDSum(x, levels=max.level.dcomp:max.level.dcomp)
z2 <- wavMRDSum(x, levels=(max.level.dcomp-1):max.level.dcomp)
z3 <- wavMRDSum(x, levels=(max.level.dcomp-2):max.level.dcomp)
z4 <- wavMRDSum(x, levels=(max.level.dcomp-3):max.level.dcomp)
if(dcompose.plot){
stackPlot(x=as.vector(t),
y=list("T-series"=x,"D2+D3+D4+D5"=z4,"D3+D4+D5"=z3,"D4+D5"=z2,"D5"=z1),
ylim=range(x,z1,z2,z3,z4),main=paste("Station:",station_name))
}

# END Creat PDF each station
if(!in.one.pdf){dev.off()}
}

# END Creat PDF for every station
if(in.one.pdf){dev.off()}