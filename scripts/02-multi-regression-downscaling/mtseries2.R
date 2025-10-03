#
#Test program to carry out various time series analyses
#
#load libraries needed
library(timeSeries)
library(Kendall)
library(Rwave)
library(TSA)
library(wmtsa)
library(plotrix)
#--Rayong-- Change station name



########################define file for input / read #############@###################
data <- read.table("Daten_Rayong1971-2000+CGCM3-DAI+NINA-selection.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
attach(data)

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)

station=pr
station_name="Rain"

#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="mtseries3"
#dir_name=station_name

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name) else dirfile=(dir_name)
dir0=dirname(file.path("D:","Temp","dummy"))
dir1=dirname(file.path("D:","Temp",paste(dirfile),"dummy"))

##########################################The stuff above needs to be processed only once.################################



#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#begin (-999) and end (999) of series
istart=which(station==-99.99)+1 
if(length(istart)==0) istart=1

ifinal=which(station==999)-1
nmax=ifinal-istart +1 
yearstart=year[istart]
monthstart=which(months==month[istart])
ts=station[istart:ifinal]
#############################this is a first approach to fix missing data, needs reconsideration
tsub=substituteNA(ts, type = "mean")
str(tsub)
tseries=ts(as.vector(tsub),start=c(yearstart,monthstart),frequency=12)
t=time(tseries)

############# max t is here #########################
tfinal = max(t)

############# decomposition #########################
#sd=stl(tseries,s.window="periodic",t.window=7)
#trend_series=sd$time.series[,2]-mean(sd$time.series[,2])

############# year of observation #########################
start0=1971
startyear=1971
endyear=2000


tseries = matrix(tsub,nrow = endyear-yearstart+1, ncol = 12, byrow=TRUE)
zm<-apply(tseries, MARGIN=2, FUN=mean)
z2<-sweep(tseries, MARGIN=2, STATS=zm) #subtract monthly means
#tfinal=2000
rowstart = startyear-start0 
#SST_red=z2[-c(1:rowstart),]     # no delete the row if start0 = startyear
SST_red=z2
SST=matrix(t(SST_red),ncol=1,byrow=TRUE)
SST=as.vector(SST)
trend_series <-ts(SST,start=startyear,frequency=12)

######################################################################################
########################define file for comparison ###################################
######################################################################################
#Column of parameters specific

nmaxpar = 28
nparmat2 = 15
nparmat3 = 14

lagmax= array(0, c(nmaxpar))
lagmin= array(0, c(nmaxpar))
maxl = array(0, c(nmaxpar))
minl = array(0, c(nmaxpar))
corcoef = array(0, c(nmaxpar))

ninoSST = array(0, c(ifinal,nmaxpar))
nfile = array(0, c(ifinal+1,nmaxpar))
tfile = array("", c(nmaxpar))

######################### auto correlation matrix plot ##################################

######################################################################################
#******************** remove unwanted column here ##################################
######################################################################################
#DAI
data2=data[-c(ifinal+1),-c(1,2,5:6,8:15)]
#2D
data3=data[-c(ifinal+1),-c(1,2,5:6,16:24)]
mtseries2=ts(data2,start=c(1971,1),frequency=12)
mtseries3=ts(data3,start=c(1971,1),frequency=12)
submt2=substituteNA(mtseries2, type = "mean")
submt3=substituteNA(mtseries3, type = "mean")

setwd(dir1)
pdf(paste("mutlti-corl_CGCM-DAI.pdf"), width= 10, height = 10)
acf2=acf(submt2, drop.lag.0 = FALSE)
dev.off()

pdf(paste("mutlti-corl_CGCM-2D.pdf"), width= 10, height = 10)
acf3=acf(submt3, drop.lag.0 = FALSE)
dev.off()

#if you want to use the MannKendall test you must load the package "Kendall"
#for (i in 1:6) {
#MK=MannKendall(mtseries[,i])
#print(MK)
#}
#dev.new()


#******************** print out the auto cross ##################################


############################## CGCM3-DAI********************************************************************

nparmat = nparmat2

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))
acfmax= array(0, c(nparmat,nparmat))
lagmax= array(0, c(nparmat,nparmat))

for(i in 1:nparmat){
for(j in 1:nparmat){
maxlag=which(acf2$acf[,i,j]==max(acf2$acf[,i,j]))
minlag=which(acf2$acf[,i,j]==min(acf2$acf[,i,j]))
acfa = acf2$acf[,i,j]
acfl = acf2$lag[,i,j]
if (acfa[maxlag]>(acfa[minlag]*-1)){lagmat[i,j] = acfl[maxlag]; acfmat[i,j] = acfa[maxlag]} else {lagmat[i,j] = acfl[minlag]; acfmat[i,j] = acfa[minlag]};
}
}
rownames(acfmat) = acf2$sname
colnames(acfmat) = acf2$sname

setwd(dir1)
pdf(paste("ColorMatrix_CGCM-DAI.pdf"), width= 15, height = 15)

par(ps =12)
color2D.matplot(acfmat,redrange=c(0,-1),greenrange=c(0,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Maximum Cross-Correlation of CGCM3-DAI paramters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)
par(ps =12)
color2D.matplot(lagmat,redrange=c(0.5,-1),greenrange=c(0.5,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Lag time at maximum correlation of CGCM3-DAI parameters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)

dev.off()

#*********************** Find max correlation from +/- ***********************************
for(i in 1:nparmat){
for(j in 1:nparmat){
if (abs(acfmat[i,j]) > abs(acfmat[j,i])){acfmax[i,j] = acfmat[i,j];lagmax[i,j] = lagmat[i,j]} else {acfmax[i,j] = acfmat[j,i];lagmax[i,j] = lagmat[j,i]};
}
}

#print output to text ############################################
#print out some variables to file
setwd(dir1)
sink("Output-DAI.csv")
setwd(dir0)

cat("Predictor:",",",nparmat,"\n") 
cat("----------------------------","\n")
#cat("Correlation-coefficients (for lag=0):  ",corcoef); corcoef
#cat("Regression-Summary:  "); reg_sum
#cat("\n","Lag and cross-correlation","\n")
cat("Predictor1,Predictor2,Cross-cort,Cross-cort2, Lag2, MaxCrs-Cort Lag, MaxCrs-Cort,at Lag\n")

#*********************** Loop for print output ***********************************
for(i in 1:nparmat){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",",acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],"\n")
}
}
sink()
#*********************** End Loop for print output to text***********************************






############################## CGCM3-2D********************************************************************

nparmat = nparmat3

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))
acfmax= array(0, c(nparmat,nparmat))
lagmax= array(0, c(nparmat,nparmat))


for(i in 1:nparmat){
for(j in 1:nparmat){
maxlag=which(acf3$acf[,i,j]==max(acf3$acf[,i,j]))
minlag=which(acf3$acf[,i,j]==min(acf3$acf[,i,j]))
acfa = acf3$acf[,i,j]
acfl = acf3$lag[,i,j]
if (acfa[maxlag]>(acfa[minlag]*-1)){lagmat[i,j] = acfl[maxlag]; acfmat[i,j] = acfa[maxlag]} else {lagmat[i,j] = acfl[minlag]; acfmat[i,j] = acfa[minlag]};
}
}
rownames(acfmat) = acf3$sname
colnames(acfmat) = acf3$sname

setwd(dir1)
pdf(paste("ColorMatrix_CGCM-2D.pdf"), width= 15, height = 15)

par(ps =12)
color2D.matplot(acfmat,redrange=c(0.2,-1),greenrange=c(0.2,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Maximum Cross-Correlation of CGCM3-2D paramters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)
par(ps =12)
color2D.matplot(lagmat,redrange=c(0.2,-1),greenrange=c(0.2,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Lag time at maximum correlation of CGCM3-2D parameters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)

dev.off()
setwd(dir0)

#*********************** Find max correlation from +/- ***********************************
for(i in 1:nparmat){
for(j in 1:nparmat){
if (abs(acfmat[i,j]) > abs(acfmat[j,i])){acfmax[i,j] = acfmat[i,j];lagmax[i,j] = lagmat[i,j]} else {acfmax[i,j] = acfmat[j,i];lagmax[i,j] = lagmat[j,i]};
}
}

#print output to text ############################################
#print out some variables to file
setwd(dir1)
sink("Output-2D.csv")
setwd(dir0)

cat("Predictor:",",",nparmat,"\n") 
cat("----------------------------","\n")
#cat("Correlation-coefficients (for lag=0):  ",corcoef); corcoef
#cat("Regression-Summary:  "); reg_sum
#cat("\n","Lag and cross-correlation","\n")
cat("Predictor1,Predictor2,Cross-cort1, Lag1, Cross-cort2, Lag2, MaxCrs-Cort,at Lag\n")

#*********************** Loop for print output ***********************************
for(i in 1:nparmat){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",",acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],"\n")
}
}
sink()
#*********************** End Loop for print output to text***********************************




