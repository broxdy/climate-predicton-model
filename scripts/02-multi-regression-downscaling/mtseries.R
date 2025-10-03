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
data <- read.table("Daten_Rayong1971-2000+CGCM3-DAI.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
attach(data)

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)

station=pr
station_name="Rain"

#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="mtseries"
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
nparmat2 = 28
nparmat3 = 21

lagmax= array(0, c(nmaxpar))
lagmin= array(0, c(nmaxpar))
maxl = array(0, c(nmaxpar))
minl = array(0, c(nmaxpar))
corcoef = array(0, c(nmaxpar))

ninoSST = array(0, c(ifinal,nmaxpar))
nfile = array(0, c(ifinal+1,nmaxpar))
tfile = array("", c(nmaxpar))


nfile[,1]=c3a2mslpas
nfile[,2]=c3a2tempas
nfile[,3]=c3a2p5_fas
nfile[,4]=c3a2p5_uas
nfile[,5]=c3a2p5_vas
nfile[,6]=c3a2p5_zas
nfile[,7]=c3a2p5thas
nfile[,8]=c3a2p5zhas
nfile[,9]=c3a2p8_fas
nfile[,10]=c3a2p8_uas
nfile[,11]=c3a2p8_vas
nfile[,12]=c3a2p8_zas
nfile[,13]=c3a2p8thas
nfile[,14]=c3a2p8zhas
nfile[,15]=c3a2p500as
nfile[,16]=c3a2p850as
nfile[,17]=c3a2p__fas
nfile[,18]=c3a2p__uas
nfile[,19]=c3a2p__vas
nfile[,20]=c3a2p__zas
nfile[,21]=c3a2p_thas
nfile[,22]=c3a2p_zhas
nfile[,23]=c3a2s500as
nfile[,24]=c3a2shumas
nfile[,25]=c3a2s850as
nfile[,26]=MnT459
nfile[,27]=MxT459
nfile[,28]=ObsRain


tfile[1]="c3a2mslpas"
tfile[2]="c3a2tempas"
tfile[3]="c3a2p5_fas"
tfile[4]="c3a2p5_uas"
tfile[5]="c3a2p5_vas"
tfile[6]="c3a2p5_zas"
tfile[7]="c3a2p5thas"
tfile[8]="c3a2p5zhas"
tfile[9]="c3a2p8_fas"
tfile[10]="c3a2p8_uas"
tfile[11]="c3a2p8_vas"
tfile[12]="c3a2p8_zas"
tfile[13]="c3a2p8thas"
tfile[14]="c3a2p8zhas"
tfile[15]="c3a2p500as"
tfile[16]="c3a2p850as"
tfile[17]="c3a2p__fas"
tfile[18]="c3a2p__uas"
tfile[19]="c3a2p__vas"
tfile[20]="c3a2p__zas"
tfile[21]="c3a2p_thas"
tfile[22]="c3a2p_zhas"
tfile[23]="c3a2s500as"
tfile[24]="c3a2shumas"
tfile[25]="c3a2s850as"
tfile[26]="MnT459"
tfile[27]="MxT459"
tfile[28]="ObsRain"

######################### auto correlation matrix plot ##################################

######################################################################################
#******************** remove unwanted column here ##################################
######################################################################################
data2=data[-c(ifinal+1),-c(1,2,5:6,8:25)]
data3=data[-c(ifinal+1),-c(1,2,5:6,26:50)]
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

############################## CGCM3-DAI

nparmat = nparmat2

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))

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

pdf(paste("ColorMatrix_CGCM-DAI.pdf"), width= 15, height = 15)

par(ps =12)
color2D.matplot(acfmat,redrange=c(0,-1),greenrange=c(0,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Maximum Cross-Correlation of CGCM3-DAI paramters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)
par(ps =12)
color2D.matplot(lagmat,redrange=c(0.5,-1),greenrange=c(0.5,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Lag time at maximum correlation of CGCM3-DAI paramters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)

dev.off()

############################## CGCM3-2D

nparmat = nparmat3

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))

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
pdf(paste("ColorMatrix_CGCM-2D.pdf"), width= 15, height = 15)

par(ps =12)
color2D.matplot(acfmat,redrange=c(0.2,-1),greenrange=c(0.2,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Maximum Cross-Correlation of CGCM3-2D paramters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)
par(ps =12)
color2D.matplot(lagmat,redrange=c(0.2,-1),greenrange=c(0.2,1),nslices=20,axes=FALSE,
show.legend=TRUE, show.values=TRUE,main="Lag time at maximum correlation of CGCM3-2D paramters")
par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(nparmat-0.5):0.5,labels=rownames(acfmat), side =4)

dev.off()
setwd(dir0)
