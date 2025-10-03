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
library(car)
library(DAAG)
library(forecast)


#--Rayong-- Change station name

p2 = "SST"
p3 = "Index"
nmaxprd2 = 7
nmaxprd3 = 6

########################define file for input / read #############@###################
data <- read.table("Daten_Rayong1971-2100+Index2+CGCM3A2.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
#data <- read.table("1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 


#******** without nino data ***************
#data = data[,-c(51:53)]

attach(data)

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)

station=ObsRn
station_name="ObsRn"

#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="selection"
#dir_name=station_name

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","arima","dummy"))
dir1=dirname(file.path("D:","Temp","arima",paste(dir_name),"dummy"))

##########################################The stuff above needs to be processed only once.################################



#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#begin (-999) and end (999) of series
istart=which(station==NA)+1 
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

############# year of observation #########################
start0=1971  #year start in table file
startyear=1971 #year to analysis begins
endyear=2000
endmonth=0

ifinal=min(which(data$year==(endyear+1)))-1 + endmonth
datafinal = length(data$year)
######################################################################################
########################define file for comparison ###################################
######################################################################################
#Column of parameters specific

nmaxpar = 28

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
subty = (startyear-start0)*12
#P2
data2=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:50,58:63)]
#P3
data3=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:57)]


nparmat2 = length(colnames(data2))
nparmat3 = length(colnames(data3))


mtseries2=ts(data2,start=c(startyear,1),frequency=12)
mtseries3=ts(data3,start=c(startyear,1),frequency=12)
submt2=substituteNA(mtseries2, type = "mean")
submt3=substituteNA(mtseries3, type = "mean")

setwd(dir1)
pdf(paste("multi-corl_",p2,".pdf"), width= 10, height = 10)
acf2=acf(submt2, drop.lag.0 = FALSE, ylim = c(-1,1))
dev.off()

pdf(paste("multi-corl_",p3,".pdf"), width= 10, height = 10)
acf3=acf(submt3, drop.lag.0 = FALSE, ylim = c(-1,1))
dev.off()

#if you want to use the MannKendall test you must load the package "Kendall"
#for (i in 1:6) {
#MK=MannKendall(mtseries[,i])
#print(MK)
#}
#dev.new()


#******************** print out the auto cross ##################################

#######################################################################################
############################## P2********************************************************************
#######################################################################################


nparmat = nparmat2

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))
acfmax= array(0, c(nparmat,nparmat))
lagmax= array(0, c(nparmat,nparmat))

for(i in 1:nparmat){
for(j in 1:nparmat){
maxlag=which(acf2$acf[,i,j]==max(acf2$acf[-c(13:14),i,j]))
minlag=which(acf2$acf[,i,j]==min(acf2$acf[-c(13:14),i,j]))
acfa = acf2$acf[,i,j]
acfl = acf2$lag[,i,j]
if (acfa[maxlag]>(acfa[minlag]*-1)){lagmat[i,j] = acfl[maxlag]; acfmat[i,j] = acfa[maxlag]} else {lagmat[i,j] = acfl[minlag]; acfmat[i,j] = acfa[minlag]};
}
}
#*********************** Find max correlation from +/- ***********************************
for(i in 1:nparmat){
for(j in 1:nparmat){
if (abs(acfmat[i,j]) > abs(acfmat[j,i])){acfmax[i,j] = acfmat[i,j];lagmax[i,j] = lagmat[i,j]} else {acfmax[i,j] = acfmat[j,i];lagmax[i,j] = lagmat[j,i]};
}
}
rownames(acfmat) = acf2$sname
colnames(acfmat) = acf2$sname
acfmax2=acfmax
lagmax2=lagmax
acfmat2=acfmat
lagmat2=lagmat


#*********************** Matrix PDF ***********************************
#*********************** Print matrix for abs corl ***********************************
setwd(dir1)
pdf(paste("ColorMatrix_",p2,".pdf"), width= 12, height = 12)
par(ps =12,mfrow=c(2,1))

temp = acfmax
mincolr = -1
maxcolr = 1
acfmax = rbind(c(mincolr,maxcolr),acfmax[c(1:3),])
	cellcol2<-matrix(rep("#000000",nparmat*4),ncol=nparmat)
	cellcol2[acfmax<0]<-color.scale(acfmax[acfmax<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
	cellcol2[acfmax>=0]<-color.scale(acfmax[acfmax>=0],c(1,0.6,0),c(0,0),c(0,0.2,1))
color2D.matplot(acfmax[-c(1),],cellcolors=cellcol2[-c(1),],nslices=20,axes=FALSE,xlab="climate parameters",ylab="observation",
show.legend=FALSE, show.values=2,main=paste("Maximum Cross-Correlation of ",p2," paramters"))
	legval<-seq(mincolr,maxcolr,length.out=20)
	legcol<-rep("#000000",20)
	legcol[legval<0]<-color.scale(legval[legval<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
	legcol[legval>=0]<-color.scale(legval[legval>=0],c(1,0.6,0),c(0,0),c(0,0.2,1))
	color.legend(0,-.55,3.5,-.45,round(c(-1,0,1),1),rect.col=legcol)
#	color.legend(0,-1,3.5,-0.8,round(c(min(acfmax),0,max(acfmax)),1),rect.col=legcol)
acfmax = temp

par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(3-0.5):0.5,labels=rownames(acfmat)[1:3], side =4)

par(ps =12)
temp = lagmax
mincolr = -11
maxcolr = 11
lagmax = rbind(c(mincolr,maxcolr),lagmax[c(1:3),])
	cellcol<-matrix(rep("#000000",nparmat*4),ncol=nparmat)
	cellcol[lagmax<0]<-color.scale(lagmax[lagmax<0],c(1,0),c(0,1),0)
	cellcol[lagmax>=0]<-color.scale(lagmax[lagmax>=0],c(0,0),c(1,0),c(0.2,.8))
color2D.matplot(lagmax[-c(1),],cellcolors=cellcol[-c(1),],axes=FALSE,xlab="climate parameters",ylab="observation",
show.legend=FALSE, show.values=TRUE,main=paste("Lag time at maximum correlation of ",p2," parameters"))
	legval<-seq(mincolr,maxcolr,length.out=23)
#	legval<-seq(min(lagmax),max(lagmax),length.out=(max(lagmax)-min(lagmax)))
	legcol<-rep("#000000",23)
	legcol[legval<0]<-color.scale(legval[legval<0],c(1,0),c(0,1),0)
	legcol[legval>=0]<-color.scale(legval[legval>=0],c(0,0),c(1,0),c(0.2,0.8))
	color.legend(0,-.55,3.5,-.45,round(c(-11,0,11),1),rect.col=legcol)
#	color.legend(0,-1,3.5,-0.8,round(c(min(lagmax),0,max(lagmax)),1),rect.col=legcol)
lagmax = temp

par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(3-0.5):0.5,labels=rownames(acfmat)[1:3], side =4)

dev.off()
setwd(dir0)

#########print output to text-P2 ############################################
#print out some variables to file
setwd(dir1)
sink(paste("Output-",p2,".csv"))
setwd(dir0)
cat("Predictor:",",",nparmat,"\n") 
cat("----------------------------","\n")
#cat("Correlation-coefficients (for lag=0):  ",corcoef); corcoef
#cat("Regression-Summary:  "); reg_sum
#cat("\n","Lag and cross-correlation","\n")
cat("Predictor1,Predictor2,Cross-cort,Cross-cort2, Lag2, MaxCrs-Cort Lag, MaxCrs-Cort,at Lag\n")

for(i in 1:nparmat){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",",acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],"\n")
}
}
sink()



#######################################################################################
############################## P3 ********************************************************************
#######################################################################################


nparmat = nparmat3

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))
acfmax= array(0, c(nparmat,nparmat))
lagmax= array(0, c(nparmat,nparmat))


for(i in 1:nparmat){
for(j in 1:nparmat){
maxlag=which(acf3$acf[,i,j]==max(acf3$acf[-c(13:15),i,j]))
minlag=which(acf3$acf[,i,j]==min(acf3$acf[-c(13:14),i,j]))
acfa = acf3$acf[,i,j]
acfl = acf3$lag[,i,j]
if (acfa[maxlag]>(acfa[minlag]*-1)){lagmat[i,j] = acfl[maxlag]; acfmat[i,j] = acfa[maxlag]} else {lagmat[i,j] = acfl[minlag]; acfmat[i,j] = acfa[minlag]};
}
}

#*********************** Find max correlation from +/- ***********************************
for(i in 1:nparmat){
for(j in 1:nparmat){
if (abs(acfmat[i,j]) > abs(acfmat[j,i])){acfmax[i,j] = acfmat[i,j];lagmax[i,j] = lagmat[i,j]} else {acfmax[i,j] = acfmat[j,i];lagmax[i,j] = lagmat[j,i]};
}
}


rownames(acfmat) = acf3$sname
colnames(acfmat) = acf3$sname
acfmax3=acfmax
lagmax3=lagmax
acfmat3=acfmat
lagmat3=lagmat

#*********************** Matrix PDF ***********************************
#*********************** Print matrix for abs corl ***********************************
setwd(dir1)
pdf(paste("ColorMatrix_",p3,".pdf"), width= 12, height = 12)
par(ps =12,mfrow=c(2,1))

temp = acfmax
mincolr = -1
maxcolr = 1
acfmax = rbind(c(mincolr,maxcolr),acfmax[c(1:3),])
	cellcol2<-matrix(rep("#000000",nparmat*4),ncol=nparmat)
	cellcol2[acfmax<0]<-color.scale(acfmax[acfmax<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
	cellcol2[acfmax>=0]<-color.scale(acfmax[acfmax>=0],c(1,0.6,0),c(0,0),c(0,0.1,0.8))
color2D.matplot(acfmax[-c(1),],cellcolors=cellcol2[-c(1),],nslices=20,axes=FALSE,xlab="climate parameters",ylab="observation",
show.legend=FALSE, show.values=2,main=paste("Maximum Cross-Correlation of ",p3," paramters"))
	legval<-seq(mincolr,maxcolr,length.out=20)
	legcol<-rep("#000000",20)
	legcol[legval<0]<-color.scale(legval[legval<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
	legcol[legval>=0]<-color.scale(legval[legval>=0],c(1,0.6,0),c(0,0),c(0,0.1,0.8))
	color.legend(0,-.55,3.5,-.45,round(c(-1,0,1),1),rect.col=legcol)
#	color.legend(0,-1,3.5,-0.8,round(c(min(acfmax),0,max(acfmax)),1),rect.col=legcol)
acfmax = temp

par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(3-0.5):0.5,labels=rownames(acfmat)[1:3], side =4)


par(ps =12)
temp = lagmax
mincolr = -11
maxcolr = 11
lagmax = rbind(c(mincolr,maxcolr),lagmax[c(1:3),])
	cellcol<-matrix(rep("#000000",nparmat*4),ncol=nparmat)
	cellcol[lagmax<0]<-color.scale(lagmax[lagmax<0],c(1,0),c(0,1),0)
	cellcol[lagmax>=0]<-color.scale(lagmax[lagmax>=0],c(0,0),c(1,0),c(0.2,.8))
color2D.matplot(lagmax[-c(1),],cellcolors=cellcol[-c(1),],axes=FALSE,xlab="climate parameters",ylab="observation",
show.legend=FALSE, show.values=TRUE,main=paste("Lag time at maximum correlation of ",p3,"parameters"))
	legval<-seq(mincolr,maxcolr,length.out=23)
#	legval<-seq(min(lagmax),max(lagmax),length.out=(max(lagmax)-min(lagmax)))
	legcol<-rep("#000000",23)
	legcol[legval<0]<-color.scale(legval[legval<0],c(1,0),c(0,1),0)
	legcol[legval>=0]<-color.scale(legval[legval>=0],c(0,0),c(1,0),c(0.2,0.8))
	color.legend(0,-.55,3.5,-.45,round(c(-11,0,11),1),rect.col=legcol)
#	color.legend(0,-1,3.5,-0.8,round(c(min(lagmax),0,max(lagmax)),1),rect.col=legcol)
lagmax = temp

par(ps =11)
axis(1,at=0.5:(nparmat-0.5),labels=colnames(acfmat))
axis(2,at=(3-0.5):0.5,labels=rownames(acfmat)[1:3], side =4)


dev.off()
setwd(dir0)


#########print output to text-P3 ############################################
#print out some variables to file
#*********************** for print max output ***********************************
setwd(dir1)
sink(paste("Output-",p3,".csv"))
setwd(dir0)
cat("Predictor:",",",nparmat,"\n") 
cat("----------------------------","\n")
#cat("Correlation-coefficients (for lag=0):  ",corcoef); corcoef
#cat("Regression-Summary:  "); reg_sum
#cat("\n","Lag and cross-correlation","\n")
cat("Predictor1,Predictor2,Cross-cort1, Lag1, Cross-cort2, Lag2, MaxCrs-Cort,at Lag\n")

for(i in 1:nparmat){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",",acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],"\n")
}
}
sink()



#######################################################################################
#print every time-lag correlation output to chart ############################################
#######################################################################################

#*********************** for print all correlation ***********************************
#########P2############################################
nparmat = nparmat2
tccf= array(0, c(3,nparmat,length(ccf(submt2[,1], submt2[,1], plot = FALSE)$acf)))

for(i in 1:3){
for(j in 1:nparmat){
tccf[i,j,]=ccf(submt2[,i], submt2[,j], plot = FALSE)$acf
}
}

setwd(dir1)
pdf(paste("Corr-line_",p2,".pdf"), width= 15, height = 15)

for(i in 1:3){
	par(mfrow=c(5,3))
	for(j in 1:nparmat){
		plot(c(-14:14),tccf[i,j,c(9:37)], main = acf2$sname[i],xlab = "Lag", ylab = "", ylim=c(-1,1))
		mtext(acf2$sname[j])
	}
}
dev.off()
setwd(dir0)




#########P3############################################
nparmat = nparmat3
tccf= array(0, c(3,nparmat,length(ccf(submt3[,1], submt3[,1], plot = FALSE)$acf)))

for(i in 1:3){
for(j in 1:nparmat){
tccf[i,j,]=ccf(submt3[,i], submt3[,j], plot = FALSE)$acf
}
}

setwd(dir1)
pdf(paste("Corr-line_",p3,".pdf"), width= 15, height = 15)

for(i in 1:3){
	par(mfrow=c(5,3))
	for(j in 1:nparmat){
		plot(c(-14:14),tccf[i,j,c(9:37)], main = acf2$sname[i],xlab = "Lag", ylab = "", ylim=c(-1,1))
		mtext(acf2$sname[j])
	}
}
dev.off()
setwd(dir0)





#######################################################################################
############################## Print correlation time-series***************************************
#######################################################################################
#*********************** Loop for ploting the cross-correlation ***********************************
#P2
setwd(dir1)
pdf(paste("Corr-hist_Obs-",p2,".pdf"))
for(i in 1:3){
par(mfrow=c(4,2))
for(j in 1:nparmat2){
ccf(submt2[,i], submt2[,j] ,main=paste(colnames(submt2)[i],"-",colnames(submt2)[j]))
}
}
dev.off()
setwd(dir0)

#P3
setwd(dir1)
pdf(paste("Corr-hist_Obs-",p3,".pdf"))
for(i in 1:3){
par(mfrow=c(4,2))
for(j in 1:nparmat3){
ccf(submt3[,i], submt3[,j] ,main=paste(colnames(submt3)[i],"-",colnames(submt3)[j]))
}
par(mfrow=c(4,1))
}
dev.off()
setwd(dir0)


#######################################################################################
############################## Print correlation time-series***************************************
#######################################################################################
#*********************** Loop for ploting the cross-correlation ***********************************
ts2 <-ts(submt2,start=startyear,frequency=12)
ts3 <-ts(submt3,start=startyear,frequency=12)

#P2
nparmat=nparmat2
ts=ts2
submt=submt2

setwd(dir1)
pdf(paste("TS_Obs-",p2,".pdf"),width= 12,height = 2*nparmat)
par(mfrow=c(nparmat,1),ps=10)

for(i in 1:3){
for(j in 1:nparmat){
plot(ts[,i],main=paste(colnames(submt)[i],"(Blue) - ",colnames(submt)[j],"(Red)"),xaxt = "n",xlab="",ylab=colnames(submt)[i],lty=1,col=4,lwd=0.5,xaxt = "n")
jan <- time(ts)[cycle(ts[,i]) == 1]  # january
mtick <- matrix(time(ts),nrow=12)[c(1,4,7,10),] # 3 month
abline(v = jan, lty = 2)
axis(1, mtick, FALSE, tcl = -0.2)
axis(1, jan, TRUE, tcl = -0.5)


par(new=TRUE)
plot(ts[,j],main="",xlab="",ylab="",axes=FALSE,lty=1,col=2,lwd=0.5)
}
}
dev.off()
setwd(dir0)

#P3
setwd(dir1)
nparmat=nparmat3
ts=ts3
submt=submt3

pdf(paste("TS_Obs-",p3,".pdf"),width= 12,height = 2*nparmat)
par(mfrow=c(nparmat,1),ps=10)

for(i in 1:3){
for(j in 1:nparmat){
plot(ts[,i],main=paste(colnames(submt)[i],"(Blue) - ",colnames(submt)[j],"(Red)"),xaxt = "n",xlab="",ylab=colnames(submt)[i],lty=1,col=4,lwd=0.5,xaxt = "n")
jan <- time(ts)[cycle(ts[,i]) == 1]  # january
mtick <- matrix(time(ts),nrow=12)[c(1,4,7,10),] # 3 month
abline(v = jan, lty = 2)
axis(1, mtick, FALSE, tcl = -0.2)
axis(1, jan, TRUE, tcl = -0.5)
par(new=TRUE)
plot(ts[,j],main="",xlab="",ylab="",axes=FALSE,lty=1,col=2,lwd=0.5)
}
}
dev.off()
setwd(dir0)


#######################################################################################
############################## Print correlation time-series to text*******************
#######################################################################################

#P2
setwd(dir1)
sink(paste("All-AutoCorrel-",p2,".csv"))
setwd(dir0)
nparmat=nparmat2
acf = acf2
acfmax=acfmax2
lagmax=lagmax2
acfmat=acfmat2
lagmat=lagmat2


cat("Predictor:",",",nparmat,"\n") 
cat("Predictor1,Predictor2,")
for(k in length(acf$acf[,2,1]):1){cat("Lag",acf$lag[k,2,1],",")}
for(k in 2:length(acf$acf[,1,2])){cat("Lag",acf$lag[k,1,2],",")}
cat("CorrMax,Max at,CorrMin,Min at,OptmCorr, Optm at,\n")
for(i in 1:3){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",")
for(k in length(acf$acf[,j,i]):1){cat(acf$acf[k,j,i],",")}
for(k in 2:length(acf$acf[,i,j])){cat(acf$acf[k,i,j],",")}
cat(acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],"\n")
}
}
sink()

#P3
setwd(dir1)
sink(paste("All-AutoCorrel-",p3,".csv"))
setwd(dir0)
nparmat=nparmat3
acf = acf3
acfmax=acfmax3
lagmax=lagmax3
acfmat=acfmat3
lagmat=lagmat3


cat("Predictor:",",",nparmat,"\n") 
cat("Predictor1,Predictor2,")
for(k in length(acf$acf[,2,1]):1){cat("Lag",acf$lag[k,2,1],",")}
for(k in 2:length(acf$acf[,1,2])){cat("Lag",acf$lag[k,1,2],",")}
cat("CorrMax,Max at,CorrMin,Min at,OptmCorr, Optm at,\n")
for(i in 1:3){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",")
for(k in length(acf$acf[,j,i]):1){cat(acf$acf[k,j,i],",")}
for(k in 2:length(acf$acf[,i,j])){cat(acf$acf[k,i,j],",")}
cat(acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],"\n")
}
}
sink()


#########################################################################################
######################################### multiple linear regression ####################
#########################################################################################

detach(data)

calerror = array(-99, c(4,6,6))
prderror = array(-99, c(4,6,6))
resultc =  array(-99, c(length(data[,1])-1,6))
opteq	  =  array(-99,c(max(nmaxprd2,nmaxprd3),6))

for(loop in 1:6){
############################################ START big loop of multi regression $$$$$$$$$$$$$$
setwd(dir1)

## set nmaxprd
if(loop%%2==1) nmaxprd = nmaxprd2 else nmaxprd = nmaxprd3


################################
##### min temp - P2  ##########
################################

if(loop==1){
mydata=data2[,-c(2:3)]
pdf(paste("RegressionMx-",p2,".pdf"), width= 12, height = 12)
predictant = "ObsMxT"
model = p2
}

#################################
##### max temp - P3  ############
#################################

if(loop==2){
mydata=data3[,-c(2:3)]
pdf(paste("RegressionMx-",p3,".pdf"), width= 12, height = 12)
predictant = "ObsMxT"
model = p3
}

################################
##### min temp - P2  ##########
################################

if(loop==3){
mydata=data2[,-c(1,3)]
pdf(paste("RegressionMn-",p2,".pdf"), width= 12, height = 12)
predictant = "ObsMnT"
model = p2
}


################################
##### min temp - P3  ###########
################################

if(loop==4){
mydata=data3[,-c(1,3)]
pdf(paste("RegressionMn-",p3,".pdf"), width= 12, height = 12)
predictant = "ObsMnT"
model = p3
}

################################
##### Rain - P2  ##############
################################
if(loop==5){
mydata=data2[,-c(1,2)]
pdf(paste("RegressionRn-",p2,".pdf"), width= 12, height = 12)
predictant = "ObsRn"
model = p2
}

################################
##### Rain - P3  ###############
################################
if(loop==6){
mydata=data3[,-c(1,2)]
pdf(paste("RegressionRn-",p3,".pdf"), width= 12, height = 12)
predictant = "ObsRn"
model = p3
}

predictor_eq = mydata[,1]
mydata=mydata[,-c(1)]


####################### ++ title on page ++
par(ps =12,mfrow=c(1,1))
plot.new()
text(0,0.5,adj=c(0,0),lab=paste("Multiple\nLinear\nRegression\n\n\n# CGCM3-",model,"\n# predictant : ",predictant),cex=3)

####################### ++ title on page ++
par(ps =12,mfrow=c(1,1))
plot.new()
text(0,0,adj=c(0,0),lab= "Predictor selection (by 4 methods)",cex=2)

par(ps =8)
leaps = regsubsets(predictor_eq~.,data=mydata)
#subsets(leaps, statistic="rsq",legend=FALSE,max.size=8,abbrev=6, main="Sets of predictors and the regression")
vorder=array(leaps$vorder,c(length(leaps$vorder),4))
method=array(leaps$method,c(length(leaps$method),4))

########################################## Method of selection - bar charts
par(ps =16,mfrow=c(4,1))

#leaps = regsubsets(mydata[,1]~.,data=mydata,method=c("exhaustive", "backward", "forward", "seqrep"),nbest=1)
leaps = regsubsets(predictor_eq~.,data=mydata,method=c("exhaustive"),nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: exhaustive"),scale="r2") 

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("backward"),nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: backward"),scale="r2") 

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("forward"),nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: forward"),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("seqrep"),nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: seqrep"),scale="r2") 


########################################## Method of selection - line charts
par(ps =9,mfrow=c(1,1))

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("exhaustive"),nbest=1)
vorder[,1]=leaps$vorder
method[1]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection method: exhaustive"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6) 

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("backward"),nbest=1)
vorder[,2]=leaps$vorder
method[2]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: backward"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("forward"),nbest=1)
vorder[,3]=leaps$vorder
method[3]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: forward"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("seqrep"),nbest=1)
vorder[,4]=leaps$vorder
method[4]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: seqrep"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)

########################################## Number of  predictors
par(ps =12,mfrow=c(3,3))

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=2)
plot(leaps, main=paste("Selection of 2 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=3)
plot(leaps, main=paste("Selection of 3 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=4)
plot(leaps, main=paste("Selection of 4 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=5)
plot(leaps, main=paste("Selection of 5 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=6)
plot(leaps, main=paste("Selection of 6 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=7)
plot(leaps, main=paste("Selection of 7 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=8)
plot(leaps, main=paste("Selection of 8 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=9)
plot(leaps, main=paste("Selection of 9 best predictors for",predictant),scale="r2")

########################################## Set of prediction
#par(ps =12,mfrow=c(2,2))
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=4,method=method[1])
#plot(leaps, main=paste(method[1]," method to optimize 4 set of predictors for",predictant),scale="r2",df=4)
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=2,method=method[2])
#plot(leaps, main=paste(method[2]," method to optimize 4 set of predictors for",predictant),scale="r2")
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=3,method=method[3])
#plot(leaps, main=paste(method[3]," method to optimize 4 set of predictors for",predictant),scale="r2")
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=4,method=c("seqrep"))
#plot(leaps, main=paste(method[4]," method to optimize 4 set of predictors for",predictant),scale="r2")



#fit <- lm(predictor_eq~.,data=vdata,nvmax=1)
#fita=array(anova(fit),c(length(anova(fit)),4))


######################################################################################
######******************** compare model and validation ##############################
######################################################################################

######******************** data for validation here ##################################
start0=1971  #year start in table file
startyear=2001 #year to analysis begins
endyear=2005
endmonth=3
ifinal=min(which(data$year==(endyear+1)))-1 + endmonth
datafinal = length(data$year)

#P2
#data2=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:50,58:63)]
#P3
#data3=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:57)]

subty = (startyear-start0)*12
#P2
vdata2=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:50,58:63)]
#P3
vdata3=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:57)]

######################################################################################

####################### ++ title on page ++
par(ps =12,mfrow=c(1,1))
plot.new()
text(0,0,adj=c(0,0),lab= "Comaparing the most fit models from 4 methods",cex=2)


vorder = vorder[-c(1),]
vorder = vorder - 1

for (i in 1:4){
	vdata = mydata[,vorder[1:nmaxprd,i]]
	fit <- lm(predictor_eq~.,data=vdata)
	layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
	plot(fit,main=paste("Modeling by ",method[i]," method\nprediction of ",predictant," from CGCM3-",model))

	par(ps =12,mfrow=c(1,1))
	if(model==p2){vrf=vdata2}
	if(model==p3){vrf=vdata3}
	prd=predict(fit,vrf)
#check conditional rainfall
if(loop>=5){for (m in 1:length(prd)){if(prd[m]<0){prd[m]=0}}}

	lmx = max(max(prd),max(vrf[which(predictant==colnames(vrf))]))
	lmn = min(min(prd),min(vrf[which(predictant==colnames(vrf))]))
	matplot(vrf[which(predictant==colnames(vrf))],prd,pch="X",xlab="Observation",ylab="Prediction",xlim=c(lmn,lmx),ylim=c(lmn,lmx),type="p",col = "red", main=paste("Model verification 2001-2006 by ",method[i]," method\nprediction of ",predictant," from CGCM3-",model))
	matlines (c(-100,100), c(-100,100), type = "l", lty = "dashed", lwd = 1, pch = NULL, col = 1:2)

#***********calibration summary result
if(model==p2){
			calf =predict(fit,data2)
			cal = cbind(data2[which(predictant==colnames(vrf))],calf)
		    }
if(model==p3){
			calf = predict(fit,data3)
			cal = cbind(data3[which(predictant==colnames(vrf))],calf)
		    }
	calerror[i,1,loop] = predictant
	calerror[i,2,loop] = model
	calerror[i,3,loop] = mean(cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),2]-cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),1],na.rm = TRUE)
	calerror[i,4,loop] = mean(abs(cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),2]-cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),1]))
	calerror[i,5,loop] = sqrt(mean((cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),2]-cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),1])^2))
	calerror[i,6,loop] = 1-sum((cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),2]-cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),1])^2)/sum((cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),1]-mean(cal[which(is.na(cal[,1])== FALSE & is.na(cal[,2])==FALSE),1]))^2)


#***********validation summary result
prd =cbind(vrf[which(predictant==colnames(vrf))],prd)
	
	prderror[i,1,loop] = predictant
	prderror[i,2,loop] = model
	prderror[i,3,loop] = mean(prd[,2]-prd[,1])
	prderror[i,4,loop] = mean(abs(prd[,2]-prd[,1]))
	prderror[i,5,loop] = sqrt(mean((prd[,2]-prd[,1])^2))
	prderror[i,6,loop] = 1-sum((prd[,2]-prd[,1])^2)/sum((prd[,1]-mean(prd[,1]))^2)
}

####################### ++ conclusion on page ++
par(ps =9,mfrow=c(2,1))
# print result to text

plot.new()
title("Conclusion for comparing the models",cex.main=2) 
for (i in 1:4){
vdata = mydata[,vorder[1:nmaxprd,i]]
fit <- lm(predictor_eq~.,data=vdata)
a=capture.output(fit$coefficients)
text(0,1-(i-1)/3.5,adj=c(0,1),lab=paste("Model ",i,": by ",method[i]," method to predict ",predictant,"\n",a[1],"\n",a[2],"\n"))
}

plot.new()
#plot.window(xlim=c(0,1),ylim=c(0,1))


atext = capture.output(anova(lm(predictor_eq~.,data=mydata[,vorder[1:nmaxprd,1]]),lm(predictor_eq~.,data=mydata[,vorder[1:nmaxprd,2]]),lm(predictor_eq~.,data=mydata[,vorder[1:nmaxprd,3]]),lm(predictor_eq~.,data=mydata[,vorder[1:nmaxprd,4]])))

for (i in 1:length(atext)){
text(0,1-i/length(atext),adj=c(0,1),lab= atext[i])
}

#define optimal equation
opteq[1:nmaxprd,loop] = vorder[1:nmaxprd,min(which(calerror[,5,loop]==min(calerror[,5,loop])))]

setwd(dir0)
dev.off()

}
############################################ END big loop of multi regression $$$$$$$$$$$$$$

######################################################################################
######******************** print multi-regression models and validations #############
######################################################################################

#***********print calibration summary result
setwd(dir1)
sink("Concl-Mlt-Rgrssn-prd.csv")
setwd(dir0)
cat("Predictant,P2/P3,Mothed,MERROR,ABSERROR,RMSE,Nash–Sutcliffe\n") 
for(i in 1:6){
	for(mthd in 1:4){
		cat(prderror[mthd,1,i],",",prderror[mthd,2,i],",",method[mthd],",",prderror[mthd,3,i],",",prderror[mthd,4,i],",",prderror[mthd,5,i],",",prderror[mthd,6,i],"\n")
			 }
		 }
sink()

#***********print validation summary result
setwd(dir1)
sink("Concl-Mlt-Rgrssn-cal.csv")
setwd(dir0)
cat("Predictant,P2/P3,Mothed,MERROR,ABSERROR,RMSE,Nash–Sutcliffe\n") 
for(i in 1:6){
	for(mthd in 1:4){
		cat(calerror[mthd,1,i],",",calerror[mthd,2,i],",",method[mthd],",",calerror[mthd,3,i],",",calerror[mthd,4,i],",",calerror[mthd,5,i],",",calerror[mthd,6,i],"\n")
			 }
		 }
sink()

#####################################
########## print time-series results
for(i in 1:6){
		if(i%%2==1){mydata=data2[,-c(1:3)]}
		if(i%%2==0){mydata=data3[,-c(1:3)]}
		predictor_eq = data2[,(i+i%%2)/2]
		
		if(loop%%2==1) nmaxprd = nmaxprd2 else nmaxprd = nmaxprd3
		vdata = mydata[,opteq[1:nmaxprd,i]]
		fit <- lm(predictor_eq~.,data=vdata)
		resultc[,i] =  predict(fit,data[-c(length(data[,1])),])
		}
setwd(dir1)
sink("Regression-Result.dat")
setwd(dir0)
cat(calerror[1,1,1],"-",calerror[1,2,1],",",calerror[1,1,2],"-",calerror[1,2,2],",",calerror[1,1,3],"-",calerror[1,2,3],",",calerror[1,1,4],"-",calerror[1,2,4],",",calerror[1,1,5],"-",calerror[1,2,5],",",calerror[1,1,6],"-",calerror[1,2,6],"\n")
print(resultc)
sink()

###################
###### AR #########
###################
rsar = array(NA, c(3,5,8,34))
mth=c("yule-walker", "burg", "ols", "mle", "yw")
for(i in 1:3){
		for(j in 1:5){
				for(k in 1:8){
						nona = which(is.na(data2[,i])== FALSE)
						if (i == 3) fit = ar(data2[nona,i],order.max=k*2,method=mth[j]) else fit = ar(data2[nona,i],order.max=k*3,method=mth[j])
						rsar[i,j,k,1] = colnames(data2)[i]
						rsar[i,j,k,2] = mth[j]
						if (i == 3) rsar[i,j,k,3] = k*2 else rsar[i,j,k,3] = k*3
						rsar[i,j,k,4] = fit$order
						rsar[i,j,k,5] = fit$var.pred
						nonars = which(is.na(fit$resid) == FALSE)
						rsar[i,j,k,6] = mean(fit$resid[nonars])
						rsar[i,j,k,7] = sqrt(mean((fit$resid[nonars])^2))
						rsar[i,j,k,8] = 1-sum((fit$resid[nonars])^2)/sum((data2[nona[nonars],i]-mean(data2[nona[nonars],i]))^2)
						rsar[i,j,k,9] = fit$n.used
						rsar[i,j,k,10] = length(nonars)
						for(l in 1:12) rsar[i,j,k,10+l] = fit$ar[l]
						if(j != 3 & j != 4) for(l in 1:12) rsar[i,j,k,10+12+l] = fit$partialacf[l]
						}
				}
		 }

setwd(dir1)
sink(paste("AR.csv"))
setwd(dir0)

cat("Obs,method,max_order,order,PredictionVariance,meanResid,rmsResid,Nash–Sutcliffe,n,nPredicted")
for(j in 1:12) cat(paste(",coef[",j,"]"))
for(j in 1:12) cat(paste(",PACF[",j,"]"))
cat("\n")

for(i in 1:3){
		for(j in 1:5){
				for(k in 1:8){
						for(l in 1:34)cat(paste(rsar[i,j,k,l],","))
						cat("\n")
						}
				}
		}
sink()


###################
###### ARIMA ######
###################
rsarm = array(NA, c(3,5,8,19))

for(i in 1:3){
		for(j in 1:5){
				for(k in 1:8){
						nona = which(is.na(data2[,i])== FALSE)
						fit = auto.arima(data2[nona,i],max.p=j-1,start.q=(k-1)*2,stepwise=TRUE)
						rsarm[i,j,k,1] = colnames(data2)[i]
						rsarm[i,j,k,2] = j-1
						rsarm[i,j,k,3] = (k-1)*2
						rsarm[i,j,k,4] = fit$arma[1] #p
						rsarm[i,j,k,5] = fit$arma[6] #q
						rsarm[i,j,k,6] = fit$arma[2] #d
						rsarm[i,j,k,7] = fit$coef[1]
						rsarm[i,j,k,8] = fit$coef[2]
						rsarm[i,j,k,9] = fit$coef[3]
						rsarm[i,j,k,10] = fit$coef[5]
						nonars = which(is.na(fit$residuals) == FALSE)
						rsarm[i,j,k,11] = mean(fit$residuals[nonars])
						rsarm[i,j,k,12] = sqrt(mean((fit$residuals[nonars])^2))
						rsarm[i,j,k,13] = 1-sum((fit$residuals[nonars])^2)/sum((data2[nona[nonars],i]-mean(data2[nona[nonars],i]))^2)
						rsarm[i,j,k,14] = fit$sigma2
						rsarm[i,j,k,15] = fit$aic
						rsarm[i,j,k,16] = fit$aicc
						rsarm[i,j,k,17] = fit$bic
						rsarm[i,j,k,18] = length(nona)
						rsarm[i,j,k,19] = length(nonars)
						}
				}
		 }
setwd(dir1)
sink(paste("ARIMA.csv"))
setwd(dir0)

cat("Obs,max_p,start_q,p,d,q,ar1,ar2,ma1,reg,meanResid,rmsResid,Nash–Sutcliffe,sigma2,AIC,AICc,BIC,n,nPredicted\n")
for(i in 1:3){
		for(j in 1:(5)){
				for(k in 1:8){
							for(l in 1:19) cat(paste(rsarm[i,j,k,l],","))
							cat("\n")
							}
				}
		 }
sink()

#####################################
###### ARIMA - non stepwise ######
#####################################
rsarmns = array(-99, c(3,5,18))

max = nmaxprd2

for(i in 1:3){
		for(j in 1:5) {
						nona = which(is.na(data2[,i])== FALSE)
						fit = auto.arima(data2[nona,i], max.order = j*3, stepwise=FALSE)
						rsarmns[i,j,1] = colnames(data2)[i]
						rsarmns[i,j,2] = j*3
						rsarmns[i,j,3] = fit$arma[1] #p
						rsarmns[i,j,4] = fit$arma[6] #q
						rsarmns[i,j,5] = fit$arma[2] #d
						rsarmns[i,j,6] = fit$coef[1]
						rsarmns[i,j,7] = fit$coef[2]
						rsarmns[i,j,8] = fit$coef[3]
						rsarmns[i,j,9] = fit$coef[5]
						nonars = which(is.na(fit$residuals) == FALSE)
						rsarmns[i,j,10] = mean(fit$residuals[nonars])
						rsarmns[i,j,11] = sqrt(mean((fit$residuals[nonars])^2))
						rsarmns[i,j,12] = 1-sum((fit$residuals[nonars])^2)/sum((data2[nona[nonars],i]-mean(data2[nona[nonars],i]))^2)
						rsarmns[i,j,13] = fit$sigma2
						rsarmns[i,j,14] = fit$aic
						rsarmns[i,j,15] = fit$aicc
						rsarmns[i,j,16] = fit$bic
						rsarmns[i,j,17] = length(nona)
						rsarmns[i,j,18] = length(nonars)
						}
		 }
setwd(dir1)
sink(paste("ARIMA-nonstepwise.csv"))
setwd(dir0)

cat("Obs,MaxOrder,p,d,q,ar1,ar2,ma1,reg,meanResid,rmsResid,Nash–Sutcliffe,sigma2,AIC,AICc,BIC,n,nPredicted\n")
for(i in 1:3){
		for(j in 1:5){
							for(k in 1:18) cat(paste(rsarmns[i,j,k],","))
							cat("\n")
							}
		 }
sink()

###############################
###### ARIMAexternal ######
###############################

exarm = array(-99, c(3,nmaxprd2+nmaxprd3,5,8,20))

max = nmaxprd2

for(i in 1:3){
		for(j in 4:(max+3)){
				for(k in 1:5){
					for(l in 1:8){
							nona = which(is.na(data2[,i])== FALSE & is.na(data2[,j])==FALSE)
							fit = auto.arima(data2[nona,i],xreg=data2[nona,j],max.p=k-1,start.q=(l-1)*2,stepwise=TRUE)
							exarm[i,j-3,k,l,1] = colnames(data2)[i]
							exarm[i,j-3,k,l,2] = colnames(data2)[j]
							exarm[i,j-3,k,l,3] = k-1
							exarm[i,j-3,k,l,4] = (l-1)*2
							exarm[i,j-3,k,l,5] = fit$arma[1] #p
							exarm[i,j-3,k,l,6] = fit$arma[6] #q
							exarm[i,j-3,k,l,7] = fit$arma[2] #d
							exarm[i,j-3,k,l,8] = fit$coef[1]
							exarm[i,j-3,k,l,9] = fit$coef[2]
							exarm[i,j-3,k,l,10] = fit$coef[3]
							exarm[i,j-3,k,l,11] = fit$coef[5]
							nonars = which(is.na(fit$residuals) == FALSE)
							exarm[i,j-3,k,l,12] = mean(fit$residuals[nonars])
							exarm[i,j-3,k,l,13] = sqrt(mean((fit$residuals[nonars])^2))
							exarm[i,j-3,k,l,14] = 1-sum((fit$residuals[nonars])^2)/sum((data2[nona[nonars],i]-mean(data2[nona[nonars],i]))^2)
							exarm[i,j-3,k,l,15] = fit$sigma2
							exarm[i,j-3,k,l,16] = fit$aic
							exarm[i,j-3,k,l,17] = fit$aicc
							exarm[i,j-3,k,l,18] = fit$bic
							exarm[i,j-3,k,l,19] = length(nona)
							exarm[i,j-3,k,l,20] = length(nonars)
							}
						}
					}
		 }

max = nmaxprd3
for(i in 1:3){
		for(j in 4:(max+3)){
				for(k in 1:5){
					for(l in 1:8){
							nona = which(is.na(data3[,i])== FALSE & is.na(data3[,j])==FALSE)
							fit = auto.arima(data3[nona,i],xreg=data3[nona,j],max.p=k-1,start.q=(l-1)*2,stepwise=TRUE)
							exarm[i,j-3+nmaxprd2,k,l,1] = colnames(data3)[i]
							exarm[i,j-3+nmaxprd2,k,l,2] = colnames(data3)[j]
							exarm[i,j-3+nmaxprd2,k,l,3] = k-1
							exarm[i,j-3+nmaxprd2,k,l,4] = (l-1)*2
							exarm[i,j-3+nmaxprd2,k,l,5] = fit$arma[1] #p
							exarm[i,j-3+nmaxprd2,k,l,6] = fit$arma[6] #q
							exarm[i,j-3+nmaxprd2,k,l,7] = fit$arma[2] #d
							exarm[i,j-3+nmaxprd2,k,l,8] = fit$coef[1]
							exarm[i,j-3+nmaxprd2,k,l,9] = fit$coef[2]
							exarm[i,j-3+nmaxprd2,k,l,10] = fit$coef[3]
							exarm[i,j-3+nmaxprd2,k,l,11] = fit$coef[5]
							nonars = which(is.na(fit$residuals) == FALSE)
							exarm[i,j-3+nmaxprd2,k,l,12] = mean(fit$residuals[nonars])
							exarm[i,j-3+nmaxprd2,k,l,13] = sqrt(mean((fit$residuals[nonars])^2))
							exarm[i,j-3+nmaxprd2,k,l,14] = 1-sum((fit$residuals[nonars])^2)/sum((data3[nona[nonars],i]-mean(data3[nona[nonars],i]))^2)
							exarm[i,j-3+nmaxprd2,k,l,15] = fit$sigma2
							exarm[i,j-3+nmaxprd2,k,l,16] = fit$aic
							exarm[i,j-3+nmaxprd2,k,l,17] = fit$aicc
							exarm[i,j-3+nmaxprd2,k,l,18] = fit$bic
							exarm[i,j-3+nmaxprd2,k,l,19] = length(nona)
							exarm[i,j-3+nmaxprd2,k,l,20] = length(nonars)
							}
						}
					}
		 }

setwd(dir1)
sink(paste("ARIMAex.csv"))
setwd(dir0)

cat("Obs,Predictor,max_p,start_q,p,d,q,ar1,ar2,ma1,reg,meanResid,rmsResid,Nash–Sutcliffe,sigma2,AIC,AICc,BIC,n,nPredicted\n")
for(i in 1:3){
		for(j in 1:(nmaxprd2+nmaxprd3)){
						for(k in 1:5){
							for(l in 1:8){
									for(m in 1:20) cat(paste(exarm[i,j,k,l,m],","))
									cat("\n")
									}
								}
							}
		 }
sink()


###########################################
###### ARIMAexternal - non stepwise ######
###########################################
exarmns = array(-99, c(3,nmaxprd2+nmaxprd3,5,19))

max = nmaxprd2

for(i in 1:3){
		for(j in 4:(max+3)){
				for(k in 1:5){
						nona = which(is.na(data2[,i])== FALSE & is.na(data2[,j])==FALSE)
						fit = auto.arima(data2[nona,i],xreg=data2[nona,j],max.order = k*3,stepwise=FALSE)
						exarmns[i,j-3,k,1] = colnames(data2)[i]
						exarmns[i,j-3,k,2] = colnames(data2)[j]
						exarmns[i,j-3,k,3] = k*3
						exarmns[i,j-3,k,4] = fit$arma[1] #p
						exarmns[i,j-3,k,5] = fit$arma[6] #q
						exarmns[i,j-3,k,6] = fit$arma[2] #d
						exarmns[i,j-3,k,7] = fit$coef[1]
						exarmns[i,j-3,k,8] = fit$coef[2]
						exarmns[i,j-3,k,9] = fit$coef[3]
						exarmns[i,j-3,k,10] = fit$coef[5]
						nonars = which(is.na(fit$residuals) == FALSE)
						exarmns[i,j-3,k,11] = mean(fit$residuals[nonars])
						exarmns[i,j-3,k,12] = sqrt(mean((fit$residuals[nonars])^2))
						exarmns[i,j-3,k,13] = 1-sum((fit$residuals[nonars])^2)/sum((data2[nona[nonars],i]-mean(data2[nona[nonars],i]))^2)
						exarmns[i,j-3,k,14] = fit$sigma2
						exarmns[i,j-3,k,15] = fit$aic
						exarmns[i,j-3,k,16] = fit$aicc
						exarmns[i,j-3,k,17] = fit$bic
						exarmns[i,j-3,k,18] = length(nona)
						exarmns[i,j-3,k,19] = length(nonars)
						}
					}
		 }

max = nmaxprd3
for(i in 1:3){
		for(j in 4:(max+3)){
				for(k in 1:5){
						nona = which(is.na(data3[,i])== FALSE & is.na(data3[,j])==FALSE)
						fit = auto.arima(data3[nona,i],xreg=data3[nona,j],max.order = k*3,stepwise=FALSE)
						exarmns[i,j-3+nmaxprd2,k,1] = colnames(data3)[i]
						exarmns[i,j-3+nmaxprd2,k,2] = colnames(data3)[j]
						exarmns[i,j-3+nmaxprd2,k,3] = k*3
						exarmns[i,j-3+nmaxprd2,k,4] = fit$arma[1] #p
						exarmns[i,j-3+nmaxprd2,k,5] = fit$arma[6] #q
						exarmns[i,j-3+nmaxprd2,k,6] = fit$arma[2] #d
						exarmns[i,j-3+nmaxprd2,k,7] = fit$coef[1]
						exarmns[i,j-3+nmaxprd2,k,8] = fit$coef[2]
						exarmns[i,j-3+nmaxprd2,k,9] = fit$coef[3]
						exarmns[i,j-3+nmaxprd2,k,10] = fit$coef[5]
						nonars = which(is.na(fit$residuals) == FALSE)
						exarmns[i,j-3+nmaxprd2,k,11] = mean(fit$residuals[nonars])
						exarmns[i,j-3+nmaxprd2,k,12] = sqrt(mean((fit$residuals[nonars])^2))
						exarmns[i,j-3+nmaxprd2,k,13] = 1-sum((fit$residuals[nonars])^2)/sum((data2[nona[nonars],i]-mean(data2[nona[nonars],i]))^2)
						exarmns[i,j-3+nmaxprd2,k,14] = fit$sigma2
						exarmns[i,j-3+nmaxprd2,k,15] = fit$aic
						exarmns[i,j-3+nmaxprd2,k,16] = fit$aicc
						exarmns[i,j-3+nmaxprd2,k,17] = fit$bic
						exarmns[i,j-3+nmaxprd2,k,18] = length(nona)
						exarmns[i,j-3+nmaxprd2,k,19] = length(nonars)
						}
					}
		 }
setwd(dir1)
sink(paste("ARIMAex-nonstepwise.csv"))
setwd(dir0)

cat("Obs,Predictor,max_order,p,d,q,ar1,ar2,ma1,reg,meanResid,rmsResid,sigma2,AIC,AICc,BIC,n\n")
for(i in 1:3){
		for(j in 1:(nmaxprd2+nmaxprd3)){
							for(k in 1:5){
								for(l in 1:19) cat(paste(exarmns[i,j,k,l],","))
								cat("\n")
									}
							}
		 }
sink()




#######################################################
#######################################################

setwd(dir0)


#for selection parameters
#P2
#data2=data[-c(0:subty,ifinal+1),-c(1,2,5:6,8:15)]
#P3
#data3=data[-c(0:subty,ifinal+1),-c(1,2,5:6,16:24)]


###################
#### appendix #####
########################### plot correlative in matrix form ##############################
#for(i in 1:3){
#par(ps =9)
#	cellcol<-matrix(rep("#000000",nparmat*nparmat),nrow=nparmat)
#	cellcol[tccf[i,,]<0]<-color.scale(tccf[i,,][tccf[i,,]<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
#	cellcol[tccf[i,,]>=0]<-color.scale(tccf[i,,][tccf[i,,]>=0],c(1,0.6,0),c(0,0),c(0,0.2,1))
#@ change here too
#color2D.matplot(tccf[i,,],cellcolors=cellcol,nslices=20,axes=FALSE,xlab="climate parameters",ylab="",
#show.legend=FALSE, show.values=2,main=paste("Cross-Correlation of ",p2," vs", acf2$sname[i] ," at every lag time"))
#	legval<-seq(min(tccf[i,,]),max(tccf[i,,]),length.out=20)
#	legcol<-rep("#000000",20)
#	legcol[legval<0]<-color.scale(legval[legval<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
#	legcol[legval>=0]<-color.scale(legval[legval>=0],c(1,0.6,0),c(0,0),c(0,0.2,1))
#	color.legend(0,-1,3.5,-0.8,round(c(-1,0,1),1),rect.col=legcol)

#par(ps =11)
#axis(1,at=0.5:(45-0.5),labels=-22:22)
#@ change here too
#axis(2,at=(nparmat-0.5):0.5,labels=acf2$sname, side =4)
#}




