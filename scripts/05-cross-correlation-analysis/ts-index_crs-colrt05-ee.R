#
#Program to product WAVELET analysis based multi-table data
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
source("wavelet.R")

dir_name="ts-autocrt05-ee"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","cross-correlation","dummy"))
dir1=dirname(file.path("D:","Temp","cross-correlation",paste(dir_name),"dummy"))

###########################
## DATA for Regional Index
###########################
############# spatial info of index #########################
start0=1950  #year start in table file
startyear=1971 #year to analysis begins
startmonth=1
startmonth0=1
endyear=2007
endmonth=0

## Define column to define indices which is needed
selectd.col = c(51:63)

dataindex <- read.table("Daten_Rayong1950-2100+Index2+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

ifinal=min(which(dataindex$year==(endyear+1)))-1 + endmonth
datafinal = length(dataindex$year)
subty = (startyear-start0)*12

#******************** remove unwanted column here ##################################
dataindex0 =dataindex[-c(0:subty,(ifinal+1):datafinal),selectd.col]

dataindexts =  ts(dataindex0, frequency=12,start=c(startyear,startmonth))

nparmatindex0 = length(colnames(dataindex0))

cat("\n Finish reading INDICES")


###            ###################################################################################
### Start here ###################################################################################
###            ###################################################################################


##datat file name
# first file is the primary file for every calculation
#datafile = c("ee.csv","ne.csv","nn.csv","ss.csv")
#datafile = c("met-ee.csv","met-ss.csv","met-cc.csv","met-ne.csv","met-nn.csv")
datafile = c("met-ee.csv")
#datafile = c("met-cc.csv","met-ne.csv","met-nn.csv")
#datafile = c("met-ee.csv","met-cc.csv")


#########Loop for reading of data set
#+/+######
###
#for(loop in 2:length(datafile)){
for(loop in 1:length(datafile)){
data <- read.table(datafile[loop], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

cat("\n Finish reading data file")


################ Start INPUT PROCESSING #########################

############# spatial info of observation #########################
start0=1950  #year start in table file
startyear=1971 #year to analysis begins
startmonth=1
startmonth0=1
endyear=2007
endmonth=0
unusedcol = 5

ifinal=min(which(data$year==(endyear+1)))-1 + endmonth
datafinal = length(data$year)
#******************** remove unwanted column here ##################################
subty = (startyear-start0)*12
#P0
data0=data[-c(0:subty,(ifinal+1):datafinal),-c(1:unusedcol)]

datall =  ts(data, frequency=12,start=c(start0,startmonth0))
datats = ts(data0, frequency=12,start=c(startyear,startmonth))

#- for (a)
newmat = data0
#- for (b)
#newmat = cbind(datats,dataindexts)

## (a) --- use this or -(b)- combine file ********** for multi-file
tscolname = paste(datafile[1],colnames(data0))
for(i in loop:loop){
	data <- read.table(datafile[i], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
	#**** remove unwanted column here ##################################
	subty = (startyear-start0)*12
	#P0
	data2=data[-c(0:subty,(ifinal+1):datafinal),-c(1:unusedcol)]
#	data2 =  ts(data2, frequency=12,start=c(start0,startmonth0))
	newmat = cbind(newmat,data2)
	tscolname = c(tscolname,paste(datafile[i],colnames(data2)))
}
newmat = cbind(newmat,dataindex0)
newmat = ts(newmat, frequency=12,start=c(start0,startmonth0))

#- define column name for multi-file
tscolname = c(tscolname, colnames(dataindex0))
colnames(newmat) = tscolname

## end of (a) ----------------------------------------- 



#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

cat("\n Finish input processing")

################ END OF INPUT PROCESSING 



#********************** Check available data *********************************
#** define uncomplete data ***
rmvnan =  integer(0)
mintocut = 12

for(i in 1:length(newmat[1,])){
					if(length(which(is.na(newmat[,i])==FALSE)) < mintocut){rmvnan = c(rmvnan,i)}
					}

## (b) use this or -(a)- ********** for single-file
#- define column name for one file
#colnames(newmat) = c(paste(datafile[loop],colnames(ts0)),colnames(dataindexts))


#- remove unwanted column
if(length(rmvnan) >= 1){newmat = newmat[,-c(rmvnan)]}




#- count available data
yearav = array(0,length(newmat[1,]))
for(i in 1:length(newmat[1,])){yearav[i]= length(which(is.na(newmat[,i])==FALSE))}

#- define new data form
nparmatall = length(colnames(newmat))

submt=substituteNA(newmat, type = "mean")
submtindex=substituteNA(dataindex0, type = "mean")
#submt=na.omit(newmat, method = "iz")
submt = ts(submt, frequency=12,start=c(startyear,startmonth))

#par(mfrow=c(2,5))
#plot(tseries,xlim=c(startyear,endyear),type="l",main=paste("Station:",colnames(ts0)[i]),
#ylab = expression(paste("Temp-Anomaly [ ", degree, "C]")))
#lines(as.vector(t),fit,type="l",col="blue")
#legend(1969,29, c(paste("", c("Station", "Cycle"))), lty = 1, col =c("black","blue"))

#******************** test ********************
#submt = ts(submt, frequency=12,start=c(startyear,startmonth))
#har=harmonic(submt[,1],1)
#plot(arima(submt[,1]))
#model=lm(newmat~har)
#summary(model)
#fit=fitted(model)
#pdf(paste("mutlti-corl",".pdf"))
#par(mfrow=c(2,1))
#for(i in 1:30){
#plot(submt[,i],type="p")
#lines(ts0[,i],col="blue")
#}
#dev.off()
#tdiff=as.vector(tseries)-as.vector(fit)
#tdiff=ts(tdiff,start=c(startyear,startmonth),frequency=12)


#####################
## Auto-Correlation
#####################
#####################
## Print correlation time-series to text
#####################

#************************************ Auto-Correlation Analysis *********************************


toggle.thissub = FALSE
#+-################# SUB C for printing out .pdf file ####
if(toggle.thissub){
setwd(dir1)
pdf(paste(datafile[1],datafile[loop],"mutlti-corl.pdf"), width= 10, height = 10)
}

#acf0=acf(submt, drop.lag.0 = FALSE, ylim = c(-1,1))

acf0=acf(submt, drop.lag.0 = FALSE, ylim = c(-1,1),lag.max = 12, plot = toggle.thissub)

cat("\n Finish ACF analysis")

if(toggle.thissub){dev.off()}


#+-################# END SUB C for printing out .pdf file ####

#*********************************** Check Auto-Correlation results *****************************

cat("\n Check correlation analysis : create conclusion table")
nparmat = nparmatall

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))
acfmax= array(0, c(nparmat,nparmat))
lagmax= array(0, c(nparmat,nparmat))

for(i in 1:nparmat){
for(j in 1:nparmat){
maxlag=which(acf0$acf[,i,j]==max(acf0$acf[-c(13:14),i,j]))[1]
minlag=which(acf0$acf[,i,j]==min(acf0$acf[-c(13:14),i,j]))[1]
acfa = acf0$acf[,i,j]
acfl = acf0$lag[,i,j]
if (acfa[maxlag]>(acfa[minlag]*-1)){lagmat[i,j] = acfl[maxlag]; acfmat[i,j] = acfa[maxlag]} else {lagmat[i,j] = acfl[minlag]; acfmat[i,j] = acfa[minlag]};
}
}
#*********************** Find max correlation from +/- ***********************************
cat("\n Check correlation analysis : Finding MAX correlation")
for(i in 1:nparmat){
for(j in 1:nparmat){
if (abs(acfmat[i,j]) > abs(acfmat[j,i])){acfmax[i,j] = acfmat[i,j];lagmax[i,j] = lagmat[i,j]} else {acfmax[i,j] = acfmat[j,i];lagmax[i,j] = lagmat[j,i]};
}
}
rownames(acfmat) = acf0$sname
colnames(acfmat) = acf0$sname
acfmax2=acfmax
lagmax2=lagmax
acfmat2=acfmat
lagmat2=lagmat


nparmat = nparmatall

acfmat= array(0, c(nparmat,nparmat))
lagmat= array(0, c(nparmat,nparmat))
acfmax= array(0, c(nparmat,nparmat))
lagmax= array(0, c(nparmat,nparmat))

for(i in 1:nparmat){
for(j in 1:nparmat){
maxlag=which(acf0$acf[,i,j]==max(acf0$acf[-c(13:14),i,j]))[1]
minlag=which(acf0$acf[,i,j]==min(acf0$acf[-c(13:14),i,j]))[1]
acfa = acf0$acf[,i,j]
acfl = acf0$lag[,i,j]
if (acfa[maxlag]>(acfa[minlag]*-1)){lagmat[i,j] = acfl[maxlag]; acfmat[i,j] = acfa[maxlag]} else {lagmat[i,j] = acfl[minlag]; acfmat[i,j] = acfa[minlag]};
}
}
#*********************** Find max correlation from +/- ***********************************
for(i in 1:nparmat){
for(j in 1:nparmat){
if (abs(acfmat[i,j]) > abs(acfmat[j,i])){acfmax[i,j] = acfmat[i,j];lagmax[i,j] = lagmat[i,j]} else {acfmax[i,j] = acfmat[j,i];lagmax[i,j] = lagmat[j,i]};
}
}
rownames(acfmat) = acf0$sname
colnames(acfmat) = acf0$sname
acfmax2=acfmax
lagmax2=lagmax
acfmat2=acfmat
lagmat2=lagmat






# ****************  Print out result to file  **************************

cat("\n Check correlation analysis : Printing conclude table")

setwd(dir1)
sink(paste(datafile[1],datafile[loop],"-Correlation.csv"))
setwd(dir0)
nparmat=nparmatall
acf = acf0
acfmax=acfmax2
lagmax=lagmax2
acfmat=acfmat2
lagmat=lagmat2

#- Header
cat(datafile[1],datafile[loop],",",nparmat,"\n") 
cat("Station,Index,")
for(k in length(acf$acf[,2,1]):1){cat("Lag",acf$lag[k,2,1],",")}
for(k in 2:length(acf$acf[,1,2])){cat("Lag",acf$lag[k,1,2],",")}
cat("CorrMax,Max at,CorrMin,Min at,OptmCorr, Optm at,dataamount1,dataamount2\n")

for(i in 1:nparmat){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",")
for(k in length(acf$acf[,j,i]):1){cat(acf$acf[k,j,i],",")}
for(k in 2:length(acf$acf[,i,j])){cat(acf$acf[k,i,j],",")}
cat(acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],",",yearav[i],",",yearav[j],"\n")
}
}
sink()


## Show progress
print(paste(datafile[1],datafile[loop],": data count:",length(newmat)))

#dev.off()

# Loop for read file by file
}
#####
#END LOOP of Reading data




#######################################################
######## ENDE #########################################
#######################################################

setwd(dir0)

