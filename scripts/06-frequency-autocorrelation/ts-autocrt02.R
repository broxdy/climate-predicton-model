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

dir_name="ts-autocrt02"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","frequency_correlation","dummy"))
dir1=dirname(file.path("D:","Temp","frequency_correlation",paste(dir_name),"dummy"))

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

dataindex <- read.table("Daten_Rayong1950-2100+Index2+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

ifinal=min(which(dataindex$year==(endyear+1)))-1 + endmonth
datafinal = length(dataindex$year)
subty = (startyear-start0)*12

#******************** remove unwanted column here ##################################
dataindex0=dataindex[-c(0:subty,(ifinal+1):datafinal),-c(1:50)]

dataindex0 =  ts(dataindex0, frequency=12,start=c(startyear,startmonth))

nparmatindex0 = length(colnames(dataindex0))



###            ###################################################################################
### Start here ###################################################################################
###            ###################################################################################


##datat file name
#datafile = c("ee.csv","ne.csv","nn.csv","ss.csv")
datafile = c("met-ee.csv","met-cc.csv","met-ne.csv","met-nn.csv","met-ss.csv")
#datafile = c("met-ee.csv")


#########Loop for reading of data set
#+/+######
###
for(loop in 1:length(datafile)){
#for(loop in 1:1){
data <- read.table(datafile[loop], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")




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

nparmat0 = length(colnames(data0))

datall =  ts(data, frequency=12,start=c(start0,startmonth0))
ts0 = ts(data0, frequency=12,start=c(startyear,startmonth))


#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

################ END OF INPUT PROCESSING 


#********************** Check available data *********************************
#** define uncomplete data ***
rmvnan =  integer(0)
mintocut = 12

for(i in 1:length(data0[1,])){
					if(length(which(is.na(data0[,i])==FALSE)) < mintocut){rmvnan = c(rmvnan,i)}
					}
	
#- remove unwanted column
newmat = cbind(ts0,dataindex0)
if(length(rmvnan) >= 1){newmat = newmat[,-c(rmvnan)]}

#- define column name
if(length(rmvnan) >= 1){colnames(newmat) = c(paste(datafile[loop],colnames(ts0)),colnames(dataindex0))[-c(rmvnan)]
}else{
colnames(newmat) = c(paste(datafile[loop],colnames(ts0)),colnames(dataindex0))
}
#- count available data
yearav = array(0,length(newmat[1,]))
for(i in 1:length(newmat[1,])){yearav[i]= length(which(is.na(newmat[,i])==FALSE))}

#- define new data form
nparmatall = nparmat0+nparmatindex0-length(rmvnan)

submt=substituteNA(newmat, type = "mean")
submtindex=substituteNA(dataindex0, type = "mean")


#####################
## Auto-Correlation
#####################
#####################
## Print correlation time-series to text
#####################

#************************************ Auto-Correlation Analysis *********************************





setwd(dir1)
pdf(paste("mutlti-corl",datafile[loop],".pdf"), width= 10, height = 10)
#acf0=acf(submt, drop.lag.0 = FALSE, ylim = c(-1,1))
acf0=acf(submt, drop.lag.0 = FALSE, ylim = c(-1,1),lag.max = 12)
dev.off()

#*********************************** Check Auto-Correlation results *****************************

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

setwd(dir1)
sink(paste(datafile[loop],"Correlation.csv"))
setwd(dir0)
nparmat=nparmatall
acf = acf0
acfmax=acfmax2
lagmax=lagmax2
acfmat=acfmat2
lagmat=lagmat2


cat(datafile[loop],",",nparmat,"\n") 
cat("Station,Index,")
for(k in length(acf$acf[,2,1]):1){cat("Lag",acf$lag[k,2,1],",")}
for(k in 2:length(acf$acf[,1,2])){cat("Lag",acf$lag[k,1,2],",")}
cat("CorrMax,Max at,CorrMin,Min at,OptmCorr, Optm at,dataamount1,dataamount2\n")
for(i in 1:3){
for(j in 1:nparmat){
cat(rownames(acfmat)[i],",",colnames(acfmat)[j],",")
for(k in length(acf$acf[,j,i]):1){cat(acf$acf[k,j,i],",")}
for(k in 2:length(acf$acf[,i,j])){cat(acf$acf[k,i,j],",")}
cat(acfmat[i,j],",",lagmat[i,j],",",acfmat[j,i],",",lagmat[j,i],",",acfmax[i,j],",",lagmax[i,j],",",yearav[i],",",yearav[j],"\n")
}
}
sink()


## Show progress
print(paste("File:",datafile[loop],": data count:",length(newmat)))

#dev.off()

}
#####
#END LOOP of Reading data




#######################################################
######## ENDE #########################################
#######################################################

setwd(dir0)

