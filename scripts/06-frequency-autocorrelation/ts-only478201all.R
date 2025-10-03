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

dir_name="ts-only478201all"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","frequency_correlation","dummy"))
dir1=dirname(file.path("D:","Temp","frequency_correlation",paste(dir_name),"dummy"))

###########################
## DATA for Regional Index
###########################
############# spatial info of index #########################
start0=1950  #year start in table file
startyear=1970 #year to analysis begins
startmonth=1
startmonth0=1
endyear=2005
endmonth=3

dataindex <- read.table("478201.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

ifinal=min(which(dataindex$year==(endyear+1)))-1 + endmonth
datafinal = length(dataindex$year)
subty = (startyear-start0)*12

#******************** remove unwanted column here ##################################
start.col = 3
end.col = 26
dataindex0 =dataindex[-c(0:subty,(ifinal+1):datafinal),c(4,3,5:end.col)]

dataindexts =  ts(dataindex0, frequency=12,start=c(startyear,startmonth))

# In input file, put obs column firt then index
n.index = 13
n.obs = length(colnames(dataindexts))- n.index



###            ###################################################################################
### Start here ###################################################################################
###            ###################################################################################

setwd(dir1)
sink(paste("Correlation.csv"))
cat("P1,P2,")
for(m in 0:24){cat(-1+m/12,",")}
cat("\n")

pdf(paste("mutlti-corl.pdf"), width= 10, height = 30)
par(mfcol=c(13,3))

for(i in 1:n.obs){
for(k in (n.obs+1):(n.index+n.obs)){
# Put index before obs (nino occur and result the obs climate)
ccf0=ccf(dataindexts[,k],dataindexts[,i], ylim = c(-1,1),lag.max = 12, plot = FALSE, na.action = na.exclude)
plot(ccf0, main = paste(colnames(dataindexts)[k]," - ",colnames(dataindexts)[i]),ylab ="Cross-Correlation",xlab= "Lag (month)",ylim = c(-0.8,0.8))

# write value
cat(colnames(dataindexts)[k],",",colnames(dataindexts)[i])
for(m in 1:25){cat(",",ccf0$acf[m])}
cat("\n")

}
}

dev.off()
sink()
cat("index=",n.index," obs=",n.obs,"\n ==> ",dir1,"\n")

#######################################################
######## ENDE #########################################
#######################################################

setwd(dir0)

