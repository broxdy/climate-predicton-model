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

dir_name="ts-only48459all-test"

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
endyear=2005
endmonth=3

dataindex <- read.table("Daten_Rayong1950-2100+Index2+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

ifinal=min(which(dataindex$year==(endyear+1)))-1 + endmonth
datafinal = length(dataindex$year)
subty = (startyear-start0)*12

#******************** remove unwanted column here ##################################
dataindex0 =dataindex[-c(0:subty,(ifinal+1):datafinal),c(3:4,7:63)]

dataindexts =  ts(dataindex0, frequency=12,start=c(startyear,startmonth))

obs = 3
prd = length(colnames(dataindexts)) - obs


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

for(i in 1:obs){
for(k in (obs+1):(prd+obs)){
ccf0=ccf(dataindexts[,i],dataindexts[,k], ylim = c(-1,1),lag.max = 12, plot = FALSE, na.action = na.exclude)
plot(ccf0, main = paste(colnames(dataindexts)[i]," - ",colnames(dataindexts)[k]),ylab ="Cross-Correlation",xlab= "Lag (year)",ylim = c(-0.8,0.8))
#plot(ccf0, main = paste(colnames(dataindexts)[i]," - ",colnames(dataindexts)[k]),ylab ="",xlab = "",ylim = c(-1,1))

# write value
cat(colnames(dataindexts)[i],",",colnames(dataindexts)[k])
for(m in 1:25){cat(",",ccf0$acf[m])}
cat("\n")

}
}

dev.off()
sink()


#######################################################
######## ENDE #########################################
#######################################################

setwd(dir0)

