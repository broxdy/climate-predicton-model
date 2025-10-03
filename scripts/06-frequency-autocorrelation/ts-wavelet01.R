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

dir_name="ts-wavelet01"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","frequency_correlation","dummy"))
dir1=dirname(file.path("D:","Temp","frequency_correlation",paste(dir_name),"dummy"))


###            ###################################################################################
### Start here ###################################################################################
###            ###################################################################################
#####################
## DATA for WAVELET
#####################


#datat file name
#datafile = c("ee.csv","ne.csv","nn.csv","ss.csv")
datafile = c("met-ee.csv","met-cc.csv","met-ne.csv","met-nn.csv","met-ss.csv")
#datafile = c("met-ee.csv")


#########Loop for reading of data set
#+/+######
###
for(loop in 1:length(datafile)){
#for(loop in 1:1){
data <- read.table(datafile[loop], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

setwd(dir1)
#pdf(paste(datafile[loop],"wavelet.pdf"),)
pdf(paste(datafile[loop],"wavelet.pdf"), width= 20, height = 10)
par(mfrow=c(1,1))
setwd(dir0)


################ Start INPUT PROCESSING #########################

############# year of observation #########################
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






######### Loop for reading of each column
#+/+######
###
for(i in 1:nparmat0){
#for(i in 1:5){
###





tsread = ts0[,i]
avyear = startyear
station_name = colnames(ts0)[i]

# check last NA in entire (half) year NOT missing data
# ts0 for whole data sheet
# ts1 for selected column only available data time
# tseries for selected column filled data from startyear
# value of max missing (maxmiss) data continously
maxmiss = 6
ts1=tsread
if(length(which(is.na(tsread))==TRUE) == length(tsread) || max(which(is.na(tsread)==TRUE)) == length(tsread)){
	avyear = "none"
	ts1=0
	tsread=0
}

if(length(which(is.na(tsread))==TRUE) > 0){

	numberofna=length(which(is.na(tsread)==TRUE))
	lastofna=max(which(is.na(tsread)==TRUE))
	ok = FALSE
	k = 0

		while(ok == FALSE && maxmiss < numberofna){
			if(which(is.na(tsread)==TRUE)[numberofna-maxmiss-k] == lastofna-maxmiss ){ok = TRUE}
			
			else{
				lastofna=which(is.na(tsread)==TRUE)[numberofna-k-1]
				avyear = time(tsread)[lastofna+1]
				if(k > (numberofna - maxmiss) || which(is.na(tsread)==TRUE)[numberofna-k-maxmiss] == min(which(is.na(tsread)==TRUE))){
							avyear = startyear
							ok = TRUE
				}
				k = k+1
			}

		}
	ts(tsread[(lastofna+1):length(tsread)],frequency=12,start=time(tsread)[lastofna+1])
}



### this is a first approach to fix missing data, needs reconsideration
tsub=substituteNA(tsread, type = "mean")
str(tsub)
tseries=ts(as.vector(tsub),start=c(startyear,startmonth),frequency=12)
t=time(tseries)

#*************************************************************** cycle remove*********************************
# harmonic analysis to remove annual cycle

# switch on harmonic function
runhar = FALSE

if(runhar){
har=harmonic(tseries,1)
model=lm(tseries~har)
#summary(model)
fit=fitted(model)

#plot(tseries,xlim=c(startyear,endyear),type="l",main=paste("Station:",colnames(ts0)[i]),
#ylab = expression(paste("Temp-Anomaly [ ", degree, "C]")))
#lines(as.vector(t),fit,type="l",col="blue")
#legend(1969,29, c(paste("", c("Station", "Cycle"))), lty = 1, col =c("black","blue"))

tdiff=as.vector(tseries)-as.vector(fit)
tdiff=ts(tdiff,start=c(startyear,startmonth),frequency=12)
#plot(tdiff,type="l",xlim=c(startyear,endyear),main="Cycle-removed series")
#par(op)
}

#********************************************************* wavelet analysis *********************************

#wavelet analysis of El Nino and cycle-removed station data 

if(tseries != 0){
#--Rayong-- Change El Nino wavelet title
#n3wsp <- wsp(tsread,s0=2,noctave=3,nvoice=20,nreal=0,plottitle=paste("Origin",station_name))
#n3wsp <- wsp(tdiff,s0=2,noctave=3,nvoice=20,nreal=0,plottitle=paste("Removed cycle",i,":",station_name,"Av:",avyear))
n3wsp <- wsp(tseries,s0=2,noctave=3,nvoice=20,nreal=0,plottitle=paste("Filled blank",i,":",station_name,"Av:",avyear))
#n3wsp <- wsp(ts1,s0=2,noctave=3,nvoice=20,nreal=0,plottitle=paste("Only available",station_name))
}
#text(0,1,adj=c(0,1),lab=paste("Filled blank",station_name,"Av:",avyear),cex=3)

print(paste("number",i,":",station_name))

}
#####
#End Loop for reading of each column

dev.off()

}
#####
#END LOOP of Reading data




#######################################################
######## ENDE #########################################
#######################################################

setwd(dir0)

