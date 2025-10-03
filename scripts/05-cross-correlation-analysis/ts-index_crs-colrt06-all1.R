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


dir_name="ts-autocrt06-all1"

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
datafile = c("met-ee.csv","met-ss.csv","met-cc.csv","met-ne.csv","met-nn.csv")
#datafile = c("met-ee.csv")
#datafile = c("met-cc.csv","met-ne.csv","met-nn.csv")
#datafile = c("met-ee.csv","met-cc.csv")


# LOOP ################################################################
################ Start INPUT PROCESSING (obs) #########################
#######################################################################

#########Loop for reading of data set

for(loop in 1:length(datafile)){
cat("\n## Start loop",loop,"(",length(datafile),") :",datafile[loop])

data <- read.table(datafile[loop], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

cat("\n Finish reading data file")


################ Start INPUT PROCESSING (obs) #########################

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




newmat = cbind(data0,dataindex0)
newmat = ts(newmat, frequency=12,start=c(startyear,startmonth))

#- define column name for multi-file
tscolname = c(paste(substr(datafile[loop],1,nchar(datafile[loop])-4),colnames(data0)))
tscolname = c(tscolname,colnames(dataindexts))
colnames(newmat) = tscolname



## end of (a) ----------------------------------------- 



#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

cat("\n Finish input processing \n")

################ END OF INPUT PROCESSING 



#********************** Check available data *********************************

runthisloop = TRUE
#** Remove uncomplete data (otherwise the error/misleading will occure when obseveration<12 month ***
if(runthisloop){
rmvnan =  integer(0)
mintocut = 12

for(i in 1:length(newmat[1,])){
					if(length(which(is.na(newmat[,i])==FALSE)) < mintocut){rmvnan = c(rmvnan,i)}
					}
#- remove unwanted column
if(length(rmvnan) >= 1){newmat = newmat[,-c(rmvnan)]}
}
#** END define uncomplete data ***



#- count available data
yearav = array(0,length(newmat[1,]))
for(i in 1:length(newmat[1,])){yearav[i]= length(which(is.na(newmat[,i])==FALSE))}

#- define new data form
nparmatall = length(colnames(newmat))




#####################
## Auto-Correlation
#####################
#####################
## Print correlation time-series to text
#####################

#************************************ Auto-Correlation Analysis *********************************

ccf.lag.max = 11

printout.thissub = TRUE
#+-################# SUB C for printing out .pdf file ####

n.obs = length(colnames(newmat)) - nparmatindex0
n.index = nparmatindex0
obsccf = array(0,c(ccf.lag.max*2+1,n.index,n.obs))
nccf = array(0,c(n.index,n.obs))


## Loop for find correlation which each index and station
for (i in 1:n.index){

# Create PDF for difference lag plot
setwd(dir1)
pdf(paste("Lag_Plot-",substr(datafile[loop],1,nchar(datafile[loop])-4),"+",colnames(newmat)[i+n.obs],".pdf"))
par(mfrow=c(3,3))
			for (j in 1:n.obs){
						checkccf = ccf(newmat[,i+n.obs],newmat[,j], type = c("correlation"),lag.max = ccf.lag.max,plot = FALSE, na.action = na.exclude)
						obsccf[,i,j] = checkccf$acf
						nccf[i,j] =  checkccf$n.used

						# Plot PDF for difference lag
						# loop through lags -8 to 9
						for(h in -4:4){
							# Trend line
							eq.trend = cbind(lag(newmat[,i+n.obs],h),newmat[,j])
							b = lsfit(eq.trend[,1],eq.trend[,2])

							# Calcualte R2
							#r21 = 1-sum(a$residuals^2)/sum((na.exclude(newmat[which(!is.na(lag(newmat[,i+n.obs],h))),j]) - mean(eq.trend[which(!is.na(eq.trend[,1])),2],na.rm=TRUE))^2)
							#r22 = 1-sum(na.exclude(b$residuals)^2)/sum((na.exclude(newmat[which(!is.na(lag(newmat[,i+n.obs],h))),j]) - mean(eq.trend[which(!is.na(eq.trend[,1])),2],na.rm=TRUE))^2)
							eq2 = na.exclude(eq.trend)
							r2 = (cor(b$coefficients[1]+b$coefficients[2]*eq2[,1],eq2[,2]))^2

							plot(lag(newmat[,i+n.obs],h),newmat[,j], main=paste(colnames(newmat)[i+n.obs]," (Lag= ",h,")",sep=""), ylab=colnames(newmat)[j],xlab=colnames(newmat)[i+n.obs])
							mtext(expression(r ^2,"             "),cex=0.8)
							mtext(paste("                   =",round(r2,3)),cex=0.7)
							# Plot trend line
							abline(b,col="green")

							}
						# loop through lags 1 to 9
						#for(h in 1:9){plot(lag(newmat[,i+n.obs],h),newmat[,j], main=paste(colnames(newmat)[i+n.obs],"(Lag= ",h,")",sep=""), ylab=colnames(newmat)[j],xlab=colnames(newmat)[i+n.obs])}
						
						}

			# Close PDF Lag plot
			dev.off()
			setwd(dir0)

			# Show text that it's end of this index
			cat(paste("  Index:",colnames(newmat)[i+n.obs]," ..done\n"))
			}


if(printout.thissub){
##Write correlation
cor.at.lag0 = obsccf[ccf.lag.max+1,,]
colnames(cor.at.lag0)= colnames(newmat)[1:n.obs]
rownames(cor.at.lag0)= colnames(dataindexts)

setwd(dir1)
sink(paste("corr at lag 0",datafile[loop],".csv"))
write.table(cor.at.lag0, quote = FALSE, sep = ",")
sink()
setwd(dir0)
}


cat("\n Finish ACF analysis")








#+-################# END SUB C for printing out .pdf file ####

#*********************************** Check Auto-Correlation results *****************************

cat("\n Check correlation analysis : create conclusion table")
nparmat = nparmatall

# create for min correlation
acfmin= array(0, c(n.index,n.obs))
lagmin= array(0, c(n.index,n.obs))

# create for max correlation
acfmax= array(0, c(n.index,n.obs))
lagmax= array(0, c(n.index,n.obs))

# create for opt correlation
acfopt= array(0, c(n.index,n.obs))
lagopt= array(0, c(n.index,n.obs))

# Find max and min correlation
for(i in 1:n.index){
for(j in 1:n.obs){
maxlag=which(obsccf[,i,j]==max(obsccf[,i,j]))[1]
minlag=which(obsccf[,i,j]==min(obsccf[,i,j]))[1]
acfa = obsccf[,i,j]
acfl = checkccf$lag

# Define max/min value
acfmin[i,j] = obsccf[minlag,i,j]
acfmax[i,j] = obsccf[maxlag,i,j]

lagmin[i,j] = minlag
lagmax[i,j] = maxlag


# Find optimal value of lag
if (abs(acfa[maxlag])>abs(acfa[minlag])){lagopt[i,j] = acfl[maxlag]; acfopt[i,j] = acfa[maxlag]
}else {lagopt[i,j] = acfl[minlag]; acfopt[i,j] = acfa[minlag]};

}
}

# Define row/column name
rownames(acfopt) = colnames(newmat)[(n.obs+1):(n.obs+n.index)]
colnames(acfopt) = colnames(newmat)[1:n.obs]






# ****************  Print out result to file  **************************

cat("\n Check correlation analysis : Printing conclude table")

setwd(dir1)
sink(paste(datafile[loop],"-Correlation.csv"))
setwd(dir0)


#- Header
cat(datafile[1],datafile[loop],",",nparmat,"\n") 
cat("Station,Index,")
for(k in 1:(ccf.lag.max*2+1)){cat("Lag",checkccf$lag[k,1,1],",")}
cat("CorrMax,Max at,CorrMin,Min at,OptmCorr, Optm at,dataamount.index,dataamount.obs,dataamount.both\n")

#- Result data
for(i in 1:n.index){
for(j in 1:n.obs){
cat(colnames(newmat)[i+n.obs],",",colnames(newmat)[j],",")
for(k in 1:(ccf.lag.max*2+1)){cat(obsccf[k,i,j],",")}
cat(acfmax[i,j],",",lagmax[i,j],",",acfmin[i,j],",",lagmin[i,j],",",acfopt[i,j],",",lagopt[i,j],",",yearav[i+n.obs],",",yearav[j],",",nccf[i,j],"\n")
}
}
sink()


## Show progress
cat(paste("\n# CrossCorrelation Indices-",datafile[loop],": data count:",length(newmat),"\n"))

#dev.off()

# Loop for read file by file
}
#####
#END LOOP of Reading data




#######################################################
######## ENDE #########################################
#######################################################

setwd(dir0)

