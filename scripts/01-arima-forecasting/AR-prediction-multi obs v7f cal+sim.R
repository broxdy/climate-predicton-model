#
#Test program to carry out various time series analyses
#
#load libraries needed
#library(timeSeries)
#library(Kendall)
#library(Rwave)
#library(TSA)
#library(wmtsa)
#library(plotrix)
#library(car)
#library(DAAG)
library(forecast)
#source("clear.R")
#clear()


#--Rayong-- Change station name
cat("\nStart AutoRegressive Model")

p2 = "GCM"
p3 = "Ocean Index"

prd.type = c(rep(c(1,2),4),rep(3,24),rep(4,13)) # fit to ord, to use optimal order which is put in "ord"(Tmax, Tmin , Rain, SST)


########################define file for input / read ################################
## obs
cat("\nReading Obs")
#data <- read.table("Daten_Rayong1971-2100+Index2+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
#data <- read.table("monthly-obs_filled1971-2006(2100)+SST.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data <- read.table("monthly-obs_filled1971-2006(2100).csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("monthly-obs_filled1971-2006(2100)sta48459.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("test-monthly-obs_filled1971-2006(2100)+SST.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")




## ocean index
cat("\nReading ocean index")
oindex <- read.table("ocean index.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

## order ARIMA
cat("\nReading ARIMA order")
order.m <- read.table("ARMA pdq optimal 1971-2000.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


#attach(data)

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)

#station=X48459_MAX # main station to check end of file
#station=ObsMnT
#station_name="ObsRn"
#station_name="X48459_MAX"
#station_name="ObsMnT"

#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="AR-prediction-multi obs v7f cal1971-1999sim2000-2004P36"

############# year of observation #########################
### Calibration
start0.c=1971  #year start in table file
startyear.c=1971 #year to analysis begins
#endyear.c=1985
endyear.c=1999
endmonth.c=0
### Verification
start0.v = 1971  #year start in table file
startyear.v = 2000 #year to analysis begins
#endyear.v = 2000 # Actually limit only 2000 (nprd = 180 =15*12) but 1 year more for lagging data (shifting part)
endyear.v = 2004 # Actually limit only 2000 (nprd = 180 =15*12) but 1 year more for lagging data (shifting part)
#startyear.v = 1986 #year to analysis begins
#endyear.v = 1986 # Actually limit only 2000 (nprd = 180 =15*12) but 1 year more for lagging data (shifting part)
#endyear.v = 1999 # Actually limit only 2000 (nprd = 180 =15*12) but 1 year more for lagging data (shifting part)
endmonth.v = 0


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","arima","dummy"))
dir1=dirname(file.path("D:","Temp","arima",paste(dir_name),"dummy"))

##########################################The stuff above needs to be processed only once.################################



#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#begin (-999) and end (999) of series
#istart=which(station==NA)+1 
#if(length(istart)==0){istart=1}

#ifinal=which(station==999)-1
#nmax=ifinal-istart +1 
#yearstart=year[istart]
#monthstart=which(months==month[istart])
#ts=station[istart:ifinal]
#############################this is a first approach to fix missing data, needs reconsideration
#tsub=substituteNA(ts, type = "mean")
#str(tsub)
#tseries=ts(as.vector(tsub),start=c(yearstart,monthstart),frequency=12)
#t=time(tseries)

############# max t is here #########################
#tfinal = max(t)


# To run scenarios
gcm.n = 1 # GCMs scenarios to run
gcm.title = c("c20m")
#gcm.n = 3 # GCMs scenarios to run
#gcm.title = c("A1B","A2","B1")

ifinal.c=min(which(data$year==(endyear.c+1)))-1 + endmonth.c
datafinal.c = length(data$year)

#ifinal.v = min(which(data$year==(endyear.v+1)))-1 + endmonth.v
ifinal.v = min(which(data$year==(endyear.v+1+1)))-1 + endmonth.v # add 1 more year for lag
datafinal.v = length(data$year)

######################################################################################
########################define file for comparison ###################################
######################################################################################
#Column of parameters specific


######################################################################################
#******************** remove unwanted column here ##################################
######################################################################################
#### column to remove
drmv1 = c(1,2) # Data1 Obs
drmv2 = c(1,2) # Data1 GCM
drmv3 = c(1,2) # Data3 Ocean Index
####

subty.c = (startyear.c-start0.c)*12
#P1 Obs
data01 = data[-c(0:subty.c,(ifinal.c+1):datafinal.c),-drmv1]



######******************** data for validation here ##################################
subty.v = (startyear.v-start0.v)*12
#P1
vdata01=data[-c(0:subty.v,(ifinal.v+1):datafinal.v),-drmv1]


######################################################################################


#####################
## time sieries
#####################

#P1 Obs
data1=  ts(data01, frequency=12,start=c(1971,1))
vdat1=  ts(vdata01, frequency=12,start=c(startyear.v,1))

#nprd = 180 # number of month for forward prediction (nPredictied)
nprd = ifinal.v-ifinal.c-12 # number of month for forward prediction (nPredictied) # remove 1 year for lag

#obsn = 32+13
obsn = ncol(data01)



##############################
###### AR prediction #########
##############################

cat("\n## AR\n")
mth = "ols"
ord = c(12,24,36)
case = length(ord)
count = 0

ar.result = array(NA, c(dim(data1)[1]+nprd,obsn*case)) ## result file
dat.col = data[(subty.c+1):dim(ar.result)[1],drmv1] ## result file
ar.result = cbind(dat.col,ar.result) ## result file

rsar = array(NA, c(obsn,case,44))
#pdar = array(NA, c(obsn,case,44))

#Prediction charts
setwd(dir1)
pdf(paste("ARp-charts.pdf"))
setwd(dir0)


leg.txt <- c("prediction", "observation")
par(mgp=c(2.0,1,0))
par(mai =  c(.8, .8, .8, .8))
#par(ps = 10,mfrow = c(case,1))
par(ps = 16,mfrow = c(2,1))

interval.vrf = 3 # number of month to repeatedly verify

for(i in 1:obsn){
			prd.i = prd.type[i]
			for(j in 1:case){
						nona = which(is.na(data1[,i])== FALSE)
						startdat = nona[1]
						dat = ts(data1[nona,],frequency = 12, start= tsp(data1)[1]+(startdat-1)/12)
						fit = ar(dat[,i],order.max=ord[j],method=mth)
						rsar[i,j,1] = colnames(data1)[i]
						rsar[i,j,2] = mth
						rsar[i,j,3] = ord[j]
						rsar[i,j,4] = fit$order
						rsar[i,j,5] = min(fit$aic[-1])
						plot(fit$aic[-1],type="l",main=paste(rsar[i,j,1]),xlab="order p",ylab="AIC")
						nonars = which(is.na(fit$resid) == FALSE)
						rsar[i,j,6] = mean(fit$resid[nonars])
						rsar[i,j,7] = sqrt(mean((fit$resid[nonars])^2))
						rsar[i,j,8] = 1-sum((fit$resid[nonars])^2)/sum((data1[nona[nonars],i]-mean(data1[nona[nonars],i]))^2)
						rsar[i,j,9] = fit$n.used
						rsar[i,j,10] = length(nonars)
						fitp = forecast(fit,nprd)
						rsar[i,j,11] = nprd
						resid = fitp$mean-vdat1[,i]
						rsar[i,j,12] = mean(resid)
						rsar[i,j,13] = sqrt(mean((resid)^2))
						rsar[i,j,14] = 1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)
						for(k in 1:10)  {
									rsar[i,j,15+(k-1)*3] = mean(resid[1:(k*interval.vrf)])
									rsar[i,j,16+(k-1)*3] = sqrt(mean((resid[1:(k*interval.vrf)])^2))
									rsar[i,j,17+(k-1)*3] = 1-sum((resid[1:(k*interval.vrf)])^2)/sum((vdat1[1:(k*interval.vrf),i]-mean(vdat1[1:(k*interval.vrf),i]))^2)
								    }
						#Plot charts
										#if(prd.i==1) plot(fitp,1,ylim=c(29,37))
										#if(prd.i==2) plot(fitp,1,ylim=c(10,40))
										#if(prd.i==3) plot(fitp,1,ylim=c(0,35))
										#if(prd.i>3) plot(fitp,1)
										plot(fitp,1)
						title(sub = paste(rsar[i,j,1],"/ NS(verification)=",format(as.numeric(rsar[i,j,14]),digits=3,scientific=FALSE)))
						lines(vdat1[,i],col = "Green",lty = "dashed")
						legend(2001,min(fitp$mean[1:24],vdat1[1:24,i])+0.5,legend = leg.txt, col=c("blue","green"), lty=c("solid","dashed"), merge=FALSE, bty = "n")
										count = count+1 # count the loop
										print(paste("AR#",i,j,rsar[i,j,1]," NS=",format(as.numeric(rsar[i,j,14])))) # count the loop print out the running progress

						# record prediction
						pr.ln = length(c(fit$resid+dat[,i],fitp$mean))
						ar.result[(dim(data1)[1]+nprd-pr.ln+1):(dim(data1)[1]+nprd),case*(i-1)+j+2] = c(fit$resid+dat[,i],fitp$mean) ## result file
						colnames(ar.result)[case*(i-1)+j+2] = paste(colnames(data1)[i],fit$order,sep="-") ## result file
				
						}
		 	}

dev.off()


# Write prediction results
setwd(dir1) ## result file
write.csv(ar.result, file = "AR prediction-all.csv", row.names = FALSE) ## result file
setwd(dir0) ## result file


#Calibration
cat("\n- AR calibration")
setwd(dir1)
sink(paste("ARc.csv"))
setwd(dir0)

cat("Obs,method,max_order,order,AIC,meanResid,rmsResid,Nash–Sutcliffe,n,nCalibrated,nPredicted,pMean-all,pRMS-all,pNS-all\n")

for(i in 1:obsn){
		for(j in 1:case){
						for(l in 1:14)cat(paste(rsar[i,j,l],","))
						cat("\n")
				}
		}
sink()

#Prediction
cat("\n- AR prediction")
setwd(dir1)
sink(paste("ARp.csv"))
setwd(dir0)

cat("Obs,order,period,n,AIC,Mean,RMS,NS\n")

for(i in 1:obsn){
		for(j in 1:case){
					cat(rsar[i,j,1],",",rsar[i,j,4],",calibrated,",rsar[i,j,10],",",rsar[i,j,5],",",rsar[i,j,6],",",rsar[i,j,7],",",rsar[i,j,8],"\n")
					cat(rsar[i,j,1],",",rsar[i,j,4],",predicted-all,",rsar[i,j,11],",",rsar[i,j,5],",",rsar[i,j,12],",",rsar[i,j,13],",",rsar[i,j,14],"\n")
					for(k in 1:10) cat(rsar[i,j,1],",",rsar[i,j,4],",predicted,",k*interval.vrf,",",rsar[i,j,5],",",rsar[i,j,15+(k-1)*3],",",rsar[i,j,16+(k-1)*3],",",rsar[i,j,17+(k-1)*3],"\n")
					}
		}
sink()

#dev.off()
