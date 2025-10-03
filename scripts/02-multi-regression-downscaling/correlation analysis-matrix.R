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

#--Rayong-- Change station name



########################define file for input / read #############@###################
data <- read.table("Daily-OBS-CGCM3_A1B-4-1952.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 


#which(obsccf$lag[1,,]==1)

#attach(data)

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)

station=ObsMnT
station_name="ObsMnT"

#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="correlation analysis-matrix"
maindir ="Temp/multi-regression model"


#dir_name=station_name

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name) else dirfile=(dir_name)
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))

####### Check time series #########
rain=data[,-c(1,2)]
#convert to time series
tsrain=ts(rain,start=c(1952,1),frequency=365)

############# Plot time-series
setwd(dir1)
pdf(paste("TS-all.pdf"), width= 12, height = 12)

#ffp = Figure per page
fpp = 10
#l number of series
l=length(colnames(obs))

for(i in 1:((l-l%%10)/10-1)){
par(cex.lab=0.8)
plot(tsrain[,c((((i+1)*fpp)-(fpp-1)):((i+1)*fpp))])
}
par(cex.lab=0.8)
plot(tsrain[,c((((i+1)*fpp)-(fpp-1)):(((i+1)*fpp)-(fpp-1)+l%%fpp))])
dev.off()
setwd(dir0)

######## Correlation coefficient +++ METHOD 1 +++ without n.used

ccoeff=cor(rain,use="pairwise.complete.obs")
#plot ccoeff for each station (each row of correlation matrix)
setwd(dir1)
pdf(paste("Rain-Correlation.pdf"), width= 12, height = 12)
par(cex.lab=1.0)
#ffp = Figure per page
fpp = 12

for(i in 1:((l-l%%fpp)/fpp-1)){
#mar=c(5,4,4,2)+0.1
		par(mar=c(2.8,4,1.2,1))
		par(mgp=c(1.8,0.7,0))
		par(mfrow=c(4,3))
		for (j in ((((i+1)*fpp)-(fpp-1)):((i+1)*fpp))) {
			station=names(rain[j])
			plot(ccoeff[j,],ylab=station)
			}
}

for (j in ((((i+1)*fpp)-(fpp-1)):((i+1)*fpp))) {
			station=names(rain[j])
			plot(ccoeff[j,],ylab=station)
			}
dev.off()

# Print out correlation coefficient matrix
setwd(dir1)
sink("rain_corr_mat.csv")
write.table(formatC(ccoeff, format="f",digits=3, width=7), sep=",",quote=FALSE)
sink()

setwd(dir0)

######## Correlation coefficient +++ METHOD 2 +++ with n.used

#******** check correlation ***************
obs = data[,-c(1:2)]
#obs = data[,c(36,45:47)]
#obs = data[,-c(1:2,45:92)]
#data = data[,-c(51:53)]
nobs = length(colnames(obs))
obsccf = matrix(0,nrow = nobs,ncol = nobs)
nccf = matrix(0,nrow = nobs,ncol = nobs)

for (i in 1:nobs){
			for (j in i:nobs){
						checkccf = ccf(obs[i],obs[j], type = c("correlation"),lag.max = 1,plot = FALSE, na.action = na.exclude)
						obsccf[i,j] = checkccf$acf[2]
						nccf[i,j] =  checkccf$n.used
						}
			print(paste("matrix : ",i,",",j))
			}
### Copy to lower diagonal of matrix ####
for (i in 1:nobs){
			for (j in i:nobs){obsccf[j,i] = obsccf[i,j]
						nccf[j,i] =	nccf[i,j]
						}
			}

#obsccf = acf(obs, lag.max = 1,drop.lag.0 = FALSE, type = c("correlation"),plot = FALSE, na.action = na.exclude)
#dataxccf = acf(obs, lag.max = 1,drop.lag.0 = FALSE, type = c("correlation"),plot = FALSE, na.action = na.exclude)

#cor(data, na.rm = TRUE)
#ccflog0 = obsccf$acf[1,,]

##Write correlation
setwd(dir1)
sink("corr of all.csv")
write.table(obsccf, quote = FALSE, sep = ",")
sink()

##Write n.used
sink("ncorr of all.csv")
write.table(nccf, quote = FALSE, sep = ",")
sink()
setwd(dir0)