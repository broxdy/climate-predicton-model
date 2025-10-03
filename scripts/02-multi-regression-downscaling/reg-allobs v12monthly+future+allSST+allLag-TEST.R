#
#ML-regression anlysis + correlation + line-charts of time series
#
#load libraries needed
library(timeSeries)
#library(Kendall)
#library(Rwave)
library(TSA)
#library(wmtsa)
#library(plotrix)
library(car)
library(MASS)
source("clear.R")
#clear()

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)
#output goes to subdirectory with name of station

#dir_name="reg-allobsV11daily cal1971-1985 vrf1986-2000 sim1971-2000 - rain intercept zero"
dir_name="reg-allobsV12monthly cal1971-1985 vrf1986-2000 sim1971-2000-future-allSST+allLag-TEST"
#dir_name="reg-allobsV12daily cal1971-2000 no-vrf sim1971-2000-future"
maindir ="Temp/multi-regression model"

run.vrf = TRUE

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name)
dirfile=(dir_name)
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))


combine.future = TRUE # if TRUE then run gcm.f.list
gcm.f.list = c("ECHO-G monthly 2000-2100_A1B.csv",
"ECHO-G monthly 2000-2100_A2.csv",
"ECHO-G monthly 2000-2100_B1.csv")
future.date.col = c(1:2)

obs.f = "monthly-obs_filled1971-2006(4)_v2.csv"
gcm.f = "ECHO-G monthly 1971-1999 selection.csv"
sst.f = "ocean index 1971-2009.csv"
day.in.year.f = "day in year.csv"
max.cor.lag = 6



#obs.zero.intercept = c(rep(FALSE,8),rep(TRUE,24)) # if rainfall prediciton then zero intercept
obs.zero.intercept = c(rep(FALSE,8),rep(FALSE,24)) # if rainfall prediciton with intercept
obs.zero.limit = c(rep(FALSE,8),rep(TRUE,24)) # if rainfall prediciton with intercept
zero.limit = 0.5



########################define file for input / read #############@###################
cat("\nReading data set")
### obs
data.obs <- read.table(obs.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
### GCM
data.gcm <- read.table(gcm.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
### SST
data.sst <- read.table(sst.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
### Day in year of OBS
#data.days <- read.table(day.in.year.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

data.sst0 = data.sst[, -c(1:2)]

start.gcm.year0 = 1971
start.obs.year0 = 1971
start.obs.year = data.obs$Year[1]
start.gcm.year = 1971
#start.gcm.year = data.gcm$year[1]
end.obs.year = 1999
#end.obs.year = 2000
end.gcm.year = end.obs.year
end.obs.day=0
end.gcm.day=0

ifinal = which(data.obs$Year==end.gcm.year)[length(which(data.obs$Year==end.gcm.year))] + end.obs.day
datafinal = length(data.obs$Year)+1
subty = (start.obs.year-start.obs.year0)*12
date.obs.sel = data.obs[-c(0:subty,(ifinal+1):datafinal), 1:2]
data.obs.sel = data.obs[-c(0:subty,(ifinal+1):datafinal), -c(1:2)]

ifinal = which(data.gcm$year==end.obs.year)[length(which(data.gcm$year==end.obs.year))] + end.gcm.day
datafinal = length(data.gcm$year)+1
subty = (start.gcm.year-start.gcm.year0)*12
date.gcm.sel = data.gcm[-c(0:subty,(ifinal+1):datafinal), 1:2]
data.gcm.sel = data.gcm[-c(0:subty,(ifinal+1):datafinal), -c(1:2)]

obs.n = ncol(data.obs.sel)
gcm.n1 = ncol(data.gcm.sel)
sst.n = ncol(data.sst0)
gcm.n = gcm.n1 + sst.n*max.cor.lag




n.year.obs = end.obs.year-start.obs.year+1
row.n.obs = which(data.obs$Year == (end.obs.year+1))[1]-1

#obs.freq = 366 # days per year
#obs.freq = data.days[which(data.days[,1] == start.obs.year)[1]:which(data.days[,1] == end.obs.year),2]
obs.freq = rep(12,n.year.obs) # months per year
#gcm.freq = 360 # days per year
gcm.freq = 12 # months per year

calnd.day.list.obs = integer(0)
for(year.i in 1:n.year.obs){calnd.day.list.obs = c(calnd.day.list.obs,1:obs.freq[year.i])}

#### add SST lag
#### creat SST to data.newtset
cat("\nCreat SST to data.newtset")
row.n.obs = dim(data.obs.sel)[1]
row.n.sst = dim(data.sst0)[1]
data.sst.sel = array(NA,c(row.n.obs,sst.n*(max.cor.lag+1)))
data.sst.all = array(NA,c(row.n.sst+max.cor.lag,sst.n*(max.cor.lag+1)))

data.temp = 1:row.n.obs # to converse it to data.frame
data.sst.sel = cbind(data.temp,data.sst.sel)[,-1] # to converse it to data.frame
data.temp = 1:(row.n.sst+max.cor.lag) # to converse it to data.frame
data.sst.all = cbind(data.temp,data.sst.all)[,-1] # to converse it to data.frame

for(lag.i in 0:max.cor.lag){
colnames(data.sst.sel)[((lag.i)*sst.n+1):((lag.i+1)*sst.n)] = paste(colnames(data.sst0),"lag",lag.i)
colnames(data.sst.all)[((lag.i)*sst.n+1):((lag.i+1)*sst.n)] = paste(colnames(data.sst0),"lag",lag.i)

data.add = data.sst0[0:lag.i,]
if(lag.i > 0){data.add[,] = NA}

for(sst.i in 1:sst.n){
				data.sst.sel[,(lag.i*sst.n+sst.i)] = rbind(data.add,data.sst0)[1:row.n.obs,sst.i]
				data.sst.all[1:(row.n.sst+lag.i),(lag.i*sst.n+sst.i)] = rbind(data.add,data.sst0)[,sst.i]
				}
}
#### END creat add SST

cat("\nCombined data.newset")
#data.newset = cbind(data.obs.sel,data.gcm.sel)
# add SST
data.newset = cbind(data.obs.sel,data.gcm.sel,data.sst.sel[1:row.n.obs,])
data.newset = cbind(data.obs$Year[1:row.n.obs],calnd.day.list.obs,data.newset)
colnames(data.newset)[1:2] = c("year","month")


data.newset.f = "TS-obs+gcm_same-timeline.csv"
cat("\nWriting new data set")
setwd(dir1)
write.csv(data.newset, file= data.newset.f, row.names = FALSE)
setwd(dir0)







### SUB to read  data set
runthissub = FALSE # If have already new data set included obs+gcm in the same file already, then run this sub
#+-################# SUB1 for starting at first time ####
if(runthissub){
cat("\nUse : reading data set from file")
########################define file for input / read #############@###################
#data <- read.table("Daily-OBS-CGCM3_A1B-4-1952rv1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data <- read.table("obs+gcm new set.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("Daily-OBS-CGCM3_A1B-4-1952rv1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
}else{
	cat("\nUse : data.newset")
	data = data.newset
	}
#+-################# END OF SUB1 for starting at first time ####





runthissub = TRUE # run at first then can turn it off (because long run waiting)
#+-################# SUB2 for correlation analysis and plot time-series ####
if(runthissub){
#******** check correlation ***************
cat("\nCheck correlation\n")

## **0 for prediction all through
data0=data[,-c(1:2)]
date0=data[,1:2] # first 2 columns are date series
obs0=data[,(2+1):(2+obs.n)] # first 2 columns are date series
station = colnames(obs0)
#PREDICTOR - data3
gcm0=data[,(2+obs.n+1):(2+obs.n+gcm.n)] # first 2 columns are date series


upper.max.nprd = 10
method.n = 4

nobs = length(colnames(obs0))
obsccf = matrix(0,nrow = obs.n,ncol = gcm.n)
nccf = matrix(0,nrow = obs.n,ncol = gcm.n)
calerror = array(NA, c(method.n,8,obs.n))
prderror = array(NA, c(method.n,8,obs.n))
opteq = array(NA, c(upper.max.nprd,obs.n))


prd.nmax.sta = integer(0) # maximum number of predictor each station
prd.nsel.sta = rep(NA,4) # selected number of predictor each station

sel.prd.all = array(NA, c(obs.n,method.n,2+upper.max.nprd))
sel.prd.all[,,1] = colnames(obs0)
#sel.prd.all = array(NA, c(obs.n,2+upper.max.nprd))

coef.all.method = array(NA, c(obs.n,method.n,1+upper.max.nprd))

mthd.bic = array(NA, c(obs.n,method.n))
sel.mthd = rep(NA,obs.n)
best.caleq = rep(NA,obs.n)



for (obs.i in 1:obs.n){
			for (gcm.i in 1:gcm.n){
						checkccf = ccf(obs0[obs.i],gcm0[gcm.i], type = c("correlation"),lag.max = 1,plot = FALSE, na.action = na.exclude)
						obsccf[obs.i,gcm.i] = checkccf$acf[2]
						nccf[obs.i,gcm.i] =  checkccf$n.used
						}
			print(paste("matrix : ",obs.i,",",gcm.i))
			}


### Copy to lower diagonal of matrix ####
#for (i in 1:nobs){
#			for (j in i:nobs){obsccf[j,i] = obsccf[i,j]
#						nccf[j,i] =	nccf[i,j]
#						}
#			}

#obsccf = acf(obs, lag.max = 1,drop.lag.0 = FALSE, type = c("correlation"),plot = FALSE, na.action = na.exclude)
#dataxccf = acf(obs, lag.max = 1,drop.lag.0 = FALSE, type = c("correlation"),plot = FALSE, na.action = na.exclude)

#cor(data, na.rm = TRUE)
#ccflog0 = obsccf$acf[1,,]

colnames(obsccf) = colnames(gcm0)
rownames(obsccf) = colnames(obs0)
colnames(nccf) = colnames(gcm0)
rownames(nccf) = colnames(obs0)

##Write correlation
setwd(dir1)
write.csv(obsccf, file= "Matrix-corr of all.csv", row.names = TRUE)
setwd(dir0)

##Write n.used
setwd(dir1)
write.csv(nccf, file = "Matrix-corr of all_n.csv", row.names = TRUE)
setwd(dir0)

runthissub = FALSE
if(runthissub){
gcmccf = matrix(0,nrow = gcm.n,ncol = gcm.n)
for (gcm.i in 1:gcm.n){
			for (gcm.j in 1:gcm.n){
						gcmccf[gcm.i,gcm.j] = ccf(gcm0[gcm.i],gcm0[gcm.j], type = c("correlation"),lag.max = 1,plot = FALSE, na.action = na.exclude)$acf[2]
						}
			print(paste("GCM matrix : ",gcm.i,",",gcm.j))
			}

colnames(gcmccf) = colnames(gcm0)
rownames(gcmccf) = colnames(gcm0)

##Write correlation
setwd(dir1)
write.csv(gcmccf, file= "corr between GCM predictors.csv", row.names = TRUE)
setwd(dir0)
}


}
#+-################# END OF SUB2 for correlation analysis and plot time-series ####
##########################################The stuff above needs to be processed only once.################################





#########################################################################################
######################################### multiple linear regression ####################
#########################################################################################

runthissub = TRUE
#+-################# SUB3 for Multi--Linear regression ANALYSIS of predictors number / R2 / AIC ####
if(runthissub){

setwd(dir1)
pdf(paste("Charts-ML-RegressionAnalysis.pdf"), width= 6, height = 6)
setwd(dir0)
par(mgp=c(2.5,0.8,0), ps =16, mfrow=c(1,1))
par(mai =  c(.8, .8, .8, .8))

############# year of observation/calibration #########################
start0=1971  #year start in table file
startyear=1971 #year to calibration begins
endyear=1985 #year to calibration END  # cal 1971-1985
endday=0

#ifinal = (min(which(data$year>=(endyear+1))))-1 + endday
ifinal = which(data$year==endyear)[length(which(data$year==endyear))] + endday
datafinal = length(data$year)
subty = (startyear-start0)*365

#******************** remove unwanted column here ##################################
### **.cal for calibration
#Date
date.cal = data[-c(0:subty,(ifinal+1):datafinal), 1:2]
#OBS
obs.cal = data[-c(0:subty,(ifinal+1):datafinal), (2+1):(2+obs.n)] # first 2 columns are date series
#PREDICTOR
gcm.cal =data[-c(0:subty,(ifinal+1):datafinal),(2+obs.n+1):(2+obs.n+gcm.n)] # first 2 columns are date series

### **.vrf for calibration # the rest of file after calibration part will be verificaiton part
#Date
date.vrf = data[-c(0:ifinal), 1:2]
#OBS
obs.vrf = data[-c(0:ifinal), (2+1):(2+obs.n)] # first 2 columns are date series
#PREDICTOR
gcm.vrf =data[-c(0:ifinal),(2+obs.n+1):(2+obs.n+gcm.n)] # first 2 columns are date series

nparmat2 = length(colnames(obs0))
nparmat3 = length(colnames(gcm0))




##### Set parameters for Analysis of Prediction / Predictant / Predictors ###############
########################################################################################

###############################################
## LOOP check different number of predictors ##
###############################################
#Define maximum number of predictor
maxnprd = 12
rs = array(NA, c(maxnprd, 5))


###################################################
################# Main loop to change obs.i
###################################################
for(obs.i in 1:obs.n){

#Define predictant colum (refer from data0)
predictantcol = obs.i
predictant = colnames(data0)[predictantcol]
cat("\n\nChecking :",obs.i,predictant)


################## check Residuals of prediction ############### 
cat("\ncheck Residuals of prediction")
for(nprd.i in 2:maxnprd){
##Define number of predictors
#nprd = i
# Define column for predictor's pool
# without negative coef
#colpool=c(30,18,2,4,16,32,26,15,8,11,12,9,27,14,1,31,3,28,25,29,13,17,19,22,5,10,6,24,21,23)
colpool = order(abs(obsccf[predictantcol,]),decreasing = TRUE)

datapool= gcm0[,colpool[1:nprd.i]]
model = colnames(datapool)


##*??*###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))
#Attach predictant at the end of matrix
pooltemp=cbind(datapool,obs0[,predictantcol])
#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp)
predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

# Check zero intercept option
#cat("\nZero intercept :",obs.zero.intercept[obs.i])
if(obs.zero.intercept[obs.i]){
				fit <- lm(predictor_eq~.-1,data=mydata)
				}else{fit <- lm(predictor_eq~.,data = mydata)}


colnames(rs) = c("Number of predictor","R2","AIC","DF","PredictorCol")
rs[nprd.i,1] = nprd.i
rs[nprd.i,2] = summary(fit)$r.squared
rs[nprd.i,3] = AIC(fit)
rs[nprd.i,4] = fit$df
colt = ""
for(k in 1:nprd.i){colt = paste(colt,colpool[k])}
rs[nprd.i,5] = colt
}
## END of LOOP check different number of predictors ## for(i in 2:maxnprd){

##Write result of different number of predictors ##
setwd(dir1)
#write.csv(rs, file = paste("Residuals of prediction",predictant,".csv"), row.names = FALSE)
setwd(dir0)

cat("\nSelecting predictor n.max all sta")
if(obs.i == 1){
			rs.all = rs[,1:3]
			colnames(rs.all)[2:3] = paste(predictant,c("R2","AIC"),sep="-")
			}else{
				rs.all = cbind(rs.all, rs[,2:3])
				colnames(rs.all)[(obs.i*2):(obs.i*2+1)] = paste(predictant,c("R2","AIC"),sep="-")
				}

low.aic = order(as.real(rs[,3]),decreasing = FALSE)[1:2] # most 2 lowest AIC
high.r2 = order(as.real(rs[low.aic,2]),decreasing = TRUE)
prd.nmax.sta = c(prd.nmax.sta,low.aic[high.r2][1])


mydata = mydata[,1:prd.nmax.sta[obs.i]]


######## Print result of regression/predictors analysis to PDF ####
#######++ title on page ++
par(ps =12,mfrow=c(1,1))
plot.new()
modellabel =""

#Write multi-line text
#Number of text per row
nrow = 3
for(i in 1:length(model)){
				if(i%%nrow==0){modellabel=paste(modellabel,model[i],",","\n")}
				else{modellabel=paste(modellabel,model[i],",")}
				}
text(0,0.5,adj=c(0,0),lab=paste("Multiple-Linear Regression\nAnalysis\n\n# Predictant: ",predictant,"\n# Predictors: ",modellabel),cex=3)

#######++ title on page ++
par(ps =12,mfrow=c(1,1))
plot.new()
text(0,0,adj=c(0,0),lab= paste("\n",obs.i,station[obs.i],"\nPredictor selection"),cex=2)
par(ps =8)



################################################################
################## determine optimal subset ###############
################################################################

# Check zero intercept option
cat("\ndetermine optimal subset - Zero intercept :",obs.zero.intercept[obs.i])
###################
## if Intercept zero
###################
if(obs.zero.intercept[obs.i]){

leaps = regsubsets(predictor_eq~.-1,data=mydata,intercept=FALSE)

#subsets(leaps, statistic="rsq",legend=FALSE,max.size=8,abbrev=6, main="Sets of predictors and the regression")
vorder=array(leaps$vorder,c(length(leaps$vorder),4))
method=array(leaps$method,c(length(leaps$method),4))

########################################## Method of selection - bar charts
par(ps =16,mfrow=c(1,1))

#leaps = regsubsets(mydata[,1]~.,data=mydata,method=c("exhaustive", "backward", "forward", "seqrep"),nbest=1)
leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("exhaustive"),nbest=1,intercept=FALSE)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: exhaustive"),scale="r2") 

leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("backward"),nbest=1,intercept=FALSE)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: backward"),scale="r2") 

leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("forward"),nbest=1,intercept=FALSE)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: forward"),scale="r2")

leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("seqrep"),nbest=1,intercept=FALSE)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: seqrep"),scale="r2") 


########################################## Method of selection - line charts
par(ps =9,mfrow=c(1,1))

leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("exhaustive"),nbest=1,intercept=FALSE)
vorder[,1]=leaps$vorder
method[1]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection method: exhaustive"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6) 
#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 1
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)]  = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor


leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("backward"),nbest=1,intercept=FALSE)
vorder[,2]=leaps$vorder
method[2]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: backward"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)
#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 2
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)]  = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor


leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("forward"),nbest=1,intercept=FALSE)
vorder[,3]=leaps$vorder
method[3]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: forward"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)
#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 3
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)]  = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor


leaps = regsubsets(predictor_eq~.-1,data=mydata,method=c("seqrep"),nbest=1,intercept=FALSE)
vorder[,4]=leaps$vorder
method[4]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: seqrep"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)
#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 4
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)]  = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor


#cat("\nMethod",mth,"Model Coef :",coef(leaps,best.opt))


###################
## if NOT Intercept zero
###################
}else{

leaps = regsubsets(predictor_eq~.,data=mydata)

#subsets(leaps, statistic="rsq",legend=FALSE,max.size=8,abbrev=6, main="Sets of predictors and the regression")
vorder=array(leaps$vorder,c(length(leaps$vorder),4))
method=array(leaps$method,c(length(leaps$method),4))

########################################## Method of selection - bar charts
par(ps =16,mfrow=c(1,1))

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

#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 1
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)]  = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor




leaps = regsubsets(predictor_eq~.,data=mydata,method=c("backward"),nbest=1)
vorder[,2]=leaps$vorder
method[2]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: backward"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)

#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 2
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)]  = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor



leaps = regsubsets(predictor_eq~.,data=mydata,method=c("forward"),nbest=1)
vorder[,3]=leaps$vorder
method[3]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: forward"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)

#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 3
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)]  = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor



leaps = regsubsets(predictor_eq~.,data=mydata,method=c("seqrep"),nbest=1)
vorder[,4]=leaps$vorder
method[4]=leaps$method
subsets(leaps, main=paste("Sets of predictors and the regression for predicting of ",predictant,"\nSelection Method: seqrep"), statistic="rsq",legend=FALSE,max.size=10,abbrev=6)

#### Define number of predictor
mtext(summary(leaps)$which)
plot(summary(leaps)$bic, xlab="predictor case", ylab="BIC")
plot(summary(leaps)$adjr2, xlab="predictor case", ylab="adjust R square")
a = summary(leaps)[1]
best.opt = which.min(summary(leaps)$bic)
best.prd = which(a$which[best.opt,][-1] == TRUE)
best.prd.n = length(best.prd) # optimal number of predictor each station
mth = 4
prd.nsel.sta[mth] = best.prd.n # combine optimal number of predictor each station
sel.prd.all[obs.i,mth,2] = prd.nsel.sta[mth] # record optimal number of predictors
sel.prd.all[obs.i,mth,3:(2+best.prd.n)] = best.prd # record optimal number of predictors
mthd.bic[obs.i,mth] = summary(leaps)$bic[best.opt]
#### END Define number of predictor


}
## END else
################################################################
################## END determine optimal subset ###############
################################################################



########
### Deterimine number of  predictors
########


# record number of optimal predictor each method
# Print results
for(mth.i in 1:method.n){
prd.n = as.real(sel.prd.all[obs.i,mth.i,2])
prd.id0 = as.real(sel.prd.all[obs.i,mth.i,3:(2+prd.n)])
prd.id1 = order(abs(obsccf[obs.i,]),decreasing = TRUE)[as.real(prd.id0[which(!is.na(prd.id0))])]
cat("\nMethod",mth.i," n=",prd.nsel.sta[mth.i],":",colnames(gcm0)[prd.id1])
}
########
### END Deterimine number of  predictors
########

#**



runthissub = TRUE
#+-################# SUB4 for calibration and validation of model ########################################################################################################################

if(runthissub){

######################################################################################
######******************** compare model and validation ##############################
######################################################################################
cat("\nStart model calibration and validation")
######******************** data for validation here ##################################
ifinal = which(data$year==endyear)[length(which(data$year==endyear))] + endday
datafinal = length(data$year)

subty = (startyear-start0)*365
#Obs
#vdata.obs=obs0[-c(0:subty,(ifinal+1):datafinal),]
#vdata2=gcm0[-c(0:subty,(ifinal+1):datafinal),]
######################################################################################

####################### ++ title on page ++
par(ps =12,mfrow=c(1,1))
plot.new()
text(0,0,adj=c(0,0),lab= "Comaparing the most fit models from 4 methods",cex=2)


for (i in 1:method.n){

	# Define set of predictor each method/obs
	prd.id0 = sel.prd.all[obs.i,i,3:(2+upper.max.nprd)]
	prd.n = as.real(sel.prd.all[obs.i,i,2])
	vorder = order(abs(obsccf[obs.i,]),decreasing = TRUE)[as.real(prd.id0[which(!is.na(prd.id0))])]

	#########################
	###### model calibration ###################
	cat("\nMethod:",i)
	#vdata = mydata[,vorder[1:prd.n,i]]

	#Define predictant and predictors
	vdata = gcm.cal[,vorder]
	prd_eq = obs.cal[,obs.i]

	#cat("\nZero intercept :",obs.zero.intercept[obs.i])
	if(prd.n > 1){
					if(obs.zero.intercept[obs.i]){
								fit <- lm(prd_eq~.-1,data=vdata)
									ln.coef = length(coef(fit))
									coef.all.method[obs.i,i,1:ln.coef+1] = coef(fit)

								}else{fit <- lm(prd_eq~.,data = vdata)
									ln.coef = length(coef(fit))
									coef.all.method[obs.i,i,1:ln.coef] = coef(fit)
									}
								#fit <- lm(prd_eq~.,data=vdata)
					}else{
						if(obs.zero.intercept[obs.i]){
								fit <- lm(prd_eq~vdata-1)
									ln.coef = length(coef(fit))
									coef.all.method[obs.i,i,1:ln.coef+1] = coef(fit)

								}else{fit <- lm(prd_eq~vdata)
									ln.coef = length(coef(fit))
									coef.all.method[obs.i,i,1:ln.coef] = coef(fit)
									}
								#fit <- lm(prd_eq~.,data=vdata)
						}

	calf = predict(fit,gcm.cal)
	## Screen negativ values at obs.zero.limit to be 0
	if(obs.zero.limit[obs.i]){calf[which(calf < zero.limit)] = 0}

	lmx = max(calf,max(obs.cal[,obs.i]),na.rm = TRUE)
	lmn = min(calf,min(obs.cal[,obs.i]),na.rm = TRUE)

	cat("- matplot cal")
	par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
	par(mai =  c(.8, .8, .8, .8))
	matplot(obs.cal[,obs.i],calf,pch="X",xlab="Observation",ylab="Prediction",xlim=c(lmn,lmx),ylim=c(lmn,lmx),type="p",col = "red", main=paste("Model calibration",station[obs.i],"method:",method[i]))
	matlines (c(-1000,1000), c(-1000,1000), type = "l", lty = "dashed", lwd = 1, pch = NULL, col = 1:2)


	#***********calibration summary result
	cal = cbind(obs.cal[,which(predictant==colnames(obs0))],calf)

	calerror[i,1,obs.i] = predictant
	calerror[i,2,obs.i] = prd.n
	calerror[i,3,obs.i] = mean(cal[which(is.na(cal[,1])==FALSE),2]-cal[which(is.na(cal[,1])==FALSE),1],na.rm = TRUE)
	calerror[i,4,obs.i] = mean(abs(cal[which(is.na(cal[,1])==FALSE),1]-cal[which(is.na(cal[,1])==FALSE),2]),na.rm = TRUE)
	calerror[i,5,obs.i] = sqrt(mean((cal[which(is.na(cal[,1])==FALSE),1]-cal[which(is.na(cal[,1])==FALSE),2])^2,na.rm = TRUE))
	calerror[i,6,obs.i] = 1-sum((cal[which(is.na(cal[,1])==FALSE),2]-cal[which(is.na(cal[,1])==FALSE),1])^2,na.rm = TRUE)/sum((cal[which(is.na(cal[,1])==FALSE),1]-mean(cal[which(is.na(cal[,1])==FALSE),1]))^2,na.rm = TRUE)
	calerror[i,7,obs.i] = AIC(fit)
	calerror[i,8,obs.i] = dim(na.omit(cal))[1]




	#########################
	###### model verification ###################
	vrf=gcm.vrf
	if(prd.n > 1){
			prd=predict(fit,vrf)
			}else{
				new = c(gcm.vrf[,vorder],rep(0,1000))
				new = cbind(new,new)
				colnames(new)[1] = "vdata"
				prd=predict(fit,as.data.frame(new))[1:dim(gcm.vrf)[1]]
				}

	## Screen negativ values at obs.zero.limit to be 0
	if(obs.zero.limit[obs.i]){prd[which(prd < zero.limit)] = 0}

	lmx = max(prd,max(obs.vrf[,obs.i]),na.rm = TRUE)
	lmn = min(prd,min(obs.vrf[,obs.i]),na.rm = TRUE)

	cat("- matplot vrf")
	par(mgp=c(2.5,1,0), ps =16, mfrow=c(1,1))
	par(mai =  c(.8, .8, .8, .8))
	matplot(obs.vrf[,obs.i],prd,pch="X",xlab="Observation",ylab="Prediction",xlim=c(lmn,lmx),ylim=c(lmn,lmx),type="p",col = "red", main=paste("Model verification",station[obs.i],"method:",method[i]))
	matlines (c(-1000,1000), c(-1000,1000), type = "l", lty = "dashed", lwd = 1, pch = NULL, col = 1:2)
	#***********validation summary result
	prd =cbind(obs.vrf[,which(colnames(obs0)==predictant)],prd)
	
	prderror[i,1,obs.i] = predictant
	prderror[i,2,obs.i] = prd.n
	prderror[i,3,obs.i] = mean(prd[,2]-prd[,1])
	prderror[i,4,obs.i] = mean(abs(prd[,1]-prd[,2]))
	prderror[i,5,obs.i] = sqrt(mean((prd[,1]-prd[,2])^2))
	prderror[i,6,obs.i] = 1-sum((prd[,2]-prd[,1])^2)/sum((prd[,1]-mean(prd[,1]))^2)
	prderror[i,7,obs.i] = AIC(fit)
	prderror[i,8,obs.i] = dim(na.omit(prd))[1]


	#cat("\n",obs.cal[1:10,obs.i])
	#cat("\n",calf[1:10])
	#cat("\n",obs.vrf[1:10,obs.i])
	#cat("\n",prd[1:10,2])


}# END for (i in 1:method.n)


	####################### ++ conclusion on page ++
	par(ps =9,mfrow=c(2,1))
	# print result to text

	plot.new()
	title("Conclusion for comparing the models",cex.main=2) 
	
	# Define set of predictor each method/obs
	#prd.id0 = sel.prd.all[obs.i,i,3:(2+upper.max.nprd)]
	#vorder = order(abs(obsccf[obs.i,]),decreasing = TRUE)[as.real(prd.id[which(!is.na(prd.id))])]

	#cat("\nMethod:",i)
	vdata = gcm.cal[,vorder]










#define optimal equation by calibration
best.caleq[obs.i] = min(which(calerror[,5,obs.i]==min(calerror[,5,obs.i])))
#opteq[1:prd.n,obs.i] = vorder[1:prd.n,min(which(calerror[,5,obs.i]==min(calerror[,5,obs.i])))]




setwd(dir0)

#######
# Select method to run by choosing minimum ERROR
#######
cat("\nSelect method to run by choosing minimum ERROR then AIC")
#sel.mthd[obs.i] = which.min(mthd.bic[obs.i,])
sel.1 = which(as.real(prderror[,6,obs.i])== max(as.real(prderror[,6,obs.i])))
if(length(sel.1)>1){
			sel.2 = which(as.real(calerror[sel.1,6,obs.i])== max(as.real(calerror[sel.1,6,obs.i])))
			if(length(sel.2)>1){
						#sel.3 = which.min(mthd.bic[obs.i,sel.1[sel.2]]== min(mthd.bic[obs.i,sel.1[sel.2]]))
						sel.3 = which.min(calerror[sel.1[sel.2],7,obs.i]== min(calerror[sel.1[sel.2],7,obs.i]))
						sel.mthd[obs.i] = sel.1[sel.2[sel.3]]
						cat("\nSelcted method :",sel.mthd[obs.i],"by vrf./cal./AIC performance")
						}else{
							sel.mthd[obs.i] = sel.1[sel.2]
							cat("\nSelcted method :",sel.mthd[obs.i],"by vrf. and cal. performance")
							}
			}else{
				sel.mthd[obs.i] = sel.1
				cat("\nSelcted method :",sel.mthd[obs.i],"by vrf.performance")
				}


# create array of selected predictors
cat("\nCreate final array of selected predictors : sel.prd.all.final")
if(obs.i == 1){
sel.prd.all.final = cbind(station,sel.prd.all[,sel.mthd[obs.i],])
}else{sel.prd.all.final[obs.i,2:(2+best.prd.n)] = sel.prd.all[obs.i,sel.mthd[obs.i],2:(2+best.prd.n)]}

colnames(sel.prd.all.final)[2:(2+upper.max.nprd)] = c("optimal n of predictor",paste("pr",1:upper.max.nprd,sep="-"))

sel.prd.all.final[obs.i,2] = sel.prd.all[obs.i,sel.mthd[obs.i],2]



}
## END Loop for(obs.i in 1:obs.n){
#+-################# END of SUB3 for Multi--Linear regression analysis of predictors number / R2 / AIC ####
}
## END if(runthissub){
dev.off()




############################################ END big loop of multi regression $$$$$$$$$$$$$$

######################################################################################
######******************** print multi-regression models and validations #############
######################################################################################

AIC.list = integer(0)
for(i in 1:obs.n){
AIC.list = c(AIC.list,calerror[,7,i])
}




#***********print validation summary result
cat("\nPrint calibration")
setwd(dir1)
sink("RES-rgrssn-cal.csv")
setwd(dir0)
cat("Predictant,GCM,Mothed,MERROR,ABSERROR,RMSE,Nash–Sutcliffe,AIC,Cal-N\n") 
for(i in 1:obs.n){
	for(mthd in 1:method.n){
		cat(calerror[mthd,1,i],",",calerror[mthd,2,i],",",method[mthd],",",calerror[mthd,3,i],",",calerror[mthd,4,i],",",calerror[mthd,5,i],",",calerror[mthd,6,i],",",calerror[mthd,7,i],",",calerror[mthd,8,i],"\n")
			 }
		 }
sink()


#***********print calibration summary result
if(run.vrf){
cat("\nPrint validation") 
setwd(dir1)
sink("RES-rgrssn-vrf.csv")
setwd(dir0)
cat("Predictant,GCM,Mothed,MERROR,ABSERROR,RMSE,Nash–Sutcliffe,AIC,Vrf-N\n")
for(i in 1:obs.n){
	for(mthd in 1:method.n){
		cat(prderror[mthd,1,i],",",prderror[mthd,2,i],",",method[mthd],",",prderror[mthd,3,i],",",prderror[mthd,4,i],",",prderror[mthd,5,i],",",prderror[mthd,6,i],",",prderror[mthd,7,i],",",prderror[mthd,8,i],"\n")
			 }
		 }
sink()
}#END if(run.vrf)








#####################################
########## print time-series results / cal at cal period
cat("\nCalculate time-series - cal at cal period") 
resultc = array(NA, c(dim(gcm0)[1],obs.n))
resultc = cbind(date0,resultc)
for(i in 1:obs.n){
		prd.n = prd.nsel.sta[sel.mthd[i]]
		cat("\nObs :",colnames(obs0)[i])

		# Define predictor
		prd.n = as.real(sel.prd.all[i,sel.mthd[i],2])
		prd.id0 = as.real(sel.prd.all[i,sel.mthd[i],3:(2+prd.n)])

		vorder = order(abs(obsccf[i,]),decreasing = TRUE)[prd.id0]
		mydata= gcm.cal[,vorder]

		# Calibration
		predictor_eq = obs.cal[,i]
		#vdata = gcm.cal[,]
		vdata = mydata
		cat(" Prd.n by BIC=",sel.mthd[i]," - by Calibration=",best.caleq[i])

		# Check zero intercept option
		cat(" - Zero limit :",obs.zero.limit[i])


		if(obs.zero.intercept[i]){
						fit <- lm(predictor_eq~.-1,data=vdata)
						}else{fit <- lm(predictor_eq~.,data = vdata)}



		# Prediction
		resultc[,i+2] =  predict(fit,gcm0)
		colnames(resultc)[i+2] = colnames(data0)[i]
		}

setwd(dir1)
write.csv(resultc, file = "TS-SimResult all-with negative rain cal_at_calperiod_GCM0.csv", row.names = FALSE)
setwd(dir0)

## Screen negativ values at obs.zero.limit to be 0
scrn.col = which(obs.zero.limit)+2
for(i in scrn.col){
change.val = which(resultc[,i] < zero.limit)
resultc[change.val,i]  = 0
}

setwd(dir1)
write.csv(resultc, file = "TS-SimResult all-without negative rain cal_at_calperiod_GCM0.csv", row.names = FALSE)
setwd(dir0)


#####################################
########## print time-series results / cal at whole period
cat("\nCalculate time-series - cal at whole period") 
resultc = array(NA, c(dim(gcm0)[1],obs.n))
resultc = cbind(date0,resultc)
for(i in 1:obs.n){
		prd.n = prd.nsel.sta[sel.mthd[i]]
		cat("\nObs :",colnames(obs0)[i])

		# Define predictor
		prd.n = as.real(sel.prd.all[i,sel.mthd[i],2])
		prd.id0 = as.real(sel.prd.all[i,sel.mthd[i],3:(2+prd.n)])

		vorder = order(abs(obsccf[i,]),decreasing = TRUE)[prd.id0]
		mydata= gcm0[,vorder]

		# Calibration
		#predictor_eq = obs.cal[,i]
		predictor_eq = obs0[,i]
		#vdata = gcm.cal[,]
		vdata = mydata
		cat(" Prd.n by BIC=",sel.mthd[i]," - by Calibration=",best.caleq[i])

		# Check zero intercept option
		cat(" - Zero intercept :",obs.zero.limit[i])
		if(obs.zero.intercept[i]){
						fit <- lm(predictor_eq~.-1,data=vdata)
						}else{fit <- lm(predictor_eq~.,data = vdata)}



		# Prediction
		resultc[,i+2] =  predict(fit,gcm0)
		colnames(resultc)[i+2] = colnames(data0)[i]
		}

setwd(dir1)
write.csv(resultc, file = "TS-SimResult all-with negative rain cal_at_wholeperiod_GCM0.csv", row.names = FALSE)
setwd(dir0)

## Screen negativ values at obs.zero.limit to be 0
scrn.col = which(obs.zero.limit)+2
for(i in scrn.col){
change.val = which(resultc[,i] < zero.limit)
resultc[change.val,i]  = 0
}

setwd(dir1)
write.csv(resultc, file = "TS-SimResult all-without negative rain cal_at_wholeperiod_GCM0.csv", row.names = FALSE)
setwd(dir0)



















}
#+-################# END OF SUB4 for calibration and validation of model ####

setwd(dir1)
write.csv(rs.all, file = paste("RES-Residuals of prediction vs number of predictor.csv"), row.names = FALSE)
setwd(dir0)

#setwd(dir1)
#write.csv(cbind(prd.nmax.sta,prd.nsel.sta), file = paste("List-n predictor-all.csv"), row.names = FALSE)
#setwd(dir0)




## 1a.number Conclusion of method and model selection
cat("\n1a. Conclusion of method and model selection")
print.table.prd = integer(0)
for(i in 1:obs.n){
print.table.prd = rbind(print.table.prd,cbind(paste("method",1:method.n),sel.prd.all[i,,],mthd.bic[i,],calerror[,3:6,i],prderror[,3:6,i]))
}
print.table.prd = cbind(print.table.prd[,1],print.table.prd[,-1])
colnames(print.table.prd) = c("method","station","number of prd",paste("pr",1:upper.max.nprd,sep="-"),"bic","cal-ME","cal-ABSE","cal-RMSE","cal-NS","vrf-ME","vrf-ABSE","vrf-RMSE","vrf-NS")
print.table.prd = cbind(print.table.prd,AIC.list) # add AIC at last column
setwd(dir1)
write.csv(print.table.prd, file = paste("1a-prd(numb)-all_selected predictor each method of",method.n," -prd- all.csv"), row.names = FALSE)
setwd(dir0)


## 1a.name convert predictor n to predictor name
cat("\n1a. Conclusion of method and model selection-convert predictor n to predictor name")
print.table.prd.name = print.table.prd
for(j in 1:dim(print.table.prd)[1]){
	obs.i = (j-1-((j-1)%%method.n) )/method.n+1
	#cat("\n",obs.i)
	for(k in 1:upper.max.nprd){
		obs.id = order(abs(obsccf[obs.i,]),decreasing = TRUE)[as.real(print.table.prd[j,k+3])]
		print.table.prd.name[j,k+3] = colnames(gcm0)[obs.id]
	}
}
setwd(dir1)
write.csv(print.table.prd.name, file = paste("1a-prd(name)-all_selected predictor each method of",method.n,".csv"), row.names = FALSE)
setwd(dir0)






## 1b. Conclusion of method and model coef.
cat("\n1b. Conclusion of method and model coef.")
print.table.coef = integer(0)
for(i in 1:obs.n){
print.table.coef = rbind(print.table.coef,cbind(paste("method",1:method.n),coef.all.method[i,,],mthd.bic[i,]))
}
print.table.coef = cbind(print.table.coef[,1],print.table.coef[,-1])
colnames(print.table.coef) = c("method","Intercept",paste("pr",1:upper.max.nprd,sep="-"),"bic")
setwd(dir1)
write.csv(print.table.coef, file = paste("1b-coef-all_predictor coef of each method of",method.n,".csv"), row.names = FALSE)
setwd(dir0)



## 2. Selection of the method (model)
setwd(dir1)
write.csv(cbind(station,sel.mthd), file = paste("2-mthd-sel_selected method.csv"), row.names = FALSE)
setwd(dir0)



setwd(dir1)
write.csv(sel.prd.all.final, file = paste("3a-prd-sel_optimal equation of predictor -prd- sel method.csv"), row.names = FALSE)
setwd(dir0)


row.sel = integer(0)
for(i in 1:obs.n){row.sel = c(row.sel,(i-1)*method.n+sel.mthd[i])}
setwd(dir1)
write.csv(cbind(station,print.table.prd[row.sel,]), file = paste("3b-prd(numb)-sel_optimal equation sel method.csv"), row.names = FALSE)
write.csv(cbind(station,print.table.coef[row.sel,]), file = paste("3c-coef-sel_optimal equation sel method.csv"), row.names = FALSE)
setwd(dir0)

setwd(dir1)
write.csv(print.table.prd.name[row.sel,], file = paste("4a-prd(name)-sel_optimal equation sel method.csv"), row.names = FALSE)
setwd(dir0)


# Combine name and coef.
name.and.coef = rbind(print.table.coef[row.sel,1:(2+upper.max.nprd)],print.table.prd.name[row.sel,1:(3+upper.max.nprd)][,-3])
row.sel.2 = integer(0)
for(i in 1:obs.n){row.sel.2 = c(row.sel.2,i+obs.n,i)}
setwd(dir1)
write.csv(name.and.coef[row.sel.2,], file = paste("4b-name+coef-sel_optimal equation sel method.csv"), row.names = FALSE)
setwd(dir0)



#dev.off()
setwd(dir0)


cat("\nSelect time of SST future")
data.sst.future = data.sst.all[-c(1:dim(data.sst.sel)[1]),]

#########################
# Loop for future.i
#########################
if(combine.future){
for(future.i in 1:length(gcm.f.list)){
gcm.future = read.table(gcm.f.list[future.i], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
gcm.future.date2 = gcm.future[,future.date.col]
gcm.future0 =gcm.future[,-future.date.col]
### add SST ###########
gcm.future0 = cbind(gcm.future0,  rbind(data.sst.future,array(NA,c(dim(gcm.future0)[1]-dim(data.sst.future)[1],dim(data.sst.future)[2]))) )

########## print time-series results
cat("\n -- Calculate future time-series :",gcm.f.list[future.i]) 
resultc = array(NA, c(dim(gcm.future0)[1],obs.n))
resultc = cbind(gcm.future.date2,resultc)
for(i in 1:obs.n){

		prd.n = prd.nsel.sta[sel.mthd[i]]
		cat("\nObs :",colnames(obs0)[i])

		# Define predictor
		prd.n = as.real(sel.prd.all[i,sel.mthd[i],2])
		prd.id0 = as.real(sel.prd.all[i,sel.mthd[i],3:(2+prd.n)])

		vorder = order(abs(obsccf[i,]),decreasing = TRUE)[prd.id0]
		#mydata= gcm.cal[,vorder]
		mydata= gcm0[,vorder]

		# Calibration
		#predictor_eq = obs.cal[,i]
		#vdata = gcm.cal[,]
		predictor_eq = obs0[,i]
		vdata = mydata
		cat(" Prd.n by Calibration=",best.caleq[i])

		# Check zero intercept option
		cat(" - Zero limit :",obs.zero.limit[i])
		if(obs.zero.intercept[i]){
						fit <- lm(predictor_eq~.-1,data=vdata)
						}else{fit <- lm(predictor_eq~.,data = vdata)}



		# Prediction
		resultc[,i+dim(gcm.future.date2)[2]] =  predict(fit,gcm.future0)
		colnames(resultc)[i+dim(gcm.future.date2)[2]] = colnames(data0)[i]
		}

setwd(dir1)
write.csv(resultc, file = paste("TS-SimResult all-with negative rain",gcm.f.list[future.i]), row.names = FALSE)
setwd(dir0)


## Screen negativ values at obs.zero.limit to be 0
scrn.col = which(obs.zero.limit)+dim(gcm.future.date2)[2]
for(i in scrn.col){
change.val = which(resultc[,i] < zero.limit)
resultc[change.val,i]  = 0
}
setwd(dir1)
write.csv(resultc,  file = paste("TS-SimResult all-without negative rain",gcm.f.list[future.i]), row.names = FALSE)
setwd(dir0)

} # END Loop for future.i
} # END if(combine.future)

