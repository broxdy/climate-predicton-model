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



runthissub = TRUE
#+-################# SUB1 for starting at first time ####
if(runthissub){

########################define file for input / read #############@###################
data <- read.table("Daily-OBS-CGCM3_A1B-4-1952rv1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("Daily-OBS-CGCM3_A1B-4-1952rv1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)
#output goes to subdirectory with name of station

dir_name="daily-reg-allobs5vrf"
maindir ="Temp/multi-regression model"

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name)
dirfile=(dir_name)
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
#tsrain=ts(data,start=c(1952,1),frequency=365)

}
#+-################# END OF SUB1 for starting at first time ####





runthissub =FALSE
#+-################# SUB2 for correlation analysis and plot time-series ####
if(runthissub){
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
setwd(dir0)


##Write n.used
setwd(dir1)
sink("ncorr of all.csv")
write.table(nccf, quote = FALSE, sep = ",")
sink()
setwd(dir0)
}
#+-################# END OF SUB2 for correlation analysis and plot time-series ####
##########################################The stuff above needs to be processed only once.################################






#########################################################################################
######################################### multiple linear regression ####################
#########################################################################################

runthissub = TRUE
#+-################# SUB3 for Multi--Linear regression ANALYSIS of predictors number / R2 / AIC ####
if(runthissub){

######################## define detail for comparison ###################################

############# year of observation #########################


start0=1952  #year start in table file
startyear=1971 #year to analysis begins
endyear=2005
endday=0

ifinal = (min(which(data$year>=(endyear+1))))-1 + endday
datafinal = length(data$year)
subty = (startyear-start0)*365

#******************** remove unwanted column here ##################################

#Index Matrix for all data using in calculation
alldata = data[,-c(1:2)]
#
data0=data[-c(0:subty,(ifinal+1):datafinal),-c(1:2)]
#OBS - data2
data2=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,47:94)]
#PREDICTOR - data3
data3=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,1:46)]

nparmat2 = length(colnames(data2))
nparmat3 = length(colnames(data3))

# Fill NA data
#mtseries2=ts(data2,start=c(startyear,1),frequency=12)
#mtseries3=ts(data3,start=c(startyear,1),frequency=12)
#submt2=substituteNA(mtseries2, type = "mean")
#submt3=substituteNA(mtseries3, type = "mean")


##### Set parameters for Analysis of Prediction / Predictant / Predictors ###############
########################################################################################

###############################################
## LOOP check different number of predictors ##
###############################################
#Define maximum number of predictor
maxnprd = 12
rs = array(NA, c(maxnprd, 5))


for(i in 2:maxnprd){

#Define predictant colum (refer from data0)
predictantcol = 20

##Define number of predictors
nprd = i
# Define column for predictor's pool
#colpool=c(30,18,7,2,4,16,32,26,15,8,11,12,9,27,14,1,31,3,28,25,29,13,17,19,22,5,10,6,24,21,23)

# without negative coef
colpool=c(30,18,2,4,16,32,26,15,8,11,12,9,27,14,1,31,3,28,25,29,13,17,19,22,5,10,6,24,21,23)

datapool=data0[,colpool[1:nprd]]
#datapool=data2[,c(4,16,20)]

predictant = colnames(data0)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 

##*??*###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
pooltemp=cbind(datapool,data0[,predictantcol])

#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp)

predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

fit = lm(predictor_eq~.,data=mydata)
colnames(rs) = c("Number of predictor","R2","AIC","DF","PredictorCol")
rs[i,1] = i
rs[i,2] = summary(fit)$r.squared
rs[i,3] = AIC(fit)
rs[i,4] = fit$df
colt = ""
for(k in 1:nprd){colt = paste(colt,colpool[k])}
rs[i,5] = colt
}
## END of LOOP check different number of predictors ##

##Write result of different number of predictors ##
setwd(dir1)
sink("Residuals of prediction.csv")
write.table(rs, quote = FALSE, sep = ",")
sink()
setwd(dir0)


######## Print result of regression/predictors analysis to PDF ####
####################### ++ title on page ++

setwd(dir1)
pdf(paste("Daily-ML-RegressionAnalysis.pdf"), width= 12, height = 12)

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
par(ps =12,mfrow=c(1,1))

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=9)
plot(leaps, main=paste("Selection of 9 best predictors for",predictant),scale="r2")

dev.off()

}
#+-################# END of SUB3 for Multi--Linear regression analysis of predictors number / R2 / AIC ####







##################################################################################################################################################################################################################################################################

######################################################################################
###### Run regression to fill the missing data NA in predictant ######################
######################################################################################

# **????*** Run extra loop to extend the predictor to EXTPRD when reaching end of standard prediction
runextprd = TRUE
##Define MAXIMUM number of extended predictors
extprd = 11
verifyfactor = 0.5



#Define predictant colum (refer from data0)
predictantcol = 20

##############
#Define column for predictor's pool (ranked by Regression coefficient)
##############
## with negative coef
#colpool=c(30,18,7,2,4,16,32,26,15,8,11,12,9,27,14,1,31,3,28,25,29,13,17,19,22,5,10,6,24,21,23)
## without negative coef
colpool=c(30,18,2,4,16,32,26,15,8,11,12,9,27,14,1,31,3,28,25,29,13,17,19,22,5,10,6,24,21,23)


##Define MAXIMUM number of predictors
nprdmax =5
firstloop = TRUE

###################################################################################################
################# Start loop to change first number of predictor in equation ######################
for(firstprd in 1:nprdmax){

#############################################################################################
################# Start loop to change number of predictor in equation ######################
for(df in nprdmax:firstprd){



##Define number of predictors
nprd =df
datapool=data0[,colpool[firstprd:nprd]]

predictant = colnames(data0)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 

##*??* ###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
pooltemp=cbind(datapool,data0[,predictantcol])

#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp)

predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

###
### Calibration & Verification here below
###

if((nprd-firstprd) == 0){startvrf = as.integer(verifyfactor*length(mydata))
		endvrf = as.integer(length(mydata))
		}else{
		startvrf = as.integer(verifyfactor*length(rownames(mydata)))
		endvrf = as.integer(length(rownames(mydata)))
		}


### Set ML Equation for calibration#
if((nprd-firstprd) == 0){fitc <- lm(predictor_eq[1:startvrf]~mydata[1:startvrf])
		}else{
		fitc <- lm(predictor_eq[1:startvrf]~.,data=mydata[1:startvrf,])
		}
### Verification
### Predict ML Equation for verification#
if((nprd-firstprd) == 0){fitv <- fitc$coefficients[1]+fitc$coefficients[2]*mydata[(startvrf+1):endvrf]
		}else{
		fitv <- predict(fitc,mydata[(startvrf+1):endvrf,])
		}
obsv = predictor_eq[(startvrf+1):endvrf]


### Set ML Equation #
if((nprd-firstprd) == 0){fit <- lm(predictor_eq~mydata)
		}else{
		fit <- lm(predictor_eq~.,data=mydata)
		}


################################
# check to run only first loop #
if(firstloop){

firstloop = FALSE
# Cut only focus NA or missing data
start0=1952  #year start in table file
startyear=1971 #year to analysis begins
endyear=2005
endday=0

ifinal = (min(which(data$year>=(endyear+1))))-1 + endday
datafinal = length(data$year)
subty = (startyear-start0)*365

#******************** remove unwanted column here ##################################
datacut=data[-c(0:subty,(ifinal+1):datafinal),-c(1:2)]


predictantdata = datacut[,predictantcol]
predictantdata = data.frame(predictantdata)

#Define row's name
rownames(predictantdata) = as.numeric(rownames(datacut))

#tscutdata0=ts(cutdata0,start=c(startyear,1),frequency=365)

#Build sheet/data.frame for filled data will be in here / copy form predictant first / Put the matrix of predictor used in prediction
filledpredictant = cbind(predictantdata,matrix(NA, nrow = length(predictantdata), ncol=(extprd+14+13)))
filledpredictant = data.frame(filledpredictant)
filledpredictant[,1] = NA
colnames(filledpredictant)= c("Predictant","Intercept",colnames(data0[,colpool[firstprd:extprd]]),"First Predictor","Last Predictor",
"nPredictor","n-cal","n-used","n-mising","avrg error","RMSQ error","NS coef","cor","R2","AIC","Predictors",

## under here for calibration and verification 12 column - continue from the above line

"Number of Calibration","Number of Verification","C-avrg error","C-RMSQ error","C-NS coef","C-correlation between obs and fitted value","C-R2","C-AIC","V-avrg error","V-RMSQ error","V-NS coef","V-correlation between obs and fitted value","V-R2")

rownames(filledpredictant)= as.numeric(rownames(datacut))

}
#######################################
# END of check to run only first loop #

#Define Predictor data set and row's name
predictorcol = colpool[firstprd:nprd]
predictordata = datacut[,predictorcol]
predictordata = data.frame(predictordata)
rownames(predictordata) = as.numeric(rownames(datacut))


#Define col of predictor which have no NA
#cmp.data.predictor = na.omit(predictortdata)
cmp.data.predictor = na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),])

#cmp.row.predictor = as.numeric(rownames(na.omit(predictortdata)))
#cmp.row.predictor = rownames(na.omit(predictortdata))
cmp.row.predictor = rownames(na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),]))

#Define predicror data frame
frameprd = data.frame(cmp.data.predictor)

#### PREDICTION #####
# Filling data by prediction funtion / check null row
if(is.null(cmp.row.predictor)== FALSE){
		if(length(cmp.row.predictor)>= 1){
# for >1 dimension predictor
		if((nprd-firstprd)!=0){	filledpredictant[cmp.row.predictor,1] = predict(fit,na.omit(frameprd))}
# for 1 dimension predictor
		if((nprd-firstprd)==0){
					t = data.frame(x = na.omit(frameprd)$cmp.data.predictor)
					filledpredictant[cmp.row.predictor,1] = fit$coefficients[2]*t+ fit$coefficients[1]
					}
		#filledpredictant[cmp.row.predictor,1] = predict(fit,cmp.data.predictor)
		# Mark used predictor #
		filledpredictant[cmp.row.predictor,2] = fit$coefficients[1]
		filledpredictant[cmp.row.predictor,(firstprd+2):(nprd+2)] = fit$coefficients[2:(nprd-firstprd+2)]
		filledpredictant[cmp.row.predictor,(extprd+3)] = firstprd
		filledpredictant[cmp.row.predictor,(extprd+4)] = nprd
		filledpredictant[cmp.row.predictor,(extprd+5)] = nprd-firstprd+1
		filledpredictant[cmp.row.predictor,(extprd+6)] = length(fit$residuals)
		filledpredictant[cmp.row.predictor,(extprd+7)] = length(cmp.row.predictor)

		filledpredictant[cmp.row.predictor,(extprd+9)] = mean(fit$residuals)
		filledpredictant[cmp.row.predictor,(extprd+10)] = (mean((fit$residuals)^2))^0.5
		filledpredictant[cmp.row.predictor,(extprd+11)] = 1-sum((fit$residuals)^2)/sum((predictor_eq-mean(predictor_eq))^2)
		filledpredictant[cmp.row.predictor,(extprd+12)] = cor(fit$model[,1],fit$fitted.values)
		filledpredictant[cmp.row.predictor,(extprd+13)] = (cor(fit$model[,1],fit$fitted.values))^2
		filledpredictant[cmp.row.predictor,(extprd+14)] = AIC(fit)
colt = ""
for(np in firstprd:nprd){colt = paste(colt,np)}
		filledpredictant[cmp.row.predictor,(extprd+15)] = colt
filling = length(predictantdata[which(filledpredictant[,extprd+15]==colt),1])-length(removeNA(predictantdata[which(filledpredictant[,extprd+15]==colt),1]))
		filledpredictant[cmp.row.predictor,(extprd+8)] = filling

## under here for calibration and verification 13 column
		filledpredictant[cmp.row.predictor,(extprd+16)] = startvrf
		filledpredictant[cmp.row.predictor,(extprd+17)] = endvrf-startvrf
		filledpredictant[cmp.row.predictor,(extprd+18)] = mean(fitc$residuals)
		filledpredictant[cmp.row.predictor,(extprd+19)] = (mean((fitc$residuals)^2))^0.5
		filledpredictant[cmp.row.predictor,(extprd+20)] = 1-sum((fitc$residuals)^2)/sum((predictor_eq[1:startvrf]-mean(predictor_eq[1:startvrf]))^2)
		filledpredictant[cmp.row.predictor,(extprd+21)] = cor(fitc$model[,1],fitc$fitted.values)
		filledpredictant[cmp.row.predictor,(extprd+22)] = (cor(fitc$model[,1],fitc$fitted.values))^2
		filledpredictant[cmp.row.predictor,(extprd+23)] = AIC(fitc)
		filledpredictant[cmp.row.predictor,(extprd+24)] = mean(fitv-obsv)
		filledpredictant[cmp.row.predictor,(extprd+25)] = (mean((fitv-obsv)^2))^0.5
		filledpredictant[cmp.row.predictor,(extprd+26)] = 1-sum((fitv-obsv)^2)/sum((predictor_eq[startvrf:endvrf]-mean(predictor_eq[startvrf:endvrf]))^2)
		filledpredictant[cmp.row.predictor,(extprd+27)] = cor(fitv,obsv)
		filledpredictant[cmp.row.predictor,(extprd+28)] = (cor(fitv,obsv))^2
								}
						}

cat("\nPredictor column :",colpool[firstprd:df])





}
#############################################################################################
################# END of loop to change number of predictor in equation ######################
}
###################################################################################################
################# END Start loop to change first number of predictor in equation ######################




##Write result of different number of predictors ##
setwd(dir1)
sink(paste("Fiiled-prediction",predictant,".csv"))
write.table(filledpredictant, quote = FALSE, sep = ",")
sink()
setwd(dir0)




###################################################################################################
# **++++*** Run extra loop to extend the predictor to EXTPRD when reaching end of standard prediction
###################################################################################################
#### duplicate with the upper module ##############################################################


if(runextprd){
firstloop = FALSE

###################################################################################################
################# Start loop to change first number of predictor in equation ######################
for(firstprd in nprdmax:extprd){

#############################################################################################
################# Start loop to change number of predictor in equation ######################
for(df in extprd:firstprd){



##Define number of predictors
nprd =df
datapool=data0[,colpool[firstprd:nprd]]

predictant = colnames(data0)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 

##*??* ###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
pooltemp=cbind(datapool,data0[,predictantcol])

#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp)

predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

###
### Calibration & Verification here below
###

if((nprd-firstprd) == 0){startvrf = as.integer(verifyfactor*length(mydata))
		endvrf = as.integer(length(mydata))
		}else{
		startvrf = as.integer(verifyfactor*length(rownames(mydata)))
		endvrf = as.integer(length(rownames(mydata)))
		}


### Set ML Equation for calibration#
if((nprd-firstprd) == 0){fitc <- lm(predictor_eq[1:startvrf]~mydata[1:startvrf])
		}else{
		fitc <- lm(predictor_eq[1:startvrf]~.,data=mydata[1:startvrf,])
		}
### Verification
### Predict ML Equation for verification#
if((nprd-firstprd) == 0){fitv <- fitc$coefficients[1]+fitc$coefficients[2]*mydata[(startvrf+1):endvrf]
		}else{
		fitv <- predict(fitc,mydata[(startvrf+1):endvrf,])
		}
obsv = predictor_eq[(startvrf+1):endvrf]


### Set ML Equation #
if((nprd-firstprd) == 0){fit <- lm(predictor_eq~mydata)
		}else{
		fit <- lm(predictor_eq~.,data=mydata)
		}


#Define Predictor data set and row's name
predictorcol = colpool[firstprd:nprd]
predictordata = datacut[,predictorcol]
predictordata = data.frame(predictordata)
rownames(predictordata) = as.numeric(rownames(datacut))

#Define col of predictor which have no NA
#cmp.data.predictor = na.omit(predictortdata)
cmp.data.predictor = na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),])

#cmp.row.predictor = as.numeric(rownames(na.omit(predictortdata)))
#cmp.row.predictor = rownames(na.omit(predictortdata))
#cmprow = which(is.na(predictordata[which(is.na(filledpredictant[,1])==TRUE),])==FALSE)
cmp.row.predictor = rownames(na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),]))
# for 1 dimension predictor
if((nprd-firstprd)==0){cmp.row.predictor = which(is.na(predictordata[which(is.na(filledpredictant[,1])==TRUE),])==FALSE)}
#which(is.na(predictordata)==FALSE & is.na(filledpredictant[,1])==TRUE)

#Define predicror data frame
frameprd = data.frame(cmp.data.predictor)

#### PREDICTION #####
# Filling data by prediction funtion / check null row
if(is.null(cmp.row.predictor)== FALSE){
		if(length(cmp.row.predictor)>= 1){
# for >1 dimension predictor
		if((nprd-firstprd)!=0){	filledpredictant[cmp.row.predictor,1] = predict(fit,na.omit(frameprd))}
# for 1 dimension predictor
		if((nprd-firstprd)==0){
					t = data.frame(x = na.omit(frameprd)$cmp.data.predictor)
					filledpredictant[cmp.row.predictor,1] = fit$coefficients[2]*t+ fit$coefficients[1]
					}
		#filledpredictant[cmp.row.predictor,1] = predict(fit,cmp.data.predictor)
		# Mark used predictor #
		filledpredictant[cmp.row.predictor,2] = fit$coefficients[1]
		filledpredictant[cmp.row.predictor,(firstprd+2):(nprd+2)] = fit$coefficients[2:(nprd-firstprd+2)]
		filledpredictant[cmp.row.predictor,(extprd+3)] = firstprd
		filledpredictant[cmp.row.predictor,(extprd+4)] = nprd
		filledpredictant[cmp.row.predictor,(extprd+5)] = nprd-firstprd+1
		filledpredictant[cmp.row.predictor,(extprd+6)] = length(fit$residuals)
		filledpredictant[cmp.row.predictor,(extprd+7)] = length(cmp.row.predictor)

		filledpredictant[cmp.row.predictor,(extprd+9)] = mean(fit$residuals)
		filledpredictant[cmp.row.predictor,(extprd+10)] = (mean((fit$residuals)^2))^0.5
		filledpredictant[cmp.row.predictor,(extprd+11)] = 1-sum((fit$residuals)^2)/sum((predictor_eq-mean(predictor_eq))^2)
		filledpredictant[cmp.row.predictor,(extprd+12)] = cor(fit$model[,1],fit$fitted.values)
		filledpredictant[cmp.row.predictor,(extprd+13)] = (cor(fit$model[,1],fit$fitted.values))^2
		filledpredictant[cmp.row.predictor,(extprd+14)] = AIC(fit)
colt = ""
for(np in firstprd:nprd){colt = paste(colt,np)}
		filledpredictant[cmp.row.predictor,(extprd+15)] = colt
filling = length(predictantdata[which(filledpredictant[,extprd+15]==colt),1])-length(removeNA(predictantdata[which(filledpredictant[,extprd+15]==colt),1]))
		filledpredictant[cmp.row.predictor,(extprd+8)] = filling

## under here for calibration and verification 13 column
		filledpredictant[cmp.row.predictor,(extprd+16)] = startvrf
		filledpredictant[cmp.row.predictor,(extprd+17)] = endvrf-startvrf
		filledpredictant[cmp.row.predictor,(extprd+18)] = mean(fitc$residuals)
		filledpredictant[cmp.row.predictor,(extprd+19)] = (mean((fitc$residuals)^2))^0.5
		filledpredictant[cmp.row.predictor,(extprd+20)] = 1-sum((fitc$residuals)^2)/sum((predictor_eq[1:startvrf]-mean(predictor_eq[1:startvrf]))^2)
		filledpredictant[cmp.row.predictor,(extprd+21)] = cor(fitc$model[,1],fitc$fitted.values)
		filledpredictant[cmp.row.predictor,(extprd+22)] = (cor(fitc$model[,1],fitc$fitted.values))^2
		filledpredictant[cmp.row.predictor,(extprd+23)] = AIC(fitc)
		filledpredictant[cmp.row.predictor,(extprd+24)] = mean(fitv-obsv)
		filledpredictant[cmp.row.predictor,(extprd+25)] = (mean((fitv-obsv)^2))^0.5
		filledpredictant[cmp.row.predictor,(extprd+26)] = 1-sum((fitv-obsv)^2)/sum((predictor_eq[startvrf:endvrf]-mean(predictor_eq[startvrf:endvrf]))^2)
		filledpredictant[cmp.row.predictor,(extprd+27)] = cor(fitv,obsv)
		filledpredictant[cmp.row.predictor,(extprd+28)] = (cor(fitv,obsv))^2
								}
						}

cat("\nPredictor column :",colpool[firstprd:df])


}
#############################################################################################
################# END of loop to change number of predictor in equation ######################
}
###################################################################################################
################# END Start loop to change first number of predictor in equation ######################


}
###################################################################################################
# **+++*** END of Run extra loop to extend the predictor to EXTPRD
###################################################################################################





##Write result of different number of predictors ##
setwd(dir1)
sink(paste("Fiiled-prediction",predictant,".csv"))
write.table(filledpredictant, quote = FALSE, sep = ",")
sink()
setwd(dir0)








#Define predictant row that have no NA
#gap = ldata[which(is.na(predictantts[ldata])==TRUE)]

#Define predictant row that have NA
#nogap = ldata[which(is.na(predictantts[ldata])==FALSE)]

# predictantts[nogap]--- all completet predictant
# predictantts[gap]--- all NA predictant
# predictorts[gap,] --- all predictors at NA in predictant

#Define predicror data frame
#frameprd = data.frame(predictorts[gap,])
#Predict the predictant
#fillingna = predict(fit,na.omit(frameprd),na.action = na.exclude)







#nogapwithcompleatedata = data0[nogap,which(is.na(tsdata0[,predictorcol])==FALSE)]

#predictorts = data0[gap,colpool[1:nprd]]
#predictortemp=na.omit(predictorts)
#predictorcompleterow =rownames(na.omit(predictorts))

# NS coef
#1-sum((fit$residuals^2))/sum((predictor_eq - mean(predictor_eq))^2)









runthissub = FALSE
#+-################# SUB4 for calibration and validation of model ########################################################################################################################

if(runthissub){

######################################################################################
######******************** compare model and validation ##############################
######################################################################################

######******************** data for validation here ##################################
start0=1971  #year start in table file
startyear=2001 #year to analysis begins
endyear=2005
endday=90
ifinal=min(which(data$year==(endyear+1)))-1 + endday
datafinal = length(data$year)

subty = (startyear-start0)*365
#DAI
vdata2=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:25)]
#2D
vdata3=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,26:50)]

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
	if(model=="DAI"){vrf=vdata2}
	if(model=="2D"){vrf=vdata3}
	prd=predict(fit,vrf)
#check conditional rainfall
if(loop>=5){for (m in 1:length(prd)){if(prd[m]<0){prd[m]=0}}}

	lmx = max(max(prd),max(vrf[which(predictant==colnames(vrf))]))
	lmn = min(min(prd),min(vrf[which(predictant==colnames(vrf))]))
	matplot(vrf[which(predictant==colnames(vrf))],prd,pch="X",xlab="Observation",ylab="Prediction",xlim=c(lmn,lmx),ylim=c(lmn,lmx),type="p",col = "red", main=paste("Model verification 2001-2006 by ",method[i]," method\nprediction of ",predictant," from CGCM3-",model))
	matlines (c(-100,100), c(-100,100), type = "l", lty = "dashed", lwd = 1, pch = NULL, col = 1:2)

#***********calibration summary result
if(model=="DAI"){
			calf =predict(fit,data2)
			cal = cbind(data2[which(predictant==colnames(vrf))],calf)
		    }
if(model=="2D"){
			calf = predict(fit,data3)
			cal = cbind(data3[which(predictant==colnames(vrf))],calf)
		    }
	calerror[i,1,loop] = predictant
	calerror[i,2,loop] = model
	calerror[i,3,loop] = mean(cal[which(is.na(cal[,1])==FALSE),2]-cal[which(is.na(cal[,1])==FALSE),1])
	calerror[i,4,loop] = mean(abs(cal[which(is.na(cal[,1])==FALSE),1]-cal[which(is.na(cal[,1])==FALSE),2]))
	calerror[i,5,loop] = sqrt(mean((cal[which(is.na(cal[,1])==FALSE),1]-cal[which(is.na(cal[,1])==FALSE),2])^2))
	calerror[i,6,loop] = 1-sum((cal[which(is.na(cal[,1])==FALSE),2]-cal[which(is.na(cal[,1])==FALSE),1])^2)/sum((cal[which(is.na(cal[,1])==FALSE),1]-mean(cal[which(is.na(cal[,1])==FALSE),1]))^2)

#***********validation summary result
prd =cbind(vrf[which(predictant==colnames(vrf))],prd)
	
	prderror[i,1,loop] = predictant
	prderror[i,2,loop] = model
	prderror[i,3,loop] = mean(prd[,2]-prd[,1])
	prderror[i,4,loop] = mean(abs(prd[,1]-prd[,2]))
	prderror[i,5,loop] = sqrt(mean((prd[,1]-prd[,2])^2))
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
opteq[,loop] = vorder[1:nmaxprd,min(which(calerror[,5,loop]==min(calerror[,5,loop])))]


setwd(dir0)
dev.off()


############################################ END big loop of multi regression $$$$$$$$$$$$$$

######################################################################################
######******************** print multi-regression models and validations #############
######################################################################################

#***********print calibration summary result
setwd(dir1)
sink("Concl-Mlt-Rgrssn-prd.csv")
setwd(dir0)
cat("Predictant,GCM,Mothed,MERROR,ABSERROR,RMSE,Nash–Sutcliffe \n") 
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
cat("Predictant,GCM,Mothed,MERROR,ABSERROR,RMSE,Nash–Sutcliffe \n") 
for(i in 1:6){
	for(mthd in 1:4){
		cat(calerror[mthd,1,i],",",calerror[mthd,2,i],",",method[mthd],",",calerror[mthd,3,i],",",calerror[mthd,4,i],",",calerror[mthd,5,i],",",prderror[mthd,6,i],"\n")
			 }
		 }
sink()

#####################################
########## print time-series results
for(i in 1:6){
		if(i%%2==1){mydata=data2[,-c(1:3)]}
		if(i%%2==0){mydata=data3[,-c(1:3)]}
		predictor_eq = data2[,(i+i%%2)/2]
		vdata = mydata[,opteq[1:nmaxprd,i]]
		fit <- lm(predictor_eq~.,data=vdata)
		resultc[,i] =  predict(fit,data[-c(length(data[,1])),])
		}
setwd(dir1)
sink("Regression-Result.csv")
setwd(dir0)
cat(calerror[1,1,1],"-",calerror[1,2,1],",",calerror[1,1,2],"-",calerror[1,2,2],",",calerror[1,1,3],"-",calerror[1,2,3],",",calerror[1,1,4],"-",calerror[1,2,4],",",calerror[1,1,5],"-",calerror[1,2,5],",",calerror[1,1,6],"-",calerror[1,2,6],"\n")
for(i in 1:length(resultc[,1])){
					for(j in 1:6){
							if(j>=5){if(resultc[i,j]<0){resultc[i,j]=0}}
							cat(resultc[i,j],",")
							}
					cat("\n")
					}
sink()

}
#+-################# END OF SUB4 for calibration and validation of model ####

dev.off()
setwd(dir0)



