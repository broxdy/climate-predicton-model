# 1)
# Working directory and file
#ML-regression anlysis + correlation + line-charts of time series
# You have to define the time-series data in parameter below
# 1a) Current working folder
mdir ="Temp/filling-NA"


# 1b) Input file
#dataf = "RainRayong-1952rv4_366days.csv"
#dataf = "Obs-hmd1971v2.csv"
#dataf = "Slr+Obs-temp-hmd1971.csv"
dataf = "Obs-temp1971.csv"
first.unwantedcol = 2

# 1c) Define input data information (first must start date with 1/1/xxxx)
start0=1952 #year start in table file
startyear=1952 #year to analysis begins
endyear=2007
endday=0 # 0 means end of endyear (endyear  1, endyear=2006, endday=0 ** end date = 31/12/2006)

# 1d) Define if need to process data for the entire input (from start to end of file), if set TRUE the start0,startyear,endyear,endday will be inactive
fromstarttoend = TRUE

 
# 1e)
# You have to chnage the rank of correlation column, you can define below
# If it's all direct variation, at the end, the multi-linear regression equation should have no negative coef
# For 
# or use entire col to predict by use delete header of each row (first.unwantedcol)
use.allcol = TRUE
if(use.allcol == FALSE){allcolpool=c(1,2,3)}

# 2)
# Creat new directory to save results and change working directory into (dir_name) or (maindir)
#dirn = "NAv5-3RainRayong-1952rv4_366days-without_intercept8-4-24-24-4-1-byr2"
dirn = "NAv6Rayong-tmp1971-with_intercept7-4-7-7-4-1-byr2"
output.dec = 1



# 3)
# Define predictant column (predictantcol) or define "pdtcol" (refer from data0) first column is at first data column (after date and year column)
# and define max number of predictors or define "pdtcol" below
pdtcol = 1
first.min.predictor = 7 # minimum number of predictors used in prediction at first loop
last.min.predictor = 4 # minimum number of predictors used in prediction at last loop
first.max.predictor = 7 # max number of predictors selected at first time
maxpredictor = 7 # entirely maximum included number of predictors in extra prediction (extprd)
run2loop = TRUE # to run extra loop when all first.max.predictor are all choosen, then extend to lower
# Ranked by R2 and select first top, then select the max number of predictors
# if FALSE it will be ranked by AIC
rank.by.r2 = TRUE

# 3)b
# Define the cirteria to extra loop
comb.max.nprd = 4 # max combination of predictor in combination loop
comb.min.nprd = 1 # min combination of predictor in combination loop
max.combination = 20000 # maximum of combination of prediction to reach in EXTRA loop

# 3)c
# Defien the formulation of fitted equation that intercept = 0 or not
zero.intercept = FALSE


# 4)
# Run the script


# 6)
# Find the best number of predictors, by less AIC (col 4) at enought DF(degree of freedom / data used - col 5)
# And the optimal number of predictors in "Daily-ML-RegressionAnalysis.pdf"

# 7)
# Define best number of predictors as below
n.optimalr2 = 1 # number to select the best optimal number of prediction (1-3 is OK)
use.optimalr2 = TRUE
if(use.optimalr2 == FALSE){colbest = 7}

# 8)
# Check coefficient in file "Filled-prediction ...." that should be reasonable
# Then extract this file to "Filled-prediction .... .xls" at sheet Fiiled-prediction....(new)

#
# 9) Define the file which all predicted data and old data will be printed in
# prdrslt = "prdall.csv"
#

# 10)
# Check result at "Residuals of prediction.csv" in working directory



#load libraries needed
library(timeSeries)
#library(Kendall)
#library(Rwave)
library(TSA)
#library(wmtsa)
#library(plotrix)
library(car)
library(MASS)


















runthissub1 = TRUE
#+-################# SUB1 for starting at first time ####
if(runthissub1){

########################define file for input / read #############@###################
data <- read.table(dataf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("Daily-OBS-CGCM3_A1B-4-1952rv1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)
#output goes to subdirectory with name of station


dir_name=dirn
maindir = mdir

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name)
dirfile=(dir_name)
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
#tsrain=ts(data,start=c(1952,1),frequency=365)

cat("\n Finish reading data source \n")
}
#+-################# END OF SUB1 for starting at first time ####
runthissub1 = FALSE




if(use.allcol == TRUE){
				lcol = length(colnames(data))-first.unwantedcol
				allcolpool = c(1:lcol)
				}else{
					lcol = length(allcolpool)
					}





runthissub2 =TRUE
#+-################# SUB2 for correlation analysis and plot time-series / to give correlation matrix ####
if(runthissub2){
#******** check correlation ***************
obs = data[,-c(1:first.unwantedcol)]
nobs = length(colnames(obs))
obsccf = matrix(0,nrow = nobs,ncol = nobs)
nccf = matrix(0,nrow = nobs,ncol = nobs)


for (i in 1:nobs){
			for (j in i:nobs){
						if(nrow(na.exclude(cbind(obs[i],obs[j])))>1){
									checkccf = ccf(obs[i],obs[j], type = c("correlation"),lag.max = 1,plot = FALSE, na.action = na.exclude)
									obsccf[i,j] = checkccf$acf[2]
									nccf[i,j] =  checkccf$n.used
							}else{checkccf = NA
								obsccf[i,j] = NA
								nccf[i,j] = 0
								}
				
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

cat("\n Finisch analysing cross-correlation")
}
#+-################# END OF SUB2 for correlation analysis and plot time-series ####

runthissub2 = FALSE

##########################################The stuff above needs to be processed only once.################################






####
################# great loop to run every column ###################
####

# Set matrix for prediction results
datafinal = length(data$year)
predict.results = as.data.frame(array(NA,c(datafinal,(lcol*2))))


for(gloop in 1:lcol){

## define pool of predictors each loop
#colpool=allcolpool[-gloop]

# rank the ccf of predictor, then put the order into colpool
colpool=order(obsccf[gloop,], decreasing = TRUE)[-1]
pdtcol = gloop






runthissub = TRUE
#+-################# SUB3 for Multi--Linear regression ANALYSIS of predictors number / R2 / AIC ####
if(runthissub){

#########################################################################################
######################################### multiple linear regression ####################
#########################################################################################


######################## define detail for comparison ###################################

############# year of observation #########################



# For dialy time-series
ifinal = (min(which(data$year>=(endyear+1))))-1 + endday
datafinal = length(data$year)
subty = (startyear-start0)*365

#******************** remove unwanted column here ##################################


#Index Matrix for all data using in calculation
alldata = data[,-c(1:2)]

if(fromstarttoend == FALSE){
#
data0=data[-c(0:subty,(ifinal+1):datafinal),-c(1:2)]
#OBS - data2
data2=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,unwanted.part2col)]
#PREDICTOR - data3
data3=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,unwanted.part3col)]
}
else{
#
data0=data[,-c(1:2)]
#OBS - data2
data2=data[,-c(1,2,47:94)]
#PREDICTOR - data3
data3=data[,-c(1,2,1:46)]
}
nparmat2 = length(colnames(data2))
nparmat3 = length(colnames(data3))


##### Set parameters for Analysis of Prediction / Predictant / Predictors ###############
########################################################################################

###############################################
## LOOP check different number of predictors ##
###############################################
#Define maximum number of predictor
maxnprd = maxpredictor
rs = array(NA, c(maxnprd, 5))


for(i in 2:maxnprd){

#Define predictant colum (refer from data0)
predictantcol = pdtcol

##Define number of predictors
nprd = i

datapool=data0[,colpool[1:nprd]]

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

################################
## loop to check available data
################################
if(length(pooltemp) > 0){

predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

# If need the intercept to be zero
if(zero.intercept){
			fit = lm(predictor_eq~.-1,data=mydata)
			}else{fit = lm(predictor_eq~.,data=mydata)}


colnames(rs) = c("Number of predictor","R2","AIC","DF","PredictorCol")
rs[i,1] = i
rs[i,2] = summary(fit)$r.squared
rs[i,3] = AIC(fit)
rs[i,4] = fit$df
colt = ""
for(k in 1:nprd){colt = paste(colt,colpool[k])}
rs[i,5] = colt

}
################################
## END loop to check available data
################################

}
## END of LOOP check different number of predictors ##

##Write result of different number of predictors ##
setwd(dir1)
sink(paste("Col-",gloop," Residuals of prediction.csv"))
write.table(rs, quote = FALSE, sep = ",")
sink()
setwd(dir0)

cat("\nColumn: ",gloop," Finish calculating prediction residuals")








runthissub = FALSE
#+-################# SUB3a for PDF ####
if(runthissub){

######## Print result of regression/predictors analysis to PDF ####
####################### ++ title on page ++

setwd(dir1)
pdf(paste("Col-",gloop," Daily-ML-RegressionAnalysis.pdf"), width= 12, height = 12)

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

leaps = regsubsets(predictor_eq~.,data=mydata,really.big=TRUE)

#subsets(leaps, statistic="rsq",legend=FALSE,max.size=8,abbrev=6, main="Sets of predictors and the regression")
vorder=array(leaps$vorder,c(length(leaps$vorder),4))
method=array(leaps$method,c(length(leaps$method),4))



########################################## Method of selection - bar charts
par(ps =16,mfrow=c(4,1))

#leaps = regsubsets(mydata[,1]~.,data=mydata,method=c("exhaustive", "backward", "forward", "seqrep"),nbest=1)
leaps = regsubsets(predictor_eq~.,data=mydata,method=c("exhaustive"),really.big=TRUE,nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: exhaustive"),scale="r2") 

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("backward"),nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: backward"),scale="r2") 

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("forward"),nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: forward"),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("seqrep"),nbest=1)
plot(leaps, main=paste("Selection of best predictor set for",predictant,"\nMethod: seqrep"),scale="r2") 

cat("\nColumn: ",gloop," Predictor selection:BarChart")

########################################## Method of selection - line charts
par(ps =9,mfrow=c(1,1))

leaps = regsubsets(predictor_eq~.,data=mydata,method=c("exhaustive"),really.big=TRUE,nbest=1)
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

cat("\nColumn: ",gloop," Predictor selection:LineChart")

########################################## Number of  predictors
par(ps =12,mfrow=c(1,1))

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=9)
plot(leaps, main=paste("Selection of 9 best predictors for",predictant),scale="r2")

dev.off()
cat("\nColumn: ",gloop," Predictor selection:Number of predictor")

cat("\nColumn: ",gloop," Finish plot preidorctor selection charts")

}
#+-################# END SUB3a for PDF ####


}
#+-################# END of SUB3 for Multi--Linear regression analysis of predictors number / R2 / AIC ####










runthissub = TRUE
#+-################# SUB4 for Run regression to fill the missing data NA ####
if(runthissub){

##################################################################################################################################################################################################################################################################

######################################################################################
###### Run regression to fill the missing data NA in predictant ######################
######################################################################################

#Define predictant colum (refer from data0)
predictantcol = pdtcol

##############
#Define column for predictor's pool (ranked by Regression coefficient)
##############
## with negative coef
#colpool=c(30,18,7,2,4,16,32,26,15,8,11,12,9,27,14,1,31,3,28,25,29,13,17,19,22,5,10,6,24,21,23)
## without negative coef
colpool=colpool

################
# Define Optimal number of predictor here
################
## Define MAXIMUM number of predictors
# Rank order by
if(rank.by.r2)	{first.nprdmax = max(order(as.numeric(rs[(1:first.max.predictor),2]), decreasing = TRUE)[1:n.optimalr2]) # Ranked by R2 and select first top, then select the max number of predictors
			}else{first.nprdmax = max(order(as.numeric(rs[(1:first.max.predictor),3]), decreasing = FALSE)[1:n.optimalr2])} # Ranked by AIC and select first top, then select the max number of predictors

				
## Define all possible predictor number
all.possible.prd = max(order(as.numeric(rs[,3]), decreasing = FALSE)[1:n.optimalr2])


# **????*** Run extra loop to extend the predictor to EXTPRD when reaching end of standard prediction
runextprd = run2loop
##Define MAXIMUM number of extended predictors
extprd = maxpredictor
verifyfactor = 0.5

# Change max number of predictors, if data are not enough
if(all.possible.prd < extprd){extprd=all.possible.prd}



###################################################################################################
################# Start loop to change MINIMUM number of predictor in equation ####################
for(min.predictor in first.min.predictor:last.min.predictor){




firstloop = TRUE

###################################################################################################
################# Start loop to change first number of predictor in equation ######################
# set first predictor
for(firstprd in 1:first.nprdmax){

# if number of predictor (first.nprdmax-firstprd) is lower than minimum predictor (min.predictor)
if(min.predictor <= (first.nprdmax-firstprd+1))	{
								limit.n.prd = firstprd+min.predictor-1 # normal case: when number of predictor (first.nprdmax-firstprd) is more than min.predictor, set the last predictor (limit.n.prd) to firstprd + minimum number of predictor
								}else	{
									limit.n.prd = first.nprdmax # minimum case: when number of predictor (first.nprdmax-firstprd) reach min.predictor, set the last predictor (limit.n.prd) to max number of predictor
									firstprd = 1 # minimum case: when number of predictor (first.nprdmax-firstprd) reach min.predictor, set the first predictor (firstprd) to 1
									}

#############################################################################################
################# Start loop to change number of predictor in equation ######################
for(df in first.nprdmax:limit.n.prd){



##Define number of predictors / control with minimum predictor (min.predictor)
if((df-firstprd) < min.predictor){df = firstprd+min.predictor-1}
nprd = df

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
pooltemp0=cbind(datapool,data0[,predictantcol])

#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp0)





################################
# check to run only first loop #
if(firstloop){

firstloop = FALSE
# Cut only focus NA or missing data

ifinal = (min(which(data$year>=(endyear+1))))-1 + endday
datafinal = length(data$year)
subty = (startyear-start0)*365

#******************** remove unwanted column here ##################################

if(fromstarttoend == FALSE){
datacut=data[-c(0:subty,(ifinal+1):datafinal),-c(1:2)]
}
else{
datacut=data[,-c(1:2)]
}

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

## Define pivot table the first line is for banking row
pivot.predictant = filledpredictant[1,]

## Creat calibration + verify storage
calbvrf = datacut[,predictantcol]
calbvrf = cbind(calbvrf,array(NA,c(nrow(calbvrf),1)))
calbvrf[,2] = 1
colnames(calbvrf) = c("measured","checked")


}
#######################################
# END of check to run only first loop #



filling = 0

## ************************************************* ##
## loop to check available predictor data (pooltemp) ##
#######################################################
if(length(pooltemp) > 0){


if((nprd-firstprd) == 0){commonrowmydata = which(is.na(pooltemp0[,1])==FALSE & is.na(pooltemp0[,2])==FALSE)
		}else{commonrowmydata = rownames(pooltemp)}

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
if((nprd-firstprd) == 0){
				if(zero.intercept){
							fitc <- lm(predictor_eq[1:startvrf]~mydata[1:startvrf]-1)
							}else{fitc <- lm(predictor_eq[1:startvrf]~mydata[1:startvrf])}
				}else	{
					if(zero.intercept){
								fitc <- lm(predictor_eq[1:startvrf]~.-1,data=mydata[1:startvrf,])
								}else{fitc <- lm(predictor_eq[1:startvrf]~.,data=mydata[1:startvrf,])}
				}
### Verification
### Predict ML Equation for verification#
if((nprd-firstprd) == 0)	{
					if(zero.intercept){fitv <- fitc$coefficients[1]*mydata[(startvrf+1):endvrf]
								}else{fitv <- fitc$coefficients[1]+fitc$coefficients[2]*mydata[(startvrf+1):endvrf]}
					}else	{
						fitv <- predict(fitc,mydata[(startvrf+1):endvrf,])
						}
obsv = predictor_eq[(startvrf+1):endvrf]


### Set ML Equation #
if((nprd-firstprd) == 0){
				if(zero.intercept){
							fit <- lm(predictor_eq~mydata-1)
							}else{fit <- lm(predictor_eq~mydata)}
		}else	{
			if(zero.intercept){
						fit <- lm(predictor_eq~.-1,data=mydata)
						}else{fit <- lm(predictor_eq~.,data=mydata)}
			}	


#Define Predictor data set and row's name
predictorcol = colpool[firstprd:nprd]
predictordata = datacut[,predictorcol]
predictordata = data.frame(predictordata)
rownames(predictordata) = as.numeric(rownames(datacut))


#Define col of predictor which have no NA
#cmp.data.predictor = na.omit(predictortdata)
cmp.data.predictor = na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),])

# define row to fill data in case that there is only one predictor
if((nprd-firstprd)==0){
temp.mat = cbind(is.na(filledpredictant[,1]),!is.na(predictordata[,1]))
cmp.row.predictor = which((temp.mat[,1] ++ temp.mat[,2])==2)
# in case that predictor > 1
				}else{cmp.row.predictor = rownames(na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),]))}

#Define predicror data frame
frameprd = data.frame(cmp.data.predictor)

#### PREDICTION #####
# Filling data by prediction funtion / check null row
if(is.null(cmp.row.predictor)== FALSE){
		if(length(cmp.row.predictor)>= 1){
# for >1 dimension predictor
		if((nprd-firstprd)!=0){	filledpredictant[cmp.row.predictor,1] = round(predict(fit,na.omit(frameprd)), output.dec)}
# for 1 dimension predictor
		if((nprd-firstprd)==0){
					t = data.frame(x = na.omit(frameprd)$cmp.data.predictor)
					if(zero.intercept){filledpredictant[cmp.row.predictor,1] = round(fit$coefficients[1]*t, output.dec)
								}else{filledpredictant[cmp.row.predictor,1] = round(fit$coefficients[2]*t+ fit$coefficients[1], output.dec)}
					}
		#filledpredictant[cmp.row.predictor,1] = predict(fit,cmp.data.predictor)
		# Mark used predictor #

		# Check intercept, if set to be zero
		if(zero.intercept){
					filledpredictant[cmp.row.predictor,2] = 0
					}else{filledpredictant[cmp.row.predictor,2] = fit$coefficients[1]}

		if(zero.intercept){
					filledpredictant[cmp.row.predictor,(firstprd+2):(nprd+2)] = matrix(fit$coefficients[1:(nprd-firstprd+1)],nrow=length(cmp.row.predictor),ncol=(nprd-firstprd+1),byrow=TRUE)
					}else{filledpredictant[cmp.row.predictor,(firstprd+2):(nprd+2)] = matrix(fit$coefficients[2:(nprd-firstprd+2)],nrow=length(cmp.row.predictor),ncol=(nprd-firstprd+1),byrow=TRUE)}

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

		# Define value to pivot table
		if(is.null(cmp.row.predictor)== FALSE){pivot.predictant=rbind(pivot.predictant,filledpredictant[cmp.row.predictor[1],1:(extprd+28)])}
		}
	}

# Storage calibration & Verification results
calbvrf = cbind(calbvrf,array(NA,c(nrow(calbvrf),2)))
colnames(calbvrf)[(length(colnames(calbvrf))-1):length(colnames(calbvrf))] = c(paste(colt,"cal/vrf"),colt)
rownames(calbvrf) = as.numeric(rownames(datacut))

if(length(commonrowmydata) == 1){calbvrf[commonrowmydata,length(colnames(calbvrf))] =  fitv
				calbvrf[commonrowmydata,length(colnames(calbvrf))-1] = 1
	}else{
	calbvrf[commonrowmydata,length(colnames(calbvrf))] = c(fitc$fitted.values,fitv)
	calbvrf[commonrowmydata,length(colnames(calbvrf))-1] = c(array(0,c(startvrf)),array(1,c(endvrf-startvrf)))
}


}
## ***************************************************** ##
## end loop to check available predictor data (pooltemp) ##
###########################################################


# Display calculated column each loop
cat("\nColumn",gloop,": (n =",format(length(cmp.row.predictor)),"Filled =",filling,
#")#firstprd:",firstprd,";nprd:",nprd,
"[min prd.=",min.predictor,"] Predictor:",colpool[firstprd:df])

## in case no available data
if(length(pooltemp) < 1){cat("not available predictor")}




}
#############################################################################################
################# END of loop to change number of predictor in equation ######################
}
###################################################################################################
################# END Start loop to change first number of predictor in equation ######################

}
###################################################################################################
################# END Start loop to change MINIMUM number of predictor in equation ####################







###################################################################################################
# **++++*** Run extra loop to find all possibilities  (using combinatorics) ###
###################################################################################################
#### duplicate with the upper module ##############################################################



## Set extra loop as default value as set above
runextprd.prd.check = runextprd
## Check that there are still any available data for extra loop
#if( (first.nprdmax+1) >= all.possible.prd ){runextprd.prd.check = FALSE}


if(runextprd.prd.check){
firstloop = FALSE
runextprd.prd.check = runextprd


###################################################################################################
################# Start loop to change first number of predictor in equation ######################

########################################
#### Loop to change number of predictors
# to find combination set of predictors
for(n.digit in comb.max.nprd:comb.min.nprd){

# generate combination set of predictors
comb.pool = combn(colpool, n.digit)

if(max.combination > length(comb.pool[1,]))	{
								max.comb.loop=length(comb.pool[1,])
								}else{max.comb.loop=max.combination}
#### Loop to select the combination number from comb.pool
for(comb.loop in 1:max.comb.loop){

# Define start and end number of predictor
firstprd = 1
df = n.digit


##Define number of predictors
nprd = df
datapool=data0[,comb.pool[,comb.loop]]

predictant = colnames(data0)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 

##*??* ###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
pooltemp0=cbind(datapool,data0[,predictantcol])

#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp0)


## ************************************************* ##
## loop to check available predictor data (pooltemp) ##
#######################################################
filling = 0
if(length(pooltemp) > 0){



if((nprd-firstprd) == 0){commonrowmydata = which(is.na(pooltemp0[,1])==FALSE & is.na(pooltemp0[,2])==FALSE)
		}else{commonrowmydata = rownames(pooltemp)}


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
if((nprd-firstprd) == 0){
				if(zero.intercept){
							fitc <- lm(predictor_eq[1:startvrf]~mydata[1:startvrf]-1)
							}else{fitc <- lm(predictor_eq[1:startvrf]~mydata[1:startvrf])}
				}else	{
					if(zero.intercept){
								fitc <- lm(predictor_eq[1:startvrf]~.-1,data=mydata[1:startvrf,])
								}else{fitc <- lm(predictor_eq[1:startvrf]~.,data=mydata[1:startvrf,])}
				}
### Verification
### Predict ML Equation for verification#
if((nprd-firstprd) == 0)	{
					if(zero.intercept){fitv <- fitc$coefficients[1]*mydata[(startvrf+1):endvrf]
								}else{fitv <- fitc$coefficients[1]+fitc$coefficients[2]*mydata[(startvrf+1):endvrf]}
					}else	{
						fitv <- predict(fitc,mydata[(startvrf+1):endvrf,])
						}
obsv = predictor_eq[(startvrf+1):endvrf]


### Set ML Equation #
if((nprd-firstprd) == 0){
				if(zero.intercept){
							fit <- lm(predictor_eq~mydata-1)
							}else{fit <- lm(predictor_eq~mydata)}
		}else	{
			if(zero.intercept){
						fit <- lm(predictor_eq~.-1,data=mydata)
						}else{fit <- lm(predictor_eq~.,data=mydata)}
			}	



#Define Predictor data set and row's name
predictorcol = comb.pool[,comb.loop]
predictordata = datacut[,predictorcol]
predictordata = data.frame(predictordata)
rownames(predictordata) = as.numeric(rownames(datacut))

#Define col of predictor which have no NA
#cmp.data.predictor = na.omit(predictortdata)
cmp.data.predictor = na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),])

# define row to fill data in case that there is only one predictor
if((nprd-firstprd)==0){
temp.mat = cbind(is.na(filledpredictant[,1]),!is.na(predictordata[,1]))
cmp.row.predictor = which((temp.mat[,1] ++ temp.mat[,2])==2)
# in case that predictor > 1
				}else{cmp.row.predictor = rownames(na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),]))}

#Define predicror data frame
frameprd = data.frame(cmp.data.predictor)

#### PREDICTION #####

# Filling data by prediction funtion / check null row
if(is.null(cmp.row.predictor)== FALSE){
		if(length(cmp.row.predictor)>= 1){
# for >1 dimension predictor
		if((nprd-firstprd)!=0){	filledpredictant[cmp.row.predictor,1] = round(predict(fit,na.omit(frameprd)),output.dec)}
# for 1 dimension predictor
		if((nprd-firstprd)==0){
					t = data.frame(x = na.omit(frameprd)$cmp.data.predictor)
					if(zero.intercept){filledpredictant[cmp.row.predictor,1] = round(fit$coefficients[1]*t, output.dec)
								}else{filledpredictant[cmp.row.predictor,1] = round(fit$coefficients[2]*t+ fit$coefficients[1], output.dec)}
					}
		#filledpredictant[cmp.row.predictor,1] = predict(fit,cmp.data.predictor)
		# Mark used predictor #

		# Check intercept, if set to be zero
		if(zero.intercept){
					filledpredictant[cmp.row.predictor,2] = 0
					}else{filledpredictant[cmp.row.predictor,2] = fit$coefficients[1]}

set = comb.pool[,comb.loop]
first.post = TRUE
for(nn in 1:length(set)){
				if(!first.post)	{post = append(post,which(colpool==set[nn]))
							}else{post = which(colpool==set[nn])}
				first.post = FALSE
				}

		# Check intercept, if set to be zero
		if(zero.intercept){
					filledpredictant[cmp.row.predictor,post+2] = matrix(fit$coefficients[1:(nprd-firstprd+1)],nrow=length(cmp.row.predictor),ncol=(nprd-firstprd+1),byrow=TRUE)
					}else{filledpredictant[cmp.row.predictor,post+2] = matrix(fit$coefficients[2:(nprd-firstprd+2)],nrow=length(cmp.row.predictor),ncol=(nprd-firstprd+1),byrow=TRUE)}

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
		# Define value to pivot table
		if(is.null(cmp.row.predictor)== FALSE){pivot.predictant=rbind(pivot.predictant,filledpredictant[cmp.row.predictor[1],1:(extprd+28)])}

								}
						}
# END Filling data by prediction funtion / check null row


runthissub = FALSE
#+-################# SUB4b for recording calibration and verification each predictor-set ####
if(runthissub){

# Storage calibration & Verification results
# If the prediction fill the missing value then put it in calibratino/verification table
if(length(cmp.row.predictor) != 0){
	calbvrf = cbind(calbvrf,array(NA,c(nrow(calbvrf),2)))
	colnames(calbvrf)[(length(colnames(calbvrf))-1):length(colnames(calbvrf))] = c(paste(colt,"cal/vrf"),colt)
	rownames(calbvrf) = as.numeric(rownames(datacut))
	if(length(commonrowmydata) == 1){calbvrf[commonrowmydata,length(colnames(calbvrf))] =  fitv
						calbvrf[commonrowmydata,length(colnames(calbvrf))-1] = 1
						}else{
							calbvrf[commonrowmydata,length(colnames(calbvrf))] = c(fitc$fitted.values,fitv)
							calbvrf[commonrowmydata,length(colnames(calbvrf))-1] = c(array(0,c(startvrf)),array(1,c(endvrf-startvrf)))
							}
	}

}
#+-################# END SUB4b for recording calibration and verification each predictor-set ####



}
## ***************************************************** ##
## end loop to check available predictor data (pooltemp) ##
###########################################################


# Display calculated column each loop
cat("\nColumn",gloop,": (n =",format(length(cmp.row.predictor)),"Filled =",filling,") [combination]Predictor:",comb.pool[,comb.loop])
## in case no available data
if(length(pooltemp) < 1){cat(":not available predictor")}




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


runthissub = FALSE
#+-################# SUB5 for Run writing Cal-Vrf results ####
if(runthissub){

sink(paste("Col-",gloop," Filled-prediction",predictant,".csv"))
write.table(filledpredictant, quote = FALSE, sep = ",")
sink()

sink(paste("Col-",gloop," Cal-Vrf-prediction",predictant,".csv"))
write.table(calbvrf, quote = FALSE, sep = ",")
sink()

}
#+-################# END SUB5 for Run writing Cal-Vrf results ####


sink(paste("Col-",gloop," PivotTable",predictant,".csv"))
write.table(pivot.predictant, quote = FALSE, sep = ",")
sink()

# Display
cat("\nColumn: ",gloop,"Finish writing results")


## Put the results in conclude predict result for all predictants
datacheck1 = data0[,gloop]
predict.results[which(is.na(datacheck1)==FALSE),(gloop*2)-1] = datacheck1[which(is.na(datacheck1)==FALSE)]
predict.results[which(is.na(datacheck1)==FALSE),(gloop*2)] = 1

datacheck2 = filledpredictant[,1]
datacheck3 = filledpredictant[,(extprd+11)]

predict.results[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE),(gloop*2)-1] = datacheck2[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)]
predict.results[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE), gloop*2] = datacheck3[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)]

# Display calculated column each loop
cat("\nColumn: ",gloop,"Finish recording results")

# Define column's name on the prediction result
colnames(predict.results)[(gloop*2)-1] = predictant
colnames(predict.results)[(gloop*2)] = paste(predictant,"-Original/NS")


setwd(dir0)




}
#+-################# END SUB4 for Run regression to fill the missing data NA ####


}
####
################# END great loop to run every column ###################
####

setwd(dir1)

sink(paste("Predictant-results.csv"))
write.table(predict.results, quote = FALSE, sep = ",")
sink()


setwd(dir0)



