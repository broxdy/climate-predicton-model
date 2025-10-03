# Great loop to change
# 1)working directory (auto change by config.)
# 2)seasonal file
# 3)maxpredictor
# 4)seasonal month
# run by 2,3,4 seasons

#######################
### 1) Current folder
# Working directory and file 
# ML-regression anlysis + correlation + line-charts of time series
# You have to define the time-series data in parameter below

maindir = "Temp/ML-regression-multiGCM"

###########################
#*********** 1 ************
###########################

# working directory
#dir_name = "ML-multi_config-GCMs1971-1999v5+high res"
dir_name = "ML-multi_config-GCMs1971-1999high res"
# conclusion directory
#c_dir_name = "ML-multi_config-GCMs1971-1999v5+high res-conclusion"
c_dir_name = "ML-multi_config-GCMs1971-1999high res-conclusion"



#######################
### 2) Input file
# Observed file
dataf = "monthly-obs_filled1971-2006(2100)+WetDay+WetRatio.csv"
# Index or GCMs file
#indexf = "GCMs-real_mo-1971-1999v5+high res.csv"
indexf = "GCMs-high res-1971-1999.csv"
# Order of GCMs (get from ts-cross-cor-GCMs-OBSs.R and cut some col. out)

###########################
#*********** 2 ************
###########################

#order.seasonal.file = c("monthly_seasonal-cor_GCM-mo_real-OBS-2seasonV4.csv","monthly_seasonal-cor_GCM-mo_real-OBS-3seasonV4.csv","monthly_seasonal-cor_GCM-mo_real-OBS-4seasonV4.csv")
#order.seasonal.file = c("monthly_seasonal-cor_GCM-mo_real-OBS-2season+WetRate_V5.csv","monthly_seasonal-cor_GCM-mo_real-OBS-3season+WetRate_V5.csv","monthly_seasonal-cor_GCM-mo_real-OBS-4season+WetRate_V5.csv") # with WetDay parameters
#order.seasonal.file = c("CORRL_monthly-v5+high res_2season.csv","CORRL_monthly-v5+high res_3season.csv","CORRL_monthly-v5+high res_4season.csv") # with WetDay parameters
order.seasonal.file = c("CORRL_monthly-high res_2season.csv","CORRL_monthly-high res_3season.csv","CORRL_monthly-high res_4season.csv") # with WetDay parameters



###########################
#*********** 3 ************
###########################

#maxpredictor = c(5,10,15,20,25,30) # entirely maximum included number of predictors in extra prediction (extprd)
maxpredictor = c(5) # entirely maximum included number of predictors in extra prediction (extprd)


#######################
### 3) Define if need to process data for the entire input (from start to end of file), if set TRUE the start0,startyear,endyear,endday will be inactive
fromstarttoend = FALSE
# or use entire col to predict by use delete header of each row (first.unwantedcol.obs)
use.allcol = TRUE
if(use.allcol == FALSE){allcolpool=c(1,2,3)}
 
#######################
### 4)
output.dec = 1

#######################
### 5) Define predictant column (predictantcol) or define "pdtcol" (refer from data.obs) first column is at first data column (after date and year column)
# Define rank of index by file or by cross-correlation
use.rank.file = TRUE

# if true the loop will run for yearly + 4 seasons
use.seasonal.file = TRUE

# First column to use for predictor
firstprd = 1


# Ranked by R2 and select first top, then select the max number of predictors
# if FALSE it will be ranked by AIC
rank.by.r2 = TRUE

# if TRUE it will be ranked by R2 first, then AIC
rank.by.r2AIC = TRUE

# Define best number of predictors as below
n.optimalr2 = 1 # number to select the best optimal number of prediction (1-3 is OK)

#######################
### 6)
# Defien the formulation of fitted equation that intercept = 0 or not
zero.intercept = FALSE

#######################
### 7a) Define OBS data information (first must start date with 1/1/xxxx)

#######################
## DATA for Observation
#######################
############# spatial info of obs #########################
monthly.time.obs = TRUE # if false then daily
first.unwantedcol.obs = 2

start0.obs=1971  #year start in table file
startyear.obs=1971 #year to analysis begins
startmonth.obs=1
startmonth0.obs=1
endyear.obs=1999 # if 12/2000 have to set to endyear= 2000
endmonth.obs=12  # if 12/2000 have to set to endmonth= 12
endday.obs = 31
unusedcol.obs = 2


#######################
### 7b) Define INDEX data information (first must start date with 1/1/xxxx)

###########################
## DATA for Regional Index
###########################
############# spatial info of index #########################
monthly.time = TRUE # if false then daily
first.unwantedcol.index = 2

start0.index=1971  #year start in table file
startyear.index=1971 #year to analysis begins
startmonth.index=1
startmonth0.index=1
endyear.index=1999 # if 12/2000 have to set to endyear= 2000
endmonth.index=12 # if 12/2000 have to set to endmonth= 12
# if specific.index.col = TRUE , then define the column , if FALSE then you all column accept first.unwanted col.index
specific.index.col = FALSE
if(specific.index.col){selectd.col.index = c(3:339)}



###########################
#*********** 4 ************
###########################

#######################
### 8) Define season from 1 to 4 less/more

####################################
## Season DEFINATION (1-4 season)#####
####################################

# 2 season
define.2season = c(1,1,1,1,2,2,2,2,2,2,1,1) # define season for 1)dry and 2)wet for Rayong (specific case)
# 3 season
define.3season = c(1,1,2,2,2,2,3,3,3,3,1,1) # define season for 1)winter, 2)summer and 3)rainy
# 4 season
# a = annual, s1=dry season [Oct-Dec] in grey dots, s2= pre-monsoon [Jan-Mar] in blue dots, s3= monsoon1 [Apr-Jun] in red dots and s4= monsoon2 [Jul-Sep] in green dots
define.4season = c(2,2,2,3,3,3,4,4,4,1,1,1) # define season each calendar month

# array of season
define.season = array(c(define.2season,define.3season,define.4season),dim=c(12,3))





#load libraries needed
library(timeSeries)
#library(Kendall)
#library(Rwave)
library(TSA)
#library(wmtsa)
#library(plotrix)
library(car)
library(MASS)


####################################
####### Directory conclusion########
####################################
# Check conclusion directory
L=file.exists(c_dir_name)  
if(!L) c_dirfile=dir.create(c_dir_name)
c_dirfile=(c_dir_name)

dir0=dirname(file.path("D:",paste(maindir),"dummy"))
dir2=dirname(file.path("D:",paste(maindir),paste(c_dirfile),"dummy"))






#################################
## DATA for Observation parameters
##################################
data <- read.table(dataf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

### remove unwanted column here ###
if(monthly.time.obs == FALSE){
	# For dialy time-series
		ifinal.obs = min(which(data$year>=(endyear.obs+1)))-1 + endday.obs
		datafinal.obs = length(data$year)
		subty.obs = (startyear.obs-start0.obs)*365
		data.obs.orgn=data[-c(0:subty.obs,ifinal.obs:datafinal.obs),-c(1:unusedcol.obs)]
	}else{
	# For monthly time-series
		ifinal.obs=min(which(data$year==endyear.obs)) + endmonth.obs
		datafinal.obs = length(data$year)+1
		subty.obs = (startyear.obs-start0.obs)*12
		data.obs.orgn=data[-c(0:subty.obs,ifinal.obs:datafinal.obs),-c(1:unusedcol.obs)]
		}
# Define time-series of observation (monhtly)
ts.obs = ts(data.obs.orgn, frequency=12,start=c(startyear.obs,startmonth.obs))
obs.month = round((time(ts.obs)%%1)*12+1)

npar.obs = length(colnames(data.obs.orgn))

#**********************************#
cat(paste("\n Finish reading data OBS :",npar.obs,"\n"))


###########################
## DATA for Regional Index
###########################
dataindex <- read.table(indexf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

if(specific.index.col==FALSE){selectd.col.index = c((first.unwantedcol.index+1):dim(dataindex)[2])}

ifinal.index=min(which(dataindex$year==endyear.index)) + endmonth.index
datafinal.index = length(dataindex$year)+1
subty.index = (startyear.index-start0.index)*12

#******************** remove unwanted column here ##################################
data.index.orgn =dataindex[-c(0:subty.index,(ifinal.index+1):datafinal.index),selectd.col.index]
#dataindexts =  ts(dataindex0, frequency=12,start=c(startyear.index,startmonth.index))
npar.index = length(colnames(data.index.orgn))

#*******************************#
cat(paste(" Finish reading INDICES :",npar.index,"\n"))










####################################################################################
########################## *** start great loop to change config here ##############
####################################################################################

first.conclude = TRUE
for(i.prd in 1:length(maxpredictor)){
for(i.season in 1:length(define.season[1,])){
total.season = length(unique(define.season[,i.season])) # number of season # Season define the number of seson each month (total 12 month)


data.index = data.index.orgn
data.obs = data.obs.orgn
cat(paste("+++++ \nSeason=",total.season,"(",i.season,"/",length(define.season[1,]),")  ##   Predictor max=",maxpredictor[i.prd],"(",i.prd,"/",length(maxpredictor),")\n+++++\n"))


####### Directory working   ########
dir_name1 = paste(dir_name,"prd",maxpredictor[i.prd],"season",length(unique(define.season[,i.season])))
# Check working directory
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))





###################################
## DATA for ordering the predictors
###################################
dataorder.all <- read.table(order.seasonal.file[i.season], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

first.unused.col = 5
model.col = 2
index.col = 4
obs.col = 5

# Set Obs name
obs.name = unique(dataorder.all[,obs.col])
index.name = unique(dataorder.all[,index.col])

# define order both yearly and seasonal for all observation [year/season,index,obs] -- year/sean: 1=yearly, 2=s1 , 3=s2 , 4=s3 , 5=s4
prd.order.all = array(NA,c(dim(dataorder.all)[2]-first.unused.col,length(index.name),length(obs.name)))



order.all = array(NA,c(length(index.name),length(obs.name)))
model.all = array(NA,c(length(index.name),length(obs.name)))
index.all = array(NA,c(length(index.name),length(obs.name)))

colnames(order.all) = obs.name
colnames(model.all) = obs.name
colnames(index.all) = obs.name


## Start (reading predictor rank) ######

for(i in 1:(length(colnames(dataorder.all))-first.unused.col))	{
					cat(paste("\nCol=",colnames(dataorder.all)[i+first.unused.col],":"))
					for(j in 1:(length(obs.name))){
										selected.row = which(dataorder.all[,obs.col]==obs.name[j])
										cor = abs(as.numeric(dataorder.all[selected.row,i+first.unused.col]))
										cat(paste(obs.name[j],","))
										rank.cor = order(cor,decreasing=TRUE)
										order.all[,j] = rank.cor
										#cat(paste("Prd:",colnames(dataorder.all)[i+first.unused.col],"\n"))
										model.all[,j] = dataorder.all[selected.row,model.col][rank.cor]
										index.all[,j] = dataorder.all[selected.row,index.col][rank.cor]
										}
									# Put the ranking result into order.all
									prd.order.all[i,,] = order.all

									setwd(dir1)
									write.csv(order.all, file=paste(colnames(dataorder.all)[i+first.unused.col],"-order_col.csv"))
									write.csv(model.all, file=paste(colnames(dataorder.all)[i+first.unused.col],"-order_model.csv"))
									write.csv(index.all, file=paste(colnames(dataorder.all)[i+first.unused.col],"order_prd.csv"))
									setwd(dir0)
									#cat(paste("\nOutputs:",colnames(dataorder.all)[i+first.unused.col],"Written\n"))

									}


cat("\nReading and Writting Predictor Rank END\n")




if(use.allcol == TRUE){
				lcol = length(colnames(data.obs))
				allcolpool = c(1:lcol)
				}else{
					lcol = length(allcolpool)
					}





runthissub2 = !use.rank.file
#+-################# SUB2 for correlation analysis and plot time-series / to give correlation matrix ####
if(runthissub2){
#******** check correlation ***************

obsccf = matrix(0,nrow = npar.index,ncol = npar.obs)
nccf = matrix(0,nrow = npar.index,ncol = npar.obs)


for (i in 1:npar.obs){
			for (j in 1:npar.index){
						if(nrow(na.exclude(cbind(data.obs[i],data.index[j])))>1){
									checkccf = ccf(data.obs[i],data.index[j], type = c("correlation"),lag.max = 1,plot = FALSE, na.action = na.exclude)
									obsccf[j,i] = checkccf$acf[2]
									nccf[j,i] =  checkccf$n.used
							}else{checkccf = NA
								obsccf[j,i] = NA
								nccf[j,i] = 0
								}
				
						}
			print(paste("matrix : ",j,",",i))
			}



##Write correlation
setwd(dir1)
write.csv(obsccf,file="corr of all.csv")
setwd(dir0)


##Write n.used
setwd(dir1)
write.table(nccf,file="ncorr of all.csv")
setwd(dir0)

cat("\n Finisch analysing cross-correlation")
}
#+-################# END OF SUB2 for correlation analysis and plot time-series ####



##########################################The stuff above needs to be processed only once.################################






# Set matrix for prediction results
datafinal = length(data$year)
predict.results = as.data.frame(array(NA,c(datafinal,(lcol*2))))

####
################# great loop to run every column ###################
####
cat("\n Start GLOOP")

data.index.origin = data.index
data.obs.origin = data.obs

for(gloop in 1:lcol){
cat(paste("\nLoop",gloop))

pdtcol = gloop

##############
#Define column for predictor's pool (ranked by Regression coefficient)
##############

# rank the ccf of predictor, then put the order into colpool
if(use.rank.file)	{
			if(use.seasonal.file)	{
							colpool.season = prd.order.all[,,gloop]
							}else{colpool = prd.order.all[1,,gloop]}
			}else	{
				colpool=order(obsccf[,gloop], decreasing = TRUE)
				}


# check if seasonal file then run 5 loop, otherwise 1 loop
if(!use.seasonal.file){total.season = 0}


#### if run seasonal file, then season.i = total.seasn+1 
############# loop to run seasonal ###################
####
# Season 0 // season.i = 1 ,Season 1 // season.i = 2
for(season.i in 1:(total.season+1)){

# define colpool for each season
if(use.seasonal.file)	{
				colpool = colpool.season[season.i,]
				if(season.i > 1)	{
							sel.month = which(define.season[,i.season]==(season.i-1))
							sel.row = integer(0)
							for(n in 1:length(sel.month))	{sel.row = c(sel.row,which(obs.month == sel.month[n]))}
							sel.row = sel.row[order(sel.row)]
							data.index = data.index.origin[sel.row,]
							data.obs = 	data.obs.origin[sel.row,]
							}else	{
								data.index = data.index.origin
								data.obs = 	data.obs.origin
								}
				}else	{
					data.index = data.index.origin
					data.obs = 	data.obs.origin
					}


runthissub = TRUE
#+-################# SUB3 for Multi--Linear regression ANALYSIS of predictors number / R2 / AIC ####
if(runthissub){

#########################################################################################
######################################### multiple linear regression ####################
#########################################################################################



##### Set parameters for Analysis of Prediction / Predictant / Predictors ###############
########################################################################################

###############################################
## LOOP check different number of predictors ##
###############################################
#Define maximum number of predictor
maxnprd = maxpredictor[i.prd]
rs = array(NA, c(maxnprd, 5))


for(n in 2:maxnprd){

#Define predictant colum (refer from data.obs)
predictantcol = pdtcol

##Define number of predictors

datapool=data.index[,colpool[1:n]]

predictant = colnames(data.obs)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 

##*??*###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
pooltemp=cbind(datapool,data.obs[,predictantcol])

#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp)

################################
## loop to check available data
################################
if(dim(pooltemp)[1] > 0){

predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

# If need the intercept to be zero
if(zero.intercept){
			fit = lm(predictor_eq~.-1,data=mydata)
			}else{fit = lm(predictor_eq~.,data=mydata)}


colnames(rs) = c("Number of predictor","R2","AIC","DF","PredictorCol")
rs[n,1] = n
rs[n,2] = summary(fit)$r.squared
rs[n,3] = AIC(fit)
rs[n,4] = fit$df
colt = ""
for(k in 1:n){colt = paste(colt,colpool[k])}
rs[n,5] = colt

}
################################
## END loop to check available data
################################

}
## END of LOOP check different number of predictors ##

##Write result of different number of predictors ##
setwd(dir1)
sink(paste("Col-",gloop,"Season-",(season.i-1),"Residuals of prediction.csv"))
write.table(rs, quote = FALSE, sep = ",")
sink()
setwd(dir0)

cat("\nColumn: ",gloop,"Finish calculating prediction residuals")







runthissub = TRUE
#+-################# SUB4a for PDF ####
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
#+-################# END SUB4a for PDF ####


}
#+-################# END of SUB3 for Multi--Linear regression analysis of predictors number / R2 / AIC ####












runthissub = TRUE
#+-################# SUB4 for Run regression to predict data ####
if(runthissub){

##################################################################################################################################################################################################################################################################

######################################################################################
###### Run regression to fill the missing data NA in predictant ######################
######################################################################################

#Define predictant colum (refer from data.obs)
predictantcol = pdtcol

################
# Define Optimal number of predictor here
################
## Define MAXIMUM number of predictors
# Rank order by
max.by.r2 = max(order(as.numeric(rs[(1:maxpredictor[i.prd]),2]), decreasing = TRUE)[1:n.optimalr2]) # Ranked by R2 and select first top, then select the max number of predictors
max.by.AIC = min(order(as.numeric(rs[(1:maxpredictor[i.prd]),3]), decreasing = FALSE)[1:n.optimalr2]) # Ranked by AIC and select first top, then select the max number of predictors

if(rank.by.r2)	{
			if(rank.by.r2AIC)	{first.nprdmax = min(c(max.by.AIC,max.by.r2))
						}else{first.nprdmax = max.by.r2}
			}else{first.nprdmax = max.by.AIC}

				
## Define all possible predictor number
# all.possible.prd = max(order(as.numeric(rs[,3]), decreasing = FALSE)[1:n.optimalr2])



runthis.plot = TRUE
#+-################# SUB4extra for R2 and AIC chart into PDF file ####
if(runthis.plot){

if((gloop == 1)&&(season.i==1)){
			setwd(dir1)
			pdf(paste("R2-AIC charts.pdf"))
			setwd(dir0)
			}

par(mai = c(.8, .8, .2, .9))  # Or change '.9' to a larger number for even 
plot(rs[,2], type = "l", xlab = paste("No. of predictor (",predictant," Season:",(season.i-1),") opt at n=",first.nprdmax), ylab = "R2", lty = 1, lwd = 2)
lines(c(first.nprdmax,first.nprdmax),c(0,1),col="red", lty = 2, lwd = 2)

par(new = TRUE)
plot(rs[,3], type = "l", ann = FALSE, yaxt = "n", col = "blue", lty = 3, lwd = 2)

axis(4)
#legend(x = "bottomleft",  bty = "n",  lty = c(1,2),  lwd = c(2,2), col = c("black", "blue"),  legend = paste(c("R2","AIC"), c("(left  y-axis)", "(right y-axis)")))
legend(x = "top",  bty = "n",  lty = c(1,3,2),  lwd = c(2,2,2), col = c("black", "blue", "red"),  legend = paste(c("R2","AIC","optimal"), c("(left  y-axis)", "(right y-axis)", "(vertical line)")))

mtext("AIC", 4, line = 3, col = "blue")

if((gloop == lcol)&&(season.i==(total.season+1)))	{
			dev.off()
			}

}
#+-################# END SUB4extra for R2 and AIC chart into PDF file ####




##Define MAXIMUM number of extended predictors
extprd = maxpredictor[i.prd]
verifyfactor = 0.5

# Change max number of predictors, if data are not enough
# if(all.possible.prd < extprd){extprd=all.possible.prd}


firstloop = TRUE

#############################################################################################

nprd = first.nprdmax

datapool=data.index[,colpool[firstprd:first.nprdmax]]

predictant = colnames(data.obs)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 

##*??* ###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
pooltemp0=cbind(datapool,data.obs[,predictantcol])

#Screen out the NA from predictant and predictors (data pool and predictant)
pooltemp=na.omit(pooltemp0)





################################
# check to run only first loop #
if(firstloop){

firstloop = FALSE


#******************** remove unwanted column here ##################################

if(fromstarttoend == TRUE){
data.obs=data[,-c(1:unusedcol.obs)]
}

predictantdata = data.obs[,predictantcol]
predictantdata = data.frame(predictantdata)

#Define row's name
rownames(predictantdata) = as.numeric(rownames(data.obs))

#tscutdata.obs=ts(cutdata.obs,start=c(startyear,1),frequency=365)

#Build sheet/data.frame for filled data will be in here / copy form predictant first / Put the matrix of predictor used in prediction
filledpredictant = cbind(predictantdata,matrix(NA, nrow = length(predictantdata), ncol=(extprd+14+13+1)))
filledpredictant = data.frame(filledpredictant)
filledpredictant[,1] = NA
colnames(filledpredictant)= c("Predictant","Intercept",colnames(data.index[,colpool[firstprd:extprd]]),"First Predictor","Last Predictor",
"nPredictor","n-cal","n-used","n-mising","avrg error","RMSQ error","NS coef","cor","R2","AIC","Predictors",

## under here for calibration and verification 12 column - continue from the above line
"Number of Calibration","Number of Verification","C-avrg error","C-RMSQ error","C-NS coef","C-correlation between obs and fitted value","C-R2","C-AIC","V-avrg error","V-RMSQ error","V-NS coef","V-correlation between obs and fitted value","V-R2",
## define additional column
"season")

rownames(filledpredictant)= as.numeric(rownames(data.obs))

## Define pivot table the first line is for banking row
pivot.predictant = filledpredictant[1,]

## Creat calibration + verify storage
calbvrf = data.obs[,predictantcol]
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
if(dim(pooltemp)[1] > 0){


if((first.nprdmax-firstprd) == 0){commonrowmydata = which(is.na(pooltemp0[,1])==FALSE & is.na(pooltemp0[,2])==FALSE)
		}else{commonrowmydata = rownames(pooltemp)}

predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

###
### Calibration & Verification here below
###

if((first.nprdmax-firstprd) == 0){startvrf = as.integer(verifyfactor*length(mydata))
		endvrf = as.integer(length(mydata))
		}else{
		startvrf = as.integer(verifyfactor*length(rownames(mydata))) # using normal separation
		endvrf = as.integer(length(rownames(mydata)))
		}


### Set ML Equation for calibration#
if((first.nprdmax-firstprd) == 0){
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
if((first.nprdmax-firstprd) == 0)	{
					if(zero.intercept){fitv <- fitc$coefficients[1]*mydata[(startvrf+1):endvrf]
								}else{fitv <- fitc$coefficients[1]+fitc$coefficients[2]*mydata[(startvrf+1):endvrf]}
					}else	{
						fitv <- predict(fitc,mydata[(startvrf+1):endvrf,])
						}
obsv = predictor_eq[(startvrf+1):endvrf]


### Set ML Equation #
if((first.nprdmax-firstprd) == 0){
				if(zero.intercept){
							fit <- lm(predictor_eq~mydata-1)
							}else{fit <- lm(predictor_eq~mydata)}
		}else	{
			if(zero.intercept){
						fit <- lm(predictor_eq~.-1,data=mydata)
						}else{fit <- lm(predictor_eq~.,data=mydata)}
			}	


#Define Predictor data set and row's name
predictorcol = colpool[firstprd:first.nprdmax]
predictordata = data.index[,predictorcol]
predictordata = data.frame(predictordata)
rownames(predictordata) = as.numeric(rownames(data.obs))


#Define col of predictor which have no NA
#cmp.data.predictor = na.omit(predictortdata)
cmp.data.predictor = na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),])

# define row to fill data in case that there is only one predictor
if((first.nprdmax-firstprd)==0){
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
		if((first.nprdmax-firstprd)!=0)	{
						p = round(predict(fit,na.omit(frameprd)), output.dec)
						if(zero.intercept){p[which(p<0)] = 0}
						filledpredictant[cmp.row.predictor,1] = p
						}
# for 1 dimension predictor
		if((first.nprdmax-firstprd)==0){
					t = data.frame(x = na.omit(frameprd)$cmp.data.predictor)
					if(zero.intercept){
								p = round(fit$coefficients[1]*t, output.dec)
								p[which(p<0)] = 0
								filledpredictant[cmp.row.predictor,1] = p
								}else{filledpredictant[cmp.row.predictor,1] = round(fit$coefficients[2]*t+ fit$coefficients[1], output.dec)}
					}
		#filledpredictant[cmp.row.predictor,1] = predict(fit,cmp.data.predictor)
		# Mark used predictor #

		# Check intercept, if set to be zero
		if(zero.intercept){
					filledpredictant[cmp.row.predictor,2] = 0
					}else{filledpredictant[cmp.row.predictor,2] = fit$coefficients[1]}

		if(zero.intercept){
					filledpredictant[cmp.row.predictor,(firstprd+2):(first.nprdmax+2)] = matrix(fit$coefficients[1:(first.nprdmax-firstprd+1)],nrow=length(cmp.row.predictor),ncol=(first.nprdmax-firstprd+1),byrow=TRUE)
					}else{filledpredictant[cmp.row.predictor,(firstprd+2):(first.nprdmax+2)] = matrix(fit$coefficients[2:(first.nprdmax-firstprd+2)],nrow=length(cmp.row.predictor),ncol=(first.nprdmax-firstprd+1),byrow=TRUE)}

		filledpredictant[cmp.row.predictor,(extprd+3)] = firstprd
		filledpredictant[cmp.row.predictor,(extprd+4)] = first.nprdmax
		filledpredictant[cmp.row.predictor,(extprd+5)] = first.nprdmax-firstprd+1
		filledpredictant[cmp.row.predictor,(extprd+6)] = length(fit$residuals)
		filledpredictant[cmp.row.predictor,(extprd+7)] = length(cmp.row.predictor)

		filledpredictant[cmp.row.predictor,(extprd+9)] = mean(fit$residuals)
		filledpredictant[cmp.row.predictor,(extprd+10)] = (mean((fit$residuals)^2))^0.5
		filledpredictant[cmp.row.predictor,(extprd+11)] = 1-sum((fit$residuals)^2)/sum((predictor_eq-mean(predictor_eq))^2)
		filledpredictant[cmp.row.predictor,(extprd+12)] = cor(fit$model[,1],fit$fitted.values)
		filledpredictant[cmp.row.predictor,(extprd+13)] = (cor(fit$model[,1],fit$fitted.values))^2
		filledpredictant[cmp.row.predictor,(extprd+14)] = AIC(fit)
colt = ""
for(np in firstprd:first.nprdmax){colt = paste(colt,np)}
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

		# Put season in the pivot
		filledpredictant[cmp.row.predictor,(extprd+29)] = (season.i-1)

		# Define value to pivot table
		if(is.null(cmp.row.predictor)== FALSE){pivot.predictant=rbind(pivot.predictant,filledpredictant[cmp.row.predictor[1],1:(extprd+29)])}
		}
	}

# Storage calibration & Verification results
calbvrf = cbind(calbvrf,array(NA,c(nrow(calbvrf),2)))
colnames(calbvrf)[(length(colnames(calbvrf))-1):length(colnames(calbvrf))] = c(paste(colt,"cal/vrf"),colt)
rownames(calbvrf) = as.numeric(rownames(data.obs))

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
cat("\nColumn: ",gloop, "Season:",(season.i-1),
" (n =",format(length(cmp.row.predictor)),"Filled =",filling,
"(", predictant,")",
"prd_chk:",obs.name[gloop],
#")#firstprd:",firstprd,";first.nprdmax:",first.nprdmax,
"maxR2:",max.by.r2," maxAIC:",max.by.AIC,
"[prd.=",first.nprdmax,"] Predictor:",colpool[firstprd:first.nprdmax])

## in case no available data
if(length(pooltemp) < 1){cat("not available predictor")}









##Write result of different number of predictors ##
runthissub = TRUE
#+-################# SUB5 for Run writing Cal-Vrf results ####
if(runthissub){
setwd(dir1)
write.csv(filledpredictant, file =paste("Col-",gloop,"Season-",(season.i-1)," Filled-prediction",predictant,".csv"))
write.csv(calbvrf, file =paste("Col-",gloop,"Season-",season.i," Cal-Vrf-prediction",predictant,".csv"))
setwd(dir0)
}

#+-################# END SUB5 for Run writing Cal-Vrf results ####


# write column's prediction information
setwd(dir1)
write.csv(pivot.predictant, file=paste("Col-",gloop,"Season-",(season.i-1)," PivotTable",predictant,".csv"))
setwd(dir0)

# attach prediction information into prediction information table
if((gloop == 1)&&(season.i == 1))	{
			pivot.all = pivot.predictant[2,-c(1:(extprd+2))]
			
			}else{pivot.all = rbind(pivot.all,pivot.predictant[2,-c(1:(extprd+2))])}
rownames(pivot.all)[dim(pivot.all)[1]] = paste(predictant,"-",(season.i-1))

# Display
cat("\nColumn: ",gloop,"Finish writing results")


## Put the results in conclude predict result for all predictants
datacheck1 = data.obs[,gloop]
predict.results[which(is.na(datacheck1)==FALSE),(gloop*2)-1] = datacheck1[which(is.na(datacheck1)==FALSE)]
predict.results[which(is.na(datacheck1)==FALSE),(gloop*2)] = 1

datacheck2 = filledpredictant[,1]
datacheck3 = filledpredictant[,(extprd+11)]

predict.results[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE),(gloop*2)-1] = datacheck2[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)]
predict.results[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE), gloop*2] = datacheck3[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)]

# Display calculated column each loop
cat("\nColumn: ",gloop,"Finish recording results\n")

# Define column's name on the prediction result
colnames(predict.results)[(gloop*2)-1] = predictant
colnames(predict.results)[(gloop*2)] = paste(predictant,"-Original/NS")


setwd(dir0)




}
#+-################# END SUB4 for Run regression to predict data ####


if(season.i > 1)	{
			fit.obs = c(fit.obs,fit$model[,1])
			fit.prd = c(fit.prd,fit$fitted.values)
			cal.obs = c(cal.obs,fitc$model[,1])
			cal.prd = c(cal.prd,fitc$fitted.values)
			vrf.obs = c(vrf.obs,obsv)
			vrf.prd = c(vrf.prd,fitv)

			if(season.i == (total.season+1))	{
									# Create pivot matric
									pivot.sum = pivot.all[1,]
									rownames(pivot.sum) = paste(predictant,"all.seasons")

									pivot.sum[] = NA
									pivot.sum[4:5] = length(fit.obs)
									pivot.sum[14] = length(cal.obs)
									pivot.sum[15] = length(vrf.obs)

									# Genral fit model
									pivot.sum[7] = mean(fit.prd-fit.obs)
									pivot.sum[8] = (mean((fit.prd-fit.obs)^2))^0.5
									pivot.sum[9] = 1-sum((fit.prd-fit.obs)^2)/sum((fit.obs-mean(fit.obs))^2)
									pivot.sum[10] = cor(fit.obs,fit.prd)
									pivot.sum[11] = pivot.sum[10]^2

									# Model calibration
									pivot.sum[16] = mean(cal.prd-cal.obs)
									pivot.sum[17] = (mean((cal.prd-cal.obs)^2))^0.5
									pivot.sum[18] = 1-sum((cal.prd-cal.obs)^2)/sum((cal.obs-mean(cal.obs))^2)
									pivot.sum[19] = cor(cal.obs,cal.prd)
									pivot.sum[20] = pivot.sum[19]^2

									# Model verification
									pivot.sum[22] = mean(vrf.prd-vrf.obs)
									pivot.sum[23] = (mean((vrf.prd-vrf.obs)^2))^0.5
									pivot.sum[24] = 1-sum((vrf.prd-vrf.obs)^2)/sum((vrf.obs-mean(vrf.obs))^2)
									pivot.sum[25] = cor(vrf.obs,vrf.prd)
									pivot.sum[26] = pivot.sum[25]^2

									pivot.sum[27] = "all"

									# Combine to pivot.all
									pivot.all = rbind(pivot.all,pivot.sum)

									cat("\n++ Combined all seasonal prediction\n")

									}
			}else	{
				fit.obs = integer(0)
				fit.prd = integer(0)
				cal.obs = integer(0)
				cal.prd = integer(0)
				vrf.obs = integer(0)
				vrf.prd = integer(0)
				}


		
			


}
############# end loop to run seasonal ###################



}
####
################# END great loop to run every column ###################
####


setwd(dir1)

# write the prediction results of all predictant
write.csv(predict.results, file = "Predictant-results.csv")
# write the prediction information of all predictant
write.csv(pivot.all, file="pivot-all.csv")

setwd(dir2)
pivot.print = pivot.all[which(pivot.all[,27] == "all"),c(8,9,17,18,23,24,3)]

## calculation of average NS
n.station = length(which(pivot.all[,27] == "all"))
# Gather all season NS
pivot.print = array(NA,c(total.season,n.station,3)) # c(conclude,cal,vrf),total season,stations)
for(ii in 1:total.season){
pivot.print[ii,,] = unlist(pivot.all[which(pivot.all[,27] == c(ii)),c(9,18,24)])
}

# average all season NS
pivot.ns = array(NA,c(n.station,3)) # c(conclude,cal,vrf),total season,stations)
for(ii in 1:3){
for(jj in 1:n.station){
pivot.ns[jj,ii] = mean(pivot.print[,jj,ii])
}
}
## End calculation of average NS

write.csv(pivot.all[which(pivot.all[,27] == "all"),c(8,9,17,18,23,24,3)], file=paste("conclude-ALL_season-season",length(unique(define.season[,i.season])),"prd",maxpredictor[i.prd],".csv"))
write.csv(pivot.all[which(pivot.all[,27] == 0),c(8,9,17,18,23,24,3)], file=paste("conclude-NO__season-season",length(unique(define.season[,i.season])),"prd",maxpredictor[i.prd],".csv"))
setwd(dir0)

#- build average NS at the end of columns
ns.col = c(9,18,24)
ns.add.col = array(NA, c(n.station*(total.season+2),length(ns.col)))
colnames(ns.add.col) = c("avrg-stNS","avrg-calNS","avrg-vrfNS")

#- calculate average NS
for(ii in 1:n.station){
for(jj in 1:length(ns.col)){
ns.add.col[((ii-1)*(total.season+2)+total.season+2),jj] = mean(pivot.all[(2+(ii-1)*(total.season+2)):((ii-1)*(total.season+2)+total.season+1),ns.col[jj]])
}
}
pivot.all = cbind(pivot.all,ns.add.col)

# build a conclusion for all season
if(first.conclude){
			temp.table = pivot.all[which(pivot.all[,27] == "all"),c(8,9,28,17,18,29,23,24,30,3)]
			colnames(temp.table) = paste(colnames(temp.table),"s:",total.season,"prd:",maxpredictor[i.prd])
			# Put the season 0 into first column each PRD change loop
			if(i.season == 1){
						temp.table0 = pivot.all[which(pivot.all[,27] == "0"),c(8,9,28,17,18,29,23,24,30,3)]
						colnames(temp.table0) = paste(colnames(temp.table0),"s:0 prd:",maxpredictor[i.prd])
						temp.table = cbind(temp.table0,temp.table)
						}
			conclude.table = temp.table
			first.conclude = FALSE
			}else{
			temp.table = pivot.all[which(pivot.all[,27] == "all"),c(8,9,28,17,18,29,23,24,30,3)]
			colnames(temp.table) = paste(colnames(temp.table),"s:",total.season,"prd:",maxpredictor[i.prd])
			# Put the season 0 into first column each PRD change loop
			if(i.season == 1){
						temp.table0 = pivot.all[which(pivot.all[,27] == "0"),c(8,9,28,17,18,29,23,24,30,3)]
						colnames(temp.table0) = paste(colnames(temp.table0),"s:0 prd:",maxpredictor[i.prd])
						temp.table = cbind(temp.table0,temp.table)
						}
			conclude.table = cbind(conclude.table,temp.table)}

}
}
####################################################################################
########################## *** END great loop to change config here ##############
####################################################################################

setwd(dir2)
write.csv(conclude.table, file="conclude-all season.csv")
setwd(dir0)

setwd(dir2)
write.csv(conclude.table[,seq(2, dim(conclude.table)[2], by=10)], file="NSmodel-all season.csv")
setwd(dir0)

setwd(dir2)
write.csv(conclude.table[,seq(5, dim(conclude.table)[2], by=10)], file="NScalibration-all season.csv")
setwd(dir0)

setwd(dir2)
write.csv(conclude.table[,seq(8, dim(conclude.table)[2], by=10)], file="NSverification-all season.csv")
setwd(dir0)





cat("\n -- END OF THE SCRIPT --\n")



