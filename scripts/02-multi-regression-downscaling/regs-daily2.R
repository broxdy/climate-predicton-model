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
data <- read.table("Daily-CGCM3_A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 

#******** without nino data ***************
data = data[,-c(51:53)]

#for selection parameters
#data <- read.table("Daten_Rayong1971-2100+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 

attach(data)

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)

station=ObsMnT
station_name="ObsMnT"

#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="regression-A1B-Daily"
#dir_name=station_name

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name) else dirfile=(dir_name)
dir0=dirname(file.path("D:","Temp","dummy"))
dir1=dirname(file.path("D:","Temp",paste(dirfile),"dummy"))

##########################################The stuff above needs to be processed only once.################################



#################### observed data ####################
#months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#begin (-999) and end (999) of series
#istart=which(station==NA)+1 
#if(length(istart)==0) istart=1

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

############# year of observation #########################
start0=1971  #year start in table file
startyear=1971 #year to analysis begins
endyear=2000
endmonth=0

ifinal=min(which(data$year==(endyear+1)))-1 + endmonth
datafinal = length(data$year)
######################################################################################
########################define file for comparison ###################################
######################################################################################
#Column of parameters specific

nmaxpar = 28

lagmax= array(0, c(nmaxpar))
lagmin= array(0, c(nmaxpar))
maxl = array(0, c(nmaxpar))
minl = array(0, c(nmaxpar))
corcoef = array(0, c(nmaxpar))

ninoSST = array(0, c(ifinal,nmaxpar))
nfile = array(0, c(ifinal+1,nmaxpar))
tfile = array("", c(nmaxpar))

######################### auto correlation matrix plot ##################################

######################################################################################
#******************** remove unwanted column here ##################################
######################################################################################
subty = (startyear-start0)*12
#DAI
data2=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,8:25)]
#2D
data3=data[-c(0:subty,(ifinal+1):datafinal),-c(1,2,5:6,26:50)]

#for selection parameters
#DAI
#data2=data[-c(0:subty,ifinal+1),-c(1,2,5:6,8:15)]
#2D
#data3=data[-c(0:subty,ifinal+1),-c(1,2,5:6,16:24)]

nparmat2 = length(colnames(data2))
nparmat3 = length(colnames(data3))


mtseries2=ts(data2,start=c(startyear,1),frequency=12)
mtseries3=ts(data3,start=c(startyear,1),frequency=12)
submt2=substituteNA(mtseries2, type = "mean")
submt3=substituteNA(mtseries3, type = "mean")


#########################################################################################
######################################### multiple linear regression ####################
#########################################################################################

detach(data)
nmaxprd = 8
calerror = array(-99, c(4,5,6))
prderror = array(-99, c(4,5,6))
##
resultc =  array(-99, c(length(data[,1]),6))
opteq	  =  array(-99,c(nmaxprd,6))


for(loop in 1:6){
############################################ START big loop of multi regression $$$$$$$$$$$$$$
setwd(dir1)

################################
##### min temp - DAI  ##########
################################

if(loop==1){
mydata=data2[,-c(2:3)]
pdf(paste("RegressionMx-DAI.pdf"), width= 12, height = 12)
predictant = "ObsMxT"
model = "DAI"
}

#################################
##### max temp - 2D  ############
#################################

if(loop==2){
mydata=data3[,-c(2:3)]
pdf(paste("RegressionMx-2D.pdf"), width= 12, height = 12)
predictant = "ObsMxT"
model = "2D"
}

################################
##### min temp - DAI  ##########
################################

if(loop==3){
mydata=data2[,-c(1,3)]
pdf(paste("RegressionMn-DAI.pdf"), width= 12, height = 12)
predictant = "ObsMnT"
model = "DAI"
}


################################
##### min temp - 2D  ###########
################################

if(loop==4){
mydata=data3[,-c(1,3)]
pdf(paste("RegressionMn-2D.pdf"), width= 12, height = 12)
predictant = "ObsMnT"
model = "2D"
}

################################
##### Rain - DAI  ##############
################################
if(loop==5){
mydata=data2[,-c(1,2)]
pdf(paste("RegressionRn-DAI.pdf"), width= 12, height = 12)
predictant = "ObsRn"
model = "DAI"
}

################################
##### Rain - 2D  ###############
################################
if(loop==6){
mydata=data3[,-c(1,2)]
pdf(paste("RegressionRn-2D.pdf"), width= 12, height = 12)
predictant = "ObsRn"
model = "2D"
}

predictor_eq = mydata[,1]
mydata=mydata[,-c(1)]


####################### ++ title on page ++
par(ps =12,mfrow=c(1,1))
plot.new()
text(0,0.5,adj=c(0,0),lab=paste("Multiple\nLinear\nRegression\n\n\n# CGCM3-",model,"\n# predictant : ",predictant),cex=3)

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
par(ps =12,mfrow=c(3,3))

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=2)
plot(leaps, main=paste("Selection of 2 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=3)
plot(leaps, main=paste("Selection of 3 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=4)
plot(leaps, main=paste("Selection of 4 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=5)
plot(leaps, main=paste("Selection of 5 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=6)
plot(leaps, main=paste("Selection of 6 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=7)
plot(leaps, main=paste("Selection of 7 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=8)
plot(leaps, main=paste("Selection of 8 best predictors for",predictant),scale="r2")

leaps = regsubsets(predictor_eq~.,data=mydata,nvmax=9)
plot(leaps, main=paste("Selection of 9 best predictors for",predictant),scale="r2")

########################################## Set of prediction
#par(ps =12,mfrow=c(2,2))
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=4,method=method[1])
#plot(leaps, main=paste(method[1]," method to optimize 4 set of predictors for",predictant),scale="r2",df=4)
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=2,method=method[2])
#plot(leaps, main=paste(method[2]," method to optimize 4 set of predictors for",predictant),scale="r2")
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=3,method=method[3])
#plot(leaps, main=paste(method[3]," method to optimize 4 set of predictors for",predictant),scale="r2")
#leaps = regsubsets(predictor_eq~.,data=mydata,nbest=4,method=c("seqrep"))
#plot(leaps, main=paste(method[4]," method to optimize 4 set of predictors for",predictant),scale="r2")



#fit <- lm(predictor_eq~.,data=vdata,nvmax=1)
#fita=array(anova(fit),c(length(anova(fit)),4))


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


#***********validation summary result
prd =cbind(vrf[which(predictant==colnames(vrf))],prd)
	
	prderror[i,1,loop] = predictant
	prderror[i,2,loop] = model
	prderror[i,3,loop] = mean(prd[,2]-prd[,1])
	prderror[i,4,loop] = mean(abs(prd[,1]-prd[,2]))
	prderror[i,5,loop] = sqrt(mean((prd[,1]-prd[,2])^2))
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


}
############################################ END big loop of multi regression $$$$$$$$$$$$$$

######################################################################################
######******************** print multi-regression models and validations #############
######################################################################################

#***********print calibration summary result
setwd(dir1)
sink("Concl-Mlt-Rgrssn-prd.csv")
setwd(dir0)
cat("Predictant,GCM,Mothed,MERROR,ABSERROR,RMSE\n") 
for(i in 1:6){
	for(mthd in 1:4){
		cat(prderror[mthd,1,i],",",prderror[mthd,2,i],",",method[mthd],",",prderror[mthd,3,i],",",prderror[mthd,4,i],",",prderror[mthd,5,i],"\n")
			 }
		 }
sink()

#***********print validation summary result
setwd(dir1)
sink("Concl-Mlt-Rgrssn-cal.csv")
setwd(dir0)
cat("Predictant,GCM,Mothed,MERROR,ABSERROR,RMSE\n") 
for(i in 1:6){
	for(mthd in 1:4){
		cat(calerror[mthd,1,i],",",calerror[mthd,2,i],",",method[mthd],",",calerror[mthd,3,i],",",calerror[mthd,4,i],",",calerror[mthd,5,i],"\n")
			 }
		 }
sink()

#####################################
########## print time-series results
for(i in 1:6){
		if(i%%2==1){mydata=data2[,-c(1:3)]}
		if(i%%2==0){mydata=data3[,-c(1:3)]}
		vdata = mydata[,opteq[1:nmaxprd,i]]
		fit <- lm(predictor_eq~.,data=vdata)
		resultc[,i] =  predict(fit,data[-c(length(data[,1])),])
		}
setwd(dir1)
sink("Regression-Result.csv")
setwd(dir0)
cat(calerror[1,1,1],"-",calerror[1,2,1],",",calerror[1,1,2],"-",calerror[1,2,2],",",calerror[1,1,3],"-",calerror[1,2,3],",",calerror[1,1,4],"-",calerror[1,2,4],",",calerror[1,1,5],"-",calerror[1,2,5],",",calerror[1,1,6],"-",calerror[1,2,6])
for(i in 1:length(resultc[,1])){
					for(j in 1:6){cat(resultc[i,j],",")}
					cat("\n")
					}
sink()




#for selection parameters
#DAI
#data2=data[-c(0:subty,ifinal+1),-c(1,2,5:6,8:15)]
#2D
#data3=data[-c(0:subty,ifinal+1),-c(1,2,5:6,16:24)]

##### 2D  ##########
#mydata=data3
#setwd(dir1)
#pdf(paste("Regression-2D.pdf"), width= 12, height= 12)
#par(ps =12,mfrow=c(2,2))
#setwd(dir0)
#dev.off()

###################
#### appendix #####
########################### plot correlative in matrix form ##############################
#for(i in 1:3){
#par(ps =9)
#	cellcol<-matrix(rep("#000000",nparmat*nparmat),nrow=nparmat)
#	cellcol[tccf[i,,]<0]<-color.scale(tccf[i,,][tccf[i,,]<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
#	cellcol[tccf[i,,]>=0]<-color.scale(tccf[i,,][tccf[i,,]>=0],c(1,0.6,0),c(0,0),c(0,0.2,1))
#@ change here too
#color2D.matplot(tccf[i,,],cellcolors=cellcol,nslices=20,axes=FALSE,xlab="climate parameters",ylab="",
#show.legend=FALSE, show.values=2,main=paste("Cross-Correlation of CGCM3-DAI vs", acf2$sname[i] ," at every lag time"))
#	legval<-seq(min(tccf[i,,]),max(tccf[i,,]),length.out=20)
#	legcol<-rep("#000000",20)
#	legcol[legval<0]<-color.scale(legval[legval<0],c(0,0.6,1),c(0,0),c(1,0.2,0))
#	legcol[legval>=0]<-color.scale(legval[legval>=0],c(1,0.6,0),c(0,0),c(0,0.2,1))
#	color.legend(0,-1,3.5,-0.8,round(c(-1,0,1),1),rect.col=legcol)

#par(ps =11)
#axis(1,at=0.5:(45-0.5),labels=-22:22)
#@ change here too
#axis(2,at=(nparmat-0.5):0.5,labels=acf2$sname, side =4)
#}




