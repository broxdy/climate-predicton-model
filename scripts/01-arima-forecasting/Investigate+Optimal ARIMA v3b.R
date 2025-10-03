
#Test program to carry out various time series analyses
#
#load libraries needed
library(timeSeries)
library(Kendall)
#library(Rwave)
library(TSA)
library(wmtsa)
library(plotrix)
library(car)
library(DAAG)
library(forecast)
#source("clear.R")
#clear()


#--Rayong-- Change station name
cat("\nStart AutoRegressive Model")

p2 = "GCM"
p3 = "Ocean Index"
obsn = 32+13
prd.type = c(rep(c(1,2),4),rep(3,24),rep(4,13)) # fit to ord, to use optimal order which is put in "ord"(Tmax, Tmin , Rain, SST)


########################define file for input / read ################################
## obs
cat("\nReading Obs")
#data <- read.table("Daten_Rayong1971-2100+Index2+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
#data <- read.table("monthly-obs_filled1971-2006(2100).csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data <- read.table("monthly-obs_filled1971-2006(2100)+SST.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")



#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="Investigate+Optimal ARIMA all sta vrf v3b-12mn"


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","arima","dummy"))
dir1=dirname(file.path("D:","Temp","arima",paste(dir_name),"dummy"))

##########################################The stuff above needs to be processed only once.################################



#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

############# year of observation #########################
start0.c=1971  #year start in table file
startyear.c=1971 #year to analysis begins
endyear.c=1999
endmonth.c=0

ifinal.c=min(which(data$year==(endyear.c+1)))-1 + endmonth.c
datafinal.c = length(data$year)

######################################################################################
########################define file for comparison ###################################
######################################################################################
#Column of parameters specific


######################################################################################
#******************** remove unwanted column here ##################################
######################################################################################
#### column to remove
drmv1 = c(1,2) # Data1 Obs
####

subty.c = (startyear.c-start0.c)*12
#P1 Obs
data01 = data[-c(0:subty.c,(ifinal.c+1):datafinal.c),-drmv1]


######******************** data for validation here ##################################
start0.v = 1971  #year start in table file
startyear.v = 2000 #year to analysis begins
endyear.v = 2005
endmonth.v = 3
ifinal.v = min(which(data$year==(endyear.v+1)))-1 + endmonth.v
datafinal.v = length(data$year)

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

########################################
##### Investigate sensitive ARIMA(p,d,q)
########################################

cat("\nInvestigate ARIMA(p,d,q)")
prd.type = c(rep(c(1,2),4),rep(3,24),rep(4,13)) # fit to ord, to use optimal order which is put in "ord"(Tmax, Tmin , Rain, SST)
nprd = 12 # verify months
obs.n = 35
pn = 12
dn = 1
qn = 5


#Prediction charts
setwd(dir1)
#pdf("InvestigateChart-ARIMApdq.pdf", width = 11, height = 5)
pdf("InvestigateChart-ARIMApdq.pdf", width = 5, height = 11)
nrow.plot = 12
ncol.plot = 5
pt.text = 10
par(ps = 10,mfrow = c(nrow.plot,ncol.plot))
par(ps =pt.text,mgp=c(0.5,0.5,0),mai = c(0.3, 0.3, .2, .1))
setwd(dir0)
leg.txt <- c("prediction", "observation")


#for(i in c(1:2,10)){
for(i in c(1:obs.n)){
			prd.i = prd.type[i]
			par(ps = pt.text,mfrow = c(1,1))
			plot.new()
			text(0,0.5,adj=c(0,0),lab=colnames(data1)[i],cex=3)
			par(ps = pt.text,mfrow = c(nrow.plot,ncol.plot))

			for(d in 0:dn){
			for(p in 0:pn){
			for(q in 0:qn){
						datmat = data1
						nona = which(is.na(datmat[,i])== FALSE )
						startdat = nona[1]
						dat = ts(datmat[nona,],frequency = 12, start= tsp(datmat)[1]+(startdat-1)/12)
						ts1 = dat[,i]
						fit = try(arima(ts1,order = c(p,d,q)))
						fitp = try(forecast(fit,nprd))
						#Plot charts
						#t = try(plot(fitp,1,main = paste("ARIMA(",p,",",d,",",q,")",sep="")))
						#t = try(plot(fitp,1,main = paste(colnames(data1)[i],"\nARIMA(",p,",",d,",",q,")","  AIC=",format(as.numeric(fit$aic),digits=3,scientific=FALSE),sep="")))
						#t = try(plot(fitp,1,main = paste("ARIMA(",p,",",d,",",q,")","  AIC=",format(as.numeric(fit$aic),digits=3,scientific=FALSE),sep="")))
						if(length(fitp) > 1){resid = fitp$mean-vdat1[,i]}
						t = try(plot(fitp,1,main = paste("ARIMA(",p,",",d,",",q,")","  NS=",format(as.numeric(1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)),digits=3,scientific=FALSE),sep="")))

						if(length(t) > 1){
							resid = fitp$mean-vdat1[,i]
							#title(sub = paste(colnames(data1)[i],p,d,q,"/ NS(verification)=",format(as.numeric(1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)),digits=3,scientific=FALSE)))
							#title(sub = paste(colnames(data1)[i],"AIC",format(as.numeric(fit$aic),digits=3,scientific=FALSE)))
							lines(vdat1[,i],col = "Green",lty = "dashed")
							legend(2001,min(fitp$mean[1:24],vdat1[1:24,i])+0.5,legend = leg.txt, col=c("blue","green"), lty=c("solid","dashed"), merge=FALSE, bty = "n")
							cat(paste("\nInvestigate ARIMA#",p,d,q,colnames(data1)[i]," NS=",format(as.numeric(1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)),digits=3,scientific=FALSE))) # count the loop print out the running progress
							}else{
								title(sub = paste(colnames(data1)[i],p,d,q,"- error !"))
								cat("\nInvestigate ARIMA#",p,d,q,colnames(data1)[i],"- error !")
								}

			}
			}
			}
		 }
dev.off()


###########################
##### select optimal ARIMA
###########################
cat("\nExamine optimal ARMA(p,d,q)")

#pn = 5
#dn = 5
#qn = 5
#nprd = 12

fit.t.results = array(NA, c(obsn*(pn+1)*(dn+1)*(qn+1),11))
optimal.pdq = array(NA, c(obsn,11))

colnames(fit.t.results) = c("Sta","p","d","q","AIC","sigma2","rmsq","NS","LogLik","V-rmsq","V-NS")
colnames(optimal.pdq) = c("Sta","p","d","q","AIC","sigma2","rmsq","NS","LogLik","V-rmsq","V-NS")

count.i = 0
aic.i = 999999
for(obs.i in 1:obsn){
	cat("\nFinding optimal ARMA(p,d,q) at obs:",obs.i,colnames(data1)[obs.i])
	sel.row = which(!is.na(data1[,obs.i]))
	sum.diff.sqr.obs = sum((data1[sel.row,obs.i]-mean(data1[sel.row,obs.i]))^2)
	m.vdat = mean(vdat1[1:nprd,i])

	for(p in 0:pn){
	for(d in 0:dn){
	for(q in 0:qn){
		count.i = count.i +1
		fit.t.results[count.i,1] = colnames(data1)[obs.i]
		fit.t.results[count.i,2] = p
		fit.t.results[count.i,3] = d
		fit.t.results[count.i,4] = q

		ts1 = data1[sel.row,obs.i]
		fit.t = try(arima(ts1,order = c(p,d,q)))
		if(length(fit.t)>1){
			fit.t.results[count.i,5] = fit.t$aic
			fit.t.results[count.i,6] = fit.t$sigma2
			fit.t.results[count.i,7] = mean((fit.t$residuals)^2)^.5
			fit.t.results[count.i,8] = 1-sum(fit.t$residuals^2)/sum.diff.sqr.obs	
			fit.t.results[count.i,9] = fit.t$loglik

			fitp = forecast(fit.t,nprd)
			resid = fitp$mean-vdat1[1:nprd,i]
			fit.t.results[count.i,10] = sqrt(mean((resid)^2))
			fit.t.results[count.i,11] = 1-sum((resid)^2)/sum((vdat1[1:nprd,i]-m.vdat)^2)
			cat("\n",p,d,q,"NS",fit.t.results[count.i,11])

	
			# if lowest aic.i
			if(fit.t.results[count.i,5] < aic.i){
								best.i = count.i
								aic.i = fit.t.results[count.i,5]
								}
			}else{cat("\nerror:",p,d,q)}

		}
	}
	}

	optimal.pdq[obs.i,] = fit.t.results[best.i,]
	aic.i = 999999
}

setwd(dir1)
write.csv(fit.t.results, file = "ARMA pdq 1971-2000.csv", row.names = FALSE)
write.csv(optimal.pdq, file = "ARMA pdq optimal 1971-2000.csv", row.names = FALSE)
setwd(dir0)
