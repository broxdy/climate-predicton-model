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
#library(forecast)
#library(sowas)
source("wavelet2.R")
#########################################
#### After running this script you need to run 
#### arrange_cor_order.R
#### to arrange the order of correlative predictor
#### ready to run the prediction in ML-multiGCMv4.R
#########################################


##################################
## DATA for Observation parameters
##################################
##datat file name
# first file is the primary file for every calculation`	
#datafile = c("met-ee.csv","met-ss.csv","met-cc.csv","met-ne.csv","met-nn.csv")
datafile = c("monthly-obs_filled1971-2006(4).csv")

############# spatial info of observation #########################
start0.obs=1971  #year start in table file
startyear.obs=1971 #year to analysis begins
startmonth.obs=1
startmonth0.obs=1
endyear.obs=2000 # if 12/2000 have to set to endyear= 2000
endmonth.obs=12  # if 12/2000 have to set to endmonth= 12
unusedcol.obs = 2

startyear=startyear.obs

###############
## I/O option
###############
plot.pdf.seasonal.lag = FALSE
print.cross.wavelet = FALSE



## Season DEFINATION (3 season)
# Season define the number of seson each month (total 12 month)
total.season = 4 # number of season
#define.season = c(1,1,2,2,2,2,3,3,3,3,1,1) # define season each calendar month
# a = annual, s1=dry season [Oct-Dec] in grey dots, s2= pre-monsoon [Jan-Mar] in blue dots, s3= monsoon1 [Apr-Jun] in red dots and s4= monsoon2 [Jul-Sep] in green dots
define.season = c(2,2,2,3,3,3,4,4,4,1,1,1) # define season each calendar month
if(total.season==4){run.season4 = TRUE
}else{run.season4 = FALSE}



# output directory
dir_name="ts-cross-cor-GCMsv4-OBSs"

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","cross-correlation","dummy"))
dir1=dirname(file.path("D:","Temp","cross-correlation",paste(dir_name),"dummy"))


###########################
## DATA for Regional Index
###########################
############# spatial info of index #########################
start0.index=1971  #year start in table file
startyear.index=1971 #year to analysis begins
startmonth.index=1
startmonth0.index=1
endyear.index=2000 # if 12/2000 have to set to endyear= 2000
endmonth.index=12 # if 12/2000 have to set to endmonth= 12

## Define column to define indices which is needed
selectd.col.index = c(3:299)
#selectd.col.index = c(3:60)

dataindex <- read.table("Monthly_GCMs1971-1999(2000)v3.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

#ifinal=min(which(dataindex$year==(endyear.index+1)))-1 + endmonth.index
ifinal=min(which(dataindex$year==endyear.index)) + endmonth.index

datafinal = length(dataindex$year)+1
subty = (startyear.index-start0.index)*12

#******************** remove unwanted column here ##################################
dataindex0 = dataindex[-c(0:subty,ifinal:datafinal),selectd.col.index]

dataindexts =  ts(dataindex0, frequency=12,start=c(startyear.index,startmonth.index))

nparmatindex0 = length(colnames(dataindex0))

cat("\n Finish reading INDICES")



###            ###################################################################################
### Start here ###################################################################################
###            ###################################################################################




# LOOP ################################################################
################ Start INPUT PROCESSING (obs) #########################
#######################################################################

#########Loop for reading of data set

for(loop in 1:length(datafile)){
cat("\n## Start loop",loop,"(",length(datafile),") :",datafile[loop])

data <- read.table(datafile[loop], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

cat("\n Finish reading data file")


#############################################
################ Start INPUT PROCESSING (obs)
#############################################

ifinal.obs=min(which(data$year==endyear.obs)) + endmonth.obs
datafinal.obs = length(data$year)+1

#******************** remove unwanted column here ##################################
subty = (startyear.obs-start0.obs)*12
#P0
data0=data[-c(0:subty,ifinal.obs:datafinal.obs),-c(1:unusedcol.obs)]

datall =  ts(data, frequency=12,start=c(start0.obs,startmonth0.obs))
datats = ts(data0, frequency=12,start=c(startyear.obs,startmonth.obs))




newmat = cbind(data0,dataindex0)
newmat = ts(newmat, frequency=12,start=c(startyear.obs,startmonth.obs))

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
## Cross-Correlation
#####################
#####################
## Print correlation time-series to text
#####################

n.obs = length(colnames(newmat)) - nparmatindex0
n.index = nparmatindex0



runthisloop = print.cross.wavelet
# Print cross wavelet spectrum ***
if(runthisloop){
cat("Start writting cross-wavelet to PDF\n")
	for (i in 1:n.index){
		# Create PDF file for difference lag plot
		setwd(dir1)
		pdf(paste("CrossWavelet-",substr(datafile[loop],1,nchar(datafile[loop])-4),"+",colnames(newmat)[i+n.obs],".pdf"))
		for (j in 1:n.obs){
					if(length(which(is.na(newmat[,j])))==0){
												#n.wsp <- wsp(newmat[,j],s0=2,noctave=3,nvoice=20,nreal=0,labsc=1,plottitle=paste("Index(",i,"):",colnames(newmat)[i+n.obs]," \Met.:",colnames(newmat)[i+n.obs]))
												n.wcs <- wcs(newmat[,i+n.obs],newmat[,j],s0=2,noctave=3,nvoice=30,labsc=1.5,plottitle=paste("\n",colnames(newmat)[i+n.obs],"  -  ",colnames(newmat)[j]))
					}
		}
	cat(paste(colnames(newmat)[j]," :finish writting cross-wavelet to PDF"))
	dev.off()
	}

}
# END Print cross wavelet spectrum ***



#************************************ Auto-Correlation Analysis *********************************

# Annual lag max/min
ccf.lag.max = 3

# Seasonal lag max/min
start.lag = -3
end.lag = 3



printout.thissub = TRUE
#+-################# SUB C Seasonl for printing out .pdf file ####

## To print trendline in PDF files
plot.this.pdf = plot.pdf.seasonal.lag



r2.season = array(NA,c(1,total.season)) # correaltion for seasons
# r2 each season each lag
r2.season.lag = array(NA,c(n.index,n.obs,total.season,abs(end.lag-start.lag)+1))
# r2 each year each lag
r2.year.lag = array(NA,c(n.index,n.obs,abs(end.lag-start.lag)+1))


# For recording entire-year parameters
obsccf.year = array(NA,c(ccf.lag.max*2+1,n.index,n.obs))
nccf.year = array(NA,c(n.index,n.obs))

# For recording seasonal parameters
obsccf.season = array(NA,c(n.index,n.obs,total.season,abs(end.lag-start.lag)+1))




## Loop for find correlation which each index and station
for (i in 1:n.index){

if(plot.this.pdf){
# Create PDF file for difference lag plot
	setwd(dir1)
	pdf(paste("Lag_Plot-",substr(datafile[loop],1,nchar(datafile[loop])-4),"+",colnames(newmat)[i+n.obs],".pdf"))
	par(mfrow=c(3,3))
	}
# END Create PDF file for difference lag plot


			for (j in 1:n.obs){
						
						checkccf = ccf(newmat[,i+n.obs],newmat[,j], type = c("correlation"),lag.max = ccf.lag.max,plot = FALSE, na.action = na.exclude)
						obsccf.year[,i,j] = checkccf$acf
						nccf.year[i,j] =  checkccf$n.used

						
						#### loop through lags START.LAG to END.LAG
						# Plot PDF for difference lag
						for(h in start.lag:end.lag){
							
							# Make lag time (h) at index (i+n.obs)
							eq.trend = cbind(lag(newmat[,i+n.obs],h),newmat[,j])

							# Seperate data into seasons
							season1 = array(NA,c(1,2)) # 1st season / 2 (index+obs)
							season2 = array(NA,c(1,2)) # 2nd season / 2 (index+obs)
							season3 = array(NA,c(1,2)) # 3rd season / 2 (index+obs)
							season4 = array(NA,c(1,2)) # 4th season / 2 (index+obs)

							for(i.month in 1:12){
								if(define.season[i.month] == 1){ season1 = rbind(season1,window(eq.trend,c(startyear,i.month),deltat=1))
												}else{if(define.season[i.month] == 2){ season2 = rbind(season2,window(eq.trend,c(startyear,i.month),deltat=1))
																	}else{if(define.season[i.month] == 3){ season3 = rbind(season3,window(eq.trend,c(startyear,i.month),deltat=1))
																						}else{if(define.season[i.month] == 4){ season4 = rbind(season4,window(eq.trend,c(startyear,i.month),deltat=1))
																											}
																						}
																		}
													}
										}



									## Season analysis

				
									# Trend line
									b = lsfit(eq.trend[,1],eq.trend[,2])

									## Calcualte R2 for entire data
									#r21 = 1-sum(a$residuals^2)/sum((na.exclude(newmat[which(!is.na(lag(newmat[,i+n.obs],h))),j]) - mean(eq.trend[which(!is.na(eq.trend[,1])),2],na.rm=TRUE))^2)
									#r22 = 1-sum(na.exclude(b$residuals)^2)/sum((na.exclude(newmat[which(!is.na(lag(newmat[,i+n.obs],h))),j]) - mean(eq.trend[which(!is.na(eq.trend[,1])),2],na.rm=TRUE))^2)
									eq2 = na.exclude(eq.trend)
									r2 = (cor(b$coefficients[1]+b$coefficients[2]*eq2[,1],eq2[,2]))^2
							
									## Compute R2 and Cross-Correlation each season

									# Season1
									eq.trend.season = cbind(season1[,1],season1[,2])
									if(length(na.exclude(eq.trend.season[,1:2])) > 4) {
										b.season = lsfit(eq.trend.season[,1],eq.trend.season[,2])
										eq2.season = na.exclude(eq.trend.season)
										r2.season[1] = (cor(b.season$coefficients[1]+b.season$coefficients[2]*eq2.season[,1],eq2.season[,2]))^2
	
										checkccf.season = ccf(eq.trend.season[,1],eq.trend.season[,2], type = c("correlation"),lag.max = 0,plot = FALSE, na.action = na.exclude)
										obsccf.season[i,j,1,h-start.lag+1] = checkccf.season$acf
										}else{
											r2.season[1] = NA
											obsccf.season[i,j,1,h-start.lag+1] = NA
											}
									
									# Season2
									eq.trend.season = cbind(season2[,1],season2[,2])
									if(length(na.exclude(eq.trend.season[,1:2])) > 4) {
										b.season = lsfit(eq.trend.season[,1],eq.trend.season[,2])
										eq2.season = na.exclude(eq.trend.season)
										r2.season[2] = (cor(b.season$coefficients[1]+b.season$coefficients[2]*eq2.season[,1],eq2.season[,2]))^2
	
										checkccf.season = ccf(eq.trend.season[,1],eq.trend.season[,2], type = c("correlation"),lag.max = 0,plot = FALSE, na.action = na.exclude)
										obsccf.season[i,j,2,h-start.lag+1] = checkccf.season$acf
										}else{
											r2.season[2] = NA
											obsccf.season[i,j,2,h-start.lag+1] = NA
											}


									# Season3
									eq.trend.season = cbind(season3[,1],season3[,2])
									if(length(na.exclude(eq.trend.season[,1:2])) > 4) {
										b.season = lsfit(eq.trend.season[,1],eq.trend.season[,2])
										eq2.season = na.exclude(eq.trend.season)
										r2.season[3] = (cor(b.season$coefficients[1]+b.season$coefficients[2]*eq2.season[,1],eq2.season[,2]))^2
	
										checkccf.season = ccf(eq.trend.season[,1],eq.trend.season[,2], type = c("correlation"),lag.max = 0,plot = FALSE, na.action = na.exclude)
										obsccf.season[i,j,3,h-start.lag+1] = checkccf.season$acf
										}else{
											r2.season[3] = NA
											obsccf.season[i,j,3,h-start.lag+1] = NA
											}


									# Season4
									if(run.season4){
									eq.trend.season = cbind(season4[,1],season4[,2])
									if(length(na.exclude(eq.trend.season[,1:2])) > 4) {
										b.season = lsfit(eq.trend.season[,1],eq.trend.season[,2])
										eq2.season = na.exclude(eq.trend.season)
										r2.season[4] = (cor(b.season$coefficients[1]+b.season$coefficients[2]*eq2.season[,1],eq2.season[,2]))^2
	
										checkccf.season = ccf(eq.trend.season[,1],eq.trend.season[,2], type = c("correlation"),lag.max = 0,plot = FALSE, na.action = na.exclude)
										obsccf.season[i,j,4,h-start.lag+1] = checkccf.season$acf
										}else{
											r2.season[4] = NA
											obsccf.season[i,j,4,h-start.lag+1] = NA
											}
									}
									# END RUN SEASON4

							

									#** Plot data to pdf file ***
									if(plot.this.pdf){

										#Plot all data set
										plot(lag(newmat[,i+n.obs],h),newmat[,j], cex = 0.2,type = "p",col="white",main=paste(colnames(newmat)[i+n.obs]," (Lag= ",h,")",sep=""), ylab=colnames(newmat)[j],xlab=colnames(newmat)[i+n.obs])

										#Scatter Plot by season
										p.size = 0.5
										p.style = 19
										points(season1[,1],season1[,2], type = "p" ,col="grey" ,cex = p.size, pch = p.style)
										points(season2[,1],season2[,2], type = "p" ,col="blue" ,cex = p.size, pch = p.style)
										points(season3[,1],season3[,2], type = "p" ,col="red" ,cex = p.size, pch = p.style)
										if(run.season4){points(season4[,1],season4[,2], type = "p" ,col="green" ,cex = p.size, pch = p.style)}
		
	
										#mtext(expression(r ^2,"\n\n r"),cex=0.8)
										if(run.season4){mtext(paste("coef. of determination (linear reg.)\na:",round(r2,2),"(s1:",round(r2.season[1],2),"/ s2:",round(r2.season[2],3),"/ s3:",round(r2.season[3],2),"/ s4:",round(r2.season[4],2),")"),cex=0.5)
										}else{mtext(paste("coef. of determination (linear reg.)\na:",round(r2,2),"(s1:",round(r2.season[1],2),"/ s2:",round(r2.season[2],3),"/ s3:",round(r2.season[3],2),")"),cex=0.5)}

										# Plot trend line
										abline(b,col="orange",cex=3)
	
									}
									#** END Plot data to pdf file ***

									## Record r2 each lag-loop	
									r2.year.lag[i,j,h-start.lag+1] = r2
									## Record r2 each season / lag-loop
									for(n.season in 1:total.season){r2.season.lag[i,j,n.season,h-start.lag+1] = r2.season[n.season]}


	
							}


						
						}

			# Close PDF Lag plot
			if(plot.this.pdf){
						dev.off()
						setwd(dir0)
						}
			# END Close PDF Lag plot

			# Show text that it's end of this index
			cat(paste("  Index(",i,"):",colnames(newmat)[i+n.obs]," ..done\n"))
			}


if(printout.thissub){
##Write correlation
cor.at.lag0 = obsccf.year[ccf.lag.max+1,,]
colnames(cor.at.lag0)= colnames(newmat)[1:n.obs]
rownames(cor.at.lag0)= colnames(dataindexts)

setwd(dir1)
sink(paste("corr at lag 0",datafile[loop],".csv"))
write.table(cor.at.lag0, quote = FALSE, sep = ",")
sink()
setwd(dir0)
}
cat("\n Finish ACF analysis")


####
#### Write seasonal correlation
####

run.thissub = TRUE
if(run.thissub){
setwd(dir1)
sink(paste("year-seasonal_corrl-",substr(datafile[loop],1,nchar(datafile[loop])-4),".csv"))
setwd(dir0)


#- Header
cat(datafile[loop],",",n.index,",",n.obs,"\n") 
cat("Index,Obs,")

for(k in (ccf.lag.max*-1):(ccf.lag.max)){cat("Year/Lag",k,",")}
cat("Year.opt.Crlt,Year.opt.at,")

for(n.season in 1:total.season){
					for(k in start.lag:end.lag){cat("S:",n.season,"/Lag",k,",")}
					cat("S:",n.season,"opt.Crlt",",","S:",n.season,"opt.at",",")
}


#- Result data
for(i in 1:n.index){
	for(j in 1:n.obs){
	cat("\n")
	cat(colnames(newmat)[i+n.obs],",",colnames(newmat)[j],",")
	### Print Yearly cor
	for(k in 1:(ccf.lag.max*2+1)){cat(obsccf.year[k,i,j],",")}
	# Value of optimal cor
	cat(obsccf.year[which(max(abs(obsccf.year[,i,j]))==abs(obsccf.year[,i,j])),i,j] ,",")
	# Position for optimal value
	cat(which(max(abs(obsccf.year[,i,j]))==abs(obsccf.year[,i,j])) - ccf.lag.max-1, ",")

	### Print seasonal cor
	for(n.season in 1:total.season){
				for(k in start.lag:end.lag){cat(obsccf.season[i,j,n.season,k-start.lag+1],",")}
				# Value of optimal r2
				cat( obsccf.season[i,j,n.season,which(max(abs(obsccf.season[i,j,n.season,]))==abs(obsccf.season[i,j,n.season,]))] ,",")
				# Position for optimal value
				cat(which(max(abs(obsccf.season[i,j,n.season,]))==abs(obsccf.season[i,j,n.season,])) + start.lag-1, ",")
						}
				}
			}

sink()
}
cat("\n Finish writting seasonal Cross-Correlation")


####
#### Write seasonal R2
####


run.thissub = TRUE
if(run.thissub){
setwd(dir1)
sink(paste("year-seasonal_r2-",substr(datafile[loop],1,nchar(datafile[loop])-4),".csv"))
setwd(dir0)


#- Header
cat(datafile[loop],n.index,",",n.obs,"\n") 
cat("Index,Obs,")

for(k in start.lag:end.lag){cat("Year/Lag",k,",")}
cat("Year.opt.r2,Year.opt.at,")

for(n.season in 1:total.season){
					for(k in start.lag:end.lag){cat("S:",n.season,"/Lag",k,",")}
					cat("S:",n.season,"opt.r2",",","S:",n.season,"opt.at",",")
}

#- Result data
for(i in 1:n.index){
	for(j in 1:n.obs){
	cat("\n")
	cat(colnames(newmat)[i+n.obs],",",colnames(newmat)[j],",")
	### Print Yearly r2
	for(k in start.lag:end.lag){cat(r2.year.lag[i,j,k-start.lag+1],",")}
	# Value of optimal r2
	cat( r2.year.lag[i,j,which(max(abs(r2.year.lag[i,j,]))==abs(r2.year.lag[i,j,]))] ,",")
	# Position for optimal value
	cat(which(max(abs(r2.year.lag[i,j,]))==abs(r2.year.lag[i,j,])) + start.lag-1, ",")


	### Print seasonal r2
	for(n.season in 1:total.season){
				for(k in start.lag:end.lag){cat(r2.season.lag[i,j,n.season,k-start.lag+1],",")}
				# Value of optimal r2
				cat( r2.season.lag[i,j,n.season,which(max(abs(r2.season.lag[i,j,n.season,]))==abs(r2.season.lag[i,j,n.season,]))] ,",")
				# Position for optimal value
				cat(which(max(abs(r2.season.lag[i,j,n.season,]))==abs(r2.season.lag[i,j,n.season,])) + start.lag-1, ",")
						}
				}
			}

sink()
}
cat("\n Finish writting seasonal R2")








#+-################# END SUB C for printing out .pdf file ####

#*********************************** Check Auto-Correlation results *****************************

cat("\n Check correlation analysis : create conclusion table")

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
maxlag=which(obsccf.year[,i,j]==max(obsccf.year[,i,j]))[1]
minlag=which(obsccf.year[,i,j]==min(obsccf.year[,i,j]))[1]
acfa = obsccf.year[,i,j]
acfl = checkccf$lag

# Define max/min value
acfmin[i,j] = obsccf.year[minlag,i,j]
acfmax[i,j] = obsccf.year[maxlag,i,j]

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
cat(datafile[loop],",",n.index,",",n.obs,"\n") 
cat("Station,Index,")
for(k in 1:(ccf.lag.max*2+1)){cat("Lag",checkccf$lag[k,1,1],",")}
cat("CorrMax,Max at,CorrMin,Min at,OptmCorr, Optm at,dataamount.index,dataamount.obs,dataamount.both\n")

#- Result data
for(i in 1:n.index){
for(j in 1:n.obs){
cat(colnames(newmat)[i+n.obs],",",colnames(newmat)[j],",")
for(k in 1:(ccf.lag.max*2+1)){cat(obsccf.year[k,i,j],",")}
cat(acfmax[i,j],",",lagmax[i,j],",",acfmin[i,j],",",lagmin[i,j],",",acfopt[i,j],",",lagopt[i,j],",",yearav[i+n.obs],",",yearav[j],",",nccf.year[i,j],"\n")
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

