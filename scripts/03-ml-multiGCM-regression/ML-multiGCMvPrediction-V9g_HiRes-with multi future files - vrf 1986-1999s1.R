# Great loop to change
# 1)working directory (auto change by config.)
# 2)seasonal file
# 3)maxpredictor
# 4)seasonal month
# run by 2,3,4 seasons


#######
# 1) SSTs
#######
#sst.f = "ocean index-selected.csv"
#sst.f = "ocean index 1971-2009.csv"
sst.f = "ocean index 1971-2009-v2.csv"
add.sst = FALSE
optimal.lag = FALSE # change config order seasonal file (CORRL) to default/optimal-lag
min.lag = 0 # if min.lag = 0, normal optimal lag SSTs ; Non-Zero-Lag(NZL) min.lag > 0
skip.na.col = FALSE # to skip col contains NA data , set to FALSE when optimal-lag SST is occupied
	min.av.in.col = 12 # if skip.na.col == FALSE then screen future indices which minimum available value of min.av.in.col

#######
# 2) Future GCMs
#######
#future.list=c("blank-GCM_high res-1971-1999future.csv")

#future.list=c("GCMv4_real-mo future V5+HiRes_CGCM2-A1_10-2099.csv",
#"GCMv4_real-mo future V5+HiRes_CGCM2-A2_2096.csv",
#"GCMv4_real-mo future V5+HiRes_CGCM2-B1_2099.csv",
#"GCMv4_real-mo future V5+HiRes_CSIRO2-A1_10-2099.csv",
#"GCMv4_real-mo future V5+HiRes_CSIRO2-A2_2096.csv",
#"GCMv4_real-mo future V5+HiRes_CSIRO2-B1_2099.csv",
#"GCMv4_real-mo future V5+HiRes_ECHAM4-A2_2096.csv",
#"GCMv4_real-mo future V5+HiRes_HadCM3-A1_10-2099.csv",
#"GCMv4_real-mo future V5+HiRes_HadCM3-A2_2096.csv",
#"GCMv4_real-mo future V5+HiRes_HadCM3-B1_2099.csv",
#"GCMv4_real-mo future V5+HiRes_PCM-A1_10-2099.csv",
#"GCMv4_real-mo future V5+HiRes_PCM-A2_2096.csv",
#"GCMv4_real-mo future V5+HiRes_PCM-B1_2099.csv"
#)

#future.list=c("GCMv4_real-mo future V5-A1B_2000-10_2099.csv",
#"GCMv4_real-mo future V5-A2_2000-2096.csv",
#"GCMv4_real-mo future V5-B1_2000-2099.csv"
#)

future.list=c("high-res_2000-2100_CSIRO2-A1.csv",
"high-res_2000-2100_CSIRO2-A2.csv",
"high-res_2000-2100_CSIRO2-B1.csv",
"high-res_2000-2100_CSIRO2-B2.csv",
"high-res_2000-2100_ECHAM4-A2.csv",
"high-res_2000-2100_ECHAM4-B2.csv",
"high-res_2000-2100_HadCM3-A1.csv",
"high-res_2000-2100_HadCM3-A2.csv",
"high-res_2000-2100_HadCM3-B1.csv",
"high-res_2000-2100_HadCM3-B2.csv",
"high-res_2000-2100_PCM-A1.csv",
"high-res_2000-2100_PCM-A2.csv",
"high-res_2000-2100_PCM-B1.csv",
"high-res_2000-2100_PCM-B2.csv"
)


#######
# 3)Present GCMs
#######
# Index or GCMs file of calibration & verification file
indexf = "GCMs-high res-1971-1999.csv"
#indexf = "GCMs-real_mo-1971-1999v4.csv"
#indexf = "GCMs-real_mo-1971-1999v5+high res.csv"
#indexf = "blank-GCM_high res-1971-1999.csv"


endyearcal.whole.fit = 1999


#######
# 4)Observed file
#######
dataf = "monthly-obs_filled1971-1999(2100)+WetDay+WetRatio.csv" # for verification 1986-1999
#dataf = "monthly-obs_filled1971-2006(2100)+WetDay+WetRatio.csv" # for verification 2000-2006
#dataf = "test.csv"

#######
# 5) Calibration ratio
#######
# ratio to put into calibration part
#verifyfactor = 0.821 
#verifyfactor = 0.823 # to seperate cal/vrf at 1971-1999/2000-2006 # OK,  1 more in 1999
verifyfactor = 0.52 # to seperate cal/vrf at 1971-1985/1986-1999
#verifyfactor = 0.95


#######
# 6) Order of GCMs 
#######
# Order of GCMs (get from ts-cross-cor-GCMs-OBSs.R and cut some col. out)
#config.nprd.f = "config.csv"
#config.nprd.f = "config all_1.csv"
#config.nprd.f = "config+wetrate_test.csv"
#config.nprd.f = "config_high res.csv"
#config.nprd.f = "config_high res.csv"
config.nprd.f = "config_high res_all_season1.csv"
#config.nprd.f = "config_GCMv5.csv"
#config.nprd.f = "config_GCMv5+high.csv"
#config.nprd.f = "config_GCMv5+high_all_season1.csv"
#config.nprd.f = "config_high res+SSTs 13 prd.csv"
#config.nprd.f = "config_high res+SSTs 10 prd.csv"
#config.nprd.f = "config_high res+SSTs 5 prd.csv"
#config.nprd.f = "config_blank+SSTs 3 season.csv"
#config.nprd.f = "config_blank+SSTs 4 season.csv"


#######
# 7) Correlation of Obs and Indices
#######
# Correlation of Obs and Indices by season
#order.seasonal.file = c("monthly_seasonal-cor_GCM-mo_real-OBS-2season+WetRate_V5.csv","monthly_seasonal-cor_GCM-mo_real-OBS-3season+WetRate_V5.csv","monthly_seasonal-cor_GCM-mo_real-OBS-4season+WetRate_V5.csv") # GCMs
#order.seasonal.file = c("monthly_seasonal-cor_GCM+optimal-lagSSTs-2season.csv","monthly_seasonal-cor_GCM+optimal-lagSSTs-3season.csv","monthly_seasonal-cor_GCM+optimal-lagSSTs-4season.csv") # GCMs+SSTs
order.seasonal.file = c("CORRL_monthly-high res_2season.csv","CORRL_monthly-high res_3season.csv","CORRL_monthly-high res_4season.csv") # HighRes
#order.seasonal.file = c("CORRL_monthly-v5+high res_2season.csv","CORRL_monthly-v5+high res_3season.csv","CORRL_monthly-v5+high res_4season.csv") # with WetDay parameters
#order.seasonal.file = c("CORRL_onthly-v5+high res+defaultSSTs_2 season.csv","CORRL_onthly-v5+high res+defaultSSTs_3 season.csv","CORRL_onthly-v5+high res+defaultSSTs_4 season.csv") # with SST default time-series
#order.seasonal.file = c("CORRL_onthly-v5+high res+optimal-lagSSTs_2 season.csv","CORRL_onthly-v5+high res+optimal-lagSSTs_3 season.csv","CORRL_onthly-v5+high res+optimal-lagSSTs_4 season.csv") # with SST optimal-lag time-series
#order.seasonal.file = c("CORRL_high res+optimal-lagSSTs_2 season.csv","CORRL_high res+optimal-lagSSTs_3 season.csv","CORRL_high res+optimal-lagSSTs_4 season.csv") # with SST optimal-lag time-series
#order.seasonal.file = c("CORRL_all-zero+optimal-lagSSTs_2 season.csv","CORRL_all-zero+optimal-lagSSTs_3 season.csv","CORRL_all-zero+optimal-lagSSTs_4 season.csv") # only SSTs (+ zero value table)
#order.seasonal.file = c("CORRL_all-zero+optimal-lagSSTs_2 season-nzl.csv","CORRL_all-zero+optimal-lagSSTs_3 season-nzl.csv","CORRL_all-zero+optimal-lagSSTs_4 season-nzl.csv") # optimal-lag (no-zero-lag) SSTs


#######
# 8) Directory Names
#######
maindir = "Temp/ML-regression-multiGCM"

# prefix all directory / prefix name for working directory
#v.prefix = "V9g_HiRes_Cal1971-1999_Vrf1986-1999_Sim1971-1999-2099" # change obsdata file name & verifyfactor to change verificaiton period
#v.prefix = "V9g_HiRes_Cal1971-1999_Vrf2000-2006_Sim1971-1999-2099" # change obsdata file name & verifyfactor to change verificaiton period
v.prefix = "V9g_HiRes_Cal1971-1999_Vrf1986-1999_Sim1971-1999-2099s1s" # change obsdata file name & verifyfactor to change verificaiton period




# all results are in this working directory
o_dir_name = paste(v.prefix,"ML-Predict",sep = "")

# Prefix conclusion directory for each GCM // prefix of dir1
o_c_dir_name = paste(v.prefix,"results",sep="")

# conclusion directory for all GCMs // dir3
fdir_name = paste(o_dir_name,"/","conclusion all flist",sep="")


#######################
# Current folder / Working directory and file / ML-regression anlysis + correlation + line-charts of time series


####### Creat Directory ########
dir0=dirname(file.path("D:",paste(maindir),"dummy"))

cat("\nCreating directory for conclusion all flist")
L=file.exists(fdir_name)  
if(!L) fdirfile=dir.create(fdir_name, recursive = TRUE)
fdirfile=(fdir_name)
dir3=dirname(file.path("D:",paste(maindir),paste(fdirfile),"dummy"))



#######################
### 2) Input file
# Observed file
cat("\nReading observation")
data <- read.table(dataf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
obs.n = ncol(data)-2 # for first 2 columns are year and month

### SSTs
data.sst <- read.table(sst.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data.sst0 = data.sst[, -c(1:2)]
sst.n = ncol(data.sst0)

pivot.all.allf = array(NA,c(0,32))
eq.sum.sta = integer(0)



###############
## correlation of obs and SST
###############
max.cor.lag = 6
cat("\nCalculating cross-corelation between Obs-SSTs")
opt.lag = matrix(NA, nrow = obs.n, ncol = sst.n)
obs.all = data[,-c(1:2)]
cat("\nDetermine optimal lag for SST")
min.row.n = min(dim(obs.all)[1],dim(data.sst0)[1])
for (obs.i in 1:obs.n){
cat("\nOptimal lag for:",obs.i,sst.n)
for (sst.i in 1:sst.n){
cbind.ccf = na.omit(cbind(obs.all[1:min.row.n,obs.i],data.sst0[1:min.row.n,sst.i]))
obs.sst.ccf = ccf(cbind.ccf[,1],cbind.ccf[,2], type = c("correlation"),lag.max = max.cor.lag ,plot = FALSE, na.action = na.exclude)
#opt.lag[obs.i,sst.i] = which.max(abs(obs.sst.ccf$acf[(max.cor.lag+1):(max.cor.lag*2+1)]))-1 # for lag 0 to max.cor.lag
opt.lag[obs.i,sst.i] = which.max(abs(obs.sst.ccf$acf[(max.cor.lag+1+min.lag):(max.cor.lag*2+1)]))-1+min.lag# for lag 1 to max.cor.lag
cat("/",obs.sst.ccf$n.used)
}
}








#############################################
## Great loop for multi future-file ML-multiGCM-Prediction_V9-with multi future files.R
#############################################

for(flist in 1:length(future.list)){
cat("\n\n*****************\nLoop",flist,"\n :",future.list[flist],"\n*****************\n")

#indexf.future = "GCMv4_ future_blank table.csv"
#indexf.future = "GCMv4_real-mo future V5-A1B.csv"
#indexf.future = "high-res_2000-2100_CSIRO2-A2.csv"
indexf.future = future.list[flist]
dataindex.future <- read.table(indexf.future, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


#######################
### 1) Directory
cat("\nCreating directory for each GCM")
dir_name1 = paste(o_dir_name,sub(".csv","",indexf.future),sep="/") # GCM results are in this directory
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1, recursive = TRUE)
dirfile=(dir_name1)
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))

#c_dir_name = paste(o_dir_name,sub(".csv","",indexf.future),sep="/") 
#dir2=dirname(file.path("D:",paste(maindir),paste(c_dirfile),"dummy"))

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

###########################
#*********** 3 ************
###########################


config.nprd <- read.table(config.nprd.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

# define max predictor
maxpredictor = config.nprd[,2] # config for maximum number of predictors in extra prediction (extprd)
max.maxpredictor = max(config.nprd[,2],na.rm = TRUE) # maximum number of predictors of all maximum config
#maxpredictor = c(5,10,15,20,25,30,50) # entirely maximum included number of predictors in extra prediction (extprd)
#maxpredictor = c(5) # entirely maximum included number of predictors in extra prediction (extprd)


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

run.till.end.index = TRUE # run through index file, every row till end of file
if(run.till.end.index){
	endyear.obs = dataindex.future$year[dim(dataindex.future)[1]] # last year in table
	endmonth.obs = dataindex.future$month[dim(dataindex.future)[1]] # last month in table
}else{
	endyear.obs=2099 # for GCMs // if 12/2000 have to set to endyear= 2000
	#endyear.obs=2100 # for high resolution // if 12/2000 have to set to endyear= 2000
	#endyear.obs=2089 # if 12/2000 have to set to endyear= 2000
	#endyear.obs=2010 # if 12/2000 have to set to endyear= 2000
	endmonth.obs=12  # if 12/2000 have to set to endmonth= 12
	}
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
endyear.index= endyear.obs # if 12/2000 have to set to endyear= 2000
#endyear.index=2000 # if 12/2000 have to set to endyear= 2000
endmonth.index= endmonth.obs # if 12/2000 have to set to endmonth= 12
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

# 1 season
define.1season = c(1,1,1,1,1,1,1,1,1,1,1,1) # define season for 1)dry and 2)wet for Rayong (specific case)
# 2 season
define.2season = c(1,1,1,1,2,2,2,2,2,2,1,1) # define season for 1)dry and 2)wet for Rayong (specific case)
# 3 season
define.3season = c(1,1,2,2,2,2,3,3,3,3,1,1) # define season for 1)winter, 2)summer and 3)rainy
# 4 season
# a = annual, s1=dry season [Oct-Dec] in grey dots, s2= pre-monsoon [Jan-Mar] in blue dots, s3= monsoon1 [Apr-Jun] in red dots and s4= monsoon2 [Jul-Sep] in green dots
define.4season = c(2,2,2,3,3,3,4,4,4,1,1,1) # define season each calendar month

# array of season
define.season = array(c(define.1season,define.2season,define.3season,define.4season),dim=c(12,4))





#load libraries needed
library(timeSeries)
#library(Kendall)
#library(Rwave)
library(nlme)
library(TSA)
#library(wmtsa)
#library(plotrix)
library(car)
library(MASS)


####################################
####### Directory conclusion########
####################################
# Check conclusion directory
#L=file.exists(c_dir_name)  
#if(!L) c_dirfile=dir.create(c_dir_name)
#c_dirfile=(c_dir_name)








#################################
## DATA for Observation parameters
##################################

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
dataindex0 <- read.table(indexf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

#combine present and future index
dataindex = rbind(dataindex0, dataindex.future)


if(specific.index.col==FALSE){selectd.col.index = c((first.unwantedcol.index+1):dim(dataindex)[2])}

ifinal.index=min(which(dataindex$year==endyear.index)) + endmonth.index
datafinal.index = length(dataindex$year)+1
subty.index = (startyear.index-start0.index)*12

#******************** remove unwanted column here ##################################
#data.index.orgn =dataindex[-c(0:subty.index,(ifinal.index+1):datafinal.index),selectd.col.index] # if it's full length of file
data.index.orgn =dataindex[-c(0:subty.index,(ifinal.index):datafinal.index),selectd.col.index] # if not full length of file

#dataindexts =  ts(dataindex0, frequency=12,start=c(startyear.index,startmonth.index))
npar.index = length(colnames(data.index.orgn))

#*******************************#
cat(paste(" Finish reading INDICES :",npar.index,"\n"))





first.conclude = TRUE
#for(i.prd in 1:length(maxpredictor)){
#for(i.season in 1:length(define.season[1,])){


# Add SSTs
if(add.sst){
cat("\nAdding SST columns")
#data.temp.sst = data.index.orgn[,1:ncol(data.sst0)]
#data.temp.sst = cbind(data.index.orgn[1:nrow(data.sst0),1:2],array(NA,c(nrow(data.sst0),ncol(data.sst0)-2)))
data.temp.sst = as.data.frame(array(NA,c(nrow(data.sst0),ncol(data.sst0)))) # convert array to data.frame
data.temp.sst[,] = NA
data.temp.sst[1:nrow(data.sst0),1:ncol(data.sst0)] = data.sst0
colnames(data.temp.sst) = colnames(data.sst0)

cat("\nCombined SST to index")
#data.index = data.index.orgn
#data.index = cbind(data.index.orgn,data.temp.sst)
data.index = cbind(data.index.orgn[1:nrow(data.sst0),],data.temp.sst)

}else{
	data.index = data.index.orgn
	}
# END add.sst

## Define Obs and Index
cat("\nDefine Obs and Index")

data.obs = data.obs.orgn
data.obs.origin = data.obs

data.index.origin = data.index
#data.index.origin = na.omit(data.index)

n.row = min(nrow(data.obs),nrow(data.index.origin))
data.obs.origin = data.obs.origin[1:n.row,]
data.obs = data.obs[1:n.row,]
obs.month = obs.month[1:n.row]
data.index.origin = data.index.origin[1:n.row,]
data.index = data.index[1:n.row,]



#cat(paste("+++++ \nSeason=",total.season,"(",i.season,"/",length(define.season[1,]),")  ##   Predictor max=",maxpredictor[i.prd],"(",i.prd,"/",length(maxpredictor),")\n+++++\n"))



##########################################The stuff above needs to be processed only once.################################


if(use.allcol == TRUE){
				lcol = length(colnames(data.obs))
				allcolpool = c(1:lcol)
				}else{
					lcol = length(allcolpool)
					}


# Set matrix for prediction results
datafinal = length(data$year)
predict.results = as.data.frame(array(NA,c(datafinal,(lcol*3))))
error.col = integer(0)
record.ignore.colpool = integer(0)


cat("\n Start GLOOP")
##############################################################################################################################
################# great loop to run every column #############################################################################
##############################################################################################################################
for(gloop in 1:lcol){
cat(paste("\nLoop",gloop,":",colnames(data.obs)[gloop]))
cat(paste("\nMax predictor:",maxpredictor[gloop]))
cat(paste("\nTotal season:",config.nprd[gloop,3]))

##
## Adding optimal SSTs
## if optimal.lag = TRUE then put SSTs at optimal lag, if optimal.lag = FALSE then put SSTs default time-series
if(optimal.lag){
	cat("\nOptimal Lag")
	for(sst.i in 1:sst.n){
		data.index[,ncol(data.index)-sst.n+sst.i] = c(rep(NA,opt.lag[gloop,sst.i]),data.sst0[,sst.i])[1:nrow(data.index)]
		data.index.origin[,ncol(data.index.origin)-sst.n+sst.i] = c(rep(NA,opt.lag[gloop,sst.i]),data.sst0[,sst.i])[1:nrow(data.index.origin)]

		colnames(data.index)[ncol(data.index)-sst.n+sst.i] = paste(colnames(data.sst0)[sst.i],"lag",opt.lag[gloop,sst.i],sep="_")
		colnames(data.index.origin)[ncol(data.index.origin)-sst.n+sst.i] = paste(colnames(data.sst0)[sst.i],"lag",opt.lag[gloop,sst.i],sep="_")
		}
} # END if optimal.lag


## Define number of season
i.season = config.nprd[gloop,3]
total.season = config.nprd[gloop,3]

pdtcol = gloop

###################################
## DATA for ordering the predictors
###################################



#total.season = length(unique(define.season[,i.season])) # number of season # Season define the number of seson each month (total 12 month)
if(total.season != 1){dataorder.all <- read.table(order.seasonal.file[total.season-1], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") # order.seasonal.file provides season 2 to season 4
			   }else{dataorder.all <- read.table(order.seasonal.file[1], stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
			         dataorder.all[,7:8] = dataorder.all[,6] # change s1 to yearly column
			         }


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
######################################################################################
######################################################################################
############# loop to run seasonal ###################
######################################################################################


####
# Season 0 // season.i = 1 ,Season 1 // season.i = 2
for(season.i in 1:(total.season+1)){
cat("\n\nSeason: ",season.i-1)

head.col = paste("//Col",gloop,"Season",season.i-1)
error.col = c(error.col,head.col)
record.ignore.colpool = c(record.ignore.colpool,head.col)

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




#########################################################################################
######################################### multiple linear regression ####################
#########################################################################################
runthissub = TRUE
#+-################# SUB3 for Multi--Linear regression ANALYSIS of predictors number / R2 / AIC ####
if(runthissub){






##### Set parameters for Analysis of Prediction / Predictant / Predictors ###############
########################################################################################

###############################################
## LOOP check different number of predictors ##
###############################################
#Define maximum number of predictor
#maxnprd = maxpredictor[i.prd]
maxnprd = maxpredictor[gloop]
rs = array(NA, c(maxnprd, 5))

##**check N/A##
#+-################# SUB3b to change colpool to away N/A column####
if(skip.na.col){
# Change colpool to away N/A column
# check each predictor in colpool is N/A or not, then remove N/A column
cat("\nChange colpool to away N/A column")
origin.colpool = colpool
ignore.colpool = integer(0)
loop.i = 1

repeat{
	check.colpool = integer(0)
	for(col.n in 1:maxnprd){if(length(which(is.na(data.index[,colpool[col.n]])==TRUE))>0){ignore.colpool = c(ignore.colpool,col.n)}}

	if(length(ignore.colpool)>0){	cat("\n !! Skip",loop.i,":", colnames(data.index)[colpool[ignore.colpool]])
						record.ignore.colpool = c(record.ignore.colpool,colnames(data.index)[colpool[ignore.colpool]])
						colpool = colpool[-ignore.colpool]
					    }

	for(col.n in 1:maxnprd){if(length(which(is.na(data.index[,colpool[col.n]])==TRUE))>0){check.colpool = c(check.colpool,col.n)}}
	loop.i = loop.i + 1
if(length(check.colpool)==0){break}
}
#end repeat loop
}else{
	### remove blanked (NA less than min.av.in.col) column in future file
	# Change colpool to away N/A column, which available data less than min.av.in.col
	# check each predictor in colpool is N/A or not, then remove N/A column
	cat("\nChange colpool // remove N/A column which available data less than min.av.in.col in future indices")
	data.future.check = data.index[which(as.real(rownames(data.index))>nrow(dataindex0)),] # future part of data.index
	#origin.colpool = colpool
	ignore.colpool = integer(0)
	loop.i = 1

	repeat{
		check.colpool = integer(0)
		for(col.n in 1:maxnprd){if(length(which(is.na(data.future.check[,colpool[col.n]])==FALSE))<min.av.in.col){ignore.colpool = c(ignore.colpool,col.n)}}

		if(length(ignore.colpool)>0){	cat("\n !! Skip",loop.i,":", colnames(data.future.check)[colpool[ignore.colpool]])
							record.ignore.colpool = c(record.ignore.colpool,colnames(data.future.check)[colpool[ignore.colpool]])
							colpool = colpool[-ignore.colpool]
						    }
		for(col.n in 1:maxnprd){if(length(which(is.na(data.future.check[,colpool[col.n]])==FALSE))<min.av.in.col){check.colpool = c(check.colpool,col.n)}}
		loop.i = loop.i + 1
	if(length(check.colpool)==0){break}
	} #end repeat loop

}## END else{skip.na.col} / end of SUB3b to change colpool to away N/A column####
## END skip.na.col / end of SUB3b to change colpool to away N/A column####



# check the sensitivity on number of predictor
cat("\nCheck sensitivity")
for(n in 2:maxnprd){

#Define predictant colum (refer from data.obs)
predictantcol = pdtcol

##Define number of predictors
datapool=data.index[,colpool[1:n]]

##**check N/A##
# check N/A column then ignore
#cat(n,":Checking N/A in predictors (check max nprd)")
ignore.mydata = integer(0)
if(skip.na.col){
for(col.i in 1:dim(datapool)[2]){
			if(length(which(is.na(datapool[,col.i]) == TRUE))>0){
										    	#cat(colnames(datapool)[col.i]," is ignored** ")
											ignore.mydata = c(ignore.mydata, col.i)
											  }
						 }
}
if(length(ignore.mydata)>0){
				   cat("\n",colnames(datapool)[ignore.mydata]," is ignored** ")
				   datapool = datapool[,-ignore.mydata]
				   }




predictant = colnames(data.obs)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 

##*??*###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]
#cat("\nScreening data")

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
#cat("\nAttach predictant at the end of matrix")
#n.row = min(nrow(datapool),nrow(data.obs))
#pooltemp=cbind(datapool[1:n.row,],data.obs[1:n.row,predictantcol])
pooltemp=cbind(datapool,data.obs[,predictantcol])


#Screen out the NA from predictant and predictors (data pool and predictant)
if(skip.na.col){pooltemp=na.omit(pooltemp)
			}else{
				row.sel = which(!is.na(pooltemp[,ncol(pooltemp)]))
				pooltemp = pooltemp[row.sel,]
				}
			


################################
## loop to check available data
################################
if((dim(pooltemp)[1] > 0)&(dim(pooltemp)[2] > 1)){

#cat("\nCheck available data")
predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

# If need the intercept to be zero
if(is.null(dim(mydata)[2])){
				if(zero.intercept){
							fit <- lm(predictor_eq~mydata-1)
							}else{fit <- lm(predictor_eq~mydata)}
		}else	{
			if(zero.intercept){
						fit <- lm(predictor_eq~.-1,data=mydata)
						}else{fit <- lm(predictor_eq~.,data=mydata)}
			}	


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
sink(paste("Col-",gloop,"Season-",season.i,"Residuals of prediction.csv"))
write.table(rs, quote = FALSE, sep = ",")
sink()
setwd(dir0)

cat("\nColumn: ",gloop,"Finish calculating prediction residuals")


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
cat("\nDefine MAXIMUM number of predictors")
max.by.r2 = max(order(as.numeric(rs[1:maxpredictor[gloop],2]), decreasing = TRUE)[1:n.optimalr2]) # Ranked by R2 and select first top, then select the max number of predictors
max.by.AIC = min(order(as.numeric(rs[1:maxpredictor[gloop],3]), decreasing = FALSE)[1:n.optimalr2]) # Ranked by AIC and select first top, then select the max number of predictors

if(rank.by.r2)	{
			if(rank.by.r2AIC)	{first.nprdmax = min(c(max.by.AIC,max.by.r2))
						}else{first.nprdmax = max.by.r2}
			}else{first.nprdmax = max.by.AIC}

				
## Define all possible predictor number
# all.possible.prd = max(order(as.numeric(rs[,3]), decreasing = FALSE)[1:n.optimalr2])



runthis.plot = TRUE
#+-################# SUB4extra for R2 and AIC chart into PDF file ####
if(runthis.plot){
cat("\nR2 and AIC chart")

if((gloop == 1)&&(season.i==1)){
			setwd(dir1)
			pdf(paste("R2-AIC charts.pdf"), width = 4.5, height = 3)
			setwd(dir0)
			}

par(mpg = c(0.2,0.5,0),mai = c(.8, .8, .2, .9))  # Or change '.9' to a larger number for even 
plot(rs[,2], type = "l", xlab = paste("No. of predictor (",predictant," Season:",(season.i-1),") opt at n=",first.nprdmax), ylab = "R2", lty = 1, lwd = 4)

# vertical line at optimal point
#lines(c(first.nprdmax,first.nprdmax),c(0,1),col="red", lty = 2, lwd = 2)

par(new = TRUE)
plot(rs[,3], type = "l", ann = FALSE, yaxt = "n", col = "blue", lty = 3, lwd = 4)

axis(4)
#legend(x = "bottomleft",  bty = "n",  lty = c(1,2),  lwd = c(2,2), col = c("black", "blue"),  legend = paste(c("R2","AIC"), c("(left  y-axis)", "(right y-axis)")))
#legend(x = "top",  cex = 0.8, bty = "n",  lty = c(1,3,2),  lwd = c(2,2,2), col = c("black", "blue", "red"),  legend = paste(c("R2","AIC","optimal"), c("(left  y-axis)", "(right y-axis)", "(vertical line)")))
legend(x = "topleft",  cex = 0.8, bty = "n",  lty = c(1,3),  lwd = c(2,2), col = c("black", "blue"),  legend = paste(c("R2","AIC")))


mtext("AIC", side = 4, line = 3, col = "blue")

if((gloop == lcol)&&(season.i==(total.season+1)))	{
			dev.off()
			}

}
#+-################# END SUB4extra for R2 and AIC chart into PDF file ####




##Define MAXIMUM number of extended predictors
cat("\nDefine MAXIMUM number of predictors")
extprd = maxpredictor[gloop]


# Change max number of predictors, if data are not enough
# if(all.possible.prd < extprd){extprd=all.possible.prd}


firstloop = TRUE

#############################################################################################

nprd = first.nprdmax

datapool=data.index[,colpool[firstprd:first.nprdmax]]

predictant = colnames(data.obs)[predictantcol]
model = colnames(datapool)


##################screen NA and put set of predictors / predictant ############### 
cat("\nscreen NA and put set of predictors / predictant")
##*??* ###
#if predictant is in the same pool - in my datapool
#datapool=datapool[,-c(predictantcol)]

#Define Length of predictor pool - datapool
lnpool = length(colnames(datapool))

#Attach predictant at the end of matrix
#n.row = min(nrow(datapool),nrow(data.obs))
#pooltemp0=cbind(datapool[1:n.row,],data.obs[1:n.row,predictantcol])
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
#filledpredictant = cbind(predictantdata,matrix(NA, nrow = length(predictantdata), ncol=(extprd+14+13+1)))
filledpredictant = cbind(predictantdata,matrix(NA, nrow = length(predictantdata), ncol=(max.maxpredictor+14+13+1)))
filledpredictant = data.frame(filledpredictant)
filledpredictant[,1] = NA
colnames(filledpredictant)= c("Predictant","Intercept",colnames(data.index[,colpool[firstprd:max.maxpredictor]]),"First Predictor","Last Predictor",
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
cat("\nColumn: ",gloop,"Start to check available (pooltemp)")
## ************************************************* ##
## loop to check available predictor data (pooltemp) ##
#######################################################
if(dim(pooltemp)[1] > 0){


if((first.nprdmax-firstprd) == 0){commonrowmydata = which(is.na(pooltemp0[,1])==FALSE & is.na(pooltemp0[,2])==FALSE)
		}else{commonrowmydata = rownames(pooltemp)}

predictor_eq = pooltemp[,lnpool+1]
mydata = pooltemp[,-c(lnpool+1)]

##**check N/A##
# check N/A column then ignore
cat("\nColumn: ",gloop,"Checking N/A in predictors (cal & vrf)")
ignore.mydata = integer(0)
if(skip.na.col){
for(col.i in 1:dim(mydata)[2]){
			if(length(which(is.na(mydata[,col.i]) == TRUE))>0){ignore.mydata = c(ignore.mydata, col.i)}
					}
}
if(length(ignore.mydata)>0){
				    	#cat(colnames(mydata)[ignore.mydata],"is ignored\n")
					mydata = mydata[,-ignore.mydata]
				   }


#change mydata to numeric
if(length(dim(mydata)) == 1){(mydata = as.numeric(mydata))
}else{for(kk in 1:dim(mydata)[2]){mydata[,kk] = as.numeric(mydata[,kk])}}

###
### Calibration & Verification here below
###

if((first.nprdmax-firstprd) == 0){endcal = as.integer(verifyfactor*length(mydata))
		endvrf = as.integer(length(mydata))
		}else{
		endcal = as.integer(verifyfactor*length(rownames(mydata)))
		endvrf = as.integer(length(rownames(mydata)))
		}
		endwhole = (endyearcal.whole.fit-start0.obs+1)*12

cat("\nColumn: ",gloop,"Start calibration")

#hello()

### Set ML Equation for calibration#
if((first.nprdmax-firstprd) == 0){
				if(zero.intercept){
							fitc <- lm(predictor_eq[1:endcal]~mydata[1:endcal]-1)
							}else{fitc <- lm(predictor_eq[1:endcal]~mydata[1:endcal])}
				}else	{
					if(zero.intercept){
								fitc <- lm(predictor_eq[1:endcal]~.-1,data=mydata[1:endcal,])
								}else{fitc <- lm(predictor_eq[1:endcal]~.,data=mydata[1:endcal,])}
				}

cat("\nColumn: ",gloop,"Start verification")
### Verification
### Predict ML Equation for verification#


if((first.nprdmax-firstprd) == 0)	{
					if(zero.intercept){fitv <- fitc$coefficients[1]*mydata[(endcal+1):endvrf]
								}else{fitv <- fitc$coefficients[1]+fitc$coefficients[2]*mydata[(endcal+1):endvrf]}
					}else	{
						fitv <- predict(fitc,mydata[(endcal+1):endvrf,])
						}
obsv = predictor_eq[(endcal+1):endvrf]

### Set ML Equation #
if((first.nprdmax-firstprd) == 0){

				if(zero.intercept){
							fit <- lm(predictor_eq[1:endwhole]~mydata[1:endwhole]-1)
							}else{fit <- lm(predictor_eq[1:endwhole]~mydata[1:endwhole])}
		}else	{
			if(zero.intercept){
						fit <- lm(predictor_eq[1:endwhole]~.-1,data=mydata[1:endwhole,])
						}else{fit <- lm(predictor_eq[1:endwhole]~.,data=mydata[1:endwhole,])}
			}	


#Define Predictor data set and row's name
predictorcol = colpool[firstprd:first.nprdmax]
predictordata = data.index[,predictorcol]
predictordata = data.frame(predictordata)
rownames(predictordata) = as.numeric(rownames(data.obs))

cat("\nColumn: ",gloop,"Define predictor (predictordata)")
#Define col of predictor which have no NA
#cmp.data.predictor = na.omit(predictortdata)
cmp.data.predictor = na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),])

##**check N/A##
#define n/a data in error.col
er.prd = dim(predictordata[which(is.na(filledpredictant[,1])==TRUE),])[1]>dim(na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),]))[1]
cat("\nCheck Predictor N/A :",er.prd)
if(er.prd){
				for(coln in 1:length(colnames(predictordata))){if(length(which(is.na(predictordata[,coln])==TRUE)) > 0){
																					  cat("\nPredictor (predictordata)",colnames(predictordata)[coln],": is not complete !!")
																					  error.col = c(error.col,colnames(predictordata)[coln])}
																					  }
				}

# check N/A column then ignore
cat("\nColumn: ",gloop,"Checking N/A in predictors (prediction)")
ignore.mydata = integer(0)
if(skip.na.col){
for(col.i in 1:dim(predictordata)[2]){
			if(length(which(is.na(predictordata[,col.i]) == TRUE))>0){ignore.mydata = c(ignore.mydata, col.i)}
						 }
}

if(length(ignore.mydata)>0){
				   cat(colnames(predictordata)[col.i],"is ignored\n")
				   predictordata = predictordata[,-ignore.mydata]
				   }




# define row to fill data in case that there is only one predictor
if(is.null(dim(predictordata))){
temp.mat = cbind(is.na(filledpredictant[,1]),!is.na(predictordata[,1]))
cmp.row.predictor = which((temp.mat[,1] ++ temp.mat[,2])==2)
# in case that predictor > 1
				}else{cmp.row.predictor = rownames(na.omit(predictordata[which(is.na(filledpredictant[,1])==TRUE),]))}

#Define predicror data frame
frameprd = data.frame(cmp.data.predictor)



#change frameprd to numeric
if(length(dim(frameprd)) == 1){(frameprd = as.numeric(frameprd))
}else{for(kk in 1:dim(frameprd)[2]){frameprd[,kk] = as.numeric(frameprd[,kk])}}

cat("\nColumn: ",gloop,"Start prediction")
#### PREDICTION #####
# Filling data by prediction funtion / check null row
if(is.null(cmp.row.predictor)== FALSE){
		if(length(cmp.row.predictor)>= 1){
# for >1 dimension predictor
		if(!is.null(dim(frameprd)))	{
						p = round(predict(fit,na.omit(frameprd)), output.dec)
						if(zero.intercept){p[which(p<0)] = 0}
						if(length(p) >= length(filledpredictant[cmp.row.predictor,1])){filledpredictant[cmp.row.predictor,1] = p
																 }else{for(coln in 1:length(colnames(frameprd))){if(length(which(is.na(frameprd[,coln])==TRUE)) > 0){error.col = c(error.col,colnames(frameprd)[coln])}}
																	filledpredictant[cmp.row.predictor,1] = NA
																	cat("\n predictor is n/a !!\n")
																	}
						}
# for 1 dimension predictor
		if(is.null(dim(frameprd))){
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

		filledpredictant[cmp.row.predictor,(max.maxpredictor+3)] = firstprd
		filledpredictant[cmp.row.predictor,(max.maxpredictor+4)] = first.nprdmax
		filledpredictant[cmp.row.predictor,(max.maxpredictor+5)] = first.nprdmax-firstprd+1
		filledpredictant[cmp.row.predictor,(max.maxpredictor+6)] = length(fit$residuals)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+7)] = length(cmp.row.predictor)

		filledpredictant[cmp.row.predictor,(max.maxpredictor+9)] = mean(fit$residuals)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+10)] = (mean((fit$residuals)^2))^0.5
		filledpredictant[cmp.row.predictor,(max.maxpredictor+11)] = 1-sum((fit$residuals)^2)/sum((predictor_eq-mean(predictor_eq))^2)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+12)] = cor(fit$model[,1],fit$fitted.values)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+13)] = (cor(fit$model[,1],fit$fitted.values))^2
		filledpredictant[cmp.row.predictor,(max.maxpredictor+14)] = AIC(fit)
colt = ""
for(np in firstprd:first.nprdmax){colt = paste(colt,np)}
		filledpredictant[cmp.row.predictor,(max.maxpredictor+15)] = colt
filling = length(predictantdata[which(filledpredictant[,max.maxpredictor+15]==colt),1])-length(removeNA(predictantdata[which(filledpredictant[,max.maxpredictor+15]==colt),1]))
		filledpredictant[cmp.row.predictor,(max.maxpredictor+8)] = filling

## under here for calibration and verification 13 column
		filledpredictant[cmp.row.predictor,(max.maxpredictor+16)] = endcal
		filledpredictant[cmp.row.predictor,(max.maxpredictor+17)] = endvrf-endcal
		filledpredictant[cmp.row.predictor,(max.maxpredictor+18)] = mean(fitc$residuals)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+19)] = (mean((fitc$residuals)^2))^0.5
		filledpredictant[cmp.row.predictor,(max.maxpredictor+20)] = 1-sum((fitc$residuals)^2)/sum((predictor_eq[1:endcal]-mean(predictor_eq[1:endcal]))^2)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+21)] = cor(fitc$model[,1],fitc$fitted.values)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+22)] = (cor(fitc$model[,1],fitc$fitted.values))^2
		filledpredictant[cmp.row.predictor,(max.maxpredictor+23)] = AIC(fitc)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+24)] = mean(fitv-obsv)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+25)] = (mean((fitv-obsv)^2))^0.5
		filledpredictant[cmp.row.predictor,(max.maxpredictor+26)] = 1-sum((fitv-obsv)^2)/sum((predictor_eq[(endcal+1):endvrf]-mean(predictor_eq[(endcal+1):endvrf]))^2)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+27)] = cor(fitv,obsv)
		filledpredictant[cmp.row.predictor,(max.maxpredictor+28)] = (cor(fitv,obsv))^2

		# Put season in the pivot
		filledpredictant[cmp.row.predictor,(max.maxpredictor+29)] = (season.i-1)

		# Define value to pivot table
		if(is.null(cmp.row.predictor)== FALSE){pivot.predictant=rbind(pivot.predictant,filledpredictant[cmp.row.predictor[1],1:(max.maxpredictor+29)])}
		}
	}

cat("\nColumn: ",gloop,"Record calibration results")
# Storage calibration & Verification results
calbvrf = cbind(calbvrf,array(NA,c(nrow(calbvrf),2)))
colnames(calbvrf)[(length(colnames(calbvrf))-1):length(colnames(calbvrf))] = c(paste(colt,"cal/vrf"),colt)
rownames(calbvrf) = as.numeric(rownames(data.obs))

if(length(commonrowmydata) == 1){calbvrf[commonrowmydata,length(colnames(calbvrf))] =  fitv
				calbvrf[commonrowmydata,length(colnames(calbvrf))-1] = 1
	}else{
	calbvrf[commonrowmydata,length(colnames(calbvrf))] = c(fitc$fitted.values,fitv)
	calbvrf[commonrowmydata,length(colnames(calbvrf))-1] = c(array(0,c(endcal)),array(1,c(endvrf-endcal)))
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
# write Calibration results (Cal/Vrf)
write.csv(calbvrf, file =paste("Col-",gloop,"Season-",season.i,"Cal-Vrf-Obs",predictant,".csv"))
# write prediction [date,obs,prediction]
write.csv(cbind(dataindex[1:dim(filledpredictant)[1],1:2],data.obs[,gloop],filledpredictant[,1]), file =paste("Col-",gloop,"Season-",season.i,"Prediction",predictant,".csv"))
setwd(dir0)

# Combine cal & vrf each season
if((season.i-1) <= 1)	{combine.calvrf = calbvrf
				}else{combine.calvrf = rbind(combine.calvrf,calbvrf)}

cat("\nColumn: ",gloop,"Finish writing seasonal calibration & verification")
}
#+-################# END SUB5 for Run writing Cal-Vrf results ####



#+-##### write the combined calibration & verification series ####
if((season.i-1) == total.season){
calvrf.element = combine.calvrf[order(as.numeric(rownames(combine.calvrf))),]
setwd(dir1)
write.csv(calvrf.element,file=paste("Col-",gloop,"combined cal+vrf.csv"))
setwd(dir0)
cat("\nColumn: ",gloop,"Finish writing combined calibration & verification series")

# combine all prd cal & vrf at every station
colnames(calvrf.element) = paste(colnames(data.obs)[gloop],colnames(calvrf.element))
if(gloop == 1){all.prd.calvrf = cbind(data[as.numeric(rownames(data.obs.origin)),1:2],calvrf.element) # insert year and month at first column
		  }else{all.prd.calvrf = cbind(all.prd.calvrf, calvrf.element[,c(1,4)])}

# write all prd cal & vrf at the end of calculation / last station
if(gloop == lcol){
#setwd(dir2)
setwd(dir1)
write.csv(all.prd.calvrf,file=paste("Calibration+Verification",sub(".csv","",indexf.future),".csv"), row.names=FALSE)
setwd(dir0)
setwd(dir3)
write.csv(all.prd.calvrf[,c(1:2,(1:lcol)*2+4)],file=paste("Calibration+Verification",sub(".csv","",indexf.future),".csv"), row.names=FALSE)
setwd(dir0)
cat(paste("\nColumn: ",gloop,"Finish writing combined all prd calibration & verification"))
}

}




# write column's prediction information // equation and regression models
runthissub = TRUE
if(runthissub){
setwd(dir1)
write.csv(pivot.predictant, file=paste("Col-",gloop,"Season-",season.i," regression performance",predictant,".csv"))
setwd(dir0)
}

# Record equation
cat("\nRecord regression equation")
eq.term = rep(NA,max.maxpredictor)
eq.term[1:maxpredictor[gloop]] = colnames(pivot.predictant[3:(maxpredictor[gloop]+2)])
eq.sta = c(indexf.future,obs.name[gloop],unlist(pivot.predictant[2,-1]),eq.term)
if(length(eq.sum.sta) == 0){
					eq.sum.sta = eq.sta
					}else{eq.sum.sta = rbind(eq.sum.sta,eq.sta)}


# attach prediction information into prediction information table
if((gloop == 1)&&(season.i == 1))	{
			pivot.all = pivot.predictant[2,-c(1:(max.maxpredictor+2))]
			
			}else{pivot.all = rbind(pivot.all,pivot.predictant[2,-c(1:(max.maxpredictor+2))])}
rownames(pivot.all)[dim(pivot.all)[1]] = paste(predictant,"-",(season.i-1))



# Display
cat("\nColumn: ",gloop,"Finish writing results")


## Put the results in conclude predict result for all predictants



datacheck1 = data.obs[,gloop]
#predict.results[which(is.na(datacheck1)==FALSE),(gloop*3)-1] = datacheck1[which(is.na(datacheck1)==FALSE)]
predict.results[which(is.na(datacheck1)==FALSE),(gloop*3)-2] = datacheck1[which(is.na(datacheck1)==FALSE)]
predict.results[which(is.na(datacheck1)==FALSE),(gloop*3)] = 1

datacheck2 = filledpredictant[,1]
datacheck3 = filledpredictant[,(max.maxpredictor+11)]

# row only the specificd season
sel.row = as.numeric(rownames(filledpredictant))

# define the predictant results
#predict.results[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE),(gloop*2)-1] = datacheck2[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)]
#predict.results[sel.row[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)],(gloop*2)-1] = datacheck2[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)]
predict.results[sel.row[which(is.na(datacheck2)==FALSE)],(gloop*3)-1] = datacheck2[which(is.na(datacheck2)==FALSE)]
# define the NS coefficient results
predict.results[sel.row[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)], gloop*3] = datacheck3[which(is.na(datacheck1)==TRUE & is.na(datacheck2)==FALSE)]

# Display calculated column each loop
cat("\nColumn: ",gloop,"Finish recording results")

# Define column's name on the prediction result
colnames(predict.results)[(gloop*3)-2] = paste(predictant,"-Obs")
colnames(predict.results)[(gloop*3)-1] = predictant
colnames(predict.results)[(gloop*3)] = paste(predictant,"-Cal/NsPrdict")


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

predict.results[,(gloop*3)-2] = data[,gloop+unusedcol.obs]

}
####
################# END great loop to run every column ###################
####



# put date column in first 2 columns
predict.results = cbind(dataindex[1:dim(predict.results)[1],1:2],predict.results)
# write the prediction results of all predictant
#setwd(dir2)
setwd(dir1)
write.csv(predict.results, file = "Sim_Predictant-results.csv", row.names=FALSE)
setwd(dir0)
setwd(dir3)
write.csv(predict.results[,c(1:2,(1:lcol)*3+1)], file = paste("Sim_Predictant-results_only predicted",sub(".csv","",indexf.future),".csv"), row.names=FALSE)
setwd(dir0)
cat("+Prediction results written\n")

# write the prediction information of all predictant
setwd(dir1)
write.csv(pivot.all, file="regression performance.csv")
setwd(dir0)
cat("+General pivot written\n")

#setwd(dir2)
setwd(dir1)
## calculation of average NS
n.station = length(which(pivot.all[,27] == "all"))
pivot.ns = array(NA,c(n.station,3)) # c(conclude,cal,vrf),total season,stations)
ns.col = c(9,18,24) # column with NS values
# Gather all season NS
for(ii in 1:length(ns.col)){
k1 = 2 # first NS row
for(jj in 1:n.station){
k2 = k1+config.nprd[jj,3]-1
pivot.ns[jj,ii] = mean(pivot.all[k1:k2,ns.col[ii]],na.rm = TRUE)
#cat(paste(rownames(pivot.all)[k1:k2],"\n"))
k1 = sum(config.nprd[1:(jj),3])+(jj)*2 + 2# k1 for next n.station
}
}
## End calculation of average NS
cat("+Average NS calculated\n")
#write.csv(pivot.all[which(pivot.all[,27] == "all"),c(8,9,17,18,23,24,3)], file="conclude-ALL_season-season.csv")
#write.csv(pivot.all[which(pivot.all[,27] == 0),c(8,9,17,18,23,24,3)], file="conclude-NO__season-season.csv")
cat("+Conclusion files written\n")

setwd(dir0)

#- build average NS at the end of columns
ns.col = c(9,18,24)
ns.add.col = array(NA, c(dim(pivot.all)[1],length(ns.col)))
colnames(ns.add.col) = c("avrg-stNS","avrg-calNS","avrg-vrfNS")

#- calculate average NS

for(ii in 1:length(ns.col)){
for(jj in 1:n.station){
#cat(paste(rownames(pivot.all)[sum(config.nprd[0:(jj-1),3])+(jj-1)*2 + 2+config.nprd[jj,3]],"\n"))
ns.add.col[sum(config.nprd[0:(jj-1),3])+(jj-1)*2 + 2+config.nprd[jj,3],ii] = pivot.ns[jj,ii]
}
}
pivot.all = cbind(pivot.all,ns.add.col)

# build a conclusion for all season
if(first.conclude){
			temp.table = pivot.all[which(pivot.all[,27] == "all"),c(8,9,28,17,18,29,23,24,30,3)]
			colnames(temp.table) = paste(colnames(temp.table),"s:",total.season,"prd:",maxpredictor[gloop])
			# Put the season 0 into first column each PRD change loop
			if(i.season == 1){
						temp.table0 = pivot.all[which(pivot.all[,27] == "0"),c(8,9,28,17,18,29,23,24,30,3)]
						colnames(temp.table0) = paste(colnames(temp.table0),"s:0 prd:",maxpredictor[gloop])
						temp.table = cbind(temp.table0,temp.table)
						}
			conclude.table = temp.table
			first.conclude = FALSE
			}else{
			temp.table = pivot.all[which(pivot.all[,27] == "all"),c(8,9,28,17,18,29,23,24,30,3)]
			colnames(temp.table) = paste(colnames(temp.table),"s:",total.season,"prd:",maxpredictor[gloop])
			# Put the season 0 into first column each PRD change loop
			if(i.season == 1){
						temp.table0 = pivot.all[which(pivot.all[,27] == "0"),c(8,9,28,17,18,29,23,24,30,3)]
						colnames(temp.table0) = paste(colnames(temp.table0),"s:0 prd:",maxpredictor[gloop])
						temp.table = cbind(temp.table0,temp.table)
						}
			conclude.table = cbind(conclude.table,temp.table)}




#setwd(dir2)
setwd(dir1)
#write.csv(conclude.table, file="conclude-all season.csv")
write.csv(error.col, file="error predictor column.csv")
write.csv(unique(error.col), file="error predictor column-unique.csv")
write.csv(record.ignore.colpool, file="ignored predictor column.csv")
write.csv(unique(record.ignore.colpool), file="ignored predictor column-unique.csv")
setwd(dir0)
cat("+Conclusion table written\n")



pivot.all.f = cbind(sub(".csv","",indexf.future),pivot.all)
pivot.all.allf = rbind(pivot.all.allf,pivot.all.f)

cat("\n****\n**** END flist :",flist," - ",indexf.future,"\n\n")
}
## END Great loop for multi future-file ML-multiGCM-Prediction_V9-with multi future files.R
## END flist
#}
#}
####################################################################################
########################## *** END great loop to change future file here ##############
####################################################################################



cat("\nWriting conclusion regression terms")
colnames(eq.sum.sta)[c(1:2,4:(3+max.maxpredictor),(31+max.maxpredictor):(30+max.maxpredictor*2))] = c("GCM","station",paste("pr",1:max.maxpredictor,sep="-"),paste("pr",1:max.maxpredictor,sep="-"))
setwd(dir3)
write.csv(eq.sum.sta, file="conclusion regression terms.csv",row.names=FALSE)
setwd(dir0)

cat("\nWriting conclusion all flist")
colnames(pivot.all.allf)[1] = "GCMfuture"
setwd(dir3)
write.csv(pivot.all.allf, file="conclusion all flist prediction.csv")
setwd(dir0)


cat("\n -- END OF THE SCRIPT --\n")

