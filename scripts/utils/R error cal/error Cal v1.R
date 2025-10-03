#######
# 1)Observed file
#######
monthly.type = TRUE # if not Monthly then Daily
check.lars.wg = FALSE # if the results from LARS-WG model, if FALSE then SDSM // replace PCP name with TMP from TMP at PCP table
check.daily.mlr = FALSE # if the results from Daily MLR (Multi linear regression)

cat("\nDefining OBS")
if(monthly.type){
#dataf = "monthly-obs_filled1971-2006(2100)+WetDay+WetRatio.csv" # for monthly
dataf = "monthly-obs_filled1971-2006(2100)+WetDay+WetRatio+SSTs.csv" # for monthly
}else{
	dataf = "Daily Filled all climate 1971-2006+Wet_V2.csv" # for daily
	}

run.till.end.index = FALSE # run through index file, every row till end of file

monthly.time.obs = monthly.type # if false then daily
start0.obs=1971  #year start in table file
startyear.obs=1971 #year to analysis begins
startmonth.obs=1
startmonth0.obs=1
if(monthly.time.obs){
			endyear.obs = 2005
			endmonth.obs = 12
			}else{
				endyear.obs = 2005
				endmonth.obs = 12
				endday.obs = 365
				}

cat("\nReading observation")
data.obs.all <- read.table(dataf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

				

#######
# 2) File list
#######
cat("\nf.list")
use.all.in.dir = TRUE    # if TRUE, use all file in defined directory (dir1),  FALSE, use f.list defination
if(!use.all.in.dir){
f.list=c("test.csv")
}
if(monthly.type){
year.month.day.col.obs = c(1,2,2)
year.month.day.col.prd = c(2,1,1)
}else{
year.month.day.col.obs = c(1,2,3)
year.month.day.col.prd = c(1,2,3)
}

############# spatial info of downscaling data #########################
start0.index=1971  #year start in table file
startyear.index=1971 #year to analysis begins
startmonth.index=1
startmonth0.index=1
endyear.index= endyear.obs # if 12/2000 have to set to endyear= 2000
endmonth.index= endmonth.obs # if 12/2000 have to set to endmonth= 12


# if specific.prd.col = TRUE , then define the column , if FALSE then you all column accept first.unwanted col.index
specific.prd.col = FALSE
if(specific.prd.col){prd.col.sel = c(1:1000)}



#######
# 3) Evaluation period and calibration
#######
## Define number of season to evaluate (season.set.i)
cat("\nDefine period and calibration")
total.season = c(3,4)
total.season.n = length(total.season)

## ratio to put into calibration part

if(monthly.type){
# For monthly data
#verifyfactor1 = 0.52 # to seperate cal/vrf at 1971-1985/1986-1999
verifyfactor1 = 12*15/348 # to seperate cal/vrf at 1971-1985/1986-1999
verifyfactor2 = 0.823 # to seperate cal/vrf at 1971-1999/2000-2006 # OK,  1 more in 1999
endvrf1.date = 348 # for monthly data upto 1999
endcal2.date = 348 # for monthly data upto 1999
}else{
# For daily data
verifyfactor1 = 0.50 # to seperate cal/vrf at 1971-1985/1986-2000 - Daily downscaling
verifyfactor2 = 10958/12874 # to seperate cal/vrf at 1971-2000/2001-2006  #
endvrf1.date = 10958 # for daily data upto 2000
endcal2.date = 10958 # for daily data upto 2000
}





#######
# 4) Directory Names
#######
cat("\nDefining DIR")
maindir = "temp/downscaling results"

# dir where source files place (results from downscaling)
file.dir = "temp/downscaling results/input_monthlyMLR"
#file.dir = "temp/downscaling results/input_MLRdailyLARS-WG"
#file.dir = "temp/downscaling results/input_MLRdailySDSM"
#file.dir = "temp/downscaling results/input_dailyMLR"
#file.dir = "temp/downscaling results/Input_AR"
#file.dir = "temp/downscaling results/test2"
#file.dir = "temp/downscaling results/test"
#file.dir = "temp/downscaling results/input_test"



# prefix all directory / prefix name for working directory
v.prefix = "results" # 

fname = gsub(paste(maindir,"/",sep=""),"",file.dir)
# all results are in this working directory
fdir_name = paste(v.prefix,fname,sep="_")


#######
# 5) check compatability of downscaling results
#######
if(monthly.type){
obs.day.freq = 12 # for monthly obs
gcm.day.freq = 12 # for monthly data, Define frequency of downscaling results
}else{
obs.day.freq = 366 # for daily obs
if(check.lars.wg){
gcm.day.freq = 365 # for daily LARS-WG, Define frequency of downscaling results
}else{
gcm.day.freq = 360 # for daily SDSM, Define frequency of downscaling results
}
}


if(monthly.type){
convert.time.scale = FALSE # (for Monthly) if the result from Downscaling have different lenght from normal calendar
first.unwantedcol.obs = 2 # for Monthly obs data
first.unwantedcol.index = 2 # for Monthly obs data
}else{
	first.unwantedcol.obs = 4 # for daily obs data
	if(check.daily.mlr){
		convert.time.scale = FALSE
		first.unwantedcol.index = 3 # for daily MLR
	}else{
		convert.time.scale = TRUE # (for SDSM/LARS-WG results) if the result from Downscaling have different lenght from normal calendar
		if(check.lars.wg){
			first.unwantedcol.index = 2 # for daily LARS-WG
		}else{
			first.unwantedcol.index = 3 # for daily SDSM
			}
		}
}





###################################
####### a) Creat Directory
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
dir1=dirname(file.path("D:",paste(file.dir),"dummy"))

cat("\nCreating directory for conclusion all flist")
L=file.exists(fdir_name)  
if(!L) fdirfile=dir.create(fdir_name, recursive = TRUE)
fdirfile=(fdir_name)
dir2=dirname(file.path("D:",paste(maindir),paste(fdirfile),"dummy"))



#######################
### Input file

######################
## DATA for Observation


if(run.till.end.index){
	endyear.obs = data.prd.all[dim(data.prd.all)[1],year.month.day.col.obs[1]] # last year in table
	endmonth.obs = data.prd.all[dim(data.prd.all)[1],year.month.day.col.obs[2]] # last month in table
}else{
	endyear.obs=endyear.obs # for GCMs // if 12/2000 have to set to endyear= 2000
	endmonth.obs=endmonth.obs # if 12/2000 have to set to endmonth= 12
	}

### remove unwanted column here ###
if(monthly.time.obs == FALSE){
	# For dialy time-series
		ifinal.obs = min(which(data.obs.all[,year.month.day.col.obs[1]]>=(endyear.obs)))-1 + endday.obs
		datafinal.obs = length(data.obs.all[,year.month.day.col.obs[1]])
		subty.obs = (startyear.obs-start0.obs)*365
		data.obs0=data.obs.all[-c(0:subty.obs,ifinal.obs:datafinal.obs),-c(1:first.unwantedcol.obs)]
	}else{
	# For monthly time-series
		ifinal.obs=min(which(data.obs.all[,year.month.day.col.obs[1]]==endyear.obs)) + endmonth.obs
		datafinal.obs = length(data.obs.all[,year.month.day.col.obs[1]])+1
		subty.obs = (startyear.obs-start0.obs)*12
		data.obs0=data.obs.all[-c(0:subty.obs,ifinal.obs:datafinal.obs),-c(1:first.unwantedcol.obs)]
		}

# Define time-series of observation (monhtly)
ts.obs = ts(data.obs0, frequency=obs.day.freq,start=c(startyear.obs,startmonth.obs))
if(monthly.time.obs){
	obs.month = round((time(ts.obs)%%1)*12+1)
	}else{
	obs.month = data.obs.all[-c(0:subty.obs,ifinal.obs:datafinal.obs),year.month.day.col.obs[2]]
	}
npar.obs = length(colnames(data.obs0))



###################
## Data flist for prd.file (downscaling results list)
if(use.all.in.dir){
setwd(dir1)
cat("\nReading DIR:",dir1)
all.list = list.files(recursive = TRUE)
setwd(dir0)
f.find = grep(".csv",all.list)
f.list1 = all.list[f.find]
if(length(f.find)>0){dir.list1 = all.list[-f.find]
			  }else{dir.list1 = all.list}
f.list = f.list1
cat("- found :",length(f.list),"file(s)")
}


runthisloop = FALSE
if(runthisloop){
repeat{
	f.list2 = integer(0)
	dir.list2 = integer(0)

	for(dir.i in 1:length(dir.list1)){
		cat("\nReading DIR:",dir.list1[dir.i])
		dir.exp = dirname(file.path("D:",paste(file.dir),dir.list1[dir.i],"dummy"))
		dir.list2.1 = integer(0)

		setwd(dir.exp)
		all.list2 = list.files()
		setwd(dir0)

		f.find2 = grep(".csv",all.list2)
		f.list2 = all.list[f.find2]
		if(length(f.find2)>0){dir.list2.1 = c(dir.list2,all.list2[-f.find2])
					    dir.list3 = all.list2[-f.find2]
					  }else{
						dir.list2.1 = c(dir.list2,all.list2)
					  	dir.list3 = all.list2
						}
		dir.list2 = c(dir.list2,paste(dir.list1[dir.i],dir.list2.1,sep="/"))

	} # END for(dir.i
	
	dir.list1 = dir.list2
	if(length(dir.list3) < 1){break()}

	} # END repeat

}



#######################
### Define season from 1 to 4 less/more
## Season DEFINATION (1-4 season)#####
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
season.print.all = integer(0)
couple.prd.obs = as.data.frame(array(NA, c(1,2)))
colnames(couple.prd.obs) = c("prd","obs")

f.update = integer(0) # update all and last file


data.obs.all.org = data.obs.all
data.obs0.org = data.obs0
obs.month.org = obs.month



##################################
## Great loop for multi-input file
##################################
for(flist in 1:length(f.list)){
cat("\n\n*****************\nLoop",flist,"/",length(f.list),"\n :",f.list[flist],"\n*****************\n")

## Downsclaing file
indexf.file = f.list[flist]
setwd(dir1)
data.prd.all <- read.table(indexf.file, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
setwd(dir0)

cat("\nOrg. Header: ",colnames(data.prd.all))

## Check header for time control
if(monthly.type){
year.month.day.col.prd = c(which(colnames(data.prd.all) == "year"),which(colnames(data.prd.all) == "month"),0)
}

# Modified the data year
if(data.prd.all[1,year.month.day.col.prd[1]] == 1){data.prd.all[,year.month.day.col.prd[1]] = data.prd.all[,year.month.day.col.prd[1]]+start0.obs-1}


## Check Obs Year, then predictor starts at different year
if(data.prd.all[1,year.month.day.col.prd[1]] != data.obs.all[1,year.month.day.col.obs[1]]){
	cat("\n*Start OBS and PRD at different year")
	start.obs.new = min(which(data.obs.all[,year.month.day.col.obs[1]] == data.prd.all[1,year.month.day.col.prd[1]]))
	data.obs.all.new = data.obs.all[start.obs.new:dim(data.obs.all)[1],]
	data.obs0 = data.obs.all.new[,-c(1:first.unwantedcol.obs)]
	ts.obs = ts(data.obs0, frequency=obs.day.freq,start=c(data.obs.all.new[1,year.month.day.col.obs[1]],data.obs.all.new[1,year.month.day.col.obs[2]]))
	if(monthly.time.obs){
		obs.month = round((time(ts.obs)%%1)*12+1)
		}else{
		obs.month.new = data.obs.all.new[,year.month.day.col.obs[2]]
		}
}else{
	data.obs.all.new = data.obs.all.org
	data.obs0 = data.obs0.org
	obs.month.new = obs.month.org
	}



## Check LARS-WG data
if(check.lars.wg){
#Check TMP at PCP station
setwd(dir0)
tmp.at.pcp <- read.table("TMP at PCP.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

tmx.f = length(grep("tmx",indexf.file)) # check is max temp
tmn.f = length(grep("tmn",indexf.file)) # check is min temp
tmp.col.name = colnames(data.prd.all)
if(length(tmx.f) > 0){
			for(col.i in 3:ncol(data.prd.all)){
				if(tmn.f > 0){tmp.col.name[col.i] = tmp.at.pcp[which(tmp.at.pcp[,1] ==colnames(data.prd.all)[col.i]),2]} #  min temp
				if(tmx.f > 0){tmp.col.name[col.i] = tmp.at.pcp[which(tmp.at.pcp[,1] ==colnames(data.prd.all)[col.i]),3]} #  max temp
				}
			}
colnames(data.prd.all) = tmp.col.name
}
## END Check LARS-WG data



# Define col of selected prd
if(specific.prd.col){
			allcolpool = c(1:26)
			lcol = length(allcolpool)
			}else{
				lcol = length(colnames(data.prd.all))-first.unwantedcol.index
				allcolpool = c(1:lcol)
				}

cat(paste("\n Finish reading data OBS :",npar.obs,"\n"))
if(specific.prd.col==FALSE){selectd.col.prd = c((first.unwantedcol.index+1):dim(data.prd.all)[2])}
#ifinal.index=min(which(data.prd.all[,year.month.day.col.prd[1]]==endyear.index)) + endmonth.index
#datafinal.index = length(data.prd.all$year)+1
subty.index = (startyear.index-start0.index)*12
# remove unwanted column here
#data.prd0 =data.prd.all[-c(0:subty.index,(ifinal.index):datafinal.index),selectd.col.prd] # if not full length of file
if(subty.index > 0){data.prd0 =data.prd.all[-c(0:subty.index),selectd.col.prd] # if not full length of file
			 }else{data.prd0 =data.prd.all[,selectd.col.prd] # if not full length of file
				}
npar.index = length(colnames(data.prd0))
cat(paste(" Finish reading INDICES :",npar.index,"\n"))



## Define Obs and Index
cat("\nDefine Obs and Index")

data.obs.origin = data.obs0

n.row = min(nrow(data.obs0),nrow(data.prd0))
data.obs = data.obs0[1:n.row,]
obs.month.final = obs.month.new[1:n.row] # if normal same time scale
data.prd = data.prd0[1:n.row,]

obs.name = colnames(data.obs)
prd.name = colnames(data.prd)

year.res = as.data.frame(array(NA, c(lcol,1+6+6*4)))
season.res = array(NA, c(lcol,1+6+6*4,max(total.season),total.season.n))





######
# Convert time scale to fit prediction
if(convert.time.scale){
data.days <- read.table("day in year.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

data.newset = data.prd

n.year.obs = length(unique(data.obs.all.new[-c(0:subty.obs,ifinal.obs:datafinal.obs),year.month.day.col.obs[1]][1:n.row]))
obs.freq = data.days[which(data.days[,1] == startyear.obs)[1]:which(data.days[,1] == endyear.obs),2] # number of day each year

## creat list of day
# Creat day list for obs -- year will be yddd (1360: year=1, day = 360)
n.day.list.obs = integer(0)
for(year.i in 1:n.year.obs){n.day.list.obs = c(n.day.list.obs,year.i*1000+1:obs.freq[year.i])}
calnd.day.list.obs = integer(0)
for(year.i in 1:n.year.obs){calnd.day.list.obs = c(calnd.day.list.obs,1:obs.freq[year.i])}

# Creat day list for gcm based on obs.freq
n.day.list.gcm = integer(0)
for(year.i in 1:n.year.obs){
	gcm.date.factor = obs.freq[year.i]/gcm.day.freq
	n.day.list.gcm = c(n.day.list.gcm,year.i*1000+ c(1:gcm.day.freq)*gcm.date.factor)
}

## Creat new  date set for prediction below this part
cat("\nCreat new date set")
data.prd.new.time = as.data.frame(array(NA,c(length(n.day.list.obs),1)))
for(gcm.i in 1:lcol){
	f <- approxfun(n.day.list.gcm,data.prd[,gcm.i],yleft = data.prd[1,gcm.i])
	data.prd.new.time = cbind(data.prd.new.time,f(n.day.list.obs))
	colnames(data.prd.new.time)[gcm.i+1] = colnames(data.prd)[gcm.i]
	}

n.row = min(nrow(data.obs0),nrow(data.prd.new.time))
data.obs = data.obs0[1:n.row,]
obs.month.final = obs.month.new[1:n.row] # if normal different time scale
data.prd = data.prd.new.time[1:n.row,-1]
}

## END Convert time scale to fit prediction
#######







cat("\n ** Start prd.i")
####################################
#### great loop to run every column 
for(prd.i in 1:lcol){
cat(paste("\n#File:",flist,"# PRD",prd.i,":",f.list[flist],":",colnames(data.prd)[prd.i]))


# Find fitting column
#cat("\n**Fitting columnames")
obs.col = which(obs.name == prd.name[prd.i])
if(length(obs.col)<1){
	#cat("\n**Fitting columnames extra")
	#for(prd.i in 1:lcol){
	prd.sel = integer(0)
	for(obs.i in 1:length(obs.name)){
			prd.rep = grep(obs.name[obs.i],prd.name[prd.i])
			if(length(prd.rep) >= 1){prd.sel=c(prd.sel,obs.i)}
			}
			if(length(prd.sel)>1)	{
							max.er = 1
							repeat{prd.rep2 = agrep(prd.name[prd.i],obs.name[prd.sel],max = max.er)
								max.er = max.er+1
								if(length(prd.rep2)>0){break}
								}
							obs.col = min(prd.sel[prd.rep2])
							}else{obs.col = prd.sel}
			cat(" -> obs. :",colnames(data.obs)[obs.col])
			#cat("\n**")
			#}# END prd.i
}

# Record matching column name
#cat("\nRecord  matching column name")
if(length(which(is.na(couple.prd.obs))) > 1){couple.prd.obs = c(paste(f.list[flist],colnames(data.prd)[prd.i],sep="-"),colnames(data.obs)[obs.col])
					}else{couple.prd.obs = rbind(couple.prd.obs,c(paste(f.list[flist],colnames(data.prd)[prd.i],sep="-"),colnames(data.obs)[obs.col]))}

# check obs. col is available
if(length(obs.col)<1){
			  	cat("!! NO obs.col available")
				couple.prd.obs[nrow(couple.prd.obs),2] = "NA"
				next
				}

# Define prd and obs
obs.list = as.real(data.obs[,obs.col])
prd.list = as.real(data.prd[,prd.i])


#############
## Cal & Vrf
#cat("\nSeparation: ",prd.i,"calibration+verification")
startcal1 = min(which(!is.na(prd.list)))
n.all = length(which(!is.na(prd.list)))

# if OBS data not complete (SLR)
if(min(which(!is.na(obs.list)))>1){
	startcal1 = min(which(!is.na(obs.list)))
	endvrf1 = endvrf1.date
	endcal1 = as.integer(verifyfactor1*(endvrf1-startcal1+1)) + startcal1 - 1 # like ratio forCal 1971-1985, Vrf 1986-1999
	endcal2 = endcal2.date
}else{
	# if data NOT complete, missing data is too much like SETIO, then separate by verifyfactor
	if(startcal1 > 12*8){
				endvrf1 = endvrf1.date
				endcal1 = as.integer(verifyfactor1*length(na.omit(prd.list[1:endvrf1]))) + startcal1 - 1 # like ratio forCal 1971-1985, Vrf 1986-1999
	
				endcal2 = endcal2.date
	}else{
		# if data complete or, NOT complete but less than 10 years
		endcal1 = endvrf1.date*verifyfactor1
		endvrf1 = endvrf1.date

		endcal2 = endcal2.date
		}
	} # END if/else if(min(which(!is.na(obs.list)))>1)

endvrf2 = length(prd.list)



#endcal1 = as.integer(verifyfactor1*length(na.omit(prd.list))) + startcal1 - 1 # Cal 1971-1985, Vrf 1986-1999
#endcal1 = as.integer(verifyfactor1*length(na.omit(prd.list))) # Cal 1971-1985, Vrf 1986-1999
#endcal2 = 348 # Cal 1971-1985, Vrf 1986-1999
#endvrf1 = 348
#endvrf2 = length(prd.list)


# Data for cal & vrf
obs.list.cal1 = obs.list[1:endcal1]
obs.list.cal2 = obs.list[1:endcal2]
prd.list.cal1 = prd.list[1:endcal1]
prd.list.cal2 = prd.list[1:endcal2]

obs.list.vrf1 = obs.list[(endcal1+1):endvrf1]
obs.list.vrf2 = obs.list[(endcal2+1):endvrf2]
prd.list.vrf1 = prd.list[(endcal1+1):endvrf1]
prd.list.vrf2 = prd.list[(endcal2+1):endvrf2]



##
## Define evaluation values in results
year.res[prd.i,1] = prd.name[prd.i]

# Genral fit all
year.res[prd.i,2] = nrow(na.omit(cbind(obs.list,prd.list)))
year.res[prd.i,3] = mean(prd.list-obs.list,na.rm=TRUE)
year.res[prd.i,4] = (mean((prd.list-obs.list)^2,na.rm=TRUE))^0.5
year.res[prd.i,5] = 1-sum((prd.list-obs.list)^2,na.rm=TRUE)/sum((obs.list - mean( obs.list[which(!is.na(prd.list))], na.rm = TRUE))^2, na.rm = TRUE)
year.res[prd.i,6] = cor(na.omit(cbind(obs.list,prd.list))[,1],na.omit(cbind(obs.list,prd.list))[,2])
year.res[prd.i,7] = as.real(year.res[prd.i,6])^2

# Cal1
year.res[prd.i,8] = nrow(na.omit(cbind(obs.list.cal1,prd.list.cal1)))
year.res[prd.i,9] = mean(prd.list.cal1-obs.list.cal1,na.rm=TRUE)
year.res[prd.i,10] = (mean((prd.list.cal1-obs.list.cal1)^2,na.rm=TRUE))^0.5
year.res[prd.i,11] = 1-sum((prd.list.cal1-obs.list.cal1)^2,na.rm=TRUE)/sum((obs.list.cal1 - mean( obs.list.cal1[which(!is.na(prd.list.cal1))], na.rm = TRUE))^2, na.rm = TRUE)
year.res[prd.i,12] = cor(na.omit(cbind(obs.list.cal1,prd.list.cal1))[,1],na.omit(cbind(obs.list.cal1,prd.list.cal1))[,2])
year.res[prd.i,13] = as.real(year.res[prd.i,12])^2

# Vrf1
year.res[prd.i,14] = nrow(na.omit(cbind(obs.list.vrf1,prd.list.vrf1)))
year.res[prd.i,15] = mean(prd.list.vrf1-obs.list.vrf1,na.rm=TRUE)
year.res[prd.i,16] = (mean((prd.list.vrf1-obs.list.vrf1)^2,na.rm=TRUE))^0.5
year.res[prd.i,17] = 1-sum((prd.list.vrf1-obs.list.vrf1)^2,na.rm=TRUE)/sum((obs.list.vrf1 - mean( obs.list.vrf1[which(!is.na(prd.list.vrf1))], na.rm = TRUE))^2, na.rm = TRUE)
year.res[prd.i,18] = cor(na.omit(cbind(obs.list.vrf1,prd.list.vrf1))[,1],na.omit(cbind(obs.list.vrf1,prd.list.vrf1))[,2])
year.res[prd.i,19] = as.real(year.res[prd.i,18])^2

# Cal2
year.res[prd.i,20] = nrow(na.omit(cbind(obs.list.cal2,prd.list.cal2)))
year.res[prd.i,21] = mean(prd.list.cal2-obs.list.cal2,na.rm=TRUE)
year.res[prd.i,22] = (mean((prd.list.cal2-obs.list.cal2)^2,na.rm=TRUE))^0.5
year.res[prd.i,23] = 1-sum((prd.list.cal2-obs.list.cal2)^2,na.rm=TRUE)/sum((obs.list.cal2 - mean( obs.list.cal2[which(!is.na(prd.list.cal2))], na.rm = TRUE))^2, na.rm = TRUE)
year.res[prd.i,24] = cor(na.omit(cbind(obs.list.cal2,prd.list.cal2))[,1],na.omit(cbind(obs.list.cal2,prd.list.cal2))[,2])
year.res[prd.i,25] = as.real(year.res[prd.i,24])^2

# Vrf2
year.res[prd.i,26] = nrow(na.omit(cbind(obs.list.vrf2,prd.list.vrf2)))
year.res[prd.i,27] = mean(prd.list.vrf2-obs.list.vrf2,na.rm=TRUE)
year.res[prd.i,28] = (mean((prd.list.vrf2-obs.list.vrf2)^2,na.rm=TRUE))^0.5
year.res[prd.i,29] = 1-sum((prd.list.vrf2-obs.list.vrf2)^2,na.rm=TRUE)/sum((obs.list.vrf2 - mean( obs.list.vrf2[which(!is.na(prd.list.vrf2))], na.rm = TRUE))^2, na.rm = TRUE)
year.res[prd.i,30] = cor(na.omit(cbind(obs.list.vrf2,prd.list.vrf2))[,1],na.omit(cbind(obs.list.vrf2,prd.list.vrf2))[,2])
year.res[prd.i,31] = as.real(year.res[prd.i,30])^2






############# loop to run all season.set ###################
for(season.set.i in 1:total.season.n){
#cat("\n#Season.set",season.set.i,"(",total.season[season.set.i],"season)")
# define order both yearly and seasonal for all observation [year/season,index,obs] -- year/sean: 1=yearly, 2=s1 , 3=s2 , 4=s3 , 5=s4
# Season 0 // season.i = 1 ,Season 1 // season.i = 2


############# loop to run seasonal ###################
for(season.i in 1:(total.season[season.set.i])){
#cat("\n Season: ",season.i,"/",total.season[season.set.i])
#if(season.i == 2){stop()}

# define colpool for each season
sel.month = which(define.season[,total.season[season.set.i]] == season.i)
sel.row = integer(0)
for(n in 1:length(sel.month))	{sel.row = c(sel.row,which(obs.month.final == sel.month[n]))}
sel.row = sel.row[order(sel.row)]

prd.list.ss = prd.list[sel.row]
obs.list.ss = obs.list[sel.row]

obs.list.cal1.ss = obs.list.cal1[sel.row]
obs.list.vrf1.ss = obs.list.vrf1[sel.row[which(sel.row > length(obs.list.cal1))]-length(obs.list.cal1)]
obs.list.cal2.ss = obs.list.cal2[sel.row]
obs.list.vrf2.ss = obs.list.vrf2[sel.row[which(sel.row > length(obs.list.cal2))]-length(obs.list.cal2)]

prd.list.cal1.ss = prd.list.cal1[sel.row]
prd.list.vrf1.ss = prd.list.vrf1[sel.row[which(sel.row > length(prd.list.cal1))]-length(prd.list.cal1)]
prd.list.cal2.ss = prd.list.cal2[sel.row]
prd.list.vrf2.ss = prd.list.vrf2[sel.row[which(sel.row > length(prd.list.cal2))]-length(prd.list.cal2)]

#cat(" obs : Cal1",length(which(!is.na(obs.list.cal1.ss))),"Vrf1",length(which(!is.na(obs.list.vrf1.ss)))," -- Cal2",length(which(!is.na(obs.list.cal2.ss))),"Vrf2",length(which(!is.na(obs.list.vrf2.ss))))
#cat(" prd : Cal1",length(which(!is.na(prd.list.cal1.ss))),"Vrf1",length(which(!is.na(prd.list.vrf1.ss)))," -- Cal2",length(which(!is.na(prd.list.cal2.ss))),"Vrf2",length(which(!is.na(prd.list.vrf2.ss))))


## Define evaluation values in seasonal results
# All.ss
season.res[prd.i,1,season.i,season.set.i] = prd.name[prd.i]
season.res[prd.i,2,season.i,season.set.i] = nrow(na.omit(cbind(obs.list.ss,prd.list.ss)))
season.res[prd.i,3,season.i,season.set.i] = mean(prd.list.ss-obs.list.ss,na.rm=TRUE)
season.res[prd.i,4,season.i,season.set.i] = (mean((prd.list.ss-obs.list.ss)^2, na.rm=TRUE))^0.5
season.res[prd.i,5,season.i,season.set.i] = 1-sum((prd.list.ss-obs.list.ss)^2, na.rm=TRUE)/sum((obs.list.ss - mean( obs.list.ss[which(!is.na(prd.list.ss))], na.rm = TRUE))^2, na.rm = TRUE)
season.res[prd.i,6,season.i,season.set.i] = cor(na.omit(cbind(obs.list.ss,prd.list.ss))[,1],na.omit(cbind(obs.list.ss,prd.list.ss))[,2])
season.res[prd.i,7,season.i,season.set.i] = as.real(season.res[prd.i,6,season.i,season.set.i])^2

# Cal1.ss
season.res[prd.i,8,season.i,season.set.i] = nrow(na.omit(cbind(obs.list.cal1.ss,prd.list.cal1.ss)))
season.res[prd.i,9,season.i,season.set.i] = mean(prd.list.cal1.ss-obs.list.cal1.ss,na.rm=TRUE)
season.res[prd.i,10,season.i,season.set.i] = (mean((prd.list.cal1.ss-obs.list.cal1.ss)^2,na.rm=TRUE))^0.5
season.res[prd.i,11,season.i,season.set.i] = 1-sum((prd.list.cal1.ss-obs.list.cal1.ss)^2,na.rm=TRUE)/sum((obs.list.cal1.ss - mean( obs.list.cal1.ss[which(!is.na(prd.list.cal1.ss))], na.rm = TRUE))^2, na.rm = TRUE)
season.res[prd.i,12,season.i,season.set.i] = cor(na.omit(cbind(obs.list.cal1.ss,prd.list.cal1.ss))[,1],na.omit(cbind(obs.list.cal1.ss,prd.list.cal1.ss))[,2])
season.res[prd.i,13,season.i,season.set.i] = as.real(season.res[prd.i,12,season.i,season.set.i])^2

# Vrf1.ss
season.res[prd.i,14,season.i,season.set.i] = nrow(na.omit(cbind(obs.list.vrf1.ss,prd.list.vrf1.ss)))
season.res[prd.i,15,season.i,season.set.i] = mean(prd.list.vrf1.ss-obs.list.vrf1.ss,na.rm=TRUE)
season.res[prd.i,16,season.i,season.set.i] = (mean((prd.list.vrf1.ss-obs.list.vrf1.ss)^2,na.rm=TRUE))^0.5
season.res[prd.i,17,season.i,season.set.i] = 1-sum((prd.list.vrf1.ss-obs.list.vrf1.ss)^2,na.rm=TRUE)/sum((obs.list.vrf1.ss - mean( obs.list.vrf1.ss[which(!is.na(prd.list.vrf1.ss))], na.rm = TRUE))^2, na.rm = TRUE)
season.res[prd.i,18,season.i,season.set.i] = cor(na.omit(cbind(obs.list.vrf1.ss,prd.list.vrf1.ss))[,1],na.omit(cbind(obs.list.vrf1.ss,prd.list.vrf1.ss))[,2])
season.res[prd.i,19,season.i,season.set.i] = as.real(season.res[prd.i,18,season.i,season.set.i])^2

# Cal2.ss
season.res[prd.i,20,season.i,season.set.i] = nrow(na.omit(cbind(obs.list.cal2.ss,prd.list.cal2.ss)))
season.res[prd.i,21,season.i,season.set.i] = mean(prd.list.cal2.ss-obs.list.cal2.ss,na.rm=TRUE)
season.res[prd.i,22,season.i,season.set.i] = (mean((prd.list.cal2.ss-obs.list.cal2.ss)^2,na.rm=TRUE))^0.5
season.res[prd.i,23,season.i,season.set.i] = 1-sum((prd.list.cal2.ss-obs.list.cal2.ss)^2,na.rm=TRUE)/sum((obs.list.cal2.ss - mean( obs.list.cal2.ss[which(!is.na(prd.list.cal2.ss))], na.rm = TRUE))^2, na.rm = TRUE)
season.res[prd.i,24,season.i,season.set.i] = cor(na.omit(cbind(obs.list.cal2.ss,prd.list.cal2.ss))[,1],na.omit(cbind(obs.list.cal2.ss,prd.list.cal2.ss))[,2])
season.res[prd.i,25,season.i,season.set.i] = as.real(season.res[prd.i,24,season.i,season.set.i])^2

# Vrf2.ss
season.res[prd.i,26,season.i,season.set.i] = nrow(na.omit(cbind(obs.list.vrf2.ss,prd.list.vrf2.ss)))
season.res[prd.i,27,season.i,season.set.i] = mean(prd.list.vrf2.ss-obs.list.vrf2.ss,na.rm=TRUE)
season.res[prd.i,28,season.i,season.set.i] = (mean((prd.list.vrf2.ss-obs.list.vrf2.ss)^2,na.rm=TRUE))^0.5
season.res[prd.i,29,season.i,season.set.i] = 1-sum((prd.list.vrf2.ss-obs.list.vrf2.ss)^2,na.rm=TRUE)/sum((obs.list.vrf2.ss - mean( obs.list.vrf2.ss[which(!is.na(prd.list.vrf2.ss))], na.rm = TRUE))^2, na.rm = TRUE)
season.res[prd.i,30,season.i,season.set.i] = cor(na.omit(cbind(obs.list.vrf2.ss,prd.list.vrf2.ss))[,1],na.omit(cbind(obs.list.vrf2.ss,prd.list.vrf2.ss))[,2])
season.res[prd.i,31,season.i,season.set.i] = as.real(season.res[prd.i,30,season.i,season.set.i])^2




} # END season.i

} # END season.set.i



#if(prd.i == 13){hallo()}


#cat("\nEND prd.i:",prd.i)
} ## END prd.i(lcol)
######################

cat("\nFnl. Header :",colnames(data.prd))

####################################
# Define season results for printing
season.print = as.data.frame(array(NA,c(lcol*sum(total.season,1),3+6+6*4)))

# Define colname name
cat("\nDefine colname name")
prefix.cal = c("All","Cal1","Vrf1","Cal2","Vrf2")
colnames(season.print)[1:3] = c("Station","Prediction","Period")
for(i in 1:length(prefix.cal)){
colnames(season.print)[((i-1)*6+4):(i*6+3)]  = paste(prefix.cal[i],c("n","avrg error","RMSQ error","NS coef","cor","R2"),sep="_")
}

# Transfer data from season.res to season.print
season.print[1] = season.res[,1,1,1] # station name
n = 1
season.print[((n-1)*lcol+1):((n)*lcol),2] = paste(fname,f.list[flist],sep="/")
season.print[((n-1)*lcol+1):((n)*lcol),3] = "whole"
season.print[((n-1)*lcol+1):((n)*lcol),c(1,4:33)] = year.res[,]

for(i in 1:total.season.n){
for(j in 1:total.season[i]){
n = n+1
season.print[((n-1)*lcol+1):((n)*lcol),2] = paste(fname,f.list[flist],sep="/")
season.print[((n-1)*lcol+1):((n)*lcol),3] = paste("s",total.season[i],"-",j,sep="")
season.print[((n-1)*lcol+1):((n)*lcol),c(1,4:33)] = season.res[,,j,i]
}
}

# Writing season.print
cat("\nWriting seasonal results",sub(".csv","",f.list[flist]))
#Remove folder
f.print = gsub("/","---",f.list[flist])
#setwd(dir2)
#write.csv(season.print, file=paste(sub(".csv","",f.print),"_season-rs.csv",sep=""),row.names=FALSE)
#setwd(dir0)

cat("\nWriting f.update")
f.update = c(f.update,paste(flist,"/",length(f.list),":",f.print))
setwd(dir2)
write.csv(f.update, file="update_file-rs.csv",row.names=FALSE)
setwd(dir0)

# Combine all season file
season.print.all = rbind(season.print.all,season.print)

cat("\n*END* file:",f.list[flist])


} ## END flist(f.list)
######################



cat("\nWriting seasonal results",sub(".csv","",f.list[flist]))
setwd(dir2)
write.csv(season.print.all, file=paste("conclude_all_season_rs_",fname,".csv",sep=""),row.names=FALSE)
setwd(dir0)

colnames(couple.prd.obs) = c("prd","obs")
setwd(dir2)
write.csv(couple.prd.obs, file="couple column names.csv",row.names=FALSE)
setwd(dir0)

####################################################################################
########################## *** END great loop to change future file here ##############
####################################################################################


cat("\n -- END OF THE SCRIPT --\n")

