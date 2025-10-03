maindir = "Temp/accessory/R Boxplot"
dir_name = "Boxplot_concludeAll-1c-PCPcal1971-1985vrf1971-1985-vertical"

day.in.month.f = "day in month.csv"
day.in.month.list =  read.table(day.in.month.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


# 1st data
data.f1.daily = "Daily Filled all climate 1971-1985+Wet_V2.csv"
#data.f1.daily = "Daily Filled all climate 1986-2000+Wet_V2.csv"
daily1.unusedcol.data = 19
daily1.unusedcol2.data = 44
daily.year.col = 1
daily1.month.col = 2
daily.day.col = 3
daily.365day.col = 5

# 2nd data
data.f2.daily = "PCPcal1971-1985vrf1971-1985.csv"
#data.f2.daily = "PCPcal1971-1985vrf1986-2000.csv"
#data.f2.daily = "conclude rain all-rlz all-sta NO-I-DEPENDENCEv3c.csv"
#data.f2.daily = "conclude rain all-rlz all-sta daily Macoef.csv"
#data.f2.daily = "conclude rain all rlz 2005.csv"
daily2.unusedcol.data = 3
daily2.unusedcol2.data = NA
daily2.year.col = 1
daily2.month.col = 2
daily2.day.col = 3
daily.365day.col = 5

# Rainy-day data
data.f1.rainy = "WetDay1971-1985.csv"
#data.f1.rainy = "WetDay1986-2000.csv"
#data.f1.rainy = "WetDay1971-2006.csv"
data.f2.rainy = "conclude rainy in month cal1971-1985vrf1971-1985.csv"
#data.f2.rainy = "conclude rainy in month cal1971-1985vrf1986-2000.csv"
#data.f2.rainy = "conclude rainy in month NO-I-DEPENDENCEv3c.csv"
#data.f2.rainy = "conclude rainy in month daily Macoef.csv"
#data.f2.rainy = "rainy-day pivot 2075.csv"

# Monthly rain data - 2nd data monthly conclusiton file (if use EXCEL to calculate)
pivot.f = "monthly-sum all-sta NO-I-DEPENDENCEv3c.csv"
#pivot.f = "conclude rain pivot-2006 daily Macoef.csv"
#pivot.f = "conclude rain pivot-2075.csv"


# PDF file
pdffile = "BoxplotDaily.pdf"
pdffile.monthly = "BoxplotMonthly.pdf"
run.monthly2.cal = FALSE # to summary monthly data 2 by this script .. spend longer time

n.sta.1 = 24
#n.sta.2 = 24
n.sta.2 = n.sta.1
n.rlz.1 = 1
n.rlz.2 = 30

n.row = 2
#n.col = 24
n.col = n.sta.1
bp.ylim = 30
#bp.ylim = 5

bp.ylim.monthly = 500
#bp.ylim.monthly = 5


PlotPDF = TRUE
#PlotPDF = FALSE

# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
#source("clear.R")
#clear()


############# 1st dialy files (obs)
## Daily data
cat("\nReading 1st data")
data1.daily.org =  read.table(data.f1.daily, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
# also remove "X478201" (col. 21)
data1.daily = data1.daily.org[,-c(1:daily1.unusedcol.data,21,(daily1.unusedcol2.data+1):dim(data1.daily.org)[2])]
station.title1 = colnames(data1.daily)


daily1.year.list = data1.daily.org[,daily.year.col]
daily1.month.list = data1.daily.org[,daily1.month.col]
daily.day.list = data1.daily.org[,daily.day.col]
daily.365day.list = data1.daily.org[,daily.365day.col]

row1.n = dim(data1.daily)[1]


############# 2nd dialy files (predicted)
## Daily data
cat("\nReading 2nd data")
data2.daily =  read.table(data.f2.daily, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,-c(1:daily2.unusedcol.data)]
#data2.daily =  data2.daily.org[,-c(1:daily2.unusedcol.data)]
#daily2.month.list = data2.daily.org[,daily2.month.col]
daily2.month.list = read.table(data.f2.daily, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,daily2.month.col]
daily2.year.list = read.table(data.f2.daily, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,daily2.year.col]

station.title2 = colnames(data2.daily)
row2.n = dim(data2.daily)[1]

############ Attributes of data
atrb.row = c("count","means","SD",names(quantile(c(1:10))))

# Rainfall
atrb.daily.1 = array(NA, c(8,n.sta.1,12))
rownames(atrb.daily.1) = atrb.row
colnames(atrb.daily.1) = station.title1
atrb.daily.2 = atrb.daily.1

atrb.monthly.1 = array(NA, c(8,n.sta.1,12))
rownames(atrb.monthly.1) = atrb.row
colnames(atrb.monthly.1) = station.title1
atrb.monthly.2 = atrb.monthly.1


# Rainy day
atrb.monthly.1.rainyday = array(NA, c(8,n.sta.1,12))
rownames(atrb.monthly.1.rainyday) = atrb.row
colnames(atrb.monthly.1.rainyday) = station.title1
atrb.monthly.2.rainyday = atrb.monthly.1.rainyday






if(PlotPDF){
cat("\n## Plot daily box plot")
setwd(dir1)
pdf(pdffile, width= 6*n.col, height = 5*n.row)
setwd(dir0)
}
par(ps =20,mfrow=c(n.row,n.col),mai = c(.8, .9, .2, .5))

##################
#### Plot box plot
cat("\n- Plot 1st daily average box plot")
##########
##1st data
for(i in 1:n.sta.1){
#for(i in 5:n.sta.1){

				cat("\nPlotting 1st data, station:",i)
				boxplot(as.real(data1.daily[1:row1.n,i])~daily1.month.list,ylim = c(0,bp.ylim),xlab = "month", ylab="daily rainfall (mm/day)", main = paste("observation",station.title1[i]), na.omit = TRUE, horizontal = FALSE, outline= FALSE)

				for(month.i in 1:12){
								atrb.daily.1[1,i,month.i] = length(which(!is.na(data1.daily[which(daily1.month.list==month.i),i])))
								atrb.daily.1[2,i,month.i] = mean(data1.daily[which(daily1.month.list==month.i),i], na.rm = TRUE)
								atrb.daily.1[3,i,month.i] = sd(data1.daily[which(daily1.month.list==month.i),i], na.rm = TRUE)
								atrb.daily.1[4:8,i,month.i] = quantile(data1.daily[which(daily1.month.list==month.i),i], na.rm = TRUE)
								}


				}

setwd(dir1)
write.csv(atrb.daily.1,file = "atributes 1st daily allrlz.csv")
setwd(dir0)

##########
##2nd data

cat("\n- Plot 2nd daily average box plot")
# run each station
for(i in 1:n.sta.2){ 
#for(i in 5:n.sta.2){ 

			# run set of realization n.rlz.2
			rang.col = (1+(i-1)*n.rlz.2):(i*n.rlz.2)
			cat("\n  -Plotting 2nd data, station:",i)
			boxplot(as.real(unlist(data2.daily[1:row2.n,rang.col]))~rep(daily2.month.list,n.rlz.2),xlab = "month", ylab="daily rainfall (mm/day)",ylim = c(0,bp.ylim), main = paste("prediction",station.title1[i]), na.omit = TRUE, horizontal = FALSE, outline= FALSE)

				for(month.i in 1:12){
								atrb.daily.2[1,i,month.i] = length(which(!is.na(as.real(unlist(data2.daily[which(daily2.month.list==month.i),rang.col])))))
								atrb.daily.2[2,i,month.i] = mean(as.real(unlist(data2.daily[which(daily2.month.list==month.i),rang.col])), na.rm = TRUE)
								atrb.daily.2[3,i,month.i] = sd(as.real(unlist(data2.daily[which(daily2.month.list==month.i),rang.col])), na.rm = TRUE)
								atrb.daily.2[4:8,i,month.i] = quantile(as.real(unlist(data2.daily[which(daily2.month.list==month.i),rang.col])), na.rm = TRUE)
								}


			}


setwd(dir1)
write.csv(atrb.daily.2,file = "atributes 2nd daily allrlz.csv")
setwd(dir0)

if(PlotPDF){dev.off()}



##########################################
##################### Summary Monthly data
##########################################

unique.year.1 = unique(daily1.year.list)
unique.month.1 = unique(daily1.month.list)

unique.year.2 = unique(daily2.year.list)
unique.month.2 = unique(daily2.month.list)

sum.month.1 = as.data.frame(array(NA,c(length(unique.year.1)*length(unique.month.1),n.sta.1*n.rlz.1)))
sum.month.2.mmmonth = as.data.frame(array(NA,c(length(unique.year.2)*length(unique.month.2),n.sta.2*n.rlz.2)))
date.list.sum.1 = array(NA,c(length(unique.year.1)*length(unique.month.1),2))
date.list.sum.2 = array(NA,c(length(unique.year.2)*length(unique.month.2),2))
colnames(date.list.sum.1) = c("year","month")
colnames(date.list.sum.2) = c("year","month")

##******************##
cat("\nSum 1st Data")
for(i in 1:n.sta.1){
for(l in 1:n.rlz.1){
for(j in 1:length(unique.year.1)){
for(k in 1:length(unique.month.1)){

		row.sel = which(daily1.year.list==unique.year.1[j])
		row.sel = row.sel[which(daily1.month.list[row.sel]== unique.month.1[k])]
		row.n = (j-1)*length(unique.month.1)+k

		dat = data1.daily[row.sel,(i-1)*n.rlz.1+l]
		if(!length(dat)==0){
					sum.month.1[row.n,(i-1)*n.rlz.1+l] = sum(as.real(dat))
					}else{sum.month.1[row.n,(i-1)*n.rlz.1+l] = NA}
		date.list.sum.1[row.n,1] = unique.year.1[j]
		date.list.sum.1[row.n,2] = unique.month.1[k]

		#cat("\nSta",i,"/Year",unique.year.1[j],"/Month",unique.month.1[k],"/Rlz",l)
}
}
colnames(sum.month.1)[(i-1)*n.rlz.1+l] = paste(station.title1[i],"-",l,sep="")
cat("\nSta",i,"/Rlz",l)
}
}
setwd(dir1)
write.csv(cbind(date.list.sum.1,sum.month.1),file = "monthly1data.csv",row.names = FALSE)
setwd(dir0)




if(run.monthly2.cal){
##******************##
cat("\nSum 2nd Data")
for(i in 1:n.sta.2){
for(l in 1:n.rlz.2){
for(j in 1:length(unique.year.2)){
#for(k in 1:length(unique.month.2)){
for(k in c(1,10:10)){

		row.sel = which(daily2.year.list==unique.year.2[j])
		row.sel = row.sel[which(daily2.month.list[row.sel]== unique.month.2[k])]
		row.n = (j-1)*length(unique.month.2)+k

		dat = data2.daily[row.sel,(i-1)*n.rlz.1+l]
		if(!length(dat)==0){
					sum.month.2.mmmonth[row.n,(i-1)*n.rlz.2+l] = sum(as.real(dat))
					}else{sum.month.2.mmmonth[row.n,(i-1)*n.rlz.2+l] = NA}
		date.list.sum.2[row.n,1] = unique.year.2[j]
		date.list.sum.2[row.n,2] = unique.month.2[k]

		#cat("\nSta",i,"/Year",unique.year.2[j],"/Month",unique.month.2[k],"/Rlz",l)
		#cat(sum.month.2.mmmonth[row.n,(i-1)*n.rlz.1+l])
}
}
colnames(sum.month.2.mmmonth)[(i-1)*n.rlz.2+l] = paste(station.title1[i],"-",l,sep="")
cat("\nSta",i,"/Rlz",l)
}
}
setwd(dir1)
write.csv(cbind(date.list.sum.2,sum.month.2.mmmonth),file = "monthly2data.csv",row.names = FALSE)
setwd(dir0)

}
#END run this loop



## Read from pivot table (FROM conclusion file)
runthisloop2 = !run.monthly2.cal
if(runthisloop2){
cat("\nSum 2nd Data by EXCEL file")
sum.month.2.mmmonth =  read.table(pivot.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,-c(1:2)]
date.list.sum.2 =  read.table(pivot.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,c(1:2)]
}





#################### Summarize monthly data (mm/month)
#################### average all realization (mm/day mm/month)
cat("\n##Writing Summary monthly data (mm/month)")

sum.month.2.mmday = matrix(NA, nrow= dim(sum.month.2.mmmonth)[1], ncol = (dim(sum.month.2.mmmonth)[2]))
sum.month.2.mmmonth.avg = matrix(NA, nrow= dim(sum.month.2.mmmonth)[1], ncol = n.sta.2)
sum.month.2.mmday.avg = matrix(NA, nrow= dim(sum.month.2.mmmonth)[1], ncol = n.sta.2)

row.i = 1
year.i = 1
month.i = 1

cat("\n-Writing Summary monthly data average (average all realization)")

while(row.i < dim(sum.month.2.mmmonth)[1]){
	cat("Year:",year.i,"\n")
	while((month.i <= 12)&&(row.i <= dim(sum.month.2.mmmonth)[1])){


		day.in.month = day.in.month.list[which(day.in.month.list[,2]==month.i),5][year.i]
		sum.month.2.mmday[row.i,] = as.real(sum.month.2.mmmonth[row.i,])/day.in.month

		for(station.i in 1:n.sta.2){
			#cat(station.i,row.i,year.i,month.i,"\n")
			sum.month.2.mmmonth.avg[row.i,station.i] = mean(unlist(sum.month.2.mmmonth[row.i,((station.i-1)*n.rlz.2+1):((station.i)*n.rlz.2)])) # average all 30 realization
			sum.month.2.mmday.avg[row.i,station.i] = mean(unlist(sum.month.2.mmday[row.i,((station.i-1)*n.rlz.2+1):((station.i)*n.rlz.2)])) # average all 30 realization, first 2 columns are date data
			}

		row.i = row.i+1
		month.i = month.i+1
		}

	month.i = 1
	year.i = year.i+1
}

colnames(sum.month.2.mmmonth.avg) = station.title1
colnames(sum.month.2.mmmonth.avg) = station.title1

setwd(dir1)
write.csv(cbind(date.list.sum.2,sum.month.2.mmmonth),file = "monthly2data-mm-month.csv",row.names = FALSE)
write.csv(cbind(date.list.sum.2,sum.month.2.mmmonth.avg),file = "monthly2data-mm-month-allrlz.csv",row.names = FALSE)
write.csv(cbind(date.list.sum.2,sum.month.2.mmday.avg),file = "monthly2data-mm-day-allrlz.csv",row.names = FALSE)
setwd(dir0)


















################### Plot monthly data ###########################
n.row = 3
if(PlotPDF){
cat("\n## Plot monthly box plot")
setwd(dir1)
pdf(pdffile.monthly, width= 6*n.col, height = 5*n.row)
setwd(dir0)
}
par(ps =20,mfrow=c(n.row,n.col),mai = c(.8, .9, .2, .5))

##################
#### Plot box plot
cat("\n- Plot observed monthly average box plot")
##########
##1st data
for(i in 1:n.sta.1){
#for(i in 5:n.sta.1){

				cat("\n  -Plotting 1st data, station:",i)
				boxplot(sum.month.1[,i]~date.list.sum.1[,2],ylim = c(0,bp.ylim.monthly), main = paste("observation",station.title1[i]), na.omit = TRUE, horizontal = FALSE, outline= FALSE)

				for(month.i in 1:12){
								atrb.monthly.1[1,i,month.i] = length(which(!is.na(as.real(sum.month.1[which(date.list.sum.1[,2]==month.i),i]))))
								atrb.monthly.1[2,i,month.i] = mean(as.real(sum.month.1[which(date.list.sum.1[,2]==month.i),i]), na.rm = TRUE)
								atrb.monthly.1[3,i,month.i] = sd(as.real(sum.month.1[which(date.list.sum.1[,2]==month.i),i]), na.rm = TRUE)
								atrb.monthly.1[4:8,i,month.i] = quantile(as.real(sum.month.1[which(date.list.sum.1[,2]==month.i),i]), na.rm = TRUE)
								}


				}

setwd(dir1)
write.csv(atrb.monthly.1,file = "atributes 1st monthly allrlz.csv")
setwd(dir0)

###########################
##2nd data - every realization
cat("\n- Plot 2nd data - every realization")

#for(i in 1:n.sta.2){	boxplot(as.real(data2.daily[1:row2.n,i])~daily2.month.list,ylim = c(0,bp.ylim), main = paste("prediction",station.title2[i],"(",n.rlz.2,"Realizations)"), na.omit = TRUE, horizontal = TRUE)	}
#boxplot(as.real(unlist(data2.daily[1:row2.n,1:n.sta.2]))~rep(daily2.month.list,n.sta.2),ylim = c(0,bp.ylim), main = paste("prediction",station.title2[i]), na.omit = TRUE, horizontal = TRUE, outline= FALSE)

# run each station
for(i in 1:n.sta.2){ 
#for(i in 5:n.sta.2){ 

			# run set of realization n.rlz.2
			rang.col = (1+(i-1)*n.rlz.2):(i*n.rlz.2)
			cat("\n  -Plotting 2nd data all realization, station:",i)
			boxplot(as.real(unlist(sum.month.2.mmmonth[,rang.col]))~rep(date.list.sum.2[,2],n.rlz.2),ylim = c(0,bp.ylim.monthly), main = paste("prediction",station.title1[i],"(",n.rlz.2,"Realizations)"), na.omit = TRUE, horizontal = FALSE, outline= FALSE)

			for(month.i in 1:12){
							atrb.monthly.2[1,i,month.i] = length(which(!is.na(as.real(unlist(sum.month.2.mmmonth[which(date.list.sum.2[,2]==month.i),rang.col])))))
							atrb.monthly.2[2,i,month.i] = mean(as.real(unlist(sum.month.2.mmmonth[which(date.list.sum.2[,2]==month.i),rang.col])), na.rm = TRUE)
							atrb.monthly.2[3,i,month.i] = sd(as.real(unlist(sum.month.2.mmmonth[which(date.list.sum.2[,2]==month.i),rang.col])), na.rm = TRUE)
							atrb.monthly.2[4:8,i,month.i] = quantile(as.real(unlist(sum.month.2.mmmonth[which(date.list.sum.2[,2]==month.i),rang.col])), na.rm = TRUE)
							}

			}

setwd(dir1)
write.csv(atrb.monthly.2,file = "atributes 2nd monthly allrlz.csv")
setwd(dir0)


###########################
##2nd data - only average of each staion
cat("\n- Plot 2nd data - average realization")

# run each station
for(i in 1:n.sta.2){ 

			cat("\n  -Plotting 2nd data average all realization, station:",i)
			boxplot(as.real(unlist(sum.month.2.mmmonth.avg[,i]))~date.list.sum.2[,2],ylim = c(0,bp.ylim.monthly), main = paste("prediction",station.title1[i],"(Annual average)"), na.omit = TRUE, horizontal = FALSE, outline= FALSE)
			}

if(PlotPDF){dev.off()}










################### Plot rainy-day data Monthly ###########################
cat("\n\n##Plot Rain-day boxplot")
data1.rainyday =  read.table(data.f1.rainy, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,-c(1:2)]
data2.rainyday =  read.table(data.f2.rainy, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")[,-c(1:2)]
data1.n = dim(data1.rainyday)[1]
data2.n = dim(data2.rainyday)[1]

n.row = 2
if(PlotPDF){
setwd(dir1)
pdf("rainy-day chart.pdf", width= 6*n.col, height = 5*n.row)
setwd(dir0)
}
par(ps =20,mfrow=c(n.row,n.col),mai = c(.8, .9, .2, .5))

##################
#### Plot box plot
cat("\n- Plot monthly average box plot")
##########
##1st data
for(i in 1:n.sta.1){
#for(i in 5:n.sta.1){

				cat("\nPlotting 1st data, station:",i)
				boxplot(data1.rainyday[,i]~date.list.sum.1[1:data1.n,2],ylim = c(0,31), main = paste("Obs. Rainy-day",station.title1[i]), na.omit = TRUE, horizontal = FALSE, outline= FALSE)

				# Attributes rainyday
				for(month.i in 1:12){
								atrb.monthly.1.rainyday[1,i,month.i] = length(which(!is.na(as.real(data1.rainyday[which(date.list.sum.1[,2]==month.i),i]))))
								atrb.monthly.1.rainyday[2,i,month.i] = mean(as.real(data1.rainyday[which(date.list.sum.1[,2]==month.i),i]), na.rm = TRUE)
								atrb.monthly.1.rainyday[3,i,month.i] = sd(as.real(data1.rainyday[which(date.list.sum.1[,2]==month.i),i]), na.rm = TRUE)
								atrb.monthly.1.rainyday[4:8,i,month.i] = quantile(as.real(data1.rainyday[which(date.list.sum.1[,2]==month.i),i]), na.rm = TRUE)
								}
				}

###########################
##2nd data - every realization
cat("\n- Plot 2nd data - every realization")

# run each station
for(i in 1:n.sta.2){ 
#for(i in 5:n.sta.2){ 

			# run set of realization n.rlz.2
			rang.col = (1+(i-1)*n.rlz.2):(i*n.rlz.2)
			cat("\nPlotting 2nd data all realization, station:",i)
			boxplot(as.real(unlist(data2.rainyday[,rang.col]))~rep(date.list.sum.2[1:data2.n,2],n.rlz.2),ylim = c(0,31), main = paste("Predicted Rainy-day",station.title1[i],"(",n.rlz.2,"Realizations)"), na.omit = TRUE, horizontal = FALSE, outline= FALSE)

			# Attributes rainyday
			for(month.i in 1:12){
							atrb.monthly.2.rainyday[1,i,month.i] = length(which(!is.na(as.real(unlist(data2.rainyday[which(date.list.sum.2[,2]==month.i),rang.col])))))
							atrb.monthly.2.rainyday[2,i,month.i] = mean(as.real(unlist(data2.rainyday[which(date.list.sum.2[,2]==month.i),rang.col])), na.rm = TRUE)
							atrb.monthly.2.rainyday[3,i,month.i] = sd(as.real(unlist(data2.rainyday[which(date.list.sum.2[,2]==month.i),rang.col])), na.rm = TRUE)
							atrb.monthly.2.rainyday[4:8,i,month.i] = quantile(as.real(unlist(data2.rainyday[which(date.list.sum.2[,2]==month.i),rang.col])), na.rm = TRUE)
							}


			}

if(PlotPDF){dev.off()}

setwd(dir1)
write.csv(atrb.monthly.1.rainyday,file = "atributes 1st rainyday allrlz.csv")
write.csv(atrb.monthly.2.rainyday,file = "atributes 2nd rainyday allrlz.csv")
setwd(dir0)







