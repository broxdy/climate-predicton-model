#library(gdata)
library(date)

source("clear.R")
#clear()

# Define file contain list of station names and file names
f.list = "file-listSDSM.txt"
#f.list = "file-list-sel staB1.txt"

# number of realization
rlz.n = 30

# Define title names
f.title.prefix = "obsal_"

f.title.suffix = ".out"
#f.title.suffix = "WG.dat"
#f.title.suffix = "_A1B_t2WG.dat"

f.title =  unlist(read.table(f.list, stringsAsFactors=FALSE,header=FALSE, sep=",",dec=".",na.strings="-"))
f.title.full = paste(f.title.prefix,f.title,f.title.suffix, sep = "")

maindir = "temp/accessory/R read SDSM results"
#dir_name = paste(f.title.prefix,"1971-2000",sep="")
dir_name = paste(f.title.prefix,"2081-2100A1Bt2",sep="")
#dir_name = paste(f.title.prefix,sub(".out","",f.title.suffix),sep="")

# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))


# Get number of row from a data file
#f1.data =  read.table(f.title.full[1], stringsAsFactors=FALSE,header=FALSE, sep="\t",dec=".",na.strings="-")
f1.data =  read.fwf(f.title.full[1], widths=rep(14,rlz.n), stringsAsFactors=FALSE,header=FALSE, dec=".", na.strings="-")
#col.n = length(f.title)
row.n = dim(f1.data)[1]
sta.n = length(f.title)
#year.list = f1.data[,2]

out.table = array(NA, c(row.n,sta.n,rlz.n))
out.table.avg = array(NA, c(row.n,sta.n))


for(sta.i in 1:sta.n){
f1 = f.title.full[sta.i]
#f1.data =  read.table(f1, stringsAsFactors=FALSE,header=FALSE, sep="\t",dec=".",na.strings="-")
f1.data =  read.fwf(f1, widths=rep(14,rlz.n), stringsAsFactors=FALSE,header=FALSE, dec=".", na.strings="-")
cat("\nRecord :",f1)
for(rlz.i in 1:rlz.n){out.table[,sta.i,rlz.i] = f1.data[,rlz.i]}
}

data.freq = 360
year.list = integer(0)
month.list = integer(0)
day.list = integer(0)
month.pat = integer(0)

# Create month pattern
for(month.i in 1:12){month.pat = c(month.pat,rep(month.i,(data.freq/12)))}
month.list = rep(month.pat,(row.n/data.freq))

# Create time columns
for(year.i in 1:(row.n/data.freq)){
		year.list = c(year.list,rep(year.i,data.freq))
		day.list = c(day.list,rep(1:(data.freq/12),12))
		}

time.cols = cbind(year.list,month.list,day.list)

##
## conclude rlz1...
setwd(dir1)
for(rlz.i in 1:rlz.n){
	cat("\nWritting conclude file rlz:",rlz.i)
	out.print = data.frame(out.table[,,rlz.i])
	colnames(out.print) = f.title
	write.csv(cbind(time.cols,out.print),file = paste(f.title.prefix,"rlz",rlz.i,".csv",sep=""),row.names = FALSE)
	}
setwd(dir0)

##
## conclude sta1...
setwd(dir1)
for(sta.i in 1:sta.n){
	cat("\nWritting conclude file sta:",sta.i)
	out.print = data.frame(out.table[,sta.i,])
	colnames(out.print) = paste("rlz",c(1:rlz.n),sep="-")
	write.csv(cbind(time.cols,out.print),file = paste(f.title.prefix,"sta",sta.i,"-",f.title[sta.i],".csv",sep=""),row.names = FALSE)
	}
setwd(dir0)

##
## conclude all rlz...
cat("\nWritting conclude file all rlz:")
for(sta.i in 1:sta.n){out.table.avg[,sta.i] = rowMeans(out.table[,sta.i,])}
colnames(out.table.avg) = f.title
setwd(dir1)
write.csv(cbind(time.cols,out.table.avg),file = paste(f.title.prefix,"avgrlz.csv",sep=""),row.names = FALSE)
setwd(dir0)




########### Monthly conclusion ###############

year.uni = unique(year.list)
year.n = length(year.uni)

#t = ts(1:data.freq, frequency=data.freq,start=c(1971,1)))
#t.date = as.Date(1:365, origin = "1970/12/31") # year 1971 has 365 days
#month.list = rep(as.POSIXlt(t.date)$mon+1,year.n)


out.month = array(NA, c(year.n*12,sta.n,rlz.n))
out.month.avg = array(NA, c(year.n*12,sta.n))
month.time = array(NA, c(year.n*12,2))
colnames(month.time) = c("month","year")

for(year.i in 1:length(unique(year.list))){
cat("\nConclude yr:",year.uni[year.i])
	row.sel.1 = which(year.list == year.i)
	for(month.i in 1:12){
		month.time[(year.i-1)*12+month.i,1] = month.i
		month.time[(year.i-1)*12+month.i,2] = year.i
		row.sel.2 = which(month.list[row.sel.1] == month.i)
		for(rlz.i in 1:rlz.n){
			out.month[(year.i-1)*12+month.i,,rlz.i] = colMeans(out.table[row.sel.1[row.sel.2],,rlz.i])
			}
		}
	}

##
## conclude rlz1...
setwd(dir1)
for(rlz.i in 1:rlz.n){
	cat("\nWritting conclude file rlz:",rlz.i)
	out.print = out.month[,,rlz.i]
	colnames(out.print) = f.title
	write.csv(cbind(month.time,out.print),file = paste(f.title.prefix,"month-rlz",rlz.i,".csv",sep=""),row.names = FALSE)
	}
setwd(dir0)

##
## conclude sta1...
setwd(dir1)
for(sta.i in 1:sta.n){
	cat("\nWritting conclude file sta:",sta.i)
	out.print = out.month[,sta.i,]
	colnames(out.print) = paste("rlz",c(1:rlz.n),sep="-")
	write.csv(cbind(month.time,out.print),file = paste(f.title.prefix,"month-sta",sta.i,"-",f.title[sta.i],".csv",sep=""),row.names = FALSE)
	}
setwd(dir0)

##
## conclude all rlz...
cat("\nWritting conclude file all rlz:")
for(sta.i in 1:sta.n){out.month.avg[,sta.i] = rowMeans(out.month[,sta.i,])}
colnames(out.month.avg) = f.title
setwd(dir1)
write.csv(cbind(month.time,out.month.avg),file = paste(f.title.prefix,"month-avgrlz.csv",sep=""),row.names = FALSE)
setwd(dir0)
