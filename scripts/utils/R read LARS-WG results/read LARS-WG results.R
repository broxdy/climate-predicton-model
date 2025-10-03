#library(gdata)
library(date)

source("clear.R")
clear()

# Define file contain list of station names and file names
#f.list = "file-list.txt"
f.list = "file-list-sel staB1.txt"

# Define title names
f.title.prefix = "obsal_"
f.title.suffix = "WG.dat"
#f.title.suffix = "_A1B_t2WG.dat"
f.title =  unlist(read.table(f.list, stringsAsFactors=FALSE,header=FALSE, sep=",",dec=".",na.strings="-"))
f.title.full = paste(f.title.prefix,f.title,f.title.suffix, sep = "")

maindir = "temp/accessory/R read LARS-WG results"
dir_name = paste(f.title.prefix,"sel outputB1",sep="")
#dir_name = paste(f.title.prefix,sub("WG.dat","",f.title.suffix),sep="")

# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))


# Get number of row from a data file
f1.data =  read.table(f.title.full[1], stringsAsFactors=FALSE,header=FALSE, sep="\t",dec=".",na.strings="-")
col.n = length(f.title)
row.n = dim(f1.data)[1]
year.list = f1.data[,2]

out.table = array(NA, c(row.n,col.n,3))


for(i in 1:col.n){

f1 = f.title.full[i]
#f1 = "ECHO-G daily 1971-2000v2.csv"
f1.data =  read.table(f1, stringsAsFactors=FALSE,header=FALSE, sep="\t",dec=".",na.strings="-")
#if limit.prd.ln == TRUE, then prd name will fix at 4 chr

cat("\nRecord :",f1)
out.table[,i,1] = f1.data[,4]
out.table[,i,2] = f1.data[,5]
out.table[,i,3] = f1.data[,6]

}

colnames(f1.data)[2:3] = c("year","jday")
colnames(out.table[,,1]) = f.title

cat("\nWritting conclude file")
setwd(dir1)
out.print = data.frame(out.table[,,1])
colnames(out.print) = f.title
write.csv(cbind(f1.data[,2:3],out.print),file = paste(f.title.prefix,"tmn.csv",sep=""),row.names = FALSE)

out.print = data.frame(out.table[,,2])
colnames(out.print) = f.title
write.csv(cbind(f1.data[,2:3],out.print),file = paste(f.title.prefix,"tmx.csv",sep=""),row.names = FALSE)

out.print = data.frame(out.table[,,3])
colnames(out.print) = f.title
write.csv(cbind(f1.data[,2:3],out.print),file = paste(f.title.prefix,"pcp.csv",sep=""),row.names = FALSE)
setwd(dir0)




########### Monthly conclusion ###############

year.uni = unique(year.list)
year.n = length(year.uni)

#t = ts(1:data.freq, frequency=data.freq,start=c(1971,1)))
t.date = as.Date(1:365, origin = "1970/12/31") # year 1971 has 365 days
month.list = rep(as.POSIXlt(t.date)$mon+1,year.n)


out.month = array(NA, c(year.n*12,col.n,3))
month.time = array(NA, c(year.n*12,2))
colnames(month.time) = c("month","year")

for(year.i in 1:length(unique(year.list))){
cat("\nConclude yr:",year.uni[year.i])
	row.sel.1 = which(year.list == year.i)
	for(month.i in 1:12){
		month.time[(year.i-1)*12+month.i,1] = month.i
		month.time[(year.i-1)*12+month.i,2] = year.i
		for(col.i in 1:3){
			row.sel.2 = which(month.list[row.sel.1] == month.i)
			out.month[(year.i-1)*12+month.i,,col.i] = colMeans(out.table[row.sel.1[row.sel.2],,col.i])
			}
		}
	}

cat("\nWritting conclude file")
setwd(dir1)
out.print = out.month[,,1]
colnames(out.print) = f.title
write.csv(cbind(month.time,out.print),file = paste(f.title.prefix,"tmn-month.csv",sep=""),row.names = FALSE)

out.print = out.month[,,2]
colnames(out.print) = f.title
write.csv(cbind(month.time,out.print),file = paste(f.title.prefix,"tmx-month.csv",sep=""),row.names = FALSE)

out.print = out.month[,,3]
colnames(out.print) = f.title
write.csv(cbind(month.time,out.print),file = paste(f.title.prefix,"pcp-month.csv",sep=""),row.names = FALSE)
setwd(dir0)

