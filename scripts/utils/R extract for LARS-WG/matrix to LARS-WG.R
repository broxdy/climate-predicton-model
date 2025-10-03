library(gdata)
f1 = "Daily Filled all climate 1971-2000_jday.csv"
#f1 = "ECHO-G daily 1971-2000v2.csv"
f1.data =  read.table(f1, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#if limit.prd.ln == TRUE, then prd name will fix at 4 chr
limit.prd.ln = FALSE

maindir = "Temp/accessory/R extract for LARS-WG"
#dir_name = "Daily Filled all climate 1971-2006+Wet_V2b"

#file_prefix = "c20_"
file_prefix = "obs"

#file_subfix = "ca"
#file_subfix = "vf"
file_subfix = "al"

dir_name = sub(".csv","",f1)
# Check working directory
dir_name1 = dir_name
L=file.exists(dir_name1)  
if(!L) dirfile=dir.create(dir_name1)
dirfile=(dir_name1)

dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
source("clear.R")
clear()

#remove X478201 col.13
f1.data = f1.data[,-c(13)]
n.row = dim(f1.data)[1]
n.col = dim(f1.data)[2]
n.col.date = 3
n.col.temp = 8
n.col.rain = n.col - n.col.date - n.col.temp

temp.col = f1.data[,4:11]
sel.temp.sta = 1 # from 1 to 4
sel.temp.sta = c(4,1,2,3,4,4,4,4,4,4,3,4,1,1,1,3,1,4,4,4,4,2,4,4)


for(i in 1:n.col.rain){
	if(limit.prd.ln){
				par.name = substr(sub("\\.","",colnames(f1.data)[n.col.date+n.col.temp+i]),1,4)
				if(nchar(par.name) <= 3){par.name = paste(par.name,paste(rep("_",4-nchar(par.name)),collapse=""),sep ="")}
				f.name = paste(file_prefix,par.name,file_subfix,".dat",sep ="")
				}else{
					par.name = paste(sub("\\.","",colnames(f1.data)[n.col.date+n.col.temp+i]),sep="")
					f.name = paste(file_prefix,file_subfix,"_",par.name,".dat",sep ="")
					}


	f.print = data.frame(	year=format(f1.data[,2],width = 5),
				 	month=format(f1.data[,3],width = 4),
					mxtmp=format(temp.col[,sel.temp.sta[i]*2-1],width = 7),
					mxtmp=format(temp.col[,sel.temp.sta[i]*2],width = 7),
				 	rain=format(f1.data[,n.col.date+n.col.temp+i],width = 7),
					stringsAsFactors=FALSE)



	setwd(dir1)
	write.fwf(f.print,file = f.name, colnames = FALSE, sep="")
	setwd(dir0)
	cat("Write :",i,f.name,"\n")


	## For creating st file for calling weather data (*.st)
	st.file = c("[SITE]",
	 sub(".dat","",f.name),
	"[LAT, LON and ALT]",
	"	12.70	101.2	  3",
	"[WEATHER FILES]",
	f.name,
	"[FORMAT]",
	"YEAR JDAY MAX MIN RAIN",
	"[END]")

	setwd(dir1)
	write(st.file,file = paste("Rayong-",i,".st",sep=""), sep="")
	setwd(dir0)

}

