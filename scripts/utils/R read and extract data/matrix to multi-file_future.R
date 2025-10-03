#f1 = "EXTRACTED ECHO-G_B1_2046-2065.csv"
f1 = "EXTRACTED ECHO-G_B1_2081-2100.csv"
#f1 = "ECHO-G daily 1971-2000v2.csv"
f1.data =  read.table(f1, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#if limit.prd.ln == TRUE, then prd name will fix at 4 chr
limit.prd.ln = TRUE

maindir = "Temp/accessory/R read and extract data"
#dir_name = "Daily Filled all climate 1971-2006+Wet_V2b"

file_prefix = "B1__"
#file_prefix = "obs_"

#file_subfix = "ca"
#file_subfix = "vf"
#file_subfix = "t1"
file_subfix = "t2"

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

#n.row = dim(f1.data)[1]
n.row = max(which(!is.na(f1.data[,1])))
n.col = dim(f1.data)[2]

#change column's names
for(i in 1:n.col){
			colnames(f1.data)[i] = sub("tasmax","tmx",colnames(f1.data)[i])
			colnames(f1.data)[i] = sub("tasmin","tmn",colnames(f1.data)[i])
			}


for(i in 1:n.col){
setwd(dir1)

if(limit.prd.ln){
			par.name = substr(sub("\\.","",colnames(f1.data)[i]),1,4)
			if(nchar(par.name) <= 3){par.name = paste(par.name,paste(rep("_",4-nchar(par.name)),collapse=""),sep ="")}
			f.name = paste(file_prefix,par.name,file_subfix,".dat",sep ="")
			}else{
				par.name = paste(sub("\\.","",colnames(f1.data)[i]),sep="")
				f.name = paste(file_prefix,file_subfix,"_",par.name,".dat",sep ="")
				}


f.print = f1.data[,i]
#write.table(f.print,file = f.name, row.names=FALSE, col.names=TRUE,sep = "")
#write.table(f.print,file = f.name, sep = ",", col.names = NA)
write(f.print[1:n.row],file = f.name, sep = "\n")
setwd(dir0)
cat("Write :",i,f.name,"\n")
}