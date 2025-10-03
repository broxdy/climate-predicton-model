## To count and conclude the all names in table
maindir = "temp/accessory"
# working directory
dir_name = "count-unique"
dataf = "Extracted top prd at selected season and nprd GCM_mo_v4.csv"
unusedcol = 1

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name)
dirfile=(dir_name)
dir0=dirname(file.path("D:",paste(maindir),"dummy"))
dir1=dirname(file.path("D:",paste(maindir),paste(dirfile),"dummy"))


## Read DATA
data <- read.table(dataf, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
table = data[,-c(unusedcol)]

temp.table = integer(0)
## Combined DATA
for(i in 1:dim(table)[2]){temp.table = c(temp.table,table[,i])}


cat(paste("Total data:",length(temp.table),"\nUnique data (",length(unique(temp.table)),"):\n"))
cat(unique(temp.table)) # Conclude unique number of data

setwd(dir1)
write.csv(temp.table, file="all-values.csv")
write.csv(unique(temp.table), file="pivot-values.csv")
setwd(dir0)


cat("\nEND of SCRIPT")