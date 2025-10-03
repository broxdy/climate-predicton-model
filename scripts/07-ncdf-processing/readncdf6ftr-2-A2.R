## Input have to be : file name , code (series), run (run number)
##

library(RNetCDF)
library(chron)
library(GenKern)
## Function to combine different-length vector "cbind.all"
cbind.all <- function(..., fill.with = NA) { 

   args <- list(...) 
   len <- sapply(args, NROW) 
   if(diff(rng <- range(len)) > 0) { 

     maxlen <- rng[2]
     pad <- function(x, n) c(x, rep(fill.with, n))
     for(j in seq(along = args)) {
       if(maxlen == len[j]) next
       if(is.data.frame(args[[j]])) {
         args[[j]] <- lapply(args[[j]], pad, maxlen - len[j])
         args[[j]] <- as.data.frame(args[[j]])
       } else if(is.matrix(args[[j]])) {
         args[[j]] <- apply(args[[j]], 2, pad, maxlen - len[j])
       } else if(is.vector(args[[j]])) {
         args[[j]] <- pad(args[[j]], maxlen - len[j])
       } else {
         stop("... must only contain data.frames or arrays.")
       }
     }

   } 
   do.call("cbind", args) 
} 
###




### INITIAL CONSTANT ####
# set initial constant
extend.dat = FALSE
first.record = TRUE
first.table1= TRUE
first.table2= TRUE
first.table3= TRUE
first.table4= TRUE


## Read input list sheet
sheetf = "future file list A2v2a.csv"
sheet.list <- read.table(sheetf, stringsAsFactors=FALSE,header=FALSE, sep=",",dec=".",na.strings = "-")
# Set TRUE when memmory is not enough to whole entire one file for time-series analysis
largefile = FALSE # when TRUE it will not engage ts() funtion and the origin date have to be yyyy/1/1

datafile = sheet.list[,1]
codefile = sheet.list[,2] # file number within same series
runfile = sheet.list[,3] # model run number : run1 / run2 / run3
#nc.long = sheet.list[,4]
#nc.lat = sheet.list[,5]
#nc.time = sheet.list[,6]
#nc.subvar = sheet.list[,7]
#nc.time.4 = sheet.list[,8]
#nc.var = sheet.list[,9]
#nc.subvar = sheet.list[,10]

cat("Read file list\n")

# define run to keep in table by order etc c(1,4) run1=output.table1 , run4=output.table2
run.list = c(1,2,3,4)
#run.list = c(1)


# Output name
out.file = "output"
## specify output folder here
dir_name ="readncdf6ftr-A2a"

# Prepare blank table for output check id
out.id = cbind(sheet.list[,1],matrix(data = NA, nrow = length(sheet.list[,1]), ncol = 10,))
#colnames(out.id)[1] = "File"
#for(n in 2:11){colnames(out.id)[n] = paste("id",(n-2))}

# Define for finding the focus grid cells
my.exact.long = 101.2
my.exact.lat = 12.7
#mync.grid.long = 1 # Position for selected lontitude
#mync.grid.lat = 1 # Position for selected latitude




# Set origin day for counting date
mync.time.start.day = 1
mync.time.start.month = 1
mync.time.start.year = 1800
mync.time.calendarday = 365 # Day in a calendar year
#mync.time.calendarday = 365.25 # Day in a calendar year
#mync.time.calendarday = 12 # month



L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","CMIP3","Future Scenario","A2","dummy"))
dir1=dirname(file.path("D:","CMIP3","Future Scenario","A2",paste(dir_name),"dummy"))

# check NC id
for(i in 1:length(datafile))	{
				    	nc = open.nc(datafile[i])
					allvar.n = file.inq.nc(nc)$nvars
					# Record ID into output check
					for(check.id in 0:(allvar.n-1)){out.id[i,check.id+2] =	var.inq.nc(nc,check.id)$name}
					close.nc(nc)
					}
setwd(dir1)
write.csv(out.id, file = "out-id.csv")
setwd(dir0)


##############################
## Read NCDF file start here #

nfile = length(datafile)

### START loop to change file ###
#nfile = 3
for(i in 1:nfile){

cat(paste("\n\nn =",i,"  Run:",runfile[i]))
nc = open.nc(datafile[i])
cat("\nStart Read NCDF\n")
if(i==1){print.nc(nc)}
#print.nc(nc)

# Define dimensions
mync.dim = file.inq.nc(nc)
allvar.n = mync.dim$nvars

# position start first id starts at 0 // define by file or default value
#mync.var.id = 6 # (default set to allvar.n -1) Position for variable to read

# search id from NC file
long.srch.id = which(out.id[i,]=="lon")-2
lat.srch.id = which(out.id[i,]=="lat")-2
time.srch.id = which(out.id[i,]=="time")-2
if(length(which(out.id[i,]=="plev"))>0){subvar.srch.id = which(out.id[i,]=="plev")-2
							}else{subvar.srch.id = which(out.id[i,]=="height")-2}



if(is.na(sheet.list[i,4])){mync.long.id = long.srch.id}else{mync.long.id = sheet.list[i,4]} # Position for Longtitude coordinate
if(is.na(sheet.list[i,5])){mync.lat.id = lat.srch.id}else{mync.lat.id = sheet.list[i,5]} # Position for Latitude coordinate
if(is.na(sheet.list[i,6])){mync.time.id = time.srch.id}else{mync.time.id = sheet.list[i,6]} # Position for time dimension
if(is.na(sheet.list[i,7])){mync.subvar.id = subvar.srch.id}else{mync.subvar.id = sheet.list[i,7]} # Position for sub variable, if there are 4 dimension in available
if(is.na(sheet.list[i,8])){mync.time.id4 = time.srch.id}else{mync.time.id4 = sheet.list[i,8]} # Position for time dimension for 4 dimensions

# Print id positions
cat(paste("Long.   id:",mync.long.id,"\n"))
cat(paste("Lat.    id:",mync.lat.id,"\n"))
cat(paste("Time    id:",mync.time.id,"\n"))
cat(paste("Sub.Var id:",mync.subvar.id,"\n"))



# Find closest grid cell
mync.grid.long = nearest(var.get.nc(nc,mync.long.id),my.exact.long)
mync.grid.lat = nearest(var.get.nc(nc,mync.lat.id),my.exact.lat)

# Read Varidable properties
# at the last position of variables
if(is.na(sheet.list[i,9])){var.info = var.inq.nc(nc, allvar.n-1)
				 }else{var.info = var.inq.nc(nc, sheet.list[i,9])}
var.name = var.info$name



# Read Time
if(var.info$ndims == 3)	{var.time = var.get.nc(nc, mync.time.id)
				}else{var.time = var.get.nc(nc, mync.time.id4)} # time ID for 4 dimension



## Check dimension and read data of extracted variable
# if have 3 dimension
if(var.info$ndims == 3)	{
		if(is.na(sheet.list[i,9])){var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1),c(1,1,NA))
						 }else{var.data = var.get.nc(nc, sheet.list[i,9], c(mync.grid.long,mync.grid.lat,1),c(1,1,NA))}
		# Convert variaable to table and put header
		var.table = as.data.frame(var.data)
		colnames(var.table) = var.name
		cat("/Dimension:3\n")
				}else	{


					# if have 4 dimension
					if(var.info$ndims == 4)	{
									if(is.na(sheet.list[i,10])){subvar.n = length(var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1,1),c(1,1,NA,1)))
													  }else{subvar.n = length(var.get.nc(nc, sheet.list[i,10], c(mync.grid.long,mync.grid.lat,1,1),c(1,1,NA,1)))}
									subvar.name = var.get.nc(nc, mync.subvar.id, 1,NA)
									# Loop for sub variable
									for(j in 1:subvar.n)	{
													if(is.na(sheet.list[i,10])){var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,j,1),c(1,1,1,NA))
																	 }else{var.data = var.get.nc(nc, sheet.list[i,10], c(mync.grid.long,mync.grid.lat,j,1),c(1,1,1,NA))}
													subvar.table = as.data.frame(var.data)
													colnames(subvar.table) =  paste(var.name,format(subvar.name[j],scientific = FALSE))
													cat("/Dimension:4\n")
													# Put core for the first loop		
													if(j == 1)	{var.table = subvar.table
															}else{var.table = cbind(var.table,subvar.table)}
													}
									}else	{
										cat("variable dimension does not meet default")
										stop()
										}
					}


## END check dimension and read data of extracted variable

# Show file name
cat(paste("File:",datafile[i]))

# Record time dimension to output table
if(largefile==TRUE)	{
				time.table = as.data.frame(mync.time.start.year + var.time/mync.time.calendarday)
				}else	{
					var.ts = ts(var.data, frequency=mync.time.calendarday,start=(c(mync.time.start.year+var.time[1]/mync.time.calendarday,mync.time.start.month,mync.time.start.day)))
					time.table = as.data.frame(time(var.ts))
					}

colnames(time.table) = paste("Time",var.name)

# Combine columns of time and variable outputs
var.table = cbind(time.table,var.table)

# Combine varible output to former output table
if(i>1){
							if(extend.dat == TRUE)	{
											var.table = rbind(var.table.extend[1:length(var.table.extend[,1]),],var.table[1:length(var.table[,1]),])
											cat(paste("\n#Combined:"))
											}
	 }
# Return recorded values to var.table.extend
var.table.extend = var.table

# Check extendation of this series
if(i==nfile){extend.dat=FALSE
		}else	{if(codefile[i+1] == (codefile[i]+1))	{extend.dat = TRUE
										}else	{extend.dat = FALSE
											}
			}

# For normal case full data within one file
if(extend.dat == FALSE)	{
				if(first.record==TRUE){
								output.table = var.table
								n.var.table  = length(colnames(var.table))
								first.record = FALSE
								output.table = cbind.all(output.table,var.table)[,-c(1:n.var.table)] # [,-c(1:2)] to delete temp col
							    }else	{
									output.table = cbind.all(output.table,var.table)
									}
				}
				


# show progress each loop
if(extend.dat == TRUE)	{cat(paste("\n#Extended:"))}
cat(paste("\n",colnames(var.table)[-c(1)]))
cat("\nEnd reading NCDF\n")

# check that data still on the same run number (runfile)
if(i < nfile){
	if(runfile[i+1]!=runfile[i])	{
						first.record = TRUE # to reset output.table
											if(first.table1==TRUE)	{
															cat("First Table1 created\n")
															table1=output.table[,1:2] # if put only one column [,1] the column's name will be defined
															table1=cbind.all(table1,output.table)[,-c(1:2)] # [,-c(1:2)] to delete temp col
															first.table1=FALSE
															}else	{
																cat("Table1 combined\n")
																table1=cbind.all(table1,output.table)
						}										}
# for last file
		}else{
			if(first.table1==TRUE)	{
							cat("Last Table1 created\n")
							table1=output.table[,1:2] # if put only one column [,1] the column's name will be defined
							table1=cbind.all(table1,output.table)[,-c(1:2)]  # [,-c(1:2)] to delete temp col
							first.table1=FALSE
							}else	{
								cat("Table1 ended\n")
								table1=cbind.all(table1,output.table)
								}
			}
			
close.nc(nc)
}
################
# END of loop to change file

nc = open.nc(datafile[i])

cat(paste("\n\n#################\n#### Read:",i,"file(s) /",nfile,"variables(s)\n"))
cat(paste("GRID:\n lat  =",var.get.nc(nc,mync.long.id)[mync.grid.long],"(",mync.grid.long,")\n long =",var.get.nc(nc,mync.lat.id)[mync.grid.lat],"(",mync.grid.long,")\n")) 
if(largefile==FALSE)	{
				cat(paste("DAY/YEAR=",mync.time.calendarday,"\n start date=",time(var.ts)[1],"\n end date=",time(var.ts)[length(var.ts)], "\n total data =",length(var.ts),"\n##Varibales(s)##\n"))
				}else	{
					cat(paste("DAY/YEAR=",mync.time.calendarday,"\n start date=",mync.time.start.year + var.time[1]/mync.time.calendarday,"\n end date=",mync.time.start.year + var.time[length(var.time)]/mync.time.calendarday
					, "\n total data =",length(var.time),"\n##Varibales(s)##\n"))
					}
cat(paste("#Table",run.list[1],"\n"))
cat(paste(colnames(table1),"\n"))

close.nc(nc)

setwd(dir1)
write.csv(table1, file=paste("EXTRACTED",gsub(".csv","",sheetf),".csv"))
setwd(dir0)
