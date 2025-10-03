## Input have to be : file name , code (series), run (run number)
##

library(RNetCDF)
library(chron)
library(GenKern)

### INITIAL CONSTANT ####
# set when file need to be bounded with another file
extend.dat = FALSE
first.record = TRUE
first.table1= TRUE
first.table2= TRUE

## Read input list sheet
sheetf = "MPI-ECH5_g-20c3m-atm-da1960+file_run.csv"
sheet.list <- read.table(sheetf, stringsAsFactors=FALSE,header=FALSE, sep=",",dec=".")

datafile = sheet.list[,1]
codefile = sheet.list[,2]
runfile = sheet.list[,3]


# define run to keep in table by order etc c(1,4) run1=output.table1 , run4=output.table2
run.list = c(1,4)


# Output name
out.file = "output"
## specify output folder here
dir_name ="readncdf4"

mync.time.id = 4 # Position for time dimension
mync.long.id = 0 # Position for longtitude
mync.lat.id = 2 # Position for lat
mync.subvar.id = 4 # Position for sub variable, if there are 4 dimension in available

# Define for finding the focus grid cells
my.exact.long = 101.2
my.exact.lat = 12.7
#mync.grid.long = 1 # Position for selected lontitude
#mync.grid.lat = 1 # Position for selected latitude




# Set origin day for counting date
mync.time.start.day = 1
mync.time.start.month = 1
mync.time.start.year = 1860
mync.time.calendarday = 365.25 # Day in a calendar year

L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","CMIP3","MPI-ECH5","dummy"))
dir1=dirname(file.path("D:","CMIP3","MPI-ECH5",paste(dir_name),"dummy"))

##############################
## Read NCDF file start here #

nfile = length(datafile)

### START loop to change file ###
#for(i in 1:7){
for(i in 1:nfile){

nc = open.nc(datafile[i])
if(i==1){print.nc(nc)}

# Synchronize to disk
#sync.nc(nc)

# Define dimensions
mync.dim = file.inq.nc(nc)
allvar.n = mync.dim$nvars


# Read Time
var.time = var.get.nc(nc, mync.time.id)


# Find closest grid cell
mync.grid.long = nearest(var.get.nc(nc,mync.long.id),my.exact.long)
mync.grid.lat = nearest(var.get.nc(nc,mync.lat.id),my.exact.lat)




# Read Varidable properties
# at the last position of variables
var.info = var.inq.nc(nc, allvar.n-1)
var.name = var.info$name
cat(paste("\n\nn =",i,"  Run:",runfile[i]))


## Check dimension and read data of extracted variable
# if have 3 dimension
if(var.info$ndims == 3)	{
		var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1),c(1,1,NA))
		# Convert variaable to table and put header
		var.table = as.data.frame(var.data)
		colnames(var.table) = var.name
		cat("/Dimension:3\n")
				}else	{


					# if have 4 dimension
					if(var.info$ndims == 4)	{
									subvar.n = length(var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,1,1),c(1,1,NA,1)))
									subvar.name = var.get.nc(nc, mync.subvar.id, 1,NA)
									# Loop for sub variable
									for(j in 1:subvar.n)	{
													var.data = var.get.nc(nc, allvar.n-1, c(mync.grid.long,mync.grid.lat,j,1),c(1,1,1,NA))
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
var.ts = ts(var.data, frequency=mync.time.calendarday,start=(c(mync.time.start.year+var.time[1]/mync.time.calendarday,mync.time.start.month,mync.time.start.day)))
time.table = as.data.frame(time(var.ts))
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
								output.table = var.table[,1]
								first.record = FALSE
								output.table = cbind(output.table,var.table)[,-1]
							    }else	{
									output.table = cbind(output.table,var.table)
									}
				}
				


# show progress each loop
if(extend.dat == TRUE)	{cat(paste("\n#Extended:"))}
cat(paste("\n",colnames(var.table)[-c(1)]))

# check that data still on the same run number (runfile)
if(i < nfile){
	if(runfile[i+1]!=runfile[i])	{
						first.record = TRUE # to reset output.table
						if(runfile[i]==run.list[1])	{
											if(first.table1==TRUE)	{
															table1=output.table[,1]
															table1=cbind(table1,output.table)[,-1]
															first.table1=FALSE
															}else	{
																table1=cbind(table1,output.table)
																}
											}
						if(runfile[i]==run.list[2])	{
											if(first.table2==TRUE)	{
															table2=output.table[,1]
															table2=cbind(table2,output.table)[,-1]
															first.table2=FALSE
															}else	{
																table2=cbind(table2,output.table)
																}
											}
						}
# for last file
		}else{
						if(runfile[i]==run.list[1])	{
											if(first.table1==TRUE)	{
															table1=output.table[,1]
															table1=cbind(table1,output.table)[,-1]
															first.table1=FALSE
															}else	{
																table1=cbind(table1,output.table)
																}
											}
						if(runfile[i]==run.list[2])	{
											if(first.table2==TRUE)	{
															table2=output.table[,1]
															table2=cbind(table2,output.table)[,-1]
															first.table2=FALSE
															}else	{
																table2=cbind(table2,output.table)
																}
											}
			}
			
				

close.nc(nc)
}
################
# END of loop to change file

nc = open.nc(datafile[i])

cat(paste("\n\n#################\n#### Read:",i,"file(s) /",nfile,"variables(s)\n"))
cat(paste("GRID:\n lat  =",var.get.nc(nc,mync.long.id)[mync.grid.long],"(",mync.grid.long,")\n long =",var.get.nc(nc,mync.lat.id)[mync.grid.lat],"(",mync.grid.long,")\n")) 
cat(paste("DAY/YEAR=",mync.time.calendarday,"\n start date=",time(var.ts)[1],"\n end date=",time(var.ts)[length(var.ts)], "\n total data =",length(var.ts),"\n##Varibales(s)##\n"))
cat(paste("#Table",run.list[1],"\n"))
cat(paste(colnames(output.table),"\n"))
cat(paste("#Table",run.list[2],"\n"))
cat(paste(colnames(output.table),"\n"))

close.nc(nc)

setwd(dir1)
write.table(table1, file=paste(out.file,run.list[1],".csv"), sep=',', col.names=TRUE)
write.table(table2, file=paste(out.file,run.list[2],".csv"), sep=',', col.names=TRUE)
setwd(dir0)
