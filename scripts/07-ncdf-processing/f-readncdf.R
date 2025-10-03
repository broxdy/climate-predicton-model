library(RNetCDF)
library(chron)
library(GenKern)

## Function to read ncdf file
read.ncdf <- function(f.nc) { 
nc = open.nc(f.nc)
print.nc(nc)
# check all id
allvar.n = file.inq.nc(nc)$nvars
# Print all ID
for(check.id in 0:(allvar.n-1)){cat(paste(check.id,")",var.inq.nc(nc,check.id)$name,"\n"))}
#close.nc(nc)
} 
###
