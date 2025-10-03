library(RNetCDF)


## specify source file here
fname ="psl_A2_a42_0108-0147.nc"
## specify output file here
oname ="setio-out.csv"
## specify output folder here
dir_name ="ncdf"

L=file.exists(dir_name)  
if(!L) dirfile=dir.create(dir_name) else dirfile=(dir_name)
dir0=dirname(file.path("D:","Temp","ncdf",paste(dirfile)))
dir1=dirname(file.path("D:","Temp","ncdf",paste(dirfile),"dummy"))


data = open.ncdf(fname)
out = get.var.ncdf(data)

## define the time dimension
dim = data$dim$TIME$vals

setwd(dir1)
sink(oname)

cat("n,value\n")
	for( j in 1 : length(out) ) {
	cat(dim[j],",",out[j],"\n")
}

sink()
close.ncdf(data)
setwd(dir0)

