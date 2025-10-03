maindir = "Temp/accessory/R Boxplot"
dir_name = "Boxplot_optimalNS-short-term"
pdf.w = 10 # width of pdf page
pdf.h = 6 # heigth of pdf page

# chart
x.title = "season"
y.title = "NS"
n.grid = 11 # number of horizontal gridlines in chart

# 1st data
data.f = "opt-short pcp.csv"

lim.y.by.maxmin = FALSE # limit by max and min value of data, otherwise define values below
ymin = -1
ymax = 1

use.fname = FALSE # use x label name by title file as defined below
f.title = "corr title.csv"



type.list.col = 1
unusedcol.data = 1
horizontal.rot = FALSE
#horizontal.rot = TRUE

# PDF file
pdffile = paste("Boxplot",data.f,".pdf",sep="")

#n.sta.1 = 1

n.row = 1
n.col = 1
#n.col = n.sta.1

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
## data
cat("\nReading 1st data")
data.org =  read.table(data.f, stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data1 = data.org[,-c(1:unusedcol.data,unusedcol2.data):dim(data.org)[2])]
data1 = data.org[,-c(1:unusedcol.data)]
rownames(data1) = data.org[,type.list.col]
station.title1 = colnames(data1)


## Axis name
type.name = data.org[,type.list.col]
type.list = rep(type.name,ncol(data1))

if(use.fname){
			fname.title  = f.title
			type.name.plot = read.table(fname.title , stringsAsFactors=FALSE,header=FALSE, sep=",",dec=".",na.strings="-")
			#type.list = rep(unlist(type.name),ncol(data1))
		}else{type.name.plot =  array(rep(unlist(type.name),ncol(data1)),c(length(type.name),ncol(data1)))}




row1.n = dim(data1)[1]
# Axis limit
bp.ylim.max = max(data1)
bp.ylim.min = min(data1)



## Plot BOXPLOT

cat("\n## Plot daily box plot")
setwd(dir1)
pdf(pdffile, width= pdf.w, height = pdf.h)
setwd(dir0)

par(ps =12,mfrow=c(n.row,n.col),mgp=c(2,0.5,0),mai = c(0.8, 0.8, .5, .5))

if(lim.y.by.maxmin){
bp.ylim.max.sta = NA
bp.ylim.min.sta = NA
}

ncol1 = ncol(data1)
nrow1 = nrow(data1)



## build data table for plotting
data1.list = matrix(NA,nrow=nrow1*ncol1,ncol=2)
for(j in 1:ncol1){
data1.list[((j-1)*nrow1+1):((j)*nrow1),2] = as.real(data1[,j])
data1.list[((j-1)*nrow1+1):((j)*nrow1),1] = unlist(type.name)
}
colnames(data1.list) = c("index","val")
setwd(dir1)
write.csv(data1.list, file="converse-table.csv",row.names=FALSE)
data.tab =  read.table("converse-table.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
setwd(dir0)
bymedian = with(data.tab, reorder(index,-val,median))

##################
#### Plot box plot
cat("\n- Plot 1st daily average box plot")
##########
##1st data
#for(i in 1:n.sta.1){
if(lim.y.by.maxmin){
bp.ylim.min.sta = min(data1)
bp.ylim.max.sta = max(data1)
}else{
bp.ylim.min.sta = ymin
bp.ylim.max.sta = ymax
}

cat("\nPlotting boxplot", data.f)

#normal box plot
boxplot.matrix(t(data1[1:row1.n,]),ylim = c(bp.ylim.min.sta,bp.ylim.max.sta), main = paste("boxplot",data.f), na.omit = TRUE, xlab = x.title, ylab = y.title, horizontal = horizontal.rot, outline= FALSE)
grid(nx=0,ny=n.grid)
abline(h=0)

boxplot(unlist(data1[1:row1.n,])~type.list,ylim = c(bp.ylim.min.sta,bp.ylim.max.sta), main = paste("box plot rank by name",data.f), na.omit = TRUE, xlab = x.title, ylab = y.title, horizontal = horizontal.rot, outline= FALSE,xaxt="n")
axis(1,1:nrow1 ,type.name.plot[order(type.name),1])
grid(nx=0,ny=n.grid)
abline(h=0)

boxplot(val~bymedian,data=data.tab,ylim = c(bp.ylim.min.sta,bp.ylim.max.sta), main = paste("box plot rank by median",data.f), na.omit = TRUE, xlab = x.title, ylab = y.title, horizontal = horizontal.rot, outline= FALSE)
grid(nx=0,ny=n.grid)
abline(h=0)

cat("\nPlotting stripchart", data.f)
stripchart(unlist(data1[1:row1.n,])~type.list,ylim = c(bp.ylim.min.sta,bp.ylim.max.sta), main = paste("strip plot rank by name",data.f), na.omit = TRUE, xlab = x.title, ylab = y.title, vertical = !horizontal.rot, outline= FALSE, pch = 4,cex = 2,xaxt="n")
axis(1,1:nrow1 ,type.name.plot[order(type.name),1])
grid(nx=0,ny=n.grid)
abline(h=0)

stripchart(val~bymedian,data=data.tab,ylim = c(bp.ylim.min.sta,bp.ylim.max.sta), main = paste("strip plot rank by median",data.f), na.omit = TRUE, xlab = x.title, ylab = y.title, vertical = !horizontal.rot, outline= FALSE, pch = 4,cex = 2)
grid(nx=0,ny=n.grid)
abline(h=0)


cat("\n-- END script --\n")
dev.off()
