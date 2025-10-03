clear <- function(){
setwd(dir0)
while(dev.cur() != 1) dev.off()
while(sink.number() != 0) sink(file=NULL)
print(dir0)
}