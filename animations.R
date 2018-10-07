setwd("/dropbox/research/demographicforecasting/")
require(demography)
nyears <- length(fr.mort$year)

# LATEX VERSION
for(i in 1:nyears)
{
    pdf(paste("figs/frmale",i,".pdf",sep=""))
    x <- fr.mort
    if(i<nyears)
        x$rate$male[,(i+1):nyears] <- NA
    plot(x,series="male",ylim=c(-9.5,1.5),main=paste("French: male mortality (",fr.mort$year[1]-1+i,")",sep=""))
    if(i>1)
        x$rate$male[,1:(i-1)] <- NA
    lines(x,series='male',lwd=2,col=1)
    dev.off()
}

# HTML VERSION

oopt = ani.options(interval = 0.1, nmax = nyears, ani.dev = png,
    ani.type = "png",outdir="/Rob", loop=FALSE,
	title = "French mortality rates", description = "")
ani.start()
opar = par(mar = c(3, 3, 2, 0.5), mgp = c(2, .5, 0), tcl = -0.3,
    cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
for(i in 1:nyears)
{
    x <- fr.mort
    if(i<nyears)
        x$rate$male[,(i+1):nyears] <- NA
    plot(x,series="male",ylim=c(-9.5,1.5),main=paste("French: male mortality (",fr.mort$year[1]-1+i,")",sep=""))
    if(i>1)
        x$rate$male[,1:(i-1)] <- NA
    lines(x,series='male',lwd=2,col=1)
}
par(opar)
ani.stop()
ani.options(oopt)

## GIF VERSION
makeplot <- function(){
for(i in 1:nyears)
{
    x <- fr.mort
    if(i<nyears)
        x$rate$male[,(i+1):nyears] <- NA
    plot(x,series="male",ylim=c(-9.5,1.5),main=paste("French: male mortality (",fr.mort$year[1]-1+i,")",sep=""))
    if(i>1)
        x$rate$male[,1:(i-1)] <- NA
    lines(x,series='male',lwd=2,col=1)
}
}
oopt = ani.options(interval = 0, nmax = nyears,loop=FALSE)
saveMovie(makeplot(),interval = 0.1, outdir = "/rob", width = 580, height = 400)
ani.options(oopt)

# PDF VERSION
library(animation)
library(demography)
nyears <- length(fr.mort$year)
for(i in 1:nyears)
{
    pdf(paste("frmale",i,".pdf",sep=""),height=4,width=6.5)
    x <- fr.mort
    if(i<nyears)
        x$rate$male[,(i+1):nyears] <- NA
    plot(x,series="male",ylim=c(-9.5,1.5),main=paste("French: male mortality (",fr.mort$year[1]-1+i,")",sep=""))
    if(i>1)
        x$rate$male[,1:(i-1)] <- NA
    lines(x,series='male',lwd=2,col=1)
    dev.off()
}
