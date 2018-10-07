## PLOTS FOR SSA ARTICLE

require(monash)
require(addb)

factor <- 2.58

savepng("ssa/frenchmales2")
plot(fr.sm,"male",main="France: male death rates (1900-2006)")
text(30,-1,"War years")
arrows(30,-1.5,30,-2.5,length=0.04)
dev.off()


savepng("ssa/fbag")
fboxplot(fts(0:100,log(fr.sm$rate$male[1:101,])),factor=factor,xlab="Age",ylab="Log death rate",lwd=2)
title("Functional bagplot of French death rates")
dev.off()

fit <- fdm(fr.sm,'male')

savepng("ssa/mortmodel")
par(oma=c(0,0,2,0),xpd=NA)
plot(fit,mean.lab="mu(x)",ylab1="phi",ylab2="beta",comp=2,main.title="",interaction.title="")
title("Components from model for French mortality",line=2)
dev.off()


fit <- fdm(fr.sm,method="M",series='male',max.age=110)
# Remove outliers
outliers <- is.element(fr.sm$year,foutliers(fts(0:100,log(fr.sm$rate$male[1:101,])))$outliers)
fit$weights <- !outliers
fcast <- forecast(fit,h=20)

savepng("ssa/mortfcast2d")
plot(fr.sm,series='male',col="gray",main="France: male death forecasts (2007 & 2026)",ylim=c(-10,0),xlim=c(0,100))
polygon(c(0:110,110:0),log(c(fcast$rate$lower[,20],rev(fcast$rate$upper[,20]))),col=rgb(.91,.72,1),border=FALSE)
polygon(c(0:110,110:0),log(c(fcast$rate$lower[,1],rev(fcast$rate$upper[,1]))),col=rgb(.51,.32,1),border=FALSE)
lines(fcast,years=c(2007,2026),col=c(4,2),lwd=2)
text(50,-10,"80% prediction intervals")
dev.off()


# Forecasting Australian fertility

savepng("ssa/ausfert")
plot(aus.fertility,ylim=c(10,260))
dev.off()

fert.sm <- smooth.demogdata(aus.fertility,obs.var="theoretical")
fit <- fdm(fert.sm)
fcast <- forecast(fit,h=20)

savepng("ssa/fertfcast2d")
plot(fert.sm,col="gray",transform=FALSE,main="Forecasts of Australian fertility: 2007 and 2006")
fcast$rate$upper[35,] <- 0
polygon(c(15:49,49:15),c(fcast$rate$lower[,20],rev(fcast$rate$upper[,20])),col=rgb(.91,.72,1),border=FALSE)
polygon(c(15:49,49:15),c(fcast$rate$lower[,1],rev(fcast$rate$upper[,1])),col=rgb(.51,.32,1),border=FALSE)
lines(fcast,years=c(2007,2026),col=c(4,2),lwd=2,transform=FALSE)
text(40,250,"80% prediction intervals")
dev.off()


#GROUP FORECASTING

#Extract the data we want to use (1950-2004, max age=100)
aus <- hmd.mx("AUS","Rob.Hyndman@buseco.monash.edu.au","ezekiel;","Australia")
aus <- extract.years(extract.ages(aus, 0:100, combine.upper=FALSE), 1950:max(aus$year))

#Smooth data
mort.sm <- smooth.demogdata(aus,obs.var='theoretical')

#Forecasts from independent models for each sex
aus.ind <- fdm.ind(mort.sm)
aus.ind.f <- forecast(aus.ind,h=20)
#Fit coherent model and use it for forecasting
aus.pr <- coherentfdm(mort.sm)
aus.pr.f <- forecast(aus.pr,h=20)

savepng("ssa/ratiof_aus")
par(mfrow=c(1,2))
plot(sex.ratio(mort.sm),ylab="Sex ratio of rates: M/F",main="Independent forecasts",ylim=c(0.8,4.1),col="gray")
lines(sex.ratio(aus.ind.f),col=rainbow(24))
plot(sex.ratio(mort.sm),ylab="Sex ratio of rates: M/F",main="Coherent forecasts",ylim=c(0.8,4.1),col="gray")
lines(sex.ratio(aus.pr.f),col=rainbow(24))
dev.off()


# Net Migration
# Ignore data prior to 1973 due to poor quality
aus.mig <- extract.years(netmigration(aus,aus.fertility,mfratio=1.052742), 1973:2020)
mig.sm <- smooth.demogdata(aus.mig,method="spline",k=20,power=.4)
rm(w) # Created in smooth.demogdata


################ MODELLING STEP ################
# MORTALITY
mort.fit <- coherentfdm(extract.years(mort.sm,1900:2006))
mortf <- forecast(mort.fit,h=20)

# FERTILITY
fert.fit <- fdm(fert.sm)
fertf <- forecast(fert.fit,h=20)

# NET MIGRATION
mig.fit <- coherentfdm(mig.sm)
migf <- forecast(mig.fit,h=20)


############### SIMULATION STEP #################
aus.sim <- pop.sim(mortf,fertf,migf,aus,mfratio=1.0545,N=1000)

## Means and intervals
popm.mean <- apply(aus.sim$male,c(1,2),mean)
popm.lo <- apply(aus.sim$male,c(1,2),quantile,p=.1)
popm.hi <- apply(aus.sim$male,c(1,2),quantile,p=.9)
popf.mean <- apply(aus.sim$female,c(1,2),mean)
popf.lo <- apply(aus.sim$female,c(1,2),quantile,p=.1)
popf.hi <- apply(aus.sim$female,c(1,2),quantile,p=.9)

migf.q <- apply(aus.mig$rate$female,1,quantile,p=c(0.025,.1,.5,.9,.975),na.rm=TRUE)
migm.q <- apply(aus.mig$rate$male,1,quantile,p=c(0.025,.1,.5,.9,.975),na.rm=TRUE)
migf.mean <- apply(aus.mig$rate$female,1,mean,na.rm=TRUE)
migm.mean <- apply(aus.mig$rate$male,1,mean,na.rm=TRUE)

## TOTAL POPULATION COMPARISONS
totfpop <- apply(aus.sim$female,c(2,3),sum)
totmpop <- apply(aus.sim$male,c(2,3),sum)
totfpop.q <- apply(totfpop,1,quantile,p=c(0.025,.1,.5,.9,.975),na.rm=TRUE)/1e6
totmpop.q <- apply(totmpop,1,quantile,p=c(0.025,.1,.5,.9,.975),na.rm=TRUE)/1e6
totauspop <- ts(colSums(australia$pop$total)/1e6,s=1921)


savepng("ssa/pyramid")
par(mar=c(4.3,3.6,3.0,3.3))
plot(c(0,100),c(0,100),type="n", main="Forecast population: 2026",yaxt="n",
    xlab="Population ('000)",ylab="Age",xaxt="n",xlim=c(-1,1)*max(popf.hi/1000))
mtext("Male                       Female")
abline(v=0)
axis(1,at=seq(-150,0,by=50),labels=rev(seq(0,150,by=50)))
axis(1,at=seq(50,150,by=50))
axis(2,at=seq(0,100,by=10),labels=seq(0,100,by=10))
axis(4,at=seq(0,100,by=10),labels=seq(0,100,by=10))
axis(2,at=seq(0,100,by=20),labels=seq(0,100,by=20))
axis(4,at=seq(0,100,by=20),labels=seq(0,100,by=20))
axis(4,at=50,line=1.5,labels="Age",col=2,tick=FALSE)
polygon(c(-popm.lo[,20],rev(-popm.hi[,20]))/1000,c(0:100,100:0),col="yellow",border=FALSE)
polygon(c(popf.lo[,20],rev(popf.hi[,20]))/1000,c(0:100,100:0),col="yellow",border=FALSE)
lines(-aus$pop$male[,ncol(aus$pop$male)]/1000,0:100,col=1,lty=2)
lines(aus$pop$female[,ncol(aus$pop$female)]/1000,0:100,col=1,lty=2)
lines(-popm.mean[,20]/1000,0:100,col="blue",lwd=2)
lines(popf.mean[,20]/1000,0:100,col="blue",lwd=2)
dev.off()



# Old age dependency ratio
oadp <- function(x)
{
    workers <- colSums(extract.ages(x,15:64,FALSE)$pop$total,na.rm=TRUE)
    aged <- colSums(extract.ages(x,65:100,TRUE)$pop$total,na.rm=TRUE)
    return(ts(aged/workers,s=x$year[1],f=1))
}

male.workers <- apply(aus.sim$male[16:65,,],c(2,3),sum)
female.workers <- apply(aus.sim$female[16:65,,],c(2,3),sum)
male.aged <- apply(aus.sim$male[66:101,,],c(2,3),sum)
female.aged <- apply(aus.sim$female[66:101,,],c(2,3),sum)

oadp.f <- (male.aged+female.aged)/(male.workers+female.workers)
oadp.lo <- apply(oadp.f,1,quantile,prob=0.1)
oadp.hi <- apply(oadp.f,1,quantile,prob=0.9)
oadp.f <- structure(list(mean=ts(rowMeans(oadp.f),s=2007),x=ts(oadp(aus),s=1950),
    upper=ts(oadp.hi,s=2007),lower=ts(oadp.lo,s=2007),
    level=80),class="forecast")

savepng("ssa/oadp")
plot(oadp.f,main="Old-age dependency ratio forecasts",xlab="Year",ylab="ratio")
dev.off()
