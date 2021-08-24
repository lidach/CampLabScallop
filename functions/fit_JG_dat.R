#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#			
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Author: Zach Siders
#Incept: 2020-Dec-12

###-----------------------------------------------------
#		Initialization
###-----------------------------------------------------
	library(lubridate)
	library(glmmTMB)

###-----------------------------------------------------
#		Data Read In
###-----------------------------------------------------
	# wd <- "" # set your working directory here
	setwd(wd)
	path <- paste0(wd, "/data/")
	eff <- read.csv(paste0(path,"Scallop_EffDy.csv"), as.is=T)
	catch <- read.csv(paste0(path,"Scallop_CatchDy.csv"), as.is=T)

###-----------------------------------------------------
#		growth
###-----------------------------------------------------
	vbk = 4/12
    vblinf = 65
    vbt0 = 0
    TL <- vblinf*(1-exp(-vbk*(seq(1,12)-vbt0)))
    sh2gal <- function(sh){
      (-6.704*sh + 480.96)/2
    }
    npgal <- sh2gal(TL)
    
###-----------------------------------------------------
#		Data Cleaning
###-----------------------------------------------------
	# convert date
	eff$date <- mdy(eff$Date)
	catch$date <- mdy(catch$Date)
	# add weekday ID
	eff$wday <- as.factor(wday(eff$date,label=TRUE))
	catch$wday <- wday(catch$date,label=TRUE)
	# add difftime
	eff$t <- as.integer(difftime(eff$date,min(eff$date)-1, units='days'))
	catch$t <- as.integer(difftime(catch$date,min(catch$date)-1, units='days'))

###-----------------------------------------------------
#		Add gallons
###-----------------------------------------------------
	catch$size <- vblinf*(1-exp(-vbk*(julian(catch$date, origin=as.Date('2018-01-01'))/365*12-vbt0)))
	catch$numpergal <- sh2gal(catch$size)
	catch$gallons <- catch$Catch/catch$numpergal

###-----------------------------------------------------
#		Model
###-----------------------------------------------------
	mod.eff <- glmmTMB(log(CVC)~t+(1|wday), data=eff)
	summary(mod.eff)

	mod.catch <- glmmTMB(log(Catch)~poly(t,3), data=catch)
	summary(mod.catch)

	mod.gallons <- glmmTMB(log(gallons)~poly(t,3), data=catch)
	summary(mod.gallons)

###-----------------------------------------------------
#		Predictions
###-----------------------------------------------------
	eff.pred <- expand.grid(t=seq(0:89),
	                        wday=levels(eff$wday))
	catch.pred <- data.frame(t=seq(0:89))
	catch.pred$month <- rep(1:3,each=30)

	eff.pred$mu <- predict(mod.eff, newdata=eff.pred, 
	                       allow.new.levels=TRUE)

	eff.agg.t <- aggregate(mu ~ t, eff.pred, mean)
	eff.agg.t$month <- rep(1:3,each=30)
	eff.agg <- aggregate(exp(mu) ~ month, eff.agg.t, sum)

	catch.pred$mu <- predict(mod.catch, newdata=catch.pred)
	catch.pred$mu.gal <- predict(mod.gallons, newdata=catch.pred)

	catch.agg <- aggregate(cbind(mu,mu.gal)~month, catch.pred, mean)
	catch.agg$Catch <- exp(catch.agg$mu)
	catch.agg$gallons <- exp(catch.agg$mu.gal)

###-----------------------------------------------------
#		Figures
###-----------------------------------------------------
	t.seq <- eff$date[1]+days(0:89)
	t.ax.seq <- seq(min(t.seq),max(t.seq),by="week")+1
	par(mfrow=c(2,1), mar=c(0.5,4,1,0.5), las=1, oma=c(3.5,0,0,0))
	with(eff,plot(CVC~t, pch=16, 
	              ylab="Vessels",xaxt='n',xlab="",
	              ylim=c(100,1200)))
	lines(eff$t, exp(predict(mod.eff)), col='red', lwd=2)
	axis(1,as.numeric(difftime(t.ax.seq,min(t.seq))),
	     labels=FALSE, tck=-0.02)

	with(catch,plot(Catch~t, pch=16, 
	                ylab="Scallops/Vessel",xaxt='n',xlab=""))
	lines(catch$t, exp(predict(mod.catch)), col='red', lwd=2)
	legend('bottomleft', legend=c('Obs.','Est.'),
	       pch=c(16,NA), lwd=c(NA,5),
	       col=c('black','red'), bty='n')
	axis(1,as.numeric(difftime(t.ax.seq,min(t.seq))),
	     labels=FALSE, tck=-0.02)
	text(as.numeric(difftime(t.ax.seq,min(t.seq))),
	     par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
	     substr(t.ax.seq,6,10), srt=45, adj=c(1,0.5), xpd=NA)
	mtext("Date", side=1, line=3)

    catch.agg$npg <- npgal[6:8]
    catch.agg$gallons <- catch.agg$Catch/catch.agg$npg
