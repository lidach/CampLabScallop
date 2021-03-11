#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#			
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

###-----------------------------------------------------
#		Initialization
###-----------------------------------------------------
	library(lubridate)
	library(glmmTMB)
###-----------------------------------------------------
#		Data Read In
###-----------------------------------------------------
	path <- ""
	eff <- read.csv(paste0(path,"Scallop_EffDy.csv"), as.is=T)
	catch <- read.csv(paste0(path,"Scallop_CatchDy.csv"), as.is=T)
###-----------------------------------------------------
#		Data Cleaning
###-----------------------------------------------------
	#convert date
	eff$date <- mdy(eff$Date)
	catch$date <- mdy(catch$Date)
	#add weekday ID
	eff$wday <- as.factor(wday(eff$date,label=TRUE))
	catch$wday <- wday(catch$date,label=TRUE)
	#add difftime
	eff$t <- as.integer(difftime(eff$date,min(eff$date)-1, units='days'))
	catch$t <- as.integer(difftime(catch$date,min(catch$date)-1, units='days'))
###-----------------------------------------------------
#		Model
###-----------------------------------------------------
	mod.eff <- glmmTMB(log(CVC)~t+(1|wday), data=eff)
	summary(mod.eff)

	mod.catch <- glmmTMB(log(Catch)~poly(t,3), data=catch)
	summary(mod.catch)
###-----------------------------------------------------
#		Predictions
###-----------------------------------------------------
	eff.pred <- expand.grid(t=seq(1:90),
	                        wday=levels(eff$wday))
	catch.pred <- data.frame(t=seq(1:90))
	catch.pred$month <- rep(1:3,each=30)

	eff.pred$mu <- predict(mod.eff, newdata=eff.pred, 
	                       allow.new.levels=TRUE)

	eff.agg.t <- aggregate(mu ~ t, eff.pred, mean)
	eff.agg.t$month <- rep(1:3,each=30)
	eff.agg <- aggregate(exp(mu) ~ month, eff.agg.t, sum)

	catch.pred$mu <- predict(mod.catch, newdata=catch.pred)

	catch.agg <- aggregate(mu~month, catch.pred, mean)
	catch.agg$Catch <- exp(catch.agg$mu)
###-----------------------------------------------------
#		Figures
###-----------------------------------------------------
	par(mfrow=c(2,1), mar=c(4,4,1,1), las=1)
	with(eff,plot(log(CVC)~t, pch=16))
	lines(eff$t, predict(mod.eff), col='red', lwd=2)

	with(catch,plot(log(Catch)~t, pch=16))
	lines(catch$t, predict(mod.catch), col='red', lwd=2)