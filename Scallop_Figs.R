################################################
#Scallop Figures Code (Figures for Final MS)
################################################

########################################
#4x4 Life History plot
########################################

source("scallop_model_fun.R")

#Remember to switch file name to your path to Figures folder in Dropbox
path_name <- "C:/Users/nfisch/Dropbox/Lab Scallops/Scallop MS/Figures"

#tiff(filename=paste0(path_name,"/4x4LH.tiff"),height = 17, width = 20, units = 'cm', compression = "lzw", res = 500)
par(mfrow=c(2,2), mar=c(2,4,1,1), oma=c(3,3,1,1))
TL<-scenario$life$vblinf*(1-exp(-scenario$life$vbk*(1:18-scenario$life$vbt0)))
plot(1:18, TL, ylab="", xaxt="n", las=1, pch=16, cex=1.2, ylim=c(0,70), xlim=c(0.5,18.5))
mtext(text="Total Length (mm)", side=2, line=2.5, font=1)
Wt<-(scenario$life$alw*(TL)^scenario$life$alwb)
plot(1:18, Wt, ylab="", xaxt="n", las=1, pch=16, cex=1.2, ylim=c(0,30), xlim=c(0.5,18.5))
mtext(text="Weight (g)", side=2, line=2.5, font=1)
Mat<-1/(1+exp(-scenario$life$mgr*(1:18-scenario$life$amat)))
plot(1:18, Mat, ylab="", xlab="", las=1, pch=16, cex=1.2, ylim=c(0,1), xlim=c(0.5,18.5))
mtext(text="Proportion Mature", side=2, line=2.5, font=1)
Ma<-(scenario$life$M*(0.5*scenario$life$vblinf)/TL)^scenario$life$lorenzc
plot(1:18, Ma, ylab="", xlab="", las=1,pch=16, cex=1.2, ylim=c(0,0.4), xlim=c(0.5,18.5))
mtext(text="Natural Mortality", side=2, line=2.5, font=1)
mtext(text="Age", side=1, line=3, at=-4, font=2)
#dev.off()



########################################
# Results
########################################
# relative change bar plots
# names for x lab for each scenario
load("./Results_RData/rel_change_plots.Rdata")
scen_names <- c("bag = 3", "incr. eff., +1 \nseason back", "same eff., +1 \nseason back", "incr., eff. +1 \nseason front", "same eff., +1 \nseason front",
                "season \nforward", "season back", "double eff.", "triple eff.", "rolling bag", "rolling bag \n+season back", "semelparity")
col <- c("#009999", "#0000FF","#104E8B")

with(bar_res,{
	# SPR
    # tiff(filename=paste0(path_name,"/1_rel_errors_SPR.tiff"),height = 14, width = 25, units = 'cm', compression = "lzw", res = 500)
    par(mar=c(6,3.5,1,7), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
    barplot(Eggs[,c(1,3:13)], beside = TRUE, ylim = c(min(Eggs[,c(1,3:13)])-0.1, max(Eggs[,c(1,3:13)])+0.1), las = 2, col = col, ylab = "Relative change", main = "Eggs/Unfished management scenarios", names = scen_names) # removed "unfished" scenario for now
    legend("right", inset = c(-0.15,0), legend = c("SPR = 74%", "SPR = 50%", "SPR = 35%"), fill = col, box.lty = 0)
    # dev.off()
	
	# hcpue
    # tiff(filename="C:/Users/nfisch/Dropbox/Lab Scallops/Scallop MS/Figures/1_rel_errors_hcpue.tiff",height = 14, width = 25, units = 'cm', compression = "lzw", res = 500)
    par(mar=c(6,3.5,1,7), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
    barplot(hcpue[,c(1,3:13)], beside = TRUE, ylim = c(min(hcpue[,c(1,3:13)])-0.1, max(hcpue[,c(1,3:13)])+0.1), las = 2, col = col, ylab = "Relative change", main = "Catch per unit of effort management scenarios", names = scen_names) # removed "unfished" scenario for now
    legend("right", inset = c(-0.15,0), legend = c("SPR = 74%", "SPR = 50%", "SPR = 35%"), fill = col, box.lty = 0)
    # dev.off()
	
	# hr
    # tiff(filename=paste0(path_name,"/1_rel_errors_hr.tiff"),height = 14, width = 25, units = 'cm', compression = "lzw", res = 500)
    par(mar=c(6,3.5,1,7), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
    barplot(hr[,c(1,3:13)], beside = TRUE, ylim = c(min(hr[,c(1,3:13)])-0.1, max(hr[,c(1,3:13)])+0.1), las = 2, col = col, ylab = "Relative change", main = "Harvest rate management scenarios", names = scen_names) # removed "unfished" scenario for now
    legend("right", inset = c(-0.15,0), legend = c("SPR = 74%", "SPR = 50%", "SPR = 35%"), fill = col, box.lty = 0)
    # dev.off()
})

# base result time series
#this function helps do scientific notation without the e+XX stuff
sci.transl <- function(range){
    x <- gsub("^[[:graph:]]+e\\+","",formatC(max(range),format='g'))
    v <- switch(x,
           '01'="10's",
           '02'="100's",
           '03'="1000's",
           '04'="10's of Thousands",
           '05'="100's of Thousands",
           '06'="Millions",
           '07'="10's of Millions",
           '08'="100's of Millions",
           '09'="Billions")
    return(list(scale = as.numeric(paste0('1e',x)),
                label = v,
                r = range/as.numeric(paste0('1e',x))))
}

load("./Results_RData/scenario_sensitivity_storage.Rdata")
#grab the global ranges for dynamic settings
vb.r <- range(sapply(scen_str,function(x)range(x$results$VB,na.rm=T)))
et.r <- range(sapply(scen_str,function(x)range(x$results$et,na.rm=T)))
yield.r <- range(sapply(scen_str,function(x)range(x$results$yield_n,na.rm=T)))
rec.r <- range(sapply(scen_str,function(x)range(x$results$recruits,na.rm=T)))
eggs.r <- range(sapply(scen_str,function(x)range(x$results$eggs_mon,na.rm=T)))


# tiff(filename="C:/Users/nfisch/Dropbox/Lab Scallops/Scallop MS/Figures/scenario_base_results.tiff",height = 17, width = 23, units = 'cm', compression = "lzw", res = 500)
with(scen_str[[1]]$results,{
    Nt <- nrow(scen_str[[1]]$results)
  par(mfrow=c(3,2), mar=c(2,3.5,1,1), las=1, mgp=c(2.5,0.5,0), tck=-0.015)
  #plot 1
    vb.sci <- sci.transl(vb.r)
    plot(time, VB/vb.sci$scale, type="l", col="blue",
         ylab=paste0("Vul. Biomass (",vb.sci$label,")"),
         lwd=3, ylim=vb.sci$r)
    abline(lm(VB/vb.sci$scale~time), lwd=2, lty=1)
    abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
  #plot 2
    plot(time, N/N[1], type='l', ylab = "Depletion (prop. Numbers)", ylim=c(0,1), lwd=2, col="mediumpurple4")
    abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
  #plot 3
    et2 <- et
    et2[et2==0] <- NA
    #  plot(time, et2, col="red", lwd=3, type='l',ylab="Effort (Vessels)", ylim=c(0,50000))
    et.sci <- sci.transl(et.r)
    plot(time, et2/et.sci$scale, col="red", 
         lwd=3, type='l',
         ylab=paste0("Effort (Persons; ", et.sci$label,")"), 
             ylim=et.sci$r)
    points(time, et2/et.sci$scale, col='red', pch=16)
    abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
  #plot 4
    yield.sci <- sci.transl(yield.r)
    yield2 <- yield_n
    yield2[yield_n==0] <- NA
    plot(time, yield2/yield.sci$scale, 
         type='l', pch=16, lwd=3,
         ylab = paste0("Catch (numbers; ",yield.sci$label,")"),
          ylim=yield.sci$r, col="salmon3", cex=1.5)
    abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
    points(time, yield2/yield.sci$scale,col="salmon3",pch=16)
  #plot 5
    rec.sci <- sci.transl(rec.r)
    plot(time[recruits>0], recruits[recruits>0]/rec.sci$scale, type='b', pch=16, 
         ylab = paste0("Recruits (",rec.sci$label,")"), 
         ylim=rec.sci$r, col='darkgreen', cex=1.5)
  #plot 6
    par(mar=c(2,3.5,1,3.8), las=1, mgp=c(2.5,0.5,0), tck=-0.015)
    eggs.sci <- sci.transl(eggs.r)
    plot(time, eggs_mon/eggs.sci$scale, type='l', 
         ylab = paste0("Eggs (Monthly; ", eggs.sci$label,")"), 
         ylim=eggs.sci$r, lwd=2, col="seagreen")
    abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
    par(new=T)
	   plot(time[eggs>0], eggs[eggs>0]/sum(scen_str[[1]]$scenario$life$Ro * scen_str[[1]]$scenario$life.vec$Lo *  scen_str[[1]]$scenario$life.vec$Mat * scen_str[[1]]$scenario$life.vec$Fec *  scen_str[[1]]$scenario$life.vec$prob_spawn), 
	   	type="l", ylim=c(0,1) ,ylab="", xlab="", xaxt="n", yaxt="n")
	   points(time[eggs>0], eggs[eggs>0]/sum(scen_str[[1]]$scenario$life$Ro * scen_str[[1]]$scenario$life.vec$Lo *  scen_str[[1]]$scenario$life.vec$Mat * scen_str[[1]]$scenario$life.vec$Fec *  scen_str[[1]]$scenario$life.vec$prob_spawn), pch = 16)
	   axis(side=4, at=seq(0,1,0.2), labels=seq(0,1,0.2), las=1)
	   mtext(text="Depletion (Eggs Deposited)", side=4,line=2.5,las=0, outer=F, cex=0.7)
})
# dev.ofF()