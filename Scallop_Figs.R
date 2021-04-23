################################################
#Scallop Figures Code (Figures for Final MS)
################################################

########################################
#4x4 Life History plot
########################################

source("scallop_model_fun.R")

#Remember to switch file name to your path to Figures folder in Dropbox
#tiff(filename="C:/Users/nfisch/Dropbox/Lab Scallops/Scallop MS/Figures/4x4LH.tiff",height = 17, width = 20, units = 'cm', compression = "lzw", res = 500)
par(mfrow=c(2,2), mar=c(2,4,1,1), oma=c(3,3,1,1))
TL<-scenario$life$vblinf*(1-exp(-scenario$life$vbk*(1:18-scenario$life$vbt0)))
plot(1:18, TL, ylab="", xaxt="n", las=1, pch=16, cex=1.2, ylim=c(0,70), xlim=c(0.5,18.5))
mtext(text="Total Length (mm)", side=2, line=2.5, font=1)
Wt<-(scenario$life$alw*(TL)^scenario$life$alwb)
plot(1:18, Wt, ylab="", xaxt="n", las=1, pch=16, cex=1.2, ylim=c(0,30), xlim=c(0.5,18.5))
mtext(text="Weight (g)", side=2, line=2.5, font=1)
Mat<-1/(1+exp(-(1:18-scenario$life$amat)/scenario$life$msd))
plot(1:18, Mat, ylab="", xlab="", las=1, pch=16, cex=1.2, ylim=c(0,1), xlim=c(0.5,18.5))
mtext(text="Proportion Mature", side=2, line=2.5, font=1)
Ma<-(scenario$life$M*(0.5*scenario$life$vblinf)/TL)^scenario$life$lorenzc
plot(1:18, Ma, ylab="", xlab="", las=1,pch=16, cex=1.2, ylim=c(0,0.4), xlim=c(0.5,18.5))
mtext(text="Natural Mortality", side=2, line=2.5, font=1)
mtext(text="Age", side=1, line=3, at=-4, font=2)
#dev.off()