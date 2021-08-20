################################################
#Scallop Figures Code (Figures for Final MS)
################################################

########################################
#4x4 Life History plot
########################################

source("scallop_model_fun.R")

#Remember to switch file name to your path to Figures folder in Dropbox
# path_name <- "C:/Users/nfisch/Dropbox/Lab Scallops/Scallop MS/Figures"

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
load("./Results_RData/semel_res_plots.Rdata")
# scen_names1 <- c("incr. bag", "incr. szn. (back, more eff.)" , "incr. szn. (back)", "incr. szn. (front, more eff.)", "incr. szn. (front)",
#                 "later szn.", "earlier szn.", "rolling bag", "rolling bag later szn.", "rolling bag earlier szn.")
scen_names2 <- c("base", "incr. bag", "decr. bag", "incr. szn. (later, more eff.)" , "incr. szn. (later)", "incr. szn. (earlier, more eff.)", "incr. szn. (earlier)",
                "later szn.", "earlier szn.", "rolling bag", "rolling bag later szn.", "rolling bag earlier szn.")
require(RColorBrewer)
col <- colorRampPalette(brewer.pal(9,"Blues"))(length(scen_names2))
m <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = TRUE)
SPR_names <- c("SPR = 63%", "SPR = 50%", "SPR = 35%")
Et_names <- c("base effort", NA,NA,"double effort",NA,NA, "triple effort")

# path_name <- getwd()
## absolute values
with(bar_res,{
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hr_abs) <- NULL

    tiff(filename=paste0(path_name,"/1_abs_depl_semel_eff.tiff"), height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
    layout(mat = m, heights = c(0.2,0.2,0.4))
    for(i in 1:9){
      if(i == 1) par(mar=c(0.1,3.2,2.2,0))
      if(i %in% c(2,3)) par(mar = c(0.1,1,2.2,1))
      if(i == 4) par(mar = c(0.1,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(0.1,1,1,1))
      if(i %in% 7) par(mar = c(11,3.2,1,0))
      if(i %in% c(8,9)) par(mar = c(11,1,1,1))

      if(i %in% c(1:6)) barplot(depl_abs[i,c(1,3:13)], axes = FALSE, ylim = c(0, max(depl_abs[,c(1,3:13)])+0.1), col = col) # removed "unfished" scenario for now
      if(i %in% c(7,8,9)){
        b <- barplot(depl_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(depl_abs[,c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names2, srt=60, adj=c(1,0),xpd=NA)
      }
      # mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6, col = "grey60")
      if(i %in% c(1,4,7)){
        axis(2, at = seq(floor(min(depl_abs[,c(1,3:13)])),ceiling(max(depl_abs[,c(1,3:13)])),0.5))
        title(ylab = Et_names[i], line = 2)
      }
      if(i %in% c(1,2,3)) title(main = SPR_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "Depletion (in eggs)", line = 1.55)
    }
    # par(mar = c(0,0,0,0))
    # plot(1, type ="n", axes = FALSE, xlab ="", ylab ="")
    # legend(0.6,0.8, legend = scen_names[legend_order], fill = col[legend_order], cex = 1, bty = "n", ncol = 1)
    dev.off()

    tiff(filename=paste0(path_name,"/1_abs_hcpue_semel_eff.tiff"),height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
    layout(mat = m, heights = c(0.2,0.2,0.4))
    for(i in 1:9){
      if(i == 1) par(mar=c(0.1,3.2,2.2,0))
      if(i %in% c(2,3)) par(mar = c(0.1,1,2.2,1))
      if(i == 4) par(mar = c(0.1,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(0.1,1,1,1))
      if(i %in% 7) par(mar = c(11,3.2,1,0))
      if(i %in% c(8,9)) par(mar = c(11,1,1,1))

      if(i %in% c(1:6))barplot(hcpue_abs[i,c(1,3:13)], axes = FALSE, ylim = c(0, max(hcpue_abs[,c(1,3:13)])+0.1), col = col) # removed "unfished" scenario for now
      if(i %in% c(7,8,9)){
        b <- barplot(hcpue_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(hcpue_abs[,c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names2, srt=60, adj=c(1,0),xpd=NA)
      }
      # mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6, col = "grey60")
      if(i %in% c(1,4,7)){
        axis(2, at = seq(floor(min(hcpue_abs[,c(1,3:13)])),ceiling(max(hcpue_abs[,c(1,3:13)])),0.5))
        title(ylab = Et_names[i], line = 2)
      }
      if(i %in% c(1,2,3)) title(main = SPR_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "Catch rate", line = 1.55)
     }
    # par(mar = c(0,0,0,0))
    # plot(1, type ="n", axes = FALSE, xlab ="", ylab ="")
    # legend(0.6,0.8, legend = scen_names[legend_order], fill = col[legend_order], cex = 1, bty = "n", ncol = 1)
    dev.off()

    tiff(filename=paste0(path_name,"/1_abs_hr_semel_eff.tiff"),height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
    layout(mat = m, heights = c(0.2,0.2,0.4))
    for(i in 1:9){
      if(i == 1) par(mar=c(0.1,3.2,2.2,0))
      if(i %in% c(2,3)) par(mar = c(0.1,1,2.2,1))
      if(i == 4) par(mar = c(0.1,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(0.1,1,1,1))
      if(i %in% 7) par(mar = c(11,3.2,1,0))
      if(i %in% c(8,9)) par(mar = c(11,1,1,1))

      if(i %in% c(1:6))barplot(hr_abs[i,c(1,3:13)], axes = FALSE, ylim = c(0, max(hr_abs[,c(1,3:13)])+0.1), col = col) # removed "unfished" scenario for now
      if(i %in% c(7,8,9)){
        b <- barplot(hr_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(hr_abs[,c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names2, srt=60, adj=c(1,0),xpd=NA)
      }
      # mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6, col = "grey60")
      if(i %in% c(1,4,7)){
        axis(2, at = seq(floor(min(hr_abs[,c(1,3:13)])),ceiling(max(hr_abs[,c(1,3:13)])),0.2))
        title(ylab = Et_names[i], line = 2)
      }
      if(i %in% c(1,2,3)) title(main = SPR_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "harvest rate", line = 1.55)
     }
    # par(mar = c(0,0,0,0))
    # plot(1, type ="n", axes = FALSE, xlab ="", ylab ="")
    # legend(0.6,0.8, legend = scen_names[legend_order], fill = col[legend_order], cex = 1, bty = "n", ncol = 1)
    dev.off()
})

## single effort
with(bar_res,{
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hr_abs) <- NULL

    tiff(filename=paste0(path_name,"/1_abs_depl_semel.tiff"), height = 10, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow = c(1,3))
    for(i in 1:3){
      if(i == 1) par(mar=c(11,3,3,0.05))
      if(i %in% c(2,3)) par(mar = c(11,0.6,3,0.6))
        b <- barplot(depl_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(depl_abs[c(1:3),c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
        scen_names2, srt=60, adj=c(1,0),xpd=NA)
      title(main = SPR_names[i], font.main = 1, line = -.1, cex = 0.8)
      if(i == 2) title(main = "Depletion (in eggs)", line = 1.55)
      if(i == 1) axis(2, at = seq(floor(min(depl_abs[c(1:3),c(1,3:13)])),ceiling(max(depl_abs[c(1:3),c(1,3:13)])),0.5))
    }
     dev.off()

    tiff(filename=paste0(path_name,"/1_abs_hcpue_semel.tiff"),height = 10, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow = c(1,3))
    for(i in 1:3){
      if(i == 1) par(mar=c(11,3,3,0.05))
      if(i %in% c(2,3)) par(mar = c(11,0.6,3,0.6))
        b <- barplot(hcpue_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0,1.4), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
        scen_names2, srt=60, adj=c(1,0),xpd=NA)
      title(main = SPR_names[i], font.main = 1, line = -.1, cex = 0.8)
      if(i == 2) title(main = "Catch rate", line = 1.55)
      if(i == 1) axis(2, at = seq(floor(min(hcpue_abs[c(1:3),c(1,3:13)])),1.6,0.5))
    }
     dev.off()   

    tiff(filename=paste0(path_name,"/1_abs_hr_semel.tiff"),height = 10, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow = c(1,3))
    for(i in 1:3){
      if(i == 1) par(mar=c(11,3,3,0.05))
      if(i %in% c(2,3)) par(mar = c(11,0.6,3,0.6))
        b <- barplot(hr_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(hr_abs[c(1:3),c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
        scen_names2, srt=60, adj=c(1,0),xpd=NA)
      title(main = SPR_names[i], font.main = 1, line = -.1, cex = 0.8)
      if(i == 2) title(main = "Harvest rate", line = 1.55)
      if(i == 1) axis(2, at = seq(floor(min(hr_abs[c(1:3),c(1,3:13)])),ceiling(max(hr_abs[c(1:3),c(1,3:13)])),0.2))
    }
    dev.off()
})



## iteroparous (appendix)
load("./Results_RData/itero_res_plots.Rdata")
# scen_names1 <- c("incr. bag", "incr. szn. (back, more eff.)" , "incr. szn. (back)", "incr. szn. (front, more eff.)", "incr. szn. (front)",
#                 "later szn.", "earlier szn.", "rolling bag", "rolling bag later szn.", "rolling bag earlier szn.")
require(RColorBrewer)
col <- colorRampPalette(brewer.pal(9,"Blues"))(length(scen_names2))
m <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = TRUE)
SPR_names <- c("SPR = 63%", "SPR = 50%", "SPR = 35%")
Et_names <- c("base effort", NA,NA,"double effort",NA,NA, "triple effort")

path_name <- getwd()
## absolute values
with(bar_res,{
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hr_abs) <- NULL

    tiff(filename=paste0(path_name,"/1_abs_depl_itero_eff.tiff"), height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
    layout(mat = m, heights = c(0.2,0.2,0.4))
    for(i in 1:9){
      if(i == 1) par(mar=c(0.1,3.2,2.2,0))
      if(i %in% c(2,3)) par(mar = c(0.1,1,2.2,1))
      if(i == 4) par(mar = c(0.1,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(0.1,1,1,1))
      if(i %in% 7) par(mar = c(11,3.2,1,0))
      if(i %in% c(8,9)) par(mar = c(11,1,1,1))

      if(i %in% c(1:6)) barplot(depl_abs[i,c(1,3:13)], axes = FALSE, ylim = c(0, max(depl_abs[,c(1,3:13)])+0.1), col = col) # removed "unfished" scenario for now
      if(i %in% c(7,8,9)){
        b <- barplot(depl_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(depl_abs[,c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names2, srt=60, adj=c(1,0),xpd=NA)
      }
      # mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6, col = "grey60")
      if(i %in% c(1,4,7)){
        axis(2, at = seq(floor(min(depl_abs[,c(1,3:13)])),ceiling(max(depl_abs[,c(1,3:13)])),0.5))
        title(ylab = Et_names[i], line = 2)
      }
      if(i %in% c(1,2,3)) title(main = SPR_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "Depletion (in eggs)", line = 1.55)
    }
    # par(mar = c(0,0,0,0))
    # plot(1, type ="n", axes = FALSE, xlab ="", ylab ="")
    # legend(0.6,0.8, legend = scen_names[legend_order], fill = col[legend_order], cex = 1, bty = "n", ncol = 1)
    dev.off()

    tiff(filename=paste0(path_name,"/1_abs_hcpue_itero_eff.tiff"),height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
    layout(mat = m, heights = c(0.2,0.2,0.4))
    for(i in 1:9){
      if(i == 1) par(mar=c(0.1,3.2,2.2,0))
      if(i %in% c(2,3)) par(mar = c(0.1,1,2.2,1))
      if(i == 4) par(mar = c(0.1,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(0.1,1,1,1))
      if(i %in% 7) par(mar = c(11,3.2,1,0))
      if(i %in% c(8,9)) par(mar = c(11,1,1,1))

      if(i %in% c(1:6))barplot(hcpue_abs[i,c(1,3:13)], axes = FALSE, ylim = c(0, max(hcpue_abs[,c(1,3:13)])+0.1), col = col) # removed "unfished" scenario for now
      if(i %in% c(7,8,9)){
        b <- barplot(hcpue_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(hcpue_abs[,c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names2, srt=60, adj=c(1,0),xpd=NA)
      }
      # mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6, col = "grey60")
      if(i %in% c(1,4,7)){
        axis(2, at = seq(floor(min(hcpue_abs[,c(1,3:13)])),ceiling(max(hcpue_abs[,c(1,3:13)])),0.5))
        title(ylab = Et_names[i], line = 2)
      }
      if(i %in% c(1,2,3)) title(main = SPR_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "Catch rate", line = 1.55)
     }
    # par(mar = c(0,0,0,0))
    # plot(1, type ="n", axes = FALSE, xlab ="", ylab ="")
    # legend(0.6,0.8, legend = scen_names[legend_order], fill = col[legend_order], cex = 1, bty = "n", ncol = 1)
    dev.off()

    tiff(filename=paste0(path_name,"/1_abs_hr_itero_eff.tiff"),height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
    layout(mat = m, heights = c(0.2,0.2,0.4))
    for(i in 1:9){
      if(i == 1) par(mar=c(0.1,3.2,2.2,0))
      if(i %in% c(2,3)) par(mar = c(0.1,1,2.2,1))
      if(i == 4) par(mar = c(0.1,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(0.1,1,1,1))
      if(i %in% 7) par(mar = c(11,3.2,1,0))
      if(i %in% c(8,9)) par(mar = c(11,1,1,1))

      if(i %in% c(1:6))barplot(hr_abs[i,c(1,3:13)], axes = FALSE, ylim = c(0, max(hr_abs[,c(1,3:13)])+0.1), col = col) # removed "unfished" scenario for now
      if(i %in% c(7,8,9)){
        b <- barplot(hr_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(hr_abs[,c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names2, srt=60, adj=c(1,0),xpd=NA)
      }
      # mtext(letters[i], side = 3, line = -1, adj = 0.1, cex = 0.6, col = "grey60")
      if(i %in% c(1,4,7)){
        axis(2, at = seq(floor(min(hr_abs[,c(1,3:13)])),ceiling(max(hr_abs[,c(1,3:13)])),0.2))
        title(ylab = Et_names[i], line = 2)
      }
      if(i %in% c(1,2,3)) title(main = SPR_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "harvest rate", line = 1.55)
     }
    # par(mar = c(0,0,0,0))
    # plot(1, type ="n", axes = FALSE, xlab ="", ylab ="")
    # legend(0.6,0.8, legend = scen_names[legend_order], fill = col[legend_order], cex = 1, bty = "n", ncol = 1)
    dev.off()
})

## single effort
with(bar_res,{
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hr_abs) <- NULL

    tiff(filename=paste0(path_name,"/1_abs_depl_itero.tiff"), height = 10, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow = c(1,3))
    for(i in 1:3){
      if(i == 1) par(mar=c(11,3,3,0.05))
      if(i %in% c(2,3)) par(mar = c(11,0.6,3,0.6))
        b <- barplot(depl_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(depl_abs[c(1:3),c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
        scen_names2, srt=60, adj=c(1,0),xpd=NA)
      title(main = SPR_names[i], font.main = 1, line = -.1, cex = 0.8)
      if(i == 2) title(main = "Depletion (in eggs)", line = 1.55)
      if(i == 1) axis(2, at = seq(floor(min(depl_abs[c(1:3),c(1,3:13)])),ceiling(max(depl_abs[c(1:3),c(1,3:13)])),0.5))
    }
     dev.off()

    tiff(filename=paste0(path_name,"/1_abs_hcpue_itero.tiff"),height = 10, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow = c(1,3))
    for(i in 1:3){
      if(i == 1) par(mar=c(11,3,3,0.05))
      if(i %in% c(2,3)) par(mar = c(11,0.6,3,0.6))
        b <- barplot(hcpue_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0,1.4), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
        scen_names2, srt=60, adj=c(1,0),xpd=NA)
      title(main = SPR_names[i], font.main = 1, line = -.1, cex = 0.8)
      if(i == 2) title(main = "Catch rate", line = 1.55)
      if(i == 1) axis(2, at = seq(floor(min(hcpue_abs[c(1:3),c(1,3:13)])),1.6,0.5))
    }
     dev.off()   

    tiff(filename=paste0(path_name,"/1_abs_hr_itero.tiff"),height = 10, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow = c(1,3))
    for(i in 1:3){
      if(i == 1) par(mar=c(11,3,3,0.05))
      if(i %in% c(2,3)) par(mar = c(11,0.6,3,0.6))
        b <- barplot(hr_abs[i,c(1,3:13)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(hr_abs[c(1:3),c(1,3:13)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
        scen_names2, srt=60, adj=c(1,0),xpd=NA)
      title(main = SPR_names[i], font.main = 1, line = -.1, cex = 0.8)
      if(i == 2) title(main = "Harvest rate", line = 1.55)
      if(i == 1) axis(2, at = seq(floor(min(hr_abs[c(1:3),c(1,3:13)])),ceiling(max(hr_abs[c(1:3),c(1,3:13)])),0.2))
    }
    dev.off()
})



## base result time series
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
scen_str <- scen_str_all[[1]]
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