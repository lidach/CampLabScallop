#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#     Code for plots
#   
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

###-----------------------------------------------------
#   Life history plot
###-----------------------------------------------------
# 4x4 Life history plot
# length at age, weight at age, maturity at age, and natural mortality at age
  LH_plot <- function(scenario){
    age.seq.sm <- seq(0.1,18, by=0.1)
    age.seq <- seq(0,18)
    age.fn <- function(age.seq, scenario){
        x <- data.frame(TL = scenario$life$vblinf*(1-exp(-scenario$life$vbk*(age.seq-scenario$life$vbt0))),
                        Mat = 1/(1+exp(-scenario$life$mgr*(age.seq-scenario$life$amat))))
        x$Wt <- (scenario$life$alw*(x$TL)^scenario$life$alwb)
        x$Ma<-(scenario$life$M*(0.5*scenario$life$vblinf)/x$TL)^scenario$life$lorenzc
        x$Vul <- 1/(1+exp(-scenario$life$vgr*(x$TL-scenario$life$vlh)))
        return(x)
    }
    lh <- age.fn(age.seq, scenario)
    lh.sm <- age.fn(age.seq.sm, scenario)
    fig.lab <- function(i, xscale=0.05, yscale=0.95, cex=1.4, adj=c(0.5,0.5),...){
        text(x = par('usr')[1] + abs(diff(par('usr')[1:2]))*xscale,
             y = par('usr')[3] + abs(diff(par('usr')[3:4]))*yscale,
             ifelse(is.numeric(i),LETTERS[i],i), xpd=NA, cex=cex, adj=adj,...)
    }

    par(mfrow=c(3,2), mar=c(1,3,0,0.5), oma=c(2,0.5,1,0),
        tck=-0.02, mgp=c(2,0.5,0))
      # Length at Age
      plot(age.seq, lh$TL, ylab="", xaxt="n", las=1, 
           pch=16, cex=1.2, xlim=c(0.5,18.1))
      lines(age.seq.sm, lh.sm$TL)
      axis(1, at=pretty(age.seq), labels=FALSE)
      mtext(text="Shell Height (mm)", side=2, line=2, font=1)
      fig.lab(1, xscale=0.05, yscale=0.91,font=2)
      # Weight at Age
      plot(age.seq, lh$Wt, ylab="", xaxt ="n", las=1, pch=16, cex=1.2, xlim=c(0.5,18.1))
      lines(age.seq.sm, lh.sm$Wt)
      axis(1, at=pretty(age.seq), labels=FALSE)
      mtext(text="Weight (g)", side=2, line=2, font=1)
      fig.lab(2, xscale=0.05, yscale=0.91,font=2)
      # Selectivity
      plot(age.seq, lh$Vul, ylab="", xaxt="n", las=1,pch=16, cex=1.2, ylim=c(0,1),xlim=c(0.5,18.1))
      lines(age.seq.sm, lh.sm$Vul)
      mtext(text="Selectivity", side=2, line=2, font=1)
      fig.lab(3, xscale=0.05, yscale=0.91,font=2)
      # Maturity 
      plot(age.seq, lh$Mat, ylab="", xlab="", las=1, pch=16, cex=1.2, ylim=c(0,1), xlim=c(0.5,18.1))
      lines(age.seq.sm, lh.sm$Mat)
      mtext(text="Proportion Mature", side=2, line=2, font=1)
      fig.lab(4, xscale=0.05, yscale=0.91,font=2)
      # Natural mortality at Age
      plot(age.seq, lh$Ma, ylab="", xlab="", las=1,pch=16, cex=1.2, ylim=c(0,1),xlim=c(0.5,18.1))
      lines(age.seq.sm, lh.sm$Ma)
      mtext(text="Natural Mortality", side=2, line=2, font=1)
      fig.lab(5, xscale=0.095, yscale=0.91,font=2)
      mtext(text="Age (months)", side=1, line=1.75, at=20, font=2)
  }



###-----------------------------------------------------
#   Time series plot
###-----------------------------------------------------
# this function helps do scientific notation without the e+XX stuff
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

# time series plot
# 2x3 plot of results
# vulnerable biomass, depletion (in numbers), effort, yield, recruits, and eggs
  time_series_plot <- function(res){
    with(res$results,{
      # grabbing the global ranges for dynamic settings (for the sci.transl function)
      vb.r <- range(VB, na.rm=TRUE)
      et.r <- range(et, na.rm=TRUE)
      yield.r <- range(yield_n, na.rm=TRUE)
      rec.r <- range(recruits, na.rm=TRUE)
      eggs.r <- range(eggs_mon, na.rm=TRUE)

      Nt <- length(N)
      par(mfrow=c(3,2), mar=c(2,3.5,1,1), las=1, mgp=c(2.5,0.5,0), tck=-0.015)

      # plot 1 - Vulnerable biomass
      vb.sci <- sci.transl(vb.r)
      plot(time, VB/vb.sci$scale, type="l", col="blue",
           ylab=paste0("Vul. Biomass (",vb.sci$label,")"),
           lwd=3, ylim=vb.sci$r)
      abline(lm(VB/vb.sci$scale~time), lwd=2, lty=1)
      abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
      
      # plot 2 - Depletion
      plot(time, N/N[1], type='l', ylab = "Depletion (prop. Numbers)", ylim=c(0,1), lwd=2, col="mediumpurple4")
      abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
      
      # plot 3 - Effort
      et2 <- et
      et2[et2==0] <- NA
      et.sci <- sci.transl(et.r)
      plot(time, et2/et.sci$scale, col="red", 
           lwd=3, type='l',
           ylab=paste0("Effort (Persons; ", et.sci$label,")"), 
               ylim=et.sci$r)
      points(time, et2/et.sci$scale, col='red', pch=16)
      abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
     
      # plot 4 - Yield
      yield.sci <- sci.transl(yield.r)
      yield2 <- yield_n
      yield2[yield_n==0] <- NA
      plot(time, yield2/yield.sci$scale, 
           type='l', pch=16, lwd=3,
           ylab = paste0("Catch (numbers; ",yield.sci$label,")"),
            ylim=yield.sci$r, col="salmon3", cex=1.5)
      abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
      points(time, yield2/yield.sci$scale,col="salmon3",pch=16)
      
      # plot 5 - Recruits 
      rec.sci <- sci.transl(rec.r)
      plot(time[recruits>0], recruits[recruits>0]/rec.sci$scale, type='b', pch=16, 
           ylab = paste0("Recruits (",rec.sci$label,")"), 
           ylim=rec.sci$r, col='darkgreen', cex=1.5)
      
      # plot 6 - Eggs
      par(mar=c(2,3.5,1,3.8), las=1, mgp=c(2.5,0.5,0), tck=-0.015)
      eggs.sci <- sci.transl(eggs.r)
      plot(time, eggs_mon/eggs.sci$scale, type='l', 
           ylab = paste0("Eggs (Monthly; ", eggs.sci$label,")"), 
           ylim=eggs.sci$r, lwd=2, col="seagreen")
      abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
      par(new=T)
       plot(time[eggs>0], eggs[eggs>0]/sum(res$life$Ro * res$life.vec$Lo *  res$life.vec$Mat * res$life.vec$Fec *  res$life.vec$prob_spawn), 
        type="l", ylim=c(0,1) ,ylab="", xlab="", xaxt="n", yaxt="n")
       points(time[eggs>0], eggs[eggs>0]/sum(res$life$Ro * res$life.vec$Lo *  res$life.vec$Mat * res$life.vec$Fec *  res$life.vec$prob_spawn), pch = 16)
       axis(side=4, at=seq(0,1,0.2), labels=seq(0,1,0.2), las=1)
       mtext(text="Depletion (Eggs Deposited)", side=4,line=2.5,las=0, outer=F, cex=0.7)
    })
  }
