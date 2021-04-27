#######################################
#Scenarios
#######################################
source("scallop_model_fun.R")

scen_str <- list()

# base model

scen_str$base <- scallop_model_fun(scenario)


# bag limit = 3
    scenario1 <- scenario
    scenario1$catch$bag <- rep(3,12)
    scen_str$scen1 <- scallop_model_fun(scenario1)

#Decrease Bag, Basically unfished scenario
    scenario2 <- scenario
    scenario2$catch$bag <- rep(0.5,12)
    scen_str$scen2 <- scallop_model_fun(scenario2)

# season length - increase in total effort, extend one month forward
    scenario3 <- scenario
    scenario3$catch$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0)
    scen_str$scen3 <- scallop_model_fun(scenario3)

# season length - same effort, extend one month forward
    scenario4 <- scenario
    scenario4$catch$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0)
    scenario4$catch$E_cap <- TRUE
    scen_str$scen4 <- scallop_model_fun(scenario4)

# season length - increase in total effort, extend one month back
    scenario5 <- scenario
    scenario5$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,1,0)
    scen_str$scen5 <- scallop_model_fun(scenario5)

# season length - same effort, extend one month back
    scenario6 <- scenario
    scenario6$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,1,0)
    scenario6$catch$E_cap <- TRUE
    scen_str$scen6 <- scallop_model_fun(scenario6)

# push season back one month
    scenario7 <- scenario
    scenario7$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
    scen_str$scen7 <- scallop_model_fun(scenario7)

# push season forward one month
    scenario8 <- scenario
    scenario8$catch$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
    scen_str$scen8 <- scallop_model_fun(scenario8)

# effort - increasing between years (doubling over ten years)
    scenario9 <- scenario
    scenario9$catch$E_years <- seq(1,2,length.out=10)
    scen_str$scen9 <- scallop_model_fun(scenario9)

# effort - increasing between years (tripling over 10 years)
    scenario10 <- scenario
    scenario10$catch$E_years <- seq(1,3,length.out=10)
    scen_str$scen10 <- scallop_model_fun(scenario10)

# rolling bag limit - increasing
    scenario11 <- scenario
    scenario11$catch$bag <- c(1,1,1,1,1,1,1,2,2,2,2,2)
    scen_str$scen11 <- scallop_model_fun(scenario11)

# rolling bag and season start - back one month
    scenario12 <- scenario
    scenario12$catch$bag <- c(1,1,1,1,1,1,1,1,2,2,2,2)
    scenario12$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
    scen_str$scen12 <- scallop_model_fun(scenario12)

# semelparity
    scenario13 <- scenario
    scenario13$life$semelparous <- TRUE
    scen_str$scen13 <- scallop_model_fun(scenario13)

##################
# sensitivity runs
##################
    sens_str <- list()

perchange <- 0.25 # +/- 25%
# changes in catchability - look at hcpue and pr_hr
    senscen_q1 <- scenario
    senscen_q2 <- scenario
    senscen_q1$catch$q <- scenario$catch$q*(1-perchange)
    senscen_q2$catch$q <- scenario$catch$q*(1+perchange)
    sens_str$senq1 <- scallop_model_fun(senscen_q1)
    sens_str$senq2 <- scallop_model_fun(senscen_q2)

# M
    senscen_M1 <- scenario
    senscen_M2 <- scenario
    senscen_M1$life$M <- scenario$life$M*(1-perchange)
    senscen_M2$life$M <- scenario$life$M*(1+perchange)
    sens_str$senM1 <- scallop_model_fun(senscen_M1)
    sens_str$senM2 <- scallop_model_fun(senscen_M2)

# vbk
    senscen_vbk1 <- scenario
    senscen_vbk2 <- scenario
    senscen_vbk1$life$vbk <- scenario$life$vbk*(1-perchange)
    senscen_vbk2$life$vbk <- scenario$life$vbk*(1+perchange)
    sens_str$senvbk1 <- scallop_model_fun(senscen_vbk1)
    sens_str$senvbk2 <- scallop_model_fun(senscen_vbk2)

# CR
    senscen_CR1 <- scenario
    senscen_CR2 <- scenario
    senscen_CR1$life$CR <- scenario$life$CR*(1-perchange)
    senscen_CR2$life$CR <- scenario$life$CR*(1+perchange)
    sens_str$senCR1 <- scallop_model_fun(senscen_CR1)
    sens_str$senCR2 <- scallop_model_fun(senscen_CR2)

# amat?
    senscen_amat1 <- scenario
    senscen_amat2 <- scenario
    senscen_amat1$life$amat <- scenario$life$amat*(1-perchange)
    senscen_amat2$life$amat <- scenario$life$amat*(1+perchange)
    sens_str$senamat1 <- scallop_model_fun(senscen_amat1)
    sens_str$senamat2 <- scallop_model_fun(senscen_amat2)


##################################
## run mgmt scenarios with low vs high q too
    sens_q_low_str <- list()
    scenario1$catch$q <- scenario$catch$q*(1-perchange)
    scenario2$catch$q <- scenario$catch$q*(1-perchange)
    scenario3$catch$q <- scenario$catch$q*(1-perchange)
    scenario4$catch$q <- scenario$catch$q*(1-perchange)
    scenario5$catch$q <- scenario$catch$q*(1-perchange)
    scenario6$catch$q <- scenario$catch$q*(1-perchange)
    scenario7$catch$q <- scenario$catch$q*(1-perchange)
    scenario8$catch$q <- scenario$catch$q*(1-perchange)
    scenario9$catch$q <- scenario$catch$q*(1-perchange)
    scenario10$catch$q <- scenario$catch$q*(1-perchange)
    scenario11$catch$q <- scenario$catch$q*(1-perchange)
    scenario12$catch$q <- scenario$catch$q*(1-perchange)
    scenario13$catch$q <- scenario$catch$q*(1-perchange)

    sens_q_low_str$scen1 <- scallop_model_fun(scenario1)
    sens_q_low_str$scen2 <- scallop_model_fun(scenario2)
    sens_q_low_str$scen3 <- scallop_model_fun(scenario3)
    sens_q_low_str$scen4 <- scallop_model_fun(scenario4)
    sens_q_low_str$scen5 <- scallop_model_fun(scenario5)
    sens_q_low_str$scen6 <- scallop_model_fun(scenario6)
    sens_q_low_str$scen7 <- scallop_model_fun(scenario7)
    sens_q_low_str$scen8 <- scallop_model_fun(scenario8)
    sens_q_low_str$scen9 <- scallop_model_fun(scenario9)
    sens_q_low_str$scen10 <- scallop_model_fun(scenario10)
    sens_q_low_str$scen11 <- scallop_model_fun(scenario11)
    sens_q_low_str$scen12 <- scallop_model_fun(scenario12)
    sens_q_low_str$scen13 <- scallop_model_fun(scenario13)


    sens_q_high_str <- list()
    scenario1$catch$q <- scenario$catch$q*(1+perchange)
    scenario2$catch$q <- scenario$catch$q*(1+perchange)
    scenario3$catch$q <- scenario$catch$q*(1+perchange)
    scenario4$catch$q <- scenario$catch$q*(1+perchange)
    scenario5$catch$q <- scenario$catch$q*(1+perchange)
    scenario6$catch$q <- scenario$catch$q*(1+perchange)
    scenario7$catch$q <- scenario$catch$q*(1+perchange)
    scenario8$catch$q <- scenario$catch$q*(1+perchange)
    scenario9$catch$q <- scenario$catch$q*(1+perchange)
    scenario10$catch$q <- scenario$catch$q*(1+perchange)
    scenario11$catch$q <- scenario$catch$q*(1+perchange)
    scenario12$catch$q <- scenario$catch$q*(1+perchange)
    scenario13$catch$q <- scenario$catch$q*(1+perchange)

    sens_q_high_str$scen1 <- scallop_model_fun(scenario1)
    sens_q_high_str$scen2 <- scallop_model_fun(scenario2)
    sens_q_high_str$scen3 <- scallop_model_fun(scenario3)
    sens_q_high_str$scen4 <- scallop_model_fun(scenario4)
    sens_q_high_str$scen5 <- scallop_model_fun(scenario5)
    sens_q_high_str$scen6 <- scallop_model_fun(scenario6)
    sens_q_high_str$scen7 <- scallop_model_fun(scenario7)
    sens_q_high_str$scen8 <- scallop_model_fun(scenario8)
    sens_q_high_str$scen9 <- scallop_model_fun(scenario9)
    sens_q_high_str$scen10 <- scallop_model_fun(scenario10)
    sens_q_high_str$scen11 <- scallop_model_fun(scenario11)
    sens_q_high_str$scen12 <- scallop_model_fun(scenario12)
    sens_q_high_str$scen13 <- scallop_model_fun(scenario13)


##################################
# save all scenarios to one .RData
    save(scen_str, sens_str, sens_q_low_str, sens_q_high_str,
     file = "scenario_sensitivity_storage.RData")

##################################
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
##################################
    #grab the global ranges for dynamic settings
    vb.r <- range(sapply(scen_str,function(x)range(x$results$VB,na.rm=T)))
    et.r <- range(sapply(scen_str,function(x)range(x$results$et,na.rm=T)))
    yield.r <- range(sapply(scen_str,function(x)range(x$results$yield_n,na.rm=T)))
    rec.r <- range(sapply(scen_str,function(x)range(x$results$recruits,na.rm=T)))
    eggs.r <- range(sapply(scen_str,function(x)range(x$results$eggs_mon,na.rm=T)))


    #plot scenarios
    for(i in 1:length(scen_str)){
        if(i==1){
            pdf(file=paste0("./Scenario_Figures/scenario_base_results.pdf"))
        }else{
            pdf(file=paste0("./Scenario_Figures/scenario_",
                        formatC(i-1,width=2,flag="0"),
                        "_results.pdf"))
        }
        
            with(scen_str[[i]]$results,{
                N <- nrows(scen_str[[i]]$results)
              par(mfrow=c(3,2), mar=c(2,3.5,1,1), las=1, mgp=c(2.5,0.5,0), tck=-0.015)
              #plot 1
                vb.sci <- sci.transl(vb.r)
                plot(time, VB/vb.sci$scale, type="l", col="blue",
                     ylab=paste0("Vul. Biomass (",vb.sci$label,")"),
                     lwd=3, ylim=vb.sci$r)
                abline(lm(VB/vb.sci$scale~time), lwd=2, lty=1)
                abline(v=seq(0.5,N+0.5,12), lty=2, col='gray70')
              #plot 2
                plot(time, N/N[1], type='l', ylab = "Depletion (prop. Numbers)", ylim=c(0,1), lwd=2, col="mediumpurple4")
                abline(v=seq(0.5,N+0.5,12), lty=2, col='gray70')
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
                abline(v=seq(0.5,N+0.5,12), lty=2, col='gray70')
              #plot 4
                yield.sci <- sci.transl(yield.r)
                yield2 <- yield_n
                yield2[yield_n==0] <- NA
                plot(time, yield2/yield.sci$scale, 
                     type='l', pch=16, lwd=3,
                     ylab = paste0("Catch (numbers; ",yield.sci$label,")"),
                      ylim=yield.sci$r, col="salmon3", cex=1.5)
                abline(v=seq(0.5,N+0.5,12), lty=2, col='gray70')
                points(time, yield2/yield.sci$scale,col="salmon3",pch=16)
              #plot 5
                rec.sci <- sci.transl(rec.r)
                plot(time[recruits>0], recruits[recruits>0]/rec.sci$scale, type='b', pch=16, 
                     ylab = paste0("Recruits (",rec.sci$label,")"), 
                     ylim=rec.sci$r, col='darkgreen', cex=1.5)
              #plot 6
                eggs.sci <- sci.transl(eggs.r)
                plot(time, eggs_mon/eggs.sci$scale, type='l', 
                     ylab = paste0("Eggs (Monthly; ", eggs.sci$label,")"), 
                     ylim=eggs.sci$r, lwd=2, col="seagreen")
                abline(v=seq(0.5,N+0.5,12), lty=2, col='gray70')
            })
        dev.off()
    }




###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# rm(list=setdiff(ls(),list_name)))


## Run all scenarios - one list ####
# base
# windows()
# with(base$results,{
#         par(mfrow=c(3,2), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
#         plot(time, VB, type="l", col="blue",ylab="Vul Biomass",lwd=3, ylim=c(0, max(VB)))
#         abline(lm(VB~time), lwd=2, lty=3)
#         plot(time, N/N[1], type='l', ylab = "Depletion (Numbers)", ylim=c(0,1), lwd=2, col="mediumpurple4")
#         et2 <- et
#         et2[et2==0] <- NA
#         plot(time, et2, col="red", lwd=3, type='l',ylab="Effort (Vessels)", ylim=c(0,50000))
#         plot(time[yield_n>0], yield_n[yield_n>0], type='p', pch=16,ylab = "Catch (numbers)", ylim=c(0,110000), col="salmon3", cex=1.5, xlim=c(0,120))
#         abline(v=seq(0.5,120.5,12), lty=2)
#         plot(time[recruits>0], recruits[recruits>0], type='p', pch=16, ylab = "Recruits", xlim=c(0,120), ylim=c(0,1.35e6), col='darkgreen', cex=1.5)
#         plot(time, eggs_mon, type='l', ylab = "Eggs (Monthly)", ylim=c(0,100000), lwd=2, col="seagreen")
# })

# # Scenarios
# with(scen5$results,{
#   par(mfrow=c(3,2), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
#   plot(time, VB, type="l", col="blue",ylab="Vul Biomass",lwd=3, ylim=c(0, max(VB)))
#   abline(lm(VB~time), lwd=2, lty=3)
#   plot(time, N/N[1], type='l', ylab = "Depletion (Numbers)", ylim=c(0,1), lwd=2, col="mediumpurple4")
#   et2 <- et
#   et2[et2==0] <- NA
# #  plot(time, et2, col="red", lwd=3, type='l',ylab="Effort (Vessels)", ylim=c(0,50000))
#   plot(time, et2, col="red", lwd=3, type='l',ylab="Effort (Vessels)", ylim=c(0,95000))
#   plot(time[yield_n>0], yield_n[yield_n>0], type='p', pch=16,ylab = "Catch (numbers)", ylim=c(0,110000), col="salmon3", cex=1.5, xlim=c(0,120))
#   abline(v=seq(0.5,120.5,12), lty=2)
#   plot(time[recruits>0], recruits[recruits>0], type='p', pch=16, ylab = "Recruits", xlim=c(0,120), ylim=c(0,1.35e6), col='darkgreen', cex=1.5)
#   plot(time, eggs_mon, type='l', ylab = "Eggs (Monthly)", ylim=c(0,100000), lwd=2, col="seagreen")
# })


# bar plots or dot plot (mean and variance) - SPR (eggs - five years/eggs0) and hcpue (avg across open months) - last five years 
# pr_hr?