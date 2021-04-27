#######################################
#Scenarios
#######################################
source("scallop_model_fun.R")

# base model
base <- scallop_model_fun(scenario)

# bag limit = 3
    scenario1 <- scenario
    scenario1$catch$bag <- rep(3,12)
    scen1 <- scallop_model_fun(scenario1)

#Decrease Bag, Basically unfished scenario
    scenario2 <- scenario
    scenario2$catch$bag <- rep(0.5,12)
    scen2 <- scallop_model_fun(scenario2)

# season length - increase in total effort, extend one month forward
    scenario3 <- scenario
    scenario3$catch$bag <- c(2,2,2,2,2,2,2,2,2,2,2,2)
    # scenario3$catch$effort <- c(0,0,0,0,0,0,10169.55,5649.75,4067.82,2711.88,0,0)*3.8
    scen3 <- scallop_model_fun(scenario3)

# season length - same effort, extend one month forward
    scenario4 <- scenario
    # scenario3$catch$effort <- c(0,0,0,0,0,0,10169.55,5649.75,4067.82,2711.88,0,0)*3.8
    scen4 <- scallop_model_fun(scenario4)

# season length - increase in total effort, extend one month back
    scenario5 <- scenario
    # scenario3$catch$effort <- c(0,0,0,0,0,10169.55,5649.75,4067.82,2711.88,0,0,0)*3.8
    scen5 <- scallop_model_fun(scenario5)

# season length - same effort, extend one month back
    scenario6 <- scenario
    # scenario3$catch$effort <- c(0,0,0,0,0,10169.55,5649.75,4067.82,2711.88,0,0,0)*3.8
    scen6 <- scallop_model_fun(scenario6)

# push season back one month
    scenario7 <- scenario
    scenario7$catch$effort <- c(0,0,0,0,0,11415,6929,4255,0,0,0,0)*3.8
    scen7 <- scallop_model_fun(scenario7)

# push season forward one month
    scenario8 <- scenario
    scenario8$catch$effort <- c(0,0,0,0,0,0,0,11415,6929,4255,0,0)*3.8
    scen8 <- scallop_model_fun(scenario8)

# effort - increasing between years (doubling over ten years)
    scenario9 <- scenario
    scenario9$catch$e_years <- seq(1,2,length.out=10)
    scen9 <- scallop_model_fun(scenario9)

# effort - increasing between years (tripling over 10 years)
    scenario10 <- scenario
    scenario10$catch$e_years <- seq(1,3,length.out=10)
    scen10 <- scallop_model_fun(scenario10)

# rolling bag limit - increasing
    scenario11 <- scenario
    scenario11$catch$bag <- c(1,1,1,1,1,1,1,2,2,2,2,2)
    scen11 <- scallop_model_fun(scenario11)

# rolling bag and season start - back one month
    scenario12 <- scenario
    scenario12$catch$bag <- c(1,1,1,1,1,1,1,1,2,2,2,2)
    scenario12$catch$effort <- c(0,0,0,0,0,0,0,11415,6929,4255,0,0)*3.8
    scen12 <- scallop_model_fun(scenario12)

# semelparity
    scenario13 <- scenario
    #
    scen13 <- scallop_model_fun(scenario13)

##################
# sensitivity runs
##################
perchange <- 0.25 # +/- 25%
# changes in catchability - look at hcpue and pr_hr
    senscen_q1 <- scenario
    senscen_q2 <- scenario
    senscen_q1 <- scenario$catch$q*(1-perchange)
    senscen_q2 <- scenario$catch$q*(1+perchange)
    senq1 <- scallop_model_fun(senscen_q1)
    senq2 <- scallop_model_fun(senscen_q2)

# M
    senscen_M1 <- scenario
    senscen_M2 <- scenario
    senscen_M1 <- scenario$life$M*(1-perchange)
    senscen_M2 <- scenario$life$M*(1+perchange)
    senM1 <- scallop_model_fun(senscen_M1)
    senM2 <- scallop_model_fun(senscen_M2)

# vbk
    senscen_vbk1 <- scenario
    senscen_vbk2 <- scenario
    senscen_vbk1 <- scenario$life$vbk*(1-0.25)
    senscen_vbk2 <- scenario$life$vbk*(1+0.25)
    senvbk1 <- scallop_model_fun(senscen_vbk1)
    senvbk2 <- scallop_model_fun(senscen_vbk2)

# CR
    senscen_CR1 <- scenario
    senscen_CR2 <- scenario
    senscen_CR1 <- scenario$life$CR*(1-0.25)
    senscen_CR2 <- scenario$life$CR*(1+0.25)
    senCR1 <- scallop_model_fun(senscen_CR1)
    senCR2 <- scallop_model_fun(senscen_CR2)

# amat?
    senscen_amat1 <- scenario
    senscen_amat2 <- scenario
    senscen_amat1 <- scenario$life$amat*(1-0.25)
    senscen_amat2 <- scenario$life$amat*(1+0.25)
    senamat1 <- scallop_model_fun(senscen_amat1)
    senamat2 <- scallop_model_fun(senscen_amat2)


##################################
## run mgmt scenarios with low vs high q too



##################################
# save all scenarios to one .RData

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