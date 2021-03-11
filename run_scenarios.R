source("scallop_model_fun.R")

base <- scallop_model_fun(scenario)
with(base$results,{
        par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        abline(lm(VB~time), lwd=2, lty=3)
        et2 <- et
        et2[et2==0] <- NA
        plot(time, et2, col="red", lwd=3, type='l', 
             ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
        plot(time, recruits, type='l')
    })

# fishing mortality
# scenario1 <- scenario
# scenario1$catch$FM_flag <- "q"
# scen1 <- scallop_model_fun(scenario1)
# with(scen1$results,{
#         par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
#         plot(time, VB, type="l", col="blue", 
#              lwd=3, ylim=c(0, max(VB)))
#         abline(lm(VB~time), lwd=2, lty=3)
#         et2 <- et
#         et2[et2==0] <- NA
#         plot(time, et2, col="red", lwd=3, type='l', 
#              ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
#         plot(time, recruits, type='l')
#     })

# catchability - VB
scenario1 <- scenario
scenario1$catch$q_flag <- "VB"
scen1 <- scallop_model_fun(scenario1)
with(scen1$results,{
        par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        abline(lm(VB~time), lwd=2, lty=3)
        et2 <- et
        et2[et2==0] <- NA
        plot(time, et2, col="red", lwd=3, type='l', 
             ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
        plot(time, recruits, type='l')
    })

# effort - constant within season
scenario2 <- scenario
scenario2$catch$effort <- c(0,0,0,0,0,0,6929,6929,6929,0,0,0)
scen2 <- scallop_model_fun(scenario2)
with(scen2$results,{
        par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        abline(lm(VB~time), lwd=2, lty=3)
        et2 <- et
        et2[et2==0] <- NA
        plot(time, et2, col="red", lwd=3, type='l', 
             ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
        plot(time, recruits, type='l')
    })

# effort - increasing between years
scenario3 <- scenario
scenario3$catch$e_years <- seq(1,2,length.out=10)
scen3 <- scallop_model_fun(scenario3)
with(scen3$results,{
        par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        abline(lm(VB~time), lwd=2, lty=3)
        et2 <- et
        et2[et2==0] <- NA
        plot(time, et2, col="red", lwd=3, type='l', 
             ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
        plot(time, recruits, type='l')
    })

# rolling bag limit - decrease
scenario4 <- scenario
scenario4$catch$bag <- c(2,2,2,2,2,2,2,1,1,1,1,1)
scen4 <- scallop_model_fun(scenario4)
with(scen4$results,{
        par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        abline(lm(VB~time), lwd=2, lty=3)
        et2 <- et
        et2[et2==0] <- NA
        plot(time, et2, col="red", lwd=3, type='l', 
             ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
        plot(time, recruits, type='l')
    })

# season start
scenario5 <- scenario
scenario5$catch$season <- c(0,0,0,0,0,0,0,1,1,1,0,0)
scen5 <- scallop_model_fun(scenario5)
with(scen5$results,{
        par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        abline(lm(VB~time), lwd=2, lty=3)
        et2 <- et
        et2[et2==0] <- NA
        plot(time, et2, col="red", lwd=3, type='l', 
             ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
        plot(time, recruits, type='l')
    })

# rolling bag and season start
scenario6 <- scenario
scenario6$catch$bag <- c(2,2,2,2,2,2,2,1,1,1,1,1)
scenario6$catch$season <- c(0,0,0,0,0,0,0,1,1,1,0,0)
scen6 <- scallop_model_fun(scenario6)
with(scen6$results,{
        par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        abline(lm(VB~time), lwd=2, lty=3)
        et2 <- et
        et2[et2==0] <- NA
        plot(time, et2, col="red", lwd=3, type='l', 
             ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
        plot(time, recruits, type='l')
    })
