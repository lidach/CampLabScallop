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
    scenario2$catch$bag <- rep(0.001,12)
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
     file = "./Scenario_Figures/scenario_sensitivity_storage.RData")

    rm(list=ls())
    gc()
    load("./Scenario_Figures/scenario_sensitivity_storage.RData")

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
    plot_out_type <- "tiff"
    for(i in 1:length(scen_str)){
        if(i==1){
            if(plot_out_type == "pdf") pdf(file=paste0("./Scenario_Figures/scenario_base_results.pdf"))
            if(plot_out_type == "tiff") tiff(filename=paste0("./Scenario_Figures/scenario_base_results.tiff"),height = 17, width = 20, units = 'cm', compression = "lzw", res = 500)

        }else{
            if(plot_out_type == "pdf"){
                pdf(file=paste0("./Scenario_Figures/scenario_",
                        formatC(i-1,width=2,flag="0"),
                        "_results.pdf"))
            }
            if(plot_out_type == "tiff"){
                tiff(filename=paste0("./Scenario_Figures/scenario_",
                        formatC(i-1,width=2,flag="0"),
                        "_results.tiff"),height = 17, width = 20, units = 'cm', compression = "lzw", res = 500)
            }
        }
        
            with(scen_str[[i]]$results,{
                Nt <- nrow(scen_str[[i]]$results)
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
                eggs.sci <- sci.transl(eggs.r)
                plot(time, eggs_mon/eggs.sci$scale, type='l', 
                     ylab = paste0("Eggs (Monthly; ", eggs.sci$label,")"), 
                     ylim=eggs.sci$r, lwd=2, col="seagreen")
                abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
            })
        dev.off()
    }

rm(list=setdiff(ls(), c("scen_str","sens_q_low_str","sens_q_high_str","sens_str")))
gc()

##################################
## bar plots
##################################
## regular q scenarios
    ## relative errors
    # calculations of metrics
    # "SPR"
        eggs0 <- sapply(1:14, function(x) scen_str[[x]]$results$eggs[12])
        eggs_list <- sapply(1:14, function(x) mean(c(scen_str[[x]]$results$eggs[6:9*12]),sum(scen_str[[x]]$results$eggs_mon[109:120]))) # no eggs calcs in last year in results$eggs so needed to calculate from eggs_mon
        SPR <- eggs_list/eggs0
    # hcpue (open months, last 5 years) - is there a better way to do this?
        hcpue_list <- lapply(1:14, function(x) matrix(scen_str[[x]]$results$hcpue, nrow = 12, ncol = 10))
        for(i in 1:14) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
        hcpue <- sapply(1:14, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
        hcpue <- sapply(1:14, function(x) mean(hcpue[6:10,x]))    
    # pr_hr (open montths, last 5 years)
        prhr_list <- lapply(1:14, function(x) matrix(scen_str[[x]]$results$pr_hr, nrow = 12, ncol = 10))
        for(i in 1:14) prhr_list[[i]][prhr_list[[i]] == 0] <- NA
        pr_hr <- sapply(1:14, function(x) colMeans(prhr_list[[x]], na.rm = TRUE))
        pr_hr <- sapply(1:14, function(x) mean(pr_hr[6:10, x])) 
        pr_hr[3] <- 0 # "unfished" scenario
    # combine all results to get relative errors
    SPR_base <- SPR[1]; hcpue_base <- hcpue[1]; prhr_base <- pr_hr[1]
    res_mat_scen <- matrix(NA, nrow = 3, ncol = 13) 
        res_mat_scen[1,] <- sapply(2:14, function(x) (SPR[x]-SPR_base)/SPR_base)
        res_mat_scen[2,] <- sapply(2:14, function(x) (hcpue[x]-hcpue_base)/hcpue_base)
        res_mat_scen[3,] <- sapply(2:14, function(x) (pr_hr[x]-prhr_base)/prhr_base)
        colnames(res_mat_scen) <- names(scen_str)[2:14]
        rownames(res_mat_scen) <- c("SPR","hcpue","pr_hr")
        res_mat_scen

## low q scenarios
    ## relative errors
    # calculations of metrics
    # "SPR"
        eggs0 <- sapply(1:13, function(x) sens_q_low_str[[x]]$results$eggs[12])
        eggs_list <- sapply(1:13, function(x) mean(c(sens_q_low_str[[x]]$results$eggs[6:9*12]),sum(sens_q_low_str[[x]]$results$eggs_mon[109:120]))) # no eggs calcs in last year in results$eggs so needed to calculate from eggs_mon
        SPR <- eggs_list/eggs0
    # hcpue (open months, last 5 years) - is there a better way to do this?
        hcpue_list <- lapply(1:13, function(x) matrix(sens_q_low_str[[x]]$results$hcpue, nrow = 12, ncol = 10))
        for(i in 1:13) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
        hcpue <- sapply(1:13, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
        hcpue <- sapply(1:13, function(x) mean(hcpue[6:10,x]))    
    # pr_hr (open montths, last 5 years)
        prhr_list <- lapply(1:13, function(x) matrix(sens_q_low_str[[x]]$results$pr_hr, nrow = 12, ncol = 10))
        for(i in 1:13) prhr_list[[i]][prhr_list[[i]] == 0] <- NA
        pr_hr <- sapply(1:13, function(x) colMeans(prhr_list[[x]], na.rm = TRUE))
        pr_hr <- sapply(1:13, function(x) mean(pr_hr[6:10, x])) 
        pr_hr[2] <- 0 # "unfished" scenario
    # combine all results to get relative errors
    res_mat_lowq <- matrix(NA, nrow = 3, ncol = 13) 
        res_mat_lowq[1,] <- sapply(1:13, function(x) (SPR[x]-SPR_base)/SPR_base)
        res_mat_lowq[2,] <- sapply(1:13, function(x) (hcpue[x]-hcpue_base)/hcpue_base)
        res_mat_lowq[3,] <- sapply(1:13, function(x) (pr_hr[x]-prhr_base)/prhr_base)
        colnames(res_mat_lowq) <- names(sens_q_low_str)[1:13]
        rownames(res_mat_lowq) <- c("SPR","hcpue","pr_hr")
        res_mat_lowq

## high q scenarios
    ## relative errors
    # calculations of metrics
    # "SPR"
        eggs0 <- sapply(1:13, function(x) sens_q_high_str[[x]]$results$eggs[12])
        eggs_list <- sapply(1:13, function(x) mean(c(sens_q_high_str[[x]]$results$eggs[6:9*12]),sum(sens_q_high_str[[x]]$results$eggs_mon[109:120]))) # no eggs calcs in last year in results$eggs so needed to calculate from eggs_mon
        SPR <- eggs_list/eggs0
    # hcpue (open months, last 5 years) - is there a better way to do this?
        hcpue_list <- lapply(1:13, function(x) matrix(sens_q_high_str[[x]]$results$hcpue, nrow = 12, ncol = 10))
        for(i in 1:13) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
        hcpue <- sapply(1:13, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
        hcpue <- sapply(1:13, function(x) mean(hcpue[6:10,x]))    
    # pr_hr (open montths, last 5 years)
        prhr_list <- lapply(1:13, function(x) matrix(sens_q_high_str[[x]]$results$pr_hr, nrow = 12, ncol = 10))
        for(i in 1:13) prhr_list[[i]][prhr_list[[i]] == 0] <- NA
        pr_hr <- sapply(1:13, function(x) colMeans(prhr_list[[x]], na.rm = TRUE))
        pr_hr <- sapply(1:13, function(x) mean(pr_hr[6:10, x])) 
        pr_hr[2] <- 0 # "unfished" scenario
    # combine all results to get relative errors
    res_mat_highq <- matrix(NA, nrow = 3, ncol = 13) 
        res_mat_highq[1,] <- sapply(1:13, function(x) (SPR[x]-SPR_base)/SPR_base)
        res_mat_highq[2,] <- sapply(1:13, function(x) (hcpue[x]-hcpue_base)/hcpue_base)
        res_mat_highq[3,] <- sapply(1:13, function(x) (pr_hr[x]-prhr_base)/prhr_base)
        colnames(res_mat_highq) <- names(sens_q_high_str)[1:13]
        rownames(res_mat_highq) <- c("SPR","hcpue","pr_hr")
        res_mat_highq

## plots
    col <- c("#009999", "#0000FF","#104E8B")
    SPR <- t(cbind(res_mat_lowq[1,], res_mat_scen[1,], res_mat_highq[1,]))
    hcpue <- t(cbind(res_mat_lowq[2,], res_mat_scen[2,], res_mat_highq[2,]))
    pr_hr <- t(cbind(res_mat_lowq[3,], res_mat_scen[3,], res_mat_highq[3,]))
    plot_export <- TRUE

    # SPR
        if(plot_export == TRUE) tiff(filename=paste0("./Scenario_Figures/1_rel_errors_SPR.tiff"),height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
        par(mar=c(6,3.5,1,7), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
        barplot(SPR[,c(1,3:13)], beside = TRUE, ylim = c(-0.6,0.3), las = 2, col = col, ylab = "Relative error", main = "Eggs/Unfished management scenarios") # removed "unfished" scenario for now
        legend("right", inset = c(-0.2,0), legend = c("low q", "base q", "high q"), fill = col, box.lty = 0)
        if(plot_export == TRUE) dev.off()
    # hcpue
        if(plot_export == TRUE) tiff(filename=paste0("./Scenario_Figures/1_rel_errors_hcpue.tiff"),height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
        par(mar=c(6,3.5,1,7), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
        barplot(hcpue[,c(1,3:13)], beside = TRUE, ylim = c(-0.5,0.5), las = 2, col = col, ylab = "Relative error", main = "Catch per unit of effort management scenarios") # removed "unfished" scenario for now
        legend("right", inset = c(-0.2,0), legend = c("low q", "base q", "high q"), fill = col, box.lty = 0)
        if(plot_export == TRUE) dev.off()
    # pr_hr
        if(plot_export == TRUE) tiff(filename=paste0("./Scenario_Figures/1_rel_errors_prhr.tiff"),height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
        par(mar=c(6,3.5,1,7), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
        barplot(pr_hr[,c(1,3:13)], beside = TRUE, ylim = c(-0.05,0.05), las = 2, col = col, ylab = "Relative error", main = "Probability of legal harvest management scenarios") # removed "unfished" scenario for now
        legend("right", inset = c(-0.2,0), legend = c("low q", "base q", "high q"), fill = col, box.lty = 0)
        if(plot_export == TRUE) dev.off()

rm(list=setdiff(ls(), c("scen_str","sens_q_low_str","sens_q_high_str","sens_str")))
gc()   

##################################
## sensitivity analysis plots
## SPR
    SPR_base <- c(scen_str$base$results$eggs[1:9*12],sum(scen_str$base$results$eggs_mon[109:120]))/scen_str$base$results$eggs[12]
    eggs0 <- sapply(1:10, function(x) sens_str[[x]]$results$eggs[12])
    eggs_list <- NA
        eggs_list <- lapply(1:10, function(x) c(sens_str[[x]]$results$eggs[1:9*12],sum(sens_str[[x]]$results$eggs_mon[109:120]))) # no eggs calcs in last year in results$eggs so needed to calculate from eggs_mon
    SPR_list <- lapply(1:10, function(x) eggs_list[[x]]/eggs0[x])
## hcpue - average per open season
    hcpue_base <- matrix(scen_str$base$results$hcpue, nrow = 12, ncol = 10)
        hcpue_base[hcpue_base == 0] <- NA
        hcpue_base <- colMeans(hcpue_base, na.rm = TRUE)
    hcpue_list <- lapply(1:10, function(x) matrix(sens_str[[x]]$results$hcpue, nrow = 12, ncol = 10))
        for(i in 1:10) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
        hcpue_list <- lapply(1:10, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
## pr_hr - average per open season
    prhr_base <- matrix(scen_str$base$results$pr_hr, nrow = 12, ncol = 10)
        prhr_base[prhr_base == 0] <- NA
        prhr_base <- colMeans(prhr_base, na.rm = TRUE)
    prhr_list <- lapply(1:10, function(x) matrix(sens_str[[x]]$results$pr_hr, nrow = 12, ncol = 10))
        for(i in 1:10) prhr_list[[i]][prhr_list[[i]] == 0] <- NA
        prhr_list <- lapply(1:10, function(x) colMeans(prhr_list[[x]], na.rm = TRUE))

## plots
plot_export <- TRUE
v <- seq(2,10,2)
plot_names <- c(rep("sensitivity in q",2), rep("sensitivity in M",2), rep("sensivity in vbk",2), rep("sensitivity in compensation ratio",2), rep(expression("sensitivity in a"[mat]),2))
plot_export_names <- c(rep("sens_q",2), rep("sens_M",2), rep("sens_vbk",2), rep("sens_CR",2), rep("sens_amat",2))

for(i in seq(1,10,2)){
    if(plot_export == TRUE) tiff(filename=paste0("./Scenario_Figures/", plot_export_names[i],".tiff"), height = 17, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow=c(3,1), mar=c(2,3.5,2,7.3), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
        plot(SPR_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Eggs/Unfished", xlab = "", main = plot_names[i])
            lines(SPR_list[[i]], lty = 2, lwd = 2, col = "red")
            lines(SPR_list[[v[i]]], lty = 2, lwd = 2, col = "orange")
    par(mar=c(2,3.5,1,7.3), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
        plot(hcpue_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Catch per unit of effort", xlab = "")
            lines(hcpue_list[[i]], lty = 2, lwd = 2, col = "red")
            lines(hcpue_list[[v[i]]], lty = 2, lwd = 2, col = "orange")
    legend("right", inset = c(-0.15,0), legend = c("base","decr. 25%", "incr. 25%"), box.lty = 0, col = c("black","red","orange"), lty = c(1,2,2))
        plot(prhr_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Probability of legal harvest", xlab = "")
            lines(prhr_list[[i]], lty = 2, lwd = 2, col = "red")
            lines(prhr_list[[v[i]]], lty = 2, lwd = 2, col = "orange")
    if(plot_export == TRUE) dev.off()
}

