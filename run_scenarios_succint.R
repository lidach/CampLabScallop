#######################################
#Scenarios
#######################################
source("scallop_model_fun_Baranov.R")

###-----------------------------------------------------
#		Tune R0
###-----------------------------------------------------

	hpue <- 0.81
	r0_hunter <- function(hpue,lower.bnd=-0.5,upper.bnd=1){
		hpue.fn <- function(perchange, hpue){
			s1 <- scenario
			s1$life$Ro <- scenario$life$Ro * (1+perchange)
			run <- try(scallop_model_fun(s1))
			if(class(run)=='try-error'){
				dev <- 1000
			}else{
				#pr_hpue
				hpue.obs <- run$result$hpue
				hpue.obs <-  hpue.obs[hpue.obs!=0]
				hpue.mu <- hpue.obs[length(hpue.obs)-2]
				dev <- (hpue.mu-hpue)^2
			}
			return(dev)
		}
		theta <- 0
		fit <- optim(par=theta, fn=hpue.fn, hpue=hpue, 
		             lower=lower.bnd, upper=upper.bnd,
		             method='Brent')
		hpue.eval <- function(perchange){
			s1 <- scenario
			s1$life$Ro <- scenario$life$Ro * (1+perchange)
			run <- scallop_model_fun(s1)
			SPR <- run$scenario$per.rec$spr
			hpue.obs <- run$result$hpue
			hpue.obs <-  hpue.obs[hpue.obs!=0]
			hpue.mu <- hpue.obs[length(hpue.obs)-2]
			pr_hr.obs <- run$results$pr_hr
			pr_hr.obs <-  pr_hr.obs[pr_hr.obs!=0]
			pr_hr.mu <- pr_hr.obs[length(pr_hr.obs)-2]
			return(list(SPR = SPR,
			         	hpue = hpue.mu,
			         	pr_hr = pr_hr.mu))
		}
		eval <- hpue.eval(fit$par)
		return(list(perchange = fit$par,
			       newR0 = (1+fit$par)*scenario$life$Ro,
			       SPR = eval[[1]],
			       hpue.mu = eval[[2]],
			       pr_hr = eval[[3]]))
	}
	(R0.hunt <- r0_hunter(hpue=0.81, upper.bnd=3))
	scenario$life$Ro <- R0.hunt$newR0 #input new R0 to get the desired probability of hitting the harvest rate
	unf <- scenario
	unf$catch$bag <- rep(0.001,12)
	unf <- scallop_model_fun(unf)
##################################
## Mgmt scenarios 

	mgmt_scen <- rep(list(list(bag=NULL,E_open=NULL,E_cap=NULL)),12)
	mgmt_scen[[1]]$bag <- rep(0.001,12) #Decrease Bag, Basically unfished scenario
	mgmt_scen[[2]]$bag <- rep(3,12) # bag limit = 3
	mgmt_scen[[3]]$bag <- rep(1,12) # bag limit = 1
	mgmt_scen[[4]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0) # season length - increase in total effort, extend one month forward
	mgmt_scen[[5]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0) # season length - same effort, extend one month forward
	mgmt_scen[[5]]$E_cap <- TRUE
	mgmt_scen[[6]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0)# season length - increase in total effort, extend one month back
	mgmt_scen[[7]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0)# season length - same effort, extend one month back
	mgmt_scen[[7]]$E_cap <- TRUE
	mgmt_scen[[8]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)# push season back one month
	mgmt_scen[[9]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)# push season forward one month
	mgmt_scen[[10]]$bag <- c(1,1,1,1,1,1,0.5,1,1.5,2,2,2)# rolling bag limit - increasing
	mgmt_scen[[11]]$bag <- c(1,1,1,1,1,1,1,0.5,1,1.5,2,2)# rolling bag and season start - back one month
	mgmt_scen[[11]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	mgmt_scen[[12]]$bag <- c(1,1,1,1,1,1,0.5,1,1.5,2,2,2)# rolling bag and season start - forward one month
	mgmt_scen[[12]]$E_open <- c(0,0,0,0,0,0,1,1,1,0,0,0)


##################
# sensitivity runs
##################
	sens.par <- c("catch$q","life$M","life$vbk","life$CR","life$amat")
	sens.runner <- function(perchange=0.25, par){
		s1 <- scenario
		s2 <- scenario
		par.spl <- strsplit(par,"\\$")[[1]]
		s1[[par.spl[1]]][[par.spl[2]]] <- scenario[[par.spl[1]]][[par.spl[2]]]*(1-perchange)
		s2[[par.spl[1]]][[par.spl[2]]] <- scenario[[par.spl[1]]][[par.spl[2]]]*(1+perchange)
		v <- list(senq1 = scallop_model_fun(s1),
		          senq2 = scallop_model_fun(s2))
	}
	sens_str <- lapply(sens.par, function(x) sens.runner(par=x))
	names(sens_str) <- sapply(strsplit(sens.par,"\\$"),function(x) x[2])

##################################
## run mgmt scenarios with different starting q
    # base scenarios for each panel
    # perchange1 <- 0.3 # ~45% SPR
    # perchange2 <- 1.57 # ~30% SPR
	mgmt_q_runner <- function(perchange,E=NULL,bag=NULL,E_open=NULL,E_cap=NULL){
		s1 <- scenario
		s1$catch$q <- scenario$catch$q * (1+perchange)
		if(!is.null(E)) s1$catch$E_years <- E
		if(!is.null(bag)) s1$catch$bag <- bag
		if(!is.null(E_open)) s1$catch$E_open <- E_open
		if(!is.null(E_cap)) s1$catch$E_cap <- E_cap
		scallop_model_fun(s1)
	}
	E1 <- seq(1,2,length.out=25) # effort - increasing between years (doubling over 25 years)
    E2 <- seq(1,3,length.out=25) # effort - increasing between years (tripling over 25 years)
	E_seq <- list(NULL,NULL,NULL,E1,E1,E1,E2,E2,E2)
	spr_seq <- rep(c(0.63,0.5,0.35),3) #desired SPRs
	q_hunter <- function(spr,E){
		spr.fn <- function(perchange, spr, E){
			s1 <- scenario
			s1$catch$q <- scenario$catch$q * (1+perchange)
			if(!is.null(E)) s1$catch$E_years <- E
			eggs.obs <- try(scallop_model_fun(s1)$results$eggs_mon)
			eggs.obs <- tapply(eggs.obs, cut(seq_along(eggs.obs),25),sum)
			#ending eggs divide unfished beginning eggs
			spr.obs <- eggs.obs[length(eggs.obs)]/sum(unf$results$eggs_mon[1:12])
			# spr.obs <- try(scallop_model_fun(s1)$scenario$per.rec$spr)
			if(class(spr.obs)=='try-error'){
				dev <- 1000
			}else{
				dev <- (spr.obs-spr)^2
				# dev <- -dnorm(spr.obs-spr,0,1,log=T)
			}
			return(dev)
		}
		theta <- 0
		fit <- optim(par=theta, fn=spr.fn, spr=spr, E=E, 
		             lower=-1, upper=8, method='Brent')

		return(fit$par)
	}
	perchange_seq <- sapply(1:length(E_seq), function(ii) q_hunter(spr=spr_seq[ii],E=E_seq[[ii]]))
	perchange_seq[1] <- 0 #set to baseline

	# perchange_seq <- c(0,0.964,2.86,-0.324,0.31,1.58,-0.493,-0.018,0.93)
	#base scenarios
	mgmt_base_q <- lapply(1:length(E_seq),function(x){mgmt_scen[[1]]$E <- E_seq[[x]]; mgmt_scen[[1]]$perchange <- perchange_seq[[x]]; mgmt_scen[[1]]$bag <- NULL; return(mgmt_scen[[1]])})

  	scen_str_base <- lapply(mgmt_base_q, function(x) mgmt_q_runner(perchange=x$perchange, E=x$E))

  	sapply(scen_str_base, function(x) x$scenario$per.rec$spr)
  	sapply(scen_str_base, function(x){v <- x$results$eggs; v <- v[v!=0]; return(v[length(v)]/v[1])})

    #mgmt scenarios
    mgmt_scen_q <- lapply(1:length(E_seq),function(x)lapply(mgmt_scen,function(y) {y$E <- E_seq[[x]]; y$perchange <- perchange_seq[[x]]; return(y)}))

    scen_str_all <- lapply(mgmt_scen_q, function(y) lapply(y,function(x)mgmt_q_runner(perchange=x$perchange,E=x$E,bag=x$bag,E_open=x$E_open,E_cap=x$E_cap)))
    
    
##################################
# save all scenarios to one .RData

    save(scen_str_base,
    	scen_str_all,
    	sens_str,
     file = "./Scenario_Figures/semel_scenario_sensitivity_storage.RData")

    rm(list=ls())
    gc()
    load("./Scenario_Figures/semel_scenario_sensitivity_storage.RData")

#################################
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
    scen_str <- scen_str_all[[1]]
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
            if(plot_out_type == "tiff") tiff(filename=paste0("./Scenario_Figures/scenario_base_results.tiff"),height = 17, width = 23, units = 'cm', compression = "lzw", res = 500)

        }else{
            if(plot_out_type == "pdf"){
                pdf(file=paste0("./Scenario_Figures/scenario_",
                        formatC(i-1,width=2,flag="0"),
                        "_results.pdf"))
            }
            if(plot_out_type == "tiff"){
                tiff(filename=paste0("./Scenario_Figures/scenario_",
                        formatC(i-1,width=2,flag="0"),
                        "_results.tiff"),height = 17, width = 23, units = 'cm', compression = "lzw", res = 500)
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
                par(mar=c(2,3.5,1,3.8), las=1, mgp=c(2.5,0.5,0), tck=-0.015)
                eggs.sci <- sci.transl(eggs.r)
                plot(time, eggs_mon/eggs.sci$scale, type='l', 
                     ylab = paste0("Eggs (Monthly; ", eggs.sci$label,")"), 
                     ylim=eggs.sci$r, lwd=2, col="seagreen")
                abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
                par(new=T)
				   plot(time[eggs>0], eggs[eggs>0]/sum(scen_str[[i]]$scenario$life$Ro * scen_str[[i]]$scenario$life.vec$Lo *  scen_str[[i]]$scenario$life.vec$Mat * scen_str[[i]]$scenario$life.vec$Fec *  scen_str[[i]]$scenario$life.vec$prob_spawn), 
				   	type="l", ylim=c(0,1) ,ylab="", xlab="", xaxt="n", yaxt="n")
				   points(time[eggs>0], eggs[eggs>0]/sum(scen_str[[i]]$scenario$life$Ro * scen_str[[i]]$scenario$life.vec$Lo *  scen_str[[i]]$scenario$life.vec$Mat * scen_str[[i]]$scenario$life.vec$Fec *  scen_str[[i]]$scenario$life.vec$prob_spawn), pch = 16)
				   axis(side=4, at=seq(0,1,0.2), labels=seq(0,1,0.2), las=1)
				   mtext(text="Depletion (Eggs Deposited)", side=4,line=2.5,las=0, outer=F, cex=0.7)
            })
        dev.off()
    }

rm(list=setdiff(ls(), c("scen_str_base","scen_str_all","sens_str")))
gc()

##################################
## bar plots
##################################
## extracting base lines values (from scen_str_base)
	eggs0 <- sapply(1:9, function(x) scen_str_base[[x]]$results$eggs[12])
		eggs_list <- sapply(1:9, function(x) mean(c(scen_str_base[[x]]$results$eggs[21:24*12]), sum(scen_str_base[[x]]$results$eggs_mon[289:300])))
		depl_base <- eggs_list/eggs0
	hcpue_list <- lapply(1:9, function(x) matrix(scen_str_base[[x]]$results$hcpue, nrow = 12, ncol = 25))
		for(i in 1:9) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
		hcpue_base <- sapply(1:9, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
		hcpue_base <- sapply(1:9, function(x) mean(hcpue_base[21:25,x]))
	hr_list <- lapply(1:9, function(x) matrix(scen_str_base[[x]]$results$hr, nrow = 12, ncol = 25))
		for(i in 1:9) hr_list[[i]][hr_list[[i]] == 0] <- NA
		hr_base <- sapply(1:9, function(x) colMeans(hr_list[[x]], na.rm = TRUE))
		hr_base <- sapply(1:9, function(x) mean(hr_base[21:25,x]))


## extracting values from all treatments and scenarios
	depl_rel <- matrix(NA, nrow = 9, ncol = 12)
	hcpue_rel <- matrix(NA, nrow = 9, ncol = 12)
	hr_rel <- matrix(NA, nrow = 9, ncol = 12)
	depl_abs <- matrix(NA, nrow = 9, ncol = 12)
	hcpue_abs <- matrix(NA, nrow = 9, ncol = 12)
	hr_abs <- matrix(NA, nrow = 9, ncol = 12)

	for(j in 1:9){
		# depletion
			eggs0 <- sapply(1:12, function(x) scen_str_all[[j]][[x]]$results$eggs[12])
			eggs_list <- sapply(1:12, function(x) mean(c(scen_str_all[[j]][[x]]$results$eggs[21:24*12]),sum(scen_str_all[[j]][[x]]$results$eggs_mon[289:300])))
			depl <- eggs_list/eggs0
		# hcpue (open months, last 5 years) - is there a better way to do this?
	        hcpue_list <- lapply(1:12, function(x) matrix(scen_str_all[[j]][[x]]$results$hcpue, nrow = 12, ncol = 25))
	        for(i in 1:12) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
	        hcpue <- sapply(1:12, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
	        hcpue <- sapply(1:12, function(x) mean(hcpue[21:25,x]))    
	    # hr (open montths, last 5 years)
	       hr_list <- lapply(1:12, function(x) matrix(scen_str_all[[j]][[x]]$results$hr, nrow = 12, ncol = 25))
	        for(i in 1:12) hr_list[[i]][hr_list[[i]] == 0] <- NA
	        hr <- sapply(1:12, function(x) colMeans(hr_list[[x]], na.rm = TRUE))
	        hr <- sapply(1:12, function(x) mean(hr[21:25, x])) 
	        hr[1] <- 0 # "unfished" scenario
	    # combine all results to get relative errors
	        depl_rel[j,] <- sapply(1:12, function(x) (depl[x]-depl_base[j])/depl_base[j])
	        hcpue_rel[j,] <- sapply(1:12, function(x) (hcpue[x]-hcpue_base[j])/hcpue_base[j])
	        hr_rel[j,] <- sapply(1:12, function(x) (hr[x]-hr_base[j])/hr_base[j])
	    # combine all results to get abs values
	        depl_abs[j,] <- sapply(1:12, function(x) depl[x])
	        hcpue_abs[j,] <- sapply(1:12, function(x) hcpue[x])
	        hr_abs[j,] <- sapply(1:12, function(x) hr[x])
  	}
  	depl_abs <- cbind(depl_base, depl_abs)
  	hcpue_abs <- cbind(hcpue_base, hcpue_abs)
  	hr_abs <- cbind(hr_base, hr_abs)
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hr_abs) <- NULL

  	bar_res <- list(depl_rel = depl_rel,
                    hcpue_rel = hcpue_rel,
                    hr_rel = hr_rel,
                    depl_abs = depl_abs,
                    hcpue_abs = hcpue_abs,
                    hr_abs = hr_abs)
    save(bar_res, file = "./Results_RData/semel_res_plots.RData")
    save(scen_str_base, scen_str_all, sens_str,
     file = "./Results_RData/semel_scenario_sensitivity_storage.RData")

# rm(list=setdiff(ls(), c("scen_str","sens_q_low_str","sens_q_high_str","sens_str")))
# gc()   

##################################
# sensitivity analysis plots
	## SPR
	    SPR_base <- c(scen_str_base[[1]]$results$eggs[1:24*12],sum(scen_str_base[[1]]$results$eggs_mon[289:300]))/scen_str_base[[1]]$results$eggs[12]
	    eggs0 <- c(sapply(sens_str, function(x) sapply(x,function(y) y$results$eggs[12])))
	    eggs_list <- unlist(lapply(sens_str, function(x) lapply(x, function(y) c(y$results$eggs[1:24*12],sum(y$results$eggs_mon[289:300])))),recursive=FALSE) # no eggs calcs in last year in results$eggs so needed to calculate from eggs_mon
	    SPR_list <- lapply(1:length(eggs_list), function(x) eggs_list[[x]]/eggs0[x])
	## hcpue - average per open season
	    hcpue_base <- matrix(scen_str_base[[1]]$results$hcpue, nrow = 12, ncol = 25)
	        hcpue_base[hcpue_base == 0] <- NA
	        hcpue_base <- colMeans(hcpue_base, na.rm = TRUE)
	    hcpue_list <- unlist(lapply(sens_str, function(x) lapply(x,function(y) matrix(y$results$hcpue, nrow = 12, ncol = 25))),recursive=FALSE)
	        for(i in 1:length(hcpue_list)) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
	        hcpue_list <- lapply(1:length(hcpue_list), function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
	## hr - average per open season
	    hr_base <- matrix(scen_str_base[[1]]$results$hr, nrow = 12, ncol = 25)
	        hr_base[hr_base == 0] <- NA
	        hr_base <- colMeans(hr_base, na.rm = TRUE)
	    hr_list <- unlist(lapply(sens_str, function(x) lapply(x, function(y) matrix(y$results$hr, nrow = 12, ncol = 25))),recursive=FALSE)
	        for(i in 1:length(hr_list)) hr_list[[i]][hr_list[[i]] == 0] <- NA
	        hr_list <- lapply(1:length(hr_list), function(x) colMeans(hr_list[[x]], na.rm = TRUE))

	## plots
		plot_export <- TRUE
		v <- seq(1,10,2)
		y <- seq(2,10,2)
		plot_names <- c("sensitivity in q", "sensitivity in M", "sensivity in vbk", "sensitivity in compensation ratio", expression("sensitivity in a"[mat]))
		plot_export_names <- c("sens_q", "sens_M", "sens_vbk", "sens_CR", "sens_amat")

		for(i in 1:5){
		    if(plot_export == TRUE) tiff(filename=paste0("./Scenario_Figures/", plot_export_names[i],".tiff"), height = 17, width = 20, units = 'cm', compression = "lzw", res = 500)
		    par(mfrow=c(3,1), mar=c(2,3.5,2,7.3), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
		        plot(SPR_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Eggs/Unfished", xlab = "", main = plot_names[i])
		            lines(SPR_list[[v[i]]], lty = 2, lwd = 2, col = "red")
		            lines(SPR_list[[y[i]]], lty = 2, lwd = 2, col = "orange")
		    par(mar=c(2,3.5,1,7.3), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
		        plot(hcpue_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Catch per unit of effort", xlab = "")
		            lines(hcpue_list[[v[i]]], lty = 2, lwd = 2, col = "red")
		            lines(hcpue_list[[y[i]]], lty = 2, lwd = 2, col = "orange")
		    legend("right", inset = c(-0.15,0), legend = c("base","decr. 25%", "incr. 25%"), box.lty = 0, col = c("black","red","orange"), lty = c(1,2,2))
		        plot(hr_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Harvest rate", xlab = "")
		            lines(hr_list[[v[i]]], lty = 2, lwd = 2, col = "red")
		            lines(hr_list[[y[i]]], lty = 2, lwd = 2, col = "orange")
		    if(plot_export == TRUE) dev.off()
		}
