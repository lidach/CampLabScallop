#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#			Code for tuning and calibration
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

###-----------------------------------------------------
#		Tune R0
###-----------------------------------------------------
# tuning unfished recruitment (R0) using harvest per unit of effort (hpue)
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