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
				hpue.obs <- run$results$hpue
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
			hpue.obs <- run$results$hpue
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


###-----------------------------------------------------
#		Tune catchability (q)
###-----------------------------------------------------
# run model with new change in  q (percent change)
mgmt_q_runner <- function(perchange, run = FALSE, E = NULL, bag = NULL, E_open = NULL, E_cap = NULL){
	s1 <- scenario
	s1$catch_eq$q <- scenario$catch_eq$q * (1+perchange)
	if(run){
		s1$sim$run <- TRUE
		s1$catch_run$q <- scenario$catch_run$q * (1+perchange)
		if(!is.null(E)) s1$catch_run$E_years <- E
		if(!is.null(bag)) s1$catch_run$bag <- bag
		if(!is.null(E_open)) s1$catch_run$E_open <- E_open
		if(!is.null(E_cap)) s1$catch_run$E_cap <- E_cap
	}else{
		if(!is.null(E)) s1$catch_eq$E_years <- E
		if(!is.null(bag)) s1$catch_eq$bag <- bag
		if(!is.null(E_open)) s1$catch_eq$E_open <- E_open
		if(!is.null(E_cap)) s1$catch_eq$E_cap <- E_cap
	}
	scallop_model_fun(s1)
}


# find q (catchability) given SPR and effort
q_hunter <- function(spr,E){
	spr.fn <- function(perchange, spr, E){
		s1 <- scenario
		s1$catch_eq$q <- scenario$catch_eq$q * (1+perchange)
		if(!is.null(E)) s1$catch_eq$E_years <- E
		eggs.obs <- try(scallop_model_fun(s1)$results$eggs)
		# eggs.obs <- tapply(eggs.obs, cut(seq_along(eggs.obs),25),sum)
		eggs.obs <- eggs.obs[eggs.obs != 0]
		#ending eggs divide unfished beginning eggs
		spr.obs <- eggs.obs[length(eggs.obs)]/eggs0
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