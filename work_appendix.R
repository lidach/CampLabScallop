#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#		Code to run appendix scenarios
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# not required to run
	# runs additional scenarios explored (24 more)
	# also gives CPUE

library(truncnorm)
require(RColorBrewer)
# wd <- "" # set your working directory here, where all the code is
setwd(wd)
source("functions/scallop_model_fun.R") # scallop model function
source("functions/tune_calibrate.R") # for tuning and calibrating the models 

# export plots to local directory (.tiff files)
export_plots <- FALSE



###-----------------------------------------------------
#		Management scenarios
###-----------------------------------------------------
scenario$sim$run <- FALSE
(R0.hunt <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = scenario))
scenario$life$Ro <- R0.hunt$newR0 #input new R0 to get the desired probability of hitting the harvest rate
# unfished scenario
unf <- scenario
unf$catch_eq$bag <- rep(0.001,12) # very low bag limit - "no fishing"
unf <- scallop_model_fun(unf)
eggs0 <- sum(unf$results$eggs_mon[1:12])


# first 10 - main scenarios, 24 alternative scenarios
mgmt_scen <- rep(list(list(bag=NULL,E_open=NULL,E_cap=NULL)),34)
	# Decrease Bag, Basically unfished scenario (not in graphs/outputs)
	mgmt_scen[[1]]$bag <- rep(0.001,12) 
	# bag limit = 3
	mgmt_scen[[2]]$bag <- rep(3,12) 
	# bag limit = 1
	mgmt_scen[[3]]$bag <- rep(1,12) 
	# season length, extend one month forward
	mgmt_scen[[4]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0) 
	# season length, extend one month back
	mgmt_scen[[5]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0) 
 	# push season back one month
	mgmt_scen[[6]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	# push season forward one month
	mgmt_scen[[7]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
	# rolling bag limit - increasing 
	mgmt_scen[[8]]$bag <- c(1,1,1,1,1,1,1,2,2,2,2,2) 
	# rolling bag and season start - back one month
	mgmt_scen[[9]]$bag <- c(1,1,1,1,1,1,1,1,2,2,2,2)
	mgmt_scen[[9]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	# rolling bag and season start - forward one month
	mgmt_scen[[10]]$bag <- c(1,1,1,1,1,1,2,2,2,2,2,2)
	mgmt_scen[[10]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)

	# Earlier season, 1 bag limit
	mgmt_scen[[11]]$bag <- rep(1,12)
	mgmt_scen[[11]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)	
	# Earlier season, 3 bag limit
	mgmt_scen[[12]]$bag <- rep(3,12)
	mgmt_scen[[12]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
	# Later season, 1 bag limit
	mgmt_scen[[13]]$bag <- rep(1,12)
	mgmt_scen[[13]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	# Later season, 3 bag limit
	mgmt_scen[[14]]$bag <- rep(3,12)
	mgmt_scen[[14]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)

	# rolling bag, c(0,0,0,0,0,0,1,1,2,0,0,0)
	mgmt_scen[[15]]$bag <- c(0,0,0,0,0,0,1,1,2,0,0,0)
	# rolling bag, c(0,0,0,0,0,0,1,2,2,0,0,0)
	mgmt_scen[[16]]$bag <- c(0,0,0,0,0,0,1,2,2,0,0,0)
	# rolling bag, c(0,0,0,0,0,0,2,2,3,0,0,0)
	mgmt_scen[[17]]$bag <- c(0,0,0,0,0,0,2,2,3,0,0,0)
	# rolling bag, c(0,0,0,0,0,0,2,3,3,0,0,0)
	mgmt_scen[[18]]$bag <- c(0,0,0,0,0,0,2,3,3,0,0,0)

	# rolling bag earlier season, c(0,0,0,0,0,1,1,2,0,0,0,0)
	mgmt_scen[[19]]$bag <- c(0,0,0,0,0,1,1,2,0,0,0,0)
	mgmt_scen[[19]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
	mgmt_scen[[19]]$E_cap <- TRUE
	# rolling bag earlier season, c(0,0,0,0,0,1,2,2,0,0,0,0)
	mgmt_scen[[20]]$bag <- c(0,0,0,0,0,1,2,2,0,0,0,0)
	mgmt_scen[[20]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
	mgmt_scen[[20]]$E_cap <- TRUE
	# rolling bag earlier season, c(0,0,0,0,0,2,2,3,0,0,0,0)
	mgmt_scen[[21]]$bag <- c(0,0,0,0,0,2,2,3,0,0,0,0)
	mgmt_scen[[21]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
	mgmt_scen[[21]]$E_cap <- TRUE
	# rolling bag earlier season, c(0,0,0,0,0,2,3,3,0,0,0,0)
	mgmt_scen[[22]]$bag <- c(0,0,0,0,0,2,3,3,0,0,0,0)
	mgmt_scen[[22]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
	mgmt_scen[[22]]$E_cap <- TRUE
	
	# rolling bag later season, c(0,0,0,0,0,0,0,1,1,2,0,0)
	mgmt_scen[[23]]$bag <- c(0,0,0,0,0,0,0,1,1,2,0,0)
	mgmt_scen[[23]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	mgmt_scen[[23]]$E_cap <- TRUE
	# rolling bag later season, c(0,0,0,0,0,0,0,1,2,2,0,0)
	mgmt_scen[[24]]$bag <- c(0,0,0,0,0,0,0,1,2,2,0,0)
	mgmt_scen[[24]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	mgmt_scen[[24]]$E_cap <- TRUE
	# rolling bag later season, c(0,0,0,0,0,0,0,2,2,3,0,0)
	mgmt_scen[[25]]$bag <- c(0,0,0,0,0,0,0,2,2,3,0,0)
	mgmt_scen[[25]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	mgmt_scen[[25]]$E_cap <- TRUE
	# rolling bag later season, c(0,0,0,0,0,0,0,2,3,3,0,0)
	mgmt_scen[[26]]$bag <- c(0,0,0,0,0,0,0,2,3,3,0,0)
	mgmt_scen[[26]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	mgmt_scen[[26]]$E_cap <- TRUE
	
	# Increased Season (earlier), 1 bag limit
	mgmt_scen[[27]]$bag <- rep(1,12)
	mgmt_scen[[27]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0) 
	mgmt_scen[[27]]$E_cap <- TRUE
	# Increased Season (earlier), 3 bag limit
	mgmt_scen[[28]]$bag <- rep(3,12)
	mgmt_scen[[28]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0) 
	mgmt_scen[[28]]$E_cap <- TRUE
	# Increased Season (earlier), rolling limit c(0,0,0,0,0,1,1,2,2,0,0,0)
	mgmt_scen[[29]]$bag <- c(0,0,0,0,0,1,1,2,2,0,0,0)
	mgmt_scen[[29]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0) 
	mgmt_scen[[29]]$E_cap <- TRUE
	# Increased Season (earlier), rolling limit c(0,0,0,0,0,2,2,3,3,0,0,0)
	mgmt_scen[[30]]$bag <- c(0,0,0,0,0,2,2,3,3,0,0,0)
	mgmt_scen[[30]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0) 
	mgmt_scen[[30]]$E_cap <- TRUE

	# Increased Season (later), 1 bag limit
	mgmt_scen[[31]]$bag <- rep(1,12)
	mgmt_scen[[31]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0) 
	mgmt_scen[[31]]$E_cap <- TRUE
	# Increased Season (later), 3 bag limit
	mgmt_scen[[32]]$bag <- rep(3,12)
	mgmt_scen[[32]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0) 
	mgmt_scen[[32]]$E_cap <- TRUE
	# Increased Season (later), rolling limit c(0,0,0,0,0,0,1,1,2,2,0,0)
	mgmt_scen[[33]]$bag <- c(0,0,0,0,0,0,1,1,2,2,0,0)
	mgmt_scen[[33]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0)
	mgmt_scen[[33]]$E_cap <- TRUE
	# Increased Season (later), rolling limit c(0,0,0,0,0,0,2,2,3,3,0,0)
	mgmt_scen[[34]]$bag <- c(0,0,0,0,0,0,2,2,3,3,0,0)
	mgmt_scen[[34]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0) 
	mgmt_scen[[34]]$E_cap <- TRUE


## run mgmt scenarios with different starting q
	E1 <- seq(1,2,length.out=25) # effort - increasing between years (doubling over 25 years)
	E_seq <- list(NULL,NULL,E1,E1)
	spr_seq <- c(0.5,0.2) #desired SPRs	(50% and 20%)

	perchange_seq <- sapply(spr_seq, function(ii) q_hunter(spr = ii, E = NULL))
	perchange_seq <- rep(perchange_seq, 2)
	# base scenarios
	mgmt_base_q <- lapply(1:length(E_seq), function(x){mgmt_scen[[1]]$E <- E_seq[[x]]; mgmt_scen[[1]]$perchange <- perchange_seq[[x]]; mgmt_scen[[1]]$bag <- NULL; return(mgmt_scen[[1]])})
  	scen_str_base <- lapply(mgmt_base_q, function(x) mgmt_q_runner(perchange=x$perchange, E=x$E, run=TRUE))

  	sapply(scen_str_base, function(x) x$scenario$per.rec$spr)
  	sapply(scen_str_base, function(x){v <- x$results$eggs; v <- v[v!=0]; return(v[length(v)]/eggs0)})

    # mgmt scenarios
    mgmt_scen_q <- lapply(1:length(E_seq),function(x)lapply(mgmt_scen,function(y) {y$E <- E_seq[[x]]; y$perchange <- perchange_seq[[x]]; return(y)}))

    scen_str_alt <- lapply(mgmt_scen_q, function(y) lapply(y,function(x)mgmt_q_runner(perchange=x$perchange,run=TRUE,E=x$E,bag=x$bag,E_open=x$E_open,E_cap=x$E_cap)))



###-----------------------------------------------------
#		Sensitivity Runs
###-----------------------------------------------------
# ran all scenarios with sensitivity analyses on natural mortality and compensation ratio/steepness
# +/- 25%, only looking at 50% and 20% spawning output

# M
	base_runs_M <- list()
	runs_M <- rep(list(rep(list(list()),34)),4)
	E_seq <- list(NULL, NULL, E1, E1)
	spr_seq <- c(0.5,0.2,0.5,0.2)
	perchange_seq <- sapply(spr_seq, function(ii) q_hunter(spr = ii, E = NULL))

	for(i in 1:4){
		s1 <- scenario
			s1$life$M <- s1$life$M*(1-0.25)
			s1$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s1)$newR0
			s1$sim$run <- TRUE
			s1$catch_run$q <- s1$catch_run$q * (1+perchange_seq[i])
			if(!is.null(E_seq[[i]])) s1$catch_run$E_years <- E_seq[[i]]
			if(!is.null(mgmt_base_q[[i]]$bag)) s1$catch_run$bag <- mgmt_base_q[[i]]$bag
			if(!is.null(mgmt_base_q[[i]]$E_open)) s1$catch_run$E_open <- mgmt_base_q[[i]]$E_open
			if(!is.null(mgmt_base_q[[i]]$E_cap)) s1$catch_run$E_cap <- mgmt_base_q[[i]]$E_cap
		s2 <- scenario
			s2$life$M <- s2$life$M*(1+0.25)
			s2$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s2)$newR0
			s2$sim$run <- TRUE
			s2$catch_run$q <- s2$catch_run$q * (1+perchange_seq[i])
			if(!is.null(E_seq[[i]])) s2$catch_run$E_years <- E_seq[[i]]
			if(!is.null(mgmt_base_q[[i]]$bag)) s2$catch_run$bag <- mgmt_base_q[[i]]$bag
			if(!is.null(mgmt_base_q[[i]]$E_open)) s2$catch_run$E_open <- mgmt_base_q[[i]]$E_open
			if(!is.null(mgmt_base_q[[i]]$E_cap)) s2$catch_run$E_cap <- mgmt_base_q[[i]]$E_cap	
		# sensitivity analyses
		base_runs_M[[i]] <- list(senq1 = scallop_model_fun(s1),
			          senq2 = scallop_model_fun(s2))
	}
	# run across 34 scenarios
	for(i in 1:4){
		for(j in 1:34){
			s1 <- scenario
				s1$life$M <- s1$life$M*(1-0.25)
				s1$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s1)$newR0
				s1$sim$run <- TRUE
				s1$catch_run$q <- s1$catch_run$q * (1+perchange_seq[i])
				if(!is.null(E_seq[[i]])) s1$catch_run$E_years <- E_seq[[i]]
				if(!is.null(mgmt_scen[[j]]$bag)) s1$catch_run$bag <- mgmt_scen[[j]]$bag
				if(!is.null(mgmt_scen[[j]]$E_open)) s1$catch_run$E_open <- mgmt_scen[[j]]$E_open
				if(!is.null(mgmt_scen[[j]]$E_cap)) s1$catch_run$E_cap <- mgmt_scen[[j]]$E_cap
			s2 <- scenario
				s2$life$M <- s2$life$M*(1+0.25)
				s2$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s2)$newR0
				s2$sim$run <- TRUE
				s2$catch_run$q <- s2$catch_run$q * (1+perchange_seq[i])
				if(!is.null(E_seq[[i]])) s2$catch_run$E_years <- E_seq[[i]]
				if(!is.null(mgmt_scen[[j]]$bag)) s2$catch_run$bag <- mgmt_scen[[j]]$bag
				if(!is.null(mgmt_scen[[j]]$E_open)) s2$catch_run$E_open <- mgmt_scen[[j]]$E_open
				if(!is.null(mgmt_scen[[j]]$E_cap)) s2$catch_run$E_cap <- mgmt_scen[[j]]$E_cap	
		
			runs_M[[i]][[j]] <- list(senq1 = scallop_model_fun(s1),
				          senq2 = scallop_model_fun(s2))
		}
	}

# CR
	# h = CR/(CR+4), CR = 4h/(1-h)
	# 4, 8, 20
	base_runs_CR <- list()
	runs_CR <- rep(list(rep(list(list()),34)),4)
	for(i in 1:4){
		s1 <- scenario
			s1$life$CR <- s1$life$CR*(1-0.5)
			s1$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s1)$newR0
			s1$sim$run <- TRUE
			s1$catch_run$q <- s1$catch_run$q * (1+perchange_seq[i])
			if(!is.null(E_seq[[i]])) s1$catch_run$E_years <- E_seq[[i]]
			if(!is.null(mgmt_base_q[[i]]$bag)) s1$catch_run$bag <- mgmt_base_q[[i]]$bag
			if(!is.null(mgmt_base_q[[i]]$E_open)) s1$catch_run$E_open <- mgmt_base_q[[i]]$E_open
			if(!is.null(mgmt_base_q[[i]]$E_cap)) s1$catch_run$E_cap <- mgmt_base_q[[i]]$E_cap
		s2 <- scenario
			s2$life$CR <- s2$life$CR*(1+0.5)
			s2$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s2)$newR0
			s2$sim$run <- TRUE
			s2$catch_run$q <- s2$catch_run$q * (1+perchange_seq[i])
			if(!is.null(E_seq[[i]])) s2$catch_run$E_years <- E_seq[[i]]
			if(!is.null(mgmt_base_q[[i]]$bag)) s2$catch_run$bag <- mgmt_base_q[[i]]$bag
			if(!is.null(mgmt_base_q[[i]]$E_open)) s2$catch_run$E_open <- mgmt_base_q[[i]]$E_open
			if(!is.null(mgmt_base_q[[i]]$E_cap)) s2$catch_run$E_cap <- mgmt_base_q[[i]]$E_cap	
	
		base_runs_CR[[i]] <- list(senq1 = scallop_model_fun(s1),
			          senq2 = scallop_model_fun(s2))
	}

	# run across 34 scenarios
	for(i in 1:4){ 
		for(j in 1:34){
			s1 <- scenario
				s1$life$CR <- s1$life$CR*(1-0.5)
				s1$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s1)$newR0
				s1$sim$run <- TRUE
				s1$catch_run$q <- s1$catch_run$q * (1+perchange_seq[i])
				if(!is.null(E_seq[[i]])) s1$catch_run$E_years <- E_seq[[i]]
				if(!is.null(mgmt_scen[[j]]$bag)) s1$catch_run$bag <- mgmt_scen[[j]]$bag
				if(!is.null(mgmt_scen[[j]]$E_open)) s1$catch_run$E_open <- mgmt_scen[[j]]$E_open
				if(!is.null(mgmt_scen[[j]]$E_cap)) s1$catch_run$E_cap <- mgmt_scen[[j]]$E_cap
			s2 <- scenario
				s2$life$CR <- s2$life$CR*(1+0.5)
				s2$life$Ro <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = s2)$newR0
				s2$sim$run <- TRUE
				s2$catch_run$q <- s2$catch_run$q * (1+perchange_seq[i])
				if(!is.null(E_seq[[i]])) s2$catch_run$E_years <- E_seq[[i]]
				if(!is.null(mgmt_scen[[j]]$bag)) s2$catch_run$bag <- mgmt_scen[[j]]$bag
				if(!is.null(mgmt_scen[[j]]$E_open)) s2$catch_run$E_open <- mgmt_scen[[j]]$E_open
				if(!is.null(mgmt_scen[[j]]$E_cap)) s2$catch_run$E_cap <- mgmt_scen[[j]]$E_cap	
		
			runs_CR[[i]][[j]] <- list(senq1 = scallop_model_fun(s1),
				          senq2 = scallop_model_fun(s2))
		}
	}



###-----------------------------------------------------
#		Extract results for bar plots
###-----------------------------------------------------
## extracting base lines values (from scen_str_base)
	# eggs0 above	
	eggs_list <- sapply(1:4, function(x) sum(scen_str_base[[x]]$results$eggs_mon[289:300]))
		depl_base <- eggs_list/eggs0
	hcpue_list <- lapply(1:4, function(x) matrix(scen_str_base[[x]]$results$hcpue, nrow = 12, ncol = 25))
		for(i in 1:4) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
		hcpue_base <- sapply(1:4, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
		hcpue_base <- sapply(1:4, function(x) mean(hcpue_base[21:25,x]))
	hpue_list <- lapply(1:4, function(x) matrix(scen_str_base[[x]]$results$hpue, nrow = 12, ncol = 25))
		for(i in 1:4) hpue_list[[i]][hpue_list[[i]] == 0] <- NA
		hpue_base <- sapply(1:4, function(x) colMeans(hpue_list[[x]], na.rm = TRUE))
		hpue_base <- sapply(1:4, function(x) mean(hpue_base[21:25,x]))


## extracting values from alternative treatments and scenarios
# not the sensitivity analyses, the results with regular input settings
	depl_abs <- matrix(NA, nrow = 4, ncol = 34)
	hcpue_abs <- matrix(NA, nrow = 4, ncol = 34)
	hpue_abs <- matrix(NA, nrow = 4, ncol = 34)

	for(j in 1:4){
		# depletion
			eggs_list <- sapply(1:34, function(x) sum(scen_str_alt[[j]][[x]]$results$eggs_mon[289:300]))
			depl <- eggs_list/eggs0
		# hcpue (open months, last 5 years)
	        hcpue_list <- lapply(1:34, function(x) matrix(scen_str_alt[[j]][[x]]$results$hcpue, nrow = 12, ncol = 25))
	        for(i in 1:34) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
	        hcpue <- sapply(1:34, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
	        hcpue <- sapply(1:34, function(x) mean(hcpue[21:25,x]))    
	    # hpue (open montths, last 5 years)
	       hpue_list <- lapply(1:34, function(x) matrix(scen_str_alt[[j]][[x]]$results$hpue, nrow = 12, ncol = 25))
	        for(i in 1:34) hpue_list[[i]][hpue_list[[i]] == 0] <- NA
	        hpue <- sapply(1:34, function(x) colMeans(hpue_list[[x]], na.rm = TRUE))
	        hpue <- sapply(1:34, function(x) mean(hpue[21:25, x])) 
	        hpue[1] <- 0 # "unfished" scenario
    	# combine all results to get abs values
	        depl_abs[j,] <- sapply(1:34, function(x) depl[x])
	        hcpue_abs[j,] <- sapply(1:34, function(x) hcpue[x])
	        hpue_abs[j,] <- sapply(1:34, function(x) hpue[x])
  	}

  	bar_res <- list(depl_abs = depl_abs,
                    hcpue_abs = hcpue_abs,
                    hpue_abs = hpue_abs)


# combine results of main 10 scenarios and alternative 24 scenarios (with regular input settings)
# leave "space" between main and alternative scenarios
depl_res <- cbind(depl_base, bar_res$depl_abs[,c(2:10)], rep(0,4), bar_res$depl_abs[,c(11:34)])
hpue_res <- cbind(hpue_base, bar_res$hpue_abs[,c(2:10)], rep(0,4), bar_res$hpue_abs[,c(11:34)])
hcpue_res <- cbind(hcpue_base, bar_res$hcpue_abs[,c(2:10)], rep(0,4), bar_res$hcpue_abs[,c(11:34)])
  	colnames(depl_res) <- NULL
  	colnames(hpue_res) <- NULL
  	colnames(hcpue_res) <- NULL

 ## sensitivity analysis plots
	# M
		# depl
  		eggs_list_base <- sapply(base_runs_M, function(y) lapply(y, function(x) sum(x$results$eggs_mon[289:300])/eggs0))
  		eggs_list_low <- sapply(runs_M, function(y) lapply(y, function(x) sum(x[[1]]$results$eggs_mon[289:300])/eggs0))
  		eggs_list_high <- sapply(runs_M, function(y) lapply(y, function(x) sum(x[[2]]$results$eggs_mon[289:300])/eggs0))
    	# hcpue
    	hcpue_list_base <- matrix(nrow = 2, ncol = 4)
    		mat <- lapply(base_runs_M, function(y) lapply(y, function(x) matrix(x$results$hcpue, nrow = 12, ncol = 25)))
    		for(j in 1:2){
    			for(i in 1:4){
    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
    				hcpue_list_base[j,i] <- mean(mat[[i]][[j]][21:25])
    			}
    		}
    	hcpue_list_low <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_M, function(y) lapply(y, function(x) matrix(x[[1]]$results$hcpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hcpue_list_low[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}
    	hcpue_list_high <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_M, function(y) lapply(y, function(x) matrix(x[[2]]$results$hcpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hcpue_list_high[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}

    	# hpue
    	hpue_list_base <- matrix(nrow = 2, ncol = 4)
    		mat <- lapply(base_runs_M, function(y) lapply(y, function(x) matrix(x$results$hpue, nrow = 12, ncol = 25)))
    		for(j in 1:2){
    			for(i in 1:4){
    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)[21:25]
    				hpue_list_base[j,i] <- mean(mat[[i]][[j]])
    			}
    		}
    	hpue_list_low <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_M, function(y) lapply(y, function(x) matrix(x[[1]]$results$hpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hpue_list_low[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}
    	hpue_list_high <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_M, function(y) lapply(y, function(x) matrix(x[[2]]$results$hpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hpue_list_high[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}
	  
	    # all
		eggs_list_M_low <- rbind(eggs_list_base[1,], eggs_list_low[2:34,])
    	eggs_list_M_high <- rbind(eggs_list_base[2,], eggs_list_high[2:34,])
	  	hcpue_list_M_low <- rbind(hcpue_list_base[1,], hcpue_list_low[2:34,])
    	hcpue_list_M_high <- rbind(hcpue_list_base[2,], hcpue_list_high[2:34,])
 		hpue_list_M_low <- rbind(hpue_list_base[1,], hpue_list_low[2:34,])
    	hpue_list_M_high <- rbind(hpue_list_base[2,], hpue_list_high[2:34,])

 	# CR
  		# depl
  		eggs_list_base <- sapply(base_runs_CR, function(y) lapply(y, function(x) sum(x$results$eggs_mon[289:300])/eggs0))
  		eggs_list_low <- sapply(runs_CR, function(y) lapply(y, function(x) sum(x[[1]]$results$eggs_mon[289:300])/eggs0))
  		eggs_list_high <- sapply(runs_CR, function(y) lapply(y, function(x) sum(x[[2]]$results$eggs_mon[289:300])/eggs0))

    	# hcpue
    	hcpue_list_base <- matrix(nrow = 2, ncol = 4)
    		mat <- lapply(base_runs_CR, function(y) lapply(y, function(x) matrix(x$results$hcpue, nrow = 12, ncol = 25)))
    		for(j in 1:2){
    			for(i in 1:4){
    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)[21:25]
    				hcpue_list_base[j,i] <- mean(mat[[i]][[j]])
    			}
    		}
    	hcpue_list_low <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_CR, function(y) lapply(y, function(x) matrix(x[[1]]$results$hcpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hcpue_list_low[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}
    	hcpue_list_high <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_CR, function(y) lapply(y, function(x) matrix(x[[2]]$results$hcpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hcpue_list_high[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}

    	# hpue
    	hpue_list_base <- matrix(nrow = 2, ncol = 4)
    		mat <- lapply(base_runs_CR, function(y) lapply(y, function(x) matrix(x$results$hpue, nrow = 12, ncol = 25)))
    		for(j in 1:2){
    			for(i in 1:4){
    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)[21:25]
    				hpue_list_base[j,i] <- mean(mat[[i]][[j]])
    			}
    		}
    	hpue_list_curr_low <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_CR, function(y) lapply(y, function(x) matrix(x[[1]]$results$hpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hpue_list_low[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}
    	hpue_list_curr_high <- matrix(nrow = 34, ncol = 4)
	    	mat <- lapply(runs_CR, function(y) lapply(y, function(x) matrix(x[[2]]$results$hpue, nrow = 12, ncol = 25)))
	    		for(j in 1:34){
	    			for(i in 1:4){
	    				mat[[i]][[j]][mat[[i]][[j]]==0] <- NA
	    				mat[[i]][[j]] <- colMeans(mat[[i]][[j]], na.rm = TRUE)
	    				hpue_list_high[j,i] <- mean(mat[[i]][[j]][21:25])
	    			}
	    		}

	    # all
		eggs_list_CR_low <- rbind(eggs_list_base[1,], eggs_list_low[2:34,])
    	eggs_list_CR_high <- rbind(eggs_list_base[2,], eggs_list_high[2:34,])
	  	hcpue_list_CR_low <- rbind(hcpue_list_base[1,], hcpue_list_low[2:34,])
    	hcpue_list_CR_high <- rbind(hcpue_list_base[2,], hcpue_list_high[2:34,])
 		hpue_list_CR_low <- rbind(hpue_list_base[1,], hpue_list_low[2:34,])
    	hpue_list_CR_high <- rbind(hpue_list_base[2,], hpue_list_high[2:34,])
      


###-----------------------------------------------------
#		Bar plots
###-----------------------------------------------------
m <- matrix(c(1:3), nrow = 1, ncol = 3, byrow = TRUE)
col <- colorRampPalette(brewer.pal(9,"Blues"))(35)
Et_names <- c("base effort", NA,"double effort",NA)
exploit_names <- c("low", "high",NA,NA)
col_25 <- c("red","orange")

## M
    if(export_plots) tiff(filename=paste0(wd,"/depl_appendix_M.tiff"), height = 17, width = 30, units = 'cm', compression = "lzw", res = 500)   
	    par(mfrow=c(2,2), oma = c(2,2,2,0))
	    for(i in 1:4){
	    	if(i %in% c(1,3)) par(mar = c(3,4,1.5,0.5))
	    	if(i %in% c(2,4)) par(mar = c(3,2,1.5,1))
	    	b <- barplot(depl_res[i,], axes = FALSE, ylim = c(0,1.2), col = col)
	    	abline(v=12.75, lwd = 3)
	    	axis(1,at=b[,1],tck=-0.01,labels=FALSE)
		    text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
		         c(1:10,"",11:34), srt=60, adj=c(1,0),xpd=NA)
	    	axis(2, at = seq(0,1,0.5), las = 1)
	    	if(i %in% c(1,3)) title(ylab = Et_names[i], line = 2.6)
	    	if(i %in% c(1,2)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
    		num <- c(1:10,12:35)
    		if(i == 3) title(main = "Exploitation level", font.main = 1, cex.main = 0.94, line = 0.9, outer = TRUE)
		    for(j in 1:34){
		    	if(i == 1){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_low[j,1]), y1 = as.numeric(eggs_list_M_low[j,1]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_high[j,1]), y1 = as.numeric(eggs_list_M_high[j,1]), col=col_25[2], lwd = 2.5)
		    	} 
		    	if(i == 2){
    		 		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_low[j,2]), y1 = as.numeric(eggs_list_M_low[j,2]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_high[j,2]), y1 = as.numeric(eggs_list_M_high[j,2]), col=col_25[2], lwd = 2.5)
		    	}
				if(i == 3){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_low[j,3]), y1 = as.numeric(eggs_list_M_low[j,3]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_high[j,3]), y1 = as.numeric(eggs_list_M_high[j,3]), col=col_25[2], lwd = 2.5)
		    	}
		    	if(i == 4){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_low[j,4]), y1 = as.numeric(eggs_list_M_low[j,4]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_M_high[j,4]), y1 = as.numeric(eggs_list_M_high[j,4]), col=col_25[2], lwd = 2.5)
		    	}
		    }
	    }
	    mtext("Spawning output (eggs/eggs0)", font.main = 1, adj = 0.5, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
	    mtext("Management regulations", font.main = 1, adj = 0.5, cex = 0.8, side = 1, line = 0.6, outer =  TRUE)
	  if(export_plots) dev.off()

	if(export_plots) tiff(filename=paste0(wd,"/hpue_appendix_M.tiff"), height = 17, width = 30, units = 'cm', compression = "lzw", res = 500)
	    par(mfrow=c(2,2), oma= c(2,2,2,0))
	    for(i in 1:4){
	    	if(i %in% c(1,3)) par(mar = c(3,4,1.5,0.5))
	    	if(i %in% c(2,4)) par(mar = c(3,2,1.5,1))	
    		b <- barplot(hpue_res[i,], axes = FALSE, ylim = c(0,1.55), col = col)
	    	abline(v=12.75, lwd = 3)
	    	axis(1,at=b[,1],tck=-0.01,labels=FALSE)
		    text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
		         c(1:10,"",11:34), srt=60, adj=c(1,0),xpd=NA)
	    	axis(2, at = seq(0,1.5,0.5), las = 1)
	    	if(i %in% c(1,3)) title(ylab = Et_names[i], line = 2.6)
	    	if(i %in% c(1,2)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
    		num <- c(1:10,12:35)
    		if(i == 3) title(main = "Exploitation level", font.main = 1, cex.main = 0.94, line = 0.9, outer = TRUE)
    		for(j in 1:34){
		    	if(i == 1){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_low[j,1]), y1 = as.numeric(hpue_list_M_low[j,1]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_high[j,1]), y1 = as.numeric(hpue_list_M_high[j,1]), col=col_25[2], lwd = 2.5)
		    	} 
		    	if(i == 2){
    		 		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_low[j,2]), y1 = as.numeric(hpue_list_M_low[j,2]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_high[j,2]), y1 = as.numeric(hpue_list_M_high[j,2]), col=col_25[2], lwd = 2.5)
		    	}
				if(i == 3){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_low[j,3]), y1 = as.numeric(hpue_list_M_low[j,3]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_high[j,3]), y1 = as.numeric(hpue_list_M_high[j,3]), col=col_25[2], lwd = 2.5)
		    	}
		    	if(i == 4){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_low[j,4]), y1 = as.numeric(hpue_list_M_low[j,4]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_M_high[j,4]), y1 = as.numeric(hpue_list_M_high[j,4]), col=col_25[2], lwd = 2.5)
		    	}
		    }
	    }
	    mtext("Harvest per unit of effort (gal/person)", font.main = 1, adj = 0.5, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
	    mtext("Management regulations", font.main = 1, adj = 0.5, cex = 0.8, side = 1, line = 0.6, outer =  TRUE)
	if(export_plots) dev.off()

	if(export_plots) tiff(filename=paste0(wd,"/hcpue_appendix_M.tiff"), height = 17, width = 30, units = 'cm', compression = "lzw", res = 500)
	    par(mfrow=c(2,2), oma= c(2,2,2,0))
	    for(i in 1:4){
	    	if(i %in% c(1,3)) par(mar = c(3,4,1.5,0.5))
	    	if(i %in% c(2,4)) par(mar = c(3,2,1.5,1))   	
    		b <- barplot(hcpue_res[i,], axes = FALSE, ylim = c(0,2), col = col)
	    	abline(v=12.75, lwd = 3)
	    	axis(1,at=b[,1],tck=-0.01,labels=FALSE)
		    text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
		         c(1:10,"",11:34), srt=60, adj=c(1,0),xpd=NA)
    		axis(2, at = seq(0,3.5,0.5), las = 1)
	    	if(i %in% c(1,3)) title(ylab = Et_names[i], line = 2.6)
	    	if(i %in% c(1,2)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
			num <- c(1:10,12:35)
			if(i == 3) title(main = "Exploitation level", font.main = 1, cex.main = 0.94, line = 0.9, outer = TRUE)
			for(j in 1:34){
		    	if(i == 1){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_low[j,1]), y1 = as.numeric(hcpue_list_M_low[j,1]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_high[j,1]), y1 = as.numeric(hcpue_list_M_high[j,1]), col=col_25[2], lwd = 2.5)
		    	} 
		    	if(i == 2){
    		 		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_low[j,2]), y1 = as.numeric(hcpue_list_M_low[j,2]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_high[j,2]), y1 = as.numeric(hcpue_list_M_high[j,2]), col=col_25[2], lwd = 2.5)
		    	}
				if(i == 3){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_low[j,3]), y1 = as.numeric(hcpue_list_M_low[j,3]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_high[j,3]), y1 = as.numeric(hcpue_list_M_high[j,3]), col=col_25[2], lwd = 2.5)
		    	}
		    	if(i == 4){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_low[j,4]), y1 = as.numeric(hcpue_list_M_low[j,4]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_M_high[j,4]), y1 = as.numeric(hcpue_list_M_high[j,4]), col=col_25[2], lwd = 2.5)
		    	}
		    }
    	}
    mtext("Catch rate", font.main = 1, adj = 0.5, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
    mtext("Management regulations", font.main = 1, adj = 0.5, cex = 0.8, side = 1, line = 0.6, outer =  TRUE)
    if(export_plots) dev.off()


## CR
  if(export_plots) tiff(filename=paste0(wd,"/depl_appendix_CR.tiff"), height = 17, width = 30, units = 'cm', compression = "lzw", res = 500)   
    par(mfrow=c(2,2), oma= c(2,2,2,0))
	    for(i in 1:4){
	    	if(i %in% c(1,3)) par(mar = c(3,4,1.5,0.5))
	    	if(i %in% c(2,4)) par(mar = c(3,2,1.5,1))
	    	b <- barplot(depl_res[i,], axes = FALSE, ylim = c(0,1.2), col = col)
	    	abline(v=12.75, lwd = 3)
	    	axis(1,at=b[,1],tck=-0.01,labels=FALSE)
		    text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
		         c(1:10,"",11:34), srt=60, adj=c(1,0),xpd=NA)
	    	axis(2, at = seq(0,1,0.5), las = 1)
	    	if(i %in% c(1,3)) title(ylab = Et_names[i], line = 2.6)
	    	if(i %in% c(1,2)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
    		num <- c(1:10,12:35)
    		if(i == 3) title(main = "Exploitation level", font.main = 1, cex.main = 0.94, line = 0.9, outer = TRUE)
    		for(j in 1:34){
		    	if(i == 1){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_low[j,1]), y1 = as.numeric(eggs_list_CR_low[j,1]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_high[j,1]), y1 = as.numeric(eggs_list_CR_high[j,1]), col=col_25[2], lwd = 2.5)
		    	} 
		    	if(i == 2){
    		 		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_low[j,2]), y1 = as.numeric(eggs_list_CR_low[j,2]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_high[j,2]), y1 = as.numeric(eggs_list_CR_high[j,2]), col=col_25[2], lwd = 2.5)
		    	}
				if(i == 3){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_low[j,3]), y1 = as.numeric(eggs_list_CR_low[j,3]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_high[j,3]), y1 = as.numeric(eggs_list_CR_high[j,3]), col=col_25[2], lwd = 2.5)
		    	}
		    	if(i == 4){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_low[j,4]), y1 = as.numeric(eggs_list_CR_low[j,4]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(eggs_list_CR_high[j,4]), y1 = as.numeric(eggs_list_CR_high[j,4]), col=col_25[2], lwd = 2.5)
		    	}
		    }
    	}
	mtext("Spawning output (eggs/eggs0)", font.main = 1, adj = 0.5, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
	mtext("Management regulations", font.main = 1, adj = 0.5, cex = 0.8, side = 1, line = 0.6, outer =  TRUE)
	if(export_plots) dev.off()

	if(export_plots) tiff(filename=paste0(wd,"/hpue_appendix_CR.tiff"), height = 17, width = 30, units = 'cm', compression = "lzw", res = 500)
	    par(mfrow=c(2,2), oma= c(2,2,2,0))
	    for(i in 1:4){
	    	if(i %in% c(1,3)) par(mar = c(3,4,1.5,0.5))
	    	if(i %in% c(2,4)) par(mar = c(3,2,1.5,1))	
    		b <- barplot(hpue_res[i,], axes = FALSE, ylim = c(0,1.3), col = col)
	    	abline(v=12.75, lwd = 3)
	    	axis(1,at=b[,1],tck=-0.01,labels=FALSE)
		    text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
		         c(1:10,"",11:34), srt=60, adj=c(1,0),xpd=NA)
	    	axis(2, at = seq(0,1,0.5), las = 1)
	    	if(i %in% c(1,3)) title(ylab = Et_names[i], line = 2.6)
	    	if(i %in% c(1,2)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
    		num <- c(1:10,12:35)
    		if(i == 3) title(main = "Exploitation level", font.main = 1, cex.main = 0.94, line = 0.9, outer = TRUE)
    		for(j in 1:34){
		    	if(i == 1){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_low[j,1]), y1 = as.numeric(hpue_list_CR_low[j,1]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_high[j,1]), y1 = as.numeric(hpue_list_CR_high[j,1]), col=col_25[2], lwd = 2.5)
		    	} 
		    	if(i == 2){
    		 		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_low[j,2]), y1 = as.numeric(hpue_list_CR_low[j,2]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_high[j,2]), y1 = as.numeric(hpue_list_CR_high[j,2]), col=col_25[2], lwd = 2.5)
		    	}
				if(i == 3){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_low[j,3]), y1 = as.numeric(hpue_list_CR_low[j,3]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_high[j,3]), y1 = as.numeric(hpue_list_CR_high[j,3]), col=col_25[2], lwd = 2.5)
		    	}
		    	if(i == 4){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_low[j,4]), y1 = as.numeric(hpue_list_CR_low[j,4]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hpue_list_CR_high[j,4]), y1 = as.numeric(hpue_list_CR_high[j,4]), col=col_25[2], lwd = 2.5)
		    	}
		    }
    	}
    mtext("Harvest per unit of effort (gal/person)", font.main = 1, adj = 0.5, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
    mtext("Management regulations", font.main = 1, adj = 0.5, cex = 0.8, side = 1, line = 0.6, outer =  TRUE)
    if(export_plots) dev.off()

	if(export_plots)tiff(filename=paste0(wd,"/hcpue_appendix_CR.tiff"), height = 17, width = 30, units = 'cm', compression = "lzw", res = 500)
	    par(mfrow=c(2,2), oma= c(2,2,2,0))
	    for(i in 1:4){
	    	if(i %in% c(1,3)) par(mar = c(3,4,1.5,0.5))
	    	if(i %in% c(2,4)) par(mar = c(3,2,1.5,1))   	
    		b <- barplot(hcpue_res[i,], axes = FALSE, ylim = c(0,2), col = col)
	    	abline(v=12.75, lwd = 3)
	    	axis(1,at=b[,1],tck=-0.01,labels=FALSE)
		    text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
		         c(1:10,"",11:34), srt=60, adj=c(1,0),xpd=NA)
    		axis(2, at = seq(0,2,0.5), las = 1)
	    	if(i %in% c(1,3)) title(ylab = Et_names[i], line = 2.6)
	    	if(i %in% c(1,2)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
			num <- c(1:10,12:35)
			if(i == 3) title(main = "Exploitation level", font.main = 1, cex.main = 0.94, line = 0.9, outer = TRUE)
			for(j in 1:34){
		    	if(i == 1){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_low[j,1]), y1 = as.numeric(hcpue_list_CR_low[j,1]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_high[j,1]), y1 = as.numeric(hcpue_list_CR_high[j,1]), col=col_25[2], lwd = 2.5)
		    	} 
		    	if(i == 2){
    		 		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_low[j,2]), y1 = as.numeric(hcpue_list_CR_low[j,2]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_high[j,2]), y1 = as.numeric(hcpue_list_CR_high[j,2]), col=col_25[2], lwd = 2.5)
		    	}
				if(i == 3){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_low[j,3]), y1 = as.numeric(hcpue_list_CR_low[j,3]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_high[j,3]), y1 = as.numeric(hcpue_list_CR_high[j,3]), col=col_25[2], lwd = 2.5)
		    	}
		    	if(i == 4){
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_low[j,4]), y1 = as.numeric(hcpue_list_CR_low[j,4]), col=col_25[1], lwd = 2.5)
		    		segments(x0 = b[num[j],]-0.5, x1 = b[num[j],]+0.5, y0 = as.numeric(hcpue_list_CR_high[j,4]), y1 = as.numeric(hcpue_list_CR_high[j,4]), col=col_25[2], lwd = 2.5)
		    	}
		    }
    	}
    mtext("Catch rate", font.main = 1, adj = 0.5, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
    mtext("Management regulations", font.main = 1, adj = 0.5, cex = 0.8, side = 1, line = 0.6, outer =  TRUE)
    if(export_plots)dev.off()
