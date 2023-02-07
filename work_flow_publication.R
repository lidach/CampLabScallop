#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#		Code to run all scenarios and models
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# not required to run
# work flow of the publication:
	# tune R0
	# set up 10 management scenarios
	# run models with various levels of exploitation levels
	# extract absolute and relative values

library(truncnorm)
require(RColorBrewer)
# wd <- "" # set your working directory here, where all the code is
setwd(wd)
source("functions/scallop_model_fun.R") # scallop model function

# export plots to local directory
export_plots <- FALSE



###-----------------------------------------------------
#		Tune R0
###-----------------------------------------------------
source("functions/tune_calibrate.R") # for tuning and calibrating the models 
# minimize estimated average gallons of scallops per person (hpue) and catchability estimate from Granneman et al. (2021)
	scenario$sim$run <- FALSE
	(R0.hunt <- r0_hunter(hpue=0.81, upper.bnd=3, scenario = scenario))
	scenario$life$Ro <- R0.hunt$newR0 #input new R0 to get the desired probability of hitting the harvest rate
	# unfished scenario
	unf <- scenario
	unf$catch_eq$bag <- rep(0.001,12) # very low bag limit - "no fishing"
	unf <- scallop_model_fun(unf)
	eggs0 <- sum(unf$results$eggs_mon[1:12])
	

###-----------------------------------------------------
#		Management scenarios
###-----------------------------------------------------
# 10 scenarios
# base - 3 month harvest season July-September, 2 gallon bag limit
# increased bag - 3 month season July-September, 3 gallon bag limit
# decreased bag - 3 month season July-September, 1 gallon bag limit
# increased season (later) - four month season July-October, 2 gallon bag limit
# decreased season (earlier) - four month season June-September, 2 gallon bag limit
# later season - 3 month season August-October, 2 gallon bag limit
# earlier season - 3 month season June-August, 2 gallon bag limit
# rolling bag limit, 3 month season July-September, 0.5-> 1 -> 2 gallon bag limit
# rolling bag limit and later season, 3 month season August-October, 0.5-> 1 -> 2 gallon bag limit
# rolling bag limit and earlier season, 3 month season June-August, 0.5-> 1 -> 2 gallon bag limit

	mgmt_scen <- rep(list(list(bag=NULL,E_open=NULL,E_cap=NULL)),10)
	mgmt_scen[[1]]$bag <- rep(0.001,12) # Decrease Bag, Basically unfished scenario (not in graphs/outputs)
	mgmt_scen[[2]]$bag <- rep(3,12) # bag limit = 3
	mgmt_scen[[3]]$bag <- rep(1,12) # bag limit = 1
	mgmt_scen[[4]]$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0) # season length, extend one month forward
	mgmt_scen[[5]]$E_open <- c(0,0,0,0,0,1,1,1,1,0,0,0) # season length, extend one month back
	mgmt_scen[[6]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0) # push season back one month
	mgmt_scen[[7]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0) # push season forward one month
	mgmt_scen[[8]]$bag <- c(1,1,1,1,1,1,1,2,2,2,2,2) # rolling bag limit - increasing
	mgmt_scen[[9]]$bag <- c(1,1,1,1,1,1,1,1,2,2,2,2) # rolling bag and season start - back one month
	mgmt_scen[[9]]$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
	mgmt_scen[[10]]$bag <- c(1,1,1,1,1,1,2,2,2,2,2,2)# rolling bag and season start - forward one month
	mgmt_scen[[10]]$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)


###-----------------------------------------------------
#		Various exploitation levels
###-----------------------------------------------------
# run mgmt scenarios with different starting q (i.e. different exploitation levels)
	E1 <- seq(1,2,length.out=25) # effort - increasing between years (doubling over 25 years)
	E_seq <- list(NULL,NULL,NULL,E1,E1,E1)
	spr_seq <- c(0.5,0.35,0.2) #desired SPRs
	
	perchange_seq <- sapply(spr_seq, function(ii) q_hunter(spr=ii, E=NULL))
	perchange_seq <- rep(perchange_seq, 3)
	#base scenarios
	mgmt_base_q <- lapply(1:length(E_seq), function(x){mgmt_scen[[1]]$E <- E_seq[[x]]; mgmt_scen[[1]]$perchange <- perchange_seq[[x]]; mgmt_scen[[1]]$bag <- NULL; return(mgmt_scen[[1]])})
  	scen_str_base <- lapply(mgmt_base_q, function(x) mgmt_q_runner(perchange=x$perchange, E=x$E, run=TRUE))

  	sapply(scen_str_base, function(x) x$scenario$per.rec$spr)
  	sapply(scen_str_base, function(x){v <- x$results$eggs; v <- v[v!=0]; return(v[length(v)]/eggs0)})

    #mgmt scenarios
    mgmt_scen_q <- lapply(1:length(E_seq),function(x)lapply(mgmt_scen,function(y) {y$E <- E_seq[[x]]; y$perchange <- perchange_seq[[x]]; return(y)}))

    scen_str_all <- lapply(mgmt_scen_q, function(y) lapply(y,function(x)mgmt_q_runner(perchange=x$perchange,run=TRUE,E=x$E,bag=x$bag,E_open=x$E_open,E_cap=x$E_cap)))
   


###-----------------------------------------------------
#		For creation of bar plots
###-----------------------------------------------------
# extracting base lines values (from scen_str_base)
# absolute and relative values of spawning output (depl) and harvest per unit of effort (hpue)
	# eggs0 above	
	eggs_list <- sapply(1:6, function(x) sum(scen_str_base[[x]]$results$eggs_mon[289:300]))
		depl_base <- eggs_list/eggs0
	hpue_list <- lapply(1:6, function(x) matrix(scen_str_base[[x]]$results$hpue, nrow = 12, ncol = 25))
		for(i in 1:6) hpue_list[[i]][hpue_list[[i]] == 0] <- NA
		hpue_base <- sapply(1:6, function(x) colMeans(hpue_list[[x]], na.rm = TRUE))
		hpue_base <- sapply(1:6, function(x) mean(hpue_base[21:25,x]))

## extracting values from all treatments and scenarios
	depl_rel <- matrix(NA, nrow = 6, ncol = 10)
	hcpue_rel <- matrix(NA, nrow = 6, ncol = 10)
	hpue_rel <- matrix(NA, nrow = 6, ncol = 10)
	depl_abs <- matrix(NA, nrow = 6, ncol = 10)
	hcpue_abs <- matrix(NA, nrow = 6, ncol = 10)
	hpue_abs <- matrix(NA, nrow = 6, ncol = 10)

	for(j in 1:6){
		# depletion
			# eggs0 <- sapply(1:12, function(x) scen_str_all[[j]][[x]]$results$eggs[12])
			eggs_list <- sapply(1:10, function(x) sum(scen_str_all[[j]][[x]]$results$eggs_mon[289:300]))
			depl <- eggs_list/eggs0
	    # hpue (open months, last 5 years)
	       hpue_list <- lapply(1:10, function(x) matrix(scen_str_all[[j]][[x]]$results$hpue, nrow = 12, ncol = 25))
	        for(i in 1:10) hpue_list[[i]][hpue_list[[i]] == 0] <- NA
	        hpue <- sapply(1:10, function(x) colMeans(hpue_list[[x]], na.rm = TRUE))
	        hpue <- sapply(1:10, function(x) mean(hpue[21:25, x])) 
	        hpue[1] <- 0 # "unfished" scenario
	    # combine all results to get relative errors
	        depl_rel[j,] <- sapply(1:10, function(x) (depl[x]-depl_base[j])/depl_base[j])
	        hpue_rel[j,] <- sapply(1:10, function(x) (hpue[x]-hpue_base[j])/hpue_base[j])
	    # combine all results to get abs values
	        depl_abs[j,] <- sapply(1:10, function(x) depl[x])
	        hpue_abs[j,] <- sapply(1:10, function(x) hpue[x])
  	}
  	depl_abs <- cbind(depl_base, depl_abs)
  	hpue_abs <- cbind(hpue_base, hpue_abs)
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hpue_abs) <- NULL

  	bar_res <- list(depl_rel = depl_rel,
                    hpue_rel = hpue_rel,
                    depl_abs = depl_abs,
                    hpue_abs = hpue_abs)



###-----------------------------------------------------
#		Bar plots in the publication
###-----------------------------------------------------
scen_names <- c("base", "incr. bag", "decr. bag", "incr. szn. (later)", "incr. szn. (earlier)",
                "later szn.", "earlier szn.", "rolling bag", "rolling bag later szn.", "rolling bag earlier szn.")
col <- colorRampPalette(brewer.pal(9,"Blues"))(length(scen_names))
m <- matrix(c(1:6), nrow = 2, ncol = 3, byrow = TRUE)
exploit_names <- c("low", "moderate", "high")
Et_names <- c("base effort", NA,NA,"double effort")



## Spawning output
with(bar_res,{
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hpue_abs) <- NULL

    # export plots?
    if(export_plots)  tiff(filename = file.path(wd ,"/spawning_output_res.tiff"), height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)

    layout(mat = m, heights = c(0.25,0.4))
    par(oma=c(0,2,0,0))
    for(i in 1:6){
      if(i == 1) par(mar=c(1.1,3.2,2.5,0))
      if(i %in% c(2,3)) par(mar = c(1.1,2.2,2.5,1))
      if(i %in% 4) par(mar = c(11,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(11,2.2,1,1))

      if(i %in% c(1:3)) barplot(depl_abs[i,c(1,3:11)], axes = FALSE, ylim = c(0,1), col = col) # removed "unfished" scenario for now
      if(i %in% c(4:6)){
        b <- barplot(depl_abs[i,c(1,3:11)], axes = FALSE, cex.lab = 0.8, ylim = c(0,1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names, srt=60, adj=c(1,0),xpd=NA)
      }
      mtext(letters[i], line = -1, adj = 0.05, cex = 0.9, las = 1, col = "grey30")
      axis(2, at = seq(0,1,0.5), las = 1)
      if(i %in% c(1,4)) title(ylab = Et_names[i], line = 2.4)

      if(i %in% c(1,2,3)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "Exploitation level", font.main = 1, line = 1.55)
      mtext("Spawning output (eggs/eggs0)", font.main = 1, adj = 0.63, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
    }

	if(export_plots) dev.off()

 })

## Harvest per unit of effort
with(bar_res,{
    if(export_plots)  tiff(filename = file.path(wd ,"/HPUE_res.tiff"), height = 14, width = 20, units = 'cm', compression = "lzw", res = 500)
    
    layout(mat = m, heights = c(0.25,0.4))
    par(oma=c(0,2,0,0))
    for(i in 1:6){
      if(i == 1) par(mar=c(1.1,3.2,2.5,0))
      if(i %in% c(2,3)) par(mar = c(1.1,2.2,2.5,1))
      if(i %in% 4) par(mar = c(11,3.2,1,0))
      if(i %in% c(5,6)) par(mar = c(11,2.2,1,1))

      if(i %in% c(1:3))barplot(hpue_abs[i,c(1,3:11)], axes = FALSE, ylim = c(0, max(hpue_abs[,c(1,3:11)])+0.1), col = col) # removed "unfished" scenario for now
      if(i %in% c(4:6)){
        b <- barplot(hpue_abs[i,c(1,3:11)], axes = FALSE, las = 2, cex.lab = 0.8, ylim = c(0, max(hpue_abs[,c(1,3:11)])+0.1), col = col)
        axis(1,at=b[,1],tck=-0.01,labels=FALSE)
        text(b[,1],par('usr')[3]-abs(diff(par('usr')[3:4]))*0.1,
             scen_names, srt=60, adj=c(1,0),xpd=NA)
      }
      mtext(letters[i], line = -1, adj = 0.05, cex = 0.9, las = 1, col = "grey30")
      axis(2, at = seq(0,1,0.5), las = 1)
      if(i %in% c(1,4)) title(ylab = Et_names[i], line = 2.4)

      if(i %in% c(1,2,3)) title(main = exploit_names[i], font.main = 1, line = -.01, cex = 0.8)
      if(i == 2) title(main = "Exploitation level", font.main = 1, line = 1.55)
      mtext("Harvest per unit of effort (gal/person)", font.main = 1, adj = 0.63, cex = 0.8, side = 2, line = 0.8, outer =  TRUE)
     }

	if(export_plots) dev.off()

})
