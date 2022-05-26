#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#			Demo code to run for scallop model
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(truncnorm)
library(tigris)
library(sp)
library(sf)
library(lwgeom)
# wd <- "" # set your working directory here, where all the code is
setwd(wd)
path <- paste0(wd,"/data/")

source("functions/scallop_model_fun.R") # scallop model function
source("functions/plot_models.R") # all plotting functions
source("functions/tune_calibrate.R") # for tuning and calibrating the models 
source("functions/map.R") # reading in data for map, specific to Florida, may take some time to load
plot_map <- TRUE # plot the map if TRUE (Florida, west Coast)

# list of inputs to the model (this list already runs with the model)
catch_list <- list(q_flag = "constant",                   # Catchability Option______constant or VB
                # Effort in each month; merges season open/close
                E_max = sum(c(11415,6929,4255))*3.8,      # maximum number vessels, need to multiply by 3.8 to get persons
                E_cap = FALSE,                            # this is a switch to cap effort @ emax or to allow it to scale following the observed effort decline from Granneman paper
                E_con = FALSE,                            # this is a switch for constant effort
                E_open = c(0,0,0,0,0,0,1,1,1,0,0,0),      # this is a vector of months that tells the model where to apply the effort. The effort does not need to be sequential as the eff_spread function will allocate the effort in each month
                q = 0.0000319/3.8,                        # Catchability, Granneman paper - (0.0000319) value is per vessel so divide by 3.8 to get persons (off by one-order of magnitude for scaling)
                qmax = .0005,                             # Max Catchability (for q varying with VB)
                bag_unit = "gallon",                      # Bag limit Units_____gallon or numbers
                bag = rep(2,12),                          # Bag limit________rep(2,12) for constant
                E_years = seq(1,1,length.out=25))         # Effort over years_________rep(1,10) for constant

scenario <- list( 
  life = list(amax = 18,                                  # maximum age in the model, this is not a plus group (we just kill all scallops after this month)
              Ro = 3e7,                                   # Unfished Recruitment, set at 30 million 
              CR = 8,                                     # Compensation Ratio, Arbitrarily set
              vbk = 1/3,                                  # Von-Bertalanffy K
              vblinf = 60,                                # Von-Bertalanffy L-infinity 
              vbt0 = 0,                                   # Von-Bertalanffy t0
              alw = 0.0001,                               # Weight-Length a
              alwb = 3,                                   # Weight-Length b
              amat = 5.5,                                 # Age at 50% maturity
              mgr = 2.5,                                  # Maturity logistic function growth rate
              vlh = 35,                                   # Vulnerability logistic param, size at 50% selected
              vgr = 0.5,                                  # Vulnerability logistic param, growth rate
              M = 0.25,                                   # Natural Mortality, note this is a Monthly Instantaneous Rate
              lorenzc = 1,                                # Lorenzen exponent for Lorenzen M
              # Probability of Scallop Spawning across any age
              prob_spawn = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.09,0.125,0.09,0.07,0.05),
              semelparous = TRUE),
  # for equilibrium
  catch_eq = catch_list, 
  # for actual model runs
  catch_run = catch_list,
  # Simulation length, number of months 
  sim = list(month_eq = 300,
             month_run = 300,
             run = TRUE)                                   
)


###-----------------------------------------------------
#   FL Bay Scallops regulation map
###-----------------------------------------------------
# map of Scallop regulation (west coast of Florida, Gulf -> Pasco counties)
if(plot_map == TRUE){
	  par(mar = c(1,2,0.1,0.1))
  plot(st_geometry(fl_state),
       xlim = c(-85.5, st_bbox(fl_cnt_sub)[3]), 
       ylim = c(28.75, 29.5), yaxs = 'i', xaxs = 'i')
  plot(st_geometry(fl_szone[-1,]),
       col = c('dodgerblue3', 'dodgerblue3', 'darkorange', 'dodgerblue3',
             'dodgerblue3'),
       add = T)
  plot(st_geometry(fl_cnt_sub), col = 'gray90', add = T)
  box()
  # axes
  long.ax <- pretty(par('usr')[c(1,2)])
  axis(1, at = long.ax[-length(long.ax)],
       labels = paste0(long.ax[-length(long.ax)], c(rep("",length(long.ax)-2),"ºW")),
       mgp = c(0,0.15,0), tck = -0.01, cex.axis = 0.9, font = 3)
  lat.ax <- pretty(par('usr')[c(3,4)])
  axis(2, at = lat.ax[-length(lat.ax)], las = 1,
       labels = paste0(lat.ax[-length(lat.ax)], c(rep("",length(lat.ax)-2),"ºN")),
       mgp = c(0,0.25,0), tck = -0.01, cex.axis = 0.9, font = 3)
  # table
  library(plotrix)
  addtable2plot(-85.35, 28.3,
                regs2[,-1], cex = 0.7)
  # labels
  text(lab.pts[,1], lab.pts[,2], LETTERS[1:5], font = 2, col = 'white',cex = 1.2)
  text(cnt.pts[,1], cnt.pts[,2],
       stringr::str_to_upper(c('Gulf','Franklin','Taylor','Dixie','Levy','Citrus','Hernando','Pasco')),
       cex = 0.5, adj = c(0,0))

  par(plt = c(0.8,0.995,0.7,0.9925), new = TRUE) 
  plot(fl_state,
       bg = 'white')
  box(bty = 'l')
  plot(st_as_sfc(bbox), border = 'firebrick1', lwd = 2, add = T)
}

dev.off()


###-----------------------------------------------------
#		Model runs
###-----------------------------------------------------
# Base model - run model with baseline scenario, no additional management actions
# 3 month harvest (July-September)
# 2 gal bag limit
base <- scallop_model_fun(scenario)

# plot 4x4 life history (length at age, weight at age, maturity at age, natural mortality at age)
LH_plot(scenario = scenario) 

# plot 2x3 time series (vulnerable biomass, depletion (in numbers), effort, yield, recruits, and eggs)
time_series_plot(base)


# Example - run with new R0 value
# hpue = 0.81
R0.hunt <- r0_hunter(hpue = 0.81, upper.bnd = 3)
print(c(R0.hunt$newR0, R0.hunt$hpue.mu))
ex1scen <- scenario
ex1scen$life$Ro <- R0.hunt$newR0 # input new R0 to get the desired probability of hitting the harvest rate
ex1 <- scallop_model_fun(ex1scen)


# Example - rolling bag limit (0.5 -> 1 -> 2 gallons)
ex2scen <- ex1scen
ex2scen$catch_run$bag <- c(1,1,1,1,1,1,0.5,1,1.5,2,2,2)
ex2 <- scallop_model_fun(ex2scen)