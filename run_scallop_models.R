#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#			Code to run all scenarios and models
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library("truncnorm")
# wd <- "" # set your working directory here
setwd(wd)
source("functions/scallop_model_fun.R") # scallop model function
source("functions/plot_models.R") # all plotting functions
source("functions/tune_calibrate.R") # for tuning and calibrating the models 

# list of inputs to the model (this list already runs with the model)
scenario <- list( 
  life = list(amax = 18, 								# maximum age in the model, this is not a plus group (we just kill all scallops after this month)
              Ro = 3e7, 								# Unfished Recruitment, set at 30 million 
              CR = 8, 									# Compensation Ratio, Arbitrarily set
              vbk = 1/3, 								# Von-Bertalanffy K
              vblinf = 60, 								# Von-Bertalanffy L-infinity 
              vbt0 = 0, 								# Von-Bertalanffy t0
              alw = 0.0001,								# Weight-Length a
              alwb = 3, 								# Weight-Length b
              amat = 5.5, 								# Age at 50% maturity
              mgr = 2.5, 								# Maturity logistic function growth rate
              vlh = 35, 								# Vulnerability logistic param, size at 50% selected
              vgr = 0.5, 								# Vulnerability logistic param, growth rate
              M = 0.25, 								# Natural Mortality, note this is a Monthly Instantaneous Rate
              lorenzc = 1, 								# Lorenzen exponent for Lorenzen M
              # Probability of Scallop Spawning across any age
              prob_spawn = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.09,0.125,0.09,0.07,0.05),
              semelparous=TRUE),
  catch = list( q_flag = "constant", 							# Catchability Option______constant or VB
                # Effort in each month; merges season open/close
                E_max  = sum(c(11415,6929,4255))*3.8, 					# maximum number vessels, need to multiply by 3.8 to get persons
                E_cap = FALSE, 								# this is a switch to cap effort @ emax or to allow it to scale following the observed effort decline from Granneman paper
                E_con = FALSE, 								# this is a switch for constant effort
                E_open = c(0,0,0,0,0,0,1,1,1,0,0,0), 					# this is a vector of months that tells the model where to apply the effort. The effort does not need to be sequential as the eff_spread function will allocate the effort in each month
                q = 0.0000319/3.8, 							# Catchability, Granneman paper - (0.0000319) value is per vessel so divide by 3.8 to get persons (off by one-order of magnitude for scaling)
                qmax = .0005, 								# Max Catchability (for q varying with VB)
                bag_unit = "gallon", 							# Bag limit Units_____gallon or numbers
                bag = rep(2,12), 							# Bag limit________rep(2,12) for constant
                E_years = seq(1,1,length.out=25) 					# Effort over years_________rep(1,10) for constant
  ), 
  sim = list(month = 300) 								# Simulation length, number of months
)

###-----------------------------------------------------
#		Base model
###-----------------------------------------------------
# run model with baseline scenario, no additional management actions
# 3 month harvest (July-September)
# 2 gal bag limit
base <- scallop_model_fun(scenario)
# plot 4x4 life history (length at age, weight at age, maturity at age, natural mortality at age)
LH_plot(scenario) 
# plot 2x3 time series (vulnerable biomass, depletion (in numbers), effort, yield, recruits, and eggs)
time_series_plot(base)



