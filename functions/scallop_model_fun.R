#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#     Scallop model
#   
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Monthly model.... "age" in the following refers to months 
## list of inputs to the model (runs with the model)
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
  catch_eq = list(q_flag = "constant",                    # Catchability Option______constant or VB
                  # Effort in each month; merges season open/close
                  E_max  = sum(c(11415,6929,4255))*3.8,     # maximum number vessels, need to multiply by 3.8 to get persons
                  E_cap = FALSE,                            # this is a switch to cap effort @ emax or to allow it to scale following the observed effort decline from Granneman paper
                  E_con = FALSE,                            # this is a switch for constant effort
                  E_open = c(0,0,0,0,0,0,1,1,1,0,0,0),      # this is a vector of months that tells the model where to apply the effort. The effort does not need to be sequential as the eff_spread function will allocate the effort in each month
                  q = 0.0000319/3.8,                        # Catchability, Granneman paper - (0.0000319) value is per vessel so divide by 3.8 to get persons (off by one-order of magnitude for scaling)
                  qmax = .0005,                             # Max Catchability (for q varying with VB)
                  bag_unit = "gallon",                      # Bag limit Units_____gallon or numbers
                  bag = rep(2,12),                          # Bag limit________rep(2,12) for constant
                  E_years = seq(1,1,length.out=25)          # Effort over years_________rep(1,10) for constant
  ), 
  # for actual model runs
  catch_run = list(q_flag = "constant",                   # Catchability Option______constant or VB
                   # Effort in each month; merges season open/close
                   E_max  = sum(c(11415,6929,4255))*3.8,     # maximum number vessels, need to multiply by 3.8 to get persons
                   E_cap = FALSE,                            # this is a switch to cap effort @ emax or to allow it to scale following the observed effort decline from Granneman paper
                   E_con = FALSE,                            # this is a switch for constant effort
                   E_open = c(0,0,0,0,0,0,1,1,1,0,0,0),      # this is a vector of months that tells the model where to apply the effort. The effort does not need to be sequential as the eff_spread function will allocate the effort in each month
                   q = 0.0000319/3.8,                        # Catchability, Granneman paper - (0.0000319) value is per vessel so divide by 3.8 to get persons (off by one-order of magnitude for scaling)
                   qmax = .0005,                             # Max Catchability (for q varying with VB)
                   bag_unit = "gallon",                      # Bag limit Units_____gallon or numbers
                   bag = rep(2,12),                          # Bag limit________rep(2,12) for constant
                   E_years = seq(1,1,length.out=25)          # Effort over years_________rep(1,10) for constant
  ), 
  sim = list(month_eq = 300,
             month_run = 300,
             run = TRUE)                                   # Simulation length, number of months
)



## function to convert shell height (sh) to gallons (gal)
sh2gal <- function(sh){
  (-6.704*sh+480.96)/2
}

## function to spread effort across the open months
eff_spread <- function(E_max, E_open, E_cap, E_con){
  if(E_con){
    effort <- E_open
    effort[E_open == 1] <- E_max/sum(E_open)
  }else{
    coef <- coef(lm(log(c(11415,6929,4255)*3.8) ~ seq(1,3)))      # fit exponential decay model to effort decline
    pred <- exp(coef[1] + seq(1,sum(E_open))*coef[2])             # predict for months open
    effort <- E_open                                              # copy 
    effort[E_open==1] <- pred                                     # fill in the predicted effort (in person-trips) for each month open
    
    if(E_cap){
      effort <- (effort/sum(effort))*E_max                        # scale the effort to the maximum effort
    }
  }
  return(effort)
}


###-----------------------------------------------------
#   Scallop model function
###-----------------------------------------------------
scallop_model_fun <- function(scenario){ 
  # need to set the effort vector first for subsequent calculations
  scenario$catch_eq$effort <- eff_spread(scenario$catch_eq$E_max, scenario$catch_eq$E_open, scenario$catch_eq$E_cap, scenario$catch_eq$E_con)
  scenario$catch_run$effort <- eff_spread(scenario$catch_run$E_max, scenario$catch_run$E_open, scenario$catch_run$E_cap, scenario$catch_run$E_con)
  
  TLref <- scenario$life$vblinf*0.5 # Reference Length for Lorenzen M, (calculate from life values)
  
  prob_spawn <- rep(0,12)                                                                    # Starting probability of spawn vector
  # normalization necessary to only scallop to spawn once, second year - iteroparity
  prob_spawn[1:12] <- prob_spawn[1:12]/sum(prob_spawn[1:12])                                   # normalizing first year of prob spawn

  # Life history vectors
  scenario$life.vec <- with(scenario$life,{
    life.vec <- data.frame(Age = seq(1,amax))
    life.vec$TL <- vblinf*(1-exp(-vbk*(life.vec$Age-vbt0)))                                               # Length of scallops at age, von Bertalanffy function
    life.vec$Wt <- (alw*(life.vec$TL)^alwb)                                                               # Weight of Scallops weight-length function
    life.vec$Vul <- 1/(1+exp(-vgr*(life.vec$TL-vlh)))                                                     # Vulnerability of Scallops to harvest, logistic function 
    life.vec$Surv <- exp(-(M*TLref/life.vec$TL))^lorenzc                                                  # Survival of scallops at age (based on Lorenzen M)
    life.vec$M <- -log(life.vec$Surv)                                                                     # Natural mortality
    life.vec$Lo <- life.vec$Lfished <- vector(length = amax)                                              # Survivorship vectors
    life.vec$Lo[1] <- life.vec$Lfished[1] <- 1                                                            # Survivorship vector start
    mean.eff <- rowMeans(sapply(scenario$catch_eq$E_years, function(x) x*scenario$catch_eq$effort))       # mean effort across years
    life.vec$pseudo.eff <- c(mean.eff, mean.eff[1:6])                                                     # mean effort by age-month
    for (i in 2:amax){
      life.vec$Lo[i] <- life.vec$Lo[i-1]*life.vec$Surv[i-1]                                               # Survivorship vector calcs
      life.vec$Lfished[i] <- life.vec$Lfished[i-1]*exp(-1*(-log(life.vec$Surv[i-1]) + (life.vec$pseudo.eff[i-1]*scenario$catch_eq$q*life.vec$Vul[i-1]))) # Survivorship fished, Continuous
    }
    life.vec$Mat <- 1/(1+exp(-mgr*(life.vec$Age-amat)))                                                   # Maturity at month, based on logistic
    # life.vec$Fec <- .1*life.vec$Wt                                                                        # Fecundity, scalar of weight at month
    life.vec$Fec <- life.vec$Wt 

    life.vec$avail_spawn<-cumprod(c(1,(1-prob_spawn[1]*scenario$life.vec$Mat[1]),  (1-prob_spawn[2]*scenario$life.vec$Mat[2]),  (1-prob_spawn[3]*scenario$life.vec$Mat[3]),
                                      (1-prob_spawn[4]*scenario$life.vec$Mat[4]),  (1-prob_spawn[5]*scenario$life.vec$Mat[5]),  (1-prob_spawn[6]*scenario$life.vec$Mat[6]),
                                      (1-prob_spawn[7]*scenario$life.vec$Mat[7]),  (1-prob_spawn[8]*scenario$life.vec$Mat[8]),  (1-prob_spawn[9]*scenario$life.vec$Mat[9]),
                                      (1-prob_spawn[10]*scenario$life.vec$Mat[10]),(1-prob_spawn[11]*scenario$life.vec$Mat[11]),(1-prob_spawn[12]*scenario$life.vec$Mat[12]),
                                      (1-prob_spawn[1]*scenario$life.vec$Mat[13]), (1-prob_spawn[2]*scenario$life.vec$Mat[14]), (1-prob_spawn[3]*scenario$life.vec$Mat[15]),
                                      (1-prob_spawn[4]*scenario$life.vec$Mat[16]), (1-prob_spawn[5]*scenario$life.vec$Mat[17])))
    
    if(!semelparous){
      #Commented out bc prob spawn is 12 month vector accounting for seasonality of spawning
#      life.vec$prob_spawn[13:18] <- prob_spawn[1:6]/sum(prob_spawn[1:6])
#      life.vec$prob_spawn[13:18] <- prob_spawn[1:6]/sum(prob_spawn[1:12])
    }
    # normalizing second year of prob spawn
    return(life.vec)
  })

  # bag limit 
  # Number of scallops in a gallon as a function of shell height (Geiger et al., 2006; Granneman et al., 2021)
  # switch for bag limit units
  scenario$catch_eq <- within(scenario$catch_eq,{
    if(bag_unit == 'gallon'){
      require(truncnorm)
      maxcat <- bag*2.5                                               # theoretical max anyone can catch 
      catch.rate <- lapply(maxcat, function(x) seq(0,x,by=0.01))
      ret <- lapply(1:12, function(x) pmin(catch.rate[[x]],bag[x]))   # how many can be retained for every individual option of numbers caught
      p.legal <- rep(1,length(scenario$life.vec$Age))                 # not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
    }else if(bag_unit == "numbers"){
      maxcat <- ceiling(bag*2.5)                                      # theoretical max anyone can catch 
      catch.rate <- lapply(maxcat, function(x) seq(0,x,by=1))
      ret <- lapply(1:12, function(x) pmin(catch.rate[[x]],bag[x]))   # how many can be retained for every individual option of numbers caught
      p.legal <- rep(1,length(scenario$life.vec$Age))                 # not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
    }
  })
  scenario$catch_run <- within(scenario$catch_run,{
    if(bag_unit == 'gallon'){
      require(truncnorm)
      maxcat <- bag*2.5                                               # theoretical max anyone can catch 
      catch.rate <- lapply(maxcat, function(x) seq(0,x,by=0.01))
      ret <- lapply(1:12, function(x) pmin(catch.rate[[x]],bag[x]))   # how many can be retained for every individual option of numbers caught
      p.legal <- rep(1,length(scenario$life.vec$Age))                 # not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
    }else if(bag_unit == "numbers"){
      maxcat <- ceiling(bag*2.5)                                      # theoretical max anyone can catch 
      catch.rate <- lapply(maxcat, function(x) seq(0,x,by=1))
      ret <- lapply(1:12, function(x) pmin(catch.rate[[x]],bag[x]))   # how many can be retained for every individual option of numbers caught
      p.legal <- rep(1,length(scenario$life.vec$Age))                 # not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
    }
  })
  
  # per recruit section
  scenario$per.rec <- with(scenario$life.vec,{
    per.rec <- list()
    per.rec$epro_spawn <- sum(Lo*scenario$life.vec$avail_spawn*Fec*Mat*c(prob_spawn,prob_spawn[1:6]))       # eggs-per-recruit unfished conditions... includes prob_spawn
    per.rec$eprf_spawn <- sum(Lfished*scenario$life.vec$avail_spawn*Fec*Mat*c(prob_spawn,prob_spawn[1:6]))  # eggs-per-recruit fished conditions... includes prob_spawn
    per.rec$epro <- sum(Lo*Fec*Mat)                        # eggs-per-recruit unfished conditions
    per.rec$eprf <- sum(Lfished*Fec*Mat)                   # eggs-per-recruit fished conditions
    per.rec$bpro <- sum(Wt,Lo)                             # biomass-per-recruit unfished conditions
    per.rec$npro <- sum(Lo)                                # numbers-per-recruit unfished conditions
    per.rec$vbpro <- sum(Vul*Wt*Lo)                        # vulnerable biomass per recruit unfished conditions
    per.rec$vbprf <- sum(Vul*Wt*Lfished)                   # vulnerable biomiass per recruit fished conditions
    per.rec$spr <- per.rec$eprf/per.rec$epro               # spawning potential ratio
    per.rec$bo <- scenario$life$Ro*per.rec$bpro            # biomass at unfished condtions
    per.rec$no <- scenario$life$Ro*per.rec$npro            # numbers at unfished conditions
    per.rec$vbo <- scenario$life$Ro*per.rec$vbpro          # vulnerable biomass at unfished conditions
    per.rec$kq <- 2/per.rec$bo                             # Scaling constant for abundance varying q (from Pine et al 2015)
    return(per.rec)
  })
  
  # recruitment calculations
  scenario$recruit <- with(scenario$per.rec,{
    recruit <- list() 
    recruit$bha <- scenario$life$CR/epro                                                  # beverton holt a param
    recruit$bhb <- (scenario$life$CR-1)/(scenario$life$Ro*epro)                           # beverton holt b param
    recruit$bha_spawn <- scenario$life$CR/epro_spawn                                      # beverton holt a param, with prob_spawn
    recruit$bhb_spawn <- (scenario$life$CR-1)/(scenario$life$Ro*epro_spawn)               # beverton holt b param, with prob_spawn
    recruit$r.eq <- (recruit$bha*eprf-1)/(recruit$bhb*eprf)                               # equilibrium recruitment
    recruit$r.eq_spawn <- (recruit$bha_spawn*eprf_spawn-1)/(recruit$bhb_spawn*eprf_spawn) # equilibrium recruitment, with prob_spawn
    recruit$yield.eq <- (1-exp(-1*(scenario$life.vec$pseudo.eff*scenario$catch_eq$q))) * vbprf * recruit$r.eq      # Harvest rate * vulbio per recruit fished * rec at equilibrium
    return(recruit)
  })
  
  # declare locally for easy calling
  amax <- scenario$life$amax
  if(scenario$sim$run){
    months <- scenario$sim$month_eq + scenario$sim$month_run
  }else{
    months <- scenario$sim$month_eq
  }
  
  # storage
  eggs <- N <- B <- VB <- hr <- yield_n <- yield_b <- cpue_n <- cpue_b <- qt <- et <- hcpue <- hpue <- pr_hr <- pr_F <- vector(length = months)
  F_harv <- Z <- hr_harv <- nage <- matrix(0, months, amax)
  
  # time keeper
  timer <- seq(1, months) %% 12                     # Index vector that identifies each month , i.e., repeats 1:12 10 times
  timer[timer == 0] <- 12                           # Filling in December (12) for index 0
  yr.timer <- rep(seq(1, months/12), each = 12)     # Index vector that identifies which year it is
  
  # simulate
  for(i in 1:months){
    if(i == 1){ # Initialize
      nage[1,1] <- scenario$life$Ro                                               # initializing first year age structure
      nage[1,13] <- scenario$life$Ro*scenario$life.vec$Lo[13]                     # Survivors from year before...                                                     # first value of total numbers
      B[1] <- sum(nage[1,]*scenario$life.vec$Wt)                                  # first value of total biomass
    }
    # check if alive
    # recruitment
    if((i %% 12) == 1 & i != 1){
      #old way
#      eggs[i-1] <- sum(scenario$life.vec$Fec * scenario$life.vec$Mat * t(nage[(i-12):(i-1),])*c(prob_spawn,rep(0,6))) # Accumulating the eggs over the course of the year.

      #Now with indv_available
      for (t in 12:1){
        eggs[i-1] <- eggs[i-1] +
          sum( nage[i-t,] * scenario$life.vec$avail_spawn *   #abundance vector in month i-t of age (1-18))
               scenario$life.vec$Mat *                        #maturity vector
               prob_spawn[abs(t-13)] *                        #Spawning seasonality for month
               scenario$life.vec$Fec)                         #fecundity vector
      }
      
      nage[i,1] <- scenario$recruit$bha_spawn*eggs[i-1]/(1+scenario$recruit$bhb_spawn*eggs[i-1])                            # the recruitment without process error
      #nage[i,1] <- scenario$recruit$bha_spawn*eggs[i-1]/(1+scenario$recruit$bhb_spawn*eggs[i-1])*exp(proc_err[i-1])        # including process error
    }
    # Summarize B for use in catchability
    B[i] = sum(nage[i,] * scenario$life.vec$Wt)
    
    # Switch for eq vs run conditions
    if(i <= scenario$sim$month_eq){
      # Bag limit
      # switch for catchability
      if(scenario$catch_eq$q_flag == "constant"){
        qt[i] <- scenario$catch_eq$q                                                                      # Fixing q at a constant
      } else if(scenario$catch_eq$q_flag == 'VB'){
        qt[i] <- scenario$catch_eq$qmax/(1+scenario$per.rec$kq*B[i])                                      # q as a function of vul bio
      }
      et[i] <- scenario$catch_eq$effort[timer[i]] * scenario$catch_eq$E_years[yr.timer[i]]                # Calculation of Effort
      # hr[i] <- 1-exp(-qt[i]*et[i])
      F_init <- qt[i]*et[i]*scenario$life.vec$Vul
      
      if(et[i] > 0){
        if(scenario$catch_eq$bag_unit == "gallon"){
          scal_gal <- (nage[i,]/sh2gal(scenario$life.vec$TL))
          Z_init <- F_init+scenario$life.vec$M
          catch_base <- (F_init/Z_init)*(1-exp(-Z_init))*scal_gal
          # using a half-normal truncated at zero to get the density of 
          hcpue[i] <- sum(catch_base)/et[i]                                   # expected catch rate of harvestable fish
          hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])                     # div0 trap for effort = 0
          dens.catch <- dtruncnorm(scenario$catch_eq$catch.rate[[timer[i]]], a=0, mean=hcpue[i], sd=hcpue[i]*0.455)
          dens.catch <- dens.catch/sum(dens.catch)                            # need to sum to 1 to prevent loss from density range
          hpue[i] <- sum(dens.catch*scenario$catch_eq$ret[[timer[i]]])        # Expected catch rate of scallops under the bag limit
          # NEWTON-RAPHSON Iterations to adjust q such that the catch rate equals what would be expected under the bag
          if(hpue[i] < hcpue[i]){                                             # If the catch rate under the bag is lower than the unregulated catch rate, then do newt-raph
            # New adjusted q for bag limit using Newton-Raphson
            qt[i] <- exp( uniroot(f = function(lq) {                          # Finding a new q that will make catch rate equal to the expected catch rate under the bag
              hpue[i] - sum(((exp(lq)*et[i]*scenario$life.vec$Vul)/((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)) * scal_gal * (1-exp(-((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)))) /  et[i] # Baranov/Effort
            }, lower=-20, upper=0, extendInt="yes")$root )
          }
        }else{
          Z_init <- F_init+scenario$life.vec$M
          catch_base <- (F_init/Z_init)*(1-exp(-Z_init))*nage[i,]
          hcpue[i] <- sum(catch_base)/et[i]                                    # Expected catch rate of harvestable fish
          hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])                      # div0 trap for effort = 0
          dens.catch <- dpois(scenario$catch_eq$catch.rate[[timer[i]]],hcpue[i])
          dens.catch <- dens.catch/sum(dens.catch)                             # need to sum to 1 to prevent loss from density range
          hpue[i] <- sum(dens.catch*scenario$catch_eq$ret[[timer[i]]])         # Expected catch rate of scallops under the bag limit
          # NEWTON-RAPHSON Iterations to adjust q such that the catch rate equals what would be expected under the bag
          if(hpue[i] < hcpue[i]){                                              # If the catch rate under the bag is lower than the unregulated catch rate, then do newt-raph
            # New adjusted q for bag limit using Newton-Raphson
            qt[i]<-exp( uniroot(f = function(lq) {                             # Finding a new q that will make catch rate equal to the expected catch rate under the bag
              hpue[i] - sum(((exp(lq)*et[i]*scenario$life.vec$Vul)/((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)) * nage[i,] * (1-exp(-((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)))) /  et[i]    # Baranov/Effort
            }, lower=-20, upper=0, extendInt="yes")$root )
          }
        }
      }
    }else{
      # Bag limit
      # switch for catchability
      if(scenario$catch_run$q_flag == "constant"){
        qt[i] <- scenario$catch_run$q                                                                      # Fixing q at a constant
      } else if(scenario$catch_run$q_flag=='VB'){
        qt[i] <- scenario$catch_run$qmax/(1+scenario$per.rec$kq*B[i])                                      # q as a function of vul bio
      }
      et[i] <- scenario$catch_run$effort[timer[i]] * scenario$catch_run$E_years[yr.timer[i]-scenario$sim$month_eq/12]         # Calculation of Effort
      # hr[i] <- 1-exp(-qt[i]*et[i])
      F_init <- qt[i]*et[i]*scenario$life.vec$Vul
      
      if(et[i] > 0){
        if(scenario$catch_run$bag_unit == "gallon"){
          scal_gal <- (nage[i,]/sh2gal(scenario$life.vec$TL))
          Z_init <- F_init+scenario$life.vec$M
          catch_base <- (F_init/Z_init)*(1-exp(-Z_init))*scal_gal
          # using a half-normal truncated at zero to get the density of 
          hcpue[i] <- sum(catch_base)/et[i]                                   # expected catch rate of harvestable fish
          hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])                     # div0 trap for effort = 0
          dens.catch <- dtruncnorm(scenario$catch_run$catch.rate[[timer[i]]], a=0, mean=hcpue[i], sd=hcpue[i]*0.455)
          dens.catch <- dens.catch/sum(dens.catch)                            # need to sum to 1 to prevent loss from density range
          hpue[i] <- sum(dens.catch*scenario$catch_run$ret[[timer[i]]])           # Expected catch rate of scallops under the bag limit
          # NEWTON-RAPHSON Iterations to adjust q such that the catch rate equals what would be expected under the bag
          if(hpue[i] < hcpue[i]){                                             # If the catch rate under the bag is lower than the unregulated catch rate, then do newt-raph
            # New adjusted q for bag limit using Newton-Raphson
            qt[i] <- exp( uniroot(f = function(lq) {                          # Finding a new q that will make catch rate equal to the expected catch rate under the bag
              hpue[i] - sum(((exp(lq)*et[i]*scenario$life.vec$Vul)/((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)) * scal_gal * (1-exp(-((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)))) /  et[i] # Baranov/Effort
            }, lower=-20, upper=0, extendInt="yes")$root )
          }
        }else{
          Z_init <- F_init+scenario$life.vec$M
          catch_base <- (F_init/Z_init)*(1-exp(-Z_init))*nage[i,]
          hcpue[i] <- sum(catch_base)/et[i]                                    # Expected catch rate of harvestable fish
          hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])                      # div0 trap for effort = 0
          dens.catch <- dpois(scenario$catch_run$catch.rate[[timer[i]]],hcpue[i])
          dens.catch <- dens.catch/sum(dens.catch)                             # need to sum to 1 to prevent loss from density range
          hpue[i] <- sum(dens.catch*scenario$catch_run$ret[[timer[i]]])            # Expected catch rate of scallops under the bag limit
          # NEWTON-RAPHSON Iterations to adjust q such that the catch rate equals what would be expected under the bag
          if(hpue[i] < hcpue[i]){                                              # If the catch rate under the bag is lower than the unregulated catch rate, then do newt-raph
            # New adjusted q for bag limit using Newton-Raphson
            qt[i]<-exp( uniroot(f = function(lq) {                             # Finding a new q that will make catch rate equal to the expected catch rate under the bag
              hpue[i] - sum(((exp(lq)*et[i]*scenario$life.vec$Vul)/((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)) * nage[i,] * (1-exp(-((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)))) /  et[i]    # Baranov/Effort
            }, lower=-20, upper=0, extendInt="yes")$root )
          }
        }
      }
    } # end for eq vs run conditions switch
    
    # Mortality
    F_harv[i,] <- qt[i]*et[i]*scenario$life.vec$Vul                  # Fishing mortality
    Z[i,] <- F_harv[i,]+scenario$life.vec$M                          # Total mortality
    # advance from all mortality sources (kill all oldest scallops)
    if(i!=months) nage[i+1,2:amax] <- nage[i,1:(amax-1)] * exp(-Z[i,1:(amax-1)])
    
  }
  # Summary (no need to be in for loop)
  N <- rowSums(nage)                                                        # total numbers in each month, sum of numbers across ages
  VB <- nage%*%(scenario$life.vec$Wt*scenario$life.vec$Vul)                 # vulernable biomass, sum of numbers at age * weight at age * vul at age
  eggs_mon <- rowSums(t(t(nage)*scenario$life.vec$avail_spawn*scenario$life.vec$Fec*scenario$life.vec$Mat*c(prob_spawn,prob_spawn[1:6]))) # eggs produced each month, calculated for visualization and results
  eggs_mon_age <- t(t(nage)*scenario$life.vec$avail_spawn*scenario$life.vec$Fec*scenario$life.vec$Mat*c(prob_spawn,prob_spawn[1:6]))      # eggs per month per age
  yield_n <- rowSums((F_harv/Z)*nage*(1-exp(-Z)))                                                          # yield in numbers
  yield_b <-  rowSums(((F_harv/Z)*nage*(1-exp(-Z))) %*% diag(scenario$life.vec$Wt))                       # yield in biomass  (CAA * Waa)
  cpue_n <- yield_n/et                                                      # cpue in numbers, yield/effort
  cpue_b <- yield_b/et                                                      # cpue in bimoass, yield/effort
  recruits <- nage[,1]                                                      # The recruits at each time step
  hr <- yield_n/N                                                           # Scallop harvest rate per month
  
  # return
  ret.l <- list(scenario = scenario,
                results_full = data.frame(time = 1:months,
                                          N = N,
                                          eggs = eggs,
                                          eggs_mon = eggs_mon,
                                          eggs_mon_age = eggs_mon_age,
                                          B = B,
                                          VB = VB,
                                          recruits = recruits,
                                          yield_n = yield_n,
                                          yield_b = yield_b,
                                          cpue_n = cpue_n,
                                          cpue_b = cpue_b,
                                          et = et,
                                          qt = qt,
                                          hcpue = hcpue,
                                          hpue = hpue,
                                          hr = hr),
                matrix_full = list(nage = nage,
                                   F_harv = F_harv))
  if(scenario$sim$run){
    ret.l$results <- ret.l$results_full[(scenario$sim$month_eq+1):months,]
    ret.l$matrix <- list(nage = nage[(scenario$sim$month_eq+1):months,],
                         F_harv = F_harv[(scenario$sim$month_eq+1):months,])
  }else{
    ret.l$results <- ret.l$results_full
    ret.l$matrix <- ret.l$matrix_full
  }
  
  return(ret.l)
} # end of function
