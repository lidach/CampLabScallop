#######################################
#Scallop Model 
#######################################

#Monthly model.... "age" in the following refers to months 

### Remaining to do:
#   consider spatial issues


scenario <- list( 
  life = list(amax = 18,              #maximum age in the model, this is not a plus group (we just kill all scallops after this month)
              Ro = 3e7,                #Unfished Recruitment, set at 30 million 
              CR = 8,                  #Compensation Ratio, Arbitrarily set
              vbk = 1/3,              #Von-Bertalanffy K
              vblinf = 60,             #Von-Bertalanffy L-infinity 
              vbt0 = 0,                #Von-Bertalanffy t0
              alw = 0.0001,            #Weight-Length a
              alwb = 3,                #Weight-Length b
              # wmat = 12,               #Weight at maturity
              amat = 5.5,               #Age at 50% maturity
              mgr = 2.5,                #Maturity logistic function growth rate
              vlh = 50,                #Vulnerability logistic param, size at 50% selected
              vgr = 0.1,                 #Vulnerability logistic param, growth rate
              M = 0.25,                 #Natural Mortality, note this is a Monthly Instantaneous Rate
              lorenzc = 1,             #Lorenzen exponent for Lorenzen M
              #Probability of Scallop Spawning across any age
              prob_spawn = c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.09,0.125,0.09,0.07,0.05),
              semelparous=FALSE),
  catch = list( q_flag = "constant",  #Catchability Option______constant or VB
                #Effort in each month; merges season open/close
                E_max  = sum(c(11415,6929,4255))*3.8, # maximum number vessels, need to multiply by 3.8 to get persons
                E_cap = FALSE, # this is a switch to cap effort @ emax or to allow it to scale following the observed effort decline from Granneman paper
                E_con = FALSE, # this is a switch for constant effort
                E_open = c(0,0,0,0,0,0,1,1,1,0,0,0), #this is a vector of months that tells the model where to apply the effort. The effort does not need to be sequential as the eff_spread function will allocate the effort in each month
                q = 0.0000319/3.8,   #Catchability, Granneman paper - (0.0000319) value is per vessel so divide by 3.8 to get persons (off by one-order of magnitude for scaling)
                qmax = .0005,  #Max Catchability (for q varying with VB)
                bag_unit = "gallon",   #Bag limit Units_____gallon or numbers
                bag = rep(2,12), #Bag limit________rep(2,12) for constant
                E_years = seq(1,1,length.out=25)               #Effort over years_________rep(1,10) for constant
  ), 
  sim = list(month = 300)                                      #Simulation length, number of months
)
sh2gal <- function(sh){
  (-6.704*sh + 480.96)/2
}
eff_spread <- function(E_max, E_open, E_cap, E_con){
  if(E_con){
    effort <- E_open
    effort[E_open==1] <- E_max/sum(E_open)
  }else{
    coef <- coef(lm(log(c(11415,6929,4255)*3.8)~seq(1,3))) #fit exponential decay model to effort decline
    pred <- exp(coef[1] + seq(1,sum(E_open))*coef[2]) # predict for months open
    effort <- E_open # copy 
    effort[E_open==1] <- pred #fill in the predicted effort (in person-trips) for each month open
    
    if(E_cap){
      effort <- (effort/sum(effort))*E_max #scale the effort to the maximum effort
    }
  }
  return(effort)
}
scallop_model_fun <- function(scenario){
  
  #need to set the effort vector first for subsequent calculations
  scenario$catch$effort <- eff_spread(scenario$catch$E_max, scenario$catch$E_open, scenario$catch$E_cap, scenario$catch$E_con)
  
  TLref <- scenario$life$vblinf*0.5                               #Reference Length for Lorenzen M, (calculate from life values)
  #Life history vectors
  scenario$life.vec <- with(scenario$life,{
    life.vec <- data.frame(Age = seq(1,amax))
    life.vec$TL <- vblinf*(1-exp(-vbk*(life.vec$Age-vbt0)))     #Length of scallops at age, VB function
    life.vec$Wt <- (alw*(life.vec$TL)^alwb)                     #Weight of Scallops WL function
    life.vec$Vul <- 1/(1+exp(-vgr*(life.vec$TL-vlh)))           #Vulnerability of Scallops to harvest, logistic function 
    life.vec$Surv <- exp(-(M*TLref/life.vec$TL))^lorenzc        #Survival of scallops at age (based on Lorenzen M)
    life.vec$M <- -log(life.vec$Surv)                           #Natural mortality
    life.vec$Lo <- life.vec$Lfished <- vector(length=amax)      #Survivorship vectors
    life.vec$Lo[1] <- life.vec$Lfished[1] <- 1                  #Survivorship vector start
    mean.eff <- rowMeans(sapply(scenario$catch$E_years, function(x) x*scenario$catch$effort)) #mean effort across years
    life.vec$pseudo.eff <- c(mean.eff, mean.eff[1:6]) #mean effort by age-month
    for (i in 2:amax){
      life.vec$Lo[i] <- life.vec$Lo[i-1]*life.vec$Surv[i-1]     #Survivorship vector calcs
      life.vec$Lfished[i] <- life.vec$Lfished[i-1]*exp(-1*(-log(life.vec$Surv[i-1]) + (life.vec$pseudo.eff[i-1]*scenario$catch$q*life.vec$Vul[i-1]))) #Survivorship fished, Continuous
    }
    life.vec$Mat <- 1/(1+exp(-mgr*(life.vec$Age-amat)))         #Maturity at month, based on logistic
    life.vec$Fec <- .1*life.vec$Wt                              #Fecundity, scalar of weight at month
    life.vec$prob_spawn <- rep(0,amax)                          #Starting probability of spawn vector
    # normalization necessary to only scallop to spawn once, second year - iteroparity
    life.vec$prob_spawn[1:12] <- prob_spawn[1:12]/sum(prob_spawn[1:12])      #normalizing first year of prob spawn
    if(!semelparous){
      life.vec$prob_spawn[13:18] <- prob_spawn[1:6]/sum(prob_spawn[1:6])
    }
    #normalizing second year of prob spawn
    return(life.vec)
  })
  #bag limit 
  #Number of scallops in a gallon as a function of shell height (Geiger et al., 2006; Granneman et al., in-review)
  
  #switch for bag limit units
  scenario$catch <- within(scenario$catch,{
    if(bag_unit=='gallon'){
      require(truncnorm)
      maxcat <- bag*2.5                                               #theoretical max anyone can catch 
      catch.rate <- lapply(maxcat, function(x) seq(0,x,by=0.01))
      ret <- lapply(1:12, function(x) pmin(catch.rate[[x]],bag[x]))   #how many can be retained for every individual option of numbers caught
      p.legal <- rep(1,length(scenario$life.vec$Age))                 #not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
    }else if(bag_unit=="numbers"){
      maxcat <- ceiling(bag*2.5)                                      #theoretical max anyone can catch 
      catch.rate <- lapply(maxcat, function(x) seq(0,x,by=1))
      ret <- lapply(1:12, function(x) pmin(catch.rate[[x]],bag[x]))   #how many can be retained for every individual option of numbers caught
      p.legal <- rep(1,length(scenario$life.vec$Age))                 #not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
    }
  })
  #per recruit section
  scenario$per.rec <- with(scenario$life.vec, {
    per.rec <- list()
    per.rec$epro_spawn <- sum(Lo*Fec*Mat*prob_spawn)       #eggs-per-recruit unfished conditions... includes prob_spawn
    per.rec$eprf_spawn <- sum(Lfished*Fec*Mat*prob_spawn)  #eggs-per-recruit fished conditions... includes prob_spawn
    per.rec$epro <- sum(Lo*Fec*Mat)                        #eggs-per-recruit unfished conditions
    per.rec$eprf <- sum(Lfished*Fec*Mat)                   #eggs-per-recruit fished conditions
    per.rec$bpro <- sum(Wt,Lo)                             #biomass-per-recruit unfished conditions
    per.rec$npro <- sum(Lo)                                #numbers-per-recruit unfished conditions
    per.rec$vbpro <- sum(Vul*Wt*Lo)                        #vulnerable biomass per recruit unfished conditions
    per.rec$vbprf <- sum(Vul*Wt*Lfished)                   #vulnerable biomiass per recruit fished conditions
    per.rec$spr <- per.rec$eprf/per.rec$epro               #spawning potential ratio
    per.rec$bo <- scenario$life$Ro*per.rec$bpro            #biomass at unfished condtions
    per.rec$no <- scenario$life$Ro*per.rec$npro            #numbers at unfished conditions
    per.rec$vbo <- scenario$life$Ro*per.rec$vbpro          #vulnerable biomass at unfished conditions
    per.rec$kq <- 2/per.rec$bo                             #Scaling constant for abundance varying q (from Pine et al 2015)
    return(per.rec)
  })
  #recruitment calculations
  scenario$recruit <- with(scenario$per.rec,{
    recruit <- list()
    recruit$bha <- scenario$life$CR/epro                                                 #beverton holt a param
    recruit$bhb <- (scenario$life$CR-1)/(scenario$life$Ro*epro)                          #beverton holt b param
    recruit$bha_spawn <- scenario$life$CR/epro_spawn                                     #beverton holt a param, with prob_spawn
    recruit$bhb_spawn <- (scenario$life$CR-1)/(scenario$life$Ro*epro_spawn)              #beverton holt b param, with prob_spawn
    recruit$r.eq <- (recruit$bha*eprf-1)/(recruit$bhb*eprf)                              #equilibrium recruitment
    recruit$r.eq_spawn <- (recruit$bha_spawn*eprf_spawn-1)/(recruit$bhb_spawn*eprf_spawn)#equilibrium recruitment, with prob_spawn
    recruit$yield.eq <- (1-exp(-1*(scenario$life.vec$pseudo.eff*scenario$catch$q))) * vbprf * recruit$r.eq       #Harvest rate * vulbio per recruit fished * rec at equilibrium
    return(recruit)
  })
  #declare locally for easy calling
  amax <- scenario$life$amax
  months <- scenario$sim$month
  #storage
  eggs <- N <- B <- VB <- hr <- yield_n <- yield_b <- cpue_n <- cpue_b <- qt <- et <- hcpue <- hpue <- pr_hr <- pr_F <- vector(length=months)
  F_harv <- Z <- hr_harv <- nage <- matrix(0, months, amax)
  
  #time keeper
  timer <- seq(1,months) %% 12                  #Index vector that identifies each month , i.e., repeats 1:12 10 times
  timer[timer==0] <- 12                         #Filling in December (12) for index 0
  yr.timer <- rep(seq(1,months/12),each=12)     #Index vector that identifies which year it is
  #simulate
  for(i in 1:months){
    if(i == 1){ #Initialize
      nage[1,1] <- scenario$life$Ro                                               #initializing first year age structure
      nage[1,13] <- scenario$life$Ro*scenario$life.vec$Lo[13]                     #Survivors from year before...
      #proc_err[1] = 0 #rnorm(1,0,sdpro)                                          #First value of process error
      # N[1] <- sum(nage[1,])                                                     #first value of total numbers
      B[1] <- sum(nage[1,]*scenario$life.vec$Wt)                                  #first value of total biomass
    }
    #check if alive
    #recruitment
    # nage[i,1] <- 0              #Recruitment without process error
    if((i %% 12)==1 & i!=1){
      eggs[i-1] <- sum(scenario$life.vec$Fec * scenario$life.vec$Mat * t(nage[(i-12):(i-1),])*scenario$life.vec$prob_spawn) #Accumulating the eggs over the course of the year.
      
      nage[i,1] <- scenario$recruit$bha_spawn*eggs[i-1]/(1+scenario$recruit$bhb_spawn*eggs[i-1])                            #the recruitment without process error
      #nage[i,1] <- scenario$recruit$bha_spawn*eggs[i-1]/(1+scenario$recruit$bhb_spawn*eggs[i-1])*exp(proc_err[i-1])        #including process error
    }
    #Summarize B for use in catchability
    B[i] = sum(nage[i,] * scenario$life.vec$Wt)
    #Bag limit
    #switch for catchability
    if(scenario$catch$q_flag == "constant"){
      qt[i] <- scenario$catch$q                                                                                      #Fixing q at a constant
    }else if(scenario$catch$q_flag=='VB'){
      qt[i] <- scenario$catch$qmax/(1+scenario$per.rec$kq*B[i])                                                    #q as a function of vul bio
    }
    et[i] <- scenario$catch$effort[timer[i]] * scenario$catch$E_years[yr.timer[i]] #Calculation of Effort
    # hr[i] <- 1-exp(-qt[i]*et[i])
    F_init <- qt[i]*et[i]*scenario$life.vec$Vul
    
    if(et[i]>0){
      if(scenario$catch$bag_unit=="gallon"){
        scal_gal <- (nage[i,]/sh2gal(scenario$life.vec$TL))
        Z_init <- F_init+scenario$life.vec$M
        catch_base <- (F_init/Z_init)*(1-exp(-Z_init))*scal_gal
        #using a half-normal truncated at zero to get the density of 
        hcpue[i] <- sum(catch_base)/et[i] #expected catch rate of harvestable fish
        hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])  #div0 trap for effort = 0
        dens.catch <- dtruncnorm(scenario$catch$catch.rate[[timer[i]]], a=0, mean=hcpue[i], sd=hcpue[i]*0.4)
        dens.catch <- dens.catch/sum(dens.catch)                       #need to sum to 1 to prevent loss from density range
        hpue[i] <- sum(dens.catch*scenario$catch$ret[[timer[i]]])      #Expected catch rate of scallops under the bag limit
        #NEWTON-RAPHSON Iterations to adjust q such that the catch rate equals what would be expected under the bag
        if(hpue[i] < hcpue[i]){                          #If the catch rate under the bag is lower than the unregulated catch rate, then do newt-raph
          #New adjusted q for bag limit using Newton-Raphson
          qt[i] <- exp( uniroot(f = function(lq) {   #Finding a new q that will make catch rate equal to the expected catch rate under the bag
             hpue[i] - sum(((exp(lq)*et[i]*scenario$life.vec$Vul)/((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)) * scal_gal * (1-exp(-((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)))) /  et[i]    # Baranov/Effort
          }, lower=-20, upper=0, extendInt="yes")$root )
        }
      }else{
        Z_init <- F_init+scenario$life.vec$M
        catch_base <- (F_init/Z_init)*(1-exp(-Z_init))*nage[i,]
        hcpue[i] <- sum(catch_base)/et[i]                                    #Expected catch rate of harvestable fish
        hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])                      #div0 trap for effort = 0
        dens.catch <- dpois(scenario$catch$catch.rate[[timer[i]]],hcpue[i])
        dens.catch <- dens.catch/sum(dens.catch)                             #need to sum to 1 to prevent loss from density range
        hpue[i] <- sum(dens.catch*scenario$catch$ret[[timer[i]]])            #Expected catch rate of scallops under the bag limit
        #NEWTON-RAPHSON Iterations to adjust q such that the catch rate equals what would be expected under the bag
        if(hpue[i] < hcpue[i]){                          #If the catch rate under the bag is lower than the unregulated catch rate, then do newt-raph
          #New adjusted q for bag limit using Newton-Raphson
          qt[i]<-exp( uniroot(f = function(lq) {   #Finding a new q that will make catch rate equal to the expected catch rate under the bag
             hpue[i] - sum(((exp(lq)*et[i]*scenario$life.vec$Vul)/((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)) * nage[i,] * (1-exp(-((exp(lq)*et[i]*scenario$life.vec$Vul)+scenario$life.vec$M)))) /  et[i]    # Baranov/Effort
          }, lower=-20, upper=0, extendInt="yes")$root )
        }
      }
    }
    #Mortality
    F_harv[i,] <- qt[i]*et[i]*scenario$life.vec$Vul                    #Fishing mortality
    Z[i,] <- F_harv[i,]+scenario$life.vec$M                          #Total mortality
    #advance from all mortality sources (kill all oldest scallops)
    if(i!=months) nage[i+1,2:amax] <- nage[i,1:(amax-1)] * exp(-Z[i,1:(amax-1)])
    
  }
  #Summary (no need to be in for loop)
  N <- rowSums(nage)                                                        #total numbers in each month, sum of numbers across ages
  VB <- nage%*%(scenario$life.vec$Wt*scenario$life.vec$Vul)                 #vulernable biomass, sum of numbers at age * weight at age * vul at age
  eggs_mon <- rowSums(t(t(nage)*scenario$life.vec$Fec*scenario$life.vec$Mat*scenario$life.vec$prob_spawn)) # eggs produced each month, calculated for visualization and results
  eggs_mon_age <- t(t(nage)*scenario$life.vec$Fec*scenario$life.vec$Mat*scenario$life.vec$prob_spawn)      # eggs per month per age
  yield_n <- rowSums((F_harv/Z)*nage*(1-exp(-Z)))                                                          # yield in numbers
  yield_b <-  rowSums( ((F_harv/Z)*nage*(1-exp(-Z))) %*% diag(scenario$life.vec$Wt))                       # yield in biomass  (CAA * Waa)
  cpue_n <- yield_n/et                                                      #cpue in numbers, yield/effort
  cpue_b <- yield_b/et                                                      #cpue in bimoass, yield/effort
  recruits <- nage[,1]                                                      #The recruits at each time step
  hr <- yield_n/N                                                           #Scallop harvest rate per month
  #return
  ret.l <- list(scenario = scenario,
                results=data.frame(time = 1:scenario$sim$month,
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
                matrix = list(nage = nage,
                              F_harv = F_harv))
  return(ret.l)
}
###-----------------------------------------------------
#    Run
###-----------------------------------------------------
# run1 <- scallop_model_fun(scenario)

# #Simple plot of VB, effort
#   with(run1$results,{
#       par(mfrow=c(3,1), mar=c(3,5,1,1), las=1, mgp=c(4,1,0))
#       plot(time, VB, type="l", col="blue", 
#            lwd=3, ylim=c(0, max(VB)))
#       abline(lm(VB~time), lwd=2, lty=3)
#       et2 <- et
#       et2[et2==0] <- NA
#       plot(time, et2, col="red", lwd=3, type='l', 
#            ylim=c(0,max(et2,na.rm=T)), ylab="Effort")
#       plot(time, recruits, type='l')
#   })


