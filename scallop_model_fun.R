#######################################
#Scallop Model 
#######################################

#Monthly model.... "age" in the following refers to months 

### Remaining to do:
#   consider spatial issues


scenario <- list( 
                 life = list(amax = 18,              #maximum age in the model, this is not a plus group (we just kill all scallops after this month) <- I think 
                            Ro = 1e6,                #Unfished Recruitment, set at 1 million 
                            CR = 8,                  #Compensation Ratio, Arbitrarily set
                            vbk = 4/12,              #Von-Bertalanffy K
                            vblinf = 65,             #Von-Bertalanffy L-infinity 
                            vbt0 = 0,                #Von-Bertalanffy t0
                            alw = 0.0001,            #Weight-Length a
                            alwb = 3,                #Weight-Length b
                            wmat = 12,               #Weight at maturity
                            amat = 10,               #Age at maturity
                            msd = .1,                #Maturity sd for logistic function
                            vlh = 50,                #Vulnerability logistic param
                            vsd = 5,                 #Vulnerability sd, for logistic form
                            M = 0.2,                 #Natural Mortality
                            lorenzc = 1,             #Lorenzen exponent for Lorenzen M
                            #Probability of Scallop Spawning at a given age
                            prob_spawn = c(0,0,0,0,0,0.1,            
                                           0.2,0.5,0.8,0.8,0.2,0.1,  
                                           0.3,0.5,1,1,1,1)),
                  catch = list( q_flag = "constant",  #Catchability Option______constant or VB
                               #Effort in each month; merges season open/close
                                effort = c(0,0,0,0,0,0,11415,6929,4255,0,0,0), 
                                q = 0.0000319,   #Catchability 
                                qmax = .0005,  #Max Catchability (for q varying with VB)
                                bag_unit = "gallon",   #Bag limit Units_____gallon or numbers
                                bag = rep(2,12), #Bag limit________rep(2,12) for constant
                                e_years = seq(1,1,length.out=10)               #Effort over years_________rep(1,10) for constant
                                ), 
                  sim = list(month = 120)                                      #Simulation length, number of months
                  )

scallop_model_fun <- function(scenario){
  
  TLref <- scenario$life$vblinf*0.5                               #Reference Length for Lorenzen M, (calculate from life values)
  #Life history vectors
    scenario$life.vec <- with(scenario$life,{
        life.vec <- data.frame(Age = seq(1,amax))
        life.vec$TL <- vblinf*(1-exp(-vbk*(life.vec$Age-vbt0)))     #Length of scallops at age, VB function
        life.vec$Wt <- (alw*(life.vec$TL)^alwb)                     #Weight of Scallops WL function
        life.vec$Vul <- 1/(1+exp(-(life.vec$TL-vlh)/vsd))           #Vulnerability of Scallops to harvest, logistic function 
        life.vec$Surv <- exp(-(M*TLref/life.vec$TL))^lorenzc        #Survival of scallops at age (based on Lorenzen M)
        life.vec$Lo <- life.vec$Lfished <- vector(length=amax)      #Need comments for this to line 59 on what is exactly being done, looks like survivorship
        life.vec$Lo[1] <- life.vec$Lfished[1] <- 1                  #Survivorship vector start
        mean.eff <- rowMeans(sapply(scenario$catch$e_years, function(x) x*scenario$catch$effort)) #mean effort across years
        life.vec$pseudo.eff <- c(mean.eff, mean.eff[1:6]) #mean effort by age-month
        for (i in 2:amax){
          life.vec$Lo[i] <- life.vec$Lo[i-1]*life.vec$Surv[i-1]     #Survivorship vector calcs
#          life.vec$Lfished[i] <- life.vec$Lfished[i-1]*life.vec$Surv[i-1]*(1-(1-exp(-life.vec$pseudo.eff[i-1]*scenario$catch$q))*life.vec$Vul[i-1])  #Survivorship fished, note it doesnt contain bag limit stuff (Why is Vul outside of F calc?)
          life.vec$Lfished[i] <- life.vec$Lfished[i-1]*exp(-1*(-log(life.vec$Surv[i-1]) + (life.vec$pseudo.eff[i-1]*scenario$catch$q*life.vec$Vul[i-1]))) #Survivorship fished, Continuous
        }
        life.vec$Mat <- 1/(1+exp(-(life.vec$Age-amat)/msd))         #Maturity at month, based on logistic
        life.vec$Fec <- .1*life.vec$Wt                              #Fecundity, scalar of weight at month
        life.vec$prob_spawn <- 0                                    #Starting probability of spawn vector
        life.vec$prob_spawn[1:12] <- prob_spawn[1:12]/sum(prob_spawn[1:12])      #normalizing first year of prob spawn
        life.vec$prob_spawn[13:18] <- prob_spawn[13:18]/sum(prob_spawn[13:18])   #normalizing second year of prob spawn
        return(life.vec)
    })
  #bag limit 
    #Number of scallops in a gallon as a function of shell height (Geiger et al., 2006; Granneman et al., in-review)
    sh2gal <- function(sh){
      (-6.704*sh + 480.96)/2
    }
    #switch for bag limit units
    scenario$catch <- within(scenario$catch,{
      if(bag_unit=='gallon'){
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
#      recruit$yield.eq <- scenario$catch$U*vbprf*recruit$r.eq       #Harvest rate * vulbio per recruit fished * rec at equilibrium      <- is this obsolete without FM_flag??
      recruit$yield.eq <- (1-exp(-1*(scenario$life.vec$pseudo.eff*scenario$catch$q))) * vbprf * recruit$r.eq       #Harvest rate * vulbio per recruit fished * rec at equilibrium
      return(recruit)
    })
  #declare locally for easy calling
    amax <- scenario$life$amax
    months <- scenario$sim$month
  #storage
    eggs <- N <- B <- VB <- hr <- yield_n <- yield_b <- cpue_n <- cpue_b <- qt <- et <- hcpue <- hpue <- pr_hr <- vector(length=months)
    hr_sel <- hr_harv <- nage <- matrix(0, months, amax)
  #initialization
    #numbers
      nage[1,1] <- scenario$life$Ro                                               #initializing first year age structure
      nage[1,13] <- scenario$life$Ro*scenario$life.vec$Lo[13]                     #Survivors from year before...
      #proc_err[1] = 0 #rnorm(1,0,sdpro)                                          #First value of process error
      # N[1] <- sum(nage[1,])                                                     #first value of total numbers
      B[1] <- sum(nage[1,]*scenario$life.vec$Wt)                                  #first value of total biomass
      # VB[1] <- sum(nage[1,]*scenario$life.vec$Wt*scenario$life.vec$Vul)         #first value of vulnerable biomass
    #harvest
      #switch for catchability
      if(scenario$catch$q_flag == "constant"){
        qt[1] <- scenario$catch$q                                                              #Fixing q at a constant
      }else if(scenario$catch$q_flag=='VB'){
        qt[1] <- scenario$catch$qmax/(1+scenario$per.rec$kq*B[1])                              #Q as a function of Vulnerable Biomass
      }
      et[1] <- scenario$catch$effort[1] * scenario$catch$e_years[1] #Effort in the first month
      hr[1] <- 1-exp(-qt[1]*et[1])                                                             #Harvest rate calc, turning continuous F into a discrete harvest rate
      hr_sel[1,] <- hr[1]*scenario$life.vec$Vul                                                #age-specific capture rate according to selectivity
      hr_harv[1,] <-  hr[1]*scenario$life.vec$Vul*scenario$catch$p.legal                       #age-specific capture of legally big animals (adjusting hr_sel for size limit)
      if(scenario$catch$bag_unit=="gallon"){
        require(truncnorm)
        #converts to gallons of scallops in the environment
        hcpue[1] <- sum((nage[1,]/sh2gal(scenario$life.vec$TL))*hr_harv[1,])/et[1] #raw catch rates not accounting for bag *also not accounting for loss due to So
        hcpue[1] <- ifelse(is.nan(hcpue[1]),0,hcpue[1])  #trap for cases where effort <- 0, will throw div0 errors (NaN)
        #using a half-normal truncated at zero to get the density of 
        dens.catch <- dtruncnorm(scenario$catch$catch.rate[[1]], a=0, mean=hcpue[1], sd=hcpue[1]*0.4)
        dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
        hpue[1] <- sum(dens.catch*scenario$catch$ret[[1]])  #rate of actually harvestable fish accounting for bag and size
    }else{
        hcpue[1] <- sum(nage[1,]*hr_harv[1,])/et[1] #raw catch rates not accounting for bag *also not accounting for loss due to So
        hcpue[1] <- ifelse(is.nan(hcpue[1]),0,hcpue[1])  #trap for cases where effort <- 0, will throw div0 errors (NaN)
        dens.catch <- dpois(scenario$catch$catch.rate[[1]],hcpue[1])
        dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
        hpue[1] <- sum(dens.catch*scenario$catch$ret[[1]])  #rate of actually harvestable fish accounting for bag and size
    }
      
      pr_hr[1] <- (hpue[1]/hcpue[1])                                             #probability of actually harvesting scallop of legal size
      pr_hr[1] <- ifelse(is.nan(pr_hr[1]),0,pr_hr[1])                            #Another trap, becuase hcpue could be 0
      yield_n[1] <- sum(hr_harv[1,]*pr_hr[1]*nage[1,])                           #Yield in numbers
      yield_b[1] <- sum(hr_harv[1,]*pr_hr[1]*nage[1,]*scenario$life.vec$Wt)      #Yield in Biomass
      cpue_n[1] <- yield_n[1]/et[1]                                              #first value of cpue numbers
      cpue_b[1] <- yield_b[1]/et[1]                                              #first value of cpue biomass
  #time keeper
    timer <- seq(1,months) %% 12                  #Index vector that identifies each month , i.e., repeats 1:12 10 times
    timer[timer==0] <- 12                         #Filling in December (12) for index 0
    yr.timer <- rep(seq(1,months/12),each=12)     #Index vector that identifies which year it is
  #simulate
    for(i in 2:months){
        #check if alive

        #recruitment
          # nage[i,1] <- 0              #Recruitment without process error
          if((i %% 12)==1){
            eggs[i-1] <- sum(scenario$life.vec$Fec * scenario$life.vec$Mat * t(nage[(i-12):(i-1),])*scenario$life.vec$prob_spawn) #Accumulating the eggs over the course of the year.

            nage[i,1] <- scenario$recruit$bha_spawn*eggs[i-1]/(1+scenario$recruit$bhb_spawn*eggs[i-1])                            #the recruitment without process error
            #nage[i,1] <- scenario$recruit$bha_spawn*eggs[i-1]/(1+scenario$recruit$bhb_spawn*eggs[i-1])*exp(proc_err[i-1])        #including process error
          }
        #Applying Natural mortality for catch rate calcs
          nage[i,2:amax] <- nage[i-1,1:(amax-1)]*scenario$life.vec$Surv[1:(amax-1)]                                               #Note that we dont have a plus group calc as we're killing all after month 18
          #OK so I assume the above is needed to get catch rates and eventually F before doing the actual mortality calc (including F & M), which will overwrite above
          
        #Bag limit
          #switch for catchability
          if(scenario$catch$q_flag == "constant"){
            qt[i] <- scenario$catch$q                                                                                      #Fixing q at a constant
          }else if(scenario$catch$q_flag=='VB'){
            qt[i] <- scenario$catch$qmax/(1+scenario$per.rec$kq*B[i-1])                                                    #q as a function of vul bio
          }
          et[i] <- scenario$catch$effort[timer[i]] * scenario$catch$e_years[yr.timer[i]] #Calculation of Effort
          hr[i] <- 1-exp(-qt[i]*et[i])
          hr_sel[i,] <- hr[i]*scenario$life.vec$Vul
          hr_harv[i,] <- hr[i]*scenario$life.vec$Vul*scenario$catch$p.legal
          if(scenario$catch$bag_unit=="gallon"){
            #using a half-normal truncated at zero to get the density of 
            hcpue[i] <- sum((nage[i,]/sh2gal(scenario$life.vec$TL))*hr_harv[i,])/et[i] #expected catch rate of harvestable fish
            hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])  #div0 trap for effort = 0
            dens.catch <- dtruncnorm(scenario$catch$catch.rate[[timer[i]]], a=0, mean=hcpue[i], sd=hcpue[i]*0.4)
            dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
            hpue[i] <- sum(dens.catch*scenario$catch$ret[[timer[i]]])  #rate of actually harvestable fish accounting for bag and size
        }else{
            hcpue[i] <- sum(nage[i,]*hr_harv[i,])/et[i] #expected catch rate of harvestable fish
            hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])  #div0 trap for effort = 0
            dens.catch <- dpois(scenario$catch$catch.rate[[timer[i]]],hcpue[i])
            dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
            hpue[i] <- sum(dens.catch*scenario$catch$ret[[timer[i]]])  #rate of actually harvestable fish accounting for bag and size
        }
          pr_hr[i] <- pmax(0,pmin((hpue[i]/hcpue[i]),1))                                                                   #probabilty of actually harvesting (i.e. landing)
          pr_hr[i] <- ifelse(is.nan(pr_hr[i]),0,pr_hr[i])                                                                  #trap for effort =0...meaning hcpue=0
        #Mortality
          nage[i,2:amax] <- nage[i-1,1:(amax-1)]*(1-hr_harv[i,1:(amax-1)] * pr_hr[i]) * scenario$life.vec$Surv[1:(amax-1)]
        #Summarize B for use in catchability
          B[i] = sum(nage[i,] * scenario$life.vec$Wt)
    }
  #Summary (no need to be in for loop)
      N <- rowSums(nage)                                                        #total numbers in each month, sum of numbers across ages
      VB <- nage%*%(scenario$life.vec$Wt*scenario$life.vec$Vul)                 #vulernable biomass, sum of numbers at age * weight at age * vul at age
      yield_n <- rowSums(hr_harv*nage*pr_hr)                                    #yield in numbers
      yield_b <-  rowSums(hr_harv*(nage%*%diag(scenario$life.vec$Wt))*pr_hr)    #yield in biomass
      cpue_n <- yield_n/et                                                      #cpue in numbers, yield/effort
      cpue_b <- yield_b/et                                                      #cpue in bimoass, yield/effort
      recruits <- nage[,1]                                                      #The recruits at each time step
  #return
    ret.l <- list(scenario = scenario,
                  results=data.frame(time = 1:scenario$sim$month,
                                      N = N,
                                      eggs = eggs,
                                      B = B,
                                      VB = VB,
                                      recruits = recruits,
                                      yield_n = yield_n,
                                      yeild_b = yield_b,
                                      cpue_n = cpue_n,
                                      cpue_b = cpue_b,
                                      et = et,
                                      qt = qt,
                                      hcpue = hcpue,
                                      hpue = hpue,
                                      hr = hr,
                                      pr_hr = pr_hr),
                  matrix = list(nage = nage,
                                hr_sel = hr_sel,
                                hr_harv = hr_harv))
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
    

