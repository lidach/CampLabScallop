### 10/30/2020 Turning generic AS model into monthly scallops 
#   -first make monthly
#   -then input more realistic scallop parms

### Remaining to do:
#     -Probably want to allow the scallops to live maybe 18 months? This would also allow for the possibility that some scallops can
#           -spawn twice. Seems ballpark that this would be a proportion <50% but greater than 10%
#     -make harvest season explicit (i.e. vulnerability off of age or month as opposed to size) or having a vul to harvest and vul to retention
#     -deal with animals that can spawn before they're killed, so need an aggregator to sum eggs in non-recruit months.
#     -consider spatial issues


scenario <- list( 
                 life = list(amax = 18,
                  Ro = 1000,
                  CR = 8,
                  vbk = 4/12,
                  vblinf = 65,
                  vbt0 = 0,
                  alw = 0.0001,
                  alwb = 3,
                  wmat = 12,
                  amat = 10,
                  msd = .1,
                  vlh = 50,
                  vsd = 5,
                  M = 0.2,
                  lorenzc = 1,
                  #this sucks, need to change to 1:amax; probably x/sum(x) and go into EPRo/f
                  prob_spawn = c(0,0,0,0,0,0.1,0.2,0.5,0.8,0.8,0.2,0.1,0.3,0.5,1,1,1,1)),
                  catch = list(FM_flag = "q",
                               q_flag = "constant",
                                effort = 0,
                                q = 0.0003,
                                U = 0,
                                qmax = .0005,
                                bag_unit = "gallon",
                                bag = 2),
                  sim = list(month = 120)
                  )

scallop_model_fun <- function(scenario){
  
  TLref <- scenario$life$vblinf*0.5 #calculate from life values
  #Life history vectors
    scenario$life.vec <- with(scenario$life,{
        life.vec <- data.frame(Age = seq(1,amax))
        life.vec$TL <- vblinf*(1-exp(-vbk*(life.vec$Age-vbt0)))
        life.vec$Wt <- (alw*(life.vec$TL)^alwb)
        life.vec$Vul <- 1/(1+exp(-(life.vec$TL-vlh)/vsd))
        life.vec$Surv <- exp(-(M*TLref/life.vec$TL))^lorenzc
        life.vec$Lo <- life.vec$Lfished <- vector(length=amax)
        life.vec$Lo[1] <- life.vec$Lfished[1] <- 1
        for (i in 2:amax){
          life.vec$Lo[i] <- life.vec$Lo[i-1]*life.vec$Surv[i-1]
          life.vec$Lfished[i] <- life.vec$Lfished[i-1]*life.vec$Surv[i-1]*(1-scenario$catch$U*life.vec$Vul[i-1])
        }
        life.vec$Mat <- 1/(1+exp(-(life.vec$Age-amat)/msd))
        life.vec$Fec <- .1*life.vec$Wt
        life.vec$prob_spawn <- 0
        life.vec$prob_spawn[1:12] <- prob_spawn[1:12]/sum(prob_spawn[1:12])
        life.vec$prob_spawn[13:18] <- prob_spawn[13:18]/sum(prob_spawn[13:18])
        return(life.vec)
    })
  #bag limit 
    #shell height to #'s per gallon
    sh2gal <- function(sh){
      (-6.704*sh + 480.96)/2
    }
    #switch for bag limit units
    scenario$catch <- within(scenario$catch,{
      if(bag_unit=='gallon'){
        maxcat <- bag*2.5             #theoretical max anyone can catch 
        catch.rate <- seq(0,maxcat,by=0.1)
        ret <- pmin(catch.rate,bag)       #how many can be retained for every individual option of numbers caught
        p.legal <- rep(1,length(scenario$life.vec$Age))   #not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
      }else if(bag_unit=="numbers"){
        maxcat <- ceiling(bag*2.5)             #theoretical max anyone can catch 
        catch.rate <- seq(0,maxcat,by=1)
        ret <- pmin(catch.rate,bag)       #how many can be retained for every individual option of numbers caught
        p.legal <- rep(1,length(scenario$life.vec$Age))   #not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))
      }
    })
  #per recruit section
    scenario$per.rec <- with(scenario$life.vec, {
      per.rec <- list()
      per.rec$epro_spawn <- sum(Lo*Fec*Mat*prob_spawn) #eggs-per-recruit unfished conditions
      per.rec$eprf_spawn <- sum(Lfished*Fec*Mat*prob_spawn) #eggs-per-recruit fished conditions
      per.rec$epro <- sum(Lo*Fec*Mat) #eggs-per-recruit unfished conditions
      per.rec$eprf <- sum(Lfished*Fec*Mat) #eggs-per-recruit fished conditions
      per.rec$bpro <- sum(Wt,Lo) #biomass-per-recruit unfished conditions
      per.rec$npro <- sum(Lo) #numbers-per-recruit unfished conditions
      per.rec$vbpro <- sum(Vul*Wt*Lo)  #vulnerable biomass per recruit unfished conditions
      per.rec$vbprf <- sum(Vul*Wt*Lfished) #vulnerable biomiass per recruit fished conditions
      per.rec$spr <- per.rec$epro/per.rec$eprf   #spawning potential ratio
      per.rec$bo <- scenario$life$Ro*per.rec$bpro #biomass at unfished condtions
      per.rec$no <- scenario$life$Ro*per.rec$npro #numbers at unfished conditions
      per.rec$vbo <- scenario$life$Ro*per.rec$vbpro  #vulnerable biomass at unfished conditions
      per.rec$kq <- 2/per.rec$bo   # scaling constant for abundance varying q (from Pine et al 2015)
      return(per.rec)
    })
  #recruitment calculations
    scenario$recruit <- with(scenario$per.rec,{
      recruit <- list()
      recruit$bha <- scenario$life$CR/epro #beverton holt a parm
      recruit$bhb <- (scenario$life$CR-1)/(scenario$life$Ro*epro) #beverton holt b parm
      recruit$bha_spawn <- scenario$life$CR/epro_spawn #beverton holt a parm
      recruit$bhb_spawn <- (scenario$life$CR-1)/(scenario$life$Ro*epro_spawn) #beverton holt b parm
      recruit$r.eq <- (recruit$bha*eprf-1)/(recruit$bhb*eprf) #equilibrium recruitment
      recruit$r.eq_spawn <- (recruit$bha_spawn*eprf_spawn-1)/(recruit$bhb_spawn*eprf_spawn) #equilibrium recruitment
      recruit$yield.eq <- scenario$catch$U*vbprf*recruit$r.eq
      return(recruit)
    })
  #declare locally for easy calling
    amax <- scenario$life$amax
    months <- scenario$sim$month
  #storage
    eggs <- N <- B <- VB <- hr <- yield_n <- yield_b <- cpue_n <- cpue_b <- qt <- et <- hcpue <- hpue <- pr_hr <- vector(length=amax)
    hr_sel <- hr_harv <- nage <- matrix(0, months, amax)
  #initialization
    #numbers
      nage[1,1] <- scenario$life$Ro #initializing first year age structure
      #need the survivors from year before...
      nage[1,13] <- scenario$life$Ro*scenario$life.vec$Lo[13]
      #proc_err[1] = 0 #rnorm(1,0,sdpro) #first value of process error
      # N[1] <- sum(nage[1,]) #first value of total numbers
      B[1] <- sum(nage[1,]*scenario$life.vec$Wt) #first value of total biomass
      # VB[1] <- sum(nage[1,]*scenario$life.vec$Wt*scenario$life.vec$Vul) #first value of vulernable biomass
    #harvest
      #switch for catchability
      if(scenario$catch$q_flag == "constant"){
        qt[1] <- scenario$catch$q #Here fixing q at a constant
      }else if(scenario$catch$q_flag=='VB'){
        qt[1] <- scenario$catch$qmax/(1+scenario$per.rec$kq*B[1])
      }
      et[1] <- scenario$catch$effort #Here fixing effort at a constant
      #swith for fishing mortality
      if(scenario$catch$FM_flag == "q"){ 
        # Here fixing q at a constant
        qt[1] <- scenario$catch$q                                                                  
        hr[1] <- 1-exp(-qt[1]*et[1])
      } else if (scenario$catch$FM_flag == "constant"){
        # Fixing hr as a constant U 
        hr[1] <- scenario$catch$U
      } 
      hr_sel[1,] <- hr[1]*scenario$life.vec$Vul #age-specific capture rate according to selectivity
      hr_harv[1,] <-  hr[1]*scenario$life.vec$Vul*scenario$catch$p.legal #age-specific capture of legally big animals (adjusting hr_sel for size limit)
      hcpue[1] <- sum(nage[1,]*hr_harv[1,])/et[1] #raw catch rates not accounting for bag *also not accounting for loss due to So
      hcpue[1] <- ifelse(is.nan(hcpue[1]),0,hcpue[1])  #trap for cases where effort <- 0, will throw div0 errors (NaN)
      if(scenario$catch$bag_unit=="gallon"){
        require(truncnorm)
        #using a half-normal truncated at zero to get the density of 
        dens.catch <- dtruncnorm(scenario$catch$catch.rate, a=0, mean=hcpue[1], sd=hcpue[1]*0.4)
        dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
        hpue[1] <- sum(dens.catch*scenario$catch$ret)  #rate of actually harvestable fish accounting for bag and size
      }else{
        dens.catch <- dpois(scenario$catch$catch.rate,hcpue[1])
        dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
        hpue[1] <- sum(dens.catch*scenario$catch$ret)  #rate of actually harvestable fish accounting for bag and size
      }
      
      pr_hr[1] <- (hpue[1]/hcpue[1])  #probability of actually harvesting fish of legal size
      pr_hr[1] <- ifelse(is.nan(pr_hr[1]),0,pr_hr[1]) #Another trap, becuase hcpue could be 0
      yield_n[1] <- sum(hr_harv[1,]*pr_hr[1]*nage[1,])
      yield_b[1] <- sum(hr_harv[1,]*pr_hr[1]*nage[1,]*scenario$life.vec$Wt) 
      cpue_n[1] <- yield_n[1]/et[1] #first value of cpue
      cpue_b[1] <- yield_b[1]/et[1] #first value of cpue
  #simulate
    for(i in 2:months){
        #recruitment
          # nage[i,1] <- 0              #the recruitment without process error
          if((i %% 12)==1){
            eggs[i-1] <- sum(scenario$life.vec$Fec * scenario$life.vec$Mat * t(nage[(i-12):(i-1),])*scenario$life.vec$prob_spawn)

            nage[i,1] <- scenario$recruit$bha_spawn*eggs[i-1]/(1+scenario$recruit$bhb_spawn*eggs[i-1])              #the recruitment without process error
          }

        #nage[i,1] = bha*eggs[i-1]/(1+bhb*eggs[i-1])                          #the recruitment without process error
        #nage[i,1] = bha*eggs[i-1]/(1+bhb*eggs[i-1])*exp(proc_err[i-1])       #the recruitment each year, a product of beverton holt, eggs, and process error ****note this is going to be changed to be a function of a habitat dependent b parmameter, see DD habitat excel file
        #Natural mortality
          #Only have age 1 at this point, so need to fill in other ages to calculate catch rates
          nage[i,2:amax] <- nage[i-1,1:(amax-1)]*scenario$life.vec$Surv[1:(amax-1)] #Note this assumes NO natural mortality yet, so these are theoretical catch rates if we think M happens before F in annual cycle
        #bag limit
          #switch for catchability
          if(scenario$catch$q_flag == "constant"){
            qt[i] <- scenario$catch$q #Here fixing q at a constant
          }else if(scenario$catch$q_flag=='VB'){
            qt[i] <- scenario$catch$qmax/(1+scenario$catch$kq*B[i])
          }
          et[i] <- scenario$catch$effort #Here fixing effort at a constant
          #swith for fishing mortality
          if(scenario$catch$FM_flag == "q"){ 
            # Here fixing q at a constant
            qt[i] <- scenario$catch$q                                                                  
            hr[i] <- 1-exp(-qt[i]*et[i])
          } else if (scenario$catch$FM_flag == "constant"){
            # Fixing hr as a constant U 
            hr[i] <- scenario$catch$U
          } 
          hr_sel[i,] <- hr[i]*scenario$life.vec$Vul
          hr_harv[i,] <- hr[i]*scenario$life.vec$Vul*scenario$catch$p.legal
          hcpue[i] <- sum(nage[i,]*hr_harv[i,])/et[i] #expected catch rate of harvestable fish
          hcpue[i] <- ifelse(is.nan(hcpue[i]),0,hcpue[i])  #div0 trap for effort = 0
          if(scenario$catch$bag_unit=="gallon"){
            #using a half-normal truncated at zero to get the density of 
            dens.catch <- dtruncnorm(scenario$catch$catch.rate, a=0, mean=hcpue[i], sd=hcpue[i]*0.4)
            dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
            hpue[i] <- sum(dens.catch*scenario$catch$ret)  #rate of actually harvestable fish accounting for bag and size
          }else{
            dens.catch <- dpois(scenario$catch$catch.rate,hcpue[i])
            dens.catch <- dens.catch/sum(dens.catch) #need to sum to 1 to prevent loss from density range
            hpue[i] <- sum(dens.catch*scenario$catch$ret)  #rate of actually harvestable fish accounting for bag and size
          }
          pr_hr[i] <- (hpue[i]/hcpue[i])  #probabilty of actually harvesting (i.e. landing)
          pr_hr[i] <- ifelse(is.nan(pr_hr[i]),0,pr_hr[i])  #trap for effort =0...meaning hcpue=0
        #Mortality
          nage[i,2:amax] <- nage[i-1,1:(amax-1)]*(1-hr_harv[i-1,1:(amax-1)]*pr_hr[i-1]) 
          #nage[i,j] = nage[i-1,j-1]*Surv[j-1]*(1-Vul[j-1]*hr[i-1])
        #Summarize B for use in catchability
          B[i] = sum(nage[i,] * scenario$life.vec$Wt)
    }
  #Summary (no need to be in for loop)
      N <- rowSums(nage)    #total numbers, sum of numbers at each age
      VB <- nage%*%(scenario$life.vec$Wt*scenario$life.vec$Vul) #vulernable biomass, sum of numbers at age * weight at age * vul at age
      yield_n <- rowSums(hr_harv*nage*pr_hr)#yeild in numbers
      yield_b <-  rowSums(hr_harv*(nage%*%diag(scenario$life.vec$Wt))*pr_hr) #yeild in biomass
      cpue_n <- yield_n/et   #cpue in numbers, yeild/effort
      cpue_b <- yield_b/et  #cpue in bimoass, yeild/effort
      recruits <- nage[,1]
  #return
    ret.l <- list(scenario = scenario,
                  results=data.frame(
                                     time = 1:scenario$sim$month,
                                      N = N,
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
                                      pr_hr = pr_hr))
    return(ret.l)
}
###-----------------------------------------------------
#    Run
###-----------------------------------------------------
  run1 <- scallop_model_fun(scenario)

  #Simple plot of VB, effort
    with(run1$results,{
        par(mfrow=c(1,2))
        plot(time, VB, type="l", col="blue", 
             lwd=3, ylim=c(0, max(VB)))
        lines(time, et, col="red", lwd=3)
        plot(time, recruits, type='l')
    })
    

