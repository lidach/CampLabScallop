### 10/30/2020 Turning generic AS model into monthly scallops 
#   -first make monthly
#   -then input more realistic scallop parms

### Remaining to do:
#     -Probably want to allow the scallops to live maybe 18 months? This would also allow for the possibility that some scallops can
#           -spawn twice. Seems ballpark that this would be a proportion <50% but greater than 10%
#     -make harvest season explicit (i.e. vulnerability off of age or month as opposed to size) or having a vul to harvest and vul to retention
#     -deal with animals that can spawn before they're killed, so need an aggregator to sum eggs in non-recruit months.
#     -consider spatial issues




rm(list = ls())

################################################################################
# Parameters #
################################################################################

### Constants, life history and fishery ###

  amax = 18    #Going to be changed to 18, maximum age, in months *possibly taken from Geiger 2010
  Ro = 1000       #recruitment at unfished conditions *made up, theoretical
  CR = 8          #recruitment compensation ratio *guessed, could look back at CR of other bivalves
  vbk = 4/12      #von bertelanfy metabolic parameter *initially guessed by manually matching to barber and blake, lines kinda up with Wolf & Mendo
  vblinf = 65     #von bertelanfy length infinity parameter, mm shell height *from Geiger
  vbt0 = 0        #von bertelanfy t0 parameter *made up
  alw = 0.0001   #length weight alometry parameter, just altered so asympt. wt = ~ 1 oz, probably *Made up, Jen is going to check on data for this
  alwb = 3        #length weight alometry b parm
  #next 3 parms may be informed by Jen's current study 
  wmat = 12       #weight at maturity *values tied to when we thought 50% maturity occurred in months, then to weights, so this changes with alw
  amat = 10       #age at 50% maturity
  msd = .1        #standard deviation of maturity *made up, will update
  vlh = 50        #length at 50% vulnerability *guessed, Jen will check on histograms
  vsd = 5        #standard deviation of length at 50% vulnerability
  M = 0.2        #natural mortality *Jen's work had the monthly of .17 - .23 (daily 0.0057 - 0.0078) *intial guess was 0.35       
  TLref = vblinf*0.5  #this is the reference length for M--i.e. at what length are we assuming length-invariant M is being measured. 
                      #So, the M=0.35 above is saying M is constant over all ages/sizes, which is probably wrong. But to use the Lorenzen-type 
                      #mortality scaling, we need to say what the constant M is representing.
  lorenzc = 1     #exponent of the scaling of mortality with length. 

  FM_flag = 1	#flag=1 for constant q OR flag=2 for effort varying q, flag=3 for constant U
  effort=2     #fixing effort at a constant level, so changing to deterministic (non-dynamic) effort
  q=0.0003        #fixing q at a constant
  U = 0           #harvest rate, used for equilibrium calculations only. In the model harvest rate (hr) is used as 1-exp(-effort *catchability), allowing for time varying effort
  qmax = .0005       #qmax is the maximum catchability (used in vary q with abundance)
 
### Age-specifc vectors ###
  Age = seq(1,amax)                             #Age
  TL = vblinf*(1-exp(-vbk*(Age-vbt0)))          #Total length
  Wt = (alw*(TL)^alwb)                          #Weight
  Vul = 1/(1+exp(-(TL-vlh)/vsd))                #Vulnerability
  #Surv = (1+vblinf/TL*(exp(vbk)-1))^(-M/vbk)    #Survival in each time step, could have alterantively done: exp(-Madult*linf/base_len)
  Surv = exp(-(M*TLref/TL))^lorenzc             #another, simpler way of looking at age-varying survival. Note this will produce more variation than the above method
  Lo = vector(length=amax); Lo[1]=1;  for (i in 2:amax){Lo[i]=Lo[i-1]*Surv[i-1]}; #Lo[Amax] = Lo[(Amax-1)]*surv[(Amax-1)]/(1-surv[(Amax-1)])       #survivorship in unfished condtions with special survivorship for terminal age
  Lfished = vector(length=amax); Lfished[1]=1;  for (i in 2:amax){Lfished[i]=Lfished[i-1]*Surv[i-1]*(1-U*Vul[i-1])}; #Lfished[Amax] = Lfished[(Amax-1)]*surv[(Amax-1)]*(1-U*vul[(Amax-1)])/(1-surv[(Amax-1)]*(1-U*vul[(Amax-1)]))       #Standard with special calculation for terminal fished survivorship

  Mat = 1/(1+exp(-(Age-amat)/msd))                #Vulnerability
  Fec = .1*Wt # the 0.1 is from the Barber & Blake paper, but Jen will double check
  #Fec = ifelse((Wt-Wt[amat])<0,0,Wt-Wt[amat])           #Fecundity

  #Bag limit stuff
    #Calculating # scallops per gallon -- first need a rough bag limit, i.e. how many scallops does it take to fill a limit?
    #from Zach citing Geiger 2006
    ash = 50  #avg shell height in mm??
    gs = (-6.704*ash + 480.96)/2  #So...this is obviously not correct. I either missed something or the units are off. I couldn't find this equation in Geiger 2006
    #Geiger also showed that across most of the areas sampled, about 100-125 scallops fills the 2gal limit in July & Aug (peak months), so going with bag of 115
    bag <- 115                      #assumed bag limit
    maxcat <- (bag*2.5)             #theoretical max anyone can catch 
    ret <- pmin(0:maxcat,bag)       #how many can be retained for every individual option of numbers caught
    p.legal <- rep(1,length(Age))   #not actually bag but would be used if there was a minimum size limit like 1/(1+exp(-1.7*(TL-MLL)/0.07))


# plot(Age, TL)
# plot(Age, Mat)
# plot(Age, Fec)

### Calculations from vectors ###
  epro = sum(Lo*Fec)                    #eggs-per-recruit unfished conditions
  eprf = sum(Lfished*Fec)               #eggs-per-recruit fished conditions
  bpro = sum(Wt,Lo)                     #biomass-per-recruit unfished conditions
  npro = sum(Lo)                        #numbers-per-recruit unfished conditions
  vbpro = sum(Vul*Wt*Lo)                #vulnerable biomass per recruit unfished conditions
  vbprf = sum(Vul*Wt*Lfished)           #vulnerable biomiass per recruit fished conditions
  spr = epro/eprf                       #spawning potential ratio
  bo = Ro*bpro                          #biomass at unfished condtions
  no = Ro*npro                          #numbers at unfished conditions
  vbo = Ro*vbpro                        #vulnerable biomass at unfished conditions
  kq = 2/bo                             # scaling constant for abundance varying q (from Pine et al 2015)
  #recruitment parms and calcs#

  ### THESE NEED TO BE ADJUSTMENT TO DEAL WITH PROP SPAWN
    bha = CR/epro                         #beverton holt a parm
    bhb = (CR-1)/(Ro*epro)                #beverton holt b parm
    r.eq = (bha*eprf-1)/(bhb*eprf)        #equilibrium recruitment
    yield.eq = U*vbprf*r.eq

    #looking at the beverton holt stock-recruit relationships--you can change CR and re-run to see how this changes
      stocksize <- seq(10, 1000, length.out=100)
      recruits <- bha*stocksize/(1+bhb*stocksize)
      plot(stocksize,recruits, type="l")

################################################################################
### time and age dynamics ###
################################################################################
  months = 120
  #setting up space for model outputs
    eggs = vector(length=months); N=vector(length=months); B=vector(length=months); VB=vector(length=months); hr=vector(length=months); 
    yield_n = vector(length=months); yield_b = vector(length=months);cpue_n = vector(length=months); cpue_b = vector(length=months); 
    qt = vector(length=months); et = vector(length=months); hcpue=vector(length=months); hpue=vector(length=months); pr_hr=vector(length=months);
    hr_sel=matrix(0,months,amax); hr_harv=matrix(0,months,amax);

  #numbers at age of fish
  nage = matrix(0,months,amax)                                                     #set up matrix
      #initializing first year age structure
      nage[1,1]=Ro#*Lo                                                              #numbers at age, year 1
      nage[cbind(1:amax,1:amax)] <- Ro*Lo
      #initializing first year of all year dependent values, note, these must go in a specific order
      prop_spawn<-c(0,0,0,0,0,0.1,0.2,0.5,0.8,0.8,0.2,0.1)                          #proportion of indv that spawn in each month (across ages), currently dummy values 
      #prop_spawn <- prop_spawn/sum(prop_spawn) ### requires adjustment of a/b BH rec
      prop_2spawn <- c(0.3,0.5,1,0,0,0,0,0,0,0,0,0)
      #prop_2spawn <- prop_2spawn/sum(prop_2spawn) ### requires adjustment of a/b BH rec

      prop_bspawn <- prop_spawn + prop_2spawn
      #initializing first year of all year dependent values, note, these must go in a specific order
      prop_spawn<-c(0,0,0,0,0,0.2,0.2,0.5,0.8,0.8,0.2,0)                          #proportion of indv that spawn in each month (across ages), currently dummy values 
      #proc_err[1] = 0 #rnorm(1,0,sdpro)                                          #first value of process error
      N[1] = sum(nage[1,])                                                        #first value of total numbers
      B[1] = sum(nage[1,]*Wt)                                                     #first value of total biomass
      VB[1] = sum(nage[1,]*Wt*Vul)                                                #first value of vulernable biomass
      qt[1] = q                                                                   #Here fixing q at a constant
      et[1] = effort                                                              #Here fixing effort at a constant
  	  # fishing mortality estimates                                                #first value of vulernable biomass 
    	  if(FM_flag == 1){                                                           # Here fixing q at a constant
      	 	qt[1] = q                                                                  
      	 	hr[1] = 1-exp(-qt[1]*et[1])
    	  } else if (FM_flag == 2){                                                  # first value for varing with effort or abundance (VB) (from Pine et al 2015)
      	 	qt[1] = qmax/(1+kq*B[i])
      	 	hr[1] = 1-exp(-qt[1]*et[1])
    	  } else if (FM_flag == 3){                                                  # Fixing hr as a constant U 
     		 hr[1] = U
       	}                                                #first value of harvest rate
      hr_sel[1,] = hr[1]*Vul                                                      #age-specific capture rate according to selectivity
      hr_harv[1,] =  hr[1]*Vul*p.legal                                            #age-specific capture of legally big animals (adjusting hr_sel for size limit)
      hcpue[1] = sum(nage[1,]*hr_harv[1,])/et[1]                                     #raw catch rates not accounting for bag *also not accounting for loss due to So
      hcpue[1] = ifelse(is.nan(hcpue[1]),0,hcpue[1])                              #trap for cases where effort = 0, will throw div0 errors (NaN)
      hpue[1] = sum(dpois(0:maxcat,hcpue[1])*ret)                                    #rate of actually harvestable fish accounting for bag and size
      pr_hr[1] = (hpue[1]/hcpue[1])                                               #probability of actually harvesting fish of legal size
      pr_hr[1] = ifelse(is.nan(pr_hr[1]),0,pr_hr[1])                              #Another trap, becuase hcpue could be 0
      yield_n[1] = sum(hr_harv[1,]*pr_hr[1]*nage[1,])
      yield_b[1] = sum(hr_harv[1,]*pr_hr[1]*nage[1,]*Wt) 
      cpue_n[1] = yield_n[1]/et[1]                                                    #first value of cpue
      cpue_b[1] = yield_b[1]/et[1]                                                    #first value of cpue

      eggs <- rep(0, months)                                                          #Filling in eggs (in each month) vector with zeroes

      #calculations for vectors--i.e. year but not age varying components of model
      for(i in 2:months){
        #recruitment
          nage[i,1] = 0              #the recruitment without process error
          if(i %% 12==1){
            eggs[i-1] <- sum((Fec * Mat * t(nage[(i-12):(i-1),])) %*% diag(prop_bspawn))

            nage[i,1] = bha*eggs[i-1]/(1+bhb*eggs[i-1])              #the recruitment without process error
          }

        #nage[i,1] = bha*eggs[i-1]/(1+bhb*eggs[i-1])                          #the recruitment without process error
        #nage[i,1] = bha*eggs[i-1]/(1+bhb*eggs[i-1])*exp(proc_err[i-1])       #the recruitment each year, a product of beverton holt, eggs, and process error ****note this is going to be changed to be a function of a habitat dependent b parmameter, see DD habitat excel file

        #bag limit
          qt[i] = q                                                                  #catchability when fixed as a constant
          et[i] = effort 
          hr[i] = 1-exp(-(et[i]*q))
          hr_sel[i,] = hr[i]*Vul
          hr_harv[i,] = hr[i]*Vul*p.legal
            for(j in 2:amax) {                                                      #Only have age 1 at this point, so need to fill in other ages to calculate catch rates
               nage[i,j]=nage[i-1,j-1]*Surv[j-1]                                    #Note this assumes NO natural mortality yet, so these are theoretical catch rates if we think M happens before F in annual cycle
               }
          hcpue[i] = sum(nage[i,]*hr_harv[i,])/et[i]                                #expected catch rate of harvestable fish
          hcpue[i] = ifelse(is.nan(hcpue[i]),0,hcpue[i])                            #div0 trap for effort = 0
          hpue[i] = sum(dpois(0:maxcat,hcpue[i])*ret)                                  #poisson prob of each capture event * number of fish that can be retained on each capture event, summed
          pr_hr[i] = (hpue[i]/hcpue[i])                                     #probabilty of actually harvesting (i.e. landing)
          pr_hr[i] = ifelse(is.nan(pr_hr[i]),0,pr_hr[i])                            #trap for effort =0...meaning hcpue=0

        for(j in 2:amax) {
          nage[i,j] = nage[i-1,j-1]*Surv[j-1]*
          (1-hr_harv[i-1,j-1]*pr_hr[i-1]) 
          
          #nage[i,j] = nage[i-1,j-1]*Surv[j-1]*(1-Vul[j-1]*hr[i-1])

        }
              
        N[i] = sum(nage[i,])                                                      #total numbers, sum of numbers at each age
        B[i] = sum(nage[i,]*Wt)                                                   #total biomass, sum of numbers at age * weight at age
        VB[i] = sum(nage[i,]*Wt*Vul)                                              #vulernable biomass, sum of numbers at age * weight at age * vul at age
        yield_n[i] = sum(hr_harv[i,]*pr_hr[i]*nage[i,])                           #yeild in numbers
        yield_b[i] = sum(hr_harv[i,]*pr_hr[i]*nage[i,]*Wt)                        #yeild in biomass
        cpue_n[i] = yield_n[i]/et[i]                                                   #cpue in numbers, yeild/effort
        cpue_b[i] = yield_b[i]/et[i]                                                   #cpue in bimoass, yeild/effort
      }

################################################################################
### Report ###
################################################################################
  #Simple plot of VB, effort
    plot(1:months, VB, type="l", col="blue", lwd=3, ylim=c(0, max(VB)))
    lines(1:months, et, col="red", lwd=3)


    plot(1:months, nage[,1], type='l')

