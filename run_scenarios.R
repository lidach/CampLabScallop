#######################################
#Scenarios
#######################################
source("scallop_model_fun_Baranov.R")

scen_str <- list()
scen_str_base <- list()

# base model
# scenario$life$semelparous <- FALSE
scen_str_base$base_63_reg <- scallop_model_fun(scenario) # 63% spr, regular effort
scen_str_base$base_63_reg$scenario$per.rec$spr # ~63%

#Decrease Bag, Basically unfished scenario
    scenario1 <- scenario
    scenario1$catch$bag <- rep(0.001,12)
    scen_str$scen1 <- scallop_model_fun(scenario1)

# bag limit = 3
    scenario2 <- scenario
    scenario2$catch$bag <- rep(3,12)
    scen_str$scen2 <- scallop_model_fun(scenario2)

# bag limit = 1
    scenario3 <- scenario
    scenario3$catch$bag <- rep(1,12)
    scen_str$scen3 <- scallop_model_fun(scenario3)

# season length - increase in total effort, extend one month forward
    scenario4 <- scenario
    scenario4$catch$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0)
    scen_str$scen4 <- scallop_model_fun(scenario4)

# season length - same effort, extend one month forward
    scenario5 <- scenario
    scenario5$catch$E_open <- c(0,0,0,0,0,0,1,1,1,1,0,0)
    scenario5$catch$E_cap <- TRUE
    scen_str$scen5 <- scallop_model_fun(scenario5)

# season length - increase in total effort, extend one month back
    scenario6 <- scenario
    scenario6$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,1,0)
    scen_str$scen6 <- scallop_model_fun(scenario6)

# season length - same effort, extend one month back
    scenario7 <- scenario
    scenario7$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,1,0)
    scenario7$catch$E_cap <- TRUE
    scen_str$scen7 <- scallop_model_fun(scenario7)

# push season back one month
    scenario8 <- scenario
    scenario8$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
    scen_str$scen8 <- scallop_model_fun(scenario8)

# push season forward one month
    scenario9 <- scenario
    scenario9$catch$E_open <- c(0,0,0,0,0,1,1,1,0,0,0,0)
    scen_str$scen9 <- scallop_model_fun(scenario9)

# rolling bag limit - increasing
    scenario10 <- scenario
    scenario10$catch$bag <- c(1,1,1,1,1,1,0.5,1,1.5,2,2,2)
    scen_str$scen10 <- scallop_model_fun(scenario10)

# rolling bag and season start - back one month
    scenario11 <- scenario
    scenario11$catch$bag <- c(1,1,1,1,1,1,1,0.5,1,1.5,2,2)
    scenario11$catch$E_open <- c(0,0,0,0,0,0,0,1,1,1,0,0)
    scen_str$scen11 <- scallop_model_fun(scenario11)

# rolling bag and season start - forward one month
    scenario12 <- scenario
    scenario12$catch$bag <- c(1,1,1,1,1,1,0.5,1,1.5,2,2,2)
    scenario12$catch$E_open <- c(0,0,0,0,0,0,1,1,1,0,0,0)
    scen_str$scen12 <- scallop_model_fun(scenario12)

# # iteroparity
#     scenario11 <- scenario
#     scenario11$life$semelparous <- TRUE
#     scen_str$scen11 <- scallop_model_fun(scenario11)

##################
# sensitivity runs
##################
sens_str <- list()
perchange <- 0.25 # +/- 25%
# changes in catchability - look at hcpue and hr
    senscen_q1 <- scenario
    senscen_q2 <- scenario
    senscen_q1$catch$q <- scenario$catch$q*(1-perchange)
    senscen_q2$catch$q <- scenario$catch$q*(1+perchange)
    sens_str$senq1 <- scallop_model_fun(senscen_q1)
    sens_str$senq2 <- scallop_model_fun(senscen_q2)

# M
    senscen_M1 <- scenario
    senscen_M2 <- scenario
    senscen_M1$life$M <- scenario$life$M*(1-perchange)
    senscen_M2$life$M <- scenario$life$M*(1+perchange)
    sens_str$senM1 <- scallop_model_fun(senscen_M1)
    sens_str$senM2 <- scallop_model_fun(senscen_M2)

# vbk
    senscen_vbk1 <- scenario
    senscen_vbk2 <- scenario
    senscen_vbk1$life$vbk <- scenario$life$vbk*(1-perchange)
    senscen_vbk2$life$vbk <- scenario$life$vbk*(1+perchange)
    sens_str$senvbk1 <- scallop_model_fun(senscen_vbk1)
    sens_str$senvbk2 <- scallop_model_fun(senscen_vbk2)

# CR
    senscen_CR1 <- scenario
    senscen_CR2 <- scenario
    senscen_CR1$life$CR <- scenario$life$CR*(1-perchange)
    senscen_CR2$life$CR <- scenario$life$CR*(1+perchange)
    sens_str$senCR1 <- scallop_model_fun(senscen_CR1)
    sens_str$senCR2 <- scallop_model_fun(senscen_CR2)

# amat?
    senscen_amat1 <- scenario
    senscen_amat2 <- scenario
    senscen_amat1$life$amat <- scenario$life$amat*(1-perchange)
    senscen_amat2$life$amat <- scenario$life$amat*(1+perchange)
    sens_str$senamat1 <- scallop_model_fun(senscen_amat1)
    sens_str$senamat2 <- scallop_model_fun(senscen_amat2)


##################################
## run mgmt scenarios with different levels of q and eff 
    # base scenarios for each panel
    # perchange1 <- 0.3 # ~45% SPR
    # perchange2 <- 1.57 # ~30% SPR

  	E1 <- seq(1,2,length.out=25) # effort - increasing between years (doubling over 25 years)
    E2 <- seq(1,3,length.out=25) # effort - increasing between years (tripling over 25 years)
    
    # different base lines
    # 2 base_45_reg
    	perchange2 <- 0.964
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange2)
	    scen_str_base$base_45_reg <- scallop_model_fun(scenario0)
	    scen_str_base$base_45_reg$scenario$per.rec$spr
    # 3 base_30_reg
	    perchange3 <- 2.86
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange3)
	    scen_str_base$base_30_reg <- scallop_model_fun(scenario0)
	    scen_str_base$base_30_reg$scenario$per.rec$spr
    # 4 base_63_double
	    perchange4 <- -0.324
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario0$catch$E_years <- E1
	    scen_str_base$base_62_double <- scallop_model_fun(scenario0)
	    scen_str_base$base_62_double$scenario$per.rec$spr
    # 5 base_45_double
	    perchange5 <- 0.31
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario0$catch$E_years <- E1
	    scen_str_base$base_45_double <- scallop_model_fun(scenario0)
	    scen_str_base$base_45_double$scenario$per.rec$spr
    # 6 base_30_double
	    perchange6 <- 1.58
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario0$catch$E_years <- E1
	    scen_str_base$base_30_double <- scallop_model_fun(scenario0)
	    scen_str_base$base_30_double$scenario$per.rec$spr
    # 7 base_63_triple
	    perchange7 <- -0.493
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario0$catch$E_years <- E2
	    scen_str_base$base_63_triple <- scallop_model_fun(scenario0)
	    scen_str_base$base_63_triple$scenario$per.rec$spr
    # 8 base_45_triple
	    perchange8 <- -0.018
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario0$catch$E_years <- E2
	    scen_str_base$base_45_triple <- scallop_model_fun(scenario0)
	    scen_str_base$base_45_triple$scenario$per.rec$spr
    # 9 base_30_triple
	    perchange9 <- 0.93
	    scenario0 <- scenario
	    scenario0$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario0$catch$E_years <- E2
	    scen_str_base$base_30_triple <- scallop_model_fun(scenario0)
	    scen_str_base$base_30_triple$scenario$per.rec$spr


    # 2 - decrease SPR 45% (scen_str2)
    scen_str2 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange2)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange2)

	    scen_str2$scen1 <- scallop_model_fun(scenario1)
	    scen_str2$scen2 <- scallop_model_fun(scenario2)
	    scen_str2$scen3 <- scallop_model_fun(scenario3)
	    scen_str2$scen4 <- scallop_model_fun(scenario4)
	    scen_str2$scen5 <- scallop_model_fun(scenario5)
	    scen_str2$scen6 <- scallop_model_fun(scenario6)
	    scen_str2$scen7 <- scallop_model_fun(scenario7)
	    scen_str2$scen8 <- scallop_model_fun(scenario8)
	    scen_str2$scen9 <- scallop_model_fun(scenario9)
	    scen_str2$scen10 <- scallop_model_fun(scenario10)
	    scen_str2$scen11 <- scallop_model_fun(scenario11)
	    scen_str2$scen12 <- scallop_model_fun(scenario12)

    # 3 - decrease SPR 30% (scen_str3)
    scen_str3 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange3)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange3)

	    scen_str3$scen1 <- scallop_model_fun(scenario1)
	    scen_str3$scen2 <- scallop_model_fun(scenario2)
	    scen_str3$scen3 <- scallop_model_fun(scenario3)
	    scen_str3$scen4 <- scallop_model_fun(scenario4)
	    scen_str3$scen5 <- scallop_model_fun(scenario5)
	    scen_str3$scen6 <- scallop_model_fun(scenario6)
	    scen_str3$scen7 <- scallop_model_fun(scenario7)
	    scen_str3$scen8 <- scallop_model_fun(scenario8)
	    scen_str3$scen9 <- scallop_model_fun(scenario9)
	    scen_str3$scen10 <- scallop_model_fun(scenario10)
	    scen_str3$scen11 <- scallop_model_fun(scenario11)
	    scen_str3$scen12 <- scallop_model_fun(scenario12)

    # 4 - increase eff double (scen_str4)
	scen_str4 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange4)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange4)
		scenario1$catch$E_years <- E1
	    scenario2$catch$E_years <- E1
	    scenario3$catch$E_years <- E1
	    scenario4$catch$E_years <- E1
	    scenario5$catch$E_years <- E1
	    scenario6$catch$E_years <- E1
	    scenario7$catch$E_years <- E1
	    scenario8$catch$E_years <- E1
	    scenario9$catch$E_years <- E1
	    scenario10$catch$E_years <- E1
	    scenario11$catch$E_years <- E1
	    scenario12$catch$E_years <- E1

	    scen_str4$scen1 <- scallop_model_fun(scenario1)
	    scen_str4$scen2 <- scallop_model_fun(scenario2)
	    scen_str4$scen3 <- scallop_model_fun(scenario3)
	    scen_str4$scen4 <- scallop_model_fun(scenario4)
	    scen_str4$scen5 <- scallop_model_fun(scenario5)
	    scen_str4$scen6 <- scallop_model_fun(scenario6)
	    scen_str4$scen7 <- scallop_model_fun(scenario7)
	    scen_str4$scen8 <- scallop_model_fun(scenario8)
	    scen_str4$scen9 <- scallop_model_fun(scenario9)
	    scen_str4$scen10 <- scallop_model_fun(scenario10)
	    scen_str4$scen11 <- scallop_model_fun(scenario11)
	    scen_str4$scen12 <- scallop_model_fun(scenario12)

    # 5 - increase eff double decrease SPR 45% (scen_str5)
    scen_str5 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange5)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange5)

	    scen_str5$scen1 <- scallop_model_fun(scenario1)
	    scen_str5$scen2 <- scallop_model_fun(scenario2)
	    scen_str5$scen3 <- scallop_model_fun(scenario3)
	    scen_str5$scen4 <- scallop_model_fun(scenario4)
	    scen_str5$scen5 <- scallop_model_fun(scenario5)
	    scen_str5$scen6 <- scallop_model_fun(scenario6)
	    scen_str5$scen7 <- scallop_model_fun(scenario7)
	    scen_str5$scen8 <- scallop_model_fun(scenario8)
	    scen_str5$scen9 <- scallop_model_fun(scenario9)
	    scen_str5$scen10 <- scallop_model_fun(scenario10)
	    scen_str5$scen11 <- scallop_model_fun(scenario11)
	    scen_str5$scen12 <- scallop_model_fun(scenario12)

    # 6 - increase eff double decrease SPR 30% (scen_str6)
    scen_str6 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange6)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange6)

	    scen_str6$scen1 <- scallop_model_fun(scenario1)
	    scen_str6$scen2 <- scallop_model_fun(scenario2)
	    scen_str6$scen3 <- scallop_model_fun(scenario3)
	    scen_str6$scen4 <- scallop_model_fun(scenario4)
	    scen_str6$scen5 <- scallop_model_fun(scenario5)
	    scen_str6$scen6 <- scallop_model_fun(scenario6)
	    scen_str6$scen7 <- scallop_model_fun(scenario7)
	    scen_str6$scen8 <- scallop_model_fun(scenario8)
	    scen_str6$scen9 <- scallop_model_fun(scenario9)
	    scen_str6$scen10 <- scallop_model_fun(scenario10)
	    scen_str6$scen11 <- scallop_model_fun(scenario11)
	    scen_str6$scen12 <- scallop_model_fun(scenario12)


    # 7 - increase eff triple (scen_str7)
    scen_str7 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange7)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange7)
    	scenario1$catch$E_years <- E2
	    scenario2$catch$E_years <- E2
	    scenario3$catch$E_years <- E2
	    scenario4$catch$E_years <- E2
	    scenario5$catch$E_years <- E2
	    scenario6$catch$E_years <- E2
	    scenario7$catch$E_years <- E2
	    scenario8$catch$E_years <- E2
	    scenario9$catch$E_years <- E2
	    scenario10$catch$E_years <- E2
	    scenario11$catch$E_years <- E2
	    scenario12$catch$E_years <- E2

	    scen_str7$scen1 <- scallop_model_fun(scenario1)
	    scen_str7$scen2 <- scallop_model_fun(scenario2)
	    scen_str7$scen3 <- scallop_model_fun(scenario3)
	    scen_str7$scen4 <- scallop_model_fun(scenario4)
	    scen_str7$scen5 <- scallop_model_fun(scenario5)
	    scen_str7$scen6 <- scallop_model_fun(scenario6)
	    scen_str7$scen7 <- scallop_model_fun(scenario7)
	    scen_str7$scen8 <- scallop_model_fun(scenario8)
	    scen_str7$scen9 <- scallop_model_fun(scenario9)
	    scen_str7$scen10 <- scallop_model_fun(scenario10)
	    scen_str7$scen11 <- scallop_model_fun(scenario11)
	    scen_str7$scen12 <- scallop_model_fun(scenario12)

    # 8 - increase eff triple decrease SPR 45% (scen_str8)
	scen_str8 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange8)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange8)

	    scen_str8$scen1 <- scallop_model_fun(scenario1)
	    scen_str8$scen2 <- scallop_model_fun(scenario2)
	    scen_str8$scen3 <- scallop_model_fun(scenario3)
	    scen_str8$scen4 <- scallop_model_fun(scenario4)
	    scen_str8$scen5 <- scallop_model_fun(scenario5)
	    scen_str8$scen6 <- scallop_model_fun(scenario6)
	    scen_str8$scen7 <- scallop_model_fun(scenario7)
	    scen_str8$scen8 <- scallop_model_fun(scenario8)
	    scen_str8$scen9 <- scallop_model_fun(scenario9)
	    scen_str8$scen10 <- scallop_model_fun(scenario10)
	    scen_str8$scen11 <- scallop_model_fun(scenario11)
	    scen_str8$scen12 <- scallop_model_fun(scenario12)

    # 9 - increase eff tripe decrease SPR 30% (scen_str9)
    scen_str9 <- list()
	    scenario1$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario2$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario3$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario4$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario5$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario6$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario7$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario8$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario9$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario10$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario11$catch$q <- scenario$catch$q*(1+perchange9)
	    scenario12$catch$q <- scenario$catch$q*(1+perchange9)

	    scen_str9$scen1 <- scallop_model_fun(scenario1)
	    scen_str9$scen2 <- scallop_model_fun(scenario2)
	    scen_str9$scen3 <- scallop_model_fun(scenario3)
	    scen_str9$scen4 <- scallop_model_fun(scenario4)
	    scen_str9$scen5 <- scallop_model_fun(scenario5)
	    scen_str9$scen6 <- scallop_model_fun(scenario6)
	    scen_str9$scen7 <- scallop_model_fun(scenario7)
	    scen_str9$scen8 <- scallop_model_fun(scenario8)
	    scen_str9$scen9 <- scallop_model_fun(scenario9)
	    scen_str9$scen10 <- scallop_model_fun(scenario10)
	    scen_str9$scen11 <- scallop_model_fun(scenario11)
	    scen_str9$scen12 <- scallop_model_fun(scenario12)


##################################
# save all scenarios to one .RData
scen_str_all <- list(scen_str = scen_str,
					scen_str2 = scen_str2,
					scen_str3 = scen_str3,
					scen_str4 = scen_str4,
					scen_str5 = scen_str5,
					scen_str6 = scen_str6,
					scen_str7 = scen_str7,
					scen_str8 = scen_str8,
					scen_str9 = scen_str9)
    save(scen_str_base,
    	scen_str_all,
    	sens_str,
     file = "./Scenario_Figures/semel_scenario_sensitivity_storage.RData")

    rm(list=ls())
    gc()
    load("./Scenario_Figures/semel_scenario_sensitivity_storage.RData")

#################################
    #this function helps do scientific notation without the e+XX stuff
    sci.transl <- function(range){
        x <- gsub("^[[:graph:]]+e\\+","",formatC(max(range),format='g'))
        v <- switch(x,
               '01'="10's",
               '02'="100's",
               '03'="1000's",
               '04'="10's of Thousands",
               '05'="100's of Thousands",
               '06'="Millions",
               '07'="10's of Millions",
               '08'="100's of Millions",
               '09'="Billions")
        return(list(scale = as.numeric(paste0('1e',x)),
                    label = v,
                    r = range/as.numeric(paste0('1e',x))))
    }

##################################
    #grab the global ranges for dynamic settings
    scen_str <- scen_str_all$scen_str
    vb.r <- range(sapply(scen_str,function(x)range(x$results$VB,na.rm=T)))
    et.r <- range(sapply(scen_str,function(x)range(x$results$et,na.rm=T)))
    yield.r <- range(sapply(scen_str,function(x)range(x$results$yield_n,na.rm=T)))
    rec.r <- range(sapply(scen_str,function(x)range(x$results$recruits,na.rm=T)))
    eggs.r <- range(sapply(scen_str,function(x)range(x$results$eggs_mon,na.rm=T)))

    #plot scenarios
    plot_out_type <- "tiff"
    for(i in 1:length(scen_str)){
        if(i==1){
            if(plot_out_type == "pdf") pdf(file=paste0("./Scenario_Figures/scenario_base_results.pdf"))
            if(plot_out_type == "tiff") tiff(filename=paste0("./Scenario_Figures/scenario_base_results.tiff"),height = 17, width = 23, units = 'cm', compression = "lzw", res = 500)

        }else{
            if(plot_out_type == "pdf"){
                pdf(file=paste0("./Scenario_Figures/scenario_",
                        formatC(i-1,width=2,flag="0"),
                        "_results.pdf"))
            }
            if(plot_out_type == "tiff"){
                tiff(filename=paste0("./Scenario_Figures/scenario_",
                        formatC(i-1,width=2,flag="0"),
                        "_results.tiff"),height = 17, width = 23, units = 'cm', compression = "lzw", res = 500)
            }
        }
        
            with(scen_str[[i]]$results,{
                Nt <- nrow(scen_str[[i]]$results)
              par(mfrow=c(3,2), mar=c(2,3.5,1,1), las=1, mgp=c(2.5,0.5,0), tck=-0.015)
              #plot 1
                vb.sci <- sci.transl(vb.r)
                plot(time, VB/vb.sci$scale, type="l", col="blue",
                     ylab=paste0("Vul. Biomass (",vb.sci$label,")"),
                     lwd=3, ylim=vb.sci$r)
                abline(lm(VB/vb.sci$scale~time), lwd=2, lty=1)
                abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
              #plot 2
                plot(time, N/N[1], type='l', ylab = "Depletion (prop. Numbers)", ylim=c(0,1), lwd=2, col="mediumpurple4")
                abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
              #plot 3
                et2 <- et
                et2[et2==0] <- NA
                #  plot(time, et2, col="red", lwd=3, type='l',ylab="Effort (Vessels)", ylim=c(0,50000))
                et.sci <- sci.transl(et.r)
                plot(time, et2/et.sci$scale, col="red", 
                     lwd=3, type='l',
                     ylab=paste0("Effort (Persons; ", et.sci$label,")"), 
                         ylim=et.sci$r)
                points(time, et2/et.sci$scale, col='red', pch=16)
                abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
              #plot 4
                yield.sci <- sci.transl(yield.r)
                yield2 <- yield_n
                yield2[yield_n==0] <- NA
                plot(time, yield2/yield.sci$scale, 
                     type='l', pch=16, lwd=3,
                     ylab = paste0("Catch (numbers; ",yield.sci$label,")"),
                      ylim=yield.sci$r, col="salmon3", cex=1.5)
                abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
                points(time, yield2/yield.sci$scale,col="salmon3",pch=16)
              #plot 5
                rec.sci <- sci.transl(rec.r)
                plot(time[recruits>0], recruits[recruits>0]/rec.sci$scale, type='b', pch=16, 
                     ylab = paste0("Recruits (",rec.sci$label,")"), 
                     ylim=rec.sci$r, col='darkgreen', cex=1.5)
              #plot 6
                par(mar=c(2,3.5,1,3.8), las=1, mgp=c(2.5,0.5,0), tck=-0.015)
                eggs.sci <- sci.transl(eggs.r)
                plot(time, eggs_mon/eggs.sci$scale, type='l', 
                     ylab = paste0("Eggs (Monthly; ", eggs.sci$label,")"), 
                     ylim=eggs.sci$r, lwd=2, col="seagreen")
                abline(v=seq(0.5,Nt+0.5,12), lty=2, col='gray70')
                par(new=T)
				   plot(time[eggs>0], eggs[eggs>0]/sum(scen_str[[i]]$scenario$life$Ro * scen_str[[i]]$scenario$life.vec$Lo *  scen_str[[i]]$scenario$life.vec$Mat * scen_str[[i]]$scenario$life.vec$Fec *  scen_str[[i]]$scenario$life.vec$prob_spawn), 
				   	type="l", ylim=c(0,1) ,ylab="", xlab="", xaxt="n", yaxt="n")
				   points(time[eggs>0], eggs[eggs>0]/sum(scen_str[[i]]$scenario$life$Ro * scen_str[[i]]$scenario$life.vec$Lo *  scen_str[[i]]$scenario$life.vec$Mat * scen_str[[i]]$scenario$life.vec$Fec *  scen_str[[i]]$scenario$life.vec$prob_spawn), pch = 16)
				   axis(side=4, at=seq(0,1,0.2), labels=seq(0,1,0.2), las=1)
				   mtext(text="Depletion (Eggs Deposited)", side=4,line=2.5,las=0, outer=F, cex=0.7)
            })
        dev.off()
    }

rm(list=setdiff(ls(), c("scen_str_base","scen_str_all","sens_str")))
gc()

##################################
## bar plots
##################################
## extracting base lines values (from scen_str_base)
	eggs0 <- sapply(1:9, function(x) scen_str_base[[x]]$results$eggs[12])
		eggs_list <- sapply(1:9, function(x) mean(c(scen_str_base[[x]]$results$eggs[21:24*12]), sum(scen_str_base[[x]]$results$eggs_mon[289:300])))
		depl_base <- eggs_list/eggs0
	hcpue_list <- lapply(1:9, function(x) matrix(scen_str_base[[x]]$results$hcpue, nrow = 12, ncol = 25))
		for(i in 1:9) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
		hcpue_base <- sapply(1:9, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
		hcpue_base <- sapply(1:9, function(x) mean(hcpue_base[21:25,x]))
	hr_list <- lapply(1:9, function(x) matrix(scen_str_base[[x]]$results$hr, nrow = 12, ncol = 25))
		for(i in 1:9) hr_list[[i]][hr_list[[i]] == 0] <- NA
		hr_base <- sapply(1:9, function(x) colMeans(hr_list[[x]], na.rm = TRUE))
		hr_base <- sapply(1:9, function(x) mean(hr_base[21:25,x]))


## extracting values from all treatments and scenarios
	depl_rel <- matrix(NA, nrow = 9, ncol = 12)
	hcpue_rel <- matrix(NA, nrow = 9, ncol = 12)
	hr_rel <- matrix(NA, nrow = 9, ncol = 12)
	depl_abs <- matrix(NA, nrow = 9, ncol = 12)
	hcpue_abs <- matrix(NA, nrow = 9, ncol = 12)
	hr_abs <- matrix(NA, nrow = 9, ncol = 12)

	for(j in 1:9){
	# depletion
		eggs0 <- sapply(1:12, function(x) scen_str_all[[j]][[x]]$results$eggs[12])
		eggs_list <- sapply(1:12, function(x) mean(c(scen_str_all[[j]][[x]]$results$eggs[21:24*12]),sum(scen_str_all[[j]][[x]]$results$eggs_mon[289:300])))
		depl <- eggs_list/eggs0
	# hcpue (open months, last 5 years) - is there a better way to do this?
        hcpue_list <- lapply(1:12, function(x) matrix(scen_str_all[[j]][[x]]$results$hcpue, nrow = 12, ncol = 25))
        for(i in 1:12) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
        hcpue <- sapply(1:12, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
        hcpue <- sapply(1:12, function(x) mean(hcpue[21:25,x]))    
    # hr (open montths, last 5 years)
       hr_list <- lapply(1:12, function(x) matrix(scen_str_all[[j]][[x]]$results$hr, nrow = 12, ncol = 25))
        for(i in 1:12) hr_list[[i]][hr_list[[i]] == 0] <- NA
        hr <- sapply(1:12, function(x) colMeans(hr_list[[x]], na.rm = TRUE))
        hr <- sapply(1:12, function(x) mean(hr[21:25, x])) 
        hr[1] <- 0 # "unfished" scenario
    # combine all results to get relative errors
        depl_rel[j,] <- sapply(1:12, function(x) (depl[x]-depl_base[j])/depl_base[j])
        hcpue_rel[j,] <- sapply(1:12, function(x) (hcpue[x]-hcpue_base[j])/hcpue_base[j])
        hr_rel[j,] <- sapply(1:12, function(x) (hr[x]-hr_base[j])/hr_base[j])
    # combine all results to get abs values
        depl_abs[j,] <- sapply(1:12, function(x) depl[x])
        hcpue_abs[j,] <- sapply(1:12, function(x) hcpue[x])
        hr_abs[j,] <- sapply(1:12, function(x) hr[x])
  	}
  	depl_abs <- cbind(depl_base, depl_abs)
  	hcpue_abs <- cbind(hcpue_base, hcpue_abs)
  	hr_abs <- cbind(hr_base, hr_abs)
    colnames(depl_abs) <- NULL; colnames(hcpue_abs) <- NULL; colnames(hr_abs) <- NULL

  	bar_res <- list(depl_rel = depl_rel,
                    hcpue_rel = hcpue_rel,
                    hr_rel = hr_rel,
                    depl_abs = depl_abs,
                    hcpue_abs = hcpue_abs,
                    hr_abs = hr_abs)
    save(bar_res, file = "./Results_RData/semel_res_plots.RData")
    save(scen_str_base, scen_str_all, sens_str,
     file = "./Results_RData/semel_scenario_sensitivity_storage.RData")

# rm(list=setdiff(ls(), c("scen_str","sens_q_low_str","sens_q_high_str","sens_str")))
# gc()   

##################################
# sensitivity analysis plots
## SPR
    scen_str <- scen_str_all[[1]]
    SPR_base <- c(scen_str$base$results$eggs[1:24*12],sum(scen_str$base$results$eggs_mon[289:300]))/scen_str$base$results$eggs[12]
    eggs0 <- sapply(1:10, function(x) sens_str[[x]]$results$eggs[12])
    eggs_list <- NA
        eggs_list <- lapply(1:10, function(x) c(sens_str[[x]]$results$eggs[1:24*12],sum(sens_str[[x]]$results$eggs_mon[289:300]))) # no eggs calcs in last year in results$eggs so needed to calculate from eggs_mon
    SPR_list <- lapply(1:10, function(x) eggs_list[[x]]/eggs0[x])
## hcpue - average per open season
    hcpue_base <- matrix(scen_str$base$results$hcpue, nrow = 12, ncol = 25)
        hcpue_base[hcpue_base == 0] <- NA
        hcpue_base <- colMeans(hcpue_base, na.rm = TRUE)
    hcpue_list <- lapply(1:10, function(x) matrix(sens_str[[x]]$results$hcpue, nrow = 12, ncol = 25))
        for(i in 1:10) hcpue_list[[i]][hcpue_list[[i]] == 0] <- NA
        hcpue_list <- lapply(1:10, function(x) colMeans(hcpue_list[[x]], na.rm = TRUE))
## hr - average per open season
    hr_base <- matrix(scen_str$base$results$hr, nrow = 12, ncol = 25)
        hr_base[hr_base == 0] <- NA
        hr_base <- colMeans(hr_base, na.rm = TRUE)
    hr_list <- lapply(1:10, function(x) matrix(sens_str[[x]]$results$hr, nrow = 12, ncol = 25))
        for(i in 1:10) hr_list[[i]][hr_list[[i]] == 0] <- NA
        hr_list <- lapply(1:10, function(x) colMeans(hr_list[[x]], na.rm = TRUE))

## plots
plot_export <- TRUE
v <- seq(1,10,2)
y <- seq(2,10,2)
plot_names <- c("sensitivity in q", "sensitivity in M", "sensivity in vbk", "sensitivity in compensation ratio", expression("sensitivity in a"[mat]))
plot_export_names <- c("sens_q", "sens_M", "sens_vbk", "sens_CR", "sens_amat")

for(i in 1:5){
    if(plot_export == TRUE) tiff(filename=paste0("./Scenario_Figures/", plot_export_names[i],".tiff"), height = 17, width = 20, units = 'cm', compression = "lzw", res = 500)
    par(mfrow=c(3,1), mar=c(2,3.5,2,7.3), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
        plot(SPR_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Eggs/Unfished", xlab = "", main = plot_names[i])
            lines(SPR_list[[v[i]]], lty = 2, lwd = 2, col = "red")
            lines(SPR_list[[y[i]]], lty = 2, lwd = 2, col = "orange")
    par(mar=c(2,3.5,1,7.3), las=1, mgp=c(2.5,0.5,0), tck=-0.015, xpd = TRUE)
        plot(hcpue_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Catch per unit of effort", xlab = "")
            lines(hcpue_list[[v[i]]], lty = 2, lwd = 2, col = "red")
            lines(hcpue_list[[y[i]]], lty = 2, lwd = 2, col = "orange")
    legend("right", inset = c(-0.15,0), legend = c("base","decr. 25%", "incr. 25%"), box.lty = 0, col = c("black","red","orange"), lty = c(1,2,2))
        plot(hr_base, type = "l", ylim = c(0,1), lwd = 2, ylab = "Harvest rate", xlab = "")
            lines(hr_list[[v[i]]], lty = 2, lwd = 2, col = "red")
            lines(hr_list[[y[i]]], lty = 2, lwd = 2, col = "orange")
    if(plot_export == TRUE) dev.off()
}
