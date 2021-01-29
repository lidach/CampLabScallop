source("scallop_model_fun.R")

fun <- scallop_model_fun(FM_flag = 1,
							effort=2,
							q=0.0003,
							U = 0,
							bag = 115,
							months = 120,
							prop_spawn = c(0,0,0,0,0,0.1,0.2,0.5,0.8,0.8,0.2,0.1), 
	     					prop_2spawn = c(0.3,0.5,1,0,0,0,0,0,0,0,0,0))