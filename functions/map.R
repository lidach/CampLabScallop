#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#			FL Bay Scallops regulation map
#		
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

###-----------------------------------------------------
#		Data Read In
###-----------------------------------------------------
	#-------
	# regs
	regs <- read.csv(paste0(path,'regs.csv'), as.is=TRUE)

	#-------
	# maps
		
	load(paste0(path,"fl_waters.Rdata"), verbose=T)
	fl_waters <- st_as_sf(fl_waters)
	fl_cnt <- counties("Florida", cb=TRUE)
	fl_cnt_sub <- fl_cnt[fl_cnt$NAME %in% c("Bay","Gulf", "Franklin", "Wakulla", "Taylor", "Dixie", "Levy", "Citrus", "Hernando", "Pasco", "Jefferson"),]
	bbox <- st_bbox(fl_cnt_sub)

	fl_state <- st_union(fl_cnt)
###-----------------------------------------------------
#		Slice on lines
###-----------------------------------------------------
	x <- as.data.frame(st_coordinates(fl_cnt[fl_cnt$NAME=='Gulf',]))
	x$abs <- apply(x,1,function(x) sum(abs(x[1:2])))
	x$Yq <- x$Y<quantile(x$Y,0.1)
	gulf.border <- x[which(x$Yq)[which.min(x$abs[which(x$Yq)])],c('X','Y')]

	# x <- as.data.frame(st_coordinates(fl_cnt[fl_cnt$NAME=='Taylor',]))
	# x$abs <- apply(x,1,function(x) sum(abs(x[1:2])))
	# x$Xq <- (x$X < -83-49.790/60 + 0.01) & (x$X > -83-49.790/60 - 0.01)
	# tay.border <- x[which(x$Xq)[which.min(x$Y[which(x$Xq)])],c('X','Y')]

	cut.pts <- c(-85.43049253363635, #MEX beach canal
	             gulf.border[[1]], #easternmost Gulf County on shore
	             -83-49.790/60, #Fenholloway
	             29+15.350/60, #Levy
	             28+26.016/60, #north Pasco
	             28+10.020/60) #south Pasco
	modes <- c('long','long','long','lat','lat','lat')

	coord.adj <- function(coord, bbox, mode='long'){
		if(mode=='long'){
			x <- cbind(c(coord,coord),bbox[c(2,4)])
		}else{
			x <- cbind(c(coord,coord),bbox[c(1,3)])
			x <- x[,c(2,1)]
		}
		
		return(x)
	}

	line.coords <- lapply(1:length(cut.pts), function(x) coord.adj(coord=cut.pts[x],bbox=st_bbox(fl_cnt_sub), mode=modes[x]))

	lines <- st_as_sfc(lapply(line.coords,st_linestring),
	          crs=st_crs(fl_cnt_sub))

	#split by lines
	fl_waters_sp <- st_split(fl_waters,lines) %>%
			st_collection_extract(c("POLYGON"))
	fl_waters_sp$zone <- 1:nrow(fl_waters_sp)

	fl_szone <- fl_waters_sp[st_intersects(fl_waters_sp,
	                                   st_as_sfc(st_bbox(c(xmin=cut.pts[1],
	                                               ymin=cut.pts[6],
	                                               xmax=as.numeric(bbox)[3],
	                                               ymax=as.numeric(bbox)[4]), 
	                                   crs=st_crs(fl_cnt_sub))),
	                                   sparse=FALSE),]

###-----------------------------------------------------
#		tidy up regs
###-----------------------------------------------------
	regs2 <- regs
	regs2$Zone <- LETTERS[c(1,2,3,3,4,5)]
	regs2 <- regs2[,c(5,4,2,3,1)]
	colnames(regs2) <- c("Vessel","Bag Limit",
	                     "Opened","Closed","Zone")

lab.pts <- matrix(c(-85.3350217475323,-84.2275513443413,-83.4895975831593,-82.8204294431009,-82.812517913473,29.5899887516388,29.9910711525729,29.6058655170471,28.9222177076238,28.3204590027809),ncol=2)
cnt.pts <- matrix(c(-85.3066933469651,-84.9436153004211,-83.8111282851377,-83.3323245101072,-82.9681812253583,-82.6246069114289,-82.5803504281209,-82.6141351086828,29.9037549239691,29.9159512137199,30.0291442876147,29.5481979264004,29.2671844874254,28.8009653395908,28.5134713137594,28.2640301730905), ncol=2)
