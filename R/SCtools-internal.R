# Helper Tool for Generating Placebos

make_syn<-function(index,
							foo,
							predictors,
							predictors.op,
							dependent,
							unit.variable,
							time.variable,
							special.predictors,
							treated.units,
							control.units,
							time.predictors.prior,
							time.optimize.ssr,
							unit.names.variable,
							time.plot,
							Sigf.ipop){
	
	treated.units <- treated.units[index]
	
	dataprep.out<-Synth::dataprep(
		foo = foo,
		predictors = predictors,
		predictors.op = predictors.op,
		dependent = dependent,
		unit.variable = unit.variable,
		time.variable = time.variable,
		special.predictors = special.predictors,
		treatment.identifier = treated.units,
		controls.identifier = control.units,
		time.predictors.prior = time.predictors.prior,
		time.optimize.ssr = time.optimize.ssr,
		unit.names.variable = unit.names.variable,
		time.plot = time.plot)
	
	synth.out<-Synth::synth(data.prep.obj = dataprep.out )
	
	a<-data.frame(dataprep.out$Y0plot %*% synth.out$solution.w)
	
	colnames(a)<-paste('unit',index,sep='')
	
	out<-list(a = a, synth.out=synth.out, dataprep.out = dataprep.out)
	
	return(out)
}

# Function used in generate,placebos
# 
syn_plac <- function(i, 
										 dataprep.out, 
										 Sigf.ipop, 
										 tr, 
										 names.and.numbers) {
	cases <- as.numeric(dataprep.out$tag$controls.identifier)
	X0 <- dataprep.out$X0[, -i]
	X1 <- matrix(dataprep.out$X0[, i, drop = F])
	Z0 <- dataprep.out$Z0[, -i]
	Z1 <- matrix(dataprep.out$Z0[, i, drop = F])
	Y0plot <- dataprep.out$Y0plot[, -i]
	Y1plot <- dataprep.out$Y0plot[, i, drop = F]
	foo <- character(length = length(dataprep.out$tag$foo))
	id <- as.numeric(dataprep.out$tag$unit.variable)
	d <-
		which(regexpr(tr, stringr::str_split(dataprep.out$tag$foo[id],
																				 ', ')[[1]]) == T)
	for (j in 1:length(dataprep.out$tag$foo)) {
		foo[j] <-
			paste(stringr::str_split(dataprep.out$tag$foo[j],
															 ', ')[[1]][-c(d[1]:d[length(d)])], 
						collapse =', ')
	}
	predictors <- dataprep.out$tag$predictors
	predictors.op <- dataprep.out$tag$predictors.op
	special.predictors <- dataprep.out$tag$special.predictors
	dependent <- dataprep.out$tag$dependent
	unit.variable <- dataprep.out$tag$unit.variable
	time.variable <- dataprep.out$tag$time.variable
	treatment.identifier <- i
	controls.identifier <- dataprep.out$tag$controls.identifier[-i]
	time.predictors.prior <- dataprep.out$tag$time.predictors.prior
	time.optimize.ssr <- dataprep.out$tag$time.optimize.ssr
	time.plot <- dataprep.out$tag$time.plot
	unit.names.variable <- dataprep.out$tag$unit.names.variable
	tag <- list(
		foo = as.character(foo),
		predictors = predictors,
		predictors.op = predictors.op,
		special.predictors = special.predictors,
		dependent = dependent,
		unit.variable = unit.variable,
		time.variable = time.variable,
		treatment.identifier = treatment.identifier,
		controls.identifier = controls.identifier,
		time.predictors.prior = time.predictors.prior,
		time.optimize.ssr = time.optimize.ssr,
		time.plot = time.plot,
		unit.names.variable = unit.names.variable
	)
	dp <- list(
		X0 = X0,
		X1 = X1,
		Z0 = Z0,
		Z1 = Z1,
		Y0plot = Y0plot,
		Y1plot = Y1plot,
		names.and.numbers = names.and.numbers,
		tag = tag
	)
	s.out <- Synth::synth(data.prep.obj = dp, 
												Sigf.ipop = Sigf.ipop)
	a <- data.frame(dp$Y0plot %*% s.out$solution.w)
	s.mspe <- s.out$loss.v
	res <- list(a = a, s.mspe = s.mspe)
	return(res)
}

# Pattern Matching on T/F arguments

match_logical <- function(x){
	
	x <- as.character(x)
	
	out <- try(grepl(pattern = "T", x = x, ignore.case = T))
	
	if(inherits(out, "try-error")){
		stop("Please pass a logical (i.e. TRUE or FALSE)")
	}
	
	out
}

# Names of objects in `Synth`

synth_object_names <- c("solution.v",
												"solution.w",
												"loss.v",
												"loss.w",
												"custom.v",
												"rgV.optim")

dataprep_object_names <- c( "X0", 
														"X1", 
														"Z0",
														"Z1",
														"Y0plot",
														"Y1plot",
														"names.and.numbers",
														"tag" )




