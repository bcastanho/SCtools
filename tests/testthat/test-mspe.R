test_that("Correct Object passed to test-mspe", {
	skip_on_cran()
	## First prepare the required objects
	library(Synth)
	# Load simulated data from Synth
	data(synth.data)
	set.seed(42)

	# Execute dataprep to produce the necessary matrices for synth
	dataprep.out<-
	  dataprep(
	    foo = synth.data,
	    predictors = c("X1", "X2", "X3"),
	    predictors.op = "mean",
	    dependent = "Y",
	    unit.variable = "unit.num",
	    time.variable = "year",
	    special.predictors = list(
	      list("Y", 1991, "mean"),
	      list("Y", 1985, "mean"),
	      list("Y", 1980, "mean")
	    ),
	    treatment.identifier = 7,
	    controls.identifier = c(29, 2, 13, 17, 32, 38),
	    time.predictors.prior = c(1984:1989),
	    time.optimize.ssr = c(1984:1990),
	    unit.names.variable = "name",
	    time.plot = 1984:1996
	  )

	# run the synth command to create the synthetic control
	synth.out <- synth(dataprep.out)

	## run the generate.placebos command to reassign treatment status
	## to each unit listed as control, one at a time, and generate their
	## synthetic versions.
	tdf<-generate.placebos(dataprep.out, synth.out, Sigf.ipop = 3)

	## Test how extreme was the observed treatment effect given the placebos:
	ratio <- mspe.test(tdf)
	
	expect_true(abs(ratio$p.val - 0.1428571) <  .001)
})
