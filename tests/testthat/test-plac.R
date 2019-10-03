test_that("Placebo Distribution Returns Desired Outputs", {
	skip_on_cran()
	library(Synth)
	data(synth.data)
	set.seed(42)
	
	multi <- multiple.synth(foo = synth.data,
													predictors = c("X1", "X2", "X3"),
													predictors.op = "mean",
													dependent = "Y",
													unit.variable = "unit.num",
													time.variable = "year",
													treatment.time = 1983,
													special.predictors = list(
														list("Y", 1991, "mean"),
														list("Y", 1985, "mean"),
														list("Y", 1980, "mean")
													),
													treated.units = 7,
													control.units = c(29, 2, 13, 17, 32, 38),
													time.predictors.prior = c(1984:1989),
													time.optimize.ssr = c(1984:1990),
													unit.names.variable = "name",
													time.plot = 1984:1996, gen.placebos = TRUE, Sigf.ipop = 3 )
	
	output <- plac.dist(multi, nboots = 30)
	
	expect_true("ggplot" %in% class(output[["p"]]))
})
