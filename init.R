# Purpose: Initialisation tools for CRAN using `usethis`

# Readme
usethis::use_readme_rmd()

# PAckages USed
usethis::use_package("Hmisc")
usethis::use_package("ggplot2")
usethis::use_package("Synth")


# Unit Testing
usethis::use_testthat()
usethis::use_test("generate.placebos.R")
usethis::use_test("mspe.plot.R")
usethis::use_test("mspe.test.R")
usethis::use_test("multiple.synth.R")
usethis::use_test("plac.dist.R")
usethis::use_test("plot.placebos.R")
usethis::use_test("pSCtools-internal.R")

# Continuous Integration and Code Coverage
usethis::use_travis()
usethis::use_coverage()
# Use Vignettes

# Licensing
usethis::use_gpl3_license(name = "Bruno Castanho Silva")
