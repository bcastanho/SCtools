# Purpose: Initialisation tools for CRAN using `usethis`

# Readme
usethis::use_readme_rmd()

# PAckages Used
usethis::use_package("Hmisc")
usethis::use_package("ggplot2")
usethis::use_package("Synth")
usethis::use_package("stringr")

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
usethis::use_badge("maturing")
usethis::use_cran_badge()

# Use Vignettes

# Build site website
pkgdown::build_site()

# Licensing
usethis::use_gpl3_license(name = "Bruno Castanho Silva")

# CRAN Prep
usethis::use_cran_comments()
usethis::use_news_md()
