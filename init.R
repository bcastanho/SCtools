# Purpose: Initialisation tools for CRAN using `usethis`

# Readme
usethis::use_readme_rmd()

# Also
usethis::use_tidy_issue_template()

# PAckages Used
usethis::use_package("cvTools")
usethis::use_package("ggplot2")
usethis::use_package("Synth")
usethis::use_package("stringr")
usethis::use_package("stats")

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
usethis::use_code_of_conduct()

# NEws!
usethis::use_news_md()

# Use Vignettes
usethis::use_vignette(name = "replicating-basque", "Replicating the Basque Study")

# Some fun raw data?

usethis::use_data_raw(name = "alcohol")

# Build site website
usethis::use_pkgdown()
pkgdown::build_site()

# Licensing
usethis::use_gpl3_license(name = "Bruno Castanho Silva")

# CRAN Prep
usethis::use_cran_comments()
usethis::use_news_md()
