rm(list = ls())

library(knitr)
library(rmarkdown)

#set root dir
opts_knit$set(root.dir='../.')

#render
#render("analysis/01-standardization.Rmd", clean=TRUE, output_dir= "report")
render("analysis/02-data-filtering.Rmd", clean=TRUE, output_dir= "report")
render("analysis/03-covariates.Rmd", clean=TRUE, output_dir= "report")
render("analysis/04-appendix.Rmd", clean=TRUE, output_dir= "report")
