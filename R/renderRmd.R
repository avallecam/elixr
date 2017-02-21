rm(list = ls())

#library(knitr)
#set root dir
knitr::opts_knit$set(root.dir='../.')

#library(rmarkdown)
#render
#rmarkdown::render("analysis/01-standardization.Rmd", clean=TRUE)
rmarkdown::render("analysis/02-data-filtering.Rmd", clean=TRUE)
rmarkdown::render("analysis/03-covariates.Rmd", clean=TRUE)
#rmarkdown::render("analysis/03-epidemiology.Rmd", clean=TRUE)
#rmarkdown::render("analysis/04-appendix.Rmd", clean=TRUE)

#move file directory path using file.rename
#move all .png to figure/
af <- list.files(path = "./analysis", pattern = "*.png", full.names = TRUE)
an <- list.files(path = "./analysis", pattern = "*.png", full.names = FALSE)
b <- paste0("./figure/",an)
file.rename(af,b)
#move all .html to report/
cf <- list.files(path = "./analysis", pattern = "*.html", full.names = TRUE)
cn <- list.files(path = "./analysis", pattern = "*.html", full.names = FALSE)
d <- paste0("./report/",cn)
file.rename(cf,d)
