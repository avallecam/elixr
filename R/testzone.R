rm(list = ls())

## TRIAL of CORRAN et al. eluted samples
x <- seq(0,2.5,.25)
newB <- function(r) { (r*max(x))/((max(x)/x)+r-1) }
plot(x,newB(0.5), xlab="serum.OD", ylab="predicted.OD", main="recovery= 0.5", bty="n")
segments(0,0,2.5,2.5, lty=2)

newA <- function(r,m) { (r*max(x))/((max(x)/m)+r-1) }
m <- seq(0,4,.25)
plot(m,newA(0.5,m), xlab="serum.OD", ylab="predicted.OD", main="recovery= 0.5", bty="n")
plot(x,newA(0.5,x), xlab="serum.OD", ylab="predicted.OD", main="recovery= 0.5", bty="n")

## DTA TEST
path <- system.file("examples", "iris.dta", package = "haven")
haven::read_dta(path)
tmp <- tempfile(fileext = ".dta")
haven::write_dta(mtcars, tmp)
haven::read_dta(tmp)


## Data dictionary to UPDATE labels

# data frame to named vector ----------------------------------------------------------
#http://stackoverflow.com/questions/19265172/converting-two-columns-of-a-data-frame-to-a-named-vector
dd = data.frame(crit = c("a","b","c","d"), 
                name = c("Alpha", "Beta", "Caesar", "Doris")
                )
haven::write_dta(dd, "data.dta") # WRITING WORKS
haven::read_dta("data.dta")        # timestamp ISSUE fixed!
haven::read_dta("data.dta") %>% haven::as_factor()
devtools::session_info()

whatiwant = c("a" = "Alpha",
              "b" = "Beta",
              "c" = "Caesar",
              "d" = "Doris")
## solution:
whatyouwant <- as.character(dd$name)
names(whatyouwant) <- dd$crit
#OR
whatyouwant <- setNames(as.character(dd$name), dd$crit)

## objetive:
Hmisc::contents(dd)
dd <- Hmisc::upData(dd, labels = whatyouwant)#, force.single = F
Hmisc::contents(dd)

# Assign variable labels of data frame columns ----------------------------------------------------------
# http://stackoverflow.com/questions/27347548/r-assign-variable-labels-of-data-frame-columns

data <- data.frame(age = c(21, 30, 25, 41, 29, 33), 
                   sex = factor(c(1, 2, 1, 2, 1, 2), 
                                labels = c("Female", "Male")))
var.labels <- c(age = "Age in Years", 
                sex = "Sex of the participant")
Hmisc::label(data) # no labels
dplyr::as.tbl(data) # as tibble
Hmisc::contents(data) # data dictionary
class(data$age); class(data$sex) # data class
haven::write_dta(data, "data.dta") # WRITING WORKS
haven::read_dta("data.dta")        # timestamp ISSUE fixed!
haven::read_dta("data.dta") %>% haven::as_factor() # WORKS PERFECT!

#(1) USING Hmisc::label ------------------------------------------
data <- data.frame(age = seq(21,27,2), 
                   sex = factor(rep(1:2,2), 
                                labels = c("Female", "Male")))
var.labels <- c(age = "Age in Years", 
                sex = "Sex of the participant")
Hmisc::label(data)
dplyr::as.tbl(data) # as tibble
#Hmisc::contents(data) # data dictionary
class(data$age); class(data$sex) # data class
haven::write_dta(data, "data.dta") # accepts numeric w/o label
haven::read_dta("data.dta")
haven::read_dta("data.dta") %>% haven::as_factor() # WORKS PERFECT!
# labels
Hmisc::label(data[["age"]]) <- "Age in years"
Hmisc::label(data[["sex"]]) <- "Sex of the participant"
#Hmisc::label(data)
dplyr::as.tbl(data) # as tibble
Hmisc::contents(data) # data dictionary
#attr(data[["age"]], "label")
#attr(data[["sex"]], "label")
#class(data$age); class(data$sex)
haven::write_dta(data, "data.dta") # ERROR: STATA requires labelled integers
haven::write_dta(data %>% 
                   mutate(age=as.integer(age)), "data.dta") # ERROR: after coerce as.integer ---> REPORT AN ISSUE
haven::write_dta(data %>% 
                   Hmisc::upData(labels = var.labels), "data.dta") #

#haven::read_dta("data.dta")
#haven::read_dta("data.dta") %>% haven::as_factor()

#(3) USING Hmisc::upData ------------------------------------------
data <- data.frame(age = c(21, 30, 25, 41, 29, 33), 
                   sex = factor(c(1, 2, 1, 2, 1, 2), 
                                labels = c("Female", "Male")))
var.labels <- c(age = "Age in Years", 
                sex = "Sex of the participant")
Hmisc::label(data)
dplyr::as.tbl(data) # as tibble
Hmisc::contents(data) # data dictionary
class(data$age); class(data$sex) # data class
haven::write_dta(data, "data.dta") # WORKS with NO LABELS
haven::read_dta("data.dta")
haven::read_dta("data.dta") %>% haven::as_factor() # WORKS PERFECT!
# labels
data <- Hmisc::upData(data,
                      labels = var.labels)# generates INTEGERS (required for STATA)
Hmisc::label(data)
dplyr::as.tbl(data) # as tibble
Hmisc::contents(data) # data dictionary
#attr(data[["age"]], "label")
#attr(data[["sex"]], "label")
class(data$age); class(data$sex)
haven::write_dta(data, "data.dta") # ERROR after labels! -> DIFFERENT ERROR MESSAGES
#haven::read_dta("data.dta")
#haven::read_dta("data.dta") %>% haven::as_factor()

devtools::session_info()

#(2) USING the APPLY FUNCTION [not working] ------------------------------------------
Hmisc::label(data)
Hmisc::label(data) <- lapply(names(var.labels), 
                             function(x) label(data[,x]) <- var.labels[x])
Hmisc::label(data)