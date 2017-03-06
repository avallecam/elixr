
# from analysis/02-dtfilter.Rmd ----------------------------------------------------------------------------------------

# "2017-03-06 12:42:41 PET"

#### Data filtering
#
#**`r mue` is the total amount of evaluated samples**. 
#For each one, 02 read were made: 01 per specie. Before any filtering, 
#the total amount of reads goes up to **`r rep` including replicates**. 
#After filtering is applied, the **expected amount of left reads is `r esp`**.
#
#A **function** called `checkIN()` would allow to evaluate the retrieving of 
#replicated reads and addition of selected ones:

#CHECK FUNCTION
checkIN <- function(a) {
  evalREP <- NULL
  for (i in 1:6) {
    evalREP <- c(evalREP, sum(table(a$ID)==i))
  }
  evalREP
}