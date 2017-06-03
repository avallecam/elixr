# PROJECT: ELISA standardization
Andree Valle Campos  
`r Sys.Date()`  




<!--html_preserve--><style>div.main-container {max-width:4000px;}</style><!--/html_preserve-->

***

# Project notebook {-}

This is a project notebook using `Rmarkdown` [@rmarkdown], `knitr` [@knitr], 
`Hmisc` [@Hmisc] and `bookdown` [@bookdown] packages. 
This *dynamic document* format integrates **text**, **code** and **results** 
[@CienciaReproducible2016].

***

## Project Objectives {-}

* __Main:__ 
    - __Standardize__ the OD~450nm~ values across ELISA plates 
    by the estimation of Antibody Units of each unknown sample.
    - __Unificate__ and __Update__ the epidemiological dataset.
    - Generate a __QC report__ of UNAP vs NAMRU measurements. [here](#qc-report)

* __Secondary:__
    - Implement a [reproducible workflow](https://www.ncbi.nlm.nih.gov/pubmed/26776185) 
    for standardization of ELISA plates from a template matrix and subsequent analysis.

***

## Last update {-}

- **[Data Dictionary](#data-dictionary)** available for __epidemiology+serology__ covariates.
- Database of __epidemiology+serology__ covariates in `.dta` format __without labels__. [issue submited](#write-dta)
- `slopegraph` showing the changes of `age` along [updates](#slope). 
- All results at the beginning + __hiperlinks__ of sections __3, 4, 5__.

## Urgent To do's {-}

- CHECK IF **ZG182-1** should be corrected to **ZG181-1**.
    + Missing reads are complementary between ID's.
    + details in [Appendix](#appendix-residual-na) **NOTE 2**
    
- Assess the **inter-plate-CV** comparing STD variability across plates to generate a 
**Limit of Quantification**
    + Actual CV w.r.t OD is the **intra-plate-CV**
    + Generate a tibble of all STD dilutions OD's
    + Extend `std.par` output to all parameters
    + Include other inter-plate measurements:
        + blank, positive and negative controls

***

<!--chapter:end:index.Rmd-->

# INTRODUCTION {#intrap}

## Aim

* Principal: 
    - Review the methods previously applied to the standarization across ELISA plates 
    in vaccine or serological surveillance research.

* Secondary: 
    - Register project updates, current problems to solve, project organization 
    and benefits of using Git.

## To Do

## Dependencies

No extra package required.


```r
##essential
library(XLConnect)    # load EXCEL workbooks
library(drc)          # Dose-Response modeling
##accesory
library("DiagrammeR") # method Flowchart
library("knitr")      # To display nice tables
library("tidyverse")
#ggplot2, tibble, tidyr, readr, purr, dplyyr
##extra
library("Rmisc")      # multiplot ggplots
library("haven")      # import STATA df
library("readxl")     # import EXCEL df
library("forcats")    # factor vectors
library("viridis")    # color visualization
```

## Input

```r
Pf.ID <- readRDS("data/01-dilution.rds")
#unlist(Pf.ID[1:12,])
```


## APPENDIX: From OD to Ab units

- Sepulveda et.al. [@Seplveda2015] cite two main publications:
    1. **Cook et.al., 2011 in Bioko Island, Equatorial Guinea** [@Cook2011], and
    2. **Cunha et.al., 2014 in Jacareacanga, Brazil** [@Cunha2014].
- Both papers followed the same experimental procedure to standardize the data 
across plates. 
Althought, non of them cite **Miura et.al., 2008**[@Miura2008].
- Nuno Sepulveda is co-author of the second paper [@Cunha2014].


### NAMRU-6 method


```r
t<-seq(1,12,1)
x <- c()
x0 <- 50
f <- function(x){x*2}
x[1]<-x0
for (i in 1:(length(t)-1)) {
  x[i+1] = f(x[i])
}
```


The method provided by Julio followed this procedure, making two main assumptions:

1. An **endpoint titer** equal to the last dilution of the STD, in this case 102400.
2. An equal **endpoint titer** among *P.falciparum* and *P.vivax* STD's.


```r
example <- cbind(unlist(Pf.ID[1:12,]), 
                 x, 
                 gsub('STD 1','102400',unlist(Pf.ID[1:12,])),
                 x[length(x)]/x)
colnames(example) <- c("Dilution", "Reciprocal of Dilution", "Factor", "ELISA Ab units")
kable(example)
```



Dilution       Reciprocal of Dilution   Factor          ELISA Ab units 
-------------  -----------------------  --------------  ---------------
STD 1/50       50                       102400/50       2048           
STD 1/100      100                      102400/100      1024           
STD 1/200      200                      102400/200      512            
STD 1/400      400                      102400/400      256            
STD 1/800      800                      102400/800      128            
STD 1/1600     1600                     102400/1600     64             
STD 1/3200     3200                     102400/3200     32             
STD 1/6400     6400                     102400/6400     16             
STD 1/12800    12800                    102400/12800    8              
STD 1/25600    25600                    102400/25600    4              
STD 1/51200    51200                    102400/51200    2              
STD 1/102400   102400                   102400/102400   1              


### K.Miura method

K.Miura et.al. [@Miura2008] previously assigned Antibody Units to the STD 
(as the reciprocal dilution giving an OD~450nm~=1*). After this step, the STD 
was applied to each ELISA plate.

  (@) For example, for the **Anti-Pvs25 mokey sera**, after generating aliquotes 
  of 1:100 from the initial pool, four-fold serial dilution was perfomed. Here, 
  an OD~450nm~=1 had a Rec.Dilution of 20,000. Then STD Ab units was equal to 20,000. 

  (@) After the assignation of Ab units to the STD, a two-fold serial dilution starting 
  with a dilution of 20 Ab units, equivalent to 1:1000 dilution, was applied on each 
  ELISA plate.

  (@) As an example, in the paper is said that the 5th dilution of the STD sera 
  is suppoused to have 1.25 Ab units. 
  So, if $1.25=\frac{STD}{16000}$ then the STD Ab units are equal to 20,000.

*The reason behind an OD~450nm~=1 may be in the variability of the STD curve Upper 
and Lower limits. This behaviour also is observed in the comparoson of the STD OD 
values across ELISA plates.

### Corran method

Cook et.al.[@Cook2011] cite Corran et.al. [@Corran2008] which have a full description 
of the methods.

Corran et.al.[@Corran2008] make the assumption of an undiluted (initial) concentration 
of the sera pool previous to a 3pLL model fitting. By this way, the OD is directly 
proportional to the independent variable. 
[suppl.file 1](https://static-content.springer.com/esm/art%3A10.1186%2F1475-2875-7-195/MediaObjects/12936_2008_670_MOESM1_ESM.pdf)

At Methods, they write the following:

> "A titration curve was fitted to the ODs obtained for the standard plasma dilutions 
> by least squares minimisation using a three variable sigmoid model and the solver 
> add-in in Excel (Microsoft), assuming an arbitrary value of **1000 Units/ml** of 
> antibody against each antigen in the **standard pool**. OD values for the spot 
> extracts were converted to units/ml using this fitted curve."

***

## APPENDIX: the 4pLL model

The **four-parameter log-logistic function** have the following presentations [@weimer2012impact]: 
$$f(x)=f(x;b,c,d,e)=c+\frac{d-c}{1+\exp[b(log(x)-log(e))]}=c+\frac{d-c}{1+\left(\frac{x}{e}\right)^b}=c+\frac{d-c}{1+{10}^{b(log_{10}(x)-log_{10}(e))}}$$

By isolating $x$, equivalent expressions are commonly reported as inverse functions 
of the 4pLL equation, assuming `b=-1` and then `c=0`:
$$x=e\left[{\left(\frac{d-f(x)}{f(x)-c}\right)}^{1/b}\right] \qquad \Rightarrow \qquad x=\frac{e}{\frac{d-c}{f(x)-c}-1} \qquad  \Rightarrow \qquad x=\frac{e}{\frac{d}{f(x)}-1}$$

If no log-transformation is required, then the following expression are often employed, 
depending on the scale of `x`: 
$$f(x)=c+\frac{d-c}{1+\exp[b(x-e)]} \qquad or \qquad f(x)=c+\frac{d-c}{1+{10}^{b(x-e)}}$$ 
For example, this last expression, with `x` in decimal-log scale, is present in the 
page 7 of the *drLumi* [@drLumi] package vignette.

Lastly, when using the nls function to fit the data, 
[Self-Starter](https://socserv.socsci.mcmaster.ca/jfox/Books/Companion/appendix/Appendix-Nonlinear-Regression.pdf) functions are required. 
This functions, in contrast with the last example, requires `x` in natural-log scale. 
E.g. the logistic model have the following parameters 
$$f(x)=\frac{\phi_1}{1+\exp\left[ \frac{\phi_2-x}{\phi_3}\right]} \qquad \Rightarrow \qquad f(x)=0+\frac{\phi_1-0}{1+\exp\left[ \frac{-1}{\phi_3} (x-\phi_2) \right]} $$

### Publications

  1. **Miura et.al, 2008** [@Miura2008] reported a "four-parameter hyperbolic curve" in this form: 
  $$x=e\left[{\left(\frac{d-f(x)}{f(x)-c}\right)}^{1/b}\right]$$ 
  where $x={Ab}_{units}$ and $f(x)={OD}_{450nm}$ without parameter letters definition. 
  Those parameter letters have been changed in order to keep concordance with the 
  extended description gave [above](#method).

  1. **Cook et.al., 2011 in Bioko Island, Equatorial Guinea** [@Cook2011] cite 
  Corran et.al. [@Corran2008] which have a full description of the methods 
  [suppl.file 1](https://static-content.springer.com/esm/art%3A10.1186%2F1475-2875-7-195/MediaObjects/12936_2008_670_MOESM1_ESM.pdf). 
  Briefly, they report a ligand-binding equation, with the following inverse function: 
  $$A=\frac{A_{max}*B}{B+k} \qquad \Rightarrow \qquad B=\frac{C}{D}=\frac{k}{\frac{A_{max}}{A}-1}$$ 
  That equation takes a recognizable form after a rearrangement, showing a 4pLL assuming `c=0` and `b=-1`, the same as with the inverse form: 
  $$A=0+\frac{A_{max}-0}{1+\left(\frac{B}{k}\right)^{-1}} \equiv f(x)=0+\frac{d-0}{1+\left(\frac{x}{e}\right)^b}  \qquad and \qquad B=\frac{k}{\frac{A_{max}-0}{A-0}-1} \equiv x=\frac{e}{\frac{d-0}{f(x)-0}-1} $$
    
  2. **Cunha & Sepulveda et.al., 2014 in Jacareacanga, Brazil** [@Cunha2014] report 
  a formula without any details, as in the cited publication [@Bousema2010]: 
  $$ {titer}= \frac{dilution}{ \frac{maximum.OD}{OD.test.serum - minimum.OD} -1} \qquad \neq \qquad x=\frac{e}{\frac{d-c}{f(x)-c}-1} $$ 
  Eventually, they tried to write the inverse of the 4pLL model but fail in the 
  description of the variables.
  

**CONCLUSION:**

Here I have shortly compare the equivalent expressions applied by several reference 
publications [@Miura2008; @Cook2011; @Cunha2014; @Corran2008], all equal to the 
**four-parameter log-logistic function** applied in this report.

***


## APPENDIX: Updates

- **From 09dic2016 to 01feb2017**

*09dic2016:*

With the Antibody Units:

- Conversion of the Estimated Reciprocal Dilutions to *Arbitrary Ab units*.
    + *SOL:* Followed NAMRU-6 SOP for ELISA data analysis.
    + A complementary description is in the [Appendix](#appendix-from-od-to-ab-units)

- Complete understanding of Cook et.al. [@Cook2011] and Cunha et.al. [@Cunha2014] or 
Cook et.al. [@Cook2011] methodology.
    + *SOL:* A complementary description is in the [Appendix](#appendix-the-4pll-model).

- Blank wells were added as STD with Ab units equal to zero. Then, a 4pLL was the 
best model to fit STD data.
    + *NOTE:* This allows to avoid background subtraction.
    
- Positive Control added to graph and Dose estimation
    + *NOTE:* ctrl+ out of STD range should be discussed.
    
*16dic2016:*

- A comparison between both states of Background Subtraction [ON] vs [OFF].
    + *SOL:* An [OFF] state after a Box-Cox transformation results in lower amount 
    of NaN's among the first 5 plates.

- Addition of residual variance and QQ-plots before and after Box-Cox transformation.

- Histogram, Density plots and QQ-plots for OD vs Ab.unit comparison.
    + *NOTE:* No dramatic changes after Background Subtraction.

*23dic2016:*

- 2 main outputs:
    + 01 SIMPLE I/O report for all ELISA templates (w/o Background subtraction)
    + 01 EXTENDED report per ELISA template available if required

- Flowchart of methodology

- 04 Templates with lower samples than expected:
    + N12 (n=39), N18 (n=37), N23 (n=10), N41 (n=18).

- 01 Template with more samples than expected:
    + N15 (n=41)
        - **ZG181-1** sampled against **Pviv** plate only
        - **ZG182-1** sampled against **Pfal** plate only.
    + Both generated NaNs at the end. Needs to be solved.

- Some XLS file modifications:
    + from N20 to N37 and from N39 to N41, 03 extra rows after "Plate 1 QTSR Pf" 
    were deleted.
    + N39 typo at cell M36 corrected.
    + N18 typo at cell J30 corrected.
    + from N23 to N26, STD cell format changed from 'character' to 'number'.
    + N41 blank and ctr+/- were moved to the right position.
    
*30dic2016:*

- Unbiased selection of Ab.units estimate among replicated samples:
    + *NOTE:* This assumes that no dilution to the original evaluated sample has 
    been performed.
  
- About **73 NaN** Ab.units, since only one is above its model Upper Limit, I suggest 
to avoid all this measurement.
    + Requires discussion!
    
*06ene2017:*

- Improvement of the unbiased selection criteria:
    + For triplicates, the closest value to the mean among replicates was selected
    + Reads with mean.OD with a %CV higher than 20% were rejected
    + For duplicates, the replicate with lower %CV was preferred.
    + If a selection was not acomplished, the last measurement was preferred.
    + Criteria was implemented in a function called `Unbiased()`

- Ab.units data frame filtering:
    + Retrieve of all replicated reads: triplicates, duplicates and NaN.
    + Addition of selected reads among triplicates and duplicates.
    + Visualization of mean.OD-Ab.unit non-linearity and whole %CV before and 
    after the procedure

- Brefly: 
    + 964 samples are **in** with 02 reads each, 01 per specie
    + 49 samples are **in** with only 01 read each
    + 03 samples are **out** because of NaN on both reads

- From the resting 55 reads with unestimated Ab.unit:
    + Only 03 reads have a mean.OD **above** their respective model upper limit
        + a re-run may be discussed.
    + All the rest, have a mean.OD **below** the lower limit
        + These reads aproaches to zero.

- Done with 02 main outputs:
    + a SIMPLE **.html** report to share with collaborators.
    + a FINAL **.csv** with the obtained Ab.units with the 
    *Zungarococha_database1_3Oct2016.csv* file format
        + Better suited for **covariates** addition.

- For a comparison between resting NaN's mean.OD and Upper/Lower limits per ELISA plate:
    + Complete list of NaN's available in 
    [02-dtfilter.html](#filter)
    + Complete list of Upper/Lower limits per ELISA plate in 
    [01-standard.html](#stdrad)

- A complete list of previous updates available in [Appendix](#appendix-updates)

*13ene2017:*

- **To discuss:** Criteria for NaN:
    + If approaching zero, then change NaN to 0? (n=46+6)
    + If aproaching infinity, then dilute and re-run? (n=3)
        + Review Corran, et al. criteria.
    
- The original covariate DB is required to fill then into the final DB
    + `Zungarococha_database1_30oct2016` have only **960 samples**
    + There is no data for **09 samples**, and
    + There are **02 samples** that are not present in the 28 evaluated Templates.

*20ene2017:*

- After Katty indication, DB typos were corrected in the current script at 
[02-dtfilter.html](#filter):
    + 2 samples were added: ZG009-3 and NN068-2.

- Complete list of replicates before and after the unbiased selection available at 
[02-dtfilter.html](#filter)

- Covariate DB was added to the analysis at 
[03-covariat.html](#covar):
    + Updated covariate DB was merged with the Ab.units per sample:
        + CSV file is available to re-analyse seropositivity in STATA.
    
    + Contrast of covariates and Ab.units between patients with doble ID?
        + Based on the current data, there is not enough support to delete those samples.

    + A comparison between covariate DB PRE and POST update is also available
        + Frequencies and age distribution per community. 

- **To discuss:** Criteria for NaN:
    + If approaching zero, then change NaN to 0? (n=46+6)
    + If aproaching infinity, then dilute and re-run? (n=3)

*01feb2017:*

- **To discuss:** Criteria for NaN:
    + If approaching zero, then change NaN to 0? (n=46+6)
    + If aproaching infinity, then dilute and re-run? (n=3)

- Covariate [labels](#new-db-stats) could be changes if required.

- Covariates visualization of the 03 samples requiring a [re-run](#re-run).

- Project folder arrangement ready to share via *Git(Hub)*[@CienciaReproducible2016].

**NEXT STEPS:**

- Writing of methodology and its limitations for a potential publication.

- Replicate seropositive analysis.
    + Check if "NaN" data cells creates dificulties. (easy to correct)
    + Contrast mean.OD and standardized Ab.units serpositivity patters per community.

- Share code via [Git(Hub)](https://speakerdeck.com/alicebartlett/git-for-humans) 
with Nuno.
    + BENEFITS:
        + Will improve collaboration:
            + code review, version control, error tracking and paper writing.
        + Will improve publication process: 
            + journal peer-review process, preference on 
            [plain-text manustripts](https://github.com/rstudio/rticles) 
            with code available.
        + Will improve publication quality and impact:
            + sharing a reproducible workflow acelerates scientific progress
                + Remember that standardized open-access procedures are still a 
                deficit in this field.
                + Recent "open-acces" publications by Biggs [@biggs2017] and 
                Dewasurendra [@dewasurendra2017effectiveness] *do not* provide 
                open-acces methods 
                + only in the last [@dewasurendra2017effectiveness] specifies that 
                "datasets and scripts are available under reasonable request".
            + This type of reports, relevant for malaria surveillance 
            [@elliott2014surveillance], would be highly improved with standardize 
            open-acess lab/comp protocols.
        + Will ease the transition from the Rproject to a Rpackage aim to analyse 
        this specific data.
            + Nuno have already made most of the require methods [@Seplveda2015] 
            and simulations [@sepulveda2015sample] with open-access tools.

**PROBLEMS TO BE SOLVED:**

- *MUST:* Improve code
    + Replace SDCV by an extended MEAN data.frame.
    + This will solve N15 NaN problem



## APPENDIX: Version Control

- **From 30jan2017 to 06mar2017**

- Some link of interest
    + [Git for humans](https://speakerdeck.com/alicebartlett/git-for-humans)
    + [Git and Github with Rstudio](http://r-pkgs.had.co.nz/git.html)

### Project directory

- Following Reproducible Research workflow 
[procedures](http://blog.jom.link/implementation_basic_reproductible_workflow.html)


```bash
less notrack/project_elixr.md
```

```
## .
## ├── analysis/								# all the .Rmd files in plane-text (under Git)
## │   ├── 00-appendix.Rmd
## │   ├── 01-standard.Rmd
## │   ├── 02-dtfilter.Rmd
## │   ├── 03-covariat.Rmd
## │   ├── 04-epidemio.Rmd
## │   ├── american-medical-association.csl 	# also .csl and .bib with bibliography and format
## │   └── SeroSurveillance.bib
## ├── data/									# all the generated .rds and .csv files and
## │   ├── 01-dilution.rds
## │   ├── 01-parametr.rds
## │   ├── 01-standard-raw.rds
## │   ├── 02-dtfilter-nty.rds
## │   ├── 02-dtfilter-uty.rds
## │   ├── 03-covariat-nty.rds
## │   ├── 03-covariat-uap.rds
## │   ├── 03-covariat-uty.rds
## │   ├── 04-epidemio-core.csv
## │   ├── 04-epidemio-core.rds
## │   ├── 04-epidemio-full.csv
## │   ├── 04-epidemio-full.rds
## │   ├── UNAP-1-DB-20sep2016.dta 				# also updated data-raw/ files
## │   ├── UNAP-2-DB-03oct2016.csv
## │   ├── UNAP-3-DB-02nov2016.dta
## │   ├── UNAP-4-UP-16ene2017.xlsx
## │   ├── UNAP-5-QC-08feb2017.xlsx
## │   └── UNAP-6-EPI-08feb2017.xlsx
## ├── data-raw/								# here are the Template_ELISA_N##.xlsx files
## ├── figure/
## ├── makefile/
## ├── man/
## ├── notrack/
## ├── R/
## │   └── renderRmd.R 							# a R script that renders all the .Rmd files.
## ├── README.md
## ├── report/									# all the automatically generated .html report/ files
## │   ├── 00-appendix.html
## │   ├── 01-standard.html
## │   ├── 02-dtfilter.html
## │   ├── 03-covariat.html
## │   └── 04-epidemio.html
## └── SeroSurveillance.Rproj
```

### Git version control

- For a graphical introduction to Git within Rstudio, check this 
[link](http://r-pkgs.had.co.nz/git.html).


```bash
git log --oneline
```

```
## 0987b97 Correct titer@appendix, residence@epidemio. Add cpenviro
## bf5276a Hyperlink results. Label micro/pcr. Issue haven
## b9aa2cf Update to elixr Rproj
## 6be1201 Update combustible-cocinar + STATA v13 dta
## 1a66077 Implement bookdown and child reports
## 7ea9ceb Add .dta output in 04-epidemio + 00-complete file
## 19e2c94 Update data instructions in 04-epidemio.Rmd
## 4c131aa Add load data instructions in 04-epidemio.Rmd
## 3323c4d Apply line breakes to ease Git
## 99b3189 Add slopegraph of age updates
## eb3bba8 Clean report organization. Explicit results
## 8cbd908 Add concensus in micro and pcr. One unique dataset
## a2dd6a6 Update objectives per Rmd. Add Git info in Appendix
## 0774fb7 Rename analysis files
## bdfa60b Change output names
## 42a4a55 Complete 04-epidemiology. Update TO DO in others
## 8bd66d3 Tide procedures in data-filtering and covariate
## fc1a6a3 Tidy standardization output, std.uty generation, and epidemiology
## b0b537e Add testzone.R script
## 48dd74b Done with unap1-5 covariates
## 81557a8 Tidy covariates using Tidyverse. New EpiDB
## 746af39 Update renderRmd and no NA retrieving
## 26f125a Connect .Rmd by .Rds and render Rscript
## a04bc1a Split .Rmd files after Merge covariates
## 536c0e4 updated README
## 46d1f7b Add git+bib benefits, Add re-run covariates, No ggplot histogram
## 3f20a88 Add .nb.html to gitignore
## 568ab5e SeroMarkerMir.Rproj deleted
## d62bebe New Rproj name
## b913e7e README update
## f62a12e Added .csl to gitignore. First commit of working files
## d356326 First commit: gitignore file
```

- For a restricted file tracking, e.g. avoiding large files like datasets, 
add their names to a `.gitignore` file

```bash
less .gitignore
```

```
## .Rproj.user
## .Rhistory
## analysis/*.csl
## analysis/trialerror/*
## analysis/*Hmisc*
## data/*
## data-raw/*
## figure/*
## notrack/*
## report/*
## *manuscript*
## *.pdf
## _book/*
## _bookdown_files/*
## *.html
```


### Git commits in detail


```bash
git log
```

```
## commit 0987b9775e7f5eddfe7990e7719da85f072e9253
## Author: avallecam <avallecam@gmail.com>
## Date:   Wed May 31 00:59:50 2017 -0500
## 
##     Correct titer@appendix, residence@epidemio. Add cpenviro
## 
## commit bf5276a65f52e092adb3b0da5b9c3017448cb977
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Apr 11 12:36:55 2017 -0500
## 
##     Hyperlink results. Label micro/pcr. Issue haven
## 
## commit b9aa2cf2c21ef6e5ac8f342f9001deb6207da818
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Apr 10 19:38:27 2017 -0500
## 
##     Update to elixr Rproj
## 
## commit 6be1201a4e9be409647257db8a0a4efaa523e90d
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Apr 10 19:27:18 2017 -0500
## 
##     Update combustible-cocinar + STATA v13 dta
## 
## commit 1a6607735597214ce9f43cf71b9755e1f7991688
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Apr 3 10:39:01 2017 -0500
## 
##     Implement bookdown and child reports
##     
##     child report:
##     - Add 00-complete using child Rmd with rossana-thesis method
##     
##     bookdown report
##     - Add index.Rmd following bookdown principles
##     - Copy all analysis/*.Rmd to root folder.
##     - remove all yaml. keep index yaml
##     - modify Rproj config after install bookdown
##     - Add _bookdown.yml w/ name: 01-complete + .Rmd inclusion.
##     
##     report content:
##     - Add index with To do + README
##     - Show Data dictionary + Urgent To do's
##     - Implement bookdown::cite-link: yes
## 
## commit 7ea9cebe399aec1c08d0ec52606dbcd894f7fe9a
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Apr 3 02:43:29 2017 -0500
## 
##     Add .dta output in 04-epidemio + 00-complete file
## 
## commit 19e2c94082006492565c24c732293f96702a466e
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Mar 14 23:56:47 2017 -0500
## 
##     Update data instructions in 04-epidemio.Rmd
## 
## commit 4c131aa061ab5e8874af22917deedd7837437db6
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Mar 14 17:10:34 2017 -0500
## 
##     Add load data instructions in 04-epidemio.Rmd
## 
## commit 3323c4de62ec0c4e4e8533b3c50f10289e2adb34
## Author: avallecam <avallecam@gmail.com>
## Date:   Thu Mar 9 17:22:34 2017 -0500
## 
##     Apply line breakes to ease Git
## 
## commit 99b318990e0f79daeff2e922863e07a6b5eab2ff
## Author: avallecam <avallecam@gmail.com>
## Date:   Wed Mar 8 18:18:30 2017 -0500
## 
##     Add slopegraph of age updates
## 
## commit eb3bba8303f62bf68a0a1f513909c95b59087639
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Mar 7 13:30:59 2017 -0500
## 
##     Clean report organization. Explicit results
## 
## commit 8cbd908b3146cbf5c4527525e731e18b95b57f30
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Mar 7 00:17:00 2017 -0500
## 
##     Add concensus in micro and pcr. One unique dataset
## 
## commit a2dd6a61bedca51d5b48282d4432201d8688717b
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Mar 6 16:04:09 2017 -0500
## 
##     Update objectives per Rmd. Add Git info in Appendix
## 
## commit 0774fb73e8153281413b1129053e64604164c213
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Mar 6 02:27:23 2017 -0500
## 
##     Rename analysis files
## 
## commit bdfa60b78725e6d52e64b2c414e933834c01a375
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Mar 6 02:25:30 2017 -0500
## 
##     Change output names
## 
## commit 42a4a55172234f3855ed606dd6a7404a97b7edb3
## Author: avallecam <avallecam@gmail.com>
## Date:   Sun Mar 5 23:35:54 2017 -0500
## 
##     Complete 04-epidemiology. Update TO DO in others
## 
## commit 8bd66d3af9d16ea9b79d3c76c14ffe7ad13be6c2
## Author: avallecam <avallecam@gmail.com>
## Date:   Thu Mar 2 18:50:05 2017 -0500
## 
##     Tide procedures in data-filtering and covariate
## 
## commit fc1a6a385f1ce1ca130cf4ab75e4a8706accf40a
## Author: avallecam <avallecam@gmail.com>
## Date:   Thu Mar 2 01:15:37 2017 -0500
## 
##     Tidy standardization output, std.uty generation, and epidemiology
## 
## commit b0b537eb06e96a333409b0014c7f17d078880c0c
## Author: avallecam <avallecam@gmail.com>
## Date:   Wed Feb 22 10:06:46 2017 -0500
## 
##     Add testzone.R script
## 
## commit 48dd74b064c82a8846e9e0f18145444d12e42697
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Feb 21 16:51:02 2017 -0500
## 
##     Done with unap1-5 covariates
## 
## commit 81557a86e0bfc0fda86582f70b346adc3e41009d
## Author: avallecam <avallecam@gmail.com>
## Date:   Thu Feb 16 01:44:18 2017 -0500
## 
##     Tidy covariates using Tidyverse. New EpiDB
## 
## commit 746af393e47880fbb0d7f898163e4db1c743e759
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Feb 14 00:50:13 2017 -0500
## 
##     Update renderRmd and no NA retrieving
##     
##     Two major updates on present commit:
##     - Follow Yihui suggestions on renderRmd.
##     - Major change on 02-workflow: No NA retrieving previous FILTERING
## 
## commit 26f125a166f1c17786bcdd8424340a091f358602
## Author: avallecam <avallecam@gmail.com>
## Date:   Fri Feb 10 16:54:38 2017 -0500
## 
##     Connect .Rmd by .Rds and render Rscript
## 
## commit a04bc1a2be5e83b760c01cf34f254e4fedab5775
## Author: avallecam <avallecam@gmail.com>
## Date:   Thu Feb 9 19:30:02 2017 -0500
## 
##     Split .Rmd files after Merge covariates
## 
## commit 536c0e45f46660eefc72402c8f1d5960a6a4ec01
## Author: avallecam <avallecam@gmail.com>
## Date:   Wed Feb 8 01:01:53 2017 -0500
## 
##     updated README
## 
## commit 46d1f7be2191e99fb584f5f1ca971a8ee79ce0e5
## Author: avallecam <avallecam@gmail.com>
## Date:   Wed Feb 1 12:54:46 2017 -0500
## 
##     Add git+bib benefits, Add re-run covariates, No ggplot histogram
## 
## commit 3f20a88d47655f43c8a962168b210468125a84a4
## Author: avallecam <avallecam@gmail.com>
## Date:   Tue Jan 31 23:31:10 2017 -0500
## 
##     Add .nb.html to gitignore
## 
## commit 568ab5e7eeb5ac0d5822dd5e57341b6b4d17058f
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Jan 30 19:12:17 2017 -0500
## 
##     SeroMarkerMir.Rproj deleted
## 
## commit d62bebecc604519418463b3ca1077f30622d13b2
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Jan 30 19:07:47 2017 -0500
## 
##     New Rproj name
## 
## commit b913e7ecff32702f01387d4c81fa9a473f023b6f
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Jan 30 17:03:34 2017 -0500
## 
##     README update
## 
## commit f62a12e65b24982e5b6f9c81cbebece0b272f2f9
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Jan 30 16:25:08 2017 -0500
## 
##     Added .csl to gitignore. First commit of working files
## 
## commit d356326b2b6a0af051bf1fc818efb81262c8cb41
## Author: avallecam <avallecam@gmail.com>
## Date:   Mon Jan 30 16:19:34 2017 -0500
## 
##     First commit: gitignore file
```


## APPENDIX: README file


```bash
less README.md
```

```
## # SeroSurveillance
## 
## This is a reproducible workflow in R for the standardization of ELISA reads across plates.
## It follows K. Miura et al. 2008 experimental procedure.
## 
## ## TO DO:
## <!-- 
## DONE:
## * Use bookdown to knit all the Rmd reports!
## -->
## * Clean code in standard.Rmd -> make one function then loop
## 
## 1. Add an alternative initial path: `plater` instead of `XLConnect`.
## 2. Generate inter-plate variability measurements: `inter-plate-CV`, `ctrl+`, `ctrl-`.
## 3. Check if `ZG182-1` should be corrected to `ZG181-1`.
## 4. Use `relational data` to conect `Serological dataset` and `Covariate+Epidemiological dataset`.
## 5. Solve inconsistencies in the Epidemiological dataset.
## 
## * Write an abstract!
## 
## - Write methodology and limitations
##     + Compare against `drLumi` statistical considerations
##     + `medrc` package
## - Add table with expected Template distribution
##     + `plater` package
## - Add details on K.Miura methods (appendix)
## 
## ## Project suggestions:
## 
## - Add STD duplicates on each plate?
##     + Provides limits of quantification
## - Add to surveillance:
##     + if longitudinal, register infection dates: days since last infection
## - Review communities history
##     + Rosas-Aguirre rejected ages older than 30
##     + Check Maki publication on 'Road construction...'
## - Standard procedure to outliers for re-run (based on ctrl comparisons)
## 
## ## Additional analysis:
## 
## - Multinomial Logistic regression
## - Individual level heterogeneity:
##     + map location
##     + climate, geographical differences, altitude, ...
##     + seroprevalence adjusted with covariates
##     + sample coordinates
## - Timeline PERU and LORETO malaria case index per year
##     + Angel Rosas-Aguirre on VIVAX EPIDEMIO PERU
##     + Veronica Soto-Calle on SPATIO-TEMPORAL MALARIA LORETO
##     
## ## Curiosities:
## 
## - Some lab replicate patterns:
##     + N21 -> N28 -> {N30, N31, N32}
##     + {N21, N20, N22} -> N24 -> N28
```


<!--## References-->

<!--chapter:end:00-appendix.Rmd-->

# ELISA STANDARDIZATION {#stdrad}

## Aim

* Principal: 
    - Implement a reproducible standardization of OD values across ELISA plates following 
    **Miura et.al., 2008**[@Miura2008].

* Secondary: 
    - Generate useful outputs to compare standardization quality.


## To Do

- Implement `plater` and `readxl` data input
- Assess the **inter-plate-CV** comparing STD variability across plates to generate a 
**Limit of Quantification**
    + Actual CV is the **intra-plate-CV**
    + Generate a tibble of all STD dilutions OD's
    + Extend `std.par` output to all parameters
- Other inter-plate measurements:
    + blank, positive and negative controls


## Dependencies

The required R packages for this analysis are:

+ `plater` [@plater]
+ `XLConnect` [@XLConnect]
+ `drc` [@Ritz2015]


```r
##essential
library(XLConnect)    # load EXCEL workbooks
library(drc)          # Dose-Response modeling
##accesory
library("DiagrammeR") # method Flowchart
library("knitr")      # To display nice tables
library("tidyverse")
#ggplot2, tibble, tidyr, readr, purr, dplyyr
##extra
library("Rmisc")      # multiplot ggplots
library("haven")      # import STATA df
library("readxl")     # import EXCEL df
library("forcats")    # factor vectors
library("viridis")    # color visualization
```

## Method

- using `DiagrammeR` [@DiagrammeR]


```r
#install.packages("DiagrammeR")
#library(DiagrammeR)

DiagrammeR("
  graph LR
    A[XLS data] -.-> |XLConnect| B{R data}
    Z[CSV data] -.-> |plater| B{R data}
    B --> C1[STD]
    B --> E[ctr +/-]
    B --> D1[UNK]
    
    C1 -.-> |drc| C2[4pLL model]
    C2 --> C3[Box-Cox]
    C3 --> F{UNK Ab.units}

    D1 --> D2[mean.OD]
    D2 --> D3[OD %CV]
    D3 --> F
    
    F --> G1[Histogram]
    F --> G2[Density]
    F --> G3[QQPlot]

    style Z fill:#ffffff, stroke:#000000, stroke-width:2px    
    style A fill:#ffffff, stroke:#000000, stroke-width:2px
    style B fill:#ffffff, stroke:#000000, stroke-width:2px
    style C1 fill:#ffffff, stroke:#000000, stroke-width:2px
    style C2 fill:#ffffff, stroke:#000000, stroke-width:2px
    style C3 fill:#ffffff, stroke:#000000, stroke-width:2px
    style D1 fill:#ffffff, stroke:#000000, stroke-width:2px
    style D2 fill:#ffffff, stroke:#000000, stroke-width:2px
    style D3 fill:#ffffff, stroke:#000000, stroke-width:2px
    style E fill:#ffffff, stroke:#000000, stroke-width:2px
    style F fill:#ffffff, stroke:#000000, stroke-width:2px
    style G1 fill:#ffffff, stroke:#000000, stroke-width:2px
    style G2 fill:#ffffff, stroke:#000000, stroke-width:2px
    style G3 fill:#ffffff, stroke:#000000, stroke-width:2px
")
```

<!--html_preserve--><div id="htmlwidget-a520bd60bd46122dcbfd" style="width:864px;height:480px;" class="DiagrammeR html-widget"></div>
<script type="application/json" data-for="htmlwidget-a520bd60bd46122dcbfd">{"x":{"diagram":"\n  graph LR\n    A[XLS data] -.-> |XLConnect| B{R data}\n    Z[CSV data] -.-> |plater| B{R data}\n    B --> C1[STD]\n    B --> E[ctr +/-]\n    B --> D1[UNK]\n    \n    C1 -.-> |drc| C2[4pLL model]\n    C2 --> C3[Box-Cox]\n    C3 --> F{UNK Ab.units}\n\n    D1 --> D2[mean.OD]\n    D2 --> D3[OD %CV]\n    D3 --> F\n    \n    F --> G1[Histogram]\n    F --> G2[Density]\n    F --> G3[QQPlot]\n\n    style Z fill:#ffffff, stroke:#000000, stroke-width:2px    \n    style A fill:#ffffff, stroke:#000000, stroke-width:2px\n    style B fill:#ffffff, stroke:#000000, stroke-width:2px\n    style C1 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style C2 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style C3 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style D1 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style D2 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style D3 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style E fill:#ffffff, stroke:#000000, stroke-width:2px\n    style F fill:#ffffff, stroke:#000000, stroke-width:2px\n    style G1 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style G2 fill:#ffffff, stroke:#000000, stroke-width:2px\n    style G3 fill:#ffffff, stroke:#000000, stroke-width:2px\n"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### Log-Logistic (4pLL) model

The curve of the **log-logistic symetric** model describe the *response* `f(x)` dependent of the *dose* `x` 
and **04 parameters**: $$ f(x)=f(x;b,c,d,e)=c+\frac{d-c}{1+\exp[b(log(x)-log(e))]}\ $$ where: 
  
- `c` is the **lower limit** of the response when the *dose* `x` approaches infinity, 
- `d` is the **upper limit** when the *dose* `x` approaches zero,
- `b` is the **slope** around the **point of inflection**, represented by 
- `e` defined as **effective dose** and commmonly denoted as [@Ritz2015]:
    + `ED50`, `EC50` or `IC50` for continuous responses,
    + `LD50` or `LC50` for binomial responses, and
    + $T_{50}$ for event-time responses.

## Procedure

**12 summary plots** per ELISA Template:

- **3x3 plots** of STD and UNK distribution, residual variance distribution, and model transformation.
- **1x3 plots** of OD~450nm~, mean.OD and Ab.units distributions by Density plots.



```r
#
## CREATE A LIST WITH THE FILE.NAMES
#
a <- "Template_ELISA_N"
b <- seq(11,41,1)
b <- b[-c(3,4,9)] # NO TEMPLATE AVAILABLE
c <- ".xlsx"
#
d <- paste0(a,b,c)
#d[j]
data.frame(d)
```

```
##                          d
## 1  Template_ELISA_N11.xlsx
## 2  Template_ELISA_N12.xlsx
## 3  Template_ELISA_N15.xlsx
## 4  Template_ELISA_N16.xlsx
## 5  Template_ELISA_N17.xlsx
## 6  Template_ELISA_N18.xlsx
## 7  Template_ELISA_N20.xlsx
## 8  Template_ELISA_N21.xlsx
## 9  Template_ELISA_N22.xlsx
## 10 Template_ELISA_N23.xlsx
## 11 Template_ELISA_N24.xlsx
## 12 Template_ELISA_N25.xlsx
## 13 Template_ELISA_N26.xlsx
## 14 Template_ELISA_N27.xlsx
## 15 Template_ELISA_N28.xlsx
## 16 Template_ELISA_N29.xlsx
## 17 Template_ELISA_N30.xlsx
## 18 Template_ELISA_N31.xlsx
## 19 Template_ELISA_N32.xlsx
## 20 Template_ELISA_N33.xlsx
## 21 Template_ELISA_N34.xlsx
## 22 Template_ELISA_N35.xlsx
## 23 Template_ELISA_N36.xlsx
## 24 Template_ELISA_N37.xlsx
## 25 Template_ELISA_N38.xlsx
## 26 Template_ELISA_N39.xlsx
## 27 Template_ELISA_N40.xlsx
## 28 Template_ELISA_N41.xlsx
```

```r
#
e <- rep("data-raw/",length(d))
#e <- rep("~/Documents/Valle_GnB/000_R_script_MIRRORS/R_test/SeroMarkerMir/data-raw/",length(d))
d <- paste0(e,d)
```


```r
## A NULL DATA.FRAME TO FEED WITH DATA.ANALYSIS OUTCOMES
std.raw <- data_frame()
std.par <- data_frame()
```


```r
#
## A LOOP TO ANALYSE EACH TEMPLATE
#
for (j in 1:length(d)) {
#
####
#### START ANALYSIS
####
# 1 UPLOAD excel data
wb <- loadWorkbook(d[j])
#
wb_Pf_main <- readWorksheet(wb, 
                         sheet = 1, 
                         startRow = 22, endRow = 30,
                         startCol = 3, endCol = 14)
wb_Pf <- readWorksheet(wb, 
                         sheet = 1, 
                         startRow = 33, endRow = 41,
                         startCol = 3, endCol = 14)
wb_Pv_main <- readWorksheet(wb, 
                         sheet = 1, 
                         startRow = 79, endRow = 87,
                         startCol = 3, endCol = 14)
wb_Pv <- readWorksheet(wb, 
                         sheet = 1, 
                         startRow = 90, endRow = 98,
                         startCol = 3, endCol = 14)
# 2 GENERATE R DATA.FRAME
Pf.OD <- matrix(wb_Pf[1,])
Pf.ID <- matrix(wb_Pf_main[1,])
Pv.OD <- matrix(wb_Pv[1,])
Pv.ID <- matrix(wb_Pv_main[1,])

for (i in 2:nrow(wb_Pf)) {
  Pf.OD <- rbind(Pf.OD,matrix(wb_Pf[i,]))
  Pf.ID <- rbind(Pf.ID,matrix(wb_Pf_main[i,]))
  Pv.OD <- rbind(Pv.OD,matrix(wb_Pv[i,]))
  Pv.ID <- rbind(Pv.ID,matrix(wb_Pv_main[i,]))
}
#
t<-seq(1,12,1)
x <- c()
x0 <- 50
f <- function(x){x*2}
x[1]<-x0
for (i in 1:(length(t)-1)) {
  x[i+1] = f(x[i])
}
#

wb_ALL <- data.frame(paste0("N",b[j]),
                      rbind(Pf.ID,Pv.ID),
                      rbind(matrix(rep("std",12)),
                            matrix(rep("unk",80)),"std",
                            matrix(rep("ctr",2)),"std"), 
                      rep(x[length(x)]/x), ##FACTOR for AB.UNITS
                      rbind(Pf.OD,Pv.OD), 
                      gl(2,96, labels = c("Pfal","Pviv")))
colnames(wb_ALL) <- c("Plate", "ID", "Type", "Ab.unit", "OD", "Specie")
wb_ALL$ID <- unlist(wb_ALL$ID)
wb_ALL$OD <- unlist(wb_ALL$OD)
wb_ALL$ID <- factor(wb_ALL$ID)
# 2.1 AVOID NA's
wb_ALL <- wb_ALL[complete.cases(wb_ALL[,2]),]
# 2.2 CLEAN data.frame
wb_ALL[wb_ALL$Type!="std","Ab.unit"] <- NA
wb_ALL[wb_ALL$ID=="Blank","Ab.unit"] <- 0
# 2.3 CORRECT variable class
wb_ALL$OD <- as.numeric(wb_ALL$OD)
#
# 2.4 IDENTIFY blank ctrl+ ctrl-
blank.Pfal <- mean(as.numeric(wb_ALL[wb_ALL$ID=="Blank" & wb_ALL$Specie=="Pfal",5]))
blank.Pviv <- mean(as.numeric(wb_ALL[wb_ALL$ID=="Blank" & wb_ALL$Specie=="Pviv",5]))
ctrPos.Pfal <- mean(as.numeric(wb_ALL[wb_ALL$ID=="C+" & wb_ALL$Specie=="Pfal",5]))
ctrPos.Pviv <- mean(as.numeric(wb_ALL[wb_ALL$ID=="C+" & wb_ALL$Specie=="Pviv",5]))
ctrNeg.Pfal <- mean(as.numeric(wb_ALL[wb_ALL$ID=="C-" & wb_ALL$Specie=="Pfal",5]))
ctrNeg.Pviv <- mean(as.numeric(wb_ALL[wb_ALL$ID=="C-" & wb_ALL$Specie=="Pviv",5]))
# 3 UNK MEAN OD
wb_UNK <- subset(wb_ALL, Type=="unk")
wb_UNK$ID <- factor(wb_UNK$ID) # unk goes from 
n <- length(levels(wb_UNK$ID))
# 3.1 MEAN DATA.FRAME
wb_MEAN <- data.frame(paste0("N",b[j]),
                        levels(wb_UNK$ID),
                        rep("unk",length(levels(wb_UNK$ID))), 
                        rep(NA,length(levels(wb_UNK$ID))),#Ab.unit
                        rep(NA,length(levels(wb_UNK$ID))),#mean.OD
                        gl(2,length(levels(wb_UNK$ID)), 
                           labels = c("Pfal","Pviv")))
colnames(wb_MEAN) <- c("Plate", "ID", "Type", "Ab.unit", "mean.OD", "Specie")
# 3.2 FEED MEAN DATA.FRAME
for (i in 1:n) {
  wb_MEAN[i,5] <- mean(as.numeric(subset(wb_UNK, 
                                           ID==levels(wb_UNK$ID)[i] &
                                             Specie==levels(wb_UNK$Specie)[1])$OD))
  wb_MEAN[n+i,5] <- mean(as.numeric(subset(wb_UNK, 
                                             ID==levels(wb_UNK$ID)[i] &
                                               Specie==levels(wb_UNK$Specie)[2])$OD))
}
#
######
###### [OFF] BACKGROUND SUBTRACTION
######
# 4 STD.DEV DATA.FRAME
wb_SDCV <- cbind(wb_MEAN, 
                   rep(NA,length(levels(wb_UNK$ID))),#mean.OD
                   rep(NA,length(levels(wb_UNK$ID))))#sd.OD)
colnames(wb_SDCV) <- c("Plate", "ID", "Type", "Ab.unit", "mean.OD", "Specie", "sd.OD", "cv.OD")
# 4.1 FEED STD.DEV DATA.FRAME
for (i in 1:n) {
  wb_SDCV[i,7] <- sd(subset(wb_UNK, 
                              ID==levels(wb_UNK$ID)[i] &
                                Specie==levels(wb_UNK$Specie)[1])$OD)
  wb_SDCV[n+i,7] <- sd(subset(wb_UNK, 
                                ID==levels(wb_UNK$ID)[i] &
                                  Specie==levels(wb_UNK$Specie)[2])$OD)
}

wb_SDCV[,8] <- 100*(wb_SDCV[,7] / wb_SDCV[,5])
#
# 5 PARAMETER ESTIMATION 4pLL model
#
wb.m1 <- drm(OD ~ Ab.unit, Specie, 
               data= subset(wb_ALL, Type=="std"),
               fct = LL.4(names = c("b", "c", "d", "e")))
#
wb.model <- wb.m1
# 6 BOX-COX TRANSFORMATION against RESIDUAL heterogeneity
wb.model.BX <- boxcox(wb.model, 
                     main=expression("Optimal " ~ lambda ~ " with confidence intervals"), 
                     plotit = FALSE)
# 7 UNK AB.UNITS ESTIMATION by INVERSE REGRESSION
wb_Resp.Pf <- ED(wb.model.BX, 
                   wb_MEAN[1:n,5],
                   type = "absolute",interval = "delta",
                   clevel = "Pfal", display = FALSE)
wb_Resp.Pv <- ED(wb.model.BX, 
                   wb_MEAN[(n+1):(2*n),5],
                   type = "absolute",interval = "delta",
                   clevel = "Pviv", display = FALSE)
# 7.1 FEED UNK AB.UNITS DATA.FRAME
for (i in 1:n) {
  wb_SDCV[i,4] <- wb_Resp.Pf[i]
  wb_SDCV[n+i,4] <- wb_Resp.Pv[i]
}
#
####
#### END ANALYSIS
#
#### START PLOTTING
####
#
par(mfrow=c(4,3))
### 1 DISTRIBUTION PLOTING
plot(OD ~ log(Ab.unit), 
     data=subset(wb_ALL, Type=="std" & Specie=="Pfal"), 
     ylim = c(0,1.5), col="red",
     xlab = "log(Ab unit)", ylab = "OD 450nm",
     main= paste0("ELISA plate"," ","N",b[j],"\n Standard samples distribution"))
points(OD ~ log(Ab.unit), 
       data=subset(wb_ALL, Type=="std" & Specie=="Pviv"), 
       pch=2, col="blue")
abline(h=c(ctrNeg.Pfal, ctrNeg.Pviv), lty=c(3, 3), col=c("red", "blue"))
abline(h=c(ctrPos.Pfal, ctrPos.Pviv), lty=c(3, 3), col=c("red", "blue"))
abline(h=c(blank.Pfal, blank.Pviv), lty=c(2, 2), col=c("red", "blue"))
legend("bottomright", c("P.vivax","P.falcip", "C+/-", "blank"), 
       col = c( "blue", "red", "black", "black"), 
       pch = c(2, 1, NA, NA), #lwd= c(1,1),
       lty = c(NA, NA, 3, 2),
       cex = 0.8,
       inset = .01#, merge = TRUE
)
#
### 2 %CV CONDITIONAL PLOTING
plot(cv.OD ~ mean.OD,wb_SDCV, 
     ylim=c(0,100), xlim=c(0,1.5),
     xlab="mean OD 450nm of duplicates",
     ylab="%CV of duplicates",
     main="Percentage Coefficient of Variation")
abline(h=20, v=0.25, lty=2, col=2)
#
### 3 UNK AB.UNITS PLOTING
plot(wb.model.BX,#broken = TRUE,
     ylim = c(0,1.5), 
     xlim = c(0,5e3),
     main=paste0("ELISA plate"," ","N",b[j],"\n Unknown samples distribution"),
     col="grey",
     legendPos = c(2,1.5))
points(y=wb_MEAN[(n+1):(2*n),5],x=wb_Resp.Pv[1:n],col="blue",pch=4)
points(y=wb_MEAN[1:n,5],x=wb_Resp.Pf[1:n],col="red",pch=4)
#abline(h=c(ctrNeg.Pfal, ctrNeg.Pviv), lty=c(3, 3), col=c("red", "blue"))
#abline(h=c(ctrPos.Pfal, ctrPos.Pviv), lty=c(3, 3), col=c("red", "blue"))
#abline(h=c(blank.Pfal, blank.Pviv), lty=c(2, 2), col=c("red", "blue"))
legend("bottomright", c("P.vivax","P.falcip"), #, "C+/-", "blank"
       col = c( "blue", "red"), #, "black", "black"
       pch = c(4, 4), #, NA, NA #lwd= c(1,1),
       #lty = c(NA, NA), #, 3, 2
       cex = 0.8,
       inset = .01#, merge = TRUE
)
#
### 4 VARIANCE HETEROGENEITY
plot(residuals(wb.model) ~ fitted(wb.model), 
     main="4p Log-Logistic model \n Variance Hetereogeneity",
     xlab="fitted",
     ylab = "residuals")
abline(h=0)
#
### 5 LAMBDA ESTIMATE ##------Box-Cox transformation-------##
wb.model.BX <- boxcox(wb.model, 
                     main=expression("Optimal " ~ lambda ~ " with confidence intervals"))
#
### 6 VARIANCE HOMOGENEITY
plot(residuals(wb.model.BX) ~ fitted(wb.model.BX), 
     main="Box-Cox transformed model \n Variance Homogeneity",
     xlab="fitted",
     ylab = "residuals")
abline(h=0)
#
### 7 RESIDUAL SKWENESS
qqnorm(residuals(wb.model), 
       main = "4p Log-Logistic model \n Normal Q-Q Plot")
qqline(residuals(wb.model))
#
### 8 DRC AND BOXCOX OVERLAPPED MODELS
plot(wb.model, #broken = TRUE, 
     xlab="Ab units", ylab="OD 450nm",
     ylim = c(0,1.5), 
     xlim = c(0,5e3),
     main="in black: 4p Log-Logistic model \n in red: Box-Cox transformed model",
     legendPos = c(2,1.5))
plot(wb.model.BX, col = "red", add = TRUE, legend = FALSE,
     ylim = c(0,1.5), 
     xlim = c(0,5e5))
#
### 9 RESIDUAL NORMALITY
qqnorm(residuals(wb.model.BX), 
       main="Box-Cox transformed model \n Normal Q-Q Plot")
qqline(residuals(wb.model.BX))
#
####
####
#
### 10 OD REPLICATES DISTRIBUTION
#par(mfrow=c(3,3))
#my <- max(hist(wb_ALL$OD, plot = F)$counts, na.rm=TRUE)
#hist(wb_ALL$OD, ylim = c(0,my), 
#     main= paste0("ELISA plate"," ","N",b[j]),
#     xlab = "OD")
plot(density(na.omit(wb_ALL$OD)),
     main = "OD Density plot",
     xlab = "OD")#, ylim = c(0,my)  #, xlim = c(0,mx)
#qqnorm(wb_ALL$OD); qqline(wb_ALL$OD)
#
### 11 OD MEAN DISTRIBUTION
#hist(wb_MEAN$mean.OD, ylim = c(0,my),
#     main = "mean.OD Histogram",
#     xlab = "mean.OD")
plot(density(na.omit(wb_MEAN$mean.OD)),
     main = "mean.OD Density plot",
     xlab = "mean.OD")#, ylim = c(0,my)
#qqnorm(wb_MEAN$mean.OD); qqline(wb_MEAN$mean.OD)
#
### 12 AB.UNIT DISTRIBUTION
#hist(wb_SDCV$Ab.unit, ylim = c(0,my),
#     main = "Ab.units Histogram",
#     xlab = "Ab.units")
plot(density(na.omit(wb_SDCV$Ab.unit)),
     main = "Ab.units Density plot",
     xlab = "Ab.units")#, ylim = c(0,my)
#qqnorm(wb_SDCV$Ab.unit); qqline(wb_SDCV$Ab.unit)
#
####
#### END PLOTTING
#### START ROW BINDING TO FINAL DATA.FRAME
####
std.raw <- rbind(std.raw,wb_SDCV)
####
std.par <- rbind(std.par, data_frame(Plate= paste0("N",b[j]),
                                       c_Pfal= coefficients(wb.model.BX)[3],
                                       c_Pviv= coefficients(wb.model.BX)[4],
                                       d_Pfal= coefficients(wb.model.BX)[5],
                                       d_Pviv= coefficients(wb.model.BX)[6]))
####
}
```

<img src="01-complete_files/figure-html/unnamed-chunk-16-1.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-2.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-3.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-4.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-5.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-6.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-7.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-8.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-9.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-10.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-11.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-12.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-13.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-14.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-15.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-16.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-17.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-18.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-19.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-20.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-21.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-22.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-23.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-24.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-25.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-26.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-27.png" style="display: block; margin: auto;" /><img src="01-complete_files/figure-html/unnamed-chunk-16-28.png" style="display: block; margin: auto;" />

## Output

```r
saveRDS(tbl_df(std.raw), "data/01-standard-raw.rds")
saveRDS(tbl_df(std.par), "data/01-parametr.rds")
saveRDS(Pf.ID, "data/01-dilution.rds")
```


***

## APPENDIX: Upper/Lower limits


```r
#std.par <- readRDS("data/01-parametr.rds")
std.par
```

```
## # A tibble: 28 x 5
##    Plate     c_Pfal     c_Pviv    d_Pfal    d_Pviv
##    <chr>      <dbl>      <dbl>     <dbl>     <dbl>
##  1   N11 0.04171865 0.03961389 1.3869566 1.4301670
##  2   N12 0.05180474 0.05755776 1.4290674 1.3541624
##  3   N15 0.06997395 0.04365285 0.8869461 1.0655199
##  4   N16 0.05079147 0.04895392 0.8461171 0.7496968
##  5   N17 0.04574432 0.04563150 1.1949685 0.8686462
##  6   N18 0.04204531 0.04150384 0.6470736 0.9601214
##  7   N20 0.04404236 0.04258166 0.8025537 0.8272903
##  8   N21 0.04361240 0.04580946 0.6655774 0.7539252
##  9   N22 0.05240877 0.05040193 0.9719488 0.8719549
## 10   N23 0.04280990 0.04158864 0.8908384 0.9028253
## # ... with 18 more rows
```

***

<!--## References-->

<!--chapter:end:01-standard.Rmd-->

# DATA FILTERING {#filter}

## Aim

* Principal: 
    - Identify and filter the replicated reads across ELISA plates.

* Secondary: 
    - Visualize OD and AU distributions post ELISA standardization.

## Results

- **02 typos** were corrected. [here](#db-typo-corrections).
- This experiment made **1937** observations for **969** ID's. [here](#all)
- Among the **73** NA observations, **04** had an OD higher than the upper limit 
given by its STD. [here](#na)
- **63** observations had duplicates, **16** had triplicates. [here](#replicates)
- A __unbiased selection criteria__ was implemented in a function [here](#unbiased-selection) and later used to filter replicates. [here](#filtering)
- __Log transformed Ab.units__ shows a __normal distribution__. [here](#distributions)
- __Outputs__ were stored in both _tidy_ and __untidy__ formats. [here](#tidy-format)


## To Do

- Is it required to change NaN with 0 and NaN or .?
    + string replacement on data.filtering
    + May not be convinient due to problems in data transformation and statistics.
- CHECK IF **ZG182-1** should be corrected to **ZG181-1**.
    + Missing reads are complementary between ID's.
    + More detail in [Appendix](#appendix-residual-na) **NOTE 2**.


## Dependencies

The required R packages for this analysis are:

+ `ggplot2` [@ggplot2]


```r
##essential
library(XLConnect)    # load EXCEL workbooks
library(drc)          # Dose-Response modeling
##accesory
library("DiagrammeR") # method Flowchart
library("knitr")      # To display nice tables
library("tidyverse")
#ggplot2, tibble, tidyr, readr, purr, dplyyr
##extra
library("Rmisc")      # multiplot ggplots
library("haven")      # import STATA df
library("readxl")     # import EXCEL df
library("forcats")    # factor vectors
library("stringr")    # strings
library("viridis")    # color visualization
```

## Input

```r
std.raw <- readRDS("data/01-standard-raw.rds")
std.par <- readRDS("data/01-parametr.rds")
```

***

## Procedure

### DB typo corrections

After showing the missing of **02** samples in the Templates, typos were identified 
and the corresponding modifications were made following Reproducible Reseach paradigms, 
i.e. avoiding any editing on the original DB.

This correction introduced the following samples:

- **ZG009-3**, by changing *ZG009-1* in plate **N17**, and


```r
#str(std.raw)
std.raw$ID <- as.character(std.raw$ID)

std.raw[std.raw$ID=="ZG009-1",]
```

```
## # A tibble: 4 x 8
##    Plate      ID   Type    Ab.unit mean.OD Specie       sd.OD    cv.OD
##   <fctr>   <chr> <fctr>      <dbl>   <dbl> <fctr>       <dbl>    <dbl>
## 1    N11 ZG009-1    unk  2.7924878  0.0810   Pfal 0.002828427 3.491885
## 2    N11 ZG009-1    unk 41.3463006  0.7825   Pviv 0.062932504 8.042492
## 3    N17 ZG009-1    unk  0.8658179  0.0590   Pfal 0.001414214 2.396972
## 4    N17 ZG009-1    unk  0.1131599  0.0590   Pviv 0.002828427 4.793944
```

```r
#std.raw[std.raw$ID=="ZG009-1" & std.raw$Plate=="N17",]
std.raw[std.raw$ID=="ZG009-1" & std.raw$Plate=="N17",2] <- rep("ZG009-3",2)
#std.raw[std.raw$ID=="ZG009-1",]
std.raw[std.raw$ID=="ZG009-3",]
```

```
## # A tibble: 2 x 8
##    Plate      ID   Type   Ab.unit mean.OD Specie       sd.OD    cv.OD
##   <fctr>   <chr> <fctr>     <dbl>   <dbl> <fctr>       <dbl>    <dbl>
## 1    N17 ZG009-3    unk 0.8658179   0.059   Pfal 0.001414214 2.396972
## 2    N17 ZG009-3    unk 0.1131599   0.059   Pviv 0.002828427 4.793944
```

- **NN068-2**, by changing *NN082-5* in plate **N33**.


```r
std.raw[std.raw$ID=="NN082-5",]
```

```
## # A tibble: 4 x 8
##    Plate      ID   Type   Ab.unit mean.OD Specie        sd.OD     cv.OD
##   <fctr>   <chr> <fctr>     <dbl>   <dbl> <fctr>        <dbl>     <dbl>
## 1    N33 NN082-5    unk 1.1838246  0.0675   Pfal 0.0148492424 21.998878
## 2    N33 NN082-5    unk 0.6503003  0.0615   Pviv 0.0007071068  1.149767
## 3    N34 NN082-5    unk 6.6846107  0.1220   Pfal 0.0028284271  2.318383
## 4    N34 NN082-5    unk 2.3090638  0.1195   Pviv 0.0049497475  4.142048
```

```r
#std.raw[std.raw$ID=="NN082-5" & std.raw$Plate=="N33",]
std.raw[std.raw$ID=="NN082-5" & std.raw$Plate=="N33",2] <- rep("NN068-2",2)
std.raw[std.raw$ID=="NN068-2",]
```

```
## # A tibble: 2 x 8
##    Plate      ID   Type   Ab.unit mean.OD Specie        sd.OD     cv.OD
##   <fctr>   <chr> <fctr>     <dbl>   <dbl> <fctr>        <dbl>     <dbl>
## 1    N33 NN068-2    unk 1.1838246  0.0675   Pfal 0.0148492424 21.998878
## 2    N33 NN068-2    unk 0.6503003  0.0615   Pviv 0.0007071068  1.149767
```

```r
#std.raw[std.raw$ID=="NN082-5",]

std.raw$ID <- as.factor(std.raw$ID)
#str(std.raw)
```

As expected, this reduced the number of duplicates.

### ALL


```r
## total de ID evaluados
mue <- length(levels(as.factor(std.raw$ID))) # TOTAL MUESTRAS EVALUADAS
## total ESPERADO + REPLICAS
rep <- dim(std.raw)[1] # TOTAL INICIAL
## total ESPERADO de ID x2 SPECIES-REPLICATES (uno de P.VIVAX y uno de P.FALCIPARUM)
esp <- 2*length(levels(as.factor(std.raw$ID))) # TOTAL ESPERADO
## total de ID REPETIDOS (corridos por duplicado o triplicado en nuevos PLATES) y que se deben retirar
repet <- rep - esp
```

The total amount of evaluated samples is **969**. However, due to the presence of 
replicates among Templates, the initial number of reported reads with both `mean.OD` 
and estimated `Ab.unit` goes up to **2128**.

If each sample have 02 reads, 01 per specie, then the **total expected** amount of 
samples is **1938**. The rest are replicates (**190**).

Here is the total amount of evaluated samples in both Pv/Pf plates of each Template file:


```r
## total de muestras por TEMPLATE (P.VIVAX igual a P.FALCIPARUM)
table(std.raw[std.raw$Specie=="Pviv",]$Plate)
```

```
## 
## N11 N12 N15 N16 N17 N18 N20 N21 N22 N23 N24 N25 N26 N27 N28 N29 N30 N31 N32 N33 N34 N35 N36 N37 N38 N39 N40 
##  40  39  41  40  40  36  40  40  40  10  40  40  40  40  40  40  40  40  40  40  40  40  40  40  40  40  40 
## N41 
##  18
```

- 04 Templates with lower samples than expected:
    + N12 (n=39), N18 (n=36), N23 (n=10), N41 (n=18).

- 01 Template with more samples than expected:
    + N15 (n=41)
        - **ZG181-1** sampled against **Pviv** plate only
        - **ZG182-1** sampled against **Pfal** plate only.
    + Both generated NaNs at the end.
    + Further dicussion at the end of the next section.

### NA

The total amount of samples with **unestimated `Ab.unit` goes up to 
73**:

```r
## muestras con Ab.units NO estimadas
table(std.raw[is.na(std.raw$Ab.unit)==TRUE,]$Plate)
```

```
## 
## N11 N12 N15 N16 N17 N18 N20 N21 N22 N23 N24 N25 N26 N27 N28 N29 N30 N31 N32 N33 N34 N35 N36 N37 N38 N39 N40 
##   0   5   8   6   1   0   1   0   4   0  35   0   0   0   0   0   8   1   0   1   0   0   1   0   0   1   1 
## N41 
##   0
```

```r
## REASON: above plate-specific model upper limit or below its lower limit

# a table of NaN only
std.nan <- std.raw %>% 
  filter(Ab.unit=="NaN")
```


```r
# tidy parameter tibble
std.par_t <- std.par %>% 
  gather(parSp, par.value, -Plate) %>% 
  separate(parSp,
           c("par","par.Specie"),
           sep="_")

# differentiate between the ones out by lower or upper limmit
std.out <- std.nan %>% 
  select(Plate, ID, Ab.unit, mean.OD, Specie) %>% 
  full_join(std.par_t,by = "Plate") %>% 
  filter(Specie == par.Specie) %>% 
  spread(par, par.value) %>% 
  mutate(out.low= c>mean.OD,
         out.upp= d<mean.OD) 
```

- **67 samples** have OD below the lower limit:


```r
# below lower limit
std.out %>% 
  filter(out.low==T)
```

```
## # A tibble: 67 x 10
##    Plate      ID Ab.unit mean.OD Specie par.Specie          c         d out.low out.upp
##    <chr>  <fctr>   <dbl>   <dbl> <fctr>      <chr>      <dbl>     <dbl>   <lgl>   <lgl>
##  1   N12 ZG023-5     NaN  0.0575   Pviv       Pviv 0.05755776 1.3541624    TRUE   FALSE
##  2   N12 ZG162-4     NaN  0.0495   Pfal       Pfal 0.05180474 1.4290674    TRUE   FALSE
##  3   N12 ZG177-4     NaN  0.0505   Pfal       Pfal 0.05180474 1.4290674    TRUE   FALSE
##  4   N12 ZG178-6     NaN  0.0500   Pfal       Pfal 0.05180474 1.4290674    TRUE   FALSE
##  5   N15 ZG031-5     NaN  0.0665   Pfal       Pfal 0.06997395 0.8869461    TRUE   FALSE
##  6   N15 ZG031-6     NaN  0.0685   Pfal       Pfal 0.06997395 0.8869461    TRUE   FALSE
##  7   N15 ZG045-4     NaN  0.0670   Pfal       Pfal 0.06997395 0.8869461    TRUE   FALSE
##  8   N15 ZG047-3     NaN  0.0620   Pfal       Pfal 0.06997395 0.8869461    TRUE   FALSE
##  9   N15 ZG047-4     NaN  0.0675   Pfal       Pfal 0.06997395 0.8869461    TRUE   FALSE
## 10   N15 ZG183-2     NaN  0.0695   Pfal       Pfal 0.06997395 0.8869461    TRUE   FALSE
## # ... with 57 more rows
```

- **04 samples** have OD above the upper limit:


```r
# above upeer limit
std.out %>% 
  filter(out.upp==T)
```

```
## # A tibble: 4 x 10
##   Plate      ID Ab.unit mean.OD Specie par.Specie          c         d out.low out.upp
##   <chr>  <fctr>   <dbl>   <dbl> <fctr>      <chr>      <dbl>     <dbl>   <lgl>   <lgl>
## 1   N12 ZG163-2     NaN  1.3875   Pviv       Pviv 0.05755776 1.3541624   FALSE    TRUE
## 2   N16 ZG174-4     NaN  0.8135   Pviv       Pviv 0.04895392 0.7496968   FALSE    TRUE
## 3   N31 NN038-6     NaN  0.9365   Pviv       Pviv 0.05246624 0.9349103   FALSE    TRUE
## 4   N39 LL031-6     NaN  0.7345   Pviv       Pviv 0.04408131 0.7307626   FALSE    TRUE
```

More details in [Appendix](#appendix-residual-na)

### Replicates

From the **total expected** samples, we calculate that the total amount of reads in excess that came from replicates (duplicates or triplicates) among Templates goes up to **190**.


```r
## FRECUENCIA por ID
Ab_freq <- data.frame(table(as.factor(std.raw$ID)))
```

The total amount of samples with **duplicated** reads is 
**63** and with **triplicated** reads is 
**16**.


```r
summary(factor(Ab_freq$Freq))
```

```
##   2   4   6 
## 890  63  16
```

#### Triplicates

Listado de muestras **triplicadas** (n=16):

```r
## LISTADO de muestras TRIPLICADAS
trpl.od <- Ab_freq[Ab_freq$Freq==6,]$Var1

std.trpl <- data.frame()
for (i in 1:length(trpl.od)) {
  std.trpl <- rbind(std.trpl,std.raw[std.raw$ID==trpl.od[i],])
}

as.character(trpl.od)
```

```
##  [1] "LL055-7"  "NN014-1"  "NN014-3"  "NN014-4"  "NN053-11" "NN053-12" "NN053-9"  "NN062-6"  "NN062-8" 
## [10] "ZG086-5"  "ZG092-1"  "ZG108-4"  "ZG155-6"  "ZG158-3"  "ZG174-4"  "ZG189-4"
```

```r
dim(std.trpl)
```

```
## [1] 96  8
```

```r
#head(std.trpl,12)
summary(std.trpl[,-c(3,6)])
```

```
##      Plate           ID        Ab.unit            mean.OD           sd.OD              cv.OD       
##  N28    :26   LL055-7 : 6   Min.   :  0.1411   Min.   :0.0500   Min.   :0.000000   Min.   : 0.000  
##  N21    :22   NN014-1 : 6   1st Qu.:  0.7135   1st Qu.:0.0655   1st Qu.:0.001414   1st Qu.: 1.700  
##  N24    :14   NN014-3 : 6   Median :  3.5548   Median :0.1452   Median :0.004243   Median : 3.178  
##  N16    : 6   NN014-4 : 6   Mean   : 31.7564   Mean   :0.2557   Mean   :0.014621   Mean   : 6.463  
##  N30    : 6   NN053-11: 6   3rd Qu.: 11.9953   3rd Qu.:0.2854   3rd Qu.:0.011490   3rd Qu.: 6.145  
##  N31    : 6   NN053-12: 6   Max.   :516.7641   Max.   :1.1270   Max.   :0.260922   Max.   :89.815  
##  (Other):16   (Other) :60   NA's   :7
```

#### Duplicates

Listado de muestras **duplicadas** (n=63):

```r
## LISTADO de muestras DUPLICADAS
dupl.od <- Ab_freq[Ab_freq$Freq==4,]$Var1

std.dupl <- data.frame()
for (i in 1:length(dupl.od)) {
  std.dupl <- rbind(std.dupl,std.raw[std.raw$ID==dupl.od[i],])
}

as.character(dupl.od)
```

```
##  [1] "LL028-2"  "LL028-3"  "LL028-4"  "LL055-6"  "LL055-8"  "NN043-11" "NN062-7"  "ZG004-2"  "ZG009-2" 
## [10] "ZG020-1"  "ZG025-1"  "ZG026-4"  "ZG027-2"  "ZG030-1"  "ZG030-5"  "ZG031-3"  "ZG034-5"  "ZG035-4" 
## [19] "ZG038-3"  "ZG039-2"  "ZG043-2"  "ZG046-2"  "ZG046-3"  "ZG053-1"  "ZG055-1"  "ZG074-2"  "ZG084-4" 
## [28] "ZG086-2"  "ZG086-8"  "ZG088-3"  "ZG090-1"  "ZG092-3"  "ZG093-3"  "ZG093-8"  "ZG100-3"  "ZG100-4" 
## [37] "ZG101-10" "ZG101-7"  "ZG103-3"  "ZG103-4"  "ZG107-14" "ZG108-11" "ZG108-13" "ZG108-15" "ZG108-16"
## [46] "ZG116-6"  "ZG154-2"  "ZG156-2"  "ZG158-2"  "ZG158-4"  "ZG159-4"  "ZG159-6"  "ZG159-7"  "ZG168-4" 
## [55] "ZG174-7"  "ZG177-3"  "ZG177-4"  "ZG178-4"  "ZG178-5"  "ZG181-1"  "ZG189-3"  "ZG189-5"  "ZG190-4"
```

```r
dim(std.dupl)
```

```
## [1] 252   8
```

```r
#head(std.dupl, 12)
summary(std.dupl[,-c(3,6)])
```

```
##      Plate           ID         Ab.unit             mean.OD            sd.OD              cv.OD       
##  N28    :54   LL028-2 :  4   Min.   :    0.008   Min.   :0.04350   Min.   :0.000000   Min.   : 0.000  
##  N21    :28   LL028-3 :  4   1st Qu.:    0.574   1st Qu.:0.06175   1st Qu.:0.001414   1st Qu.: 1.194  
##  N24    :26   LL028-4 :  4   Median :    2.039   Median :0.09650   Median :0.003536   Median : 2.539  
##  N12    :20   LL055-6 :  4   Mean   :   98.403   Mean   :0.22072   Mean   :0.010598   Mean   : 5.896  
##  N16    :20   LL055-8 :  4   3rd Qu.:   12.340   3rd Qu.:0.30525   3rd Qu.:0.010607   3rd Qu.: 5.647  
##  N17    :18   NN043-11:  4   Max.   :15225.946   Max.   :1.21400   Max.   :0.197283   Max.   :85.492  
##  (Other):86   (Other) :228   NA's   :12          NA's   :1         NA's   :1          NA's   :1
```

### Unbiased selection

Unbiased selection criteria of Ab.units estimate among replicated reads of per sample:

+ **NOTE:** This assumes that no dilution to the original evaluated sample has been performed.
    + For triplicates, the closest value to the mean among replicates was selected
    + Reads with mean.OD with a %CV higher than 20% were rejected
    + For duplicates, the replicate with lower %CV was preferred.
    + If a selection was not acomplished, the last read performed was preferred.

This criteria have been implemented in a **function** called `Unbiased()`:


```r
#### FUNCION PARA LA APLICACION DEL CRITERIO
Unbiased <- function(repliMAT) {
  # LOOP: closest value to the mean
  z <- NULL
  # for each ID with replicates
  for(i in 1:length(levels(repliMAT$ID))){
    # for each Specie
    for (j in 1:length(levels(repliMAT$Specie))) {
      # extrae Ab.unit y %CV para cada especie
      x <- repliMAT[repliMAT$ID==levels(repliMAT$ID)[i] & 
                    repliMAT$Specie==levels(repliMAT$Specie)[j], c(4,8)]
      if (any(x$cv.OD>=20, na.rm = TRUE)) {
        # lecturas con %CV>=20% seran descartadas (independiente de OD>0.25)
        x <- x[-which(x$cv.OD>=20),]
      }
      y <- mean(x$Ab.unit, na.rm = TRUE)
      # elije cual de los valores es igual a la menor diferencia con la media aritmetica
      a <- x[which(min(abs(x$Ab.unit - y), na.rm = TRUE)==abs(x$Ab.unit - y)),]
    
      if (dim(a)[1]>1) {
        # para 2 lecturas, elegir la que tenga menor %CV
        a <- a[which(a$cv.OD==min(a$cv.OD, na.rm = TRUE)),]$Ab.unit
        if (length(a)>1) {
          # si no hay diferencias por %CV, entonces optar por la ultima lectura
          a <- a[2] 
        }
      } else {
        # si la dimension es igual a 1, entonces extraer las Ab.units
        a <- a$Ab.unit
      }
    
      z <- c(z,a)
    }
  }
  # LOOP para asignar eleccion a una tabla
  w <- NULL
  for(i in 1:length(z)){
    # evita IDENTIDAD entre Ab.Units de distintas muestras
    # seleccion por bloques
    v <- repliMAT[repliMAT$ID==levels(repliMAT$ID)[ceiling(i/2)],] 
    w <- rbind(w,subset(v, Ab.unit==z[i])) 
  }
  # output
  return(w)
}
####
```


The results will be shown as follows:

- First, the number of reads per samples: equal to 06 for triplicates and 04 for duplicates. 

- After criteria execution, the same table is presented with only 02 remaining reads per sample, 01 per specie.

- Finally, a table is presented with the samples and selected reads.

#### Triplicates

```r
std.trpl$ID <- factor(std.trpl$ID)
table(std.trpl$ID)
```

```
## 
##  LL055-7  NN014-1  NN014-3  NN014-4 NN053-11 NN053-12  NN053-9  NN062-6  NN062-8  ZG086-5  ZG092-1  ZG108-4 
##        6        6        6        6        6        6        6        6        6        6        6        6 
##  ZG155-6  ZG158-3  ZG174-4  ZG189-4 
##        6        6        6        6
```

```r
#std.trpl[std.trpl$ID==levels(std.trpl$ID)[1],]

w <- Unbiased(std.trpl)
# output
table(w$ID)
```

```
## 
##  LL055-7  NN014-1  NN014-3  NN014-4 NN053-11 NN053-12  NN053-9  NN062-6  NN062-8  ZG086-5  ZG092-1  ZG108-4 
##        2        2        2        2        2        2        2        2        2        2        2        2 
##  ZG155-6  ZG158-3  ZG174-4  ZG189-4 
##        2        2        2        2
```

```r
dim(w)
```

```
## [1] 32  8
```

```r
summary(w[,-c(3,6)])
```

```
##      Plate           ID        Ab.unit            mean.OD            sd.OD              cv.OD       
##  N28    :15   LL055-7 : 2   Min.   :  0.2749   Min.   :0.05100   Min.   :0.000000   Min.   : 0.000  
##  N21    : 7   NN014-1 : 2   1st Qu.:  0.5833   1st Qu.:0.06037   1st Qu.:0.002121   1st Qu.: 1.839  
##  N16    : 5   NN014-3 : 2   Median :  2.4488   Median :0.11525   Median :0.003182   Median : 3.129  
##  N31    : 3   NN014-4 : 2   Mean   : 27.5971   Mean   :0.24667   Mean   :0.006828   Mean   : 3.836  
##  N24    : 1   NN053-11: 2   3rd Qu.: 11.6826   3rd Qu.:0.28025   3rd Qu.:0.008839   3rd Qu.: 4.775  
##  N32    : 1   NN053-12: 2   Max.   :176.0337   Max.   :0.86650   Max.   :0.030406   Max.   :12.856  
##  (Other): 0   (Other) :20
```

```r
#w

std.trpl_new <- w

## CHECK
#length(z) == dim(w)[1]
#sum(table(w$ID)!=2)
```

#### Duplicates

Sample **ZG177-4** is initially removed due to both reads for **Pfal** with **NaN**. 
After criteria execution, the selected replicated read for **Pviv** is going to be added.


```r
std.dupl$ID <- factor(std.dupl$ID)
#table(std.dupl$ID)
extra <- std.dupl[std.dupl$ID=="ZG177-4",]
extra
```

```
## # A tibble: 4 x 8
##    Plate      ID   Type  Ab.unit mean.OD Specie       sd.OD     cv.OD
##   <fctr>  <fctr> <fctr>    <dbl>   <dbl> <fctr>       <dbl>     <dbl>
## 1    N12 ZG177-4    unk      NaN  0.0505   Pfal 0.003535534  7.001057
## 2    N12 ZG177-4    unk 1.386856  0.0855   Pviv 0.041719300 48.794503
## 3    N16 ZG177-4    unk      NaN  0.0500   Pfal 0.008485281 16.970563
## 4    N16 ZG177-4    unk 2.414783  0.1140   Pviv 0.007071068  6.202691
```

```r
std.dupl <- std.dupl[std.dupl$ID!="ZG177-4",] # RETIRAR debido a que tiene DOS lecturas con NAN para FALCIPARUM
std.dupl$ID <- factor(std.dupl$ID)
table(std.dupl$ID)
```

```
## 
##  LL028-2  LL028-3  LL028-4  LL055-6  LL055-8 NN043-11  NN062-7  ZG004-2  ZG009-2  ZG020-1  ZG025-1  ZG026-4 
##        4        4        4        4        4        4        4        4        4        4        4        4 
##  ZG027-2  ZG030-1  ZG030-5  ZG031-3  ZG034-5  ZG035-4  ZG038-3  ZG039-2  ZG043-2  ZG046-2  ZG046-3  ZG053-1 
##        4        4        4        4        4        4        4        4        4        4        4        4 
##  ZG055-1  ZG074-2  ZG084-4  ZG086-2  ZG086-8  ZG088-3  ZG090-1  ZG092-3  ZG093-3  ZG093-8  ZG100-3  ZG100-4 
##        4        4        4        4        4        4        4        4        4        4        4        4 
## ZG101-10  ZG101-7  ZG103-3  ZG103-4 ZG107-14 ZG108-11 ZG108-13 ZG108-15 ZG108-16  ZG116-6  ZG154-2  ZG156-2 
##        4        4        4        4        4        4        4        4        4        4        4        4 
##  ZG158-2  ZG158-4  ZG159-4  ZG159-6  ZG159-7  ZG168-4  ZG174-7  ZG177-3  ZG178-4  ZG178-5  ZG181-1  ZG189-3 
##        4        4        4        4        4        4        4        4        4        4        4        4 
##  ZG189-5  ZG190-4 
##        4        4
```

```r
#std.dupl[std.dupl$ID==levels(std.dupl$ID)[1],]

w <- Unbiased(std.dupl)
# output
table(w$ID)
```

```
## 
##  LL028-2  LL028-3  LL028-4  LL055-6  LL055-8 NN043-11  NN062-7  ZG004-2  ZG009-2  ZG020-1  ZG025-1  ZG026-4 
##        2        2        2        2        2        2        2        2        2        2        2        2 
##  ZG027-2  ZG030-1  ZG030-5  ZG031-3  ZG034-5  ZG035-4  ZG038-3  ZG039-2  ZG043-2  ZG046-2  ZG046-3  ZG053-1 
##        2        2        2        2        2        2        2        2        2        2        2        2 
##  ZG055-1  ZG074-2  ZG084-4  ZG086-2  ZG086-8  ZG088-3  ZG090-1  ZG092-3  ZG093-3  ZG093-8  ZG100-3  ZG100-4 
##        2        2        2        2        2        2        2        2        2        2        2        2 
## ZG101-10  ZG101-7  ZG103-3  ZG103-4 ZG107-14 ZG108-11 ZG108-13 ZG108-15 ZG108-16  ZG116-6  ZG154-2  ZG156-2 
##        2        2        2        2        2        2        2        2        2        2        2        2 
##  ZG158-2  ZG158-4  ZG159-4  ZG159-6  ZG159-7  ZG168-4  ZG174-7  ZG177-3  ZG178-4  ZG178-5  ZG181-1  ZG189-3 
##        2        2        2        2        2        2        2        2        2        2        2        2 
##  ZG189-5  ZG190-4 
##        2        2
```

```r
dim(w)
```

```
## [1] 124   8
```

```r
summary(w[,-c(3,6)])
```

```
##      Plate           ID         Ab.unit            mean.OD            sd.OD               cv.OD        
##  N28    :24   LL028-2 :  2   Min.   :  0.0078   Min.   :0.04350   Min.   :0.0000000   Min.   : 0.0000  
##  N21    :16   LL028-3 :  2   1st Qu.:  0.5736   1st Qu.:0.05937   1st Qu.:0.0007071   1st Qu.: 0.7198  
##  N24    :13   LL028-4 :  2   Median :  1.6009   Median :0.09650   Median :0.0024749   Median : 2.1674  
##  N17    :11   LL055-6 :  2   Mean   : 34.1687   Mean   :0.22335   Mean   :0.0060788   Mean   : 2.9946  
##  N16    :10   LL055-8 :  2   3rd Qu.: 10.6684   3rd Qu.:0.30288   3rd Qu.:0.0058336   3rd Qu.: 4.1070  
##  N20    :10   NN043-11:  2   Max.   :406.2099   Max.   :1.21400   Max.   :0.0692965   Max.   :19.8374  
##  (Other):40   (Other) :112
```

```r
#w
std.dupl_new <- w

## CHECK:
#length(z) == dim(w)[1]
#sum(table(w$ID)!=2)
#

# ADD EXTRA "ZG177-4" only VIVAX
std.dupl_new <- rbind(std.dupl_new,extra[4,]) # +1
#dim(std.raw)
```

### Filtering

- Aim:
    + Retrieve of all replicated reads: triplicates, duplicates and NaN.
    + Addition of selected reads among triplicates and duplicates.
    + Visualization of mean.OD-Ab.unit non-linearity and whole %CV before and after the procedure

#### Plot previous to filtering


```r
summary(std.raw[,-c(1:2)])
```

```
##   Type         Ab.unit             mean.OD        Specie         sd.OD              cv.OD       
##  unk:2128   Min.   :    0.003   Min.   :0.0420   Pfal:1064   Min.   :0.000000   Min.   : 0.000  
##             1st Qu.:    0.593   1st Qu.:0.0620   Pviv:1064   1st Qu.:0.001414   1st Qu.: 1.221  
##             Median :    1.994   Median :0.0975               Median :0.002828   Median : 2.599  
##             Mean   :   28.841   Mean   :0.1886               Mean   :0.007124   Mean   : 4.241  
##             3rd Qu.:    8.379   3rd Qu.:0.2250               3rd Qu.:0.007071   3rd Qu.: 5.105  
##             Max.   :15225.946   Max.   :1.3875               Max.   :0.260922   Max.   :89.815  
##             NA's   :73          NA's   :2                    NA's   :2          NA's   :2
```

```r
par(mfrow=c(1,3))
### NON LINEAR RELATIONSHIP
plot(mean.OD ~ Ab.unit,std.raw,
     ylim = c(0,1.5), 
     xlim = c(0,5e3),
     main="OD-Ab.unit Non-linearity", pch=4) 
with(subset(std.raw, Specie=="Pviv"), 
     points(Ab.unit, mean.OD, col="blue", pch=4))
with(subset(std.raw, Specie=="Pfal"), 
     points(Ab.unit, mean.OD, col="red", pch=4))
legend("topright", c("P.vivax","P.falcip"), #, "C+/-", "blank"
       col = c( "blue", "red"), #, "black", "black"
       pch = c(4, 4), #, NA, NA #lwd= c(1,1),
       #lty = c(NA, NA), #, 3, 2
       cex = 0.8,
       inset = .01#, merge = TRUE
)
plot(mean.OD ~ log(Ab.unit),std.raw,
     ylim = c(0,1.5), 
     xlim = c(-6,10),
     main="OD-Ab.unit Non-linearity", pch=4) 
with(subset(std.raw, Specie=="Pviv"), 
     points(log(Ab.unit), mean.OD, col="blue", pch=4))
with(subset(std.raw, Specie=="Pfal"), 
     points(log(Ab.unit), mean.OD, col="red", pch=4))
legend("topleft", c("P.vivax","P.falcip"), #, "C+/-", "blank"
       col = c( "blue", "red"), #, "black", "black"
       pch = c(4, 4), #, NA, NA #lwd= c(1,1),
       #lty = c(NA, NA), #, 3, 2
       cex = 0.8,
       inset = .01#, merge = TRUE
)
### %CV
plot(cv.OD ~ mean.OD,std.raw, 
     ylim=c(0,100), xlim=c(0,1.5),
     xlab="mean OD 450nm of duplicates",
     ylab="%CV of duplicates",
     main="Percentage Coefficient of Variation")
abline(h=20, v=0.25, lty=2, col=2)
```

<img src="01-complete_files/figure-html/prefil-1.png" style="display: block; margin: auto;" />

```r
## SD
#plot(sd.OD ~ mean.OD,std.raw,
#     ylim = c(0,0.27),
#     main="Heteroskedasticity?") 
```

#### Data filtering


```r
#mue # TOTAL MUESTRAS EVALUADAS
#rep # TOTAL INICIAL
#esp # TOTAL ESPERADO
```

**969 is the total amount of evaluated samples**. For each one, 02 read were made: 
01 per specie. Before any filtering, the total amount of reads goes up to 
**2128 including replicates**. After filtering is applied, the **expected amount 
of left reads is 1938**.

A **function** called `checkIN()` would allow to evaluate the retrieving of replicated 
reads and addition of selected ones:


```r
#CHECK FUNCTION
checkIN <- function(a) {
  evalREP <- NULL
  for (i in 1:6) {
    evalREP <- c(evalREP, sum(table(a$ID)==i))
  }
  evalREP
}
```

First, replicates are retrieve:

```r
# a COPY is used throughout FILTERING
std.fil <- std.raw

## RETRIEVE
dim(std.fil)[1]
```

```
## [1] 2128
```

```r
checkIN(std.fil)
```

```
## [1]   0 890   0  63   0  16
```

```r
# triplicates
for (i in 1:length(as.character(trpl.od))) {
  std.fil <- std.fil[std.fil$ID!=as.character(trpl.od)[i],] # -96
}
checkIN(std.fil)
```

```
## [1]   0 890   0  63   0   0
```

```r
#dim(std.fil)[1]

# duplicates
for (i in 1:length(as.character(dupl.od))) {
  std.fil <- std.fil[std.fil$ID!=as.character(dupl.od)[i],] # -256
}
checkIN(std.fil)
```

```
## [1]   0 890   0   0   0   0
```

```r
#dim(std.fil)[1]

# END of retrieving

## NA
#std.fil <- std.fil[is.na(std.fil$Ab.unit)!=TRUE,] # -73
#checkIN(std.fil)
##
#dim(std.fil)[1]
#
```

Then, selected reads added:

```r
## ADDITION to `std.nty` as a filtered and tidy df

# ADD NEW TRIPLICATES
std.nty <- rbind(std.fil,std.trpl_new) # + 32 = 16x2
checkIN(std.nty)
```

```
## [1]   0 906   0   0   0   0
```

```r
#dim(std.fil)

# ADD NEW DUPLICATES
std.nty <- rbind(std.nty,std.dupl_new) # +128 + 1 = 64x2 + 1
checkIN(std.nty)
```

```
## [1]   1 968   0   0   0   0
```

```r
dim(std.nty)[1]
```

```
## [1] 1937
```

```r
# END of addition
```

#### Filtering evaluation

After filtering, we obtain 

- **969 samples** of **969 expected**, 
- giving a **total of 1937 reads** on the data frame.

As seen in the last `checkIN`, 

- **1 samples** have only one read because of **Ab.unit NaN**. 
- For more detail, go to [Appendix](#appendix-residual-na)


#### Plot after filtering

```r
summary(std.nty[,-c(1:2)])
```

```
##   Type         Ab.unit             mean.OD         Specie        sd.OD              cv.OD       
##  unk:1937   Min.   :   0.0032   Min.   :0.04200   Pfal:968   Min.   :0.000000   Min.   : 0.000  
##             1st Qu.:   0.5892   1st Qu.:0.06150   Pviv:969   1st Qu.:0.001414   1st Qu.: 1.209  
##             Median :   1.8949   Median :0.09675              Median :0.002828   Median : 2.565  
##             Mean   :  20.1530   Mean   :0.18431              Mean   :0.006230   Mean   : 3.831  
##             3rd Qu.:   8.1394   3rd Qu.:0.21750              3rd Qu.:0.007071   3rd Qu.: 4.962  
##             Max.   :1758.8289   Max.   :1.38750              Max.   :0.140007   Max.   :57.823  
##             NA's   :54          NA's   :1                    NA's   :1          NA's   :1
```

```r
par(mfrow=c(1,3))
### NON LINEAR RELATIONSHIP
plot(mean.OD ~ Ab.unit,std.nty,
     ylim = c(0,1.5), 
     xlim = c(0,5e3),
     main="OD-Ab.unit Non-linearity", pch=4) 
with(subset(std.nty, Specie=="Pviv"), 
     points(Ab.unit, mean.OD, col="blue", pch=4))
with(subset(std.nty, Specie=="Pfal"), 
     points(Ab.unit, mean.OD, col="red", pch=4))
legend("topright", c("P.vivax","P.falcip"), #, "C+/-", "blank"
       col = c( "blue", "red"), #, "black", "black"
       pch = c(4, 4), #, NA, NA #lwd= c(1,1),
       #lty = c(NA, NA), #, 3, 2
       cex = 0.8,
       inset = .01#, merge = TRUE
)
plot(mean.OD ~ log(Ab.unit),std.nty,
     ylim = c(0,1.5), 
     xlim = c(-6,10),
     main="OD-Ab.unit Non-linearity", pch=4) 
with(subset(std.nty, Specie=="Pviv"), 
     points(log(Ab.unit), mean.OD, col="blue", pch=4))
with(subset(std.nty, Specie=="Pfal"), 
     points(log(Ab.unit), mean.OD, col="red", pch=4))
legend("topleft", c("P.vivax","P.falcip"), #, "C+/-", "blank"
       col = c( "blue", "red"), #, "black", "black"
       pch = c(4, 4), #, NA, NA #lwd= c(1,1),
       #lty = c(NA, NA), #, 3, 2
       cex = 0.8,
       inset = .01#, merge = TRUE
)
### %CV
plot(cv.OD ~ mean.OD,std.nty, 
     ylim=c(0,100), xlim=c(0,1.5),
     xlab="mean OD 450nm of duplicates",
     ylab="%CV of duplicates",
     main="Percentage Coefficient of Variation")
abline(h=20, v=0.25, lty=2, col=2)
```

<img src="01-complete_files/figure-html/posfil-1.png" style="display: block; margin: auto;" />

```r
## SD
#plot(sd.OD ~ mean.OD,std.nty,
#     ylim = c(0,0.27),
#     main="Heteroskedasticity?") 
```



### Distributions

#### ggplot


```r
#library(ggplot2)
#library(Rmisc)        #multiploting ggplots
a <- ggplot(std.nty, aes(x=mean.OD, fill=Specie)) + 
  geom_histogram(alpha=.5, position = "identity") + 
  labs(title="OD in linear scale")
b <- ggplot(std.nty, aes(x=Ab.unit, fill=Specie)) + 
  geom_histogram(alpha=.5, position = "identity") + 
  labs(title="AU in linear scale")

c <- ggplot(std.nty, aes(x=log(mean.OD), fill=Specie)) + 
  geom_histogram(alpha=.5, position = "identity") + 
  labs(title="OD in log scale")
d <- ggplot(std.nty, aes(x=log(Ab.unit), fill=Specie)) + 
  geom_histogram(alpha=.5, position = "identity") + 
  labs(title="AU in log scale")

Rmisc::multiplot(a,c, 
                 b,d,
                 cols = 2)
```

<img src="01-complete_files/figure-html/ggplot-1.png" style="display: block; margin: auto;" />

#### base

##### Linear scale


```r
### OD distribution
par(mfrow=c(2,3))

my <- max(hist(std.nty$mean.OD, plot = F)$counts, na.rm=TRUE)

hist(std.nty$mean.OD, xlim = c(0,1.4), 
     main = "mean.OD Histogram", xlab = "mean.OD")

hist(std.nty[std.nty$Specie=="Pviv",]$mean.OD, ylim = c(0,my), xlim = c(0,1.4), 
     main = "P.vivax", xlab = "mean.OD")

hist(std.nty[std.nty$Specie=="Pfal",]$mean.OD, ylim = c(0,my), xlim = c(0,1.4),
     main = "P.falciparum", xlab = "mean.OD")



### Ab distribution
#par(mfrow=c(1,3))

my <- max(hist(std.nty$Ab.unit, plot = F)$counts, na.rm=TRUE)

hist(std.nty$Ab.unit, 
     main = "Ab.units Histogram", xlab = "Ab.units")#, xlim = c(0,15000)

hist(std.nty[std.nty$Specie=="Pviv",]$Ab.unit, ylim = c(0,my),
     main = "P.vivax", xlab = "Ab.units")#, xlim = c(0,15000)

hist(std.nty[std.nty$Specie=="Pfal",]$Ab.unit, ylim = c(0,my),
     main = "P.falciparum", xlab = "Ab.units")#, xlim = c(0,15000)
```

<img src="01-complete_files/figure-html/linplt-1.png" style="display: block; margin: auto;" />

##### Log scale


```r
par(mfrow=c(2,3))

my <- max(hist(log(std.nty$mean.OD), plot = F)$counts, na.rm=TRUE)

hist(log(std.nty$mean.OD), xlim = c(-3.5,1),
     main = "mean.OD Histogram", xlab = "log(mean.OD)")
hist(log(std.nty[std.nty$Specie=="Pviv",]$mean.OD), ylim = c(0,my), xlim = c(-3.5,1),
     main = "P.vivax", xlab = "log(mean.OD)")
hist(log(std.nty[std.nty$Specie=="Pfal",]$mean.OD), ylim = c(0,my), xlim = c(-3.5,1),
     main = "P.falciparum", xlab = "log(mean.OD)")

############
#par(mfrow=c(1,3))

my <- max(hist(log(std.nty$Ab.unit), plot = F)$counts, na.rm=TRUE)

hist(log(std.nty$Ab.unit), xlim = c(-6,10),
     main = "Ab.units Histogram", xlab = "log(Ab.units)")
hist(log(std.nty[std.nty$Specie=="Pviv",]$Ab.unit), ylim = c(0,my), xlim = c(-6,10),
     main = "P.vivax", xlab = "log(Ab.units)")
hist(log(std.nty[std.nty$Specie=="Pfal",]$Ab.unit), ylim = c(0,my), xlim = c(-6,10),
     main = "P.falciparum", xlab = "log(Ab.units)")
```

<img src="01-complete_files/figure-html/logplt-1.png" style="display: block; margin: auto;" />


## Output

### tidy format:

+ 01 row, 01 observation, 01 read


```r
#summary(std.nty[,-c(1:2)])
str(std.nty)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1937 obs. of  8 variables:
##  $ Plate  : Factor w/ 28 levels "N11","N12","N15",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ID     : Factor w/ 969 levels "LL001-2","LL001-3",..: 529 531 533 534 536 537 538 540 563 564 ...
##  $ Type   : Factor w/ 1 level "unk": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Ab.unit: num  1.169 0.85 0.885 0.815 0.545 ...
##  $ mean.OD: num  0.06 0.0555 0.056 0.055 0.051 0.054 0.0495 0.081 0.042 0.132 ...
##  $ Specie : Factor w/ 2 levels "Pfal","Pviv": 1 1 1 1 1 1 1 1 1 1 ...
##  $ sd.OD  : num  0 0.00354 0.00424 0.00566 0.00424 ...
##  $ cv.OD  : num  0 6.37 7.58 10.29 8.32 ...
```

```r
summary(std.nty)
```

```
##      Plate             ID        Type         Ab.unit             mean.OD         Specie   
##  N27    :  80   LL001-2 :   2   unk:1937   Min.   :   0.0032   Min.   :0.04200   Pfal:968  
##  N29    :  80   LL001-3 :   2              1st Qu.:   0.5892   1st Qu.:0.06150   Pviv:969  
##  N33    :  80   LL003-1 :   2              Median :   1.8949   Median :0.09675             
##  N34    :  80   LL003-10:   2              Mean   :  20.1530   Mean   :0.18431             
##  N35    :  80   LL003-3 :   2              3rd Qu.:   8.1394   3rd Qu.:0.21750             
##  N36    :  80   LL003-4 :   2              Max.   :1758.8289   Max.   :1.38750             
##  (Other):1457   (Other) :1925              NA's   :54          NA's   :1                   
##      sd.OD              cv.OD       
##  Min.   :0.000000   Min.   : 0.000  
##  1st Qu.:0.001414   1st Qu.: 1.209  
##  Median :0.002828   Median : 2.565  
##  Mean   :0.006230   Mean   : 3.831  
##  3rd Qu.:0.007071   3rd Qu.: 4.962  
##  Max.   :0.140007   Max.   :57.823  
##  NA's   :1          NA's   :1
```

### untidy format:

+ 01 row, 01 patient, 02 observations, 02 reads
+ done in a tidy-way.


```r
std.uty_ab <- std.nty %>% 
  tbl_df() %>% 
  select(ID, Ab.unit, Specie) %>% 
  spread(key = Specie, value = Ab.unit) %>% 
  dplyr::rename(Ab.unit_Pfal=Pfal, Ab.unit_Pviv=Pviv)

std.uty_od <- std.nty %>% 
  tbl_df() %>% 
  select(ID, mean.OD, Specie) %>% 
  spread(key = Specie, value = mean.OD) %>% 
  dplyr::rename(mean.OD_Pfal=Pfal, mean.OD_Pviv=Pviv)

std.uty <- full_join(std.uty_ab, std.uty_od, by="ID")
str(std.uty)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	969 obs. of  5 variables:
##  $ ID          : Factor w/ 969 levels "LL001-2","LL001-3",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Ab.unit_Pfal: num  0.757 0.893 32.481 11.794 0.245 ...
##  $ Ab.unit_Pviv: num  7.48 1.08 1.16 16.1 9.25 ...
##  $ mean.OD_Pfal: num  0.0585 0.0605 0.2975 0.158 0.052 ...
##  $ mean.OD_Pviv: num  0.193 0.079 0.081 0.291 0.298 ...
```

```r
summary(std.uty)
```

```
##         ID       Ab.unit_Pfal        Ab.unit_Pviv       mean.OD_Pfal      mean.OD_Pviv   
##  LL001-2 :  1   Min.   :   0.0032   Min.   :  0.0078   Min.   :0.04200   Min.   :0.0425  
##  LL001-3 :  1   1st Qu.:   0.4473   1st Qu.:  0.9310   1st Qu.:0.05600   1st Qu.:0.0810  
##  LL003-1 :  1   Median :   1.1093   Median :  3.1413   Median :0.07025   Median :0.1502  
##  LL003-10:  1   Mean   :  13.9245   Mean   : 26.1670   Mean   :0.12078   Mean   :0.2478  
##  LL003-3 :  1   3rd Qu.:   4.4097   3rd Qu.: 13.5217   3rd Qu.:0.11850   3rd Qu.:0.3445  
##  LL003-4 :  1   Max.   :1758.8289   Max.   :817.2931   Max.   :0.89250   Max.   :1.3875  
##  (Other) :963   NA's   :44          NA's   :11         NA's   :1         NA's   :1
```

### save .rds

```r
saveRDS(std.nty, "data/02-dtfilter-nty.rds")
saveRDS(std.uty, "data/02-dtfilter-uty.rds")

#saveRDS(std.nan, "data/02-out-of-bounce.rds")

#saveRDS(std.trpl, "data/02-triplicates.rds")
#saveRDS(std.dupl, "data/02-duplicates.rds")
#saveRDS(std.trpl_new, "data/02-triplicates-filter.rds")
#saveRDS(std.dupl_new, "data/02-duplicates-filter.rds")
```

***

## APPENDIX: residual NA

**NOTE 1:** 

- Only **4 samples** with NaN in both reads. 
- All `mean.OD` are below the respective lower limit
- Sample **ZG177-4** was specially evaluated in the `Duplicates` section of 
[UNBIASED selection](#unbiased-selection):


```r
std.nan_both <- std.nan %>% 
  count("ID") %>% 
  filter(freq > 1) %>% 
  select(ID) %>% 
  inner_join(std.raw %>% 
               select(Plate, ID, Ab.unit, mean.OD, Specie)) 
```

```
## Joining, by = "ID"
```

```r
std.nan_both
```

```
##         ID Plate  Ab.unit mean.OD Specie
## 1  PA067-8   N30      NaN  0.0905   Pfal
## 2  PA067-8   N30      NaN  0.1070   Pviv
## 3  ZG119-6   N24      NaN  0.0710   Pfal
## 4  ZG119-6   N24      NaN  0.0585   Pviv
## 5  ZG133-6   N24      NaN  0.0620   Pfal
## 6  ZG133-6   N24      NaN  0.0590   Pviv
## 7  ZG177-4   N12      NaN  0.0505   Pfal
## 8  ZG177-4   N12 1.386856  0.0855   Pviv
## 9  ZG177-4   N16      NaN  0.0500   Pfal
## 10 ZG177-4   N16 2.414783  0.1140   Pviv
```


```r
# NaN in both PFAL and PVIV ----> PRE filtering
std.nan_both %>% 
  left_join(std.out) #%>% 
```

```
## Joining, by = c("ID", "Plate", "Ab.unit", "mean.OD", "Specie")
```

```
##         ID Plate  Ab.unit mean.OD Specie par.Specie          c         d out.low out.upp
## 1  PA067-8   N30      NaN  0.0905   Pfal       Pfal 0.13073847 0.9127062    TRUE   FALSE
## 2  PA067-8   N30      NaN  0.1070   Pviv       Pviv 0.10971110 0.9341066    TRUE   FALSE
## 3  ZG119-6   N24      NaN  0.0710   Pfal       Pfal 0.09928469 0.6803841    TRUE   FALSE
## 4  ZG119-6   N24      NaN  0.0585   Pviv       Pviv 0.05912138 0.8036373    TRUE   FALSE
## 5  ZG133-6   N24      NaN  0.0620   Pfal       Pfal 0.09928469 0.6803841    TRUE   FALSE
## 6  ZG133-6   N24      NaN  0.0590   Pviv       Pviv 0.05912138 0.8036373    TRUE   FALSE
## 7  ZG177-4   N12      NaN  0.0505   Pfal       Pfal 0.05180474 1.4290674    TRUE   FALSE
## 8  ZG177-4   N12 1.386856  0.0855   Pviv       <NA>         NA        NA      NA      NA
## 9  ZG177-4   N16      NaN  0.0500   Pfal       Pfal 0.05079147 0.8461171    TRUE   FALSE
## 10 ZG177-4   N16 2.414783  0.1140   Pviv       <NA>         NA        NA      NA      NA
```

```r
  #count("ID")
```

**NOTE 2:** 

On the other hand, only **2** samples have 
`mean.OD` as `NA`:

```r
## muestras con OD con valor NA?
std.raw %>% filter(mean.OD=="NaN") ## ZG181 ZG182
```

```
## # A tibble: 2 x 8
##    Plate      ID   Type Ab.unit mean.OD Specie sd.OD cv.OD
##   <fctr>  <fctr> <fctr>   <dbl>   <dbl> <fctr> <dbl> <dbl>
## 1    N15 ZG181-1    unk     NaN     NaN   Pfal    NA    NA
## 2    N15 ZG182-1    unk     NaN     NaN   Pviv    NA    NA
```

Both samples belong to **N15**. The problem here was that in this Template, 
one sample was evaluates in the Pf plate, and the other one in the Pv plate: 

- **ZG181-1**, since it is a replicate, it have reads for both species.

```r
std.raw[std.raw$ID=="ZG181-1",]
```

```
## # A tibble: 4 x 8
##    Plate      ID   Type  Ab.unit mean.OD Specie       sd.OD    cv.OD
##   <fctr>  <fctr> <fctr>    <dbl>   <dbl> <fctr>       <dbl>    <dbl>
## 1    N12 ZG181-1    unk 1.350149   0.077   Pfal 0.002828427 3.673282
## 2    N12 ZG181-1    unk 2.292322   0.106   Pviv 0.001414214 1.334164
## 3    N15 ZG181-1    unk      NaN     NaN   Pfal          NA       NA
## 4    N15 ZG181-1    unk 3.821821   0.166   Pviv 0.005656854 3.407744
```

- However, **ZG182-1** was only evaluated once, for one ELISA plate and for one specie.

```r
std.raw[std.raw$ID=="ZG182-1",]
```

```
## # A tibble: 2 x 8
##    Plate      ID   Type  Ab.unit mean.OD Specie       sd.OD    cv.OD
##   <fctr>  <fctr> <fctr>    <dbl>   <dbl> <fctr>       <dbl>    <dbl>
## 1    N15 ZG182-1    unk 2.198322   0.083   Pfal 0.001414214 1.703872
## 2    N15 ZG182-1    unk      NaN     NaN   Pviv          NA       NA
```

- CHECK IF **ZG182-1** should be corrected to **ZG181-1**.
	+ Missing reads are complementary between ID's.


## APPENDIX: Replicates

### PRE selection

#### pre triplicates

```r
as.data.frame(std.trpl %>%
            format(scientific=FALSE, digits=2))
```

```
##                           std.trpl %>% format(scientific = FALSE, digits = 2)
## 1                                                          # A tibble: 96 x 8
## 2      Plate      ID   Type     Ab.unit mean.OD Specie        sd.OD     cv.OD
## 3     <fctr>  <fctr> <fctr>       <dbl>   <dbl> <fctr>        <dbl>     <dbl>
## 4   1    N21 LL055-7    unk  83.7716797  0.5445   Pfal 0.0091923882 1.6882256
## 5   2    N21 LL055-7    unk   0.9472349  0.0995   Pviv 0.0021213203 2.1319802
## 6   3    N28 LL055-7    unk 130.8580380  0.7815   Pfal 0.0021213203 0.2714421
## 7   4    N28 LL055-7    unk   4.8829795  0.2470   Pviv 0.0113137085 4.5804488
## 8   5    N37 LL055-7    unk 157.6120101  0.7070   Pfal 0.0452548340 6.4009666
## 9   6    N37 LL055-7    unk   6.7486308  0.2830   Pviv 0.0084852814 2.9983326
## 10  7    N21 NN014-1    unk  12.9858827  0.2925   Pfal 0.0007071068 0.2417459
## 11  8    N21 NN014-1    unk   0.5243095  0.0795   Pviv 0.0021213203 2.6683275
## 12  9    N28 NN014-1    unk   9.9220246  0.2195   Pfal 0.0007071068 0.3221443
## 13 10    N28 NN014-1    unk   0.8027611  0.0945   Pviv 0.0021213203 2.2447834
## 14                                                    # ... with 86 more rows
```

#### pre duplicates

```r
as.data.frame(std.dupl %>%
            format(scientific=FALSE, digits=2))
```

```
##                         std.dupl %>% format(scientific = FALSE, digits = 2)
## 1                                                       # A tibble: 248 x 8
## 2      Plate      ID   Type    Ab.unit mean.OD Specie        sd.OD    cv.OD
## 3     <fctr>  <fctr> <fctr>      <dbl>   <dbl> <fctr>        <dbl>    <dbl>
## 4   1    N21 LL028-2    unk 0.50721006  0.0605   Pfal 0.0035355339 5.843858
## 5   2    N21 LL028-2    unk 0.78100800  0.0920   Pviv 0.0000000000 0.000000
## 6   3    N28 LL028-2    unk 0.68000628  0.0570   Pfal 0.0000000000 0.000000
## 7   4    N28 LL028-2    unk 0.77281311  0.0930   Pviv 0.0014142136 1.520660
## 8   5    N21 LL028-3    unk 0.12534427  0.0480   Pfal 0.0014142136 2.946278
## 9   6    N21 LL028-3    unk 0.04752802  0.0505   Pviv 0.0007071068 1.400211
## 10  7    N28 LL028-3    unk 0.53498540  0.0540   Pfal 0.0028284271 5.237828
## 11  8    N28 LL028-3    unk 0.04094118  0.0515   Pviv 0.0007071068 1.373023
## 12  9    N21 LL028-4    unk 0.24461861  0.0520   Pfal 0.0000000000 0.000000
## 13 10    N21 LL028-4    unk 0.05994405  0.0515   Pviv 0.0021213203 4.119069
## 14                                                 # ... with 238 more rows
```


### POST selection

#### pos triplicates

```r
as.data.frame(std.trpl_new %>%
            format(scientific=FALSE, digits=2))
```

```
##                        std.trpl_new %>% format(scientific = FALSE, digits = 2)
## 1                                                           # A tibble: 32 x 8
## 2      Plate       ID   Type     Ab.unit mean.OD Specie       sd.OD      cv.OD
## 3     <fctr>   <fctr> <fctr>       <dbl>   <dbl> <fctr>       <dbl>      <dbl>
## 4   1    N28  LL055-7    unk 130.8580380  0.7815   Pfal 0.002121320  0.2714421
## 5   2    N28  LL055-7    unk   4.8829795  0.2470   Pviv 0.011313708  4.5804488
## 6   3    N31  NN014-1    unk  11.5783911  0.2630   Pfal 0.009899495  3.7640665
## 7   4    N28  NN014-1    unk   0.8027611  0.0945   Pviv 0.002121320  2.2447834
## 8   5    N21  NN014-3    unk  11.9953065  0.2810   Pfal 0.008485281  3.0196731
## 9   6    N31  NN014-3    unk   6.0716815  0.2800   Pviv 0.004242641  1.5152288
## 10  7    N31  NN014-4    unk   0.8184847  0.0635   Pfal 0.007778175 12.2490938
## 11  8    N28  NN014-4    unk   0.3580807  0.0710   Pviv 0.002828427  3.9837002
## 12  9    N21 NN053-11    unk   0.3053697  0.0540   Pfal 0.002828427  5.2378280
## 13 10    N28 NN053-11    unk  32.1989438  0.6100   Pviv 0.011313708  1.8547063
## 14                                                     # ... with 22 more rows
```

#### pos duplicates

```r
as.data.frame(std.dupl_new %>%
            format(scientific=FALSE, digits=2))
```

```
##                       std.dupl_new %>% format(scientific = FALSE, digits = 2)
## 1                                                         # A tibble: 125 x 8
## 2      Plate      ID   Type     Ab.unit mean.OD Specie        sd.OD     cv.OD
## 3     <fctr>  <fctr> <fctr>       <dbl>   <dbl> <fctr>        <dbl>     <dbl>
## 4   1    N28 LL028-2    unk  0.68000628  0.0570   Pfal 0.0000000000 0.0000000
## 5   2    N21 LL028-2    unk  0.78100800  0.0920   Pviv 0.0000000000 0.0000000
## 6   3    N21 LL028-3    unk  0.12534427  0.0480   Pfal 0.0014142136 2.9462783
## 7   4    N28 LL028-3    unk  0.04094118  0.0515   Pviv 0.0007071068 1.3730229
## 8   5    N21 LL028-4    unk  0.24461861  0.0520   Pfal 0.0000000000 0.0000000
## 9   6    N28 LL028-4    unk  0.16289054  0.0595   Pviv 0.0007071068 1.1884148
## 10  7    N21 LL055-6    unk  1.77016052  0.0975   Pfal 0.0007071068 0.7252377
## 11  8    N28 LL055-6    unk  2.38769205  0.1635   Pviv 0.0049497475 3.0273685
## 12  9    N21 LL055-8    unk  3.08664349  0.1310   Pfal 0.0028284271 2.1591047
## 13 10    N28 LL055-8    unk 10.14829184  0.3705   Pviv 0.0106066017 2.8627805
## 14                                                   # ... with 115 more rows
```


<!--## References-->

<!--chapter:end:02-dtfilter.Rmd-->

# COVARIATE UNIFICATION {#covar}

## Aim

* Principal: 
    - Generate a unified covariate database for the following statistical analysis.

* Secondary: 
    - Generate a Quality Control report comparing UNAP and NAMR measurements on microscopy and PCR.

## Results

- __non-stardardized DB names__ were cronologically standardized. [here](#bashcov)
- __prior and update__ information was summarized in a __diagram__. [here](#priorupgraph)
- `unap0` was created as a **merged** DB of covariates. [here](#merge-covariates)
- `unap0` was **merged** with both `std.nty` and `std.uty` DB's of **mean.OD** and **Ab.units** 
per specie previously obtained. [here](#merge-ab-units)
    + At the moment, **`std.uty.cov`** is the most convenient dataset.
- **Consensus variable** was generated for `micro` and `pcr` giving preference to **NAMR** measurement. [here](#generate-consensus)
- A **QC report** shows __97.5% similarity__ for `microscopy` and __73.4%__ for `pcr`. [here](#qc-report)
    + Percentages increases assuming independent observations with regard to the `specie`. [here](#tidyqcrev)

## To Do

- In order to keep using `std.nty` as a tidy dataset 
    + A [relational data](http://r4ds.had.co.nz/relational-data.html) framework 
    could be used.
    + The **Serological dataset** and **Covariate/Epidemiological** dataset could 
    keep to be separated but **related**, instead.
    + More details in [APPENDIX](#appendix-tidy-vs-untidy)
- re-evaluate 01 patient 02 id after `unap6`
    + evaluation in [APPENDIX](#appendix-id-errors)



## Dependencies

The required R packages for this analysis are:

+ `tidyverse` [@tidyverse]
+ `haven` [@haven]
+ `readxl` [@readxl]
+ `forcats` [@forcats]
+ `stringr` [@stringr]


```r
##essential
library(XLConnect)    # load EXCEL workbooks
library(drc)          # Dose-Response modeling
##accesory
library("DiagrammeR") # method Flowchart
library("knitr")      # To display nice tables
library("tidyverse")
#ggplot2, tibble, tidyr, readr, purr, dplyr
##extra
library("Rmisc")      # multiplot ggplots
library("haven")      # import STATA df
library("readxl")     # import EXCEL df
library("forcats")    # factor vectors
library("stringr")    # for strings
library("viridis")    # color visualization
```

## Input

```r
std.nty <- readRDS("data/02-dtfilter-nty.rds")
std.uty <- readRDS("data/02-dtfilter-uty.rds")
```

- total amount of observations **1937** in `std.nty`.
- total amount of evaluated patients **969** in `std.nty`.
- giving **969** available samples in `std.uty`.

## Covariates {#bashcov}

The original non-standardized names of the **06** datasets with covariates are:


```bash
ls data-raw/ | grep -v "Template" | grep -v "zip" | grep -v "do"
```

```
## Base datos epidemiologicos 2015.xlsx
## QC results Zungarococha.xlsx
## RESULTADOS_LABORATORIO_PRIMER BARRIDO (JUL- AGO 2015).xlsx
## UNAPdatabase20Sept16.dta
## UNAPdatabase20Sept16-pre.dta
## Zungarococha_database1_3Oct2016.csv
```

In order to follow standardize names, the files we copied and renamed:


```bash
cp data-raw/UNAPdatabase20Sept16-pre.dta \
data/UNAP-1-DB-20sep2016.dta

cp data-raw/Zungarococha_database1_3Oct2016.csv \
data/UNAP-2-DB-03oct2016.csv

cp data-raw/UNAPdatabase20Sept16.dta \
data/UNAP-3-DB-02nov2016.dta

cp data-raw/RESULTADOS_LABORATORIO_PRIMER\ BARRIDO\ \(JUL-\ AGO\ 2015\).xlsx \
data/UNAP-4-UP-16ene2017.xlsx

cp data-raw/QC\ results\ Zungarococha.xlsx \
data/UNAP-5-QC-08feb2017.xlsx

cp data-raw/Base\ datos\ epidemiologicos\ 2015.xlsx \
data/UNAP-6-EPI-08feb2017.xlsx
```

New names include:

- order of appearance throughout the project
- relevand info: DB, UP, QC or EPI
- date of registered last edition, or
- receipt date


```bash
ls -sh data/UNAP-*
```

```
##  88K data/UNAP-1-DB-20sep2016.dta
##  64K data/UNAP-2-DB-03oct2016.csv
## 108K data/UNAP-3-DB-02nov2016.dta
##  72K data/UNAP-4-UP-16ene2017.xlsx
##  20K data/UNAP-5-QC-08feb2017.xlsx
## 2,1M data/UNAP-6-EPI-08feb2017.xlsx
```

## Priors {#priorupgraph}

Prior information before the former analysis:

- `unap1` should be equal to `unap2`
- `unap3` should be equal to `unap2` plus seropositivity agregate columns
- `unap3` was used in the online meeting with Nuno
- `unap4` have updated covariates and differs from `unap3`
- `unap5` have QC evaluations
- `unap6` was the original epidemilogical dataset.

## Updates

Updated information after the analysis was done:

- `unap6` is the **source** for `unap1` age and sex covariates only.
- `unap1` is the **source** of all other `unap#` DB's
- `unap2` is a **subset** of `unap1` shared with Nuno
- `unap3` is `unap1` plus **05** more seropositivity covariates
- `unap4` is `unap1` but with **updated** age and sex covariates
- `unap5` is `unap1` plus **QC** evaluations of micro and pcr covariates
- `unap5` is **different** from `unap1` only in micro UNAP covariate
- `unap1` UNAP covariates were arbitrarily **selected** to compare against `unap5` NAMR ones.

A diagram of DB history using `DiagrammeR` [@DiagrammeR]


```r
#install.packages("DiagrammeR")
#library(DiagrammeR)

grViz("
digraph unap_summary {

graph [layout= dot,
        rankdir= LR]

# node definitions with substituted label text
node [fontname = Helvetica]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']
d [label = '@@4', color= red, fontcolor= red]
e [label = '@@5', color= red, fontcolor= red]
f [label = '@@6', color= green, fontcolor= green]
g [label = '@@7', color= blue, fontcolor= blue]

# edge definitions with the node IDs
a -> {b c}
a -> {d e}  [color= red]
f -> a [color= green]
{a d e} -> g [color= blue]
}

[1]: 'unap1 DB (27)'
[2]: 'unap2 DB (8)'
[3]: 'unap3 DB (32)'
[4]: 'unap4 UP (8)'
[5]: 'unap5 QC (4)'
[6]: 'unap6 EPI (109)'
[7]: 'unap0 DB (14)'
")
```

<!--html_preserve--><div id="htmlwidget-ab9aed367bd7b1e477d1" style="width:864px;height:288px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-ab9aed367bd7b1e477d1">{"x":{"diagram":"\ndigraph unap_summary {\n\ngraph [layout= dot,\n        rankdir= LR]\n\n# node definitions with substituted label text\nnode [fontname = Helvetica]\na [label = \"unap1 DB (27)\"]\nb [label = \"unap2 DB (8)\"]\nc [label = \"unap3 DB (32)\"]\nd [label = \"unap4 UP (8)\", color= red, fontcolor= red]\ne [label = \"unap5 QC (4)\", color= red, fontcolor= red]\nf [label = \"unap6 EPI (109)\", color= green, fontcolor= green]\ng [label = \"unap0 DB (14)\", color= blue, fontcolor= blue]\n\n# edge definitions with the node IDs\na -> {b c}\na -> {d e}  [color= red]\nf -> a [color= green]\n{a d e} -> g [color= blue]\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


## Procedure

### UNAP-1-DB-20sep2016.dta

discoveries:

- `unap1` already have part of the seropositivity output (also present in `unap3`)
- variables absent in `unap2` are listed
- removed columns:
    + micro2 and dens2
    + OD reads
    + sero-positive results (cutoff + categories)
    + age categories
    + pcrsnou 1/2 and pcrrubio 1/2


```r
unap1 <- haven::read_dta("data/UNAP-1-DB-20sep2016.dta") %>%
  haven::as_factor()
# previous removing
unap1.out <- unap1 %>% select((micro2:age3), -pcrrubio)
unap1 <- unap1 %>% select(-(micro2:age3), pcrrubio)
#unap1$id <- forcats::as_factor(unap1$id) # not required
str(unap1)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	960 obs. of  7 variables:
##  $ id       : atomic  ZG159-4 ZG057-9 ZG017-1 ZG156-4 ...
##   ..- attr(*, "label")= chr "unique patient identifier"
##   ..- attr(*, "format.stata")= chr "%9s"
##  $ sex      : Factor w/ 2 levels "Female","Male": 1 2 1 2 2 2 1 2 1 1 ...
##   ..- attr(*, "label")= chr "gender "
##  $ age      : atomic  14 21 48 3 28 19 10 14 23 10 ...
##   ..- attr(*, "label")= chr "age as continues variable"
##   ..- attr(*, "format.stata")= chr "%8.0g"
##  $ community: Factor w/ 4 levels "Zungarococha",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "label")= chr "community where the patient lives and was enrolled from"
##  $ micro1   : Factor w/ 4 levels "Negative","P vivax",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "label")= chr "microscopy result by reader 1 and 2"
##  $ dens1    : atomic  NA NA NA NA NA NA NA NA NA NA ...
##   ..- attr(*, "label")= chr "parasite density counts by reader 1 and 2"
##   ..- attr(*, "format.stata")= chr "%8.0g"
##  $ pcrrubio : Factor w/ 3 levels "Negative","P vivax",..: 1 1 1 1 2 1 1 1 1 1 ...
##   ..- attr(*, "label")= chr "PCR results for both Pv and Pf using Rubio primers"
```

```r
summary(unap1)
```

```
##       id                sex           age                  community                     micro1   
##  Length:960         Female:505   Min.   : 2.00   Zungarococha   :442   Negative             :906  
##  Class :character   Male  :455   1st Qu.: 7.00   Puerto Almendra:107   P vivax              : 41  
##  Mode  :character                Median :15.00   Ninarrumi      :292   P falciparum         : 12  
##                                  Mean   :22.76   Llanchama      :119   Pv/Pf mixed infection:  1  
##                                  3rd Qu.:35.00                                                    
##                                  Max.   :88.00                                                    
##                                                                                                   
##      dens1                 pcrrubio  
##  Min.   :   35.0   Negative    :838  
##  1st Qu.:  131.5   P vivax     :115  
##  Median :  528.5   P falciparum:  7  
##  Mean   : 3701.4                     
##  3rd Qu.: 2129.2                     
##  Max.   :71115.0                     
##  NA's   :906
```


```r
colnames(unap1.out)
```

```
##  [1] "micro2"         "dens2"          "pvmsp1"         "pvmsp1n"        "pfmsp1"         "pcrrubio1"     
##  [7] "pcrrubio2"      "pcrsnou1"       "pcrsnou2"       "age2"           "age1"           "cutoffpvmsp1"  
## [13] "cutoffpvmsp1_1" "cutoffpfmsp1_1" "pvmsp1pos"      "pvmsp1npos1"    "pvmsp1pos1"     "cutoffpfmsp1"  
## [19] "pfmsp1pos1"     "age3"
```


### UNAP-2-DB-03oct2016.csv

discoveries:

- `unap2` is a **subset** of `unap1`
- `unap2` do not used **pvmsp1n** variable
- removed columns:
    + OD reads



```r
unap2 <- readr::read_csv("data/UNAP-2-DB-03oct2016.csv") %>%
  dplyr::mutate_each(funs(factor), 2, 4, 5, 9)
```

```
## Parsed with column specification:
## cols(
##   id = col_character(),
##   sex = col_character(),
##   age = col_integer(),
##   community = col_character(),
##   micro1 = col_character(),
##   dens1 = col_integer(),
##   pvmsp1 = col_double(),
##   pfmsp1 = col_double(),
##   pcrrubio = col_character()
## )
```

```r
#str(unap2)
unap2.out <- unap2 %>% select(matches(".msp.")) # previous removing
unap2 <- unap2 %>% select(-matches(".msp.")) # OD reads removing
str(unap2)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	960 obs. of  7 variables:
##  $ id       : chr  "ZG159-4" "ZG057-9" "ZG017-1" "ZG156-4" ...
##  $ sex      : Factor w/ 2 levels "Female","Male": 1 2 1 2 2 2 1 2 1 1 ...
##  $ age      : int  14 21 48 3 28 19 10 14 23 10 ...
##  $ community: Factor w/ 4 levels "Llanchama","Ninarrumi",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ micro1   : Factor w/ 4 levels "Negative","P falciparum",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ dens1    : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ pcrrubio : Factor w/ 3 levels "Negative","P falciparum",..: 1 1 1 1 3 1 1 1 1 1 ...
```

```r
summary(unap2)
```

```
##       id                sex           age                  community                     micro1   
##  Length:960         Female:505   Min.   : 2.00   Llanchama      :119   Negative             :906  
##  Class :character   Male  :455   1st Qu.: 7.00   Ninarrumi      :292   P falciparum         : 12  
##  Mode  :character                Median :15.00   Puerto Almendra:107   P vivax              : 41  
##                                  Mean   :22.76   Zungarococha   :442   Pv/Pf mixed infection:  1  
##                                  3rd Qu.:35.00                                                    
##                                  Max.   :88.00                                                    
##                                                                                                   
##      dens1                 pcrrubio  
##  Min.   :   35.0   Negative    :838  
##  1st Qu.:  131.5   P falciparum:  7  
##  Median :  528.5   P vivax     :115  
##  Mean   : 3701.4                     
##  3rd Qu.: 2129.2                     
##  Max.   :71115.0                     
##  NA's   :906
```

```r
colnames(unap2.out)
```

```
## [1] "pvmsp1" "pfmsp1"
```



### UNAP-3-DB-02nov2016.dta

discoveries:

- `unap3` is identical to `unap1` plus **05 more columns** about seropositivity
- removed columns:
    + micro2 and dens2
    + OD reads
    + sero-positive results (cutoff + categories + 5 columns)
    + age categories
    + pcrsnou 1/2 and pcrrubio 1/2


```r
unap3 <- haven::read_dta("data/UNAP-3-DB-02nov2016.dta") %>% 
  haven::as_factor()
#str(unap3)
# previous removing
unap3.out <- unap3 %>% select((micro2:ll_pvmsp1pos), -pcrrubio)
unap3 <- unap3 %>% select(-(micro2:ll_pvmsp1pos), pcrrubio)
#unap3$id <- forcats::as_factor(unap3$id) # not required
str(unap3)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	960 obs. of  7 variables:
##  $ id       : atomic  ZG029-7 ZG102-1 ZG174-4 ZG026-2 ...
##   ..- attr(*, "label")= chr "unique patient identifier"
##   ..- attr(*, "format.stata")= chr "%9s"
##  $ sex      : Factor w/ 2 levels "Female","Male": 2 1 1 1 2 1 1 1 1 2 ...
##   ..- attr(*, "label")= chr "gender "
##  $ age      : atomic  3 45 13 2 13 58 53 42 4 55 ...
##   ..- attr(*, "label")= chr "age as continues variable"
##   ..- attr(*, "format.stata")= chr "%8.0g"
##  $ community: Factor w/ 4 levels "Zungarococha",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ micro1   : Factor w/ 4 levels "Negative","P vivax",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "label")= chr "microscopy result by reader 1 and 2"
##  $ dens1    : atomic  NA NA NA NA NA NA NA NA NA NA ...
##   ..- attr(*, "label")= chr "parasite density counts by reader 1 and 2"
##   ..- attr(*, "format.stata")= chr "%8.0g"
##  $ pcrrubio : Factor w/ 3 levels "Negative","P vivax",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "label")= chr "PCR results for both Pv and Pf using Rubio primers"
```

```r
summary(unap3)
```

```
##       id                sex           age                  community                     micro1   
##  Length:960         Female:505   Min.   : 2.00   Zungarococha   :442   Negative             :906  
##  Class :character   Male  :455   1st Qu.: 7.00   Puerto Almendra:107   P vivax              : 41  
##  Mode  :character                Median :15.00   Ninarrumi      :292   P falciparum         : 12  
##                                  Mean   :22.76   Llanchama      :119   Pv/Pf mixed infection:  1  
##                                  3rd Qu.:35.00                                                    
##                                  Max.   :88.00                                                    
##                                                                                                   
##      dens1                 pcrrubio  
##  Min.   :   35.0   Negative    :838  
##  1st Qu.:  131.5   P vivax     :115  
##  Median :  528.5   P falciparum:  7  
##  Mean   : 3701.4                     
##  3rd Qu.: 2129.2                     
##  Max.   :71115.0                     
##  NA's   :906
```


```r
colnames(unap3.out)
```

```
##  [1] "micro2"         "dens2"          "pvmsp1"         "pvmsp1n"        "pfmsp1"         "pcrrubio1"     
##  [7] "pcrrubio2"      "pcrsnou1"       "pcrsnou2"       "age2"           "age1"           "cutoffpvmsp1"  
## [13] "cutoffpvmsp1_1" "cutoffpfmsp1_1" "pvmsp1pos"      "pvmsp1npos1"    "pvmsp1pos1"     "cutoffpfmsp1"  
## [19] "pfmsp1pos1"     "age3"           "seroposall"     "pfmsp1pos"      "c"              "ll"            
## [25] "ll_pvmsp1pos"
```

```r
#summary(select(unap3.out, (seroposall:ll_pvmsp1pos)))
```


### UNAP-4-UP-16ene2017.xlsx

discoveries:

- `unap4` is identical to `unap1` with **updated** `age` and `sex` covariates
- `unap1` is the original source of all `unap#` covariates
    + then, all covariates should be retrieved from `unap1`


```r
#read xl
unap4 <- readxl::read_excel("data/UNAP-4-UP-16ene2017.xlsx", 
                            sheet = 1, na = ".") %>%
  dplyr::select(id=1, sex=3, age=2, community=4, micro1=5, dens1=6, 
                pcrrubio_Pv=12, pcrrubio_Pf=13, pcrsnounou_Pv=14, pcrsnounou_Pf=15) %>%
  dplyr::mutate(pcrrubio= pcrrubio_Pf + pcrrubio_Pv,
                pcrsnounou= pcrsnounou_Pf + pcrsnounou_Pv) %>%
  dplyr::select(id:dens1,pcrrubio, pcrsnounou) %>%
  dplyr::mutate_each(funs(factor), 2,4,5,7,8)
str(unap4)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	960 obs. of  8 variables:
##  $ id        : chr  "LL001-2" "LL001-3" "LL003-1" "LL003-10" ...
##  $ sex       : Factor w/ 2 levels "1","2": 1 2 1 2 2 2 2 2 2 2 ...
##  $ age       : num  11 8 32 40 9 6 4 29 30 56 ...
##  $ community : Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 4 4 4 4 ...
##  $ micro1    : Factor w/ 4 levels "0","1","2","3": 1 1 1 1 1 2 1 1 1 1 ...
##  $ dens1     : num  NA NA NA NA NA ...
##  $ pcrrubio  : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 2 1 1 2 1 ...
##  $ pcrsnounou: Factor w/ 3 levels "0","1","2": NA NA NA NA NA NA NA NA NA NA ...
```

```r
summary(unap4)
```

```
##       id            sex          age        community micro1      dens1         pcrrubio pcrsnounou
##  Length:960         1:523   Min.   : 2.00   1:442     0:906   Min.   :   35.0   0:838    0   : 11  
##  Class :character   2:437   1st Qu.: 8.00   2:107     1: 41   1st Qu.:  131.5   1:115    1   :  3  
##  Mode  :character           Median :16.00   3:292     2: 12   Median :  528.5   2:  7    2   :  4  
##                             Mean   :23.86   4:119     3:  1   Mean   : 3701.4            NA's:942  
##                             3rd Qu.:36.00                     3rd Qu.: 2129.2                      
##                             Max.   :85.00                     Max.   :71115.0                      
##                                                               NA's   :906
```

#### unap4 covariate labels

Covariate labeling is based on labels available in covariate DB sheet 2.


```r
unap4.xl <- XLConnect::loadWorkbook("data/UNAP-4-UP-16ene2017.xlsx")
#
unap4.lb <- readWorksheet(unap4.xl, header = F,
                      sheet = 2, 
                      startRow = 8, endRow = 16,
                      startCol = 2, endCol = 6)
unap4.lb
```

```
##            Col1     Col2      Col3 Col4  Col5
## 1          SEXO FEMENINO MASCULINO <NA>  <NA>
## 2          <NA>        1         2 <NA>  <NA>
## 3     COMUNIDAD       ZG        PA   NN    LL
## 4          <NA>        1         2    3     4
## 5       ESPECIE       PV        PF   NG PV/PF
## 6          <NA>        1         2    0     3
## 7       NO DATO        .      <NA> <NA>  <NA>
## 8 RESULTADO PCR   Pos_Pv    Pos_Pf <NA>  <NA>
## 9          <NA>        1         2 <NA>  <NA>
```


### UNAP-5-QC-08feb2017.xlsx

discoveries:

- `unap1` **micro1** is different from `unap5` **micro.unap**
- **why?**


```r
#read xl
unap5 <- readxl::read_excel("data/UNAP-5-QC-08feb2017.xlsx", na = "-")
#colnames(unap5)
unap5 <- unap5[,1:5]
#
unap5 <- unap5 %>% 
  dplyr::select(id=1, pcr.unap=4, pcr.namr=5,  
                micro.unap=2, micro.namr=3) %>%
  dplyr::mutate(micro.comp= micro.unap==micro.namr,
                pcr.comp= pcr.unap==pcr.namr) %>%
  dplyr::select(1:3,7,4:5,6) #%>%
  #dplyr::mutate_each(funs(factor), 2,3,5,6)
str(unap5)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	222 obs. of  7 variables:
##  $ id        : chr  "NN043-9" "NN047-1" "NN048-4" "NN055-3" ...
##  $ pcr.unap  : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ pcr.namr  : num  0 0 0 0 0 1 0 1 1 1 ...
##  $ pcr.comp  : logi  TRUE TRUE TRUE TRUE TRUE FALSE ...
##  $ micro.unap: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ micro.namr: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ micro.comp: logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
```

```r
summary(unap5 %>%
          select(1,4,7))
```

```
##       id             pcr.comp       micro.comp     
##  Length:222         Mode :logical   Mode :logical  
##  Class :character   FALSE:59        FALSE:31       
##  Mode  :character   TRUE :163       TRUE :90       
##                                     NA's :101
```

#### test equality

- `unap1` and `unap5` should have equal **micro** and **pcr** ***UNAP*** covariates
- suffix:
    + `_1`: `unap1`
    + `_5`: `unap5` **UNAP** covariates
    + `_0`: comparison between `_1` and `_5`
    + `_n`: `unap5` **NAMR** covariates
    + `_n1`: `_n` vs `_1`
    + `_n5`: `_n` vs `_5`


```r
unap5c <- unap5 %>%
  select(id, micro_5= micro.unap, pcr_5=pcr.unap) %>%
  join(unap1 %>% #full_join(unap1 %>%
              select(id, micro_1= micro1, pcr_1=pcrrubio), 
            by= "id") %>%
  mutate(micro_1= fct_recode(micro_1,
                             "0"= "Negative", 
                             "1"= "P vivax", 
                             "2"= "P falciparum", 
                             "3"= "Pv/Pf mixed infection"),
         pcr_1= fct_recode(pcr_1, 
                           "0"= "Negative", 
                           "1"= "P vivax", 
                           "2"= "P falciparum",
                           "3"= "Pv/Pf mixed infection")) %>%
  mutate(micro_0= micro_1==micro_5, 
         pcr_0= pcr_1==pcr_5) %>%
  full_join(unap5 %>%
              select(id, micro_n= micro.namr, pcr_n= pcr.namr), 
            by="id") %>%
  mutate(micro_n1= micro_n==micro_1,
         micro_n5= micro_n==micro_5,
         pcr_n1= pcr_n==pcr_1,
         pcr_n5= pcr_n==pcr_5) %>%
  select(1,4,2,6,8,10,11,5,3,7,9,12,13) %>%
  dplyr::mutate_each(funs(factor), 3,5,9,11)
  
str(unap5c)
```

```
## 'data.frame':	222 obs. of  13 variables:
##  $ id      : chr  "NN043-9" "NN047-1" "NN048-4" "NN055-3" ...
##  $ micro_1 : Factor w/ 4 levels "0","1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
##  $ micro_5 : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 1 1 1 1 1 ...
##  $ micro_0 : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ micro_n : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 1 1 1 1 1 ...
##  $ micro_n1: logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ micro_n5: logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ pcr_1   : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 1 1 1 1 1 ...
##  $ pcr_5   : Factor w/ 3 levels "0","1","2": 1 1 1 1 1 1 1 1 1 1 ...
##  $ pcr_0   : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ pcr_n   : Factor w/ 4 levels "0","1","2","3": 1 1 1 1 1 2 1 2 2 2 ...
##  $ pcr_n1  : logi  TRUE TRUE TRUE TRUE TRUE FALSE ...
##  $ pcr_n5  : logi  TRUE TRUE TRUE TRUE TRUE FALSE ...
```

**pcr_1** and **pcr_5** are equivalent:


```r
summary(unap5c %>% 
          select(8:13))
```

```
##  pcr_1   pcr_5    pcr_0         pcr_n     pcr_n1          pcr_n5       
##  0:100   0:100   Mode:logical   0: 52   Mode :logical   Mode :logical  
##  1:115   1:115   TRUE:222       1:158   FALSE:59        FALSE:59       
##  2:  7   2:  7                  2:  6   TRUE :163       TRUE :163      
##                                 3:  6
```

However, **micro_1** and **micro_5** are different in **43** cells


```r
summary(unap5c %>% 
          select(2:7))
```

```
##  micro_1 micro_5  micro_0        micro_n     micro_n1        micro_n5      
##  0:168   0:191   Mode :logical   0   : 82   Mode :logical   Mode :logical  
##  1: 41   1: 23   FALSE:43        1   : 31   FALSE:3         FALSE:31       
##  2: 12   2:  8   TRUE :179       2   :  8   TRUE :118       TRUE :90       
##  3:  1                           NA's:101   NA's :101       NA's :101
```

#### selection

Arbitrary selection of `unap1` as **UNAP** covariates to compare against `unap5` **NAMRU** ones.


```r
unap5d <- unap5c %>%
  select(1, 
         2,5,6, 
         8,11,12)
summary(unap5d)
```

```
##       id            micro_1 micro_n     micro_n1       pcr_1   pcr_n     pcr_n1       
##  Length:222         0:168   0   : 82   Mode :logical   0:100   0: 52   Mode :logical  
##  Class :character   1: 41   1   : 31   FALSE:3         1:115   1:158   FALSE:59       
##  Mode  :character   2: 12   2   :  8   TRUE :118       2:  7   2:  6   TRUE :163      
##                     3:  1   NA's:101   NA's :101               3:  6
```


### UNAP-6-EPI-08feb2017.xlsx

- New question, New .Rmd


```r
#read xl
unap6 <- readxl::read_excel("data/UNAP-6-EPI-08feb2017.xlsx", sheet = 1)
#filter NA
unap6 <- unap6[1:sum(unap6$id_integrante != "", na.rm = T), 
           1:sum(colnames(unap6) != "", na.rm= T)]
#963x109
#colnames(unap6)
#str(unap6)
```

## Merge covariates

AIM:

- create `unap0`
- merge 
    + `unap1` age, sex
    + `unap4` age, sex
    + change factor label in sex
    + create 2 logic variables
    + `unap5` all (micro, pcr)
    + `unap1` community
    + avoided: dens1, micro2, dens2, pcr.snounou
- lastly, 
    + create a concensus for `micro_` and `pcr_`
- suffix:
    + `_1`: `unap1`
    + `_4`: `unap4`
    + `_0`: comparison between `_1` and `_4`
    + `_n`: `unap5` **NAMR** covariates
    + `_n1`: `_n` vs `_1`
    + `_c`: consensus variable between `_n` vs `_1`

A plot showing How `age` updating changed with respect to the patient `Ab.units` is in the 
[APPENDIX](#appendix-age-vs-ab-unit)


```r
#str(unap1)
#str(unap4)
unap4$sex <- fct_recode(unap4$sex, Female="1", Male="2")
#str(unap4)

unap0 <- 
  full_join(
    unap1 %>%
      select(id, age, sex), 
    unap4 %>%
      select(id, age, sex), 
    by= "id", suffix= c("_1", "_4")) %>%
  mutate(sex_0= sex_1==sex_4,
         age_0= age_1==age_4)%>%
  select(1,3,5,6,2,4,7)%>%
  full_join(unap5d, by= "id") %>%
  full_join(unap1 %>%
              select(id,community_1= community#, 
                     #micro_1= micro1, dens_1= dens1, pcr_1=pcrrubio
                     ),
            by="id")
```

### Generate consensus

- `micro` and `pcr` consensus variables
- criteria:
    + NAMR measurement had preference


```r
unap0 <- unap0 %>% 
  #select(contains("micro")) %>% #, -micro_n1
  tidyr::unite(micro_c, micro_1, micro_n,remove = F) %>% #count()
  mutate(micro_c=str_replace(micro_c,"(\\d)_\\1", "\\1")) %>%
  mutate(micro_c=str_replace(micro_c,"(.+)_NA", "\\1")) %>% 
  mutate(micro_c=str_replace(micro_c,"(\\d)_(\\d)", "\\2")) %>%  #%>% count()
  mutate(micro_c=str_replace(micro_c,"NA", replacement = NA_character_)) %>%  #%>% count()
  #select(contains("pcr")) %>% #, -micro_n1
  tidyr::unite(pcr_c, pcr_1, pcr_n,remove = F) %>% #count()
  mutate(pcr_c=str_replace(pcr_c,"(\\d)_\\1", "\\1")) %>%
  mutate(pcr_c=str_replace(pcr_c,"(.+)_NA", "\\1")) %>%   #count()
  mutate(pcr_c=str_replace(pcr_c,"(\\d)_(\\d)", "\\2")) %>% #summary()
  mutate(pcr_c=str_replace(pcr_c,"NA", replacement = NA_character_)) %>% dplyr::mutate_each(funs(factor),8,12)

#
#unap0
str(unap0)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	960 obs. of  16 variables:
##  $ id         : atomic  ZG159-4 ZG057-9 ZG017-1 ZG156-4 ...
##   ..- attr(*, "label")= chr "unique patient identifier"
##   ..- attr(*, "format.stata")= chr "%9s"
##  $ sex_1      : Factor w/ 2 levels "Female","Male": 1 2 1 2 2 2 1 2 1 1 ...
##   ..- attr(*, "label")= chr "gender "
##  $ sex_4      : Factor w/ 2 levels "Female","Male": 1 2 1 2 2 2 1 1 1 1 ...
##  $ sex_0      : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ age_1      : atomic  14 21 48 3 28 19 10 14 23 10 ...
##   ..- attr(*, "label")= chr "age as continues variable"
##   ..- attr(*, "format.stata")= chr "%8.0g"
##  $ age_4      : num  14 21 48 3 30 19 10 14 23 10 ...
##  $ age_0      : logi  TRUE TRUE TRUE TRUE FALSE TRUE ...
##  $ micro_c    : Factor w/ 3 levels "0","1","2": NA NA NA NA 1 NA NA NA NA NA ...
##  $ micro_1    : Factor w/ 4 levels "0","1","2","3": NA NA NA NA 1 NA NA NA NA NA ...
##  $ micro_n    : Factor w/ 3 levels "0","1","2": NA NA NA NA NA NA NA NA NA NA ...
##  $ micro_n1   : logi  NA NA NA NA NA NA ...
##  $ pcr_c      : Factor w/ 4 levels "0","1","2","3": NA NA NA NA 2 NA NA NA NA NA ...
##  $ pcr_1      : Factor w/ 3 levels "0","1","2": NA NA NA NA 2 NA NA NA NA NA ...
##  $ pcr_n      : Factor w/ 4 levels "0","1","2","3": NA NA NA NA 2 NA NA NA NA NA ...
##  $ pcr_n1     : logi  NA NA NA NA TRUE NA ...
##  $ community_1: Factor w/ 4 levels "Zungarococha",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "label")= chr "community where the patient lives and was enrolled from"
```

```r
#summary(unap0)
```



## Merge Ab units

- `std.uty` can be directly merged with `unap0`
- `std.nty` + `unap0` assumes that:
    + `micro_` and `pcr_` are independent observations with regard to the `specie`.

### untidy format

- At the moment, the most convenient dataset.
- Check its tidy version summary [here](#validate-tidy)


```r
std.uty.cov <- std.uty %>%
  select(id=ID, 2:5) %>%
  full_join(unap0, by="id") #%>%
  #select(1:14,21,15:17,19,20,18)


str(std.uty.cov)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	969 obs. of  20 variables:
##  $ id          : chr  "LL001-2" "LL001-3" "LL003-1" "LL003-10" ...
##  $ Ab.unit_Pfal: num  0.757 0.893 32.481 11.794 0.245 ...
##  $ Ab.unit_Pviv: num  7.48 1.08 1.16 16.1 9.25 ...
##  $ mean.OD_Pfal: num  0.0585 0.0605 0.2975 0.158 0.052 ...
##  $ mean.OD_Pviv: num  0.193 0.079 0.081 0.291 0.298 ...
##  $ sex_1       : Factor w/ 2 levels "Female","Male": 1 2 1 2 2 2 2 NA 2 2 ...
##   ..- attr(*, "label")= chr "gender "
##  $ sex_4       : Factor w/ 2 levels "Female","Male": 1 2 1 2 2 2 2 NA 2 2 ...
##  $ sex_0       : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ age_1       : atomic  11 8 32 40 40 12 4 NA 18 30 ...
##   ..- attr(*, "label")= chr "age as continues variable"
##   ..- attr(*, "format.stata")= chr "%8.0g"
##  $ age_4       : num  11 8 32 40 9 6 4 NA 29 30 ...
##  $ age_0       : logi  TRUE TRUE TRUE TRUE FALSE FALSE ...
##  $ micro_c     : Factor w/ 3 levels "0","1","2": NA NA NA NA NA 2 NA NA NA 1 ...
##  $ micro_1     : Factor w/ 4 levels "0","1","2","3": NA NA NA NA NA 2 NA NA NA 1 ...
##  $ micro_n     : Factor w/ 3 levels "0","1","2": NA NA NA NA NA 2 NA NA NA NA ...
##  $ micro_n1    : logi  NA NA NA NA NA TRUE ...
##  $ pcr_c       : Factor w/ 4 levels "0","1","2","3": NA NA NA NA NA 2 NA NA NA 2 ...
##  $ pcr_1       : Factor w/ 3 levels "0","1","2": NA NA NA NA NA 2 NA NA NA 2 ...
##  $ pcr_n       : Factor w/ 4 levels "0","1","2","3": NA NA NA NA NA 2 NA NA NA 2 ...
##  $ pcr_n1      : logi  NA NA NA NA NA TRUE ...
##  $ community_1 : Factor w/ 4 levels "Zungarococha",..: 4 4 4 4 4 4 4 NA 4 4 ...
##   ..- attr(*, "label")= chr "community where the patient lives and was enrolled from"
```

```r
summary(std.uty.cov)
```

```
##       id             Ab.unit_Pfal        Ab.unit_Pviv       mean.OD_Pfal      mean.OD_Pviv       sex_1    
##  Length:969         Min.   :   0.0032   Min.   :  0.0078   Min.   :0.04200   Min.   :0.0425   Female:505  
##  Class :character   1st Qu.:   0.4473   1st Qu.:  0.9310   1st Qu.:0.05600   1st Qu.:0.0810   Male  :455  
##  Mode  :character   Median :   1.1093   Median :  3.1413   Median :0.07025   Median :0.1502   NA's  :  9  
##                     Mean   :  13.9245   Mean   : 26.1670   Mean   :0.12078   Mean   :0.2478               
##                     3rd Qu.:   4.4097   3rd Qu.: 13.5217   3rd Qu.:0.11850   3rd Qu.:0.3445               
##                     Max.   :1758.8289   Max.   :817.2931   Max.   :0.89250   Max.   :1.3875               
##                     NA's   :44          NA's   :11         NA's   :1         NA's   :1                    
##     sex_4       sex_0             age_1           age_4         age_0         micro_c    micro_1   
##  Female:523   Mode :logical   Min.   : 2.00   Min.   : 2.00   Mode :logical   0   :167   0   :168  
##  Male  :437   FALSE:42        1st Qu.: 7.00   1st Qu.: 8.00   FALSE:137       1   : 44   1   : 41  
##  NA's  :  9   TRUE :918       Median :15.00   Median :16.00   TRUE :823       2   : 11   2   : 12  
##               NA's :9         Mean   :22.76   Mean   :23.86   NA's :9         NA's:747   3   :  1  
##                               3rd Qu.:35.00   3rd Qu.:36.00                              NA's:747  
##                               Max.   :88.00   Max.   :85.00                                        
##                               NA's   :9       NA's   :9                                            
##  micro_n     micro_n1        pcr_c      pcr_1      pcr_n       pcr_n1                 community_1 
##  0   : 82   Mode :logical   0   : 52   0   :100   0   : 52   Mode :logical   Zungarococha   :442  
##  1   : 31   FALSE:3         1   :158   1   :115   1   :158   FALSE:59        Puerto Almendra:107  
##  2   :  8   TRUE :118       2   :  6   2   :  7   2   :  6   TRUE :163       Ninarrumi      :292  
##  NA's:848   NA's :848       3   :  6   NA's:747   3   :  6   NA's :747       Llanchama      :119  
##                             NA's:747              NA's:747                   NA's           :  9  
##                                                                                                   
## 
```

```r
#View(std.uty.cov)
```

### tidy format

- `std.nty` + `unap0` assumes that:
    + `micro_` and `pcr_` are independent observations with regard to the `specie`.

**Would require:**

- merge `std.nty` and `unap0`
- filter by `Specie`
- recode `micro_` and `pcr_` levels per `Specie`
- generate `std.nty.cov` in a tidy format by combining all cases per Specie
- correct `micro_n1` and `pcr_n1` logic vectors

**Instead:**

- A [Relational data](http://r4ds.had.co.nz/relational-data.html) framework could be used.
    + The **Serological dataset** and **Covariate/Epidemiological** dataset would be 
    separated but **related**.
- This would imply:
    + keep `std.nty` as it is: tidy, and avoid `std.uty`.
    + keep `unap0` as it is: with no issues that came from the `Specie` covariate.
    + avoid untiding `std.nty` + `unap0` for `micro_` and `pcr_` coherence.
- At the moment, the most convenient dataset is `std.uty.cov`.

**By now:**

- `micro_` and `pcr_` are `Specie` related covariates
- `micro_` and `pcr_` were merged with serological data.

**TO DO:**

- A concensus, as in untidy, is missing.


```r
std.nty.pre <- std.nty %>%
  select(id=ID, 4:6) %>%
  full_join(unap0, by="id") #%>% count("micro_1")

std.nty.pre_Pv <- std.nty.pre %>% 
  select(1:4, micro_1:pcr_n1, -pcr_c) %>% 
  filter(Specie=="Pviv") %>% #count("pcr_1")
  mutate(micro_1=fct_recode(micro_1,
                            "0"="2",
                            "1"="3"),
         micro_n=fct_recode(micro_n,
                            "0"="2",
                            "1"="3"),
         pcr_1=fct_recode(pcr_1,
                          "0"="2"),
         pcr_n=fct_recode(pcr_n,
                          "0"="2",
                          "1"="3")) #%>% count("pcr_n")

std.nty.pre_Pf <- std.nty.pre %>% 
  select(1:4, micro_1:pcr_n1, -pcr_c) %>% 
  filter(Specie=="Pfal") %>% #count("pcr_n")
  mutate(micro_1=fct_recode(micro_1,
                            "1"="2",
                            "0"="1",
                            "1"="3"),
         micro_n=fct_recode(micro_n,
                            "1"="2",
                            "0"="1",
                            "1"="3"),
         pcr_1=fct_recode(pcr_1,
                          "1"="2",
                          "0"="1"),
         pcr_n=fct_recode(pcr_n,
                          "1"="2",
                          "0"="1",
                          "1"="3")) #%>% count("micro_1")

std.nty.cov <- dplyr::union(std.nty.pre_Pf, std.nty.pre_Pv)

## correct logic vectors of micro and pcr
std.nty.cov <- std.nty.cov %>% 
  mutate(micro_n1= micro_1==micro_n,
         pcr_n1= pcr_1==pcr_n)


str(std.nty.cov)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1937 obs. of  10 variables:
##  $ id      : chr  "ZG177-3" "ZG116-6" "ZG108-16" "ZG108-13" ...
##  $ Ab.unit : num  5.424 406.21 0.253 4.301 0.261 ...
##  $ mean.OD : num  0.18 0.724 0.0645 0.2295 0.065 ...
##  $ Specie  : Factor w/ 2 levels "Pfal","Pviv": 2 2 2 2 2 2 2 2 2 2 ...
##  $ micro_1 : Factor w/ 2 levels "0","1": NA NA NA 1 NA 2 NA 1 NA NA ...
##  $ micro_n : Factor w/ 2 levels "0","1": NA NA NA NA NA NA NA NA NA NA ...
##  $ micro_n1: logi  NA NA NA NA NA NA ...
##  $ pcr_1   : Factor w/ 2 levels "0","1": NA NA NA 1 NA 1 NA 2 NA NA ...
##  $ pcr_n   : Factor w/ 2 levels "0","1": NA NA NA 2 NA 1 NA 2 NA NA ...
##  $ pcr_n1  : logi  NA NA NA FALSE NA TRUE ...
```

```r
#summary(std.nty.cov)
```

**CHECK:**

- Note that `micro_` and `pcr_` covariates are now standardized as factors with only **2** labels.
- Validation in [APPENDIX](#appendix-tidy-vs-untidy)



## QC report

### using untidy dataset

```r
std.uty.cov %>% 
  select(micro_n1) %>% 
  filter(micro_n1!="NA") %>% 
  count() %>% 
  mutate(percent_similarity= (freq/sum(freq))*100)
```

```
##   micro_n1 freq percent_similarity
## 1    FALSE    3           2.479339
## 2     TRUE  118          97.520661
```

```r
std.uty.cov %>% 
  select(pcr_n1) %>% 
  filter(pcr_n1!="NA") %>% 
  count() %>% 
  mutate(percent_similarity= (freq/sum(freq))*100)
```

```
##   pcr_n1 freq percent_similarity
## 1  FALSE   59           26.57658
## 2   TRUE  163           73.42342
```

### using tidy dataset

- assumes that `micro_` and `pcr_` are independent observations with regard to the `specie`.
- given the mentioned assumption, percentage increases due to an updated logic variables.
    + Check details in [APPENDIX](#appendix-tidy-vs-untidy)


```r
std.nty.cov %>% 
  select(micro_n1) %>% 
  filter(micro_n1!="NA") %>% 
  count() %>% 
  mutate(percent_similarity= (freq/sum(freq))*100)
```

```
##   micro_n1 freq percent_similarity
## 1    FALSE    4           1.652893
## 2     TRUE  238          98.347107
```

```r
std.nty.cov %>% 
  select(pcr_n1) %>% 
  filter(pcr_n1!="NA") %>% 
  count() %>% 
  mutate(percent_similarity= (freq/sum(freq))*100)
```

```
##   pcr_n1 freq percent_similarity
## 1  FALSE   60           13.51351
## 2   TRUE  384           86.48649
```


## Output

- `std.uty.cov` output was written as `data/03-covariat-uty.csv`.
- `std.nty.cov` output was written as `data/03-covariat-nty.csv`.


```r
#
saveRDS(unap0, "data/03-covariat-uap.rds")
#
#readr::write_csv(std.nty.cov %>% format(scientific=FALSE, digits=2), "data/03-covariat-nty.csv")
saveRDS(std.nty.cov, "data/03-covariat-nty.rds")
#
#readr::write_csv(std.uty.cov %>% format(scientific=FALSE, digits=2), "data/03-covariat-uty.csv")
saveRDS(std.uty.cov, "data/03-covariat-uty.rds")
```


***

## APPENDIX: age vs Ab unit

- the difference between `age_1` and `age_4` was stored in `age_d` and plotted against `Ab.units`.


```r
db_ageAb <- std.uty.cov %>% 
  select(1:5,contains("age")) %>% 
  filter(age_0==FALSE) %>% 
  mutate(age_d= age_4-age_1) 

a <- db_ageAb %>% 
  ggplot(aes(Ab.unit_Pviv,age_d)) +
  geom_point()+
  geom_hline(aes(yintercept=0), colour="red")

b <- db_ageAb %>% 
  ggplot(aes(Ab.unit_Pfal,age_d)) +
  geom_point()+
  geom_hline(aes(yintercept=0), colour="red")

c <- db_ageAb %>% 
  ggplot(aes(mean.OD_Pviv,age_d)) +
  geom_point()+
  geom_hline(aes(yintercept=0), colour="red")

d <- db_ageAb %>% 
  ggplot(aes(mean.OD_Pfal,age_d)) +
  geom_point()+
  geom_hline(aes(yintercept=0), colour="red")

Rmisc::multiplot(c,d,a,b,cols = 2)
```

<img src="01-complete_files/figure-html/unnamed-chunk-79-1.png" style="display: block; margin: auto;" />


## APPENDIX: tidy vs untidy

### How to subset mixed infections with tidy?

- with regard to `pcr_n`


```r
std.nty.cov %>% 
  select(1,4,9) %>% #count("pcr_n")
  mutate(pcr_n= as.numeric(pcr_n)) %>% #count("pcr_n") # WARNING: 0 and 1 -> 1 and 2
  group_by(id) %>%
  summarise_if(is.numeric,sum) %>% # count("pcr_n")
  filter(pcr_n=="4") %>% 
  select(id)
```

```
## # A tibble: 6 x 1
##        id
##     <chr>
## 1 LL013-1
## 2 LL016-1
## 3 LL022-1
## 4 LL027-2
## 5 NN133-7
## 6 NN221-2
```

```r
  #filter(id=="LL013-1")#4 = 2+2 = mixed infection
  #filter(id=="LL011-3")#3 = 2+1 | 1+2 = mono infections
  #filter(id=="LL022-3")#2 = 1+1 = no infection
```


### Validate tidy

- Contrast this subset against `summary(std.uty.cov)` [here](#untidy-format):


```r
# ONLY FOR SEROLOGICAL DATA and -if required- for MICRO AND PCR data
summary(std.nty.cov %>% filter(Specie=="Pfal"))
```

```
##       id               Ab.unit             mean.OD         Specie    micro_1    micro_n     micro_n1      
##  Length:968         Min.   :   0.0032   Min.   :0.04200   Pfal:968   0   :209   0   :113   Mode :logical  
##  Class :character   1st Qu.:   0.4473   1st Qu.:0.05600   Pviv:  0   1   : 13   1   :  8   FALSE:2        
##  Mode  :character   Median :   1.1093   Median :0.07025              NA's:746   NA's:847   TRUE :119      
##                     Mean   :  13.9245   Mean   :0.12078                                    NA's :847      
##                     3rd Qu.:   4.4097   3rd Qu.:0.11850                                                   
##                     Max.   :1758.8289   Max.   :0.89250                                                   
##                     NA's   :43                                                                            
##   pcr_1      pcr_n       pcr_n1       
##  0   :215   0   :210   Mode :logical  
##  1   :  7   1   : 12   FALSE:7        
##  NA's:746   NA's:746   TRUE :215      
##                        NA's :746      
##                                       
##                                       
## 
```

```r
summary(std.nty.cov %>% filter(Specie=="Pviv"))
```

```
##       id               Ab.unit            mean.OD        Specie    micro_1    micro_n     micro_n1      
##  Length:969         Min.   :  0.0078   Min.   :0.0425   Pfal:  0   0   :180   0   : 90   Mode :logical  
##  Class :character   1st Qu.:  0.9310   1st Qu.:0.0810   Pviv:969   1   : 42   1   : 31   FALSE:2        
##  Mode  :character   Median :  3.1413   Median :0.1502              NA's:747   NA's:848   TRUE :119      
##                     Mean   : 26.1670   Mean   :0.2478                                    NA's :848      
##                     3rd Qu.: 13.5217   3rd Qu.:0.3445                                                   
##                     Max.   :817.2931   Max.   :1.3875                                                   
##                     NA's   :11         NA's   :1                                                        
##   pcr_1      pcr_n       pcr_n1       
##  0   :107   0   : 58   Mode :logical  
##  1   :115   1   :164   FALSE:53       
##  NA's:747   NA's:747   TRUE :169      
##                        NA's :747      
##                                       
##                                       
## 
```

- updated logic variables in `std.nty.cov`


```r
## TIDY DF needs NEW LOGIC vectors
std.nty.cov %>% filter(id=="ZG178-5") %>% select(-3)
```

```
## # A tibble: 2 x 9
##        id   Ab.unit Specie micro_1 micro_n micro_n1  pcr_1  pcr_n pcr_n1
##     <chr>     <dbl> <fctr>  <fctr>  <fctr>    <lgl> <fctr> <fctr>  <lgl>
## 1 ZG178-5 44.821053   Pviv       1      NA       NA      0      1  FALSE
## 2 ZG178-5  1.262928   Pfal       0      NA       NA      0      0   TRUE
```

```r
std.uty.cov %>% filter(id=="ZG178-5") %>% select(1:3,12:17)
```

```
## # A tibble: 1 x 9
##        id Ab.unit_Pfal Ab.unit_Pviv micro_c micro_1 micro_n micro_n1  pcr_c  pcr_1
##     <chr>        <dbl>        <dbl>  <fctr>  <fctr>  <fctr>    <lgl> <fctr> <fctr>
## 1 ZG178-5     1.262928     44.82105       1       1      NA       NA      1      0
```

### tidy is relevant for the QC evaluation {#tidyqcrev}
- In case **NAMR** differs in the identification of mixed-infection with respect to **UNAP**, 
- tidy dataset **QC report** takes **UNAP** mono-infection detections into account.

```r
# Pvivax + Pfalcip
std.nty.cov %>% 
  #select(1,2,4,11:16) %>% 
  filter(id=="LL013-1") # problem solved!!
```

```
## # A tibble: 2 x 10
##        id   Ab.unit mean.OD Specie micro_1 micro_n micro_n1  pcr_1  pcr_n pcr_n1
##     <chr>     <dbl>   <dbl> <fctr>  <fctr>  <fctr>    <lgl> <fctr> <fctr>  <lgl>
## 1 LL013-1  15.15406  0.2745   Pviv       0      NA       NA      1      1   TRUE
## 2 LL013-1 358.71734  0.7415   Pfal       0      NA       NA      0      1  FALSE
```

```r
std.uty.cov %>% 
  select(1:3,12:17) %>% 
  filter(id=="LL013-1")
```

```
## # A tibble: 1 x 9
##        id Ab.unit_Pfal Ab.unit_Pviv micro_c micro_1 micro_n micro_n1  pcr_c  pcr_1
##     <chr>        <dbl>        <dbl>  <fctr>  <fctr>  <fctr>    <lgl> <fctr> <fctr>
## 1 LL013-1     358.7173     15.15406       0       0      NA       NA      3      1
```

```r
#unap5 %>% 
#  filter(id=="LL013-1")
```


```r
## TEST all cases

# Pvivax
std.nty.cov %>% 
  select(1:4,11:16) %>% 
  filter(id=="LL003-4") # ok
# Pfalcip
std.nty.cov %>% 
  select(1:4,11:16) %>% 
  filter(id=="LL038-3") # ok
# non
std.nty.cov %>% 
  select(1:4,11:16) %>% 
  filter(id=="LL008-8") # ok
```




## APPENDIX: ID errors

### EVALUATE 01 patient 02 IDs

- TO DO:
    + In stand-by until finished with the epidemiological database `unap6`.

- AIM:
    + Contrast the data of the samples mentioned in the 2nd sheet of `UNAP-4-UP-16ene2017.xlsx`


```r
#unap4.xl <- XLConnect::loadWorkbook("data/UNAP-4-UP-16ene2017.xlsx")
unap4.dl <- readWorksheet(unap4.xl, 
                      sheet = 2, 
                      startRow = 20, endRow = 27,
                      startCol = 2, endCol = 3)
unap4.dl
```

```
##   paciente.con.doble.codigo..debe.eliminarse
## 1                          LL029-5 = ZG093-8
## 2                         PA065-5 = LL076-12
## 3                          ZG024-4 = ZG023-3
## 4                          ZG165-2 = ZG166-2
## 5                          ZG175-4 = ZG174-5
## 6                          ZG177-3 = ZG017-1
## 7                          ZG203-3 = PA001-4
```

```r
#str(unap4.dl)
```

Strings were split into individual characters in the following way:

```r
strsplit(unap4.dl[5,], split = " ")[[1]]
```

```
## [1] "ZG175-4" "="       "ZG174-5"
```

Check equality between pair of samples:

```r
unap4.dlMIX <- data.frame()
for(i in 1:dim(unap4.dl)[1]) {
  unap4.dlMIX <- rbind(unap4.dlMIX,
                      std.uty.cov[std.uty.cov$id==
                                   strsplit(unap4.dl[i,], 
                                            split = " ")[[1]][1],],
                      std.uty.cov[std.uty.cov$id==
                                   strsplit(unap4.dl[i,], 
                                            split = " ")[[1]][3],])
}
unap4.dlMIX
#format(unap4.dlMIX[,-c(7,8,10)], scientific=FALSE, digits=2)#
```

Some equalities do not seem to be from the same patient. This step needs more support prior to any sample deletion.


***

<!--## References-->

<!--chapter:end:03-covariat.Rmd-->

# EPIDEMIOLOGICAL COVARIATES {#epide}

## Aim

* Principal: 
    - Clean and standardize the UNAP-6 Epidemiological and Census database.

* Secondary: 
    - Check variable consistency across the database.

## Results

- The **109 variables** of the Epidemiological database were reduced to **88** 
after cleaning.
- typos were corrected:
    + some variables were renamed, and some factors recoded. [here](#typo-correction)
- previous datasets share **959 ID's** with the current Epidemiological dataset. [here](#merge-datasets)
- Two unified dataset were generated:
    + One with the **full 973** ID's
    + One with only the **core 959** ID's
- a plot `age` vs `residence` show some inconsistencies corrected throughout updates. [here](#age-vs-residence)
- **inconsistencies** between other variables are shown as table summaries. [here](#problemasx)
- `sheet2` included as **[Data Dictionary](#data-dictionary)**. [here](#data-dictionary)
- file output as a `.dta`. [here](#write-dta)


## To Do

- Solve **inconsistencies**
- CHECK IF **intersections** outside the core **959 samples** have ID typos.
- introduce a `datetime` class variable


## Dependencies

The required R packages for this analysis are:

+ `tidyverse` [@tidyverse]
+ `readxl` [@readxl]
+ `forcats` [@forcats]
+ `stringr` [@stringr]


```r
##essential
library(XLConnect)    # load EXCEL workbooks
library(drc)          # Dose-Response modeling
##accesory
library("DiagrammeR") # method Flowchart
library("knitr")      # To display nice tables
library("tidyverse")
#ggplot2, tibble, tidyr, readr, purr, dplyyr
##extra
library("Rmisc")      # multiplot ggplots
library("haven")      # import STATA df
library("readxl")     # import EXCEL df
library("forcats")    # factor vectors
library("lubridate")  # for dates
library("stringr")    # for strings
library("viridis")    # color visualization
```

## Input

```r
std.nty <- readRDS("data/02-dtfilter-nty.rds")
std.uty <- readRDS("data/02-dtfilter-uty.rds")

unap0 <- readRDS("data/03-covariat-uap.rds")
std.nty.cov <- readRDS("data/03-covariat-nty.rds")
std.uty.cov <- readRDS("data/03-covariat-uty.rds")
```


## Covariate datasets


```bash
ls -sh data/UNAP-*
```

```
##  88K data/UNAP-1-DB-20sep2016.dta
##  64K data/UNAP-2-DB-03oct2016.csv
## 108K data/UNAP-3-DB-02nov2016.dta
##  72K data/UNAP-4-UP-16ene2017.xlsx
##  20K data/UNAP-5-QC-08feb2017.xlsx
## 2,1M data/UNAP-6-EPI-08feb2017.xlsx
```


## UNAP-6-EPI-08feb2017.xlsx

```r
#check sheets
readxl::excel_sheets("data/UNAP-6-EPI-08feb2017.xlsx")
```

```
## [1] "database"               "data dictionarySpanish" "data dictionaryEnglish"
```

### sheet 1

```r
#read xl
#read xl
unap6 <- readxl::read_excel("data/UNAP-6-EPI-08feb2017.xlsx", sheet = 1) %>% 
  mutate(anios_residencia= as.numeric(anios_residencia))
#filter NA
unap6 <- unap6[1:sum(unap6$id_integrante != "", na.rm = T), 
           1:sum(colnames(unap6) != "", na.rm= T)]
#963x109
unap6 %>% glimpse()
```

```
## Observations: 963
## Variables: 109
## $ id_integrante                         <chr> "LL001_2", "LL001_3", "LL003_1", "LL003_10", "LL003_3", "LL...
## $ barrido                               <chr> "Barrido01", "Barrido01", "Barrido01", "Barrido01", "Barrid...
## $ barrio                                <chr> "Llanchama", "Llanchama", "Llanchama", "Llanchama", "Llanch...
## $ referencia                            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ grupos_personas                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ direccion                             <chr> "Calle Nueva Jerusalen", "Calle Nueva Jerusalen", "Calle. J...
## $ telefono                              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ celular                               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ email                                 <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ Edad                                  <dbl> 11, 8, 32, 40, 40, 12, 4, 18, 30, 56, 6, 33, 15, 7, 12, 10,...
## $ sexo                                  <chr> "F", "M", "F", "M", "M", "M", "M", "M", "M", "M", "F", "F",...
## $ estado_civil                          <chr> "Soltero", "Soltero", "Conviviente", "Conviviente", "Conviv...
## $ nivel_educacion                       <chr> "Primaria Incompleta", "Primaria Incompleta", "Primaria Com...
## $ tipo_trabajo                          <chr> "0. Actualmente desempleado", "0. Actualmente desempleado",...
## $ otro_tipo_trabajo                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ anios_residencia                      <dbl> 11, 8, 4, 7, 4, 5, 4, 3, 2, 44, 6, 22, 14, 5, 12, 10, 2, 45...
## $ electricidad_red_publica              <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "Si",...
## $ combustible_cocinar                   <chr> "5. Leña", "5. Leña", "5. Leña", "5. Leña", "5. Leña", "5. ...
## $ otro_combustible_cocinar              <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ ah_radio                              <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,...
## $ ah_television                         <dbl> 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ ah_estereo                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ ah_refrigerador                       <dbl> 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1,...
## $ ah_motocicleta                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ ah_mototaxi                           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ ah_auto                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ pertenencia_casa                      <chr> "1. Propia", "1. Propia", "1. Propia", "1. Propia", "1. Pro...
## $ otro_pertenencia_casa                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ tipo_casa                             <chr> "1. Casa independiente", "1. Casa independiente", "1. Casa ...
## $ otro_tipo_casa                        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ cantidad_habitaciones                 <dbl> 3, 2, 3, 1, 7, 4, 3, 3, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1,...
## $ cantidad_habitaciones_dormir          <dbl> 3, 2, 3, 1, 7, 4, 3, 3, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1,...
## $ material_paredes                      <chr> "6. Madera", "6. Madera", "6. Madera", "6. Madera", "6. Mad...
## $ otro_material_paredes                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ material_piso                         <chr> "6. Tierra/arena", "6. Tierra/arena", "6. Tierra/arena", "6...
## $ otro_material_piso                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ fuente_agua                           <chr> "4. Pozo", "4. Pozo", "4. Pozo", "4. Pozo", "4. Pozo", "4. ...
## $ otro_fuente_agua                      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ banio_contectado                      <chr> "1. Sistema publico dentro de la casa", "1. Sistema publico...
## $ otro_banio_conectado                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ tenido_malaria                        <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ cuantas_tenido_malaria                <dbl> 4, 2, 3, 2, 2, 3, 1, 2, 1, 2, 1, 5, 2, 1, 0, 2, 0, 6, 7, 2,...
## $ opciones_tenido_malaria               <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ recibio_tratamiento_malaria           <chr> "1. Mas de una vez (especifique)", "1. Mas de una vez (espe...
## $ cantidad_tratamiento_malaria          <dbl> 1, 1, 3, NA, NA, NA, NA, NA, NA, NA, NA, 5, 2, 0, 0, 2, 0, ...
## $ busco_ayuda_tratamiento               <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ no_busco_ayuda_tratamiento            <chr> "0. No pensa que era un problema serio", "0. No pensa que e...
## $ otro_no_busco_ayuda_tratamiento       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ donde_busco_tratamiento               <chr> "1. Posta MINSA /CCSS MINSA", "1. Posta MINSA /CCSS MINSA",...
## $ otro_donde_busco_tratamiento          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ respuesta_drogas_tratamiento          <chr> "1. Si", "1. Si", "1. Si", "1. Si", "1. Si", "1. Si", "1. S...
## $ drogras_tratamiento_01                <chr> "Cloroquina", "Cloroquina", "Cloroquina", "Cloroquina", "Cl...
## $ ndias_drogras_tratamiento_01          <dbl> 4, 5, 3, 3, 4, 3, 5, 3, 3, 3, 3, 0, 3, 0, 0, 0, 0, 0, 3, 3,...
## $ drogras_tratamiento_02                <chr> "Primaquina", "Primaquina", "Primaquina", "Primaquina", "Pr...
## $ ndias_drogras_tratamiento_02          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 4, 4,...
## $ drogras_tratamiento_03                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ ndias_drogras_tratamiento_03          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ enfermedad_cronica                    <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ enf_diabetes_si                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ enf_diabetes_no                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ enf_neurologica_si                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ enf_neurologica_no                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ enf_especifique_neurologica           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ enf_pulmonar_si                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ enf_pulmonar_no                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ enf_especifique_pulmonar              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ enf_cardiaca_si                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ enf_cardiaca_no                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ enf_especifique_cardiaca              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ enf_renal_si                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ enf_renal_no                          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ enf_especifique_renal                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ enf_especifique_otro                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ enf_tomo_medicinas_parasitos          <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ enf_medicinas_parasitos               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "albendazol", "...
## $ epi_cerca_fuente_agua                 <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ epi_tipo_fuente_agua                  <chr> "1. Rio", "1. Rio", "1. Rio", "1. Rio", "1. Rio", "1. Rio",...
## $ epi_distancia_fuente_agua             <chr> "1. Menor de 100 metros", "1. Menor de 100 metros", "1. Men...
## $ epi_malaria_ultimos_meses             <chr> "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No ...
## $ epi_viajo_fuera_villa                 <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ epi_especifique_viajo_fuera_villa     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ epi_meses_ultima_malaria              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 2, 2,...
## $ epi_especie_causo_malaria             <chr> "1. P. Vivax", "1. P. Vivax", "1. P. Vivax", "1. P. Vivax",...
## $ epi_uso_repelente_mosquito            <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_uso_mangas_largas                 <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_uso_ropa_impregnada               <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_cercania_fuente_agua              <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_estuvo_campo_antes                <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_uso_redes_cama                    <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_duerme_ventanas_abiertas          <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_duerme_cerca_monte                <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_rocia_con_insecticida             <chr> "0. No Aplica", "0. No Aplica", "0. No Aplica", "0. No Apli...
## $ epi_ultimas_semanas_viajo             <chr> "1. Nunca", "1. Nunca", "1. Nunca", "1. Nunca", "1. Nunca",...
## $ epi_numero_veces_viajo                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ epi_frecuencia_rocia_casa             <chr> "1. Nunca", "1. Nunca", "1. Nunca", "1. Nunca", "1. Nunca",...
## $ epi_alguien_tuvo_malaria              <chr> "1. Nunca", "1. Nunca", "1. Nunca", "1. Nunca", "1. Nunca",...
## $ epi_estado_campos_agricultura         <chr> "5. Siempre", "5. Siempre", "5. Siempre", "5. Siempre", "5....
## $ epi_estado_canal_agua                 <chr> "5. Siempre", "5. Siempre", "5. Siempre", "5. Siempre", "5....
## $ epi_adquirio_redes_dormir             <chr> "2. Si, redes de fabrica con insecticida", "2. Si, redes de...
## $ epi_cantidad_redes_adquirio_dormir    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4,...
## $ epi_uso_red_dormir                    <chr> "3. Desde siempre", "3. Desde siempre", "3. Desde siempre",...
## $ epi_redes_usadas_actualmente          <chr> "1. Si, redes de fabrica sin insecticida", "1. Si, redes de...
## $ epi_cantidad_redes_usadas_actualmente <dbl> 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 3, 2, 2, 2, 2, 2, 2, 4, 4,...
## $ relacion_jefe_familia                 <chr> "3. Hijo o Hijastro quien es soltero y tiene hijos", "3. Hi...
## $ actualmente_estudiando                <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ trabajo_paga_utlima                   <chr> "No", "No", "Si", "Si", "Si", "No", "No", "Si", "Si", "Si",...
## $ fecha_registro                        <chr> "09/08/2015 11:35:48 a.m.", "09/08/2015 11:35:48 a.m.", "09...
## $ fecha_actualizacion                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ estado                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
```

```r
#colnames(unap6)
#str(unap6)
#summary(unap6)
```

### typo correction

- rename variables
- recode factors
- `unap6a` created

Bad idea:

- remove varaibles with full **NA** values (109 -> 95)
- affects class of numeric variables

TO DO:
- who to fix it?
- function to identify numeric values inside characters: **`chr` -> `dbl`**


```r
unap6a <- unap6 %>%
  dplyr::rename(id=id_integrante, 
         community=barrio, 
         age= Edad,
         residence= anios_residencia,
         sex= sexo,
         trabajo_paga_ultima= trabajo_paga_utlima,
         banio_conectado= banio_contectado,
         drogas_tratamiento_01= drogras_tratamiento_01,
         ndias_drogas_tratamiento_01= ndias_drogras_tratamiento_01,
         drogas_tratamiento_02= drogras_tratamiento_02,
         ndias_drogas_tratamiento_02= ndias_drogras_tratamiento_02,
         drogas_tratamiento_03= drogras_tratamiento_03,
         ndias_drogas_tratamiento_03= ndias_drogras_tratamiento_03) %>%
  mutate(id= str_replace_all(id, "_","-"),
         sex= fct_recode(sex, M="m"), 
         nivel_educacion= fct_recode(nivel_educacion, 
                                    "Tecnica Completa"="TÃƒÂ©cnica Completa",
                                    "Tecnica Incompleta"="TÃƒÂ©cnica Incompleta"),
         otro_tipo_trabajo= fct_recode(otro_tipo_trabajo, 
                                    "albanil"="albaÃƒÂ±il",
                                    "albanil"="albaÃ±il",
                                    "contruccion"="construcciÃƒÂ³n"),
         otro_material_paredes= fct_recode(otro_material_paredes, 
                                    "Tripley"="tripley",
                                    "Tripley"="tripleyy"),
         fuente_agua= fct_recode(fuente_agua,
                                "3. Tuberia publica, fuente fuera de la casa, 
                                compartido con la comunidad"=
                                  "3. TuberiƒÂa publica, fuente fuera de la casa, 
                                compartido con la comunidad"),
         banio_conectado= fct_recode(banio_conectado,
                                     "0. No tiene banio"="0. No tiene baÃ±o"),
         recibio_tratamiento_malaria= fct_recode(recibio_tratamiento_malaria,
                                                "1. Mas de una vez (especifique)"=
                                                  "1.Mas de una vez (especifique)",
                                                "1. Mas de una vez (especifique)"=
                                                  "1. MAs de una vez (especifique)",
                                                "1. Mas de una vez (especifique)"=
                                                  "1. MÃ¡s de una vez (especifique)"),
         epi_alguien_tuvo_malaria= fct_recode(epi_alguien_tuvo_malaria,
                                             "2. Si, hace algunos anos"=
                                               "2. Si, hace algunos aÃ±os",
                                             "3. Si, el ultimo ano"=
                                               "3. Si, el ultimo aÃ±o"),
         no_busco_ayuda_tratamiento= fct_recode(no_busco_ayuda_tratamiento,
                                               "0. No pensaba que era un problema serio"=
                                                 "0. No pensa que era un problema serio",
                                               "0. No pensaba que era un problema serio"=
                                                 "0. No pensÃƒÆ’Ã‚Â³ que era un problema serio"),
         drogas_tratamiento_01= fct_recode(drogas_tratamiento_01,
                                           "artesunato"="artesunatu",
                                           "artesunato"="Artesunatu",
                                           "cloroquina"="Cloroquina ",
                                           "cloroquina"="Cloroquina",
                                           "FANSIDAR"="fansedal",
                                           "primaquina"="trimaquina"),
         drogas_tratamiento_02= fct_recode(drogas_tratamiento_02,
                                           "clindamicina"="Clinamicina",
                                           "mefloquina"="nefloquina",
                                           "mefloquina"="Nefloquina",
                                           "primaquina"="Pramaquina",
                                           "primaquina"="Primaquima",
                                           "primaquina"="Primaquina",
                                           "primaquina"="trimaquina"),
         enf_especifique_renal=fct_recode(enf_especifique_renal,
                                          "dolor de rinones"="dolor de riÃ±oned",
                                          "dolor de rinones"="dolor de RiÃ±ones",
                                          "dolor de rinones"="dolores",
                                          "piedras en rinones"="piedras en los riÃ±ones",
                                          "piedras en rinones"="piedrillas"),
         enf_especifique_otro=fct_recode(enf_especifique_otro,
                                         "gastritis"="Gastritis",
                                         "migrana"="Migrana"),
         enf_medicinas_parasitos=fct_recode(enf_medicinas_parasitos,
                                         "albendazol"="abbendazol",
                                         "albendazol"="abendabzol",
                                         "albendazol"="abendazol",
                                         "albendazol"="Abendazol",
                                         "albendazol"="abendazol3",
                                         "albendazol"="abendazolp",
                                         "albendazol"="abenzadol",
                                         "albendazol"="albendazol",
                                         "antiparasitario"="antiparasitarias",
                                         "antiparasitario"="antiparasitarios"),
         combustible_cocinar=fct_recode(combustible_cocinar,
                                        "5. Lenha"="5. Leña")
         ) #%>% mutate_each(funs(factor), 3) 
  #gather(key= variable, value = observation, -id, na.rm = T) %>% 
  #spread(variable, observation) 

#unap6a %>% glimpse()

#unap6a %>% #dplyr::count(epi_alguien_tuvo_malaria, sort = T)
#  str()
  #count(tipo_trabajo)
  #count(combustible_cocinar)
  #count(material_paredes)
#count(material_piso)
  #count(fuente_agua)
  #count(recibio_tratamiento_malaria)
  #count(drogras_tratamiento_01) #--------------------dirty
  #count(drogras_tratamiento_02) #--------------------dirty
  #count(fecha_registro) #--------------------------------------lubridate
  #count(epi_especie_causo_malaria) %>%
  #mutate(prop=n/sum(n))
```

#### all NA variables

- **14** variables are full of NA values
- remove throughout the evaluations


```r
unap6x <- unap6a %>%   
  gather(key= variable, value = observation, -id, na.rm = T) %>% 
  spread(variable, observation)

xd <- data_frame(v= colnames(unap6x))
ad <- data_frame(v= colnames(unap6a))

dplyr::setdiff(ad,xd)
```

```
## # A tibble: 8 x 1
##                                 v
##                             <chr>
## 1                           email
## 2        otro_combustible_cocinar
## 3                  otro_tipo_casa
## 4              otro_material_piso
## 5                otro_fuente_agua
## 6 otro_no_busco_ayuda_tratamiento
## 7     enf_especifique_neurologica
## 8             fecha_actualizacion
```


### problems 1

#### age vs residence

discovery:

- `unap6` differs from `unap1` in **age** variable

TO DO:

- CHECK WHY some `age_4` are still lower than patient `residence` time.


```r
unap60ar <- std.uty.cov %>% 
  dplyr::select(id, age_1, age_4) %>% 
  left_join(unap6a %>% 
              dplyr::select(id, age_e=age, residence), by="id") %>% 
  mutate(age_e1= age_1==age_e,
         age_e4= age_4==age_e,
         age_0= age_1==age_4) #%>% #filter(age_e1==F) #count("age_e1")

a <- unap60ar %>% 
  group_by(age_e, residence) %>%
  dplyr::summarise(n=n()) %>%
  mutate(prop= n/sum(n)) %>%
  ggplot(aes(age_e, residence, colour=n)) + 
  geom_point() + 
  scale_color_viridis() +
  labs(title="unap6-EPI")
  
b <- unap60ar %>% 
  group_by(age_1, residence) %>%
  dplyr::summarise(n=n()) %>%
  mutate(prop= n/sum(n)) %>%
  ggplot(aes(age_1, residence, colour=n)) + 
  geom_point() + 
  scale_color_viridis() +
  labs(title="unap1-DB")

c <- unap60ar %>% 
  group_by(age_4, residence) %>%
  dplyr::summarise(n=n()) %>%
  mutate(prop= n/sum(n)) %>%
  ggplot(aes(age_4, residence, colour=n)) + 
  geom_point() + 
  scale_color_viridis() +
  labs(title="unap4-UP")

Rmisc::multiplot(a,b,c,cols = 3)
```

<img src="01-complete_files/figure-html/ageres-1.png" style="display: block; margin: auto;" />

### problems 2 {#problemasx}

- `unap6a` was updated on each step

#### ah_

- change `ah_` factor labels


```r
unap6a <- unap6a %>% 
  mutate_at(vars(ah_radio:ah_auto), factor) %>% #dplyr::select(1, dplyr::contains("enf")) %>% glimpse()
  mutate(ah_radio=fct_recode(ah_radio,"si"="1","no"="0"), 
         ah_television=fct_recode(ah_television,"si"="1","no"="0"), 
         ah_estereo=fct_recode(ah_estereo,"si"="1","no"="0"), 
         ah_refrigerador=fct_recode(ah_refrigerador,"si"="1","no"="0"), 
         ah_motocicleta=fct_recode(ah_motocicleta,"si"="1","no"="0"), 
         ah_mototaxi=fct_recode(ah_mototaxi,"si"="1","no"="0"), 
         ah_auto=fct_recode(ah_auto,"si"="1","no"="0")) #%>% dplyr::select(enf_diabetes_si,enf_diabetes_no) %>% count()

unap6a %>% 
  dplyr::select(1, starts_with("ah_")) %>%
  gather(key= ah, value = ah_m, -id, na.rm = T) %>% 
  filter(ah_m=="si") %>% dplyr::count(ah, sort = T)
```

```
## # A tibble: 6 x 2
##                ah     n
##             <chr> <int>
## 1   ah_television   842
## 2 ah_refrigerador   650
## 3        ah_radio   601
## 4     ah_mototaxi   460
## 5      ah_estereo   119
## 6  ah_motocicleta    88
```

```r
  #group_by(ah, ah_m) %>% 
  #dplyr::summarise(n=n()) %>%
  #mutate(prop= (n/sum(n)*100))
```

#### otro_

discovery:

- this step informs that only **3 out of 11** variables that start with `otro_` have significant variables.
- remove **8** `otro_` variables


```r
unap6a %>% 
  dplyr::select(1, starts_with("otro_")) %>% #str() # 3/11 son significativas
  gather(key= otro, value = otro_m, -id, na.rm = T) %>% dplyr::count(otro, sort = T)
```

```
## # A tibble: 5 x 2
##                           otro     n
##                          <chr> <int>
## 1            otro_tipo_trabajo    65
## 2        otro_material_paredes    41
## 3        otro_pertenencia_casa     9
## 4         otro_banio_conectado     6
## 5 otro_donde_busco_tratamiento     4
```

```r
unap6_otro <- unap6a %>% 
  dplyr::select(1, starts_with("otro_")) %>% #str() # 3/11 son significativas
  dplyr::select(1, 2,6,9) #%>% #str() # 3/11 son significativas
  
unap6a <- unap6a %>% 
  dplyr::select(-starts_with("otro_")) %>% #str() # 3/11 son significativas
  left_join(unap6_otro, by="id") #%>% dplyr::select(1, starts_with("otro_")) %>% str() # 3/11 son significativas
  #spread(key= otro, value = otro_m)
```

#### trabajo

- create `trabajo` in order to merge `tipo_trabajo`and `otro_tipo_trabajo`
- `trabajo` is a **lumped** `fct_lump()` variable


```r
unap6a <- unap6a %>% 
  mutate(tipo_trabajo= str_replace(tipo_trabajo,"\\d+\\.\\s", "")) %>% #count("tipo_trabajo")
  mutate(tipo_trabajo= str_replace(tipo_trabajo,"Actualmente ", "")) %>% #dplyr::count(trabajo, sort = T)
  mutate(tipo_trabajo= str_replace(tipo_trabajo,str_c("Otro ","\\(","especificar","\\)"), "NA")) %>% #dplyr::count(tipo_trabajo, sort = T)
  unite(trabajo, tipo_trabajo, otro_tipo_trabajo) %>% 
  mutate(trabajo= str_replace(trabajo,"_NA", "")) %>% #dplyr::count(trabajo, sort = T)
  mutate(trabajo= str_replace(trabajo,"NA_", "")) %>% #dplyr::count(trabajo, sort = T)
  mutate(trabajo= fct_lump(trabajo, 5)) 

#unap6a %>% glimpse()

unap6a %>% 
  dplyr::select(1, dplyr::contains("trabajo")) %>% 
  dplyr::count(trabajo, sort = T)
```

```
## # A tibble: 6 x 2
##         trabajo     n
##          <fctr> <int>
## 1   desempleado   580
## 2   Ama de casa   262
## 3    Agricultor    51
## 4         Other    45
## 5 independiente    14
## 6        obrero    11
```

```r
  #group_by(trabajo, trabajo_paga_ultima) %>% dplyr::count(trabajo_paga_ultima, sort = T)
```

#### material

- create `material_pared` in order to merge `material_paredes`and `otro_material_paredes`


```r
unap6a <- unap6a %>% 
  #dplyr::select(1, dplyr::contains("material")) %>% 
  mutate(material_piso= str_replace(material_piso,"\\d+\\.\\s", "")) %>% #dplyr::count(material_piso, sort = T)
  mutate(material_paredes= str_replace(material_paredes,"\\d+\\.\\s", "")) %>% #dplyr::count(material_paredes, sort = T)
  mutate(material_paredes= str_replace(material_paredes,"\\so\\s", "_")) %>% #dplyr::count(material_paredes, sort = T)
  mutate(material_paredes= str_replace(material_paredes,str_c("Otro ","\\(","especifique","\\)"), "NA")) %>%#dplyr::count(material_paredes, sort = T)
  #group_by(material_paredes, otro_material_paredes) %>% dplyr::count()
  unite(material_pared, material_paredes, otro_material_paredes) %>% 
  mutate(material_pared= str_replace(material_pared,"_NA", "")) %>% #dplyr::count(trabajo, sort = T)
  mutate(material_pared= str_replace(material_pared,"NA_", "")) 

#unap6a %>% glimpse()

unap6a %>% 
  dplyr::count(material_pared, sort = T)
```

```
## # A tibble: 7 x 2
##              material_pared     n
##                       <chr> <int>
## 1                    Madera   481
## 2          Ladrillo_cemento   441
## 3                   Tripley    18
## 4  Ladrillo_cemento_Tripley    11
## 5            Madera_Tripley     8
## 6             Madera_costal     3
## 7 Ladrillo_cemento_calamina     1
```

```r
  #mutate(material_pared= fct_lump(material_pared, 3)) %>% dplyr::count(material_pared, sort = T)
  #group_by(trabajo, trabajo_paga_ultima) %>% dplyr::count(trabajo_paga_ultima, sort = T)
  
unap6a %>% 
  dplyr::select(1, dplyr::contains("material")) %>% 
  mutate(material_piso= str_replace(material_piso,"\\d+\\.\\s", "")) %>% dplyr::count(material_piso, sort = T)
```

```
## # A tibble: 5 x 2
##             material_piso     n
##                     <chr> <int>
## 1                 Cemento   464
## 2            Tierra/arena   429
## 3        Tablas de madera    46
## 4 Parquet o madera pulida    21
## 5                    <NA>     3
```

#### banio

- create `banio_conexion` in order to merge `banio_conectado`and `otro_banio_conectado`


```r
unap6a <- unap6a %>% 
  #dplyr::select(1, dplyr::contains("banio")) %>% #dplyr::count(banio_conectado, sort = T)
  mutate(banio_conectado= str_replace(banio_conectado,"\\d+\\.\\s", "")) %>% #dplyr::count(material_paredes, sort = T)
  mutate(banio_conectado= str_replace(banio_conectado,str_c("Otro","\\.\\s","\\(","especifique","\\)"), "NA"))%>%#dplyr::count(banio_conectado, sort = T)
  #group_by(material_paredes, otro_material_paredes) %>% dplyr::count()
  unite(banio_conexion, banio_conectado, otro_banio_conectado) %>% 
  mutate(banio_conexion= str_replace(banio_conexion,"_NA", "")) %>% #dplyr::count(trabajo, sort = T)
  mutate(banio_conexion= str_replace(banio_conexion,"NA_", "")) 


#unap6a %>% glimpse()

#unap6a %>% dplyr::select(1, dplyr::contains("banio")) %>% glimpse()

unap6a %>% dplyr::count(banio_conexion, sort = T)
```

```
## # A tibble: 7 x 2
##                      banio_conexion     n
##                               <chr> <int>
## 1 Sistema publico dentro de la casa   592
## 2                           Letrina   337
## 3                     Campo abierto    17
## 4                              silo     6
## 5                    No tiene banio     5
## 6  Sistema publico fuera de la casa     5
## 7                                NA     1
```

```r
  #mutate(material_pared= fct_lump(material_pared, 3)) %>% dplyr::count(material_pared, sort = T)
  #group_by(trabajo, trabajo_paga_ultima) %>% dplyr::count(trabajo_paga_ultima, sort = T)
```

#### malaria

- `recibio_**tratamiento**_malaria` will be used in the next step


```r
unap6a <- unap6a %>% #str()
  #dplyr::select(1, age, residence, dplyr::contains("malaria")) %>% #str()
  mutate(recibio_tratamiento_malaria= str_replace(recibio_tratamiento_malaria,"\\d+\\.\\s", "")) %>% 
  mutate(recibio_tratamiento_malaria= str_replace(recibio_tratamiento_malaria,str_c("\\s","\\(","especifique","\\)"), "")) %>%
  mutate(epi_especie_causo_malaria= str_replace(epi_especie_causo_malaria,"\\d+\\.\\s", "")) %>% 
  mutate(epi_alguien_tuvo_malaria= str_replace(epi_alguien_tuvo_malaria,"\\d+\\.\\s", "")) #%>% #dplyr::count(epi_alguien_tuvo_malaria, sort = T)
  #dplyr::count(cuantas_tenido_malaria, sort = T)
  #dplyr::count(epi_meses_ultima_malaria, sort = T)
  #dplyr::count(cantidad_tratamiento_malaria, sort = T)

#unap6a %>% glimpse()

unap6a %>% 
  dplyr::select(1, age, residence, dplyr::contains("malaria")) %>%
  glimpse()
```

```
## Observations: 963
## Variables: 12
## $ id                           <chr> "LL001-2", "LL001-3", "LL003-1", "LL003-10", "LL003-3", "LL003-4", "...
## $ age                          <dbl> 11, 8, 32, 40, 40, 12, 4, 18, 30, 56, 6, 33, 15, 7, 12, 10, 3, 58, 3...
## $ residence                    <dbl> 11, 8, 4, 7, 4, 5, 4, 3, 2, 44, 6, 22, 14, 5, 12, 10, 2, 45, 33, 35,...
## $ tenido_malaria               <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "S...
## $ cuantas_tenido_malaria       <dbl> 4, 2, 3, 2, 2, 3, 1, 2, 1, 2, 1, 5, 2, 1, 0, 2, 0, 6, 7, 2, 3, 3, 4,...
## $ opciones_tenido_malaria      <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "S...
## $ recibio_tratamiento_malaria  <chr> "Mas de una vez", "Mas de una vez", "Mas de una vez", "Mas de una ve...
## $ cantidad_tratamiento_malaria <dbl> 1, 1, 3, NA, NA, NA, NA, NA, NA, NA, NA, 5, 2, 0, 0, 2, 0, 6, 7, 2, ...
## $ epi_malaria_ultimos_meses    <chr> "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No Sabe", "N...
## $ epi_meses_ultima_malaria     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2,...
## $ epi_especie_causo_malaria    <chr> "P. Vivax", "P. Vivax", "P. Vivax", "P. Vivax", "P. Vivax", "P. Viva...
## $ epi_alguien_tuvo_malaria     <chr> "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunc...
```

```r
unap6a %>% 
  group_by(age, residence, cuantas_tenido_malaria) %>% #dplyr::count(sort = T)
  dplyr::summarise(n=n()) %>%
  mutate(prop= n/sum(n)) %>%
  ggplot(aes(age, cuantas_tenido_malaria, colour=residence)) + 
  geom_point() + 
  scale_color_viridis() +
  labs(title="episodios de malaria")
```

<img src="01-complete_files/figure-html/unnamed-chunk-100-1.png" style="display: block; margin: auto;" />

#### tratamiento

- **`ndias_drogas_tratamiento_03`** is a inconsistent variable
- remove variables containing **`_tratamiento_03`** due to full of NA's and the shown inconsistence.



```r
unap6a %>% 
  group_by(drogas_tratamiento_03, ndias_drogas_tratamiento_03) %>% dplyr::count(sort = T)
```

```
## Source: local data frame [30 x 3]
## Groups: drogas_tratamiento_03 [4]
## 
## # A tibble: 30 x 3
##    drogas_tratamiento_03 ndias_drogas_tratamiento_03     n
##                    <chr>                       <dbl> <int>
##  1                  <NA>                           0   932
##  2            primaquina                           0     2
##  3                  <NA>                          NA     2
##  4            artesunato                           3     1
##  5            primaquina                           1     1
##  6            Primaquina                           2     1
##  7                  <NA>                           3     1
##  8                  <NA>                           4     1
##  9                  <NA>                           5     1
## 10                  <NA>                           6     1
## # ... with 20 more rows
```

```r
unap6a <- unap6a %>% 
  #dplyr::select(-dplyr::contains("tratamiento_03")) %>% 
  #dplyr::select(1, dplyr::contains("tratamiento")) %>% #str() #dplyr::count(busco_ayuda_tratamiento)
  mutate(no_busco_ayuda_tratamiento= str_replace(no_busco_ayuda_tratamiento,
                                                 "\\d+\\.\\s", "")) %>% 
  #dplyr::count(no_busco_ayuda_tratamiento)
  mutate(donde_busco_tratamiento= str_replace(donde_busco_tratamiento,
                                              "\\d+\\.\\s", "")) %>% 
  #dplyr::count(donde_busco_tratamiento)
  mutate(respuesta_drogas_tratamiento=str_replace(respuesta_drogas_tratamiento,
                                                  "\\d+\\.\\s","")) # %>% 
  #dplyr::count(respuesta_drogas_tratamiento)
  #dplyr::count(drogas_tratamiento_01)
  #str()

unap6a %>% 
  dplyr::select(1, dplyr::contains("tratamiento")) %>% 
  glimpse()
```

```
## Observations: 963
## Variables: 13
## $ id                           <chr> "LL001-2", "LL001-3", "LL003-1", "LL003-10", "LL003-3", "LL003-4", "...
## $ recibio_tratamiento_malaria  <chr> "Mas de una vez", "Mas de una vez", "Mas de una vez", "Mas de una ve...
## $ cantidad_tratamiento_malaria <dbl> 1, 1, 3, NA, NA, NA, NA, NA, NA, NA, NA, 5, 2, 0, 0, 2, 0, 6, 7, 2, ...
## $ busco_ayuda_tratamiento      <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "S...
## $ no_busco_ayuda_tratamiento   <chr> "No pensaba que era un problema serio", "No pensaba que era un probl...
## $ donde_busco_tratamiento      <chr> "Posta MINSA /CCSS MINSA", "Posta MINSA /CCSS MINSA", "Posta MINSA /...
## $ respuesta_drogas_tratamiento <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "N...
## $ drogas_tratamiento_01        <fctr> cloroquina, cloroquina, cloroquina, cloroquina, cloroquina, cloroqu...
## $ ndias_drogas_tratamiento_01  <dbl> 4, 5, 3, 3, 4, 3, 5, 3, 3, 3, 3, 0, 3, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3,...
## $ drogas_tratamiento_02        <fctr> primaquina, primaquina, primaquina, primaquina, primaquina, primaqu...
## $ ndias_drogas_tratamiento_02  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 4, 4, 4, 4, 4,...
## $ drogas_tratamiento_03        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ ndias_drogas_tratamiento_03  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
```

```r
#unap6a %>% dplyr::count(drogas_tratamiento_01, sort = T)
unap6a %>% 
  group_by(drogas_tratamiento_01, ndias_drogas_tratamiento_01) %>% 
  dplyr::count(sort = T)
```

```
## Source: local data frame [18 x 3]
## Groups: drogas_tratamiento_01 [7]
## 
## # A tibble: 18 x 3
##    drogas_tratamiento_01 ndias_drogas_tratamiento_01     n
##                   <fctr>                       <dbl> <int>
##  1                    NA                           0   390
##  2            cloroquina                           0   253
##  3            cloroquina                           3   239
##  4            artesunato                           3    18
##  5            primaquina                           7    13
##  6            cloroquina                           2    11
##  7                    NA                           3     8
##  8            artesunato                           0     6
##  9            cloroquina                           1     6
## 10 artesunato mefloquina                           3     3
## 11            cloroquina                           4     3
## 12                    NA                           2     3
## 13            cloroquina                           5     2
## 14            cloroquina                           7     2
## 15                    NA                           1     2
## 16                    NA                           7     2
## 17              FANSIDAR                           1     1
## 18               quinina                           3     1
```

```r
#unap6a %>% dplyr::count(drogas_tratamiento_02, sort = T)
unap6a %>% 
  group_by(drogas_tratamiento_02, ndias_drogas_tratamiento_02) %>% 
  dplyr::count(sort = T)
```

```
## Source: local data frame [17 x 3]
## Groups: drogas_tratamiento_02 [5]
## 
## # A tibble: 17 x 3
##    drogas_tratamiento_02 ndias_drogas_tratamiento_02     n
##                   <fctr>                       <dbl> <int>
##  1            primaquina                           0   367
##  2                    NA                           0   290
##  3            primaquina                           7   123
##  4            primaquina                           4    69
##  5                    NA                           2    27
##  6                    NA                           1    25
##  7            mefloquina                           7    14
##  8            cloroquina                           3    13
##  9            primaquina                           2     9
## 10            primaquina                           5     7
## 11            primaquina                           3     6
## 12            mefloquina                           2     5
## 13            primaquina                           1     4
## 14          clindamicina                           4     1
## 15          clindamicina                           5     1
## 16            mefloquina                           3     1
## 17                    NA                           3     1
```


#### enf

- remove **3** variables due to NA's
- `diabetes` and `cardiaca` with factor inconsistencies
- `enf_..._si` and `enf_..._no` were merge into `enf_...` for each disease
- remove **5** more variables after merging
- show a combination of chronic diseases frequencies

- inconsistency between `enfermedad_cronica` and `enf_...` variables


```r
unap6a <- unap6a %>% #str()
  dplyr::select(-enf_especifique_neurologica,
         -enf_especifique_pulmonar,
         -enf_especifique_cardiaca) %>% 
  mutate_at(vars(enf_diabetes_si:enf_renal_no), factor) %>% 
  #dplyr::select(1, dplyr::contains("enf")) %>% glimpse()
  
  mutate(enf_diabetes_si=fct_recode(enf_diabetes_si,"si"="1","no"="0"), 
         enf_diabetes_no=fct_recode(enf_diabetes_no,"no"="1","si"="0")) %>% 
  #dplyr::select(enf_diabetes_si,enf_diabetes_no) %>% count()
  unite(enf_diabetes, enf_diabetes_si, enf_diabetes_no, sep = "") %>% 
  #dplyr::count(enf_diabetes)
  mutate(enf_diabetes= str_replace_all(enf_diabetes, "nono","no")) %>% 
  #dplyr::count(enf_diabetes)
  mutate(enf_diabetes= str_replace_all(enf_diabetes, "..si|si..","si")) %>% 
  #dplyr::count(enf_diabetes)

  mutate(enf_neurologica_si=fct_recode(enf_neurologica_si,"si"="1","no"="0"),
         enf_neurologica_no=fct_recode(enf_neurologica_no,"no"="1","si"="0")) %>% 
  #dplyr::select(enf_neurologica_si,enf_neurologica_no) %>% count()
  unite(enf_neurologica, enf_neurologica_si, enf_neurologica_no, sep = "") %>% 
  #dplyr::count(enf_neurologica)
  mutate(enf_neurologica= str_replace_all(enf_neurologica, "nono","no")) %>% 
  #dplyr::count(enf_neurologica)
  mutate(enf_neurologica= str_replace_all(enf_neurologica, "..si|si..","si")) %>% 
  #dplyr::count(enf_neurologica)
    
  mutate(enf_pulmonar_si=fct_recode(enf_pulmonar_si,"si"="1","no"="0"), 
         enf_pulmonar_no=fct_recode(enf_pulmonar_no,"no"="1","si"="0")) %>% 
  #dplyr::select(enf_pulmonar_si,enf_pulmonar_no) %>% count()
  unite(enf_pulmonar, enf_pulmonar_si, enf_pulmonar_no, sep = "") %>% 
  #dplyr::count(enf_pulmonar)
  mutate(enf_pulmonar= str_replace_all(enf_pulmonar, "nono","no")) %>% 
  #dplyr::count(enf_pulmonar)
  mutate(enf_pulmonar= str_replace_all(enf_pulmonar, "..si|si..","si")) %>% 
  #dplyr::count(enf_pulmonar)
  
  mutate(enf_cardiaca_si=fct_recode(enf_cardiaca_si,"si"="1","no"="0"), 
         enf_cardiaca_no=fct_recode(enf_cardiaca_no,"no"="1","si"="0")) %>% 
  #dplyr::select(enf_cardiaca_si,enf_cardiaca_no) %>% count()
  unite(enf_cardiaca, enf_cardiaca_si, enf_cardiaca_no, sep = "") %>% 
  #dplyr::count(enf_cardiaca)
  mutate(enf_cardiaca= str_replace_all(enf_cardiaca, "nono|NAno|noNA","no")) %>% 
  #dplyr::count(enf_cardiaca)
  mutate(enf_cardiaca= str_replace_all(enf_cardiaca, "NANA",replacement = NA_character_)) %>% 
  #dplyr::count(enf_cardiaca)
  mutate(enf_cardiaca= str_replace_all(enf_cardiaca, "..si|si..","si")) %>% 
  #dplyr::count(enf_cardiaca)

  mutate(enf_renal_si=fct_recode(enf_renal_si,"si"="1","no"="0"), 
         enf_renal_no=fct_recode(enf_renal_no,"no"="1","si"="0")) %>% 
  #dplyr::select(enf_renal_si,enf_renal_no) %>% count()
  unite(enf_renal, enf_renal_si, enf_renal_no, sep = "") %>% 
  #dplyr::count(enf_renal)
  mutate(enf_renal= str_replace_all(enf_renal, "nono","no")) %>% 
  #dplyr::count(enf_renal)
  mutate(enf_renal= str_replace_all(enf_renal, "..si|si..","si")) 

unap6a %>% #dplyr::count(enf_renal)
  
  #dplyr::count(enf_especifique_renal)
  #dplyr::count(enf_especifique_otro)
  #dplyr::count(enf_tomo_medicinas_parasitos)
  #dplyr::count(enf_medicinas_parasitos)
  
  dplyr::select(1, dplyr::contains("enf")) %>% glimpse()
```

```
## Observations: 963
## Variables: 11
## $ id                           <chr> "LL001-2", "LL001-3", "LL003-1", "LL003-10", "LL003-3", "LL003-4", "...
## $ enfermedad_cronica           <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "S...
## $ enf_diabetes                 <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "s...
## $ enf_neurologica              <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "n...
## $ enf_pulmonar                 <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "n...
## $ enf_cardiaca                 <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "n...
## $ enf_renal                    <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "n...
## $ enf_especifique_renal        <fctr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ enf_especifique_otro         <fctr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ enf_tomo_medicinas_parasitos <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "S...
## $ enf_medicinas_parasitos      <fctr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, albendazol, albendazol,...
```

```r
unap6a %>%
  dplyr::select(enfermedad_cronica, enf_diabetes, enf_neurologica, enf_pulmonar, enf_cardiaca, enf_renal) %>% 
  group_by(enfermedad_cronica, enf_diabetes, enf_neurologica, enf_pulmonar, enf_cardiaca, enf_renal) %>% count()
```

```
##    enfermedad_cronica enf_diabetes enf_neurologica enf_pulmonar enf_cardiaca enf_renal freq
## 1                  No           no              no           no           no        no  905
## 2                  No           no              no           no           no        si    2
## 3                  No           no              no           no           si        no    3
## 4                  No           no              no           no         <NA>        no    2
## 5                  No           si              no           no           no        no   12
## 6                  Si           no              no           no           no        no   31
## 7                  Si           no              no           no           no        si    3
## 8                  Si           no              no           no           si        no    2
## 9                  Si           no              no           si           no        no    2
## 10                 Si           si              no           no           no        no    1
```

#### epi_


```r
unap6a <- unap6a %>%
  dplyr::select(-epi_especifique_viajo_fuera_villa) %>% 
  mutate(epi_tipo_fuente_agua= str_replace(epi_tipo_fuente_agua,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_tipo_fuente_agua)
  mutate(epi_distancia_fuente_agua= str_replace(epi_distancia_fuente_agua,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_distancia_fuente_agua)
  
  mutate(epi_uso_repelente_mosquito= str_replace(epi_uso_repelente_mosquito,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_uso_repelente_mosquito)
  mutate(epi_uso_mangas_largas= str_replace(epi_uso_mangas_largas,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_uso_mangas_largas)
  mutate(epi_uso_ropa_impregnada= str_replace(epi_uso_ropa_impregnada,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_uso_ropa_impregnada)
  mutate(epi_cercania_fuente_agua= str_replace(epi_cercania_fuente_agua,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_cercania_fuente_agua)
  mutate(epi_estuvo_campo_antes= str_replace(epi_estuvo_campo_antes,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_estuvo_campo_antes)
  
  mutate(epi_uso_redes_cama= str_replace(epi_uso_redes_cama,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_uso_redes_cama)
  mutate(epi_duerme_ventanas_abiertas= str_replace(epi_duerme_ventanas_abiertas,"\\d+\\.\\s", "")) %>%
  #dplyr::count(epi_duerme_ventanas_abiertas)
  mutate(epi_duerme_cerca_monte= str_replace(epi_duerme_cerca_monte,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_duerme_cerca_monte)
  mutate(epi_rocia_con_insecticida= str_replace(epi_rocia_con_insecticida,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_rocia_con_insecticida)
  mutate(epi_ultimas_semanas_viajo= str_replace(epi_ultimas_semanas_viajo,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_ultimas_semanas_viajo)
  
  mutate(epi_frecuencia_rocia_casa= str_replace(epi_frecuencia_rocia_casa,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_frecuencia_rocia_casa)
  
  mutate(epi_estado_campos_agricultura= str_replace(epi_estado_campos_agricultura,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_estado_campos_agricultura)
  mutate(epi_estado_canal_agua= str_replace(epi_estado_canal_agua,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_estado_canal_agua)
  mutate(epi_adquirio_redes_dormir= str_replace(epi_adquirio_redes_dormir,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_adquirio_redes_dormir)
  
  mutate(epi_uso_red_dormir= str_replace(epi_uso_red_dormir,"\\d+\\.\\s", "")) %>% 
  #dplyr::count(epi_uso_red_dormir)
  mutate(epi_redes_usadas_actualmente= str_replace(epi_redes_usadas_actualmente,"\\d+\\.\\s", "")) 

unap6a %>%#dplyr::count(epi_redes_usadas_actualmente)
  
  #dplyr::count(epi_meses_ultima_malaria)
  #dplyr::count(epi_especie_causo_malaria)
  #dplyr::count(epi_cantidad_redes_usadas_actualmente)
  dplyr::select(1, dplyr::contains("epi_")) %>% glimpse()
```

```
## Observations: 963
## Variables: 28
## $ id                                    <chr> "LL001-2", "LL001-3", "LL003-1", "LL003-10", "LL003-3", "LL...
## $ epi_cerca_fuente_agua                 <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ epi_tipo_fuente_agua                  <chr> "Rio", "Rio", "Rio", "Rio", "Rio", "Rio", "Rio", "Rio", "Ri...
## $ epi_distancia_fuente_agua             <chr> "Menor de 100 metros", "Menor de 100 metros", "Menor de 100...
## $ epi_malaria_ultimos_meses             <chr> "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No ...
## $ epi_viajo_fuera_villa                 <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ epi_meses_ultima_malaria              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 2, 2,...
## $ epi_especie_causo_malaria             <chr> "P. Vivax", "P. Vivax", "P. Vivax", "P. Vivax", "P. Vivax",...
## $ epi_uso_repelente_mosquito            <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_uso_mangas_largas                 <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_uso_ropa_impregnada               <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_cercania_fuente_agua              <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_estuvo_campo_antes                <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_uso_redes_cama                    <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_duerme_ventanas_abiertas          <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_duerme_cerca_monte                <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_rocia_con_insecticida             <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_ultimas_semanas_viajo             <chr> "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunc...
## $ epi_numero_veces_viajo                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ epi_frecuencia_rocia_casa             <chr> "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunc...
## $ epi_alguien_tuvo_malaria              <chr> "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunc...
## $ epi_estado_campos_agricultura         <chr> "Siempre", "Siempre", "Siempre", "Siempre", "Siempre", "Sie...
## $ epi_estado_canal_agua                 <chr> "Siempre", "Siempre", "Siempre", "Siempre", "Siempre", "Sie...
## $ epi_adquirio_redes_dormir             <chr> "Si, redes de fabrica con insecticida", "Si, redes de fabri...
## $ epi_cantidad_redes_adquirio_dormir    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4,...
## $ epi_uso_red_dormir                    <chr> "Desde siempre", "Desde siempre", "Desde siempre", "Desde s...
## $ epi_redes_usadas_actualmente          <chr> "Si, redes de fabrica sin insecticida", "Si, redes de fabri...
## $ epi_cantidad_redes_usadas_actualmente <dbl> 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 3, 2, 2, 2, 2, 2, 2, 4, 4,...
```

#### no patterns


```r
unap6a <- unap6a %>%
  dplyr::select(-email) %>% 
  dplyr::select(-fecha_actualizacion) %>%  
  mutate(combustible_cocinar= str_replace(combustible_cocinar,"\\d+\\.\\s", "")) %>% 
  mutate(pertenencia_casa= str_replace(pertenencia_casa,"\\d+\\.\\s", "")) %>% 
  mutate(tipo_casa= str_replace(tipo_casa,"\\d+\\.\\s", "")) %>% 
  mutate(fuente_agua= str_replace(fuente_agua,"\\d+\\.\\s", "")) %>% 
  mutate(relacion_jefe_familia= str_replace(relacion_jefe_familia,"\\d+\\.\\s", "")) 

unap6a %>%
  dplyr::select(1, 
         -dplyr::contains("ah_"),
         -dplyr::contains("otro_"),
         -dplyr::contains("trabajo"),
         -dplyr::contains("material"),
         -dplyr::contains("banio"),
         -dplyr::contains("malaria"),
         -dplyr::contains("tratamiento"),
         -dplyr::contains("enf"),
         -dplyr::contains("epi_")) %>% 
  dplyr::select(-starts_with("ah_")) %>%
  #dplyr::select(-email) %>% 
  #dplyr::select(-fecha_actualizacion) %>%
  #dplyr::count(estado_civil, sort = T)
  #dplyr::count(nivel_educacion, sort = T)
  #dplyr::count(estado, sort = T)
  glimpse()
```

```
## Observations: 963
## Variables: 24
## $ id                           <chr> "LL001-2", "LL001-3", "LL003-1", "LL003-10", "LL003-3", "LL003-4", "...
## $ barrido                      <chr> "Barrido01", "Barrido01", "Barrido01", "Barrido01", "Barrido01", "Ba...
## $ community                    <chr> "Llanchama", "Llanchama", "Llanchama", "Llanchama", "Llanchama", "Ll...
## $ referencia                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ grupos_personas              <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ direccion                    <chr> "Calle Nueva Jerusalen", "Calle Nueva Jerusalen", "Calle. Jerusalen"...
## $ telefono                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ celular                      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ age                          <dbl> 11, 8, 32, 40, 40, 12, 4, 18, 30, 56, 6, 33, 15, 7, 12, 10, 3, 58, 3...
## $ sex                          <fctr> F, M, F, M, M, M, M, M, M, M, F, F, F, F, F, F, M, F, M, F, F, F, M...
## $ estado_civil                 <chr> "Soltero", "Soltero", "Conviviente", "Conviviente", "Conviviente", "...
## $ nivel_educacion              <fctr> Primaria Incompleta, Primaria Incompleta, Primaria Completa, Secund...
## $ residence                    <dbl> 11, 8, 4, 7, 4, 5, 4, 3, 2, 44, 6, 22, 14, 5, 12, 10, 2, 45, 33, 35,...
## $ electricidad_red_publica     <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "Si", "Si", "N...
## $ combustible_cocinar          <chr> "Lenha", "Lenha", "Lenha", "Lenha", "Lenha", "Lenha", "Lenha", "Lenh...
## $ pertenencia_casa             <chr> "Propia", "Propia", "Propia", "Propia", "Propia", "Propia", "Propia"...
## $ tipo_casa                    <chr> "Casa independiente", "Casa independiente", "Casa independiente", "C...
## $ cantidad_habitaciones        <dbl> 3, 2, 3, 1, 7, 4, 3, 3, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1,...
## $ cantidad_habitaciones_dormir <dbl> 3, 2, 3, 1, 7, 4, 3, 3, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1,...
## $ fuente_agua                  <chr> "Pozo", "Pozo", "Pozo", "Pozo", "Pozo", "Pozo", "Pozo", "Pozo", "Poz...
## $ relacion_jefe_familia        <chr> "Hijo o Hijastro quien es soltero y tiene hijos", "Hijo o Hijastro q...
## $ actualmente_estudiando       <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "N...
## $ fecha_registro               <chr> "09/08/2015 11:35:48 a.m.", "09/08/2015 11:35:48 a.m.", "09/08/2015 ...
## $ estado                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
```

## Merge datasets

- new DB names:
    + **`epid`**: current `unap6a` of Epidemiological variables **(963x88)**
    + **`unap`**: previous `unap0` **(960x16)** stored in `data/03-covariat-uap.rds` with:
        + undated `age`, `sex`, `community`
        + consensus `micro`, `pcr`
    + **`sero`**: previous `std.uty` **(969x5)** stored in `data/02-dtfilter-uty.rds` with: 
        + `Ab.units`, `mean.OD` per `Specie`

### rows differ

- all the datasets share **959 core ID's**.

TO DO:

- CHECK IF **intersections** outside the core **959 samples** have ID typos.


```r
unapid <- unap0 %>% dplyr::select(id)      # 960
seroid <- std.uty %>% dplyr::select(id=ID) # 969
epidid <- unap6a %>% dplyr::select(id)     # 963

id_unap= unlist(as.list(unapid))
id_sero= unlist(as.list(seroid))
id_epid= unlist(as.list(epidid))

tmp <- gplots::venn(list(unap=id_unap,epid=id_epid,sero=id_sero))
```

<img src="01-complete_files/figure-html/unnamed-chunk-105-1.png" style="display: block; margin: auto;" />
**INTERSECTIONS**

- **epid** only

```r
attr(tmp, "intersections")$epid
```

```
## [1] "LL029-7" "ZG096-5" "ZG141-2" "ZG178-1"
```
- **sero** only

```r
attr(tmp, "intersections")$sero
```

```
## [1] "LL003-6" "LL062-1" "LL062-5" "NN067-7" "NN192-1" "NN192-4" "NN192-7" "ZG146-5" "ZG165-1"
```
- **unap:sero** only

```r
attr(tmp, "intersections")$`unap:sero`
```

```
## [1] "ZG178-11"
```

### merge

- Merge **`unap + sero`** stored in `data/03-covariat-uty.csv` (`std.uty.cov`) 
with current **`epid`** (`unap6a`)
- This will be the final unified dataset.

dicoveries:

- **`epid`** and **`unap + sero`** differs in `age`, `sex`, `community`
    + `age`, `sex`, `community` were removed from **`epid`** 
    + logical vectors such as `_0` and `_n1` were removed from **`unap+sero`** 
    previous merging


```r
std.uty.cov.new <- std.uty.cov %>% 
  dplyr::select(-sex_0, 
                -micro_n1, 
                #-sex_1, 
                #-micro_1,
                #-pcr_1, 
                #-age_1, 
                -pcr_n1,
                -age_0) %>% 
  mutate(
    micro_1=fct_recode(micro_1,
                       "NG"="0","PV"="1","PF"="2","PV/PF"="3"),
    micro_n=fct_recode(micro_n,
                       "NG"="0","PV"="1","PF"="2","PV/PF"="3"),
    micro_c=fct_recode(micro_c,
                       "NG"="0","PV"="1","PF"="2","PV/PF"="3"),
    pcr_1=fct_recode(pcr_1,
                       "NG"="0","PV"="1","PF"="2","PV/PF"="3"),
    pcr_n=fct_recode(pcr_n,
                       "NG"="0","PV"="1","PF"="2","PV/PF"="3"),
    pcr_c=fct_recode(pcr_c,
                       "NG"="0","PV"="1","PF"="2","PV/PF"="3")
  )

unap6b <- unap6a %>% 
  dplyr::select(-age, -sex, -community) %>% 
  full_join(
    std.uty.cov.new
            , by="id") # 973
unap6b %>% glimpse()
```

```
## Observations: 973
## Variables: 100
## $ id                                    <chr> "LL001-2", "LL001-3", "LL003-1", "LL003-10", "LL003-3", "LL...
## $ barrido                               <chr> "Barrido01", "Barrido01", "Barrido01", "Barrido01", "Barrid...
## $ referencia                            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ grupos_personas                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ direccion                             <chr> "Calle Nueva Jerusalen", "Calle Nueva Jerusalen", "Calle. J...
## $ telefono                              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ celular                               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ estado_civil                          <chr> "Soltero", "Soltero", "Conviviente", "Conviviente", "Conviv...
## $ nivel_educacion                       <fctr> Primaria Incompleta, Primaria Incompleta, Primaria Complet...
## $ trabajo                               <fctr> desempleado, desempleado, Ama de casa, Agricultor, Agricul...
## $ residence                             <dbl> 11, 8, 4, 7, 4, 5, 4, 3, 2, 44, 6, 22, 14, 5, 12, 10, 2, 45...
## $ electricidad_red_publica              <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "Si",...
## $ combustible_cocinar                   <chr> "Lenha", "Lenha", "Lenha", "Lenha", "Lenha", "Lenha", "Lenh...
## $ ah_radio                              <fctr> si, si, si, si, si, si, si, si, si, si, si, si, si, si, si...
## $ ah_television                         <fctr> no, no, no, si, no, si, si, si, si, si, si, si, si, si, si...
## $ ah_estereo                            <fctr> no, no, no, no, no, no, no, no, no, no, no, no, no, no, no...
## $ ah_refrigerador                       <fctr> si, no, si, si, si, si, no, si, si, no, si, si, si, si, no...
## $ ah_motocicleta                        <fctr> no, no, no, no, no, no, no, no, no, no, no, no, no, no, no...
## $ ah_mototaxi                           <fctr> si, si, si, si, si, si, si, si, si, si, si, si, si, si, si...
## $ ah_auto                               <fctr> no, no, no, no, no, no, no, no, no, no, no, no, no, no, no...
## $ pertenencia_casa                      <chr> "Propia", "Propia", "Propia", "Propia", "Propia", "Propia",...
## $ tipo_casa                             <chr> "Casa independiente", "Casa independiente", "Casa independi...
## $ cantidad_habitaciones                 <dbl> 3, 2, 3, 1, 7, 4, 3, 3, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1,...
## $ cantidad_habitaciones_dormir          <dbl> 3, 2, 3, 1, 7, 4, 3, 3, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1,...
## $ material_pared                        <chr> "Madera", "Madera", "Madera", "Madera", "Madera", "Madera",...
## $ material_piso                         <chr> "Tierra/arena", "Tierra/arena", "Tierra/arena", "Tierra/are...
## $ fuente_agua                           <chr> "Pozo", "Pozo", "Pozo", "Pozo", "Pozo", "Pozo", "Pozo", "Po...
## $ banio_conexion                        <chr> "Sistema publico dentro de la casa", "Sistema publico dentr...
## $ tenido_malaria                        <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ cuantas_tenido_malaria                <dbl> 4, 2, 3, 2, 2, 3, 1, 2, 1, 2, 1, 5, 2, 1, 0, 2, 0, 6, 7, 2,...
## $ opciones_tenido_malaria               <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ recibio_tratamiento_malaria           <chr> "Mas de una vez", "Mas de una vez", "Mas de una vez", "Mas ...
## $ cantidad_tratamiento_malaria          <dbl> 1, 1, 3, NA, NA, NA, NA, NA, NA, NA, NA, 5, 2, 0, 0, 2, 0, ...
## $ busco_ayuda_tratamiento               <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ no_busco_ayuda_tratamiento            <chr> "No pensaba que era un problema serio", "No pensaba que era...
## $ donde_busco_tratamiento               <chr> "Posta MINSA /CCSS MINSA", "Posta MINSA /CCSS MINSA", "Post...
## $ respuesta_drogas_tratamiento          <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ drogas_tratamiento_01                 <fctr> cloroquina, cloroquina, cloroquina, cloroquina, cloroquina...
## $ ndias_drogas_tratamiento_01           <dbl> 4, 5, 3, 3, 4, 3, 5, 3, 3, 3, 3, 0, 3, 0, 0, 0, 0, 0, 3, 3,...
## $ drogas_tratamiento_02                 <fctr> primaquina, primaquina, primaquina, primaquina, primaquina...
## $ ndias_drogas_tratamiento_02           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 4, 4,...
## $ drogas_tratamiento_03                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ ndias_drogas_tratamiento_03           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ enfermedad_cronica                    <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ enf_diabetes                          <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no",...
## $ enf_neurologica                       <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no",...
## $ enf_pulmonar                          <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no",...
## $ enf_cardiaca                          <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no",...
## $ enf_renal                             <chr> "no", "no", "no", "no", "no", "no", "no", "no", "no", "no",...
## $ enf_especifique_renal                 <fctr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ enf_especifique_otro                  <fctr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
## $ enf_tomo_medicinas_parasitos          <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ enf_medicinas_parasitos               <fctr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, albendazol, al...
## $ epi_cerca_fuente_agua                 <chr> "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si", "Si",...
## $ epi_tipo_fuente_agua                  <chr> "Rio", "Rio", "Rio", "Rio", "Rio", "Rio", "Rio", "Rio", "Ri...
## $ epi_distancia_fuente_agua             <chr> "Menor de 100 metros", "Menor de 100 metros", "Menor de 100...
## $ epi_malaria_ultimos_meses             <chr> "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No Sabe", "No ...
## $ epi_viajo_fuera_villa                 <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ epi_meses_ultima_malaria              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 2, 2,...
## $ epi_especie_causo_malaria             <chr> "P. Vivax", "P. Vivax", "P. Vivax", "P. Vivax", "P. Vivax",...
## $ epi_uso_repelente_mosquito            <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_uso_mangas_largas                 <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_uso_ropa_impregnada               <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_cercania_fuente_agua              <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_estuvo_campo_antes                <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_uso_redes_cama                    <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_duerme_ventanas_abiertas          <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_duerme_cerca_monte                <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_rocia_con_insecticida             <chr> "No Aplica", "No Aplica", "No Aplica", "No Aplica", "No Apl...
## $ epi_ultimas_semanas_viajo             <chr> "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunc...
## $ epi_numero_veces_viajo                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ epi_frecuencia_rocia_casa             <chr> "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunc...
## $ epi_alguien_tuvo_malaria              <chr> "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunca", "Nunc...
## $ epi_estado_campos_agricultura         <chr> "Siempre", "Siempre", "Siempre", "Siempre", "Siempre", "Sie...
## $ epi_estado_canal_agua                 <chr> "Siempre", "Siempre", "Siempre", "Siempre", "Siempre", "Sie...
## $ epi_adquirio_redes_dormir             <chr> "Si, redes de fabrica con insecticida", "Si, redes de fabri...
## $ epi_cantidad_redes_adquirio_dormir    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4,...
## $ epi_uso_red_dormir                    <chr> "Desde siempre", "Desde siempre", "Desde siempre", "Desde s...
## $ epi_redes_usadas_actualmente          <chr> "Si, redes de fabrica sin insecticida", "Si, redes de fabri...
## $ epi_cantidad_redes_usadas_actualmente <dbl> 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 3, 2, 2, 2, 2, 2, 2, 4, 4,...
## $ relacion_jefe_familia                 <chr> "Hijo o Hijastro quien es soltero y tiene hijos", "Hijo o H...
## $ actualmente_estudiando                <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No",...
## $ trabajo_paga_ultima                   <chr> "No", "No", "Si", "Si", "Si", "No", "No", "Si", "Si", "Si",...
## $ fecha_registro                        <chr> "09/08/2015 11:35:48 a.m.", "09/08/2015 11:35:48 a.m.", "09...
## $ estado                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ Ab.unit_Pfal                          <dbl> 0.7573890, 0.8926718, 32.4810929, 11.7938932, 0.2446186, 0....
## $ Ab.unit_Pviv                          <dbl> 7.4776915, 1.0842230, 1.1617778, 16.0984292, 9.2495689, 9.3...
## $ mean.OD_Pfal                          <dbl> 0.0585, 0.0605, 0.2975, 0.1580, 0.0520, 0.0545, 0.0510, 0.2...
## $ mean.OD_Pviv                          <dbl> 0.1930, 0.0790, 0.0810, 0.2915, 0.2985, 0.3000, 0.6795, 0.0...
## $ sex_1                                 <fctr> Female, Male, Female, Male, Male, Male, Male, Male, Male, ...
## $ sex_4                                 <fctr> Female, Male, Female, Male, Male, Male, Male, Male, Male, ...
## $ age_1                                 <dbl> 11, 8, 32, 40, 40, 12, 4, 18, 30, 56, 6, 33, 15, 7, 12, 10,...
## $ age_4                                 <dbl> 11, 8, 32, 40, 9, 6, 4, 29, 30, 56, 6, 33, 15, 7, 12, 10, 3...
## $ micro_c                               <fctr> NA, NA, NA, NA, NA, PV, NA, NA, NG, NA, NA, NA, PV, NA, NA...
## $ micro_1                               <fctr> NA, NA, NA, NA, NA, PV, NA, NA, NG, NA, NA, NA, PV, NA, NA...
## $ micro_n                               <fctr> NA, NA, NA, NA, NA, PV, NA, NA, NA, NA, NA, NA, PV, NA, NA...
## $ pcr_c                                 <fctr> NA, NA, NA, NA, NA, PV, NA, NA, PV, NA, NA, NA, PV, NA, NA...
## $ pcr_1                                 <fctr> NA, NA, NA, NA, NA, PV, NA, NA, PV, NA, NA, NA, PV, NA, NA...
## $ pcr_n                                 <fctr> NA, NA, NA, NA, NA, PV, NA, NA, PV, NA, NA, NA, PV, NA, NA...
## $ community_1                           <fctr> Llanchama, Llanchama, Llanchama, Llanchama, Llanchama, Lla...
```

```r
unap6c <- unap6a %>% 
  dplyr::select(-age, -sex, -community) %>% 
  inner_join(
    std.uty.cov.new
             , by="id")# 959
#unap6c %>% glimpse()
```

## Data dictionary

```r
datadic <- readxl::read_excel("data/UNAP-6-EPI-08feb2017.xlsx", sheet = 2)#SPANISH
#datadic <- readxl::read_excel("data/UNAP-6-EPI-08feb2017.xlsx", sheet = 3)#ENGLISH
whatyouwant <- setNames(as.character(datadic$`EXPLICACIÓN DE VARIABLE`), datadic$VARIABLE)
#http://stackoverflow.com/questions/19265172/converting-two-columns-of-a-data-frame-to-a-named-vector
unap6x <- Hmisc::upData(unap6c, labels = whatyouwant, force.single = F)
```

```
## Input object size:	 791752 bytes;	 100 variables	 959 observations
## New object size:	819840 bytes;	100 variables	959 observations
```

```r
unap6x <- Hmisc::upData(unap6x, 
                        labels = c(Ab.unit_Pfal="standardize AU Pfal",
                                   Ab.unit_Pviv="standardize AU Pviv",
                                   mean.OD_Pfal="raw mean OD Pfal",
                                   mean.OD_Pviv="raw mean OD Pviv",
                                   sex_4="gender-UNAP_UPDATE",
                                   age_1="age",
                                   age_4="age-UNAP_UPDATE",
                                   micro_1="microscopy UNAP",
                                   micro_n="microscopy NAMRU",
                                   micro_c="microscopy CONCENSUS",
                                   pcr_1="pcr UNAP",
                                   pcr_n="pcr NAMRU",
                                   pcr_c="pcr CONCENSUS"), 
                        force.single = F)
```

```
## Input object size:	 819840 bytes;	 100 variables	 959 observations
## New object size:	824328 bytes;	100 variables	959 observations
```


```r
#Hmisc::contents(unap6x)
Hmisc::html(Hmisc::contents(unap6x), maxlevels=10, levelType='table')
```

<!--html_preserve--><hr><h4>Data frame:unap6x</h4>959 observations and 100 variables, maximum # NAs:957&#8195;&#8195;
 <hr>
 <style>
 .hmisctable917190 {
 border: 1px solid gray;
 border-collapse: collapse;
 font-size: 100%;
 }
 .hmisctable917190 td {
 text-align: right;
 padding: 0 1ex 0 1ex;
 }
 .hmisctable917190 th {
 color: Black;
 text-align: center;
 padding: 0 1ex 0 1ex;
 font-weight: bold;
 }
 </style>
 <table class="hmisctable917190" border="1">
 <tr><th>Name</th><th>Labels</th><th>Levels</th><th>Class</th><th>Storage</th><th>NAs</th></tr>
 <tr><td>id</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>barrido</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>referencia</td><td></td><td></td><td></td><td>character</td><td>510</td></tr>
 <tr><td>grupos_personas</td><td>N° de familias que cocina en el hogar</td><td></td><td>numeric</td><td>double</td><td>  3</td></tr>
 <tr><td>direccion</td><td></td><td></td><td></td><td>character</td><td>  3</td></tr>
 <tr><td>telefono</td><td></td><td></td><td></td><td>double</td><td>957</td></tr>
 <tr><td>celular</td><td></td><td></td><td></td><td>double</td><td>941</td></tr>
 <tr><td>estado_civil</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>nivel_educacion</td><td></td><td><a href="#levels.nivel_educacion">10</a></td><td></td><td>integer</td><td>  0</td></tr>
 <tr><td>trabajo</td><td></td><td><a href="#levels.trabajo"> 6</a></td><td></td><td>integer</td><td>  0</td></tr>
 <tr><td>residence</td><td></td><td></td><td></td><td>double</td><td>  0</td></tr>
 <tr><td>electricidad_red_publica</td><td>el poblador está conectado a la red pública de elecrtricidad</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>combustible_cocinar</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>ah_radio</td><td>Tiene radio, Si=1 y No=0</td><td><a href="#levels.ah_radio"> 2</a></td><td></td><td>integer</td><td>  2</td></tr>
 <tr><td>ah_television</td><td>Tiene television, Si=1 y No=0</td><td><a href="#levels.ah_radio"> 2</a></td><td></td><td>integer</td><td>  0</td></tr>
 <tr><td>ah_estereo</td><td>Tiene estereo, Si=1 y No=0</td><td><a href="#levels.ah_radio"> 2</a></td><td></td><td>integer</td><td>  2</td></tr>
 <tr><td>ah_refrigerador</td><td>Tiene refrigerador, Si=1 y No=0</td><td><a href="#levels.ah_radio"> 2</a></td><td></td><td>integer</td><td>  2</td></tr>
 <tr><td>ah_motocicleta</td><td>Tiene motocicleta, Si=1 y No=0</td><td><a href="#levels.ah_radio"> 2</a></td><td></td><td>integer</td><td>  2</td></tr>
 <tr><td>ah_mototaxi</td><td>Tiene mototaxi, Si=1 y No=0</td><td><a href="#levels.ah_radio"> 2</a></td><td></td><td>integer</td><td>  2</td></tr>
 <tr><td>ah_auto</td><td>Tiene auto, Si=1 y No=0</td><td><a href="#levels.ah_auto"> 1</a></td><td></td><td>integer</td><td>  1</td></tr>
 <tr><td>pertenencia_casa</td><td>A quién pertenece la casa donde vive</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>tipo_casa</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>cantidad_habitaciones</td><td>N° de habitaciones de la casa</td><td></td><td>numeric</td><td>double</td><td>  0</td></tr>
 <tr><td>cantidad_habitaciones_dormir</td><td>N° de habitaciones usadas para dormir</td><td></td><td>numeric</td><td>double</td><td>  0</td></tr>
 <tr><td>material_pared</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>material_piso</td><td>De qué material está hecho el piso</td><td></td><td>character</td><td>character</td><td>  3</td></tr>
 <tr><td>fuente_agua</td><td>De qué lugar obtiene el agua</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>banio_conexion</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>tenido_malaria</td><td>Ha tenido malaria?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>cuantas_tenido_malaria</td><td>N° de veces que le dio malaria</td><td></td><td>numeric</td><td>double</td><td>  5</td></tr>
 <tr><td>opciones_tenido_malaria</td><td>Ha tenido malaria?</td><td></td><td>character</td><td>character</td><td>  1</td></tr>
 <tr><td>recibio_tratamiento_malaria</td><td>recibió tratamiento contra malaria</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>cantidad_tratamiento_malaria</td><td>N° de veces que recibió tratamiento</td><td></td><td>numeric</td><td>double</td><td> 75</td></tr>
 <tr><td>busco_ayuda_tratamiento</td><td>Buscó ayuda o tratamiento?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>no_busco_ayuda_tratamiento</td><td>Razón por la que no buscó ayuda o tratamiento</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>donde_busco_tratamiento</td><td>Lugar donde buscó tratamiento</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>respuesta_drogas_tratamiento</td><td>Conoce los nombres de las drogas administradas durante el tratamiento</td><td></td><td>character</td><td>character</td><td>  4</td></tr>
 <tr><td>drogas_tratamiento_01</td><td></td><td><a href="#levels.drogas_tratamiento_01"> 6</a></td><td></td><td>integer</td><td>404</td></tr>
 <tr><td>ndias_drogas_tratamiento_01</td><td></td><td></td><td></td><td>double</td><td>  0</td></tr>
 <tr><td>drogas_tratamiento_02</td><td></td><td><a href="#levels.drogas_tratamiento_02"> 4</a></td><td></td><td>integer</td><td>342</td></tr>
 <tr><td>ndias_drogas_tratamiento_02</td><td></td><td></td><td></td><td>double</td><td>  0</td></tr>
 <tr><td>drogas_tratamiento_03</td><td></td><td></td><td></td><td>character</td><td>954</td></tr>
 <tr><td>ndias_drogas_tratamiento_03</td><td></td><td></td><td></td><td>double</td><td>  2</td></tr>
 <tr><td>enfermedad_cronica</td><td>Tiene enfermedad crónica?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>enf_diabetes</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>enf_neurologica</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>enf_pulmonar</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>enf_cardiaca</td><td></td><td></td><td></td><td>character</td><td>  2</td></tr>
 <tr><td>enf_renal</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>enf_especifique_renal</td><td>Enfermedad renal especificada</td><td><a href="#levels.enf_especifique_renal"> 2</a></td><td></td><td>integer</td><td>954</td></tr>
 <tr><td>enf_especifique_otro</td><td>Otro tipo de enfermedad (especificada)</td><td><a href="#levels.enf_especifique_otro"> 7</a></td><td></td><td>integer</td><td>944</td></tr>
 <tr><td>enf_tomo_medicinas_parasitos</td><td>Tomó medicina para los parásitos en los últimos 12 meses</td><td></td><td>character</td><td>character</td><td>  9</td></tr>
 <tr><td>enf_medicinas_parasitos</td><td>Nombre de la medicina que tomó</td><td><a href="#levels.enf_medicinas_parasitos"> 3</a></td><td></td><td>integer</td><td>798</td></tr>
 <tr><td>epi_cerca_fuente_agua</td><td>Vive cerca a una fuente de agua</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_tipo_fuente_agua</td><td>Tipo de fuente de agua a la que vive ceca</td><td></td><td>character</td><td>character</td><td> 44</td></tr>
 <tr><td>epi_distancia_fuente_agua</td><td>Distacia a la fuente de agua desde el lugar donde vive</td><td></td><td>character</td><td>character</td><td> 44</td></tr>
 <tr><td>epi_malaria_ultimos_meses</td><td>Alguien en su casa tuvo malaria en los últimos 12 meses</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_viajo_fuera_villa</td><td>La persona con malaria viajó fuera de la villa?</td><td></td><td>character</td><td>character</td><td>  7</td></tr>
 <tr><td>epi_meses_ultima_malaria</td><td>Hace cuánto tiempo fue el último episodio de malaria en esa persona?</td><td></td><td>numeric</td><td>double</td><td> 11</td></tr>
 <tr><td>epi_especie_causo_malaria</td><td>Especie que causó la efermedada</td><td></td><td>character</td><td>character</td><td> 25</td></tr>
 <tr><td>epi_uso_repelente_mosquito</td><td>Usa repelente?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_uso_mangas_largas</td><td>Usa mangas largas?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_uso_ropa_impregnada</td><td>Usa ropa impregnada con insecticida?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_cercania_fuente_agua</td><td>Está cerca a las fuentes de agua al atardecer o amanecer?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_estuvo_campo_antes</td><td>Estuvo en el campo o monte al atardecer o amanecer?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_uso_redes_cama</td><td>Usa redes de cama?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_duerme_ventanas_abiertas</td><td>Duerme con las ventanas abiertas?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_duerme_cerca_monte</td><td>Duere cerca al monte?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_rocia_con_insecticida</td><td>Rocía su casa con insecticida?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_ultimas_semanas_viajo</td><td>Viajó en las úlrimas semanas?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_numero_veces_viajo</td><td>N° de veces que viajó?</td><td></td><td>numeric</td><td>double</td><td>  0</td></tr>
 <tr><td>epi_frecuencia_rocia_casa</td><td>Con qué frecuencia rocía su casa?</td><td></td><td>character</td><td>character</td><td>  1</td></tr>
 <tr><td>epi_alguien_tuvo_malaria</td><td>Durante su vida, alguién ha tenido malaria en su casa?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_estado_campos_agricultura</td><td>Ha estado en campos de agricultura?</td><td></td><td>character</td><td>character</td><td> 47</td></tr>
 <tr><td>epi_estado_canal_agua</td><td>Ha estado cerca a un canal de agua?</td><td></td><td>character</td><td>character</td><td>  4</td></tr>
 <tr><td>epi_adquirio_redes_dormir</td><td>Adquirió redes de dormir para su casa?</td><td></td><td>character</td><td>character</td><td>  3</td></tr>
 <tr><td>epi_cantidad_redes_adquirio_dormir</td><td>N° de redes que adquirió</td><td></td><td>numeric</td><td>double</td><td> 88</td></tr>
 <tr><td>epi_uso_red_dormir</td><td>Uso de esa red para dormir?</td><td></td><td>character</td><td>character</td><td>  4</td></tr>
 <tr><td>epi_redes_usadas_actualmente</td><td>Tipo de red para dormir usada</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>epi_cantidad_redes_usadas_actualmente</td><td>N° de redes para dormir que se usan actualmente</td><td></td><td>numeric</td><td>double</td><td>  6</td></tr>
 <tr><td>relacion_jefe_familia</td><td>Tipo de relación con el jefe de familia</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>actualmente_estudiando</td><td>Está estudiando?</td><td></td><td>character</td><td>character</td><td>  0</td></tr>
 <tr><td>trabajo_paga_ultima</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>fecha_registro</td><td></td><td></td><td></td><td>character</td><td>  0</td></tr>
 <tr><td>estado</td><td></td><td></td><td></td><td>double</td><td>  1</td></tr>
 <tr><td>Ab.unit_Pfal</td><td>standardize AU Pfal</td><td></td><td>numeric</td><td>double</td><td> 44</td></tr>
 <tr><td>Ab.unit_Pviv</td><td>standardize AU Pviv</td><td></td><td>numeric</td><td>double</td><td> 11</td></tr>
 <tr><td>mean.OD_Pfal</td><td>raw mean OD Pfal</td><td></td><td>numeric</td><td>double</td><td>  1</td></tr>
 <tr><td>mean.OD_Pviv</td><td>raw mean OD Pviv</td><td></td><td>numeric</td><td>double</td><td>  1</td></tr>
 <tr><td>sex_1</td><td>gender </td><td><a href="#levels.sex_1"> 2</a></td><td></td><td>integer</td><td>  0</td></tr>
 <tr><td>sex_4</td><td>gender-UNAP_UPDATE</td><td><a href="#levels.sex_1"> 2</a></td><td></td><td>integer</td><td>  0</td></tr>
 <tr><td>age_1</td><td>age</td><td></td><td>numeric</td><td>double</td><td>  0</td></tr>
 <tr><td>age_4</td><td>age-UNAP_UPDATE</td><td></td><td>numeric</td><td>double</td><td>  0</td></tr>
 <tr><td>micro_c</td><td>microscopy CONCENSUS</td><td><a href="#levels.micro_c"> 3</a></td><td></td><td>integer</td><td>737</td></tr>
 <tr><td>micro_1</td><td>microscopy UNAP</td><td><a href="#levels.micro_1"> 4</a></td><td></td><td>integer</td><td>737</td></tr>
 <tr><td>micro_n</td><td>microscopy NAMRU</td><td><a href="#levels.micro_c"> 3</a></td><td></td><td>integer</td><td>838</td></tr>
 <tr><td>pcr_c</td><td>pcr CONCENSUS</td><td><a href="#levels.micro_1"> 4</a></td><td></td><td>integer</td><td>737</td></tr>
 <tr><td>pcr_1</td><td>pcr UNAP</td><td><a href="#levels.micro_c"> 3</a></td><td></td><td>integer</td><td>737</td></tr>
 <tr><td>pcr_n</td><td>pcr NAMRU</td><td><a href="#levels.micro_1"> 4</a></td><td></td><td>integer</td><td>737</td></tr>
 <tr><td>community_1</td><td>community where the patient lives and was enrolled from</td><td><a href="#levels.community_1"> 4</a></td><td></td><td>integer</td><td>  0</td></tr>
 </table>

 <hr>
 <style>
 .hmisctable114169 {
 border: 1px solid gray;
 border-collapse: collapse;
 font-size: 100%;
 }
 .hmisctable114169 td {
 text-align: right;
 padding: 0 1ex 0 1ex;
 }
 .hmisctable114169 th {
 color: Black;
 text-align: center;
 padding: 0 1ex 0 1ex;
 font-weight: bold;
 }
 </style>
 <table class="hmisctable114169" border="1">
 <tr><th>Variable</th><th>Levels</th></tr>
 <tr><td><a name="levels.nivel_educacion">nivel_educacion</a></td><td>Inicial</td></tr>
 <tr><td></td><td>Primaria Completa</td></tr>
 <tr><td></td><td>Primaria Incompleta</td></tr>
 <tr><td></td><td>Secundaria Completa</td></tr>
 <tr><td></td><td>Secundaria Incompleta</td></tr>
 <tr><td></td><td>Sin Educacion</td></tr>
 <tr><td></td><td>Tecnica Completa</td></tr>
 <tr><td></td><td>Tecnica Incompleta</td></tr>
 <tr><td></td><td>Universidad Completa</td></tr>
 <tr><td></td><td>Universidad Incompleta</td></tr>
 <tr><td><a name="levels.micro_c">trabajo</a></td><td>Agricultor</td></tr>
 <tr><td></td><td>Ama de casa</td></tr>
 <tr><td></td><td>desempleado</td></tr>
 <tr><td></td><td>independiente</td></tr>
 <tr><td></td><td>obrero</td></tr>
 <tr><td></td><td>Other</td></tr>
 <tr><td><a name="levels.ah_auto">ah_radio, ah_television, ah_estereo</a></td><td>no</td></tr>
 <tr><td><a name="levels.drogas_tratamiento_01">&emsp;ah_refrigerador, ah_motocicleta, ah_mototaxi</a></td><td>si</td></tr>
 <tr><td><a name="levels.drogas_tratamiento_02">ah_auto</a></td><td>no</td></tr>
 <tr><td><a name="levels.enf_especifique_renal">drogas_tratamiento_01</a></td><td>artesunato</td></tr>
 <tr><td></td><td>artesunato mefloquina</td></tr>
 <tr><td></td><td>cloroquina</td></tr>
 <tr><td></td><td>FANSIDAR</td></tr>
 <tr><td></td><td>primaquina</td></tr>
 <tr><td></td><td>quinina</td></tr>
 <tr><td><a name="levels.community_1">drogas_tratamiento_02</a></td><td>clindamicina</td></tr>
 <tr><td></td><td>cloroquina</td></tr>
 <tr><td></td><td>mefloquina</td></tr>
 <tr><td></td><td>primaquina</td></tr>
 <tr><td><a name="levels.ah_auto">enf_especifique_renal</a></td><td>dolor de rinones</td></tr>
 <tr><td></td><td>piedras en rinones</td></tr>
 <tr><td><a name="levels.drogas_tratamiento_02">enf_especifique_otro</a></td><td>artritis</td></tr>
 <tr><td></td><td>colesterol</td></tr>
 <tr><td></td><td>colesterol y trigliceridos</td></tr>
 <tr><td></td><td>gastritis</td></tr>
 <tr><td></td><td>migrana</td></tr>
 <tr><td></td><td>presion alta</td></tr>
 <tr><td></td><td>reumatismo</td></tr>
 <tr><td><a name="levels.community_1">enf_medicinas_parasitos</a></td><td>albendazol</td></tr>
 <tr><td></td><td>antiparasitario</td></tr>
 <tr><td></td><td>planta medicinal : Oje</td></tr>
 <tr><td><a name="levels.ah_radio">sex_1</a></td><td>Female</td></tr>
 <tr><td><a name="levels.ah_auto">&emsp;sex_4</a></td><td>Male</td></tr>
 <tr><td><a name="levels.drogas_tratamiento_01">micro_c</a></td><td>NG</td></tr>
 <tr><td><a name="levels.drogas_tratamiento_02">&emsp;micro_n</a></td><td>PV</td></tr>
 <tr><td><a name="levels.enf_especifique_renal">&emsp;pcr_1</a></td><td>PF</td></tr>
 <tr><td><a name="levels.enf_especifique_otro">micro_1</a></td><td>NG</td></tr>
 <tr><td><a name="levels.enf_medicinas_parasitos">&emsp;pcr_c</a></td><td>PV</td></tr>
 <tr><td><a name="levels.sex_1">&emsp;pcr_n</a></td><td>PF</td></tr>
 <tr><td></td><td>PV/PF</td></tr>
 <tr><td><a name="levels.micro_1">community_1</a></td><td>Zungarococha</td></tr>
 <tr><td></td><td>Puerto Almendra</td></tr>
 <tr><td></td><td>Ninarrumi</td></tr>
 <tr><td></td><td>Llanchama</td></tr>
 </table>

 <hr>
<!--/html_preserve-->


## Output

- a **`full`** dataset **(973x100)** was written as `data/04-epidemio-full.csv`.
- a **`core`** dataset **(959x100)** was written as `data/04-epidemio-core.csv`.

### write .csv .rds

```r
readr::write_csv(unap6b, "data/04-epidemio-full.csv")
saveRDS(unap6b, "data/04-epidemio-full.rds")

readr::write_csv(unap6c, "data/04-epidemio-core.csv")
saveRDS(unap6c, "data/04-epidemio-core.rds")
```

### write dta

- without labels due to errors in writing labelled variables

**.dta restrictions:**

- STATA requires labelled integers (not numeric variables)
- do not allow dots in variable name
- string length of variable names


```r
haven::write_dta(unap6c %>% 
                   dplyr::rename(Ab_unit_Pfal=Ab.unit_Pfal,
                                 Ab_unit_Pviv=Ab.unit_Pviv,
                                 mean_OD_Pfal=mean.OD_Pfal,
                                 mean_OD_Pviv=mean.OD_Pviv,
                                 epi_n_redes_adquirio_dormir=
                                   epi_cantidad_redes_adquirio_dormir,
                                 epi_n_redes_usadas_actualmente=
                                   epi_cantidad_redes_usadas_actualmente)
                 , "data/04-epidemio-core.dta", version=13)

#haven::read_dta("data/04-epidemio-core.dta")
#haven::read_dta("data/04-epidemio-core.dta") %>% haven::as_factor() # WORKS PERFECT!
```


To check the data in R, use the following commands:


```r
# load data
data <- readRDS("path-to-file/04-epidemio-core.rds")

# attach specialized package
library(tidyverse) 
# if not installed, run the following:
# install.packages("tidyverse")

# repetitive strings are recognized as factors (as shown in APPENDIX: Plots and problems)
# in order to ease summary output, coerce them to factors:
data %>% mutate_if(is.character, factor) %>% glimpse()  # all variables and class
data %>% mutate_if(is.character, factor) %>% str()      # all variables and structure
data %>% mutate_if(is.character, factor) %>% summary()  # all variables and descriptive stats
```

***

## APPENDIX: age updates {#slope}

Using `slopegraph` [@slopegraph]


```r
x <- unap6a %>% 
  dplyr::select(1,dplyr::contains("age")) %>% 
  full_join(std.uty.cov %>% 
              dplyr::select(1,dplyr::contains("age"), -age_0), 
            by="id") %>% 
  mutate(age_0= age==age_1 & age_1==age_4) %>% 
  filter(age_0==FALSE) %>% 
  dplyr::select(-age_0)

y <- x$id
z <- x %>% dplyr::select(-id) %>% as.data.frame()
rownames(z) <- y

slopegraph::ggslopegraph(z, 
                         offset.x = 0.06,
                         xlim = c(.5,3.5),
                         xlabels = c("unap6", "unap1", "unap4")) + theme_minimal() +
  labs(title="Age updates across datasets")
```

<img src="01-complete_files/figure-html/unnamed-chunk-115-1.png" style="display: block; margin: auto;" />


## APPENDIX: Plots and problems


```r
#
by_age <- unap6a %>%
  filter(!is.na(age)) %>%
  group_by(age, estado_civil) %>%
  dplyr::count() %>%
  mutate(prop= n / sum(n)) # counts marital inside each age level
#
a <- ggplot(by_age, aes(age, prop)) +
  geom_line(aes(colour= fct_reorder2(estado_civil, 
                                     age, prop))) +
  labs(colour= "Marital status",
       title= "Proportion of marital status per age",
       subtitle= "Data from a Epidemiological census in Iquitos, Loreto - Peru",
       caption= "a caption using ggplot() + labs()")

by_edu <- unap6a %>%
  filter(!is.na(age)) %>%
  group_by(age, nivel_educacion) %>%
  dplyr::count() %>%
  mutate(prop= n / sum(n)) # counts marital inside each age level

b <- ggplot(by_edu, aes(age, prop)) +
  geom_line(aes(colour= fct_reorder2(nivel_educacion, 
                                     age, prop))) +
  labs(colour= "Education level",
       title= "Proportion of education level per age",
       subtitle= "Data from a Epidemiological census in Iquitos, Loreto - Peru",
       caption= "a caption using ggplot() + labs()")

Rmisc::multiplot(a, b, cols=2)
```

<img src="01-complete_files/figure-html/unnamed-chunk-116-1.png" style="display: block; margin: auto;" />


***

### problems 3

#### malaria incidence plot

- check [above](#malaria) for a plot relating `age`, `residence` and `malaria`


```r
unap6a %>%
  ggplot(aes(cuantas_tenido_malaria)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="01-complete_files/figure-html/unnamed-chunk-117-1.png" style="display: block; margin: auto;" />

```r
unap6a %>%
  group_by(tenido_malaria, opciones_tenido_malaria) %>%
  dplyr::summarise(n=n())
```

```
## Source: local data frame [8 x 3]
## Groups: tenido_malaria [?]
## 
## # A tibble: 8 x 3
##   tenido_malaria opciones_tenido_malaria     n
##            <chr>                   <chr> <int>
## 1             No                      No   233
## 2             No                 No Sabe     1
## 3        No Sabe                      No    23
## 4        No Sabe                 No Sabe   177
## 5             Si                      No     8
## 6             Si                 No Sabe    15
## 7             Si                      Si   505
## 8             Si                    <NA>     1
```

```r
unap6a %>%
  group_by(tenido_malaria, opciones_tenido_malaria, cuantas_tenido_malaria) %>%
  dplyr::summarise(n=n()) %>%
  filter(tenido_malaria!="Si" | opciones_tenido_malaria!="Si")
```

```
## Source: local data frame [11 x 4]
## Groups: tenido_malaria, opciones_tenido_malaria [6]
## 
## # A tibble: 11 x 4
##    tenido_malaria opciones_tenido_malaria cuantas_tenido_malaria     n
##             <chr>                   <chr>                  <dbl> <int>
##  1             No                      No                      0   232
##  2             No                      No                      1     1
##  3             No                 No Sabe                      0     1
##  4        No Sabe                      No                      0    23
##  5        No Sabe                 No Sabe                      0   177
##  6             Si                      No                      0     1
##  7             Si                      No                      1     2
##  8             Si                      No                      2     5
##  9             Si                 No Sabe                      1     7
## 10             Si                 No Sabe                      2     7
## 11             Si                 No Sabe                     NA     1
```

#### date and time class


```r
unap6at <- unap6 %>% 
  dplyr::select(fecha_registro) %>% 
  mutate(fecha_registro= str_replace_all(fecha_registro, "\\/","-")) %>% 
  rownames_to_column(var = "row")

unap6at_1 <- unap6at %>% 
  tidyr::separate(fecha_registro, c("date", "time", "mer"), sep = " ") %>% filter(mer=="p.m.")
  #tidyr::separate(date, c("day", "month", "year"), sep = "-") #%>% count("year")
  
dmy_hms(unap6at$fecha_registro[1])
```

```
## [1] "2015-08-09 11:35:48 UTC"
```




***

### sheet 2

```r
readxl::read_excel("data/UNAP-6-EPI-08feb2017.xlsx", sheet = 2)
```

```
## # A tibble: 89 x 2
##                    VARIABLE                                    `EXPLICACIÓN DE VARIABLE`
##                       <chr>                                                        <chr>
##  1                    anios                                                         años
##  2                   barrio                                                    comunidad
##  3          grupos_personas                        N° de familias que cocina en el hogar
##  4 electricidad_red_publica el poblador está conectado a la red pública de elecrtricidad
##  5                 ah_radio                                     Tiene radio, Si=1 y No=0
##  6            ah_television                                Tiene television, Si=1 y No=0
##  7               ah_estereo                                   Tiene estereo, Si=1 y No=0
##  8          ah_refrigerador                              Tiene refrigerador, Si=1 y No=0
##  9           ah_motocicleta                               Tiene motocicleta, Si=1 y No=0
## 10              ah_mototaxi                                  Tiene mototaxi, Si=1 y No=0
## # ... with 79 more rows
```


### sheet 3

```r
readxl::read_excel("data/UNAP-6-EPI-08feb2017.xlsx", sheet = 3)
```

```
## # A tibble: 93 x 2
##                    VARIABLE
##                       <chr>
##  1             estado_civil
##  2          nivel_educacion
##  3             tipo_trabajo
##  4        otro_tipo_trabajo
##  5                    anios
##  6                   barrio
##  7          grupos_personas
##  8 electricidad_red_publica
##  9                 ah_radio
## 10            ah_television
## # ... with 83 more rows, and 1 more variables: `EXPLICACIÓN DE VARIABLE` <chr>
```

<!--## References-->

<!--chapter:end:04-epidemio.Rmd-->

# abstract {#astmh}

## Title:

<!-- 
Towards standardized malaria serological surveillance:
A reproducible workflow
previous to the application of serology-based modeling

A reproducible workflow for the 
standardization of 
high-throughput ELISA experiments

A standarization of high-throughput ELISA experiments/assays
previous to the application of serological surveillance modeling

BERKELEY BOOK 2017
https://www.practicereproducibleresearch.org/

-->

**Towards reproducible malaria sero-surveillance:
A workflow for high-throughput ELISA plate standardization using R**

## Abstract:
@WHOreport2016
@elliott2014surveillance
@Seplveda2015
@CienciaReproducible2016

### Introduction

intro sentence:

- sero-surveillance or seroepidemiological studies
- antibody data is an excellent indicator of malaria exposure, transmission and 
immunity at population levels. 

Known field achievement:

- antigen selection
- mathematical models to define seropositivity, and changes in transmission and exposure.

Problem:

- No common procedure to standardize ELISA plates among publications
- Usage of privative softwares without workflow recording
- Lack of reproducibility of studies

Solution:

- Reproducible workflow for ELISA plates standardization
- Adds standard analysis tools to develop high-throughput assays
- Optimize sero-surveillance development

### Methods

- Plate protocol by @Miura2008
- Data input with `XLConnect` @XLConnect
- 4 parameter log-logistic regression with `drc` @Ritz2015
- Unbiased selection and Filtering of replicated reads across plates.

### Results

- residual variance homogenization of regression models per plate.
- Inter and Intra-plate Coefficients of variation for QC.

### Discussion

This type of reports, relevant for malaria surveillance [@elliott2014surveillance], 
would be highly improved with standardize open-acess lab/comp protocols

- Data input `plater` @plater
- Availability in GitHub

- Improve collaboration
- Standardize data generation and analysis methods will improve data comparison 
between populations and geographical regions

## Possible meetings

### August -> October (no AWARD)
- https://meetings.cshl.edu/meetings.aspx?meet=DATA&year=16

### January -> May (with TRAVEL AWARD)
- http://www.immunology2017.org/abstracts/

### April -> November
- http://www.astmh.org/annual-meeting

### April -> July
- http://symposium.iscbsc.org/

### September -> December (with SCI AWARD)
- http://www.syndromic.org/annual-conference/2016-isds-conference

<!--## References-->

<!--chapter:end:abstract.Rmd-->

# Computer environment {-}


```r
devtools::session_info()
```

```
## Session info -------------------------------------------------------------------------------------------------
```

```
##  setting  value                       
##  version  R version 3.4.0 (2017-04-21)
##  system   x86_64, linux-gnu           
##  ui       X11                         
##  language en_US                       
##  collate  en_US.UTF-8                 
##  tz       America/Lima                
##  date     2017-06-02
```

```
## Packages -----------------------------------------------------------------------------------------------------
```

```
##  package       * version    date       source                            
##  acepack         1.4.1      2016-10-29 CRAN (R 3.4.0)                    
##  assertthat      0.2.0      2017-04-11 CRAN (R 3.4.0)                    
##  backports       1.0.5      2017-01-18 CRAN (R 3.4.0)                    
##  base64enc       0.1-3      2015-07-28 CRAN (R 3.4.0)                    
##  bitops          1.0-6      2013-08-17 CRAN (R 3.4.0)                    
##  bookdown        0.3        2016-11-28 CRAN (R 3.4.0)                    
##  brew            1.0-6      2011-04-13 CRAN (R 3.4.0)                    
##  broom           0.4.2      2017-02-13 CRAN (R 3.4.0)                    
##  car             2.1-4      2016-12-02 CRAN (R 3.4.0)                    
##  caTools         1.17.1     2014-09-10 CRAN (R 3.4.0)                    
##  cellranger      1.1.0      2016-07-27 CRAN (R 3.4.0)                    
##  checkmate       1.8.2      2016-11-02 CRAN (R 3.4.0)                    
##  cluster         2.0.6      2017-03-16 CRAN (R 3.4.0)                    
##  codetools       0.2-15     2016-10-05 CRAN (R 3.3.1)                    
##  colorspace      1.3-2      2016-12-14 CRAN (R 3.4.0)                    
##  data.table      1.10.4     2017-02-01 CRAN (R 3.4.0)                    
##  DBI             0.6-1      2017-04-01 CRAN (R 3.4.0)                    
##  devtools        1.12.0     2016-12-05 CRAN (R 3.4.0)                    
##  DiagrammeR    * 0.9.0      2017-01-04 CRAN (R 3.4.0)                    
##  digest          0.6.12     2017-01-27 CRAN (R 3.4.0)                    
##  dplyr         * 0.5.0      2016-06-24 CRAN (R 3.4.0)                    
##  drc           * 3.0-1      2016-08-30 CRAN (R 3.4.0)                    
##  evaluate        0.10       2016-10-11 CRAN (R 3.4.0)                    
##  forcats       * 0.2.0      2017-01-23 CRAN (R 3.4.0)                    
##  foreign         0.8-68     2017-04-24 CRAN (R 3.4.0)                    
##  Formula       * 1.2-1      2015-04-07 CRAN (R 3.4.0)                    
##  gdata           2.17.0     2015-07-04 CRAN (R 3.4.0)                    
##  ggplot2       * 2.2.1      2016-12-30 CRAN (R 3.4.0)                    
##  gplots          3.0.1      2016-03-30 CRAN (R 3.4.0)                    
##  gridExtra       2.2.1      2016-02-29 CRAN (R 3.4.0)                    
##  gtable          0.2.0      2016-02-26 CRAN (R 3.4.0)                    
##  gtools          3.5.0      2015-05-29 CRAN (R 3.4.0)                    
##  haven         * 1.0.0.9000 2017-05-05 Github (hadley/haven@4cb0d51)     
##  highr           0.6        2016-05-09 CRAN (R 3.4.0)                    
##  Hmisc         * 4.0-3      2017-05-02 CRAN (R 3.4.0)                    
##  hms             0.3        2016-11-22 CRAN (R 3.4.0)                    
##  htmlTable       1.9        2017-01-26 CRAN (R 3.4.0)                    
##  htmltools       0.3.6      2017-04-28 CRAN (R 3.4.0)                    
##  htmlwidgets     0.8        2016-11-09 CRAN (R 3.4.0)                    
##  httr            1.2.1      2016-07-03 CRAN (R 3.4.0)                    
##  igraph          1.0.1      2015-06-26 CRAN (R 3.4.0)                    
##  influenceR      0.1.0      2015-09-03 CRAN (R 3.4.0)                    
##  jsonlite        1.4        2017-04-08 CRAN (R 3.4.0)                    
##  KernSmooth      2.23-15    2015-06-29 CRAN (R 3.4.0)                    
##  knitr         * 1.16       2017-05-18 cran (@1.16)                      
##  labeling        0.3        2014-08-23 CRAN (R 3.4.0)                    
##  lattice       * 0.20-35    2017-03-25 CRAN (R 3.3.3)                    
##  latticeExtra    0.6-28     2016-02-09 CRAN (R 3.4.0)                    
##  lazyeval        0.2.0      2016-06-12 CRAN (R 3.4.0)                    
##  lme4            1.1-13     2017-04-19 CRAN (R 3.4.0)                    
##  lubridate     * 1.6.0      2016-09-13 CRAN (R 3.4.0)                    
##  magrittr        1.5        2014-11-22 CRAN (R 3.4.0)                    
##  MASS          * 7.3-47     2017-04-21 CRAN (R 3.4.0)                    
##  Matrix          1.2-10     2017-04-28 CRAN (R 3.4.0)                    
##  MatrixModels    0.4-1      2015-08-22 CRAN (R 3.4.0)                    
##  memoise         1.1.0      2017-04-21 CRAN (R 3.4.0)                    
##  mgcv            1.8-17     2017-02-08 CRAN (R 3.4.0)                    
##  minqa           1.2.4      2014-10-09 CRAN (R 3.4.0)                    
##  mnormt          1.5-5      2016-10-15 CRAN (R 3.4.0)                    
##  modelr          0.1.0      2016-08-31 CRAN (R 3.4.0)                    
##  multcomp        1.4-6      2016-07-14 CRAN (R 3.4.0)                    
##  munsell         0.4.3      2016-02-13 CRAN (R 3.4.0)                    
##  mvtnorm         1.0-6      2017-03-02 CRAN (R 3.4.0)                    
##  nlme            3.1-131    2017-02-06 CRAN (R 3.4.0)                    
##  nloptr          1.0.4      2014-08-04 CRAN (R 3.4.0)                    
##  nnet            7.3-12     2016-02-02 CRAN (R 3.4.0)                    
##  pbkrtest        0.4-7      2017-03-15 CRAN (R 3.4.0)                    
##  plotrix         3.6-4      2016-12-30 CRAN (R 3.4.0)                    
##  plyr          * 1.8.4      2016-06-08 CRAN (R 3.4.0)                    
##  psych           1.7.5      2017-05-03 CRAN (R 3.4.0)                    
##  purrr         * 0.2.2.2    2017-05-11 cran (@0.2.2.2)                   
##  quantreg        5.33       2017-04-18 CRAN (R 3.4.0)                    
##  R6              2.2.1      2017-05-10 cran (@2.2.1)                     
##  RColorBrewer    1.1-2      2014-12-07 CRAN (R 3.4.0)                    
##  Rcpp            0.12.11    2017-05-22 cran (@0.12.11)                   
##  readr         * 1.1.0      2017-03-22 CRAN (R 3.4.0)                    
##  readxl        * 1.0.0      2017-04-18 CRAN (R 3.4.0)                    
##  reshape2        1.4.2      2016-10-22 CRAN (R 3.4.0)                    
##  rgexf           0.15.3     2015-03-24 CRAN (R 3.4.0)                    
##  rJava           0.9-8      2016-01-07 CRAN (R 3.2.3)                    
##  rlang           0.1.1      2017-05-18 cran (@0.1.1)                     
##  rmarkdown       1.5        2017-04-26 CRAN (R 3.4.0)                    
##  Rmisc         * 1.5        2013-10-22 CRAN (R 3.4.0)                    
##  Rook            1.1-1      2014-10-20 CRAN (R 3.4.0)                    
##  rpart           4.1-11     2017-04-21 CRAN (R 3.4.0)                    
##  rprojroot       1.2        2017-01-16 CRAN (R 3.4.0)                    
##  rstudioapi      0.6        2016-06-27 CRAN (R 3.4.0)                    
##  rvest           0.3.2      2016-06-17 CRAN (R 3.4.0)                    
##  sandwich        2.3-4      2015-09-24 CRAN (R 3.3.1)                    
##  scales          0.4.1      2016-11-09 CRAN (R 3.4.0)                    
##  slopegraph      0.1.13     2017-03-08 Github (leeper/slopegraph@aace3dd)
##  SparseM         1.77       2017-04-23 CRAN (R 3.4.0)                    
##  stringi         1.1.5      2017-04-07 CRAN (R 3.4.0)                    
##  stringr       * 1.2.0      2017-02-18 CRAN (R 3.4.0)                    
##  survival      * 2.41-3     2017-04-04 CRAN (R 3.4.0)                    
##  TH.data         1.0-8      2017-01-23 CRAN (R 3.4.0)                    
##  tibble        * 1.3.1      2017-05-17 cran (@1.3.1)                     
##  tidyr         * 0.6.2      2017-05-04 CRAN (R 3.4.0)                    
##  tidyverse     * 1.1.1      2017-01-27 CRAN (R 3.4.0)                    
##  viridis       * 0.4.0      2017-03-27 CRAN (R 3.4.0)                    
##  viridisLite   * 0.2.0      2017-03-24 CRAN (R 3.4.0)                    
##  visNetwork      1.0.3      2016-12-22 CRAN (R 3.4.0)                    
##  withr           1.0.2      2016-06-20 CRAN (R 3.4.0)                    
##  XLConnect     * 0.2-12     2016-06-24 CRAN (R 3.4.0)                    
##  XLConnectJars * 0.2-12     2016-06-24 CRAN (R 3.4.0)                    
##  XML             3.98-1.7   2017-05-03 CRAN (R 3.4.0)                    
##  xml2            1.1.1      2017-01-24 CRAN (R 3.4.0)                    
##  yaml            2.1.14     2016-11-12 CRAN (R 3.4.0)                    
##  zoo             1.8-0      2017-04-12 CRAN (R 3.4.0)
```

<!--chapter:end:cpenviro.Rmd-->

# References {-}

<!--chapter:end:05-citation.Rmd-->

