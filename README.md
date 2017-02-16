# SeroSurveillance

This is a reproducible workflow in R for the standardization of ELISA reads across plates, following [K. Miura et al. 2008](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2253722/) experimental procedure.

## TO-DO

1. Order covariate data sets (Excel and Stata formats)
2. Manage large covariate dataset (Excel)
3. Add an alternative initial path: 'plater' instead of 'XLConnect'
4. Merge histograms by factor Specie

- Write methodology and limitations
    + Compare against drLumi statistical considerations
    + `medrc` package
- Add table with expected Template distribution
    + `plater` package
- Keep a tidy export DF format
    + each variable is a column
    + each observation is a row
    + each type of observational unit forms a table
- Add note on maximum Ab.unit **previous to filtering**
- Add details on K.Miura methods (appendix)

## Project suggestions

- Add STD duplicates on each plate?
    + Provides limits of quantification
- Add to surveillance:
    + if longitudinal, register infection dates: days since last infection
- Review communities history
    + Rosas-Aguirre rejected ages older than 30
    + Check Maki publication on 'Road construction...'
- Standard procedure to outliers for re-run

## Additional analysis?

- Study Logistic regression + odds-ratio statistics (handbookstatistics!)
- Individual level heterogeneity
    + map location
    + climate, geographical differences, altitude, ...
    + seroprevalence adjusted with covariates
    + sample coordinates
- Timeline PERU and LORETO malaria case index per year
    + Angel Rosas-Aguirre on VIVAX EPIDEMIO PERU
    + Veronica Soto-Calle on SPATIO-TEMPORAL MALARIA LORETO
    
## Curiosities

- Some lab replicate patterns:
    + N21 -> N28 -> {N30, N31, N32}
    + {N21, N20, N22} -> N24 -> N28