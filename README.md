# SeroSurveillance

This is a reproducible workflow in R for the standardization of ELISA reads across plates.
It follows K. Miura et al. 2008 experimental procedure.

## TO DO:

* Use bookdown to knit all the Rmd reports!
* Clean code in standard.Rmd -> make one function then loop

1. Add an alternative initial path: `plater` instead of `XLConnect`.
2. Generate inter-plate variability measurements: `inter-plate-CV`, `ctrl+`, `ctrl-`.
3. Check if `ZG182-1` should be corrected to `ZG181-1`.
4. Use `relational data` to conect `Serological dataset` and `Covariate+Epidemiological dataset`.
5. Solve inconsistencies in the Epidemiological dataset.

* Write an abstract!

- Write methodology and limitations
    + Compare against drLumi statistical considerations
    + `medrc` package
- Add table with expected Template distribution
    + `plater` package
- Add details on K.Miura methods (appendix)

## Project suggestions:

- Add STD duplicates on each plate?
    + Provides limits of quantification
- Add to surveillance:
    + if longitudinal, register infection dates: days since last infection
- Review communities history
    + Rosas-Aguirre rejected ages older than 30
    + Check Maki publication on 'Road construction...'
- Standard procedure to outliers for re-run (based on ctrl comparisons)

## Additional analysis:

- Multinomial Logistic regression
- Individual level heterogeneity:
    + map location
    + climate, geographical differences, altitude, ...
    + seroprevalence adjusted with covariates
    + sample coordinates
- Timeline PERU and LORETO malaria case index per year
    + Angel Rosas-Aguirre on VIVAX EPIDEMIO PERU
    + Veronica Soto-Calle on SPATIO-TEMPORAL MALARIA LORETO
    
## Curiosities:

- Some lab replicate patterns:
    + N21 -> N28 -> {N30, N31, N32}
    + {N21, N20, N22} -> N24 -> N28
