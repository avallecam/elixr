# SeroSurveillance

This is a reproducible workflow in R for the standardization 
of ELISA reads across plates.
It follows K. Miura et al. 2008 experimental procedure.

## TO DO:

<!-- 
DONE:
* Use bookdown to knit all the Rmd reports!
-->

### abstract!

- Write methodology and limitations
    + Compare against `drLumi` statistical considerations
    + `medrc` package
- Add table with expected Template distribution
    + `plater` package
- Add details on K.Miura methods (appendix)


### now

1. Clean code in standard.Rmd -> tidy functions in thesis projects.
2. Individual level heterogeneity:
    + cluster analisis
    + regression + spatial ditribution.
3. Add bootstrap 95%CI to transmission curves.

### technical:

1. Add an alternative initial path: `plater` instead of `readxl+tidyxl`.
2. Generate inter-plate variability measurements: 
`inter-plate-CV`, `ctrl+`, `ctrl-`.

### must:

1. Check if `ZG182-1` should be corrected to `ZG181-1`.
2. Check if you solved inconsistencies in the Epidemiological dataset. (:v)

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


- Timeline PERU and LORETO malaria case index per year
    + Angel Rosas-Aguirre on VIVAX EPIDEMIO PERU
    + Veronica Soto-Calle on SPATIO-TEMPORAL MALARIA LORETO
    
## Curiosities:

- Some lab replicate patterns:
    + N21 -> N28 -> {N30, N31, N32}
    + {N21, N20, N22} -> N24 -> N28

## Personal communication

- Viviana (28ago2017)

> anexo las bases de resultados y de datos demográficos 
finales corregidos hasta ahora seria el final, 
> en la base de resultados està corregido la edad y 
sexo y datos de ELISA.
> la base epidemiológica hay tres datos de edad, 
correcta edad_dni y correcto sexo_consentimiento. 
> Si tienes dudas me avisas, 
> en breve te envio do file que me envio Christian 
del análisis previo que el hizo, 
> por favor vas enviándome los resultados en tabla y/o figuras 
para poder escribir los resultados del articulo e iniciar la discusión.
