clear
set more off // para no poner barrita espaciadora ;)
cd "D:\0proyectos\R_\elixr"
log using "D:\0proyectos\R_\elixr\zungdb-z0.log", replace


************************************ VIVIENDA ************************************ 

use "data/z0_viv.dta"

misstable sum
egen mis = rowmiss(grupospersonas ahauto epitipofuenteagua epidistanciafuenteagua epiviajofueravilla epimesesultimamalaria epiespeciecausomalaria )
codebook mis

duplicates report

*describe
*codebook

codebook community1

ds, has(type long)
foreach var in `r(varlist)' {
	if "`var'" == "community1" continue
	if "`var'" == "vivienda" continue
	codebook `var', detail
	tab `var' community1, chi col nokey //exp
}


ds, has(type double)
foreach var in `r(varlist)' {
	codebook `var', detail
	tabstat `var', s(n min max mean sd q sk k) by(community1)
	oneway `var' community1, bonferroni
	*kwallis2 `var', by(community1)
}

kwallis2 cantidadhabitaciones, by(community1)
kwallis2 cantidadhabitacionesdormir, by(community1)
kwallis2 epimesesultimamalaria, by(community1)


************************************ SUJETO ************************************ 
clear
set more off // para no poner barrita espaciadora ;)

use "data/z0_ind.dta"

codebook drogastratamiento01 drogastratamiento02 drogastratamiento03
drop drogastratamiento01 drogastratamiento02 drogastratamiento03
codebook enfespecifiquerenal enfespecifiquerenal enfmedicinasparasitos enfespecifiqueotro
drop enfespecifiquerenal enfespecifiquerenal enfmedicinasparasitos enfespecifiqueotro

*dropeados pero generar otra base con estas variables --> en R
codebook micro1 micron pcr1 pcrn densidad
drop micro1 micron pcr1 pcrn densidad
drop estado

codebook cantidadtratamientomalaria epiestadocamposagricultura epicantidadredesadquiriodormir

misstable sum

*egen mis = rowmiss(grupospersonas ahauto epitipofuenteagua epidistanciafuenteagua epiviajofueravilla epimesesultimamalaria epiespeciecausomalaria )
*codebook mis

duplicates report

*describe
*codebook

codebook community1

ds, has(type long)
foreach var in `r(varlist)' {
	if "`var'" == "community1" continue
	if "`var'" == "fecharegistro" continue
	if "`var'" == "vivienda" continue
	if "`var'" == "id" continue
	if "`var'" == "barrido" continue
	codebook `var', detail
	tab `var' community1, chi col nokey //exp
}

bysort agequart : tab prevviv community1, chi col nokey
bysort ageoms : tab prevviv community1, chi col nokey
bysort agec18 : tab prevviv community1, chi col nokey

hist age7 , norm discr frac by(sex8 )
hist residence, norm discr

ds, has(type double)
foreach var in `r(varlist)' {
	codebook `var', detail
	tabstat `var', s(n min max mean sd q sk k) by(community1)
	oneway `var' community1, bonferroni
	*kwallis2 `var', by(community1)
}


************************************ TABLA 3 ************************************ 
clear
set more off // para no poner barrita espaciadora ;)

use "data/z0_ind_viv_t3.dta"
descr
*n=936

misstable sum
*n=872
*egen mis = rowmiss(tenido_malaria epi_estado_canal_agua_c epi_uso_red_dormir abunit_pviv abunit_pfal)
*n=923
egen mis = rowmiss(tenido_malaria epi_estado_canal_agua_c)
codebook mis

egen mis2 = rowmiss(tenido_malaria epi_estado_canal_agua_c abunit_pviv abunit_pviv_4 abunit_pfal abunit_pfal_4)
codebook mis2

*CORREGIR MISSING ABUNITS!

*egen abmisv = rowmiss(abunit_pviv abunit_pviv_4)
*codebook abmisv
*bysort sero_viv: tab abmisv abunit_pviv_4

*egen abmisf = rowmiss(abunit_pfal abunit_pfal_4)
*codebook abmisf
*bysort sero_fal: tab abmisf abunit_pfal_4

duplicates report

************************************ PREPARAR DESENLACES ************************************ 
local o = "micr_fal micr_viv micr_mix  prev_viv prev_fal prev_mix sero_viv sero_fal sero_mix"
codebook `o'
label list `o'

foreach var in `o' {
	replace `var' = `var'-1
	label define `var' 0 "negative" 1 "positive", replace
}

codebook `o'
label list `o'

************************************ OUTCOME 95% CONFIDENCE INTERVAL

foreach var in `o' {
	tab `var'
	prtest `var' == 0.32
	tab `var' community_1, chi col nokey //exp
	bysort community_1 : prtest `var' == 0.32
}


************************************ CORREGIR INDEPENDIENTE ************************************ 

local e = "epi_estado_campos_agricultura_c"
codebook `e'
label list `e'

replace `e' = `e'-1
label define `e' 0 "nunca-raravez" 1 "amenudo-siempre", replace

codebook `e'
label list `e'

*no incluir
codebook epi_uso_ropa_impregnada_c


************************************ PREV-VIVAX+ ************************************ 

local s = "i.trabajo_rpl"
local a = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8"
local b = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c "
local c = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria "
local d = "i.trabajo_rpl##i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria "

*modelo simple
glm prev_viv `s', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto total ind: exposicion + confusor
glm prev_viv `a', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo ind: exposicion + confusor + mediador
glm prev_viv `b', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo marginal ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) pa
glm prev_viv `c', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*modificador de efecto
glm prev_viv `d', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo condicional ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) re/mle


************************************ SERO-VIVAX+ ************************************ 

local s = "i.residence_quart"
local a = "i.residence_quart i.community_1 i.age_quart"
local b = "i.residence_quart i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.community_1 i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.tenido_malaria i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c"
local c = "i.residence_quart i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.community_1 i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.tenido_malaria i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c i.epi_alguien_tuvo_malaria i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.material_pared_c i.material_piso_c "
*local d = "i.residence_quart i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.community_1 i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.tenido_malaria i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c i.epi_alguien_tuvo_malaria i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.material_pared_c i.material_piso_c "

*modelo simple
glm sero_viv `s', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto total ind: exposicion + confusor
glm sero_viv `a', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo ind: exposicion + confusor + mediador
glm sero_viv `b', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo marginal ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) pa
glm sero_viv `c', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*modificacion de efecto
*glm sero_viv `d', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo condicional ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) re/mle

*local s = "i.tenido_malaria"
*local a = "i.tenido_malaria i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.residence_quart i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c"
*local b = "i.tenido_malaria i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.residence_quart i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c"
*local c = "i.tenido_malaria i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.residence_quart i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c i.epi_alguien_tuvo_malaria i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.material_pared_c i.material_piso_c"

*modelo simple
*glm sero_viv `s', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto total ind: exposicion + confusor
*glm sero_viv `a', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo ind: exposicion + confusor + mediador
*glm sero_viv `b', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo marginal ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) pa
*glm sero_viv `c', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo condicional ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) re/mle


************************************ SERO-FALCIP+ ************************************ 

local s = "i.residence_quart"
local a = "i.residence_quart i.community_1 i.age_quart"
local b = "i.residence_quart i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.community_1 i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.tenido_malaria i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c"
local c = "i.residence_quart i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.community_1 i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.tenido_malaria i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c i.epi_alguien_tuvo_malaria i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.material_pared_c i.material_piso_c "
*local d = "i.residence_quart i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.community_1 i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.tenido_malaria i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c i.epi_alguien_tuvo_malaria i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.material_pared_c i.material_piso_c "

*modelo simple
glm sero_fal `s', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto total ind: exposicion + confusor
glm sero_fal `a', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo ind: exposicion + confusor + mediador
glm sero_fal `b', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo marginal ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) pa
glm sero_fal `c', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*modificacion de efecto
*glm sero_viv `d', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo condicional ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) re/mle

*local s = "i.tenido_malaria"
*local a = "i.tenido_malaria i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.residence_quart i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c"
*local b = "i.tenido_malaria i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.residence_quart i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c"
*local c = "i.tenido_malaria i.epi_estuvo_campo_antes_c i.epi_cercania_fuente_agua_c i.epi_duerme_cerca_monte_c i.epi_duerme_ventanas_abiertas_c  i.age_quart i.residence_quart i.trabajo_rpl i.epi_uso_mangas_largas_c epi_uso_redes_cama_c i.epi_uso_repelente_mosquito_c i.epi_alguien_tuvo_malaria i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.material_pared_c i.material_piso_c"

*modelo simple
*glm sero_fal `s', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto total ind: exposicion + confusor
*glm sero_fal `a', family(binomial) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo ind: exposicion + confusor + mediador
*glm sero_viv `b', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo marginal ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) pa
*glm sero_fal `c', family(poisson) link(log) eform  vce(cluster vivienda ) nolog
*efecto directo / modelo condicional ind+viv: exposicion + confusor + mediador (nivel sujeto + nivel vivienda) re/mle

************************************ EXPLORATORY MODEL (NESTED METHOD) ************************************ 

codebook mis

************************************ PREV-VIVAX+ (NESTED METHOD) ************************************ 

*SIN vce(cluster vivienda )**************************

****CORREGIR AB MIN O MAX
*abunit_pviv i.abunit_pviv_4 i.sero_fal abunit_pfal i.abunit_pfal_4 

*nivel 0
quietly: glm prev_viv if mis==0, family(binomial) link(log) eform nolog
estimate store m0

*nivel 1
*glm prev_viv community_1 if mis==0, family(binomial) link(log) eform nolog
*estimate store m01
*lrtest m0 m01 //no

local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_viv i.sero_fal"

foreach var in `l' {
	glm prev_viv `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m01
	lrtest m0 m01 //no
}

*resultado 1
glm prev_viv i.community_1 if mis==0, family(binomial) link(log) eform nolog
estimate store m1

*nivel 2

local l = "i.trabajo_rpl i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_viv i.sero_fal"

foreach var in `l' {
	glm prev_viv i.community_1 `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m11
	lrtest m1 m11 //no
}

*resultado 2
glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c if mis==0, family(binomial) link(log) eform nolog
estimate store m2

*nivel 3

local l = "i.trabajo_rpl i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_viv i.sero_fal"

foreach var in `l' {
	glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m21
	lrtest m2 m21 //no
}

*resultado 3
glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv if mis==0, family(binomial) link(log) eform nolog
estimate store m3

*nivel 4

local l = "i.trabajo_rpl i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_fal"

foreach var in `l' {
	glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m31
	lrtest m3 m31 //no
}


*resultado 4
*ninguna otra variable entra al modelo
*glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv i.trabajo_rpl i.age_quart i.epi_cerca_fuente_agua if mis==0, family(poisson) link(log) eform nolog

*modelo final
glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv if mis==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog
*modelo multinivel
meqrlogit prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv if mis==0 || vivienda: , or
meqrlogit prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv if mis==0 || vivienda: R.epi_cerca_fuente_agua , or

*PROBAR AGREGAR ABUNITS
*i.abunit_pvix_4 
*i.abunit_pfal_4 



************************************ SERO-VIVAX+ (NESTED METHOD) ************************************ 

*SIN vce(cluster vivienda )**************************

****CORREGIR AB MIN O MAX
*abunit_pviv i.abunit_pviv_4 i.sero_fal abunit_pfal i.abunit_pfal_4 

*nivel 0
quietly: glm sero_viv if mis==0, family(binomial) link(log) eform nolog
estimate store m0

*nivel 1
local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_viv "

foreach var in `l' {
	glm sero_viv `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m01
	lrtest m0 m01 //no
}

*resultado 1
glm sero_viv i.age_quart if mis==0, family(binomial) link(log) eform nolog
estimate store m1


*nivel 2
local l = "i.trabajo_rpl i.community_1 i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_viv "

foreach var in `l' {
	glm sero_viv i.age_quart `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m11
	lrtest m1 m11 //no
}

*resultado 2
glm sero_viv i.age_quart i.community_1 if mis==0, family(binomial) link(log) eform nolog
estimate store m2


*nivel 3
local l = "i.trabajo_rpl i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_viv "

foreach var in `l' {
	glm sero_viv i.age_quart i.community_1 `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m21
	lrtest m2 m21 //no
}

*resultado 3
*glm sero_viv i.age_quart i.community_1 if mis==0, family(binomial) link(log) eform nolog
*estimate store m3

*modelo final
glm sero_viv i.age_quart i.community_1 if mis==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog
*modelo multinivel
*meqrlogit sero_viv i.age_quart i.community_1 if mis==0 || vivienda: , or
*meqrlogit sero_viv i.age_quart i.community_1 if mis==0 || vivienda: R.epi_cerca_fuente_agua , or



************************************ SERO-FALCIP+ (NESTED METHOD) ************************************ 

*SIN vce(cluster vivienda )**************************

****CORREGIR AB MIN O MAX
*abunit_pviv i.abunit_pviv_4 i.sero_fal abunit_pfal i.abunit_pfal_4 

*nivel 0
quietly: glm sero_fal if mis==0, family(binomial) link(log) eform nolog
estimate store m0

*nivel 1
local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_malaria_ultimos_meses_c i.prev_fal "

foreach var in `l' {
	glm sero_fal `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m01
	lrtest m0 m01 //no
}

*resultado 1
glm sero_fal i.community_1 if mis==0, family(poisson) link(log) eform nolog
estimate store m1


*nivel 2
local l = "i.trabajo_rpl i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_malaria_ultimos_meses_c i.prev_fal "

foreach var in `l' {
	glm sero_fal i.community_1 `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m11
	lrtest m1 m11 //no
}

*resultado 2
glm sero_fal i.community_1 i.age_quart if mis==0, family(poisson) link(log) eform nolog
estimate store m2



*nivel 3
local l = "i.trabajo_rpl i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_malaria_ultimos_meses_c i.prev_fal "

foreach var in `l' {
	glm sero_fal i.community_1 i.age_quart `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m21
	lrtest m2 m21 //no
}

*resultado 3
glm sero_fal i.community_1 i.age_quart i.prev_fal if mis==0, family(poisson) link(log) eform nolog
estimate store m3



*nivel 4
local l = "i.trabajo_rpl i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_malaria_ultimos_meses_c"

foreach var in `l' {
	glm sero_fal i.community_1 i.age_quart i.prev_fal `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m31
	lrtest m3 m31 //no
}

*resultado 4
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis==0, family(poisson) link(log) eform nolog
estimate store m4



*nivel 5
local l = "i.trabajo_rpl i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_malaria_ultimos_meses_c"

foreach var in `l' {
	glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m41
	lrtest m4 m41 //no
}

*resultado 5
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis==0, family(poisson) link(log) eform nolog
estimate store m5


*modelo final
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
glm sero_fal i.community_1 age_7 i.prev_fal i.tenido_malaria if mis==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
*modelo multinivel
*meqrlogit sero_viv i.age_quart i.community_1 if mis==0 || vivienda: , or
*meqrlogit sero_viv i.age_quart i.community_1 if mis==0 || vivienda: R.epi_cerca_fuente_agua , or



************************************ MODELOS FINALES ************************************

glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv if mis==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog
glm sero_viv i.age_quart i.community_1 if mis==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog

************************************ REVERSE CATALITIC MODEL ************************************ 

codebook community_1

*********************************** viv

revcat sero_viv age_7, nolog
*revcat sero_viv age_7, plot

revcat sero_viv age_7 if community_1 == 1, nolog
revcat sero_viv age_7 if community_1 == 1, change(15) age nolog

revcat sero_viv age_7 if community_1 == 2
revcat sero_viv age_7 if community_1 == 2, change(25) age nolog


revcat sero_viv age_7 if community_1 == 3, nolog
revcat sero_viv age_7 if community_1 == 3, change(15) age nolog


revcat sero_viv age_7 if community_1 == 4, nolog
revcat sero_viv age_7 if community_1 == 4, change(15) age nolog

*********************************** fal

revcat sero_fal age_7, nolog
*revcat sero_fal age_7, plot

revcat sero_fal age_7 if community_1 == 1, nolog
revcat sero_fal age_7 if community_1 == 1, change(15) age nolog

revcat sero_fal age_7 if community_1 == 2, nolog
revcat sero_fal age_7 if community_1 == 2, change(15) age nolog


revcat sero_fal age_7 if community_1 == 3, nolog
revcat sero_fal age_7 if community_1 == 3, change(10) age nolog


revcat sero_fal age_7 if community_1 == 4, nolog
revcat sero_fal age_7 if community_1 == 4, change(25) age nolog

*********************************** plots

*use "data/z0_revcat.dta"




************
log close


*************************** recomendaciones *************************

*MOSQUITEROS PAREDES CASA difieren de MOSQUITEROS DE CAMA?
*QUIEN EJECUTA EL ROCIAMIENTO DE INSECTICIDA AL MOSQUITERO
*SI ES UNA ACCION DEL EJEFE DE LA CASA, ESTA CONSULTA DEBE IR AL INICIO, EN SECCION DE ENTRADA, CASA!
