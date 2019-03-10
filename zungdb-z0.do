clear
set more off // para no poner barrita espaciadora ;)
cd "D:\0proyectos\R_\elixr"
log using "D:\0proyectos\R_\elixr\zungdb-z0.log", replace

************************************ TABLA 3 ************************************ 
clear
set more off // para no poner barrita espaciadora ;)

use "data/z0_ind_viv_t3.dta"
descr
*n=936

misstable sum
*n=872 ******CORREGIR EPI_US_RED_DORMIR_C
*egen mis = rowmiss(tenido_malaria epi_estado_canal_agua_c epi_uso_red_dormir_c abunit_pviv abunit_pfal)
*n=923
egen mis = rowmiss(tenido_malaria epi_estado_canal_agua_c)
codebook mis

egen mis1 = rowmiss(tenido_malaria epi_estado_canal_agua_c epi_uso_red_dormir_c)
codebook mis1

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


************************************ EXPLORATORY MODEL (NESTED METHOD) RESULTS ************************************ 

codebook mis
codebook mis1

** PREV-VIVAX+ (NESTED METHOD) ************************************ 
*modelo simple
glm prev_viv i.epi_duerme_cerca_monte_c if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.material_piso_c if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.sero_viv if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.epi_alguien_tuvo_malaria if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.trabajo_rpl if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.epi_malaria_ultimos_meses_c if mis==0, family(poisson) link(log) eform nolog
*modelo progreso
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl if mis==0, family(poisson) link(log) eform nolog
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c if mis==0, family(poisson) link(log) eform nolog
*modelos cluster 
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c if mis==0, family(poisson) link(log) eform nolog vce(cluster vivienda)
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c if mis==0, family(poisson) link(log) eform nolog vce(cluster community_1)
*modelo multinivel
*meglm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c if mis==0 || community_1: , family(poisson) link(log) eform nolog
*meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.sero_viv if mis==0 || vivienda: , or
*meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv if mis==0 || vivienda: R.material_piso_c , or
*meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1 if mis==0 || vivienda: , or
**meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1 if mis==0 || vivienda: R.material_piso_c , or
**meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1 if mis==0 || vivienda: R.material_piso_c || vivienda: R.epi_alguien_tuvo_malaria, or
**meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.sero_viv i.trabajo_rpl i.age_quart i.community_1 if mis==0 || vivienda: R.material_piso_c R.epi_alguien_tuvo_malaria, or
*meglm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv if mis==0 || vivienda: , family(poisson) link(log) eform nolog
*meglm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv if mis==0 || vivienda: R.material_piso_c , family(poisson) link(log) eform nolog


** SERO-VIVAX+ (NESTED METHOD) ************************************ 
*modelo final
glm sero_viv age_7 if mis1==0, family(binomial) link(log) eform nolog
glm sero_viv i.age_quart if mis1==0, family(binomial) link(log) eform nolog
*modelo multinivel
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: , or
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: R.epi_cerca_fuente_agua , or

*sensibilidad x serología mixta
glm sero_viv age_7 if mis1==0 & sero_mix == 0, family(binomial) link(log) eform nolog
glm sero_viv i.age_quart if mis1==0 & sero_mix == 0, family(binomial) link(log) eform nolog


** SERO-FALCIP+ (NESTED METHOD) ************************************ 
*modelo final
glm sero_fal i.community_1 if mis1==0, family(poisson) link(log) eform nolog
glm sero_fal i.community_1 i.age_quart if mis1==0, family(poisson) link(log) eform nolog
glm sero_fal i.community_1 i.age_quart i.prev_fal if mis1==0, family(poisson) link(log) eform nolog
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis1==0, family(poisson) link(log) eform nolog
*glm sero_fal i.community_1 age_7 i.prev_fal i.tenido_malaria if mis1==0, family(poisson) link(log) eform nolog
*modelo multinivel
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: , or
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: R.epi_cerca_fuente_agua , or

*sensibilidad x serología mixta
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis1==0 & sero_mix == 0, family(poisson) link(log) eform nolog





************************************ EXPLORATORY MODEL (NESTED METHOD) DEVELOPMENT ************************************ 


************************************ PREV-VIVAX+ (NESTED METHOD) ************************************ 

corr trabajo_rpl community_1 age_quart sex_8 epi_cercania_fuente_agua_c epi_estuvo_campo_antes_c epi_uso_repelente_mosquito_c epi_uso_mangas_largas_c epi_duerme_cerca_monte_c epi_estado_campos_agricultura_c epi_estado_canal_agua_c material_pared_c material_piso_c epi_cerca_fuente_agua epi_rocia_con_insecticida_c epi_alguien_tuvo_malaria  age_7 residence_fct residence_quart actualmente_estudiando tenido_malaria epi_uso_redes_cama_c epi_duerme_ventanas_abiertas_c epi_frecuencia_rocia_casa_c epi_malaria_ultimos_meses_c sero_viv sero_fal
coldiag2 epi_duerme_cerca_monte_c material_piso_c sero_viv trabajo_rpl  age_quart epi_alguien_tuvo_malaria community_1

*SIN vce(cluster vivienda )**************************

*nivel 0
quietly: glm prev_viv if mis==0, family(binomial) link(log) eform nolog
estimate store m0

*nivel 1
local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_viv i.sero_fal"

foreach var in `l' {
	glm prev_viv `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m01
	lrtest m0 m01 //no
}

*resultado 1
glm prev_viv i.epi_duerme_cerca_monte_c if mis==0, family(binomial) link(log) eform nolog
estimate store m1

*nivel 2

local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_viv i.sero_fal"

foreach var in `l' {
	glm prev_viv i.epi_duerme_cerca_monte_c `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m11
	lrtest m1 m11 //no
}

*resultado 2
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c if mis==0, family(binomial) link(log) eform nolog
estimate store m2

*nivel 3

local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_viv i.sero_fal"

foreach var in `l' {
	glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m21
	lrtest m2 m21 //no
}

*resultado 3
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv if mis==0, family(binomial) link(log) eform nolog
estimate store m3

*nivel 4

local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_fal"

foreach var in `l' {
	glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv `var' if mis==0, family(binomial) link(log) eform nolog
	estimate store m31
	lrtest m3 m31
}


*resultado 4
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria if mis==0, family(binomial) link(log) eform nolog
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria if mis==0, family(poisson) link(log) eform nolog
estimate store m4


*nivel 5

local l = "i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_fal"

foreach var in `l' {
	glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m41
	lrtest m4 m41
}

*resultado 5
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl if mis==0, family(poisson) link(log) eform nolog
estimate store m5


*nivel 6

local l = "i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_fal"

foreach var in `l' {
	glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m51
	lrtest m5 m51
}

*resultado 6
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c if mis==0, family(poisson) link(log) eform nolog
estimate store m6


*nivel 7

local l = "i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.sero_fal"

foreach var in `l' {
	glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c `var' if mis==0, family(poisson) link(log) eform nolog
	estimate store m61
	lrtest m6 m61
}

*no ingresos en nivel 7

*modelo final
glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c if mis==0, family(poisson) link(log) eform nolog
xi: glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.epi_alguien_tuvo_malaria i.trabajo_rpl i.epi_malaria_ultimos_meses_c if mis==0, family(poisson) link(log) eform nolog

*stepwise
*stepwise, pe(.05): xi: glm prev_viv i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.sero_viv i.sero_fal if mis==0, family(poisson) link(log) eform nolog

*cluster vivienda
*glm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1  if mis==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
*modelo multinivel
*meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.sero_viv if mis==0 || vivienda: , or
*meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv if mis==0 || vivienda: R.material_piso_c , or
*meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1 if mis==0 || vivienda: , or
**meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1 if mis==0 || vivienda: R.material_piso_c , or
**meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1 if mis==0 || vivienda: R.material_piso_c || vivienda: R.epi_alguien_tuvo_malaria, or
**meqrlogit prev_viv i.epi_duerme_cerca_monte_c i.sero_viv i.trabajo_rpl i.age_quart i.community_1 if mis==0 || vivienda: R.material_piso_c R.epi_alguien_tuvo_malaria, or

*meglm prev_viv i.epi_duerme_cerca_monte_c i.material_piso_c i.sero_viv i.trabajo_rpl  i.age_quart i.epi_alguien_tuvo_malaria i.community_1 if mis==0 || vivienda: R.material_piso_c || vivienda: R.epi_alguien_tuvo_malaria, family(poisson) link(log) eform

*PROBAR AGREGAR ABUNITS
*i.abunit_pvix_4 
*i.abunit_pfal_4 



************************************ SERO-VIVAX+ (NESTED METHOD) ************************************ 

*SIN vce(cluster vivienda )**************************

****CORREGIR AB MIN O MAX
*abunit_pviv i.abunit_pviv_4 i.sero_fal abunit_pfal i.abunit_pfal_4 

*nivel 0
quietly: glm sero_viv if mis1==0, family(binomial) link(log) eform nolog
estimate store m0

*nivel 1
local l = "i.epi_uso_red_dormir_c i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_viv "

foreach var in `l' {
	glm sero_viv `var' if mis1==0, family(binomial) link(log) eform nolog
	estimate store m01
	lrtest m0 m01 //no
}

*resultado 1
glm sero_viv age_7 if mis1==0, family(binomial) link(log) eform nolog
estimate store m1


*nivel 2
local l = "i.epi_uso_red_dormir_c i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_viv "

foreach var in `l' {
	glm sero_viv age_7 `var' if mis1==0, family(binomial) link(log) eform nolog
	estimate store m11
	lrtest m1 m11 //no
}

*resultado 2
*glm sero_viv age_7 i.community_1 if mis1==0, family(binomial) link(log) eform nolog
*estimate store m2

*modelo final
glm sero_viv age_7 if mis1==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog
*modelo multinivel
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: , or
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: R.epi_cerca_fuente_agua , or

*sensibilidad x serología mixta
glm sero_viv age_7 if mis1==0 & sero_mix == 0, family(binomial) link(log) vce(cluster vivienda ) eform nolog

************************************ SERO-FALCIP+ (NESTED METHOD) ************************************ 

*SIN vce(cluster vivienda )**************************

****CORREGIR AB MIN O MAX
*abunit_pviv i.abunit_pviv_4 i.sero_fal abunit_pfal i.abunit_pfal_4 

*nivel 0
quietly: glm sero_fal if mis1==0, family(binomial) link(log) eform nolog
estimate store m0

*nivel 1
local l = "i.epi_uso_red_dormir_c i.trabajo_rpl i.community_1 i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_fal "

foreach var in `l' {
	glm sero_fal `var' if mis1==0, family(binomial) link(log) eform nolog
	estimate store m01
	lrtest m0 m01 //no
}

*resultado 1
glm sero_fal i.community_1 if mis1==0, family(poisson) link(log) eform nolog
estimate store m1


*nivel 2
local l = "i.epi_uso_red_dormir_c i.trabajo_rpl i.age_quart i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_fal "

foreach var in `l' {
	glm sero_fal i.community_1 `var' if mis1==0, family(poisson) link(log) eform nolog
	estimate store m11
	lrtest m1 m11 //no
}

*resultado 2
glm sero_fal i.community_1 i.age_quart if mis1==0, family(poisson) link(log) eform nolog
estimate store m2


*nivel 3
local l = "i.epi_uso_red_dormir_c i.trabajo_rpl i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c i.prev_fal "

foreach var in `l' {
	glm sero_fal i.community_1 i.age_quart `var' if mis1==0, family(poisson) link(log) eform nolog
	estimate store m21
	lrtest m2 m21 //no
}

*resultado 3
glm sero_fal i.community_1 i.age_quart i.prev_fal if mis1==0, family(poisson) link(log) eform nolog
estimate store m3


*nivel 4
local l = "i.epi_uso_red_dormir_c i.trabajo_rpl i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.tenido_malaria i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c"

foreach var in `l' {
	glm sero_fal i.community_1 i.age_quart i.prev_fal `var' if mis1==0, family(poisson) link(log) eform nolog
	estimate store m31
	lrtest m3 m31 //no
}

*resultado 4
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis1==0, family(poisson) link(log) eform nolog
estimate store m4


*nivel 5
local l = "i.epi_uso_red_dormir_c i.trabajo_rpl i.sex_8 i.epi_cercania_fuente_agua_c i.epi_estuvo_campo_antes_c i.epi_uso_repelente_mosquito_c i.epi_uso_mangas_largas_c i.epi_duerme_cerca_monte_c i.epi_estado_campos_agricultura_c i.epi_estado_canal_agua_c i.material_pared_c i.material_piso_c i.epi_cerca_fuente_agua i.epi_rocia_con_insecticida_c i.epi_alguien_tuvo_malaria  age_7 i.residence_fct i.residence_quart i.actualmente_estudiando i.epi_uso_redes_cama_c i.epi_duerme_ventanas_abiertas_c i.epi_frecuencia_rocia_casa_c i.epi_malaria_ultimos_meses_c"

foreach var in `l' {
	glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria `var' if mis1==0, family(poisson) link(log) eform nolog
	estimate store m41
	lrtest m4 m41 //no
}

*resultado 5
*glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis1==0, family(poisson) link(log) eform nolog
*estimate store m5


*modelo final
glm sero_fal i.community_1 if mis1==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
glm sero_fal i.community_1 i.age_quart if mis1==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
glm sero_fal i.community_1 i.age_quart i.prev_fal if mis1==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis1==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
*glm sero_fal i.community_1 age_7 i.prev_fal i.tenido_malaria if mis1==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
*modelo multinivel
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: , or
*meqrlogit sero_viv i.age_quart i.community_1 if mis1==0 || vivienda: R.epi_cerca_fuente_agua , or

*sensibilidad x serología mixta
glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis1==0 & sero_mix == 0, family(poisson) link(log) vce(cluster vivienda ) eform nolog

************************************ MODELOS FINALES ************************************

*glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.sero_viv if mis==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog
*glm prev_viv i.community_1 i.epi_duerme_cerca_monte_c i.abunit_pviv_4 if mis2==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog

*glm sero_viv i.age_quart i.community_1 if mis==0, family(binomial) link(log) vce(cluster vivienda ) eform nolog

*glm sero_fal i.community_1 i.age_quart i.prev_fal i.tenido_malaria if mis==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog
*glm sero_fal i.community_1 age_7 i.prev_fal i.tenido_malaria if mis==0, family(poisson) link(log) vce(cluster vivienda ) eform nolog



******************************************************** CAUSAL MODEL ******************************************************** 

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




************************************ REVERSE CATALITIC MODEL ************************************ 

codebook community_1

*OBJETIVO: ANALISIS GLOBALES

*********************************** viv

quietly: revcat sero_viv age_7, nolog
estimate store rc0
*28
revcat sero_viv age_7, change(28) age nolog //optimal point after changing between percentile: 17-25
estimate store rc1
lrtest rc0 rc1
*
foreach i of num 23/50 {
	quietly: revcat sero_viv age_7, change(`i') age nolog //optimal point after changing between percentile: 17-25
	estimate store rc1
	lrtest rc0 rc1
}
*
drop p u_p l_p 
revcat sero_viv age_7, change(28) age nolog
revcat_pred p
save "data/z0_revcat_zg_pv", replace
twoway rcap u_p l_p age_7, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

******per comunity

drop p u_p l_p 
revcat sero_viv age_7 if community_1 == 3 //, change(28) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 3, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

** loop
quietly: revcat sero_viv age_7 if community_1 == 3 , nolog
estimate store rc0
foreach i of num 20/40 {
	quietly: revcat sero_viv age_7 if community_1 == 3 , change(`i') age nolog //optimal point after changing between percentile: 17-25
	estimate store rc1
	lrtest rc0 rc1
}

**llanchama
drop p u_p l_p 
revcat sero_viv age_7 if community_1 == 1 //, change(28) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 1, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

**ninarumi
drop p u_p l_p 
revcat sero_viv age_7 if community_1 == 2 //, change(60-70) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 2, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

**ptoalmendra
drop p u_p l_p 
revcat sero_viv age_7 if community_1 == 3, change(37) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 3, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

**zungaro
drop p u_p l_p 
revcat sero_viv age_7 if community_1 == 4 //, change(60-70) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 4, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

*********************************** fal

quietly: revcat sero_fal age_7, nolog
estimate store rc0
*23
revcat sero_fal age_7, change(22) age nolog //optimal point after changing between percentile: 17-25
estimate store rc1
lrtest rc0 rc1
*
foreach i of num 17/30 {
	quietly: revcat sero_fal age_7, change(`i') age nolog //optimal point after changing between percentile: 17-25
	estimate store rc1
	lrtest rc0 rc1
}
*
drop p u_p l_p 
revcat sero_fal age_7, change(22) age nolog
revcat_pred p
save "data/z0_revcat_zg_pf", replace
twoway rcap u_p l_p age_7, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

******per comunity

** loop
quietly: revcat sero_fal age_7 if community_1 == 4 , nolog
estimate store rc0
foreach i of num 20/30 {
	quietly: revcat sero_fal age_7 if community_1 == 4 , change(`i') age nolog //optimal point after changing between percentile: 17-25
	estimate store rc1
	lrtest rc0 rc1
}

**llanchama
drop p u_p l_p 
revcat sero_fal age_7 if community_1 == 1 //, change(28) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 1, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

**ninarumi
drop p u_p l_p 
revcat sero_fal age_7 if community_1 == 2, change(23) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 2, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

**ptoalmendra
drop p u_p l_p 
revcat sero_fal age_7 if community_1 == 3, change(26) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 3, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

**zungaro
drop p u_p l_p 
revcat sero_fal age_7 if community_1 == 4 //, change(60-70) age nolog
revcat_pred p
twoway rcap u_p l_p age_7 if community_1 == 4, lstyle(ci) || scatter p age_7, ylabel(0[0.1]1)

*********************************** plots

*use "data/z0_revcat.dta"


************************************ TABLA 1 /// TABLA 2 ************************************ 


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





************
log close


*************************** recomendaciones *************************

*MOSQUITEROS PAREDES CASA difieren de MOSQUITEROS DE CAMA?
*QUIEN EJECUTA EL ROCIAMIENTO DE INSECTICIDA AL MOSQUITERO
*SI ES UNA ACCION DEL EJEFE DE LA CASA, ESTA CONSULTA DEBE IR AL INICIO, EN SECCION DE ENTRADA, CASA!


