library(tidyverse)
library(haven)
library(broom)

z0db <- read_dta("data/z0_ind_viv_t3.dta") %>% 
  as_factor() %>% 
  select(
    
    id,vivienda,
    
    age_quart,
    sex_8,
    nivel_educacion,
    trabajo_tdy,trabajo_rpl,
    actualmente_estudiando,
    residence_quart,#residence,#
    
    community_1 , 
    
    electricidad_red_publica,
    combustible_cocinar,
    ah_estereo,ah_television,ah_radio,ah_refrigerador,ah_motocicleta,ah_mototaxi,
    material_pared_c,material_piso_c,
    fuente_agua,banio_conexion,
    
    tenido_malaria,
    recibio_tratamiento_malaria,
    enfermedad_cronica,
    #enf_tomo_medicinas_parasitos,
    
    epi_cerca_fuente_agua,#epi_tipo_fuente_agua,epi_distancia_fuente_agua,
    epi_malaria_ultimos_meses_c,#epi_viajo_fuera_villa,
    new_malaria_ultimos_meses_c,
    epi_meses_ultima_malaria,#epi_especie_causo_malaria,
    
    
    epi_alguien_tuvo_malaria,
    new_alguien_tuvo_malaria,
    
    
    epi_uso_repelente_mosquito_c,
    epi_uso_mangas_largas_c,
    epi_uso_ropa_impregnada_c,
    epi_cercania_fuente_agua_c,
    epi_estuvo_campo_antes_c,
    epi_uso_redes_cama_c,
    epi_duerme_ventanas_abiertas_c,
    epi_duerme_cerca_monte_c,
    epi_rocia_con_insecticida_c,
    
    
    epi_ultimas_semanas_viajo,
    epi_frecuencia_rocia_casa_c,
    epi_estado_campos_agricultura_c,
    epi_estado_canal_agua_c,
    
    epi_uso_red_dormir_c,
    sero_viv,
    prev_viv
  ) %>% 
  mutate(prev_viv=as.numeric(prev_viv)-1)

z0db %>% count(prev_viv)
#z0db %>% visdat::vis_dat()
z0db %>% glimpse()

z0db_cc <- z0db %>% filter(complete.cases(.))
z0db_cc %>% glimpse()

myformula <- as.formula(prev_viv ~ 
                        age_quart+
                        sex_8+
                        nivel_educacion+
                        trabajo_tdy+trabajo_rpl+
                        actualmente_estudiando+
                        residence_quart+#residence+#
                        
                        community_1 + 
                        
                        electricidad_red_publica+
                        combustible_cocinar+
                        ah_estereo+ah_television+ah_radio+ah_refrigerador+ah_motocicleta+ah_mototaxi+
                        material_pared_c+material_piso_c+
                        fuente_agua+banio_conexion+
                        
                        tenido_malaria+
                        recibio_tratamiento_malaria+
                        enfermedad_cronica+
                        #enf_tomo_medicinas_parasitos+
                        
                        epi_cerca_fuente_agua+#epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                        epi_malaria_ultimos_meses_c+#epi_viajo_fuera_villa+
                        new_malaria_ultimos_meses_c+
                        epi_meses_ultima_malaria+#epi_especie_causo_malaria+
                        
                        
                        epi_alguien_tuvo_malaria+
                        new_alguien_tuvo_malaria+
                        
                        
                        epi_uso_repelente_mosquito_c+
                        epi_uso_mangas_largas_c+
                        epi_uso_ropa_impregnada_c+
                        epi_cercania_fuente_agua_c+
                        epi_estuvo_campo_antes_c+
                        epi_uso_redes_cama_c+
                        epi_duerme_ventanas_abiertas_c+
                        epi_duerme_cerca_monte_c+
                        epi_rocia_con_insecticida_c+
                        
                        
                        epi_ultimas_semanas_viajo+
                        epi_frecuencia_rocia_casa_c+
                        epi_estado_campos_agricultura_c+
                        epi_estado_canal_agua_c+
                        
                        epi_uso_red_dormir_c+
                        sero_viv)

glm.full <- glm(myformula, data = z0db_cc, family = poisson(link = "log"))
glm.null <- glm(prev_viv ~ 1, data = z0db_cc, family = poisson(link = "log"))

#https://rstudio-pubs-static.s3.amazonaws.com/2899_a9129debf6bd47d2a0501de9c0dc583d.html

add1(glm.null,scope = myformula,test = "LRT") %>% broom::tidy() %>% arrange(p.value) %>% rownames_to_column("rank") %>% print() -> rank_l1
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c),scope = myformula,test = "LRT") %>% broom::tidy() %>% arrange(p.value) %>% rownames_to_column("rank") %>% print() -> rank_l2
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c),scope = myformula,test = "LRT") %>% broom::tidy() %>% arrange(p.value) %>% rownames_to_column("rank") %>% print() -> rank_l3
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria),scope = myformula,test = "LRT") %>% broom::tidy() %>% arrange(p.value) %>% rownames_to_column("rank") %>% print() -> rank_l4
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria + sero_viv),scope = myformula,test = "LRT") %>% broom::tidy() %>% arrange(p.value) %>% rownames_to_column("rank") %>% print() -> rank_l5
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria + sero_viv + nivel_educacion),scope = myformula,test = "LRT") %>% broom::tidy() %>% arrange(p.value) %>% rownames_to_column("rank") %>% print() -> rank_l6

wm1 <- update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria + sero_viv + nivel_educacion)

m1 <- wm1 %>% tidy() %>% mutate(pr=exp(estimate)) %>% rownames_to_column()
m2 <- wm1 %>% confint_tidy() %>% mutate_all(funs(exp)) %>% rownames_to_column()
left_join(m1,m2) %>% 
  dplyr::select(term,log.pr=estimate,se=std.error,pr,
                conf.low,conf.high,p.value) %>% 
  mutate_at(.vars = vars(-term,-p.value),round, digits = 2) %>% 
  mutate_at(.vars = vars(p.value),round, digits = 3) 

#tareas:
#evaluar retirar variable con missing poco relevante
#cruza rankings por nivel
#modelos con predictores no incluidos y temáticamente relevantes

rank_l1