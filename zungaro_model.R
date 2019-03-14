
# import libraries --------------------------------------------------------

library(tidyverse)
library(haven)
library(naniar)
library(broom)
library(CGPfunctions)

rm(list = ls())
theme_set(theme_bw())

# import dataset ----------------------------------------------------------

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

# evaluate outcome --------------------------------------------------------

z0db %>% count(prev_viv)

# identify missings -------------------------------------------------------
#http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html

#naniar::gg_miss_var(z0db)
miss_var_summary(z0db)
n_miss(z0db)
pct_miss(z0db)
num <- z0db %>% filter(!complete.cases(.)) %>% dim()
dem <- z0db %>% dim()
num/dem
#z0db %>% visdat::vis_dat()
#z0db %>% glimpse()

#how many months ago was your last malaria episode?
#z0db %>% filter(!complete.cases(.)) %>% 
#  select(vivienda,id,epi_meses_ultima_malaria)

# complete case analysis --------------------------------------------------

z0db_cc <- z0db %>% filter(complete.cases(.))
z0db_cc %>% glimpse()

# create formula ----------------------------------------------------------

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

# create functions --------------------------------------------------------

glm.full <- glm(myformula, data = z0db_cc, family = poisson(link = "log"))
glm.null <- glm(prev_viv ~ 1, data = z0db_cc, family = poisson(link = "log"))

# nested-forward-LRT ------------------------------------------------------
#https://rstudio-pubs-static.s3.amazonaws.com/2899_a9129debf6bd47d2a0501de9c0dc583d.html

epi_tidynested <- function(add_nested,level=i) {
  add_nested %>% 
    broom::tidy() %>% 
    arrange(p.value) %>% 
    print() %>% 
    rownames_to_column("rank") %>% 
    select(term,df,LRT,p.value,rank) %>% 
    rename_at(.vars = c("rank","LRT","p.value"),.funs = str_replace, "(.+)",paste0("\\1\\_",level))
}

add1(glm.null,scope = myformula,test = "LRT") %>% 
  epi_tidynested(1) -> rank_l1
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c),scope = myformula,test = "LRT") %>% 
  epi_tidynested(2) -> rank_l2
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c),scope = myformula,test = "LRT") %>% 
  epi_tidynested(3) -> rank_l3
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria),scope = myformula,test = "LRT") %>% 
  epi_tidynested(4) -> rank_l4
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria + sero_viv),scope = myformula,test = "LRT") %>% 
  epi_tidynested(5) -> rank_l5
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria + sero_viv + nivel_educacion),scope = myformula,test = "LRT") %>% 
  epi_tidynested(6) -> rank_l6

# parsimonius model -------------------------------------------------------------

wm1 <- update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + epi_meses_ultima_malaria + sero_viv + nivel_educacion)

epi_tidymodel <- function(wm1) {
  m1 <- wm1 %>% tidy() %>% mutate(pr=exp(estimate)) %>% rownames_to_column()
  m2 <- wm1 %>% confint_tidy() %>% mutate_all(list(exp)) %>% rownames_to_column()
  
  left_join(m1,m2) %>% 
    dplyr::select(term,log.pr=estimate,se=std.error,pr,
                  conf.low,conf.high,p.value) %>% 
    mutate_at(.vars = vars(-term,-p.value),round, digits = 3) %>% 
    mutate_at(.vars = vars(p.value),round, digits = 3) %>% 
    print() %>% 
    return()
}

mpf_tidy <- epi_tidymodel(wm1)

# table: LRT p values -----------------------------------------------------

rank_table <- rank_l1 %>% 
  left_join(rank_l2) %>% 
  left_join(rank_l3) %>% 
  left_join(rank_l4) %>% 
  left_join(rank_l5) %>% 
  left_join(rank_l6) %>% 
  mutate_at(vars(starts_with("rank_")), funs(as.numeric)) %>% 
  mutate(rank_2=if_else(is.na(rank_2),rank_1,rank_2+1),
         rank_3=if_else(is.na(rank_3),rank_2,rank_3+2),
         rank_4=if_else(is.na(rank_4),rank_3,rank_4+3),
         rank_5=if_else(is.na(rank_5),rank_4,rank_5+4),
         rank_6=if_else(is.na(rank_6),rank_5,rank_6+5)
  ) %>% 
  select(-starts_with("LRT")) %>% 
  filter(term!="<none>")

rank_table %>% arrange(rank_6) %>% print(n=Inf)
#rank_table %>% arrange(rank_2) %>% print(n=Inf)
#rank_table %>% arrange(rank_3) %>% print(n=Inf)
#rank_table %>% arrange(rank_4) %>% print(n=Inf)
#rank_table %>% arrange(rank_5) %>% print(n=Inf)

# graph: visualize levels -------------------------------------------------

rank_table_term <- rank_table %>% 
  select(term,starts_with("rank")) %>% 
  arrange(rank_6) %>% 
  column_to_rownames("term") 

#library(slopegraph)
#slopegraph(rank_table_term,xlim = c(-5.5,8.5), cex.lab = 0.9, cex.num = 0.9,na.span=TRUE)

rank_table_term_l <- rank_table_term %>% 
  rownames_to_column("term") %>% 
  as_tibble() %>% 
  gather(rank,value,-term) %>% 
  #filter(rank!="rank_6") %>% 
  mutate(#term=str_replace(term,"(.+)\\_(.+)","\\2\\s\\1"),
         term=as.factor(term),
         rank=as.ordered(rank))

CGPfunctions::newggslopegraph(rank_table_term_l,rank,value,term)

#MySpecial <- list(  
#  # move the x axis labels up top
#  scale_x_discrete(position = "top"),
#  theme_bw(),
#  # Format tweaks
#  # Remove the legend
#  theme(legend.position = "none"),
#  # Remove the panel border
#  theme(panel.border     = element_blank()),
#  # Remove just about everything from the y axis
#  theme(axis.title.y     = element_blank()),
#  theme(axis.text.y      = element_blank()),
#  theme(panel.grid.major.y = element_blank()),
#  theme(panel.grid.minor.y = element_blank()),
#  # Remove a few things from the x axis and increase font size
#  theme(axis.title.x     = element_blank()),
#  theme(panel.grid.major.x = element_blank()),
#  theme(axis.text.x.top      = element_text(size=12)),
#  # Remove x & y tick marks
#  theme(axis.ticks       = element_blank()),
#  # Format title & subtitle
#  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)),
#  theme(plot.subtitle    = element_text(hjust = 0.5))
#)
#
#rank_table_term_l %>% 
#  ggplot(aes(rank, value, group=term)) +
#  geom_line(aes(colour=term,alpha = 1),size=1) +
#  ggrepel::geom_text_repel(data = rank_table_term_l %>% filter(rank == "rank_1"), 
#                           aes(label = term) , 
#                           hjust = "left", 
#                           fontface = "bold", 
#                           size = 3, 
#                           nudge_x = -5, 
#                           direction = "y") +
#  MySpecial


# extra-models ------------------------------------------------------------

rank_table %>% arrange(rank_6) %>% print(n=Inf)

mpf_tidy

ex1 <- update(wm1, ~ . + enfermedad_cronica)
ex1_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + combustible_cocinar)
ex2_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + sex_8)
ex3_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + epi_frecuencia_rocia_casa_c)
ex4_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + trabajo_rpl)
ex5_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + trabajo_tdy)
ex6_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + age_quart)
ex7_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + community_1)
ex8_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + material_pared_c)
ex9_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + recibio_tratamiento_malaria)
ex10_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + epi_cerca_fuente_agua)
ex11_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

ex1 <- update(wm1, ~ . + epi_duerme_ventanas_abiertas_c)
ex12_tidy <- epi_tidymodel(ex1) %>% slice(-c(1:7))

# modelos simple ----------------------------------------------------------

mpf_tidy
update(glm.null, ~ . + epi_duerme_cerca_monte_c) %>% epi_tidymodel()
update(glm.null, ~ . + material_piso_c) %>% epi_tidymodel()
update(glm.null, ~ . + epi_meses_ultima_malaria) %>% epi_tidymodel()
update(glm.null, ~ . + sero_viv) %>% epi_tidymodel()
update(glm.null, ~ . + nivel_educacion) %>% epi_tidymodel()
