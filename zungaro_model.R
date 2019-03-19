
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
    
    age_quart,age_7,
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
    recibio_tratamiento_malaria_c,
    enfermedad_cronica,
    #enf_tomo_medicinas_parasitos,
    
    epi_cerca_fuente_agua,#epi_tipo_fuente_agua,epi_distancia_fuente_agua,
    epi_malaria_ultimos_meses_c,#epi_viajo_fuera_villa,
    new_malaria_ultimos_meses_c,
    #epi_meses_ultima_malaria,#epi_especie_causo_malaria,
    
    
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
    
    #epi_uso_red_dormir_c,
    sero_viv,
    prev_viv
  ) %>% 
  mutate(prev_viv=as.numeric(prev_viv)-1)

# evaluate outcome --------------------------------------------------------

z0db %>% count(prev_viv)
z0db %>% dim()

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
#z0db %>% filter(!complete.cases(.)) %>% visdat::vis_dat()
#  select(vivienda,id,epi_meses_ultima_malaria)

# duplicates --------------------------------------------------------------

z0db %>% filter(duplicated(.))

# complete case analysis --------------------------------------------------

z0db_cc <- z0db %>% filter(complete.cases(.))
z0db_cc %>% dim()
#z0db_cc %>% glimpse()

# create formula ----------------------------------------------------------

myformula <- as.formula(prev_viv ~ 
                        age_quart+age_7+
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
                        recibio_tratamiento_malaria_c+
                        enfermedad_cronica+
                        #enf_tomo_medicinas_parasitos+
                        
                        epi_cerca_fuente_agua+#epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                        epi_malaria_ultimos_meses_c+#epi_viajo_fuera_villa+
                        new_malaria_ultimos_meses_c+
                        #epi_meses_ultima_malaria+#epi_especie_causo_malaria+
                        
                        
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
                        
                        #epi_uso_red_dormir_c+
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
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + sero_viv),scope = myformula,test = "LRT") %>% 
  epi_tidynested(4) -> rank_l4
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + sero_viv + new_alguien_tuvo_malaria),scope = myformula,test = "LRT") %>% 
  epi_tidynested(5) -> rank_l5
add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + sero_viv + new_alguien_tuvo_malaria + trabajo_rpl),scope = myformula,test = "LRT") %>% 
  epi_tidynested(6) -> rank_l6
#add1(update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + sero_viv + new_alguien_tuvo_malaria + trabajo_rpl + age_quart),scope = myformula,test = "LRT") %>% 
#  epi_tidynested(7) -> rank_l7

# parsimonius model -------------------------------------------------------------

wm1 <- update(glm.null, ~ . + epi_duerme_cerca_monte_c + material_piso_c + sero_viv + new_alguien_tuvo_malaria + trabajo_rpl)
#wm1 <- update(glm.null, ~ . + community_1 + epi_duerme_cerca_monte_c + sero_viv + age_quart + sex_8 + banio_conexion + epi_frecuencia_rocia_casa_c)

epi_tidymodel_pr <- function(wm1) {
  m1 <- wm1 %>% tidy() %>% mutate(pr=exp(estimate)) %>% rownames_to_column()
  m2 <- wm1 %>% confint_tidy() %>% mutate_all(list(exp)) %>% rownames_to_column()
  
  left_join(m1,m2) %>% 
    dplyr::select(term,log.pr=estimate,se=std.error,pr,
                  conf.low,conf.high,p.value) %>% 
    mutate_at(.vars = vars(-term,-p.value),round, digits = 5) %>% 
    mutate_at(.vars = vars(p.value),round, digits = 5) %>% 
    #print() %>% 
    return()
}

parsimonioso_tidy <- epi_tidymodel_pr(wm1) %>% print()

# table: LRT p values -----------------------------------------------------

rank_table <- rank_l1 %>% 
  left_join(rank_l2) %>% 
  left_join(rank_l3) %>% 
  left_join(rank_l4) %>% 
  left_join(rank_l5) %>% select(-df) %>% 
  left_join(rank_l6) %>% select(-df) %>% #lost of one degree of freedom in work
  mutate_at(vars(starts_with("rank_")), funs(as.numeric)) %>% 
  mutate(rank_2=if_else(is.na(rank_2),rank_1,rank_2+1),
         rank_3=if_else(is.na(rank_3),rank_2,rank_3+2),
         rank_4=if_else(is.na(rank_4),rank_3,rank_4+3),
         rank_5=if_else(is.na(rank_5),rank_4,rank_5+4),
         rank_6=if_else(is.na(rank_6),rank_5,rank_6+5)
  ) %>% 
  select(-starts_with("LRT")) %>% 
  filter(term!="<none>")

rank_table %>% arrange(rank_6) %>% 
  left_join(rank_l1 %>% select(term,df),by = "term") %>% 
  select(term,df,everything()) %>% 
  print(n=Inf) %>% 
  xlsx::write.xlsx("table/z0-nested-table.xlsx")
#rank_table %>% arrange(rank_2) %>% print(n=Inf)
#rank_table %>% arrange(rank_3) %>% print(n=Inf)
#rank_table %>% arrange(rank_4) %>% print(n=Inf)
#rank_table %>% arrange(rank_5) %>% print(n=Inf)

# graph: visualize levels -------------------------------------------------

#TAREA:
#- personalizar slopegraph

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

CGPfunctions::newggslopegraph(rank_table_term_l,rank,value,term,Title = "Niveles modelo anidado",SubTitle = "Y: PCR+ P. vivax")
ggsave("figure/slopegraph-nested.png",width = 12,height = 8)

# extra-models ------------------------------------------------------------

rank_table %>% arrange(rank_6) %>% print(n=Inf)

parsimonioso_tidy

ajustado_var <- c("age_quart",
                  "sex_8",
                  "nivel_educacion",
                  "trabajo_tdy",
                  "community_1",
                  "residence_quart",
                  "electricidad_red_publica",
                  "combustible_cocinar",
                  "ah_radio",
                  "ah_mototaxi",
                  "material_pared_c",
                  "tenido_malaria",
                  "recibio_tratamiento_malaria_c",
                  "enfermedad_cronica",
                  "epi_cerca_fuente_agua",
                  "new_malaria_ultimos_meses_c",
                  "epi_estuvo_campo_antes_c",
                  "epi_duerme_ventanas_abiertas_c",
                  "epi_ultimas_semanas_viajo",
                  "epi_frecuencia_rocia_casa_c")

#https://adv-r.hadley.nz/quasiquotation.html
#19.4.1 Unquoting one argument
epi_postadjusted <- function(wm1, ajustado_var) {
  
  ex0_tidy <- parsimonioso_tidy %>% .[FALSE,]
  
  for (i in 1:length(ajustado_var)) { #i=1
    
    x_var <- sym(ajustado_var[i])
    
    ex1_tidy <- update(wm1, expr(~ . + !!x_var)) %>% 
      epi_tidymodel_pr() %>% slice(-c(1:7))
    
    ex0_tidy <- union_all(ex0_tidy,ex1_tidy)
    
  }
  
  postadjusted <- ex0_tidy %>% rename_at(.vars = vars(-term),.funs = str_replace, "(.+)","\\1\\_post")
  
  postadjusted %>% return()
  
}

ajustados_post <- epi_postadjusted(wm1,ajustado_var)
ajustados_post %>% print(n=Inf)


# collineality -------------------------------------------------------
#https://stackoverflow.com/questions/7337761/linear-regression-na-estimate-just-for-last-coefficient
#https://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient

update(wm1, ~ . + trabajo_tdy) %>% 
  epi_tidymodel_pr()

update(glm.null, ~ . + trabajo_tdy) %>% 
  epi_tidymodel_pr()

update(glm.null, ~ . + trabajo_rpl) %>% 
  epi_tidymodel_pr()

update(glm.null, ~ . + trabajo_rpl + trabajo_tdy) %>% 
  epi_tidymodel_pr()

z0db_cc %>% count(trabajo_rpl,trabajo_tdy)

# __solucion --------------------------------------------------------------

update(wm1, ~ . - trabajo_rpl + trabajo_tdy) %>% 
  epi_tidymodel_pr() #%>% slice(-c(1:6))

# modelos simple ----------------------------------------------------------

parsimonioso_var <- rank_table %>% arrange(rank_6) %>% filter(is.na(p.value_6)) %>% .$term
#parsimonioso_var
#ajustado_var
todo_var <- c(parsimonioso_var,ajustado_var)

epi_presimples <- function(glm.null,todo_var) {
  
  sm0_tidy <- parsimonioso_tidy %>% .[FALSE,]
  nul_tidy <- glm.null %>% epi_tidymodel_pr()
  
  for (i in 1:length(todo_var)) {
    
    x_var <- sym(todo_var[i])
    
    sm1 <- update(glm.null, expr(~ . + !!x_var)) %>% 
      epi_tidymodel_pr()
    
    sm0_tidy <- union_all(sm0_tidy,sm1)
    
  }
  
  simples_tidy <- sm0_tidy %>% 
    filter(term!="(Intercept)") %>% 
    union_all(nul_tidy) %>% 
    rename_at(.vars = vars(-term),.funs = str_replace, "(.+)","\\1\\_simple")
  
  simples_tidy %>% return()
  
}

simples_tidy <- epi_presimples(glm.null,todo_var)
simples_tidy %>% print(n=Inf)

# final output ------------------------------------------------------------

final <- left_join(simples_tidy,parsimonioso_tidy,"term") %>% 
  left_join(ajustados_post)

final %>% print(n=Inf)
final %>% as.data.frame() %>% 
  xlsx::write.xlsx("table/z0-nested-r.xlsx",showNA = FALSE)
