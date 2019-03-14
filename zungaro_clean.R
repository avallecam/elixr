#just reproduce all the cleaning process
#03-covariate
#in bash

library(tidyverse)
library(stringr)
library(forcats)
library(naniar)

rm(list = ls())
theme_set(theme_bw())

# BASE FINAL -----------------------------------------------------

bas <- read_rds("data/x-zungaro_basal.rds") %>% 
  replace_with_na(replace = list(pertenencia_casa = "Otro (especifique)",
                                 recibio_tratamiento_malaria =  "No sabe",
                                 enf_tomo_medicinas_parasitos =  "No Sabe",
                                 epi_estado_campos_agricultura = "98. No sabe")) %>% #%>% count(pertenencia_casa)
  mutate(trabajo_tdy=fct_collapse(trabajo,
                                  "Housewife"=c("Ama de casa"),
                                  "Farmer/Breeder/Guard/Fisher"=c("Agricultor","pescador","Vigilante","Granjero"),
                                  "Laborer/Trader/Independent/Driver(Mototaxi)"=c("Obrero/albanil/construccion","Comerciante","independiente",
                                                     "Mecanico/tecnico","Administrativo/limpieza/profesor","carpintero",
                                                     "panadero","Profesional de la salud","guia en la unap","pensionista",
                                                     "mototaxista","Chofer"),
                                  "None/Unemployed/Pensionist"=c("pensionista","no aplica","desempleado"),
                                  "Student"=c("Estudiante")
                                  ),
         #definir y corregir por missing
         trabajo_rpl=if_else(trabajo_tdy=="Farmer/Breeder/Guard/Fisher","Outdoors","Other"),
         trabajo_rpl=if_else(is.na(trabajo_rpl) & actualmente_estudiando=="Si","Other",trabajo_rpl),
         trabajo_rpl=fct_relevel(trabajo_rpl,"Outdoors"),
         trabajo_tdy=fct_relevel(trabajo_tdy,"Farmer/Breeder/Guard/Fisher"),
         
         nivel_educacion_c=fct_collapse(nivel_educacion,
                                      "Primary"=c("primaria"),
                                      #"Secondary"=c("secundaria incompleta","secundaria completa"),
                                      "Incomplete Secondary"=c("secundaria incompleta"),
                                      #"Complete Secondary"=c("secundaria completa"),
                                      "Complete Secondary or more"=c("secundaria completa", "tecnica","universitaria"),
                                      "None"=c("sin educacion","inicial")
                                      ),
         
         nivel_educacion=fct_collapse(nivel_educacion,
                                      "Primary"=c("primaria"),
                                      "Secondary or more"=c("secundaria incompleta","secundaria completa","tecnica","universitaria"),
                                      #"Secondary"=c("secundaria incompleta","secundaria completa"),
                                      #"Incomplete Secondary"=c("secundaria incompleta"),
                                      #"Complete Secondary"=c("secundaria completa"),
                                      #"Complete Secondary or more"=c("secundaria completa", "tecnica","universitaria"),
                                      "None"=c("sin educacion","inicial")
         ),
         
         tenido_malaria=fct_collapse(tenido_malaria,"no"=c("no","no sabe")),
         tenido_malaria=if_else(cuantas_tenido_malaria==0,"no",as.character(tenido_malaria)) %>% as.factor(),
         
         epi_uso_repelente_mosquito_c=fct_collapse(epi_uso_repelente_mosquito,
                                                   "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                                   "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_uso_mangas_largas_c=fct_collapse(epi_uso_mangas_largas,
                                              "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                              "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_uso_ropa_impregnada_c=fct_collapse(epi_uso_ropa_impregnada,
                                                "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                                "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_cercania_fuente_agua_c=fct_collapse(epi_cercania_fuente_agua,
                                                 "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                                 "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_estuvo_campo_antes_c=fct_collapse(epi_estuvo_campo_antes,
                                               "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                               "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_uso_redes_cama_c=fct_collapse(epi_uso_redes_cama,
                                           "nunca-raramente"=c("0. no aplica","1. nunca","2. raramente"),
                                           "algunas-siempre"=c("3. algunas veces","4. a menudo","5. siempre")),
         epi_duerme_ventanas_abiertas_c=fct_collapse(epi_duerme_ventanas_abiertas,
                                                     "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                                     "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_duerme_cerca_monte_c=fct_collapse(epi_duerme_cerca_monte,
                                               "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                               "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_rocia_con_insecticida_c=fct_collapse(epi_rocia_con_insecticida,
                                                  "nunca-raramente"=c("0. No Aplica","1. Nunca","2. Raramente"),
                                                  "algunas-siempre"=c("3. Algunas Veces","4. A Menudo","5. Siempre")),
         epi_estado_campos_agricultura_c=fct_collapse(epi_estado_campos_agricultura,
                                                  "nunca-raravez"=c("1. Nunca","2. Rara vez"),
                                                  "amenudo-siempre"=c("3. A menudo","4. A veces","5. Siempre")),
         epi_estado_campos_agricultura_c=as.character(epi_estado_campos_agricultura_c),
         epi_estado_campos_agricultura_c=if_else(is.na(epi_estado_campos_agricultura_c) & trabajo=="Agricultor",
                                                 "amenudo-siempre",
                                                 epi_estado_campos_agricultura_c),
         #corregir segun otra variable
         epi_estado_campos_agricultura_c=as.factor(epi_estado_campos_agricultura_c),
         epi_estado_campos_agricultura_c=fct_explicit_na(epi_estado_campos_agricultura_c,na_level = "nunca-raravez"),
         epi_estado_canal_agua_c=fct_collapse(epi_estado_canal_agua,
                                                      "nunca-raravez"=c("1. Nunca","2. Rara vez"),
                                                      "amenudo-siempre"=c("3. A menudo","4. A veces","5. Siempre")),
         epi_frecuencia_rocia_casa_c=fct_collapse(epi_frecuencia_rocia_casa,
                                                  "nunca-raravez-unaxano"=c("Nunca","Rara vez","Una vez al ano")),
         epi_uso_red_dormir_c=fct_collapse(epi_uso_red_dormir,
                                           "nunca"="no sabe",
                                           "desde siempre"="recientemente"
                                           ) ,
         
         #recategorizar EDAD: 0-6 6-11 12-17 18+
         age_jem = cut(age_7,c(0,6,11,17,Inf)),
         age_gcb = cut(age_7,c(0,5,18,Inf)),
         age_oms = cut(age_7,c(0,11,17,29,59,Inf)),
         age_c18 = cut(age_7,c(0,17,Inf)),
         
         #estado civil restringido a MAYORES DE 15 AÑOS
         estado_civil_15 = if_else(age_7>15,estado_civil,NA_integer_),
         
         #limpiar estado de estudiante
         #trabajo_tdy_s=if_else(trabajo_tdy!="Student",trabajo_tdy,NA_integer_),
         trabajo_tdy_s=fct_collapse(trabajo_tdy,
                                    "None/Unemployed/Pensionist/Student"=c("Student","None/Unemployed/Pensionist")),
         
         #base vivienda
         #MATERIAL piso pared : ¿como dicotomizar? cemento y no cemento (madera, triplay, arena, tierra)
         material_pared_c = fct_collapse(material_pared,
                                         "madera/tripley"=c("madera","tripley")),
         material_piso_c = fct_collapse(material_piso,
                                        "tierra/arena/madera"=c("tierra o arena","madera")),
         
         #EPI CERCA FUENTE AGUA (270 -> 254): <100, >100, no cerca
         epi_distancia_fuente_agua_c=fct_collapse(epi_distancia_fuente_agua,
                                    "Mas de 100 metros"=c("101 - 500 metros","Mas de 500 metros")),
         #EPI MALARIA ULTIMOS MESES (266 -> 141): sí/no -> meses a última infeccion
         epi_malaria_ultimos_meses_c=fct_collapse(epi_malaria_ultimos_meses,
                                                  "No/No sabe"=c("No","No Sabe")),
         
         #base sujetos
         #restringir EDUCACION >= 18 AÑOS
         nivel_educacion_18 = if_else(age_7>18,nivel_educacion,NA_integer_),
         nivel_educacion_c_18 = if_else(age_7>18,nivel_educacion_c,NA_integer_),
         #restringir OCUPACION > 15 (al igual que ESTADO CIVIL)
         trabajo_tdy_15 = if_else(age_7>18,trabajo_tdy,NA_integer_),
         trabajo_tdy_s_15 = if_else(age_7>18,trabajo_tdy_s,NA_integer_),
         actualmente_estudiando_18 = if_else(age_7<=18,actualmente_estudiando,NA_integer_),
         #categorizar jefe de familia
         relacion_jefe_familia_c = as.character(relacion_jefe_familia),
         relacion_jefe_familia_c = if_else(relacion_jefe_familia_c=="Cabeza de familia",relacion_jefe_familia_c,
                                           if_else(relacion_jefe_familia_c=="Esposa/Esposo o pareja" 
                                                   | relacion_jefe_familia_c=="Hijo o Hijastro quien es soltero y no tiene hijos"
                                                   | relacion_jefe_familia_c=="Hijo o Hijastro quien es soltero y tiene hijos"
                                                   | relacion_jefe_familia_c=="Hijo o Hijastro quien tiene una pareja y familia",
                                                   "Esposa/Esposo/Pareja/Hijo/hijastro","Otro familiar"
                                                   )),
         relacion_jefe_familia_c = as.factor(relacion_jefe_familia_c),
         #colapsar dondebuscotratamiento
         donde_busco_tratamiento_c= fct_collapse(donde_busco_tratamiento,
                                                 "Medicina tradicional u otros"=c("Otro. (especifique)","Medicina Tradicional(Chaman,Curandero)")),
         
         #restringir preguntas condicionales
         #epi_cerca_fuente_agua
         epi_tipo_fuente_agua=if_else(epi_cerca_fuente_agua=="Si",epi_tipo_fuente_agua,NA_integer_),
         epi_distancia_fuente_agua=if_else(epi_cerca_fuente_agua=="Si",epi_distancia_fuente_agua,NA_integer_),
         epi_distancia_fuente_agua=fct_relevel(epi_distancia_fuente_agua,"Menor de 100 metros"),
         fuente_agua=fct_recode(fuente_agua,
                                "agua de pozo"="Pozo",
                                "agua de lluvia"="Tanque de agua o similar",
                                "Red pública, fuera de casa, de uso compartido"="Tuberia publica, fuente fuera de la casa, compartido con la comunidad",
                                "Red pública, dentro de casa, de uso exclusivo"="Sistema publico en la casa, uso exclusivo de la familia",
                                "Red pública, dentro de casa, de uso exclusivo"="TuberiƒÂa publica, fuente fuera de la casa, compartido con la comunidad",
                                "Red pública, dentro de casa, de uso exclusivo"="Sistema publico en casa, compartido en otras casas"),
         fuente_agua=fct_collapse(fuente_agua,"agua de pozo o lluvia"=c("agua de pozo","agua de lluvia")),
         #epi_malaria_ultimos_meses
         epi_viajo_fuera_villa=if_else(epi_malaria_ultimos_meses=="Si",epi_viajo_fuera_villa,NA_integer_),
         #epi_meses_ultima_malaria=if_else(epi_malaria_ultimos_meses=="Si",epi_meses_ultima_malaria,NA_real_),
         epi_especie_causo_malaria=if_else(epi_malaria_ultimos_meses=="Si",epi_especie_causo_malaria,NA_integer_),
         
         #relevelear
         epi_frecuencia_rocia_casa_c=fct_relevel(epi_frecuencia_rocia_casa_c,
                                                 "nunca-raravez-unaxano","Cada 6 meses"
                                                 #"Nunca","raravez-unaxano"
                                                 ),
         epi_alguien_tuvo_malaria=fct_relevel(epi_alguien_tuvo_malaria,
                                              "Nunca","Si, hace algunos anos","Si, el ultimo ano"),
         
         #linealidad de residencia
         residence_ten=cut(residence,c(quantile(residence,probs = seq(0,1,0.1),na.rm = T)[1],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[2],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[3],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[4],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[5],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[6],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[7],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[8],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[9],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[10],
                                       quantile(residence,probs = seq(0,1,0.1),na.rm = T)[11]),
                           include.lowest = T),
         residence_quart=cut(residence,c(quantile(residence,probs = seq(0,1,0.25),na.rm = T)[1],
                                         quantile(residence,probs = seq(0,1,0.25),na.rm = T)[2],
                                         quantile(residence,probs = seq(0,1,0.25),na.rm = T)[3],
                                         quantile(residence,probs = seq(0,1,0.25),na.rm = T)[4],
                                         quantile(residence,probs = seq(0,1,0.25),na.rm = T)[5]),
                             include.lowest = T),
         banio_conexion=fct_collapse(banio_conexion,
                                   "campo abierto, silo, sin banio"=c("Campo abierto","silo","No tiene banio","NA"),
                                   "letrina"=c("Letrina","Sistema publico fuera de la casa")),
         banio_conexion=fct_collapse(banio_conexion,"otro (letrina, campo abierto, silo, sin banio)"=c("letrina","campo abierto, silo, sin banio")),
         banio_conexion=fct_relevel(banio_conexion,"Sistema publico dentro de la casa")
         
         #epialguientuvomalaria ---> VARIABLE DE VIVIENDA igual a otra pregunta!!!
         #epiredesusadasactualmente --> recategorizar
         
         #critica de muestreo
         #relacionjefefamilia
         #sexo
         
         #corregir abunits_cuartil
         
         
         ) %>% 
  select(-opciones_tenido_malaria)

#bas %>% count(donde_busco_tratamiento_c)
#bas %>% count(relacion_jefe_familia_c)
#bas %>% count(actualmente_estudiando_18)

#bas %>% count(Ab.unit_Pviv_4,Ab.unit_Pfal_4,sero_c) %>% print(n=Inf)

bas %>% glimpse()

#bas %>% count(epi_cerca_fuente_agua,epi_tipo_fuente_agua,epi_distancia_fuente_agua)
#bas %>% count(epi_malaria_ultimos_meses,epi_viajo_fuera_villa,epi_meses_ultima_malaria,epi_especie_causo_malaria)
#bas %>% count(epi_malaria_ultimos_meses)

bas %>% count(epi_distancia_fuente_agua)
bas %>% count(fuente_agua)
bas %>% count(banio_conexion)

bas %>% count(community_1)

bas %>% count(nivel_educacion_c)
bas %>% count(nivel_educacion)
bas %>% count(actualmente_estudiando)
bas %>% count(trabajo_rpl,trabajo_tdy)

bas %>% 
  select(epi_viajo_fuera_villa,epi_ultimas_semanas_viajo,epi_numero_veces_viajo) %>% 
  count(epi_ultimas_semanas_viajo)

# _HISTORIA DE MALARIA -----------------------------------------------------

#ind_viv %>% 
#  select(id,tenido_malaria,cuantas_tenido_malaria,recibio_tratamiento_malaria,
#         epi_alguien_tuvo_malaria,epi_malaria_ultimos_meses,
#         epi_malaria_ultimos_meses_c,epi_meses_ultima_malaria) %>% 
#  summary()
#
#viv %>% 
#  select(#id,#tenido_malaria,cuantas_tenido_malaria,recibio_tratamiento_malaria,
#    vivienda,epi_alguien_tuvo_malaria,epi_malaria_ultimos_meses,
#    epi_malaria_ultimos_meses_c,epi_meses_ultima_malaria) %>% 
#  summary()

history_issue <- bas %>% 
  #ind_viv %>% 
  select(id,vivienda,tenido_malaria,cuantas_tenido_malaria,epi_meses_ultima_malaria) %>%
  count(vivienda,epi_meses_ultima_malaria) %>% 
  arrange(desc(n)) %>% 
  count(vivienda) %>% 
  arrange(desc(n)) %>% 
  filter(n>1)
#distinct(vivienda,.keep_all = T)

bas %>% 
  filter(vivienda %in% history_issue$vivienda) %>% 
  select(vivienda,age_7,epi_meses_ultima_malaria) %>% 
  arrange(vivienda,desc(age_7)) %>% 
  print(n=Inf)

history_vivienda <- bas %>% 
  select(vivienda,tenido_malaria) %>% 
  mutate(tenido_01=as.numeric(tenido_malaria)-1) %>% #count(tenido_01)
  group_by(vivienda) %>% 
  summarise(tenido_01_vivienda=sum(tenido_01,na.rm = T),
            total_ind=n()) %>% 
  ungroup() %>% 
  mutate(tenido_malaria_vivienda=if_else(tenido_01_vivienda>0,"si","no"),
         tenido_malaria_vivienda=fct_relevel(tenido_malaria_vivienda,"no")) 

history_vivienda %>% filter(tenido_01_vivienda==0)

bas %>% 
  select(vivienda,tenido_malaria) %>% 
  filter(vivienda=="NN004")

## __problema

bas %>% 
  select(vivienda,id,epi_malaria_ultimos_meses_c,epi_meses_ultima_malaria) %>% 
  count(epi_malaria_ultimos_meses_c,epi_meses_ultima_malaria) %>% print(n=Inf)

## __solución ----
history_sol <- bas %>% 
  #filter(vivienda %in% history_issue$vivienda) %>% 
  select(vivienda,age_7,epi_meses_ultima_malaria) %>% 
  #left_join(history_vivienda) %>% #filter(vivienda=="NN004") #personas no registradas con malaria
  
  group_by(vivienda) %>% #objetivo: capturar la observación con el mayor reporte de MESES a último episodio de MALARIA
  arrange(desc(epi_meses_ultima_malaria)) %>% 
  slice(1) %>%
  ungroup() %>% #count(epi_meses_ultima_malaria)
  
  mutate(new_alguien_tuvo_malaria=if_else(epi_meses_ultima_malaria<6 & epi_meses_ultima_malaria!=0 & !is.na(epi_meses_ultima_malaria),
                                          "si, hace menos de 6 meses",
                                          if_else(epi_meses_ultima_malaria<=12 & epi_meses_ultima_malaria!=0 & !is.na(epi_meses_ultima_malaria),
                                                  "si, el ultimo ano",
                                                  if_else((epi_meses_ultima_malaria>12  | epi_meses_ultima_malaria!=0 | is.na(epi_meses_ultima_malaria)),
                                                          "si, hace mas de un ano",
                                                          #"nunca o hace mas de un ano",
                                                          "nunca"
                                                          #"na"
                                                  ))),
         new_alguien_tuvo_malaria=fct_relevel(new_alguien_tuvo_malaria,
                                              "nunca",
                                              "si, hace mas de un ano",
                                              #"nunca o hace mas de un ano",
                                              #"si, hace algunos anos",
                                              "si, el ultimo ano"),
         new_malaria_ultimos_meses_c=if_else(epi_meses_ultima_malaria<=12 & epi_meses_ultima_malaria!=0 & !is.na(epi_meses_ultima_malaria),
                                             "si, hace 12 meses o menos",
                                             "No/No sabe"),
         new_alguien_tuvo_malaria=fct_collapse(new_alguien_tuvo_malaria,"si, hace 6 meses o más"=c("si, hace mas de un ano","si, el ultimo ano"))
  ) %>% 
  select(-age_7,-epi_meses_ultima_malaria)

history_sol %>% count(new_alguien_tuvo_malaria)
#viv %>% count(epi_alguien_tuvo_malaria)
history_sol %>% count(new_malaria_ultimos_meses_c)
#viv %>% count(epi_malaria_ultimos_meses_c)

#viv %>% 
#  left_join(history_sol,by = "vivienda") %>% 
#  count(epi_alguien_tuvo_malaria,new_meses_ultima_malaria)

bas <- bas %>% 
  left_join(history_sol) #%>% glimpse()

# _BASE DE SUJETOS ---------------------------------------------------------

#sujetos por vivienda
bas %>% count(vivienda) %>% arrange(desc(n))

ind <- bas %>% 
  select(#vivienda,#-id,-age_7,
         #-vivienda,-id,-age_7,
         -referencia,-direccion,
         -latitud,-longitud,
         -grupos_personas,
         -electricidad_red_publica,-combustible_cocinar,
         -ah_auto,-ah_estereo,-ah_television,-ah_radio,-ah_refrigerador,-ah_motocicleta,-ah_mototaxi,
         -pertenencia_casa,-tipo_casa,-cantidad_habitaciones,-cantidad_habitaciones_dormir,
         -material_pared,-material_piso,
         -material_pared_c,-material_piso_c,
         -fuente_agua,-banio_conexion,
         -epi_cerca_fuente_agua,-epi_tipo_fuente_agua,-epi_distancia_fuente_agua,
         -epi_malaria_ultimos_meses,-epi_malaria_ultimos_meses_c,
         -epi_viajo_fuera_villa,-epi_meses_ultima_malaria,-epi_especie_causo_malaria,
         #consevada a nivel sujeto
         -epi_frecuencia_rocia_casa,-epi_frecuencia_rocia_casa_c,-epi_alguien_tuvo_malaria,
         #preferida sobre rociamiento
         -epi_rocia_con_insecticida_c
         ) %>% 
  
  #correccion del nombre de comunidad
  mutate(community_1=forcats::fct_recode(community_1,"Ninarumi"="Ninarrumi")) %>%
  
  #prevalencia y seroprevalencia por especie
  mutate(micr_fal=if_else(micro_c=="fal" | micro_c=="mix", "positive","negative"),
         micr_viv=if_else(micro_c=="viv" | micro_c=="mix", "positive","negative"),
         micr_mix=if_else(micro_c=="mix", "positive","negative"),
         prev_fal=if_else(pcr_c=="fal" | pcr_c=="mix", "positive","negative"),
         prev_viv=if_else(pcr_c=="viv" | pcr_c=="mix", "positive","negative"),
         prev_mix=if_else(pcr_c=="mix", "positive","negative"),
         sero_fal=if_else(sero_c=="fal" | sero_c=="mix", "positive","negative"),
         sero_viv=if_else(sero_c=="viv" | sero_c=="mix", "positive","negative"),
         sero_mix=if_else(sero_c=="mix", "positive","negative")
         ) %>%
  mutate(#epi_redes_usadas_actualmente=stringr::str_to_lower(epi_redes_usadas_actualmente),
         residence_fct=if_else(residence<3,"<=2",">2"))

#ind %>% count(sero_fal,Ab.unit_Pfal_4) #no olvidar la distribucion de la variable!!, recontra sesgada y 3/4 cuartiles son negativos!!
#ind %>% count(sero_viv,Ab.unit_Pviv_4)

# _BASE DE VIVIENDAS -------------------------------------------------------

viv <- bas %>% 
  select(community_1,
         vivienda,id,age_7,#referencia,direccion,
         latitud,longitud,
         grupos_personas,
         electricidad_red_publica,combustible_cocinar,
         ah_auto,ah_estereo,ah_television,ah_radio,ah_refrigerador,ah_motocicleta,ah_mototaxi,
         pertenencia_casa,tipo_casa,cantidad_habitaciones,cantidad_habitaciones_dormir,
         material_pared,material_piso,
         material_pared_c,material_piso_c,
         fuente_agua,banio_conexion,
         epi_cerca_fuente_agua,epi_tipo_fuente_agua,epi_distancia_fuente_agua,
         epi_malaria_ultimos_meses,epi_malaria_ultimos_meses_c,
         epi_viajo_fuera_villa,epi_meses_ultima_malaria,epi_especie_causo_malaria,
         #consevada a nivel sujeto
         epi_frecuencia_rocia_casa,epi_frecuencia_rocia_casa_c,epi_alguien_tuvo_malaria,
         #preferida sobre rociamiento
         epi_rocia_con_insecticida_c,new_alguien_tuvo_malaria,new_malaria_ultimos_meses_c
         ) %>% 
  #por cada vivienda
  group_by(vivienda) %>% 
  #quedarnos con la respuesta de la persona con mayor edad
  filter(age_7==max(age_7)) %>% ungroup() %>% 
  #retirar a más de dos sujetos con la misma edad
  distinct(vivienda,.keep_all = T) %>% 
  select(-id,-age_7) %>% 
  mutate(grupos_personas=as.factor(grupos_personas))

# __VIVIENDA outcome --------------------------------------------

#no son comparables: total de sujetos por vivienda 2015 vs 2018
#cen_viv <- read_rds("data/vivienda_zungarococha_2018.rds") %>% 
#  select(hh,n_indiv)

viv_y <- ind %>% select(vivienda,id,micr_viv,prev_viv,prev_fal,sero_viv,sero_fal) %>% 
  gather(key,value,-vivienda,-id) %>% select(vivienda,key,id,value) %>% arrange(vivienda,key,id) %>% 
  mutate(value=if_else(value=="positive",1,0)) %>% #dplyr::count(value)
  group_by(vivienda,key) %>% 
  summarise(hoth= sum(value)>0,
            nume=sum(value),
            deno=n(),
            prop=(sum(value)/n()),
            prct=(sum(value)/n())*100
  ) %>% 
  ungroup() %>% 
  gather(measure,number,-vivienda,-key) %>% arrange(vivienda,key,measure) %>% filter(measure!="deno") %>% 
  mutate(key_measure=str_c(key,"_",measure)) %>% select(-key,-measure) %>% 
  spread(key_measure,number) %>% #%>% print(n=20)
  mutate(
    micr_viv_hoth = as.factor(micr_viv_hoth),
    prev_viv_hoth = as.factor(prev_viv_hoth),
    prev_fal_hoth = as.factor(prev_fal_hoth),
    sero_fal_hoth = as.factor(sero_fal_hoth),
    sero_viv_hoth = as.factor(sero_viv_hoth)
  )

viv_n <- ind %>% select(vivienda) %>% group_by(vivienda) %>% summarise(n_indiv=n())

viv <- viv %>% left_join(viv_n) %>% left_join(viv_y)


# VARIABLES -----------------------------------------------------

ind %>% glimpse()
viv %>% glimpse()
viv %>% count(vivienda) %>% arrange(desc(n))

#viv %>% filter(vivienda=="LL015") %>% glimpse()
#viv %>% filter(vivienda=="ZG074") %>% glimpse()
#viv %>% filter(vivienda=="ZG175") %>% glimpse()

# CONTRASTAR con NACIMIENTO censo 2018 -> EDAD ----------------------------

cen <- read_rds("data/individuo_zungarococha_2018.rds") %>% 
  select(id,edad_z0,sexo)

ind %>% select(id,sex_8,age_7,residence) %>% 
  mutate(
    id=if_else(str_count(id)<8,str_replace(id,"(......)(.)","\\10\\2"),id),
    id=str_replace(id,"-","_")
  ) %>% 
  left_join(cen) %>% 
  mutate(differ=edad_z0-age_7) %>% 
  filter(differ!=0 & differ!=1 & differ!=-1 & differ!=2 & differ!=-2) %>% print(n=Inf)

#conclusion:
# 47 sujetos difieren en EDAD con más de +/-2 años o SEXO --> qué hacer?

# **CORREGIR DUPLICAS CON MISMA EDAD ----------------------------------------


# MERGE ENTRE BASE SUJETO + BASE VIVIENDA ---------------------------------

ind_viv <- ind %>% select(-community_1) %>% 
  left_join(viv) %>% 
  
  #asignar etiquetas a variables
  labelled::set_variable_labels(
    age_quart = "Age quartiles (years)", 
    sex_8 = "Sex",
    nivel_educacion = "Education")


# BASES FINALES -----------------------------------------------------------

#viv %>% visdat::vis_dat()
#ind %>% visdat::vis_dat()

viv %>% glimpse()
ind %>% glimpse()
ind_viv %>% glimpse()


# GRABAR DTA --------------------------------------------------------------

library(haven)

#!grepl("^[A-Za-z_]{1}[A-Za-z0-9_]{0,31}$", names(ind %>% rename_all(funs(str_replace_all(.,"_","")))))

ind %>% 
  rename_all(funs(str_replace_all(.,"_",""))) %>% 
  rename_all(funs(str_replace_all(.,"\\.",""))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  rename("epicantidadredesusadasactual"="epicantidadredesusadasactualmente") %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  write_dta("data/z0_ind.dta")
viv %>% 
  rename_all(funs(str_replace_all(.,"_",""))) %>% 
  rename_all(funs(str_replace_all(.,"\\.",""))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  #rename("epicantidadredesusadasactual"="epicantidadredesusadasactualmente") %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  write_dta("data/z0_viv.dta")
ind_viv %>% 
  rename_all(funs(str_replace_all(.,"_",""))) %>% 
  rename_all(funs(str_replace_all(.,"\\.",""))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  rename("epicantidadredesusadasactual"="epicantidadredesusadasactualmente") %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  write_dta("data/z0_ind_viv.dta")

viv %>% write_rds("data/z0_viv.rds")
ind_viv %>% write_rds("data/z0_ind_viv.rds")

# DESCRIPTIVOS ------------------------------------------------------------

library(compareGroups)

# _vivienda ---------------------------------------------------------------

compareGroups(community_1 ~ ., 
              data = viv #,byrow=T 
              ,method = c(cantidad_habitaciones = 2, 
                          cantidad_habitaciones_dormir=2, 
                          epi_meses_ultima_malaria=2,
                          micr_viv_nume = 2,
                          micr_viv_prct = 2,
                          micr_viv_prop = 2,
                          prev_viv_nume = 2,
                          prev_viv_prct = 2,
                          prev_viv_prop = 2,
                          sero_fal_nume = 2,
                          sero_fal_prct = 2,
                          sero_fal_prop = 2,
                          sero_viv_nume = 2,
                          sero_viv_prct = 2,
                          sero_viv_prop = 2
                          )
              ) %>% 
  createTable(show.all = T, show.n = T) %>% 
  export2xls("table/z0-tab1_viv_r0.xls")

# __tabla 1 ----------------------------------------------------------------


#viv %>% select(material_pared,material_piso,epi_cerca_fuente_agua,epi_malaria_ultimos_meses,epi_frecuencia_rocia_casa_c,epi_alguien_tuvo_malaria,
#               micr_viv_hoth,prev_viv_hoth,sero_viv_hoth,sero_fal_hoth,sero_viv_prct,sero_fal_prct)
  
compareGroups(community_1 ~ 
                material_pared+material_piso+
                epi_cerca_fuente_agua+
                epi_frecuencia_rocia_casa_c+epi_rocia_con_insecticida_c+
                epi_malaria_ultimos_meses+
                epi_alguien_tuvo_malaria+
                micr_viv_hoth+prev_viv_hoth+sero_viv_hoth+sero_fal_hoth+sero_viv_prct+sero_fal_prct, 
              data = viv #,byrow=T 
              ,method = c(micr_viv_prct = 2,
                          prev_viv_prct = 2,
                          sero_fal_prct = 2,
                          sero_viv_prct = 2
                          )
              ) %>% 
  createTable(show.all = T, show.n = T) %>% 
  export2xls("table/z0-tab1_viv_r1.xls")


# __tabla2 ----------------------------------------------------------------

compareGroups(micr_viv_hoth ~ 
                material_pared_c+material_piso_c+
                epi_cerca_fuente_agua+#epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_frecuencia_rocia_casa_c+epi_rocia_con_insecticida_c+
                epi_malaria_ultimos_meses_c+#epi_viajo_fuera_villa+epi_meses_ultima_malaria+epi_especie_causo_malaria+
                epi_alguien_tuvo_malaria, 
              data = viv ,byrow=T 
              ) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_micr_viv_hoth.xls")

compareGroups(prev_viv_hoth ~ 
                electricidad_red_publica+combustible_cocinar+
                ah_estereo+ah_television+ah_radio+ah_refrigerador+ah_motocicleta+ah_mototaxi+
                fuente_agua+banio_conexion+
                material_pared_c+material_piso_c+
                epi_cerca_fuente_agua+epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_frecuencia_rocia_casa_c+epi_rocia_con_insecticida_c+
                epi_malaria_ultimos_meses_c+epi_viajo_fuera_villa+epi_meses_ultima_malaria+epi_especie_causo_malaria+
                epi_alguien_tuvo_malaria+new_alguien_tuvo_malaria+new_malaria_ultimos_meses_c+
                sero_viv_hoth, 
              data = viv ,byrow=T 
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_prev_viv_hoth.xls")

compareGroups(sero_viv_hoth ~ 
                electricidad_red_publica+combustible_cocinar+
                ah_estereo+ah_television+ah_radio+ah_refrigerador+ah_motocicleta+ah_mototaxi+
                fuente_agua+banio_conexion+
                material_pared_c+material_piso_c+
                epi_cerca_fuente_agua+epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_frecuencia_rocia_casa_c+epi_rocia_con_insecticida_c+
                epi_malaria_ultimos_meses_c+epi_viajo_fuera_villa+epi_meses_ultima_malaria+epi_especie_causo_malaria+
                epi_alguien_tuvo_malaria+new_alguien_tuvo_malaria+new_malaria_ultimos_meses_c+
                prev_viv_hoth, 
              data = viv ,byrow=T 
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_sero_viv_hoth.xls")

compareGroups(sero_fal_hoth ~ 
                electricidad_red_publica+combustible_cocinar+
                ah_estereo+ah_television+ah_radio+ah_refrigerador+ah_motocicleta+ah_mototaxi+
                fuente_agua+banio_conexion+
                material_pared_c+material_piso_c+
                epi_cerca_fuente_agua+epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_frecuencia_rocia_casa_c+epi_rocia_con_insecticida_c+
                epi_malaria_ultimos_meses_c+epi_viajo_fuera_villa+epi_meses_ultima_malaria+epi_especie_causo_malaria+
                epi_alguien_tuvo_malaria+new_alguien_tuvo_malaria+new_malaria_ultimos_meses_c+
                prev_fal_hoth
              , 
              data = viv ,byrow=T 
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_sero_fal_hoth.xls")

# _individuo --------------------------------------------------------------

#ind %>% select(age_7,age_quart,age_oms,sex_8,trabajo_tdy_s_15,nivel_educacion_c_18,actualmente_estudiando_18,estado_civil_15,residence,residence_fct,relacion_jefe_familia_c)
#ind %>% select(tenido_malaria,recibio_tratamiento_malaria,busco_ayuda_tratamiento,donde_busco_tratamiento,enfermedad_cronica,enf_tomo_medicinas_parasitos)
#ind %>% select(epi_uso_repelente_mosquito_c,epi_uso_mangas_largas_c,epi_uso_ropa_impregnada_c,epi_cercania_fuente_agua_c,epi_estuvo_campo_antes_c,epi_uso_redes_cama_c,epi_duerme_ventanas_abiertas_c,epi_duerme_cerca_monte_c,epi_rocia_con_insecticida_c,epi_frecuencia_rocia_casa_c,epi_ultimas_semanas_viajo,epi_frecuencia_rocia_casa_c,epi_estado_campos_agricultura_c,epi_estado_canal_agua_c,epi_uso_red_dormir,epi_redes_usadas_actualmente)
#ind %>% select(micro_c,pcr_c,sero_c,Ab.unit_Pviv,Ab.unit_Pviv_4,Ab.unit_Pfal,Ab.unit_Pfal_4)

compareGroups(community_1 ~ age_7+age_quart+age_oms+age_gcb+sex_8+
                residence+residence_fct+residence_quart+
                trabajo_tdy_s_15+
                nivel_educacion_18+nivel_educacion_c_18+actualmente_estudiando_18+estado_civil_15+
                residence+residence_fct+relacion_jefe_familia_c, 
              data = ind #,byrow=T 
              ,method = c(residence = 2,age_7 = 2)
) %>% 
  createTable(show.all = T, show.n = T) %>% 
  export2xls("table/z0-tab1_ind_r01.xls")

compareGroups(community_1 ~ tenido_malaria+recibio_tratamiento_malaria+busco_ayuda_tratamiento+donde_busco_tratamiento_c+
                enfermedad_cronica+enf_tomo_medicinas_parasitos, 
              data = ind #,byrow=T 
              #,method = c(residence = 2)
) %>% 
  createTable(show.all = T, show.n = T) %>% 
  export2xls("table/z0-tab1_ind_r02.xls")

compareGroups(community_1 ~ 
                epi_uso_repelente_mosquito_c+
                epi_uso_mangas_largas_c+
                epi_uso_ropa_impregnada_c+
                epi_cercania_fuente_agua_c+
                epi_estuvo_campo_antes_c+
                epi_uso_redes_cama_c+
                epi_duerme_ventanas_abiertas_c+
                epi_duerme_cerca_monte_c+
                #epi_rocia_con_insecticida_c+
                
                epi_ultimas_semanas_viajo+
                #epi_frecuencia_rocia_casa_c+epi_alguien_tuvo_malaria+
                
                epi_estado_campos_agricultura_c+
                epi_estado_canal_agua_c+
                
                epi_uso_red_dormir_c+
                epi_redes_usadas_actualmente, 
              data = ind #,byrow=T 
              #,method = c(residence = 2)
) %>% 
  createTable(show.all = T, show.n = T) %>% 
  export2xls("table/z0-tab1_ind_r03.xls")

compareGroups(community_1 ~ micro_c+pcr_c+sero_c+Ab.unit_Pviv+Ab.unit_Pviv_4+Ab.unit_Pfal+Ab.unit_Pfal_4, 
              data = ind #,byrow=T 
              ,method = c(Ab.unit_Pviv = 2,Ab.unit_Pfal=2)
) %>% 
  createTable(show.all = T, show.n = T) %>% 
  export2xls("table/z0-tab1_ind_r04.xls")

# __tabla 1 ---------------------------------------------------------------
#sosbre residencia:
#-para mantener comparabilidad, usamos el punto de corte de 2 años
#-sin embargo, la distribucion no es lineal con respecto al desenlace, 
#- por lo tanto, en el modelo 3 decidimos por a categorizarla con el fin de capturar dicha no-linealidad

compareGroups(community_1 ~ 
                age_quart+sex_8+
                residence_fct+residence_quart+#residence+#
                trabajo_rpl+actualmente_estudiando+
                tenido_malaria+
                
                epi_uso_repelente_mosquito_c+
                epi_uso_mangas_largas_c+
                epi_uso_ropa_impregnada_c+
                epi_cercania_fuente_agua_c+
                epi_estuvo_campo_antes_c+
                epi_uso_redes_cama_c+
                epi_duerme_ventanas_abiertas_c+
                epi_duerme_cerca_monte_c+
                #epi_rocia_con_insecticida_c+
                
                epi_estado_campos_agricultura_c+
                epi_estado_canal_agua_c+
                
                epi_uso_red_dormir_c +
                
                micro_c+pcr_c+sero_c+
                Ab.unit_Pviv+Ab.unit_Pviv_4+
                Ab.unit_Pfal+Ab.unit_Pfal_4, 
              data = ind #,byrow=T 
              ,method = c(Ab.unit_Pviv = 2,Ab.unit_Pfal=2)
) %>% 
  createTable(show.all = T, show.n = T) %>% 
  export2xls("table/z0-tab1_ind_r05.xls")

# __tabla 2 ---------------------------------------------------------------

compareGroups(prev_viv ~ 
                
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
                enf_tomo_medicinas_parasitos+
                
                epi_cerca_fuente_agua+epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_malaria_ultimos_meses_c+epi_viajo_fuera_villa+epi_meses_ultima_malaria+#epi_especie_causo_malaria+
                
                
                #epi_alguien_tuvo_malaria+
                new_alguien_tuvo_malaria+
                new_malaria_ultimos_meses_c+
                
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
                sero_viv+
                prev_viv
              
              ,data = ind_viv ,byrow=T 
              #,method = c(Ab.unit_Pviv = 2,Ab.unit_Pfal=2)
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_prev_viv.xls")


compareGroups(sero_viv ~ 
                community_1 + age_7 + new_alguien_tuvo_malaria +
                
                age_quart+sex_8+
                residence_fct+residence_quart+#residence+#
                trabajo_rpl+actualmente_estudiando+nivel_educacion+
                tenido_malaria+epi_ultimas_semanas_viajo+
                
                epi_uso_repelente_mosquito_c+
                epi_uso_mangas_largas_c+
                epi_uso_ropa_impregnada_c+
                epi_cercania_fuente_agua_c+
                epi_estuvo_campo_antes_c+
                epi_uso_redes_cama_c+
                epi_duerme_ventanas_abiertas_c+
                epi_duerme_cerca_monte_c+
                #epi_rocia_con_insecticida_c+
                
                epi_estado_campos_agricultura_c+
                epi_estado_canal_agua_c+
                
                epi_uso_red_dormir_c+prev_viv
              
              ,data = ind ,byrow=T 
              #,method = c(Ab.unit_Pviv = 2,Ab.unit_Pfal=2)
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_sero_viv.xls")

compareGroups(sero_fal ~ 
                community_1 + 
                
                age_quart+sex_8+
                residence_fct+residence_quart+#residence+
                trabajo_rpl+actualmente_estudiando+nivel_educacion+
                tenido_malaria+epi_ultimas_semanas_viajo+
                
                epi_uso_repelente_mosquito_c+
                epi_uso_mangas_largas_c+
                epi_uso_ropa_impregnada_c+
                epi_cercania_fuente_agua_c+
                epi_estuvo_campo_antes_c+
                epi_uso_redes_cama_c+
                epi_duerme_ventanas_abiertas_c+
                epi_duerme_cerca_monte_c+
                #epi_rocia_con_insecticida_c+
                
                epi_estado_campos_agricultura_c+
                epi_estado_canal_agua_c+
                
                epi_uso_red_dormir_c+prev_fal
              
              ,data = ind ,byrow=T 
              #,method = c(Ab.unit_Pviv = 2,Ab.unit_Pfal=2)
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_sero_fal.xls")


# _tabla 3 a DTA ---------------------------------------------------------

ind %>% 
  select(
    id,
    community_1 , age_7,
    
    age_quart,sex_8,
    residence_fct,residence_quart,
    trabajo_rpl,actualmente_estudiando,nivel_educacion,
    tenido_malaria,epi_ultimas_semanas_viajo,
    
    epi_uso_repelente_mosquito_c,
    epi_uso_mangas_largas_c,
    epi_uso_ropa_impregnada_c,
    epi_cercania_fuente_agua_c,
    epi_estuvo_campo_antes_c,
    epi_uso_redes_cama_c,
    epi_duerme_ventanas_abiertas_c,
    epi_duerme_cerca_monte_c,
    #epi_rocia_con_insecticida_c,
    
    epi_estado_campos_agricultura_c,
    epi_estado_canal_agua_c,
    
    epi_uso_red_dormir_c,
    
    prev_viv,
    sero_viv,Ab.unit_Pviv,Ab.unit_Pviv_4,
    sero_fal,Ab.unit_Pfal,Ab.unit_Pfal_4
  ) %>% 
  #rename_all(funs(str_replace_all(.,"_",""))) %>% 
  rename_all(funs(str_replace_all(.,"\\.",""))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  #rename("epicantidadredesusadasactual"="epicantidadredesusadasactualmente") %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  write_dta("data/z0_ind_t3.dta")
viv %>% 
  select(
    vivienda,community_1,
    
    electricidad_red_publica,combustible_cocinar,
    ah_estereo,ah_television,ah_radio,ah_refrigerador,ah_motocicleta,ah_mototaxi,
    fuente_agua,banio_conexion,
    new_alguien_tuvo_malaria,new_malaria_ultimos_meses_c,
    
    material_pared,material_piso,
    epi_cerca_fuente_agua,#epi_tipo_fuente_agua,epi_distancia_fuente_agua,
    epi_frecuencia_rocia_casa_c,epi_rocia_con_insecticida_c,
    epi_malaria_ultimos_meses,#epi_viajo_fuera_villa,epi_meses_ultima_malaria,epi_especie_causo_malaria,
    epi_alguien_tuvo_malaria,
    
    longitud,latitud,
    
    micr_viv_hoth,
    prev_viv_hoth,prev_fal_hoth,
    sero_viv_hoth,sero_fal_hoth,
    sero_viv_prct,sero_fal_prct
  ) %>% 
  #rename_all(funs(str_replace_all(.,"_",""))) %>% 
  rename_all(funs(str_replace_all(.,"\\.",""))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  #rename("epicantidadredesusadasactual"="epicantidadredesusadasactualmente") %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  write_dta("data/z0_viv_t3.dta")
ind_viv %>% 
  select(
    id,
    
    vivienda,
    longitud,latitud,
    
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
    enf_tomo_medicinas_parasitos,
    
    epi_cerca_fuente_agua,epi_tipo_fuente_agua,epi_distancia_fuente_agua,
    epi_malaria_ultimos_meses_c,
    new_malaria_ultimos_meses_c,
    epi_viajo_fuera_villa,epi_meses_ultima_malaria,#epi_especie_causo_malaria,
    
    
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
    #prev_viv
    
    micr_viv,prev_viv,
    sero_viv,Ab.unit_Pviv,Ab.unit_Pviv_4,
    sero_fal,Ab.unit_Pfal,Ab.unit_Pfal_4,
    
    micr_fal,prev_fal,
    micr_mix,prev_mix,sero_mix
    
  ) %>% 
  #rename_all(funs(str_replace_all(.,"_",""))) %>% 
  rename_all(funs(str_replace_all(.,"\\.",""))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  #rename("epicantidadredesusadasactual"="epicantidadredesusadasactualmente") %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  write_dta("data/z0_ind_viv_t3.dta")

# LINEALIDAD pre-modelo --------------------------------------------------------------

ind %>% count(age_gcb)
ind %>% count(age_quart)

#linealidad edad-outcome
ind %>% 
  ggplot(aes(age_ten)) + 
  geom_bar(aes(fill=prev_viv),position = position_fill()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ind %>% 
  ggplot(aes(age_ten)) + 
  geom_bar(aes(fill=sero_viv),position = position_fill()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ind %>% 
  ggplot(aes(age_ten)) + 
  geom_bar(aes(fill=sero_fal),position = position_fill()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#linealidad residencia-outcome
#ind %>% ggplot(aes(residence_ten)) + geom_bar(aes(fill=prev_viv),position = position_fill()) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ind %>% 
  ggplot(aes(residence_ten)) + 
  geom_bar(aes(fill=sero_viv),position = position_fill()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ind %>% 
  ggplot(aes(residence_ten)) + 
  geom_bar(aes(fill=sero_fal),position = position_fill()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# MAPA CASO-CONTROL -------------------------------------------------------

#ver: maps-per_iq_zung.R

# REVERSE CATALITYC MODEL -------------------------------------------------

pv <- haven::read_dta("data/z0_revcat_zg_pv.dta") %>% 
  select(age=age_7,p, u_p, l_p) %>% 
  mutate(specie="P. vivax",
         community_1="Whole community")

pf <- haven::read_dta("data/z0_revcat_zg_pf.dta") %>% 
  select(age=age_7,p, u_p, l_p) %>% 
  mutate(specie="P. falciparum",
         community_1="Whole community")

rcm <- union_all(pv,pf)

read_rds("data/z0_revcat_zg.rds") %>% 
  rename(age=age_ten) %>% 
  mutate(community_1="Whole community") %>% 
  dplyr::select(community_1,
                age,"P. vivax"=sero.viv.p,"P. falciparum"=sero.fal.p) %>%
  gather(specie,sero.p,-community_1,
         -age#-age_7
  ) %>% 
  ggplot(aes(age,sero.p*100,colour=specie)) +
  geom_point() +
  
  coord_fixed(ratio = 1) +
  
  geom_ribbon(data=rcm, aes(x=age, y=p*100, ymin=l_p*100, ymax=u_p*100, fill=specie), alpha=0.2, linetype=0) +
  geom_line(data=rcm, aes(x=age, y=p*100, colour=specie)) +
  
  #facet_grid(specie~community_1) + 
  ylim(c(0,50)) +
  scale_x_continuous(breaks = seq(0,80,by = 10)) +
  
  ylab("% seropositive") + xlab("Age (years)")
  
ggsave("figure/rcm_whole.png",width = 6,height = 4)

read_rds("data/z0_revcat.rds") %>% 
  dplyr::select(community_1,age_ten,#age_7,#tot.sero.viv,tot.sero.fal,
                5,8) %>% 
  gather(Specie,sero.p,-community_1,-age_ten#-age_7
  ) %>% 
  ggplot(aes(age_ten,#age_7,
             sero.p)) +
  geom_point() +
  facet_grid(Specie~community_1) +
  ylim(c(0,1))
ggsave("figure/rcm_commu.png",width = 12,height = 4)
