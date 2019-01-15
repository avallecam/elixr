library(tidyverse)
library(lubridate)
library(haven)
library(gridExtra)
library(zoo)
library(naniar)

# importar CSV ------

#censo <- readr::read_csv("data-raw/surveym/censo_final-180920/CSV/Censo-Zungarococha.csv")
censo <- readr::read_csv("data-raw/surveym/censo_final-181018/CSV/Censo-Zungarococha.csv")
#censo %>% glimpse()

# limpiar base ------

x <- censo %>% 
  slice(1) %>% 
  mutate_all(funs(replace_na(.,""))) %>% 
  #first, make clear names
  rename_all(funs(stringr::str_to_lower(.))) %>% 
  rename_all(funs(make.names(.))) %>%
  mutate_all(funs(make.names(.))) %>% 
  mutate_all(funs(stringr::str_to_lower(.)))

a <- colnames(x) %>% as_data_frame() %>% 
  mutate(value=if_else(value=="x20","fuente.principal.de.agua.para.tomar.o.beber.otros",value),
         value=if_else(str_detect(value,"^x"),NA_character_,value),
         value=if_else(str_detect(value,"\\.\\."),str_replace(value,"id(\\.\\.)(.+)","id\\2__"),value)
         ) %>% 
  do(na.locf(.)) %>% #print(n=Inf)
  .$value #print(n=Inf) #%>% length()

b <- x[1,] %>% 
  as.character() %>% # %>% length()
  as_data_frame() %>% 
  mutate(value=if_else(str_detect(value,"^x|response"),NA_character_,value),
         value=if_else(is.na(value),"",value)) %>% .$value #%>% print(n=Inf)

colnames(censo) <- str_c(a,b)

#hecho: un solo nombre de columna

censo_df <- censo %>% 
  
  #start cleaning
  #retirar por tener los falsos headers
  slice(-1) %>% 
  #retirar por ser los dos primeros intentos fallidos
  slice(-(n()-1):-n()) %>% #%>% tail() #%>% 
  
  #crear idvivienda
  mutate(hh=str_c(str_replace(poblado,"(..)(.+)","\\1"),vivienda),
         
         #poblado y #individuos
         poblado=stringr::str_replace(poblado,"(..: )(.+)","\\2"),
         n_indiv=as.numeric(número.de.individuos.censados),
         
         #formato fecha
         date_created=lubridate::mdy_hms(date_created),
         date_modified=lubridate::mdy_hms(date_modified),
         
         #duracion de encuesta
         date=str_c("0",month(date_modified),"/",day(date_modified)) %>% as.factor()
         ,
         difftime=date_modified-date_created,
         difftime.dur=as.duration(date_modified-date_created)
         ) %>% 
  
  #rename(latitud=gpslat,longitud=gpslon) %>% 
  
  #retirar columnas no informativas
  select(-número.de.individuos.censados,-vivienda) #%>% 
  
  #filtrar replicas posteriores [NO TODOS]
  #distinct(poblado,vivienda,.keep_all = TRUE) %>% 
  
  #solo filtrar is.na(n_indiv) (n=4) ##verificar si tienen ingreso como continuación de hojas (perser viviendas con más de 20 sujetos!)
  #filter(!is.na(n_indiv))

#nan <- censo_df %>% filter(is.na(n_indiv))
#aporte: permite verificar que is.na(n_indiv) tiene contrapartes correctas
#censo_df %>% 
#  filter(hh %in% nan$hh) %>% 
#  select(hh,n_indiv,date_created,difftime) %>% 
#  arrange(hh,desc(date_created)) %>% 
#  print(n=Inf)

censo_df %>% glimpse()

## __termino real  ------

#before end of scripting
r <- censo_df %>% #select(date_created) %>% print(n=100)
  filter(date_created < ymd("2018-09-26"))# %>% select(hh,date_created)

#after end of scripting
t <- censo_df %>% #select(date_created) %>% print(n=100)
  filter(date_created > ymd("2018-09-26"))# %>% select(hh,date_created)

r #396
inner_join(r,t,"hh") %>% select(1,3) %>% unique() #32
anti_join(r,t,"hh") #%>% print(n=Inf) #364

t #67
inner_join(t,r,"hh") %>% select(1,3) %>% unique() #25
anti_join(t,r,"hh") #%>% print(n=Inf) #42

censo_df <- union_all(r,anti_join(t,r,"hh")) #438
backup <- t %>% filter(respondent_id %in% (inner_join(t,r,"hh") %>% select(1,3) %>% unique() %>% .$respondent_id.x))

# diccionario codigos ------------

# mayuscula
# en blanco: -1 ----> na_if("-1")
# pre-impresa: -5 --> filter(value!=-5)
# erronea: -9 ------> ver observaciones

# (1) base viviendas -------------

## __preimpresa -----
censo_pr_hh <- read_csv("data/vivienda_zungarococha.csv") %>% 
  mutate(latitud=as.numeric(latitud),
         longitud=as.numeric(longitud),
         hh=str_replace(hh,"(..)(\\.\\.\\..+)",NA_character_),
         direccion=str_replace(direccion,"\\.\\.\\..+",NA_character_),
         referencia=str_replace(referencia,"\\.\\.\\..+",NA_character_)#,
           )
censo_pr_hh %>% Hmisc::contents()
#censo_pr_hh %>% #filter(is.na(direccion)) %>% count(referencia)

censo_pr_hh %>% 
  select(hh,n)

## __censo 2018 -----
censo_hh <- censo_df %>% #glimpse()
  select(respondent_id,hh,poblado:observaciones.de.vivienda,date_created,n_indiv:difftime.dur,-date) %>% 
  
  #durante limpieza
  #retirar 10 ingresos con NA que sí tienen réplica
  filter(!is.na(n_indiv)) %>%  
  #retirar 02 con NA en fuente de agua y con replica
  filter(!respondent_id %in% c(10042616011,10042174020)) %>% 
  #retirar 03 con NA en observ vivienda
  filter(!respondent_id %in% c(10213032566,10208518830,10208483418)) %>% 
  #retirar ingresos con replica en var de opcion fija
  filter(!respondent_id %in% c(10196234345,10278853356,10217841471,10221652948,10221823262)) %>% 
  #retirar según archivo ddig_hh_diff_na.pdf (revisado el 15/10/2018)
  filter(!respondent_id %in% c(10222573881,10102921585,10082531668,10196284637,10198382926,10213312166,10082105265,10215369623,10218366526,10218273295)) 

censo_hh %>% Hmisc::contents()
visdat::vis_dat(censo_hh)

## __repeticiones ----------

uni <- censo_hh %>% 
  count(hh) %>% #¿cuantas viviendas únicas? rpta: 360
  #count(n) # 1:(315) ; 2:(42) ; 3:(3) #¿con qué frecuencia se repiten?
  arrange(desc(n)) %>%  #¿quienes? #cuando
  filter(n==1)

#subgrupo de ingresos repetitivos -> 61 repetidas
rep <- censo_hh %>% 
  count(hh) %>% #¿cuantas viviendas únicas? rpta: 361
  #count(n) # 1:(296) ; 2:(55) ; 3:(8), 4:(2) #¿con qué frecuencia se repiten?
  arrange(desc(n)) %>%  #¿quienes? #cuando
  filter(n>1)

rep %>% filter(hh %in% backup$hh) #only 4 have a backup replicate

#retirar ingresos con NA en otras variables y con réplicas
censo_hh %>% filter(is.na(n_indiv)) %>% filter(hh %in% rep$hh)
censo_hh %>% filter(is.na(fuente.principal.de.agua.para.tomar.o.beber)) %>% 
  filter(hh %in% rep$hh) #%>% glimpse() #2
  #filter(hh %in% backup$hh) #no
censo_hh %>% filter(is.na(observaciones.de.vivienda)) %>% 
  filter(hh %in% rep$hh) %>% glimpse() #3
  #filter(hh %in% backup$hh) %>% glimpse()
#censo_hh %>% filter(is.na(tipo.de.vivienda)) #%>% 
  #filter(hh %in% rep$hh) #no
  #filter(hh %in% backup$hh) #no

#censo_hh %>% filter(hh=="PA016") #verificacion

#LISTA DE VIVIENDAS repetidas con replicas: n=121
rep_hh <- 
  censo_hh %>%  #423
  filter(hh %in% rep$hh) %>% #121
  #select(hh,n_indiv,date_created,difftime,respondent_id) %>% 
  arrange(hh,desc(date_created)) %>% #print(n=Inf) #visdat::vis_dat()
  #solo 17 viviendas con ingresos diferentes en variables con opciones fijas
  select(-respondent_id,-difftime,-difftime.dur,-date_created,-dirección,-referencia,
         -gpslat,-gpslon,-fuente.principal.de.agua.para.tomar.o.beber.otros,-observaciones.de.vivienda) %>% #glimpse()
  #filtrar valores únicos por grupo de vivienda repetida
  group_by(hh) %>% 
  unique() %>% 
  ungroup() %>% #print(n=Inf)
  count(hh) %>% arrange(desc(n)) 

#54
rep_hh_1 <- rep_hh %>% filter(n==1) #42 tienen todas sus réplicas idénticas!
rep_hh_2 <- rep_hh %>% filter(n>1) #12 tienen replicas diferentes

## __solve it -----

censo_hh_1 <- censo_hh %>% 
  filter(hh %in% rep_hh_1$hh) %>% 
  distinct(hh,.keep_all = T)

censo_hh_2 <- censo_hh %>% 
  filter(hh %in% rep_hh_2$hh) %>% arrange(hh,desc(date_created)) %>% 
  #replace_with_na_all(condition = ~.x %in% c("-1","-5")) %>% #print(n=Inf) #visdat::vis_dat()
  #select(#respondent_id,hh,fuente.principal.de.agua.para.tomar.o.beber,
  #       -difftime,-difftime.dur,#-date_created,
  #       -dirección,-referencia,-poblado,-utilizan.mosquiteros,
  #       -gpslat,-gpslon#,
  #       #fuente.principal.de.agua.para.tomar.o.beber.otros,observaciones.de.vivienda,n_indiv
  #       ) %>% 
  ##decisión: quedarte con el último digitado
  #select(date_created,everything()) %>% 
  distinct(hh,.keep_all = T) #%>% 
  #visdat::vis_dat()
  #print(n=Inf)
  #mutate(na=is.na())
  #filter()
#10203084167,10285645682,10285768109

#backup %>% select(hh) %>% arrange(hh) %>% print(n=Inf)

censo_hh <- censo_hh %>%  #423
  #filtrar viviendas de ingreso único
  filter(hh %in% uni$hh) %>% 
  #resueltos los 45 conflictos
  union_all(censo_hh_1) %>% 
  union_all(censo_hh_2)


# __merge with GPX --------------------------------------------------------

censo_hh <- readr::read_rds("../spatial/data/gpx_update.rds") %>% 
  select(hh=name,lon,lat,gps_registro=time,gps_fuente=id) %>% 
  mutate(hh=str_to_upper(hh)) %>% 
  right_join(censo_hh)

# __any to solve? -----------------------------------------------------------

#extraer info de OBSERVACIONES!!!

censo_hh %>% glimpse()

# __export it -------------------------------------------------------------

censo_hh %>% write_rds("data/vivienda_zungarococha_2018.rds")
censo_hh %>% write_csv("data/vivienda_zungarococha_2018.csv")
censo_hh %>% xlsx::write.xlsx("data/vivienda_zungarococha_2018.xlsx")
censo_hh %>% 
  rename_all(funs(str_replace_all(.,"\\.","_"))) %>% 
  rename("direccion" = "dirección",
         "servicio_usado_para_deposiciones" = "tipo_de_servicio_usado_para_deposiciones",
         "fuente_agua_para_tomar_o_beber" = "fuente_principal_de_agua_para_tomar_o_beber",
         "fuente_agua_para_tomar_o_beber_o" = "fuente_principal_de_agua_para_tomar_o_beber_otros") %>% 
  write_dta("data/vivienda_zungarococha_2018.dta")


# __fast descriptives -----------------------------------------------------

#preimpreso
censo_pr_hh %>% group_by(poblado) %>% 
  summarise(count_hh=n_distinct(hh),
            sum_indiv=sum(n,na.rm = T)) %>% 
  mutate(ratio_ind_hh=sum_indiv/count_hh) %>% ungroup()

#censo2018
censo_hh %>% group_by(poblado) %>% 
  summarise(count_hh=n_distinct(hh),
            sum_indiv=sum(n_indiv,na.rm = T)) %>% 
  mutate(ratio_ind_hh=sum_indiv/count_hh) %>% ungroup()


# (2) base sujetos -------------

## __preimpresa -----
censo_pr_in <- read_csv("data/individuo_zungarococha.csv") %>% 
  rename(hh=idvivienda) %>% 
  rename_all(funs(str_to_lower(.)))
#FALTA AGREGAR LABELS DE PARENTESCO, OCUPACION, CONCENTIMIENTO INFORMADO

## __censo 2018 -----
censo_in <- censo_df %>% #glimpse()
  #analizar la selección realizada por vivienda
  filter(respondent_id %in% censo_hh$respondent_id) %>% 
  #ordenar base
  select(respondent_id,hh,id01__dni:id20__observaciones) %>% 
  gather(key,value,-hh,-respondent_id) %>% 
  separate(key,c("id","var"),sep = "__") %>% #count(var)
  arrange(hh,id,var) %>% #print(n=100)
  mutate(var=if_else(var=="registro...dd.mm.aaaa.","registro..dd.mm.aaaa.",var)) %>% 
  filter(!is.na(value)) %>% 
  spread(var,value) %>% #visdat::vis_dat()
  mutate(id_=str_replace(id,"id(.+)","\\1"),
         id=str_c(hh,"_",id_)) %>%  #%>% #visdat::vis_dat()
  #control calidad
  mutate(nac...dd.mm.aaaa.=if_else(par=="05/08/1988","05/08/1988",nac...dd.mm.aaaa.),
         par=if_else(par=="05/08/1988","-9",par),
         ocu=if_else(ocu=="-55","-5",ocu),
         ocu=if_else(ocu=="13","-9",ocu))

# __qc -------------

a <- data_frame(#n=1:20,
  n=1:13,
  par=1:13 %>% as.character(),
  parentesco=c("padre","madre","hijo","hermano","abuelo","tío","nieto","primo","sobrino","suegro","cuñado","yerno/nuera","otro"))
b <- data_frame(#n=1:20,
  n=1:13,
  ocu=1:13 %>% as.character(),
  ocupacion= c("estudiante", "carpintero", "ama de casa", "mecanico", "agricultor", "pescador", "vigilante", "mototaxista"
               , "otro","","","",""))


censo_in %>% filter(id=="ZG005_01")

censo_in <- censo_in %>% left_join(b %>% select(-n)) %>% left_join(a %>% select(-n)) %>% 
  mutate(ocupacion=if_else(is.na(ocupacion),ocu,ocupacion) %>% str_to_lower(),
         parentesco=if_else(is.na(parentesco),par,parentesco) %>% str_to_lower()) %>% select(-ocu,-par)

#censo_in %>% count(par,parentesco) #corroborado
#censo_in %>% count(ocu,ocupacion) #corroborado



# __merge w/ print --------------------------------------------------------

censo_pr_in_long <- censo_pr_in %>% 
  mutate(registro=as.character(registro),nacimiento=as.character(nacimiento)) %>% 
  rename_all(funs(make.names(.))) %>% 
  select(-ocupacion_n,-parentesco_n,-hh) %>% arrange(id) %>% 
  filter(!str_count(id)>8) %>% 
  mutate(nacimiento=str_replace(nacimiento,"(.+)-(.+)-(.+)","\\3-\\2-\\1"),
         nacimiento=if_else(str_count(nacimiento)>3,str_replace_all(nacimiento,"-","\\/"),nacimiento),
         nacimiento=if_else(str_count(nacimiento)>3,str_replace_all(nacimiento,"^\\/",""),nacimiento),
         
         registro=str_replace(registro,"(.+)\\s(.+)","\\1"),
         registro=str_replace(registro,"(.+)-(.+)-(.+)","\\3-\\2-\\1"),
         registro=if_else(str_count(registro)>3,str_replace_all(registro,"-","\\/"),registro),
         
         id=if_else(str_count(id)<8,str_replace(id,"(......)(.)","\\10\\2"),id),
         pr_="pre_print") %>% #count(nacimiento)
  #glimpse()
  gather(key,value_pr,-id) #%>% count(key)

censo_in_nu <- censo_in %>% 
  rename(nacimiento=nac...dd.mm.aaaa.,sexo=sex,registro=registro..dd.mm.aaaa.,nombre=nombres.y.apellidos) %>% 
  select(-respondent_id,-hh,-id_) %>% 
  mutate(nacimiento=if_else(str_count(nacimiento)>3,str_replace_all(nacimiento,"-","\\/"),nacimiento),
         registro=if_else(str_count(registro)>3,str_replace_all(registro,"-","\\/"),registro),
         nacimiento=if_else(str_count(nacimiento)>3,str_replace_all(nacimiento,"^\\/",""),nacimiento),
         nu_="2018_census") %>% #count(nacimiento)
  #glimpse()
  gather(key,value,-id) %>% #count(key)
  mutate(keep=if_else(value=="-5",TRUE,FALSE)) %>% 
  full_join(censo_pr_in_long) %>% 
  arrange(id,key) %>% 
  #OJO: con if_else: advantage: TRUE, FALSE, NA
  mutate(value_nu=if_else(value=="-5",value_pr,value,value_pr)) %>% #print(n=100)
  select(-value,-keep,-value_pr) %>% 
  distinct(.keep_all = T) %>% 
  distinct(id,key,.keep_all = T) %>% 
  #group_by(id) %>% 
  #rownames_to_column() %>% filter(rowname %in% c(22510, 22511,22521, 22522) )
  #ungroup() %>% 
  spread(key,value_nu) %>% 
  select(id,registro,ci,dni,sexo,nacimiento,nombre,ocupacion,parentesco,observaciones,pr_,nu_, fam,actualizacion,estado.basal,estado.actual) %>% 
  mutate(observaciones_sum=if_else(str_detect(observaciones,"(.+)\\/(.+)\\/(.+)|NAC"),"nacimiento",
                                   if_else(str_detect(observaciones,"MUDO|VIVE"),"mudanza_vive",
                                           if_else(str_detect(observaciones,"VIAJO|SALIO"),"viaje_salio","otro"))))
  
  
#censo_pr_in
#censo_in
censo_in_nu %>% count(observaciones_sum) %>% mutate(per=100*n/sum(n))
censo_in_nu %>% filter(!is.na(observaciones) & observaciones!="-1") #%>% print(n=Inf)


#absolver observaciones [pendiente!!!!]
#censo_in_nu %>% filter(nacimiento=="-9") %>% select(id,nacimiento,observaciones,registro) %>% 
#  mutate(nacimiento=if_else(str_detect(observaciones,"(.+)\\/(.+)\\/(.+)"),"TRUE","FALSE"),
#         observaciones=make.names(observaciones),
#         nacimiento=if_else(nacimiento=="TRUE",str_replace(observaciones,"(FECHA.NAC..)|(FECHA.DE.NAC...)|(MOD..DE.NAC..)|(MOD..FECHA.DE.NAC...)|(MOD.FECHA.DE.NAC...)|(FECHA.DE.NAC:)|(FECHA.DE.NACIMIENTO.)|(FEC.NAC.)|(NO.TIENE.DNI)|(YATSURI.LIAN.VASQUEZ.TAMANI.)|X",""),nacimiento),
#         #nacimiento=if_else(nacimiento=="TRUE",str_replace(nacimiento,"\\(",""),nacimiento)
#         ) %>% 
#  print(n=Inf)


# __verify dates -----------------------------------------------------------


censo_pr_in
censo_pr_in_long %>% filter(key=="nacimiento")
censo_in
censo_in_nu #%>% glimpse()


# __calculate age at MAY 2018 ---------------------------------------------

censo_in_nu <- censo_in_nu %>% #select(nacimiento) %>% 
  mutate(nacimiento_=dmy(nacimiento),
         z3_censo=ymd("2018-04-01"),
         z0_censo=ymd("2015-12-31"),
         #edad_t=z3_censo-nacimiento_,
         #edad_d=as.double(edad_t),
         edad_z3=interval(nacimiento_,z3_censo)/years(1),
         edad_z0=interval(nacimiento_,z0_censo)/years(1),
         edad_z3=floor(edad_z3),
         edad_z0=if_else(edad_z0<0,NA_real_,floor(edad_z0))
         ) %>% 
  select(-nacimiento_,-z3_censo,-z0_censo)

# __export it -------------------------------------------------------------

censo_in_nu %>% write_rds("data/individuo_zungarococha_2018.rds")
censo_in_nu %>% write_csv("data/individuo_zungarococha_2018.csv")
censo_in_nu %>% xlsx::write.xlsx("data/individuo_zungarococha_2018.xlsx")
censo_in_nu %>% 
  rename_all(funs(str_replace_all(.,"\\.","_"))) %>% 
  write_dta("data/individuo_zungarococha_2018.dta")


# __fast descriptives -----------------------------------------------------

#preimpreso
censo_pr_in %>% 
  mutate(poblado=str_replace(hh,"(..).+","\\1")) %>% 
  group_by(poblado) %>% 
  summarise(count_hh=n_distinct(hh),
            sum_indiv=n_distinct(id)) %>% 
  mutate(ratio_ind_hh=sum_indiv/count_hh) %>% ungroup()

#censo2018
censo_in_nu %>% filter(!is.na(nu_))
censo_in_nu %>% filter(!is.na(pr_))
censo_in_nu %>% filter(!is.na(nu_) & observaciones_sum!="mudanza_vive")
#censo_in_nu %>% filter((!is.na(pr_) | !is.na(nu_)) & observaciones_sum!="mudanza_vive")

censo_in_nu %>%
  filter(!is.na(nu_) & observaciones_sum!="mudanza_vive") %>% 
  filter(sexo=="F" | sexo=="M") %>% 
  count(sexo) %>% mutate(per=100*n/sum(n))

censo_in_nu %>%
  filter(!is.na(nu_) & observaciones_sum!="mudanza_vive") %>% 
  mutate(poblado=str_replace(id,"(..).+","\\1"),
         hh=str_replace(id,"(.....).+","\\1")) %>% 
  group_by(poblado) %>% 
  summarise(count_hh=n_distinct(hh),
            sum_indiv=n_distinct(id)) %>% 
  mutate(ratio_ind_hh=sum_indiv/count_hh) %>% ungroup()


# (3) esperado vs observado -------------

censo_pr_hh %>% count(hh) #423
censo_hh %>% count(hh) #315

zunga_sm <- censo_pr_hh %>% count(hh) %>% rename(preprnt=nn) %>% 
  full_join(censo_hh %>% count(hh)) %>% rename(cen18=n) %>% 
  arrange(hh) %>% 
  mutate(update = (preprnt>=1) == (cen18>=1),
         only1 = is.na(cen18) ,
         only2 = is.na(preprnt),
         poblado = str_replace(hh,"(..).+","\\1")) 
#filter(test==FALSE | test==NA) %>%

zunga_sm %>% count(preprnt) #423 preimpreso
zunga_sm %>% count(cen18) #315 unicas

zunga_sm %>% count(update) #270 actualizadas
zunga_sm %>% count(only1) #153 solo en preimpreso
zunga_sm %>% count(only2) #45 solo en censo 2018

zunga_sm %>% count(cen18,update,only2) #226+40+4+45 = 315 registros finales

#descriptivo de VIVIENDAS ACTUALIZADAS
zunga_sm %>% #group_by(poblado) %>% 
  summarise(n_update=sum(update,na.rm = T),
            n_=n(),
            p_update=100*sum(update,na.rm = T)/n())
zunga_sm %>% group_by(poblado) %>% 
  summarise(n_update=sum(update,na.rm = T),
            n_=n(),
            p_update=100*sum(update,na.rm = T)/n())

#descriptivo de VIVIENDAS NO CENSADAS
zunga_sm %>% #group_by(poblado) %>% 
  summarise(n_only1=sum(only1,na.rm = T),
            n_=n(),
            p_only1=100*sum(only1,na.rm = T)/n())
zunga_sm %>% group_by(poblado) %>% 
  summarise(n_only1=sum(only1,na.rm = T),
            n_=n(),
            p_only1=100*sum(only1,na.rm = T)/n())

#descriptivo de NUEVAS VIVIENDAS
zunga_sm %>% #group_by(poblado) %>% 
  summarise(n_only2=sum(only2,na.rm = T),
            n_=n(),
            p_only2=100*sum(only2,na.rm = T)/n())
zunga_sm %>% group_by(poblado) %>% 
  summarise(n_only2=sum(only2,na.rm = T),
            n_=n(),
            p_only2=100*sum(only2,na.rm = T)/n())

zunga_sm %>% #count(poblado)
  select(poblado,hh,preprint=preprnt,censo2018=cen18#,update,only1,only2
         ) %>% #count(censo2018)
  mutate(obs="                                                                      ") %>% 
  #print(n=Inf)
  xlsx::write.xlsx("data-raw/surveym/ddig_hh_list.xlsx")


#REPLICAS CORRECTAS
#LL055 -> 1
#NN075 -> 2
#NN086 -> 1
#PA009 -> 2 [ingreso multiple]
#PA013 -> 2
#PA023 -> 2 
#PA054 -> 2 [revisar]
#PA107 -> 1 [multiple]
#ZG006 -> 2
#ZG047 -> 1
#ZG074 -> 2 [corregir: censada]
#ZG131 -> 1
#ZG159 -> 1


# (x) tiempo digitacion vs numero de individuos --------


#png("data-raw/surveym/eduardo/table.png", height=500, width=1000)
#p<-tableGrob(censo_df)
#grid.arrange(p)
#dev.off()

censo_df %>% 
  filter(difftime<600) %>% 
  mutate(month=lubridate::month(date_created,label=T,abbr=F)) %>% 
  ggplot(aes(n_indiv,difftime)) +
  geom_hline(yintercept = 5, colour="red", lwd=1) +
  geom_point(aes(colour=date(date_created)),alpha=0.4) + 
  #ggrepel::geom_text_repel(aes(label=hh), direction = "x", nudge_y = 0.15) +
  scale_x_continuous(breaks=seq(1,15)) +
  facet_grid(~month) +
  labs(title="Relation between #individuals and typing time") + 
  xlab("number of individuals at household") + ylab("typing time per household (minutes)") #+
  #annotate("text", x=12, y=8, label= "estimate time: 5-7min", colour="red")
ggsave("data-raw/surveym/eduardo/indiv_time.png",width = 15,height = 3)


# AHORA
# agregar base de individuo


# (4) MAPS ----------------------------------------------------------------
rm(list = ls())

library(ggmap)
register_google(key = "AIzaSyAvGB5P3z9Y7ATJa1GUzYaYNwX7SaM0Vk4")
ggmap_credentials()
ggmap(get_googlemap()) #works

# __data -----------
movm <- read_rds("data/vivienda_movimiento_gps.rds") %>% select(-id) %>% 
  rename(lat_movm=lat,
         lon_movm=lon)
prev <- read_csv("data/vivienda_zungarococha.csv") %>% 
  select(hh,community_1=poblado,latitud,longitud) %>% 
  mutate(latitud=as.numeric(latitud),
         longitud=as.numeric(longitud)) %>% 
  rename(lat_2015=latitud,
         lon_2015=longitud)
dtmp <- read_rds("data/vivienda_zungarococha_2018.rds") %>% 
  select(hh,
         community_1=poblado,
         longitud=lon,
         latitud=lat) %>% 
  rename(lat_2018=latitud,
         lon_2018=longitud)
per <- ggmap::get_googlemap("amazon golf course",zoom = 12,
                            color = "bw",
                            maptype = "roadmap"
                            #maptype = "terrain"
                            #maptype="satellite"
                            #maptype="hybrid" (satellite + terrain)
                            )

# __listas relevantes -----------------------------------------------------


# ____missing -------------------------------------------------------------

dtmp %>% 
  filter(is.na(lon_2018))

# ____incorrectas ---------------------------------------------------------


# ______ censo vs movmal --------------------------------------------------


left_join(movm,dtmp) %>% 
  filter(!is.na(lon_2018)) %>% 
  mutate(distance_e=sqrt((lat_movm-lat_2018)^2+(lon_movm-lon_2018)^2)*(10^5)) %>% 
  arrange(desc(distance_e)) %>% xlsx::write.xlsx("data")
  #arrange(distance_e) %>% #print(n=100)
  filter(distance_e<100) %>% 
  ggplot(aes(distance_e)) + geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 5,colour="red")

# ______ censo vs pre-censo ------------------------------------------------------


#426 -> 360 = 486
full_join(prev,dtmp) %>% 
  filter(!is.na(lon_2018)) %>% 
  mutate(distance_e=sqrt((lat_2015-lat_2018)^2+(lon_2015-lon_2018)^2)*(10^5)) %>% 
  arrange(desc(distance_e)) %>% #xlsx::write.xlsx("data") #print(n=100)
  #arrange(distance_e) %>% #print(n=100)
  filter(distance_e<10) %>% 
  ggplot(aes(distance_e)) + geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 5,colour="red")


# _graficos ---------------------------------------------------------------


# __por area ----------
ggmap::ggmap(per) +
  geom_point(aes(x=longitud, y=latitud,
                 colour=community_1), data=dtmp) +
  theme(legend.position=c(0.88,0.17),
        legend.margin = margin(0.2,0.2,0.2,0.2)) + 
  scale_colour_discrete(name="Villages") +
  xlab("Longitud") +
  ylab("Latitud") +
  coord_fixed(ylim = c(-3.87,-3.72)) +
  ggsn::scalebar(dist = 2, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84',
                 x.min=-73.40,x.max=-73.28,y.min=-3.86,y.max=-3.76,
                 location = "bottomright") +
  ggsn::north(x.min=-73.40,x.max=-73.21,y.min=-3.86,y.max=-3.722,scale = .2,symbol = 5)
#ggsave("figure/map-iquitos_zungaro.png",height=4.5,width=7.5)
#ggsave("figure/map-iquitos_zungaro.pdf",height=4.5,width=7.5)

# __por puntos ---------------
ggmap::qmplot(longitud, latitud, 
              data = dtmp, 
              zoom = 13,
              color = community_1,
              alpha = I(.4),
              extent = "panel")
ggsave("figure/map-iquitos_zungaro-all_censo.pdf",height=4.5,width=7.5)

ggmap::qmplot(longitud, latitud, 
              data = dtmp, 
              zoom = 13,
              color = community_1,
              alpha = I(.4),
              extent = "panel") +
  theme(legend.position=c(0.9,0.2),
        legend.margin = margin(0,0,0,0)) + 
  scale_colour_discrete(name="Villages") +
  xlab("Longitud") +
  ylab("Latitud") +
  coord_fixed(ylim = c(-3.863,-3.824)) +
  ggsn::scalebar(dist = 1, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84',
                 x.min=-73.40,x.max=-73.357,y.min=-3.86,y.max=-3.83,
                 location = "bottomright") +
  ggsn::north(x.min=-73.40,x.max=-73.339,y.min=-3.86,y.max=-3.825)

#ggsave("figure/map-iquitos_zungaro-bw.png",height=4.5,width=7.5)
ggsave("figure/map-iquitos_zungaro-bw.pdf",height=4.5,width=7.5)
