
# DESCRIPTIVOS ------------------------------------------------------------

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(compareGroups)

# importar base -----------------------------------------------------------

ind_viv <- read_rds("data/z0_ind_viv.rds") %>% 
  
  #retirar observaciones segun reporte 25junio2019
  filter(!id %in% c("NN003-4","NN061-1"))

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
                recibio_tratamiento_malaria_c+
                enfermedad_cronica+
                enf_tomo_medicinas_parasitos+
                
                epi_cerca_fuente_agua+epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_malaria_ultimos_meses_c+epi_viajo_fuera_villa+epi_meses_ultima_malaria+#epi_especie_causo_malaria+
                
                
                #epi_alguien_tuvo_malaria+
                new_alguien_tuvo_malaria+new_alguien_tuvo_malaria_c+
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
              ,method = c(epi_meses_ultima_malaria = 2)
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_prev_viv.xls")


compareGroups(sero_viv ~ 
                
                age_quart+
                sex_8+
                nivel_educacion+
                trabajo_tdy+trabajo_rpl+
                actualmente_estudiando+
                
                community_1 + residence_quart+#residence+#  
                
                electricidad_red_publica+
                combustible_cocinar+
                ah_estereo+ah_television+ah_radio+ah_refrigerador+ah_motocicleta+ah_mototaxi+
                material_pared_c+material_piso_c+
                fuente_agua+banio_conexion+
                
                tenido_malaria+
                recibio_tratamiento_malaria_c+
                enfermedad_cronica+
                enf_tomo_medicinas_parasitos+
                
                epi_cerca_fuente_agua+epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_malaria_ultimos_meses_c+epi_viajo_fuera_villa+epi_meses_ultima_malaria+#epi_especie_causo_malaria+
                
                
                #epi_alguien_tuvo_malaria+
                new_alguien_tuvo_malaria+new_alguien_tuvo_malaria_c+
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
                prev_viv+
                sero_viv
              
              ,data = ind_viv ,byrow=T 
              ,method = c(epi_meses_ultima_malaria = 2)
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_sero_viv.xls")

compareGroups(sero_fal ~ 
                
                age_quart+
                sex_8+
                nivel_educacion+
                trabajo_tdy+trabajo_rpl+
                actualmente_estudiando+
                
                community_1 + residence_quart+#residence+#  
                
                electricidad_red_publica+
                combustible_cocinar+
                ah_estereo+ah_television+ah_radio+ah_refrigerador+ah_motocicleta+ah_mototaxi+
                material_pared_c+material_piso_c+
                fuente_agua+banio_conexion+
                
                tenido_malaria+
                recibio_tratamiento_malaria_c+
                enfermedad_cronica+
                enf_tomo_medicinas_parasitos+
                
                epi_cerca_fuente_agua+epi_tipo_fuente_agua+epi_distancia_fuente_agua+
                epi_malaria_ultimos_meses_c+epi_viajo_fuera_villa+epi_meses_ultima_malaria+#epi_especie_causo_malaria+
                
                
                #epi_alguien_tuvo_malaria+
                new_alguien_tuvo_malaria+new_alguien_tuvo_malaria_c+
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
                prev_fal + prev_viv+
                sero_viv+sero_fal
              
              ,data = ind_viv ,byrow=T 
              ,method = c(epi_meses_ultima_malaria = 2)
) %>% 
  createTable(show.n = T,show.all = T) %>% 
  export2xls("table/z0-tab2_sero_fal.xls")

