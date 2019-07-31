
# import libraries --------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(xlsx)
library(janitor)

theme_set(theme_bw())

# import dataset ----------------------------------------------------------

zbas <- read_rds("data/x-zungaro_basal.rds") %>% 
  as_factor() %>% 
  select(id) %>% 
  mutate(zbas=TRUE)

z0db <- read_dta("data/z0_ind_viv_t3.dta") %>% 
  as_factor() %>% 
  select(id) %>% 
  mutate(id_en_zungaro_basal=TRUE)

zicd <- read_xlsx("data-raw/Codes_from_original_datebase_with_ICDs-20190625.xlsx",skip = 1) %>% 
  rename_all(~make.names(.)) %>% 
  rename_all(~str_to_lower(.)) %>% 
  mutate(id=str_replace(code,"_","-")) %>% 
  select(id) %>% 
  mutate(id_con_consentimiento=TRUE)

# contrastar ID --------------------------------------------------------------

z0db %>% print(n=Inf)
zicd

anti_join(zbas,z0db)
anti_join(z0db,zbas)

full_comp <- full_join(z0db,zicd) %>% arrange(id) %>% print(n=Inf)
subs_comp <- full_join(z0db,zicd) %>% 
  filter(is.na(id_en_zungaro_basal)|is.na(id_con_consentimiento)) %>% 
  arrange(id_en_zungaro_basal,id) %>% print(n=Inf)

subs_comp %>% write.xlsx("table/z0-contrast_id.xlsx",sheetName = "subset")
full_comp %>% write.xlsx("table/z0-contrast_id.xlsx",sheetName = "all_id",append = T)

anti_join(z0db,zicd)
anti_join(zicd,z0db)

# resumen final -----------------------------------------------------------

#tabla
full_comp %>% 
  mutate(id_en_zungaro_basal=replace_na(id_en_zungaro_basal,FALSE),
         id_con_consentimiento=replace_na(id_con_consentimiento,FALSE)) %>% 
  tabyl(id_en_zungaro_basal,id_con_consentimiento) %>% 
  adorn_totals(where = c("row","col")) %>% 
  adorn_title()

#detallado
subs_comp %>% print(n=Inf)
