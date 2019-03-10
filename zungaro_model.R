library(tidyverse)
library(haven)

z0db <- read_dta("data/z0_ind_viv_t3.dta") %>% as_factor()
z0db %>% glimpse()

glm.full <- glm(low ~ age + lwt + race.cat + smoke + preterm + ht + ui + ftv.cat, data = lbw, family = binomial)
glm.null <- glm(low ~ 1, data = lbw, family = binomial)

https://rstudio-pubs-static.s3.amazonaws.com/2899_a9129debf6bd47d2a0501de9c0dc583d.html