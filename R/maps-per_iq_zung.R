# MAPAS ------------------
library(tidyverse)
library(maps)
library(mapdata)
library(grDevices)
library(ggmap)
library(ggsn)

rm(list = ls())

# credentials ------------------
register_google(key = "AIzaSyAvGB5P3z9Y7ATJa1GUzYaYNwX7SaM0Vk4")
ggmap_credentials()
ggmap(get_googlemap()) #works

# PERU ------------------
grDevices::png("figure/map-peru_iquitos_river.png")
#grDevices::pdf("figure/map-peru_iquitos_river.pdf")
maps::map(regions=maps::sov.expand(c("Peru","Ecuador","Brazil","Chile",
                                     "Bolivia","Colombia")), 
          xlim=c(-82,-68),ylim=c(-19,0),col="gray95", fill=TRUE)
maps::map('rivers', add=TRUE, col = "blue")
maps::map.axes()
points(-73.253801, -3.743316, pch=19, col="red", cex=1.5)
text(-73.253801, -3.743316,"Iquitos",pos = 2,cex=2)
dev.off()

# DATA ZUNGAROCOCHA ------------------
dtmp <- readr::read_rds("data/z0_viv.rds") #readr::read_rds("data/x-zungaro_basal.rds")
per <- ggmap::get_googlemap("amazon golf course",zoom = 12,
                            maptype = "roadmap")

#_ MAPA IQUITOS ---------------------
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
ggsave("figure/map-iquitos_zungaro.png",height=4.5,width=7.5)
ggsave("figure/map-iquitos_zungaro.pdf",height=4.5,width=7.5)

#_ MAPA ZUNGAROCOCHA ------------------
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
ggsave("figure/map-iquitos_zungaro-bw.png",height=4.5,width=7.5)
*ggsave("figure/map-iquitos_zungaro-bw.pdf",height=4.5,width=7.5)


#_ COMMUNITY MAPS ------------------------------------------------------


# __ pcr vivax ---------------------------------------------------------

com <- levels(dtmp$community_1)

for (i in com) { #i="Zungarococha"
  ggmap::qmplot(longitud, latitud, 
                data = dtmp %>% 
                  filter(community_1==i 
                         #& prev_viv_hoth==1
                         ) %>% 
                  mutate(prev_viv_hoth=fct_relevel(prev_viv_hoth,"1"))
                , 
                zoom = 13,
                color = prev_viv_hoth,
                #alpha = I(.4),
                extent = "panel") +
    #stat_density_2d(aes(fill = ..level..), 
    #                geom = "polygon", alpha = .03, color = NA) +
    #scale_fill_gradient2("Household\nDensity", 
    #                     low = "white", 
    #                     mid = "yellow", 
    #                     high = "red"
    #                     ) +
    #geom_point(aes(colour=prev_viv_hoth)) +
    xlab("Longitud") +
    ylab("Latitud") +
    guides(fill=FALSE) +
    labs(title=i,
         subtitle=expression(paste("PCR positive households for ",
                                italic("Plasmodium vivax")," malaria")))
  
  ggsave(paste0("figure/map-cc-pcrv-",i,".png"),height=4.5,width=7.5)
  
}

# __ sero viv ---------------------------------------------------------

for (i in com) {
  ggmap::qmplot(longitud, latitud, 
                data = dtmp %>% 
                  filter(community_1==i 
                         #& sero_viv_hoth==1
                  ) %>% 
                  mutate(sero_viv_hoth=fct_relevel(sero_viv_hoth,"1"))
                , 
                zoom = 13,
                color = sero_viv_hoth,
                #alpha = I(.4),
                extent = "panel") +
    #stat_density_2d(aes(fill = ..level..), 
    #                geom = "polygon", alpha = .03, color = NA) +
    #scale_fill_gradient2("Household\nDensity", 
    #                     low = "white", 
    #                     mid = "yellow", 
    #                     high = "red", 
    #                     midpoint = 650) +
    #geom_point(aes(colour=sero_viv_hoth)) +
    xlab("Longitud") +
    ylab("Latitud") +
    guides(fill=FALSE) +
    labs(title=i,
         subtitle=expression(paste("Serology positive households for ",
                                   italic("Plasmodium vivax")," malaria")))
  
  ggsave(paste0("figure/map-cc-serv-",i,".png"),height=4.5,width=7.5)
  
}


# __ sero fal ------------------------------------------------------------

for (i in com) {
  ggmap::qmplot(longitud, latitud, 
                data = dtmp %>% 
                  filter(community_1==i 
                         #& sero_fal_hoth==1
                  ) %>% 
                  mutate(sero_fal_hoth=fct_relevel(sero_fal_hoth,"1"))
                , 
                zoom = 13,
                color = sero_fal_hoth,
                #alpha = I(.4),
                extent = "panel") +
    #stat_density_2d(aes(fill = ..level..), 
    #                geom = "polygon", alpha = .03, color = NA) +
    #scale_fill_gradient2("Household\nDensity", 
    #                     low = "white", 
    #                     mid = "yellow", 
    #                     high = "red", 
    #                     midpoint = 650) +
    #geom_point(aes(colour=sero_fal_hoth)) +
    xlab("Longitud") +
    ylab("Latitud") +
    guides(fill=FALSE) +
    labs(title=i,
         subtitle=expression(paste("Serology positive households for ",
                                   italic("Plasmodium falciparum")," malaria")))
  
  ggsave(paste0("figure/map-cc-serf-",i,".png"),height=4.5,width=7.5)
  
}

