library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggtext)
library(forcats)
library(readxl)
library(sf)
library(tidyverse)
library(haven) 


veli <- read_dta("C:/Users/Dell/Desktop/Sly/Main1-5.dta")


eli_map<- readxl::read_excel("C:/Users/Dell/Desktop/Sly/eli_map.xlsx")

regional_shp <- read_sf("C:/Users/Dell/Desktop/Sly/gss_phc2021_bnd_edgematched_20240821151831.gpkg", 
                        layer = "gss_phc2021_16_regions")

district_shp <- read_sf("C:/Users/Dell/Desktop/Sly/gss_phc2021_bnd_edgematched_20240821151831.gpkg", 
                        layer = "gss_phc2021_261_admin_districts")



fig42 <- readxl::read_excel("C:/Users/Dell/Desktop/Sly/eli_map.xlsx")
fig42_sf <- fig42 |> 
  left_join(district_shp,
            by = c("District"="distcode_name")) |> 
  st_as_sf()


fig42_sf |> 
  ggplot()+
  geom_sf(aes(fill=`Basic Handwashing`, geometry = geom), colour = "gray80", linewidth=0.0001)+
  geom_sf(data = regional_shp, aes(geometry = geom),colour="black",linewidth=0.1,
          fill = "transparent") +
   #geom_sf_text(
    # aes(label = case_when(
       #str_detect(district, "(?i)\\bMunicipal\\b") ~ str_wrap(str_remove(district,"(?i)\\bMunicipal\\b"),width = 15),
      # TRUE ~ str_wrap(district,width = 15)
     #)), size = 1)+
  geom_sf_text(
    data = regional_shp,
    aes(label = region_name, geometry = geom),
    vjust = .5, show.legend = F,
    family = "Century Gothic", size = 3, fontface = "bold") +
  
  scale_fill_gradientn(colours= c("#FECCCC","#FF9999","#FF6666","#FF3333","#CC0000","#990000"),
                       name = "Percent",
                       limits = c(0, 90)) +
  #scale_color_manual(values = c("black","white"))+
  labs(x=NULL,y=NULL)+
  theme(legend.text  = element_text(size = 7.5, family = "Century Gothic", face = "bold"),
        legend.title = element_text(size = 7.5, family = "Century Gothic", face="bold"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.95,0.6),
        plot.title = element_text(family = "Century Gothic", hjust = 0.3, face = "bold.italic"))













fig42_sf |> 
  ggplot()+
  geom_sf(aes(fill=`Handwashing-Percent of Households`, geometry = geom), colour = "#e5e5e5", linewidth=0.01)+
  geom_sf(data = regional_shp, aes(geometry = geom),colour="black",linewidth=0.1,
          fill = "transparent") +
  # geom_sf_text(
  #   aes(label = case_when(
  #     str_detect(district, "(?i)\\bMunicipal\\b") ~ str_wrap(str_remove(district,"(?i)\\bMunicipal\\b"),width = 15),
  #     TRUE ~ str_wrap(district,width = 15)
  #   )), size = 1)+
  geom_sf_text(
    data = regional_shp,
    aes(label = region_name, geometry = geom, colour = ifelse(region_name == "Greater Accra","yes","no")),
    vjust = .5, show.legend = F,
    family = "Century Gothic", size = 2, fontface = "bold") +
  # scale_fill_gradientn(colours =(population_color_scheme), name = "Percent",
  #                      limits = c(10, 80)) +
  scale_fill_gradientn(colours =(incidence_color_scheme), name = "Percent",
                       limits = c(10, 80)) +
  scale_color_manual(values = c("black","black"))+
  labs(x=NULL,y=NULL)+
  theme(legend.text  = element_text(size = 8.5, family = "Century Gothic", face = "bold"),
        legend.title = element_text(size = 8.5, family = "Century Gothic", face="bold"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.95,0.6),
        plot.title = element_text(family = "Century Gothic", hjust = 0.3, face = "bold"))
