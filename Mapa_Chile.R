rm(list = ls())

library(chilemapas)
library(tidyverse)
library(sf)
library(tikzDevice)

regiones = generar_regiones(mapa = chilemapas::mapa_comunas) |>
  st_transform(crs = 4326)

regs = st_centroid(regiones)

regs["cod_reg"] = sort(c("01: Tarapacá", "02: Antofagasta", "03: Atacama", "04: Coquimbo", "05: Valparaíso",
                        "06: O'Higgins", "07: Maule", "08: Biobío", "09: La Araucanía", "10: Los Lagos", "11: Aysén", "12: Magallanes",
                        "13: Metropolitana", "14: Los Ríos", "15: Arica", "16: Ñuble"))

st_bbox(regiones)

Leyenda = data.frame(X = -87, Y = seq(from = -18, to = -33, length.out = 16)) |>
  st_as_sf(coords = c("X", "Y"), crs = 4326)

Leyenda["cod_reg"] = sort(c("01: Tarapacá", "02: Antofagasta", "03: Atacama", "04: Coquimbo", "05: Valparaíso",
                            "06: O'Higgins", "07: Maule", "08: Biobío", "09: La Araucanía", "10: Los Lagos", "11: Aysén", "12: Magallanes",
                            "13: Metropolitana", "14: Los Ríos", "15: Arica", "16: Ñuble"))

regs$geometry[5] = regs$geometry[5] + c(0.55,0.25)
regs$geometry[13] = regs$geometry[13] + c(0,0.1)
regs$geometry[8] = regs$geometry[8] + c(0.5,0)
regs$geometry[10] = regs$geometry[10] + c(2,-0.3)
regs$geometry[11] = regs$geometry[11] + c(0.5,0)
regs$geometry[12] = regs$geometry[12] + c(4.25,-1.25)

chile = ggplot() +
  theme_bw() +
  theme(text = element_text(family = "serif"))+
  geom_sf(data = regiones, fill = "white", col = "black")+
  geom_sf_text(data = regs, aes(label = codigo_region),
               size = 3, col = "black", fontface = "bold",
               family = "serif") +
  geom_sf_text(data = Leyenda, aes(label = cod_reg),
               size = 3, hjust = 0, col = "darkslategray",
               family = "serif")+
  xlab("") +
  ylab("") +
  coord_sf(xlim = c(-90, -60), ylim = c(-60, -15), expand = FALSE)

chile

tikz("Chile-Covid19.tex", standAlone = TRUE)
chile
dev.off()


# #color chart http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#
# #colores de tener o no datos
# Data<-NULL
# for(i in 1:length(regiones$codigo_region)){
# if(regiones$codigo_region[i]=="20" | regiones$codigo_region[i]=="20"){
# Data[i]="Not observed"}
# else{
# Data[i]="Observed"}}
# #etiquetas de las regiones
# eti<-NULL
# for(i in 1:length(regiones$codigo_region)){
# if(regiones$codigo_region[i]=="20" | regiones$codigo_region[i]=="20"){
# eti[i]="black"}
# else{
# eti[i]="white"}
#   }
# eti[10]="black"
# eti[12]="black"
#
# #nugget para correr los n?meros de las estaciones
# regiones$nudge_x <- 0
# regiones$nudge_x2<- -10
# regiones$nudge_x[regiones$codigo_region == "05"] <- 0.5
# regiones$nudge_x[regiones$codigo_region == "10"] <- 2
# regiones$nudge_x[regiones$codigo_region == "11"] <- 0.5
# regiones$nudge_x[regiones$codigo_region == "12"] <- 4
#
