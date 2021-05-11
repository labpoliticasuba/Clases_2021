library(tidyverse)
library(sf)
library(rgdal)
library(leaflet)
library(ggplot2)

#MAPAS ESTÁTICOS

#primero cargamos el dataset con el que vamos a trabajar
espacios_culturales_csv<-read.csv("espacios-culturales.csv")

#ahora vamos a cargar el mapa de CABA, sería la base sobre la que vamos a graficar
mapa_caba<-st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

# miramos nuestros datasets para ver las variables en común sobre las cuales podemos unirlos
head(mapa_caba)
glimpse(espacios_culturales_csv)

#Si queremos graficar por ejemplo por "barrio" debemos modificar el nombre de la columna
espacios_culturales_csv<-espacios_culturales_csv %>%
  #group_by(BARRIO) %>%
  rename(barrio=BARRIO)
# Vamos a graficar la ubicación geográfica de cada espacio
#Unimos csv y GeoJSON
espacios_culturales<-full_join(mapa_caba,espacios_culturales_csv, by = "barrio")

#Ploteamos
m_espacios_culturales<-ggplot(espacios_culturales, aes(x=LONGITUD, y=LATITUD))+
  geom_sf()+
  geom_point(aes(color=FUNCION_PR))+
  labs(title= "Espacios Culturales", subtitle = "Ciudad de Bs As", 
       colour = "Tipo de espacio", x = "long", y = "lat", caption = "Fuente: https://data.buenosaires.gob.ar/dataset/espacios-culturales")
m_espacios_culturales

####
#Podemos filtrar y graficar solo un tipo de espacio que nos interese graficar mapear

cc_monumentos<-espacios_culturales_csv%>%
  filter(str_detect(FUNCION_PR, "MONUMENTOS"))%>%
  group_by(barrio)%>%
  summarise(cantidad=n())

##Unimos ambos datasets. geografías y csv.
cc_monumentos<-left_join(mapa_caba, cc_monumentos)

#ahora graficamos, toda la carne al asador
m_cc_monumentos<-ggplot(cc_monumentos, aes(fill=cantidad))+
  geom_sf()+
  scale_fill_distiller("cantidad", palette = "Reds", direction = 1)+
  labs(title= "cantidad de monumentos por barrio")
m_cc_monumentos


