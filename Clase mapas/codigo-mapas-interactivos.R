#MAPAS INTERACTIVOS con leaflet
pacman::p_load(leaflet, RColorBrewer, htmltools, tidyverse, rgeos)




#creamos un mapa basico
m_prueba1 <- leaflet() %>% 
  addTiles() %>% #addTiles() nos da la capa base del mapa. si lo dejamos vacío agrega tiles default de OpenStreetMap
  addMarkers(lng = -58.445071, lat = -34.616823, popup = "Centro Geografico de la CABA") #agregamos marcador con popup clickeable
m_prueba1

#Opciones de Inicializacion para mapas interactivos (van dentro de parentesis original, puede estar vacio y usar los valores por defecto)

m_prueba2 <- leaflet (options = leafletOptions(minZoom = 1, maxZoom = 18)) %>% #Asignar valores de minZoom y maxZoom (*18 es el max)
  addTiles() %>% 
  addMarkers(lng = -58.445071, lat = -34.616823, popup = "Centro Geografico de la CABA") %>% 
  setMaxBounds(lng1 = -58.4311512, lat1 = -34.6038625, lng2 = -58.4689355, lat2 = -34.6483649) #Asignar rectangulo limites de visualizacion con coord de dos de sus vértices
m_prueba2

#agregamos un cítculo a nuestro mapa

m_prueba3 <- leaflet (options = leafletOptions(minZoom = 5, maxZoom = 18)) %>% #Asignar valores de minZoom y maxZoom *18 es el max
  addTiles() %>% 
  addMarkers(lng = -58.445071, lat = -34.616823, popup = "Centro Geografico de la CABA") %>% 
  addCircles(lng = -58.445071, lat = -34.616823, radius = 120, color = "Red", opacity = 1 ) %>% #coords del centro, radio, color y opacidad del borde
  setMaxBounds(lng1 = -58.4311512, lat1 = -34.6038625, lng2 = -58.4689355, lat2 = -34.6483649) #Asignar rectangulo limites de visualizacion
m_prueba3

#Descargamos el shp para poder graficar los espacios culturales en el mapa
#Puede ser desde la página de datos del GCBA o del repositorio de github con el código que sigue:
#https://data.buenosaires.gob.ar/dataset/espacios-culturales/archivo/juqdkmgo-712-resource

download.file("", 
              destfile = "espacios-culturales.zip", mode='wb')
unzip("espacios-culturales.zip", exdir = ".")
file.remove("espacios-culturales.zip")

#Generamos un objeto data_distritos del Shapefile descargado, y preparamos los datos

espacios_shp <- readOGR("espacios.shp", encoding = "UTF-8")
#readOGR nos permite leer un .shp y utilizamos la codificación de caracteres "UTF-8".

espacios_shp <- spTransform(espacios_shp, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#spTransform nos permite asignar al .shp el sistema de coordenadas.(WGS84 suele ser estándar pero puede ocurrir que sea incorrecto) 

##
#Filtramos por sitios de nuestro interes
bares_notables_shp <- espacios_shp %>%
  subset(FUNCION_PR == "BAR" & SUBCATEGOR == "NOTABLE")

###INICIO DE lA FUNCION de MAPEO
m_bares_notables <- leaflet (bares_notables_shp, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>% 
  #Podemos cargar mapas de base alternativos al default de OSM:
  addProviderTiles("Stamen.Toner") %>% 
  #Agregamos marcadores segun los datos que preparamos antes:
  addMarkers(label = ~ESTABLECIM) %>% 
  setView(lng = -58.445071, lat = -34.616823, zoom = 12)
###FIN
m_bares_notables

# que pasa si queremos mapear todos los espacios culturales?

leaflet(espacios_shp)%>%
  #también podemos utilizar las tiles del gcba
  addTiles("https://servicios.usig.buenosaires.gob.ar/mapcache/tms/1.0.0/amba_con_transporte_3857@GoogleMapsCompatible/{z}/{x}/{-y}.png") %>%
  addMarkers(label=espacios_shp@data[["ESTABLECIM"]],
             clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))%>%
  addLayersControl(position = "bottomleft",
                   overlayGroups = espacios_shp@data[["FUNCION_PR"]])
#Dentro de las opciones de addMarkers podemos clusterizar. y luego agregar controles.

#############
#LEAFLET BIS#
#############

cc_espacios<-espacios_culturales_csv %>%
  group_by(barrio)%>%
  summarise(cantidad=n())

cc_espaciosxbarrios <- left_join(mapa_caba, cc_espacios)

#Tambien podemos crear paletas de colores personalizadas, o usar paquetes disponibles. 

pal_barrio <- colorFactor("Reds", domain = cc_espaciosxbarrios$cantidad)

#Creamos etiquetas con los datos que necesitemos mostrar de nuestro dataset, por ejemplo, incluimos la cantidad de espacios en cada barrio
#Creamos un objeto que contenga las etiquetas con HTMLtools. <strong> es para negritas, %s reserva un espacio para el dato y también puedo escribir strings.
etiquetas_dist <- sprintf(
  "<strong>%s</strong><br/>%s Espacios",
  cc_espaciosxbarrios$barrio, cc_espaciosxbarrios$cantidad) %>% 
  lapply(htmltools::HTML)


#Mapeamos
leaflet() %>%
  addTiles("https://servicios.usig.buenosaires.gob.ar/mapcache/tms/1.0.0/amba_con_transporte_3857@GoogleMapsCompatible/{z}/{x}/{-y}.png") %>%
  addPolygons(data = cc_espaciosxbarrios, weight = 2, opacity = 1, smoothFactor = 1, fillOpacity = 0.9,
              color = pal_barrio(cc_espaciosxbarrios$cantidad),label = etiquetas_dist,
              highlightOptions = highlightOptions(color = pal_barrio(pal_barrio(cc_espaciosxbarrios$cantidad)),
                                                  weight = 10, bringToFront = TRUE, fillOpacity = 0.6)) 
  
  
