#INTRODUCCION A R

#Objetos en R:
  
A <- 3  #Enteros númericos

B <- A + 1 #Enteros

B #chequeamos b

X <- c("Laboratorio", "de", "Politicas", "Públicas") #strings / characteres

class(A) ### Podemos ver los tipos de objetos
class(B)
class(X)


1:5 #R nos da secuencia de números

sum(1:5) #nos da la suma

mean(1:5) #nos da el promedio


A < B     # Operadores lógicos (<, >, <=, >=, ==, !=)

#Valores, vectores y data.frames :
#Definir df es lo mismo que definir un objeto
#vector
D <- c(1,2,3)

E <- D + 2

G <- c("Carlos","Maria","Juan","Victoria")

elementoG <- G[3]

#######
# El # se usa para hacer comentarios que no impactan en el codigo,
# <- se usa para definir el nombre de un objeto (variable o dataframe)
# R reconoce la diferencia entre mayosculas y minusculas
# "" se usa para hacer referencia a valores que son caracteres
# espacios en blanco y enters son para visualizar mejor (no impacta en el codigo),
# [corchetes] y $ son metodos para hacer referencia a objetos,
# (parentesis) es para definir parametros de una función,
# comas, es para separar parametros dentro de una funcion

#borrar objetos
rm(A,
   B)

## TIDYVERSE

# INSTALAMOS PAQUETES
#es necesario instalar y activar los paquetes. es recomendable hacerlo cada vez
#que se van a usar para actualizarlos

install.packages("tidyverse")
install.packages("plotly")

#Lo activamos
library(tidyverse)
library(plotly)
library(sf)

#Cargamos el data frame con el que vamos a trabajar

#Tenemos dos opciones:
#OPCION 1 (desde internet con el siguiente link):

data <- read.csv("https://github.com/labpoliticasuba/Clases_2020/raw/master/Clase_02/data_final_clase.csv", fileEncoding = "UTF-8")

#OPCION 2 (desde nuestra pc):

### descargamos el archivo, 
### lo ubicamos en la carpeta de nuestro proyecto 
### hacemos click derecho en la carpeta > Propiedades > copiamos y pegamos el path 
### path: dirección donde se encuentra almacenado nuestro csv

data <- read.csv("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/data_final_clase.csv")

### la función head() nos permite ver las primeras 10 observaciones
head(data)

### la función dim() nos permite ver la cantidad de filas y columnas de nuestro dataset 
dim(data)

### la función summary() nos permite ver medidas resumenes de nuestras variables 
summary(data)

### FUNCIONES PARA TRABAJAR CON NUESTRAS COLUMNAS 

#Si queremos ver todos los valores de nuestra columna DISTRITO:
data$DISTRITO

#si queremos ver los nombres de nuestras columnas podemos usar la funcion
colnames(data)

#si queremos saber los valores unicos de una columna
unique(data$DISTRITO)

#modificamos la notacion cientifica
options(scipen = 20)

### funciones en r

  select()        # para seleccionar columnas

  filter()        # para filtrar filas
  
  mutate()        # para agregar o modificar valores de columnas no agrupadas
  
  group_by()      # para agrupar filas
  
  summarise()     # para agregar información de columnnas agrupadas (calcula 1 sólo valor por grupo)


##### ¿Cuál es la provincia más capacitada (de la región pampeana ó que tengan más de 1.5 millones de habitantes) 
####  para enfrentar la emergencia sanitaria en relación a la cantidad de
#####  de médicos c/10.000 habitantes?

data_seleccionados <- select(data, REGION, DISTRITO, POBLACION, CANTIDAD_MEDICOS)

data_filtrado <- filter(data_seleccionados, REGION == 'PAMPEANA' | POBLACION > 1500000)

data_agrupado <-  group_by(data_filtrado, DISTRITO) %>% distinct()

data_summarise  <- data_agrupado %>% summarise( CANT_MEDICOS_CADA_CIENMIL = (CANTIDAD_MEDICOS/POBLACION)*10000)


############## PIPE ( %>% )


Ejercicio_CLASE_pipe <- data %>% 
  select(REGION, DISTRITO, POBLACION, CANTIDAD_MEDICOS) %>%
  filter(REGION == 'PAMPEANA' | POBLACION > 1500000) %>%
  group_by(DISTRITO) %>%
  distinct() %>%
  summarise( CANT_MEDICOS_CADA_CIENMIL = round((CANTIDAD_MEDICOS/POBLACION)*10000),3) %>%
  arrange(desc(CANT_MEDICOS_CADA_CIENMIL)) 


##### GRAFIQUEMOS

p <- ggplot(data=Ejercicio_CLASE_pipe, aes(x=DISTRITO, y=CANT_MEDICOS_CADA_CIENMIL, fill=DISTRITO)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE) +
  xlab("Distrito") +
  ylab("Cantidad de Médicos") + # Set axis labels
  ggtitle("Cantidad de Médicos cada 100 mil habitantes")  +
  geom_text(aes(label=CANT_MEDICOS_CADA_CIENMIL), vjust=1.6, color="black", size=3.5)

p

ggplotly(p) ## si queremos hacer gráficos interactivos
