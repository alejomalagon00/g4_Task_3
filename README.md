# Task 3

# En este repositorio se podrá encontrar un archivo .zip con el proyecto de R y su respectivo Script. No obstante, en esta misma parte se pegará el Script elaborado para el desarrollo del Taller 2 de la clase



### John Alejandro Malagon
### 201713658
### Universidad de los Andes
### Taller de R - Task 3
## R version 4.0.5 (2022-03-31)
## Running under: Windows 10 x64 (build 18363)


getwd()
sessionInfo()

#Limpieza del entorno e instalacion de paquetes 
rm(list = ls())
require("pacman")
require("rio")
require("tidyverse")
require("dplyr")
require("broom")
require("sf")
require("osmdata")
require("xm12")
require("rvest")
require("leaflet")
require("ggsn")
require("tmap")
require("stargazer")
require("margins")
require("mfx")
require("modelsummary")
require("XML")


# 1. Datos espaciales
##Para el desarrollo de este punto se escogio a Bogota debido a que se conoce la ciudad relativo a las demas ciudades

# Descarga de datos
# Estaciones de bus
osm = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf()


amenities = osm$osm_points %>% dplyr::select(osm_id,amenity)

# Vias principales y de transporte masivo
calles = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "Autopista") %>%
  osmdata_sf()

street = street$osm_lines %>% dplyr::select(osm_id,name)

# Vias principales que sean de Transmilenio
calles = calles %>%
  subset(str_detect(name,"Transmilenio")==T)

# Limites de los barrios 

# Limites de los barrios
barrio = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = 'admin_level', value = '9') %>% 
  osmdata_sf()

barrio = barrio$osm_multipolygons %>% dplyr::select(osm_id,name,geometry)

# 1.2 Visualizacion de informacion 
# Visualizacion estaciones de bus
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=amenities, weight=1 , col="red")

# Visualizacion de vias principales transmilenio
leaflet() %>%
  addTiles() %>%
  addPolylines(data=street,col="red")

# Visualizacion de limites de barrios o manzanas
tmap::qtm(barrio)


# 1.3 Estimacion de distancias

# Distancia promedio de barrio o manzana hasta estacion
distancia_bus = st_distance(x=barrio, y=amenities)
View(distancia_bus)

# Promedio por barrio de la distancia a cada estacion
mean_bus = apply(distancia_bus,1,mean)

# Guardar el promedio donde esta guarda la informacion de los barrios
barrio$means_bus = mean_bus





# Distancia promedio de barrio/manza hasta vias principales Transmilenio
distancia_vias = st_distance(x=barrio, y=street)
View(distancia_vias)

  # Promedio por barrio de la distancia a cada via principal de Transmilenio
mean_vias = apply(distancia_vias,1,mean)

# Guardar el promedio donde esta guarda la informacion de los barrios
barrio$mean_vias = mean_vias



# Tablas con las estadasticas descriptivas de las variables de distancias
# Promedio de la distancia de las estaciones de bus
sum_bus = summary(barrio$means_bus)
sum_bus
# Tabla con las estadasticas descriptivas para 
# el promedio de la distancia a vias principales del Transmilenio
sum_vias = summary(barrio$mean_vias)
sum_vias

# 1.4 Plot Mapping
# Mapa cuartiles estacion de buses
quantiles_bus=quantile(barrio$means_bus, probs = seq(0, 1, 1/4))
quantiles_bus

# Categorizar los barrios por los cuartiles
barrio <- barrio %>%
  mutate(mean_bus_q = cut(mean_bus, breaks = quantiles_bus,
                         include.lowest = T, dig.lab=5))
# Graficas usando ggplot la informacion por cuartiles
plot_estaciones = ggplot() +
  geom_sf(data = barrio, aes(fill = mean_bs_q), color = "blue", size = 0.25) + 
  scale_fill_brewer(name="Distancia promedio [m]", palette = "YlGn",
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))

# Exportar el grafico a la carpeta output
ggsave(plot=plot_estaciones , filename="Task 3/output/quartiles_estacion_buses.pdf" , width=7, height=9)

# 1.4.2 Mapa cuartiles vias principales de Transmilenio
# Los cuartiles de la variable de promedio de distancia hasta via principal de Transmilenio
quantiles_vias=quantile(barrio$mean_via, probs = seq(0, 1, 1/4))
quantiles_vias

# Categorizar los barrios por los cuartiles
barrio <- barrio %>%
  mutate(mean_vias_q = cut(mean_via, breaks = quantiles_vias,
                           include.lowest = T, dig.lab=5))

# Graficas usando ggplot la informacion por cuartiles
plot_vias = ggplot() +
  geom_sf(data = barrio, aes(fill = mean_vias_q), color = "black", size = 0.25) + 
  scale_fill_brewer(name="Distancia promedio [m]", palette = "YlGnBu",
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))

# Exportar el grafico a la carpeta output
ggsave(plot=plot_vias , filename="Task 3/output/quartiles_vias.pdf" , width=7 , height=9)

# 1.5 Exportacion de datos 
export(barrio,"Task 3/output/barrios.rds") 


# 2. Regresiones
# 2.1 Importacion de datos
datos = import(file = "Task 3/output/f_mapmuse.rds")

# Modelo de regresion con la variables de la base de datos
ols = lm(fallecido ~ 
           as.factor(tipo_accidente)+year+month+as.factor(condicion)
         +as.factor(genero) + as.factor(actividad) + as.factor(cod_mpio)
         +dist_hospi+dist_cpoblado+dist_vias
         ,datos)

# 2.2 Coefplot

# Grafica de los coeficientes
modelplot(ols) + coord_flip()+labs(title="Grafico - Coeficientes de las estimaciones")
db = tidy(ols , conf.int = TRUE)

# Exportacion del grafico de los coeficientes
jpeg("Task 3/output/coeficientes.jpeg")
ggplot(db , aes(x = estimate, y = term)) + theme_light() + 
  geom_vline(aes(xintercept = 0),color="blue",linetype="dashed") + 
  geom_errorbar(width=.5, aes(xmin=conf.low, xmax=conf.high) , col="black" , show.legend = T) + 
  geom_point(size = 3,show.legend = F , col="black") +
  theme(axis.text = element_text(color = "black", size = 10))
dev.off()

# 2.3 Estimacion de modelos de regresion

# Modelo logit
logit = glm(fallecido ~ 
              as.factor(tipo_accidente)+year+month+as.factor(condicion)
            +as.factor(genero) + as.factor(actividad) + as.factor(cod_mpio)
            +dist_hospi+dist_cpoblado+dist_vias
            ,datos, family = binomial(link="logit"))

# Modelo probit
probit = glm(fallecido ~ 
               as.factor(tipo_accidente)+year+month+as.factor(condicion)
             +as.factor(genero) + as.factor(actividad) + as.factor(cod_mpio)
             +dist_hospi+dist_cpoblado+dist_vias
             ,datos,family = binomial(link = "probit"))

# 2.4 Exportacion resultados
stargazer(ols, logit, probit,
          type= 'text',
          dep.var.labels = c('','Fallecido',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('Task 3/output/modelos_de_reg.text'))

# 2.5 Presentar resultados
# Efecto marginal promedio de la distancia hasta un hospital del modelo logit
logit_marg = margins(logit, variables = ("dist_hospital"))

# Efecto marginal promedio de la distancia hasta un hospital del modelo probit
probit_marg = margins(probit, variables = ("dist_hospital"))

# Exportar el grafico efecto marginal promedio de la distancia hasta un hospital del modelo probit
jpeg("Task 3/view/logit_dist_hospital.jpeg")
modelplot(logit_marg,coef_map = ("dist_hospital")) + coord_flip()
dev.off()

# Exportar el grafico efecto marginal promedio de la distancia hasta un hospital del modelo logit
jpeg("Task 3/view/probit_dist_hospital.jpeg")
modelplot(probit_marg,coef_map = ("dist_hospital")) + coord_flip()
dev.off()

# 3. Web-scraping
myurl = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"

# Objeto que contenga el HTML de la pagina
myhtml = read_html(myurl)

# El tipo de objeto que es la variable myhtml
class(myhtml)

# 3.2 Extraer el titulo de la pagina
# Direccion Xpath para obtener el nombre del articulo y convertirlo a texto
texto = myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]/text()') %>% html_text() # Convertir en texto
texto

# 3.3 Extraccion de la tabla que contiene los departamentos de Colombia

tabla = myhtml %>% html_nodes('table') 

departamentos = tabla[4] %>% html_table(header = T,fill=T)  %>% as.data.frame()
