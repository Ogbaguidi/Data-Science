#Chargement du script de la construction des bases de travail-----
source("Script/Construction des bases, recodage et nettoyage.R")
library(rgdal)
library(sf)
library(mapview)
library(tmap)
tmap_mode(mode = "plot")
library(sp)
library(leaflet)

#Construction d'une base des vendeurs sur l'effectif par état brésilien----

geo_seller <- sellers %>% 
  dplyr :: count(seller_state) %>%
  mutate(pourcentage_de_vendeurs = n/sum(n)*100) %>%
  arrange (desc(n)) 
geo_seller <- geo_seller %>% 
  dplyr :: rename(nombre_de_vendeurs = n)

#creation pour la 1 periode d'une base qui renseigne par etat le CA ----
#et le nombre de client

geo.first <- olist %>% 
  filter(order_status != "canceled" & order_purchase_timestamp <= as.Date("2017-09-26")) %>% 
  select (1,3,6) %>% distinct () 

geo.first <- left_join(geo.first,payments1)

geo.first <- geo.first %>% 
            mutate(order_value = replace_na(order_value,143.46))

##Construction d'une base qui renseigne sur l'effectif, la répartition (%) 
##de clients et le chiffres d'affaire par état brésilien
#première période

geo.first <- geo.first %>% group_by(customer_state) %>% 
  dplyr :: summarise(nbr_client = n_distinct(customer_unique_id),
                     CA.first = sum(order_value)) %>%
  mutate(percent.first = nbr_client / sum(nbr_client)*100) %>%
  arrange (desc(nbr_client)) %>% relocate (1,2,4,3)

#creation pour la periode 2 d'une base qui renseigne par etat le CA ----
#et le nombre de client

geo.second <- olist %>% 
  filter(order_status != "canceled" & order_purchase_timestamp > as.Date("2017-09-26")) %>% 
  select (1,3,6) %>% distinct () 

geo.second <- left_join(geo.second,payments1)

##Construction d'une base qui renseigne sur l'effectif, la répartition (%) 
##de clients et le chiffres d'affaire par état brésilien
#première période

geo.second <- geo.second %>% group_by(customer_state) %>% 
  dplyr :: summarise(nbr_client = n_distinct(customer_unique_id),
                     CA.second= sum(order_value)) %>%
  mutate(percent.second = nbr_client / sum(nbr_client)*100) %>%
  arrange (desc(nbr_client)) %>% relocate (1,2,4,3)

#Construction d'une base ----

geo.first <- geo.first %>% select(1,3,4)
geo.second <- geo.second %>% select (1,3,4)
geo <- left_join(geo.first,geo.second)
geo <- left_join(geo,geo_seller, by=c("customer_state"="seller_state"))

#Recodage des modalités de la variable customer_state-----

geo <- geo %>%  mutate(customer_state = fct_recode(customer_state,
                                                   "Acre"="AC","Amazonas"="AM","Bahia"="BA","Ceara"="CE",
                                                   "Distrito Federal"="DF","Goias"="GO","Espirito Santos"="ES",
                                                   "Maranhao"="MA","Mato Grosso do Sul"="MS","Mato Grosso"="MT",
                                                   "Minas Gerais"="MG","Para"="PA","Paraiba"="PB","Parana"="PR",
                                                   "Pernambuco"="PE","Piaui"="PI","Rio de Janeiro"="RJ",
                                                   "Rio Grande do Norte"="RN","Rio Grande do Sul"="RS",
                                                   "Rondonia"="RO","Sao Paulo"="SP","Santa Catarina"="SC",
                                                   "Sergipe"="SE","Alagoas"="AL","Amapa"="AP","Roraima"="RR",
                                                   "Tocantins"="TO"))
geo <- rename (geo ,NAME_1 = customer_state)
#Importation de la couche relatif au Brésil-----------------------------------
##lecture de l'encodage du fichier shp
guess_encoding("BRA_adm/BRA_adm1.shp")

##Importation du fichier shp
shp <- readOGR(dsn = 'BRA_adm',
               layer = 'BRA_adm1')

##Recodage des modalités
shp@data[["NAME_1"]] <- fct_recode(shp@data[["NAME_1"]] ,
                     "Ceara" ="CearÃ¡","Goias"="GoiÃ¡s","Sao Paulo"="SÃ£o Paulo",
                      "Espirito Santos"="EspÃ­rito Santo","Rondonia"="RondÃ´nia",
                      "Maranhao"="MaranhÃ£o","Para"="ParÃ¡","Amapa"="AmapÃ¡",
                       "Paraiba"="ParaÃ­ba","Parana"="ParanÃ¡","Piaui"="PiauÃ­")

##jointure de la base au fichier shp

shp@data = left_join(shp@data,geo)

##Construction de la carte de répartition des vendeurs par état----
  tm_shape(shp) + 
  tm_fill(col = "nombre_de_vendeurs", id="NAME_1", alpha = 1 ,
          legend.format = list(format = "f", big.num.abbr = NA),
          legend.position = c("right", "top"),legend.outside=FALSE,
          palette = "YlOrBr",title = "Répartition des vendeurs")+
  tm_text ("NAME_1", size = 0.65) + 
  tm_borders("black", alpha = 0.5) +
  tm_style("white", frame.lwd = 0) + 
  tm_format("NLD")

##Construction de la carte de répartition des clients par état-----
#première periode
tm_shape(shp) + 
  tm_fill(col = "percent.first", id="NAME_1", alpha = 1 ,
          legend.format = list(format = "f", big.num.abbr = NA),
          palette = "YlOrBr",title = "Répartition des clients (%)")+
  tm_text ("NAME_1", size = 0.55) + 
  tm_borders("black", alpha = 0.5) +
  tm_style("white", frame.lwd = 5) + 
  tm_format("NLD", title="Premiere Periode",
            title.position = c("right", "bottom"))

#deuxième periode
tm_shape(shp) + 
  tm_fill(col = "percent.second", id="NAME_1", alpha = 1 ,
          legend.format = list(format = "f", big.num.abbr = NA),
          palette = "YlOrBr",title = "Répartition des clients (%)")+
  tm_text ("NAME_1", size = 0.55) + 
  tm_borders("black", alpha = 0.5) +
  tm_style("white", frame.lwd = 5) + 
  tm_format("NLD", title="Deuxième Periode",
            title.position = c("right", "bottom"))

#Répartition du Chiffres d'affaires par region----------------------
#Première periode
g1 = tm_shape(shp) + 
  tm_fill(col = "CA.first", id="NAME_1", alpha = 1 ,
          legend.format = list(format = "f", big.num.abbr = NA),
          palette = "YlOrBr",title = "Chiffres d'Affaires")+
  tm_text ("NAME_1", size = 0.50) + 
  tm_borders("black", alpha = 0.75) +
  tm_style("white", frame.lwd = 5) + 
  tm_format("NLD", title="Premiere Periode",
            title.position = c("right", "top"))
g1
#Seconde periode
g2 = tm_shape(shp) + 
     tm_fill(col = "CA.second", id="NAME_1", alpha = 1 ,
             legend.format = list(format = "f", big.num.abbr = NA),
             palette = "YlOrBr",title = "Chiffres d'Affaires")+
     tm_text ("NAME_1", size = 0.50) + 
  tm_borders("black", alpha = 0.70) +
  tm_style("white", frame.lwd = 5) + 
  tm_format("NLD", title="Deuxième Periode",
              title.position = c("right", "top") )
g2    
tmap_arrange( g1, g2)




