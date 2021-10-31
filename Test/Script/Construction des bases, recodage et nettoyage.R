#Chargement des librairies----
library(tidyverse)
library(lubridate)

#Importation des bases de données----
payments <- read_csv("bases/olist_order_payments_dataset.csv")
products <- read_csv("bases/olist_products_dataset.csv")
orders <- read_csv("bases/olist_orders_dataset.csv")
reviews <- read_csv("bases/olist_order_reviews_dataset.csv")
customers <- read_csv("bases/olist_customers_dataset.csv")
sellers <- read_csv("bases/olist_sellers_dataset.csv")
items <- read_csv("bases/olist_order_items_dataset.csv")
geolocation <- read_csv("bases/olist_geolocation_dataset.csv")

#Conception d'une grande Base de travail (Olist)-----
#Traitement de la base items(olist_order_items_dataset)

items1 <- items %>% group_by(across(c(order_id,product_id))) %>%
  filter(order_item_id == max(order_item_id)) %>% ungroup()

#Jointures

olist <-left_join(orders,items1)

olist <- left_join(olist,customers)

olist <- left_join(olist,sellers)

olist <- left_join(olist,products)

olist <- olist %>% relocate (1,2,15:18,3:6,12,8,7,10,22,11,19:21,9,13,14,23:29)

#Traitement de la base olist_order_payments_dataset

payments1 <- payments %>% group_by(order_id) %>% 
                         dplyr:: summarise(order_value = sum(payment_value))

#Codification et création de nouvelles variables utiles pour l'analyse----
#exploratoire

olist$order_purchase_timestamp <- lubridate :: date(olist$order_purchase_timestamp)
olist$order_approved_at <- lubridate :: date(olist$order_approved_at)
olist$order_delivered_carrier_date <- lubridate :: date(olist$order_delivered_carrier_date)
olist$order_estimated_delivery_date <- lubridate :: date(olist$order_estimated_delivery_date)
olist$order_delivered_customer_date <- lubridate :: date(olist$order_delivered_customer_date)
olist$shipping_limit_date <- lubridate :: date(olist$shipping_limit_date)

#Création d'une variable qui renseigne sur le temps d'attente (en nombre de jours)
#entre le moment ou la commande est lancé et le moment elle est approuvé,
#et d'une variable qui renseigne sur l'écart entre la date estimé de la 
#livraison et la date effective de livraison et création d'une variable qui 
#renseigne sur le volume du produit contenu dans une commande

olist <- olist %>% 
  mutate (order_waiting_time_approved = order_approved_at - order_purchase_timestamp,
          delivered_time = order_delivered_customer_date - order_estimated_delivery_date,
          waiting_time_shipping = order_delivered_carrier_date - shipping_limit_date,
          product_capacity_cm3 = product_length_cm * product_height_cm * product_width_cm)

olist <- olist %>% relocate (1:9,30,10,11,32,12,13,31,14:26,33)

olist <- olist %>% select (1:30)

#Création des indicateurs

olist <- olist %>% 
  mutate (delivered_time_indicator=if_else(delivered_time <= 0,"delai respecte"
                                           ,"delai non respecte")) 
olist <- olist %>% relocate (1:16,31)

#quelques modifications pour la statistique descriptive

olist <- olist %>% 
  mutate(customer_state = as.factor(customer_state),
         order_status = as.factor(order_status),
         product_category_name =as.factor(product_category_name),
         seller_state = as.factor(seller_state),
         order_waiting_time_approved = as.numeric (order_waiting_time_approved),
         waiting_time_shipping = as.numeric(waiting_time_shipping),
         delivered_time = as.numeric(delivered_time))

#nous procédons à la scission de la base selon les deux périodes

olist_first <- olist %>% filter( order_purchase_timestamp <= as.Date("2017-09-26"))
olist_second <- olist %>% filter( order_purchase_timestamp > as.Date("2017-09-26"))



