#Appel des librairies----
library(dplyr)
library(ggplot2)
library(tidyr)
library(fastcluster)
library(factoextra)
library(frequency)
library(gridExtra)
library(grid)
#Chargement du script de la construction des bases de travail-----
source("Script/Construction des bases, recodage et nettoyage.R")
#Segmentation RFM première période----
#Base pour la segmentation
first.RFM <- olist_first %>% select (1,3,7,8) %>% distinct()

first.RFM <- first.RFM %>% filter(order_status != "canceled")

first.RFM <- left_join(first.RFM,payments1)

first.RFM <- first.RFM %>% group_by(customer_unique_id) %>% 
  summarise(recency=as.numeric(as.Date("2018-12-31")-max(order_purchase_timestamp)),
            frequency=n_distinct(order_id), 
            monitery= sum(order_value)/n_distinct(order_id))

#Il y a une valeur manquante au niveau de monitery
first.RFM <- first.RFM %>% 
  mutate(monitery = replace_na(monitery,143.46))

summary(first.RFM)
sd(first.RFM$recency)
sd(first.RFM$frequency)
sd(first.RFM$monitery)

##Histogramme Recence/frequence/Montant
g1 = ggplot(first.RFM,aes(recency)) + geom_histogram(fill = "blue" , stat = "count" ) +
                                              theme_minimal () + labs(x="recency (jours)")

g2 = ggplot(first.RFM,aes(frequency)) + geom_histogram(fill = "blue" ,col = "black",
                                             stat = "count") + theme_minimal ()

g3 = ggplot(first.RFM,aes(monitery)) + 
          geom_histogram(fill = "blue" , col = "black",bins = 200) +
                          labs(x="monitery") + theme_minimal()
                                 

## Normalisation
first.RFM$monitery <- log(first.RFM$monitery)
g4 <-ggplot(first.RFM,aes(monitery)) + 
                        geom_histogram(fill = "blue" , col = "black",bins = 200) +
                        labs(x="log(monitery)") + theme_minimal()

grid.arrange (g1,g2,g3,ncol = 2) 
#Distribution initial de monitery et calcul de la valeur client
first.RFM <- first.RFM %>% mutate ( monitery = exp(monitery),
                                      value_client = monitery*frequency)
summary(first.RFM$value_client)
sd(first.RFM$value_client)
#Exportons la base sous Excel
write.csv(first.RFM,"first.csv")

#Segmentation RFM deuxième période----
second.RFM <- olist_second %>% select (1,3,7,8) %>% distinct()

second.RFM <- second.RFM %>% filter(order_status != "canceled")

second.RFM <- left_join(second.RFM,payments1)

second.RFM <- second.RFM %>% group_by(customer_unique_id) %>% 
  summarise(recency=as.numeric(as.Date("2018-12-31")-max(order_purchase_timestamp)),
            frequency=n_distinct(order_id), 
            monitery= sum(order_value)/n_distinct(order_id))

summary(second.RFM)
sd(second.RFM$recency)
sd(second.RFM$frequency)
sd(second.RFM$monitery)

##Histogramme Recence/frequence/Montant
g4 =ggplot(second.RFM,aes(recency)) + geom_histogram(fill = "blue" , stat = "count" ) +
  theme_minimal () + labs(x="recency (jours)")

g5 = ggplot(second.RFM,aes(frequency)) + geom_histogram(fill = "blue" ,col = "black",
                                                  stat = "count") + theme_minimal ()
g6 =ggplot(second.RFM,aes(monitery)) + 
  geom_histogram(fill = "blue" , col = "black",bins = 200) +
  labs(x="monitery") + theme_minimal()



## Normalisation
second.RFM$monitery <- log(second.RFM$monitery)
g7 = ggplot(second.RFM,aes(monitery)) + 
  geom_histogram(fill = "blue" , col = "black",bins = 200) +
  labs(x="log(monitery)") + theme_minimal()
grid.arrange(g4,g5,g6,g7, ncol = 2)

## Graphique sur les résultats
# change fill and outline color manually 
#recence
ggplot(second.RFM, aes(x = recency, fill = groupe ,color = groupe)) +
  geom_histogram(position = "identity" ,stat = "count") + scale_color_manual(values = c("black", "#E7B800","red","skyblue","green")) +
  scale_fill_manual(values = c("black", "#E7B800","red","skyblue","green"))
#frequence
ggplot(second.RFM, aes(x = frequency, fill = groupe ,color = groupe)) +
  geom_histogram(position = "identity" ,stat = "count") + scale_color_manual(values = c("black", "#E7B800","red","skyblue","green")) +
  scale_fill_manual(values = c("black", "#E7B800","red","skyblue","green"))
#monitery
ggplot(second.RFM, aes(x = monitery, fill = groupe ,color = groupe)) +
  geom_histogram(position = "identity" ,bins = 200) + scale_color_manual(values = c("black", "#E7B800","red","skyblue","green")) +
  scale_fill_manual(values = c("black", "#E7B800","red","skyblue","green"))

#Distribution initial de monitery et calcul de la valeur client
second.RFM <- second.RFM %>% mutate ( monitery = exp(monitery),
                                      value_client = monitery*frequency)
summary(second.RFM$value_client)
sd(second.RFM$value_client)
#Exportons la base sous Excel
write.csv(second.RFM,"second.csv")

