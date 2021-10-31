#Chargement des extensions et des données--------------------
library(FactoMineR)
library(factoextra)
library(fastcluster)
#Chargement du script de la construction des bases de travail-----
source("Script/Construction des bases, recodage et nettoyage.R")
#Statistique descriptive----
#Puisque dans la base Olist, certaines commandes se répètent plusieurs fois à
#cause du nombre d'articles différents contenus en leur sein,nous procédons à 
#une transformation de la base en sélectionnant les variables d'intèrêt et 
#en supprimant les lignes qui se ressemblent pour ne conserver que celle
#disctinct sur laquelle nous ferons les statistiques descriptives
#variables continues

#première période
olist_first %>% select(1,8:17) %>% distinct() %>% summary()

#deuxième période
olist_second %>% select(1,8:17) %>% distinct() %>% summary()

#variables qualitatives 
#première période
olist_first %>% 
  mutate(delivered_time_indicator = as.factor(delivered_time_indicator)) %>% 
           select(1,6,7,11) %>% distinct() %>% summary()
olist_first %>% 
  select(product_id,product_category_name) %>% summary()
#deuxième période
olist_second %>% 
  mutate(delivered_time_indicator = as.factor(delivered_time_indicator)) %>% 
  select(1,6,7,11) %>% distinct() %>% summary()
olist_second %>% 
  select(product_id,product_category_name) %>% summary()

#Payment_type
#premiere periode
payment.type1 <- olist_first %>% select(1) %>% distinct()
payment.type1 <- left_join(payment.type1,payments[,c(1,3)])                                 
payment.type1 %>% dplyr :: count (payment_type)
#premiere periode
payment.type2 <- olist_second %>% select(1) %>% distinct()
payment.type2 <- left_join(payment.type2,payments[,c(1,3)])                                 
payment.type2 %>% dplyr :: count (payment_type)

#Conception d'une base client pour la première période----
#a utilisé dans la classification
ol_first <- left_join(olist_first,payments1) 

ol_first <- ol_first %>% select(1,3,32)

ol_first <- ol_first %>% 
  mutate(order_value = replace_na(order_value,143.46))

ol_first <- ol_first %>% distinct() %>% 
                       group_by(customer_unique_id) %>%
                       dplyr::summarise(order_client =sum(order_value),
                       nbr_order = n_distinct(order_id))

ol_customer <- olist_first %>% select (1,3,6,8)

#Puisqu'il existe des clients ayant lancés des commandes à travers plusieurs
#états, nous avons donc décidé de considérer l'état du brésil ou la dernière 
#commande de ces clients fut lancés

ol_customer <- ol_customer %>% distinct () %>% group_by(customer_unique_id) %>%
  filter(order_purchase_timestamp == max(order_purchase_timestamp)) 

ol_customer <- ol_customer %>% select(2,3) %>% distinct()

ol_customer <- ol_customer[!(ol_customer$customer_unique_id =="2410195f6521688005612363835a2671" & ol_customer$customer_state == "RS"),] 
ol_customer <- ol_customer[!(ol_customer$customer_unique_id =="818a54422350b38d3fb1cb3a1553889f" & ol_customer$customer_state == "RJ"),] 

ol_first <- left_join (ol_first, ol_customer) 

ol_first = as.data.frame(column_to_rownames(ol_first,
                                            var = "customer_unique_id"))
  
#Conception d'une base client pour la seconde période----
#a utilisé dans la classification
ol_second <- left_join(olist_second,payments1) 

ol_second <- ol_second %>% select(1,3,32)

ol_second <- ol_second %>% distinct() %>% 
  group_by(customer_unique_id) %>%
  dplyr::summarise(order_client =sum(order_value),
                   nbr_order = n_distinct(order_id))

ol_customer <- olist_second %>% select (1,3,6,8)

#Puisqu'il existe des clients ayant lancés des commandes à travers plusieurs
#états, nous avons donc décidé de considérer l'état du brésil ou la dernière 
#commande de ces clients fut lancés

ol_customer <- ol_customer %>% distinct () %>% group_by(customer_unique_id) %>%
  filter(order_purchase_timestamp == max(order_purchase_timestamp)) 

ol_customer <- ol_customer %>% select(2,3) %>% distinct()

ol_customer <- ol_customer[!(ol_customer$customer_unique_id =="67806996190d3af60247f64e1d03877f" & ol_customer$customer_state == "MG"),] 

ol_customer <- ol_customer[!(ol_customer$customer_unique_id =="8b433c0e7b7d2a1f1148b4277cc673f8" & ol_customer$customer_state == "SC"),] 

ol_customer <- ol_customer[!(ol_customer$customer_unique_id =="b531620286c6c6fbd1d3e7b58390fec0" & ol_customer$customer_state == "MG"),] 

ol_second <- left_join (ol_second, ol_customer) 

ol_second = as.data.frame(column_to_rownames(ol_second,
                                             var = "customer_unique_id"))

#Analyse factorielle Première période----

first.pca <- PCA(ol_first, scale.unit =TRUE, graph = FALSE, quali.sup = 3)

str(first.pca)

## Extraction valeurs propres et variance

get_eig(first.pca)

## Pour faire le graphe

fviz_screeplot(first.pca, addlabels = TRUE, ylim = c(0, 70))

## pour extraire le rsultat des variables
var.first <- get_pca_var(first.pca)
var.first

##   ANALYSE SUR LES VARIABLES

## Coordonées des variables
head(var.first$coord)

## Contribution des variables

head(var.first$contrib)

## Avec plus de couleur par contribution 

p12 <- fviz_pca_var(first.pca, col.var="contrib",axes = c(1,2),
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE )
p12

##ANALYSE SUR LES INDIVIDUS

ind <- get_pca_ind(first.pca)
ind
head(ind$coord)
head(ind$contrib)
fviz_pca_ind(first.pca, col.ind = "cos2", ## qualité des individus sur le plan factoriel
             repel = TRUE,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             
)
## superposition des individus et des variables

fviz_pca_biplot(first.pca, repel = TRUE)

#Analyse factorielle seconde période----

second.pca <- PCA(ol_second,graph = FALSE, quali.sup = 3)

str(second.pca)

## Extraction valeurs propres et variance

get_eig(second.pca)

## Pour faire le graphe

fviz_screeplot(second.pca, addlabels = TRUE, ylim = c(0, 70))

## pour extraire le rsultat des variables
var.second <- get_pca_var(second.pca)
var.second

##   ANALYSE SUR LES VARIABLES

## Coordonées des variables
head(var.second$coord)

## Contribution des variables

head(var.second$contrib)

## Avec plus de couleur par contribution 

p12 <- fviz_pca_var(second.pca, col.var="contrib",axes = c(1,2),
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE)
p12
                    
##ANALYSE SUR LES INDIVIDUS

ind <- get_pca_ind(second.pca)
ind
head(ind$coord)
fviz_pca_ind(second.pca, col.ind = "cos2", ## qualité des individus sur le plan factoriel
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # évite le chevauchement des textes
)
## superposition des individus et des variables

fviz_pca_biplot(second.pca, repel = TRUE)

#Classification hiérachique première période ----

first.cah <- hclust.vector(ol_first[,-3],method = "ward", 
                            metric = "euclidean")
#Dendogramme
plot(first.cah, labels = FALSE, main = "Dendrogramme")
#inertie
inertie <- sort(first.cah$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3, 4), inertie[c(2, 3, 4)], col = c("green3", "red3", "blue3"), 
       cex = 2, lwd = 3)

#Construction des groupes

grp.cah <- cutree(first.cah, k = 3)  

grp.cah <-as.data.frame(grp.cah)

grp.cah <- as_tibble(rownames_to_column(grp.cah))

grp.cah <- rename(grp.cah, customer_unique_id = rowname, grp = grp.cah )

ol_first <- as_tibble(rownames_to_column(ol_first))

grp.cah <- left_join (grp.cah, ol_first,
                      by = c("customer_unique_id" = "rowname"))

grp.cah <- grp.cah %>% mutate (grp = as.factor(grp))

grp.cah %>% dplyr :: count(grp) 

#Méthode Kmeans pour la seconde période----
##Détermination d'un nombre optimale de classe pour la méthode Kmeans
set.seed(123)
wss <- function(k) {
  kmeans(ol_second[,-3], k)$tot.withinss
}
k.values <- 1:10
wss_values <- map_dbl(k.values, wss)
wss_values
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Nombre de classe K",
     ylab="Variation totale intraClasse")

##A la lecture le nombre de classe optimale est de 4

k3 <- kmeans(ol_second[,-3], centers = 3, nstart = 10)

# graphs

fviz_cluster(k3, geom = "point",  data = ol_second[,-3]) + ggtitle("k = 3")

#Groupe

#Construction des groupes

grp.kmeans <- k3[["cluster"]]

grp.kmeans <-as.data.frame(grp.kmeans)

grp.kmeans <- as_tibble(rownames_to_column(grp.kmeans))

grp.kmeans <- rename(grp.kmeans, customer_unique_id = rowname, groupe = grp.kmeans )

ol_second <- as_tibble(rownames_to_column(ol_second))

grp.kmeans <- left_join (grp.kmeans, ol_second,
                      by = c("customer_unique_id" = "rowname"))

grp.kmeans %>% dplyr :: count(groupe) 



