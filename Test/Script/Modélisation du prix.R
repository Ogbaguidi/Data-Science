#Chargement de la librairie et source
source("Script/Construction des bases, recodage et nettoyage.R")
library(ggplot2)
produit <- olist %>% dplyr :: count (product_id)
#produit le plus vendu : 99a4788cb24856965c36a24e339b6058
produit <- olist %>% filter(product_id =="99a4788cb24856965c36a24e339b6058") %>%
                            select (3,6,18:20,23,24:26)  
produit <- produit %>% select(2,3,8,9) %>% relocate(3) 
produit <- produit[,-3]
produit <- as.data.frame(produit)
#nuage de point 
ggplot(produit,aes(price,freight_value))+geom_point()
cor.test(produit$price, produit$freight_value ,method="kendall")
ggplot(produit,aes(price,customer_state))+geom_point()

#Découpons en classe les valeurs de la variable price
cut(produit$price,breaks=2)
produit <- produit %>% 
          mutate (price = if_else(price <= 85,"[74,82[","[82,90["))

ggplot(produit,aes(price,freight_value))+geom_boxplot()
ggplot(produit,aes(freight_value))+geom_density()
produit$price <- factor(produit$price, c("[74,82[","[82,90["), ordered = TRUE)

#Test de Kruskal-Wallis
library(rstatix)
kruskal.test(freight_value ~ price, data =produit)
kruskal_effsize(freight_value ~ price, data =produit)

#V de cramer
cramer.V = function(x, y) {
  # La fonction suivante permet de calculer le V de Cramer
  # Application du test du Chi2 de Pearson
  res <- chisq.test(x, y, correct = FALSE, simulate.p.value=TRUE)
  # Extraction de la statistique de test
  chi2 = as.numeric(res$statistic)
  # Récupération de la taille d’échantillon
  n = length(x)
  # Récupération des éléménets K1 et K2
  p = length(levels(as.factor(x)))
  q = length(levels(as.factor(y)))
  # Calcul du dénominateur du V de Cramér
  a = p-1
  b = q-1
  m = min(a,b)
  # Calcul du V de Cramér
  V = sqrt(chi2/(n * m))
  # Retour du résultat voulu
  return(V)
}

cramer.V(produit$price,produit$customer_state)

#Régression logistique ordinal
library(ordinal)
rego <- clm(price ~ freight_value + customer_state
                        , data = produit, link = "logit")
rego <- clm(price ~ freight_value , data = produit, link = "logit") 
            
summary(rego)
