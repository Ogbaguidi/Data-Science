# Modèle Taux d'attrition----
#importation des bases qui renseignent pour chaque client leur score
client1 <- read.csv("Fichier/first.RFM.csv", header = TRUE,sep = ";")
client2 <- read.csv("Fichier/second.RFM.csv", header = TRUE , sep = ";")
#recodification
client1$groupe <- case_when(client1$Score.total == 2 ~ "1",
                            client1$Score.total == 3 ~ "2",
                            client1$Score.total == 4 ~ "3")
client2$groupe <- case_when(client2$Score.total == 2 ~ "1",
                            client2$Score.total == 3 ~ "2",
                            client2$Score.total == 4 ~ "3")
#importation de la base qui renseigne si les clients de la 
#première période ont achetees a la deuxième période
d <- read.csv("Fichier/taux.csv",header= TRUE, sep = ";")
#recodification
d$renouveler_code <- ifelse(d$Renouveler == "FAUX","0","1")
#jointure
client1 <-left_join(client1,d[,-2])
client1 <- left_join(client1,first.RFM[,-2])
client1 <- left_join(client1,ol_first[,c(-2,-3)],by = c("customer_unique_id" = "rowname"))
client1 <- column_to_rownames(client1,var="customer_unique_id")

client2 <- left_join(client2,second.RFM[,-2])
client2 <- left_join(client2,ol_second[,c(-2,-3)],by = c("customer_unique_id" = "rowname"))
client2 <- column_to_rownames(client2,var="customer_unique_id")

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
attach(d)
cramer.V(client1$renouveler_code,client1$groupe)
cramer.V(client1$renouveler_code,client1$customer_state)
#Impossible de faire une modélisation
#Calcul du taux d'attrition par groupe----
client1 <- as_tibble(rownames_to_column(client1))
nbr_ind1 <- client1 %>% dplyr::count(groupe)
churn.rate <- client1 %>% filter (renouveler_code == "0") 
churn.rate <- churn.rate %>% group_by(groupe) %>% 
                          summarise(nbr.no.renouveler=n())
churn.rate <- left_join(churn.rate,nbr_ind1)
churn.rate <- churn.rate %>% mutate(rate = nbr.no.renouveler/n)
churn.rate <- churn.rate %>% mutate(lifetime = 1/rate)
#Fréquence d'achat moyenne par période et par groupe----
#première période
freq.achat1 <- client1 %>% group_by(groupe) %>%
                          summarise(nbr.achat = sum(frequency))
freq.achat1 <- left_join(freq.achat1,nbr_ind1)
freq.achat1 <- freq.achat1 %>% mutate(mean.freq = nbr.achat/n)
#deuxième période
nbr_ind2 <- client2 %>% dplyr::count(groupe)
freq.achat2 <- client2 %>% group_by(groupe) %>%
  summarise(nbr.achat = sum(frequency))
freq.achat2 <- left_join(freq.achat2,nbr_ind2)
freq.achat2 <- freq.achat2 %>% mutate(mean.freq = nbr.achat/n)
#Valeur client potentielle----
#Première periode
clv1 <- left_join(client1,churn.rate[,-c(2,3)])
clv1 <- left_join(clv1,freq.achat1[,-c(2,3)])
clv1 <- clv1 %>% mutate(clv = monitery*mean.freq*lifetime)
clv1 <- clv1 %>% mutate(ecart = clv - value_client)
#histogramme
ggplot(clv1,aes(clv))+geom_histogram()+facet_wrap(~groupe)
ggplot(clv1,aes(log(clv)))+geom_histogram()+facet_wrap(~groupe)
ggplot(clv1,aes(ecart))+geom_histogram()+facet_wrap(~groupe)
#valeur client moyen
clv1.moyen <- clv1 %>% group_by(groupe) %>%
                    summarise(value.moy.total = sum(monitery*mean.freq))
clv1.moyen <- left_join(clv1.moyen,churn.rate[,-c(2,3,4)])
clv1.moyen <- left_join(clv1.moyen,nbr_ind1)
clv1.moyen <- clv1.moyen %>% mutate(value.moy = value.moy.total/n,
                                    clv = value.moy*lifetime)
#Deuxième periode
clv2 <- left_join(client2,churn.rate[,-c(2,3)])
clv2 <- left_join(clv2,freq.achat2[,-c(2,3)])
clv2 <- clv2 %>% mutate(clv = monitery*mean.freq*lifetime)
clv2 <- clv2 %>% mutate(ecart = clv - value_client)
#histogramme
ggplot(clv2,aes(log(clv)))+geom_histogram()+facet_wrap(~groupe)
ggplot(clv2,aes(ecart))+geom_histogram()+facet_wrap(~groupe)
#valeur client moyen
clv2.moyen <- clv2 %>% group_by(groupe) %>%
  summarise(value.moy.total = sum(monitery*mean.freq))
clv2.moyen <- left_join(clv2.moyen,churn.rate[,-c(2,3,4)])
clv2.moyen <- left_join(clv2.moyen,nbr_ind2)
clv2.moyen <- clv2.moyen%>% mutate(value.moy = value.moy.total/n,
                                    clv = value.moy*lifetime)

