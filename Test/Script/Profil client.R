#chargement source
source("Script/Statistique descriptive et classification.R")
#Statistique descriptives quantitative----
#Premiere periode
grp.cah %>% group_by(grp)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),min)
grp.cah %>% group_by(grp)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),median)
grp.cah %>% group_by(grp)%>%
            dplyr :: summarise_at( c("order_client","nbr_order"),mean)
grp.cah %>% group_by(grp)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),max)  
grp.cah %>% group_by(grp)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),sd)
grp.cah %>% group_by(grp)%>%
  dplyr :: summarise( montant = sum(order_client))

#deuxieme periode
grp.kmeans %>% group_by(groupe)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),min)
grp.kmeans %>% group_by(groupe)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),median)
grp.kmeans %>% group_by(groupe)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),mean)
grp.kmeans %>% group_by(groupe)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),max)  
grp.kmeans %>% group_by(groupe)%>%
  dplyr :: summarise_at( c("order_client","nbr_order"),sd)
grp.kmeans %>% group_by(groupe)%>%
  dplyr :: summarise( montant = sum(order_client))

#Statistique descriptives qualitative----
tcd <- as.data.frame(table(grp.cah$grp,grp.cah$customer_state))
#Groupe 1
tcd %>% filter(Var1 == "1") %>% mutate(prop = Freq/sum(Freq)) %>%
        arrange(desc(prop))
tcd <- as.data.frame(table(grp.kmeans$groupe,grp.kmeans$customer_state))
#Groupe 1
tcd %>% filter(Var1 == "1") %>% mutate(prop = Freq/sum(Freq)) %>%
  arrange(desc(prop))

