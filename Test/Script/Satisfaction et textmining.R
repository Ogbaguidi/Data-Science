#Chargement package----
library(NLP)
library("tm")
library("tidytext")
library("gutenbergr")
library("tidyverse")
lapply(X = c("tm", "tidytext", "gutenbergr", "magrittr", "ggplot2", "dplyr"), 
       FUN =library, character.only = TRUE)
library(stopwords)
library(dplyr)
library(gridExtra)
source("Script/Construction des bases, recodage et nettoyage.R")
#Analyse de la satisfaction Première période----

satisf.first <- olist_first %>% select (1,7,17) %>% distinct()
#filtre pour choisir par commande la note du dernier commentaire créer. 
#Puisqu'il y a que les dates de création de commentaire certains commentaires
#furent crées le même jour et pour les départager, on choisira la note maximale
reviews.first <- reviews %>% group_by(order_id) %>%
  filter (review_creation_date == max(review_creation_date) & 
            review_score == max(review_score)) %>% ungroup()
#jointure pour créer une base pour l'analyse
satisf.first <- left_join(satisf.first,reviews.first[,-c(1,4:7)])
satisf.first <- satisf.first %>% distinct ()
#mise en factor des notes (scores) afin de faire une aanalyse dexcriptive
satisf.first <- satisf.first %>% mutate(review_score = as.factor(review_score))
satisf.first <- satisf.first %>% filter(!is.na(review_score))
table(satisf.first$review_score)
b = table(satisf.first$delivered_time_indicator , satisf.first$review_score)
capture.output(b,file = "satisf.xls")

#Analyse de la satisfaction Deuxième période----
satisf.second <- olist_second %>% select (1,7,17) %>% distinct()
satisf.second <- left_join(satisf.second,reviews.first[,-c(1,4:7)])
satisf.second <- satisf.second %>% distinct ()
satisf.first <- satisf.first %>% mutate(review_score = as.factor(review_score))
satisf.first <- satisf.first %>% filter(!is.na(review_score))
table(satisf.second$review_score)
b = table(satisf.second$delivered_time_indicator , satisf.second$review_score)
capture.output(b,file = "satisf.xls")

#Textmining----
## construction de la base pour textmining
reviews1 <- reviews %>% select (2,5)

#Séparation de la base en deux; un pour la première période
#et l'autre pour la deuxième période
reviews1.first <- left_join(satisf.first[,-c(2,3,4)],reviews1)
reviews1.second <- left_join(satisf.second[,-c(2,3,4)],reviews1)
# Nettoyage de la base
reviews1.first <- reviews1.first %>% drop_na()
reviews1.second <- reviews1.second %>% drop_na()

## Suppression des caracteres speciaux de la base review
reviews1.first <-reviews1.first %>% 
              as.data.frame(str_replace_all(reviews1$review_comment_message,"[^[:alnum:]]"," ")) 
reviews1.second <-reviews1.second %>% 
  as.data.frame(str_replace_all(reviews1$review_comment_message,"[^[:alnum:]]"," ")) 

## convertir les observations en colonnes
reviews1.first <- reviews1.first %>% 
                        rownames_to_column(var = "commentaire")
reviews1.first %>% as.data.frame()
text.1 <- select(reviews1.first,3)

reviews1.second <- reviews1.second %>% 
  rownames_to_column(var = "commentaire")
reviews1.second %>% as.data.frame()
text.2 <- select(reviews1.second,3)

## methode tidytext
stop_words=stopwords("portuguese")
stop_words <- as.data.frame(stop_words)

## Definition des themes du graphique
tkrtheme <- theme(plot.title=element_text(margin=margin(0,0,20,0), size=20, hjust = 0.5),
                  plot.subtitle = element_text(margin=margin(0,0,20,0), size = 15, hjust = 0.5),
                  panel.background = element_rect(fill = "white"), 
                  panel.grid.major = element_line(colour = "grey"), 
                  plot.margin = margin(20,50,20,50)) 

##Analyse de frequence des mots 
#Premiere periode
tidytext.1 <- text.1 %>%
  unnest_tokens(word, review_comment_message)
clear_words.1 <- tidytext.1 %>% 
              dplyr :: anti_join(stop_words, by = c("word" ="stop_words")) %>% 
              dplyr :: count(word, sort = TRUE) 
#deuxième periode
tidytext.2 <- text.2 %>%
  unnest_tokens(word, review_comment_message)
clear_words.2 <- tidytext.2 %>% 
  dplyr :: anti_join(stop_words, by = c("word" ="stop_words")) %>% 
  dplyr :: count(word, sort = TRUE) 


## suppression des mots vides. 
# le mot produto est souvent accompagne de tous les commentaires 
#puisqu'il s'agit d'un commentaire sur les produits achetes
#par consequents nous le supprimons avec les autres mots vides.
clear_words.1 <- clear_words.1[-c(1,9,54,79,88,110,266,267),]

clear_words.2 <- clear_words.2[-c(1,7,36,50,87,116,157,173,179,196,199),]


## repr?sentation graphique des 25 mots les plus frequents avec ggplot2
library(stopwords)
n1 = ggplot(clear_words.1[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots recurrents dans les commentaires",
       caption = "textmining Premiere Periode") + 
  tkrtheme 

n2 =ggplot(clear_words.2[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots recurrents dans les commentaires",
       caption = "textmining Deuxieme Periode") + 
  tkrtheme 

grid.arrange(n1,n2,ncol = 2)

## Nuage des mots 
## les librairies a charger
library(wordcloud2)
library(RColorBrewer)
## le nuage
wordcloud2(clear_words.1)
wordcloud2(clear_words.2)
