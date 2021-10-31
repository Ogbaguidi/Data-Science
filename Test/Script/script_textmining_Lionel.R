## les librairies à charger
library(NLP)
library("tm")
library("tidytext")
library("gutenbergr")
library("tidyverse")
lapply(X = c("tm", "tidytext", "gutenbergr", "magrittr", "ggplot2", "dplyr"), 
       FUN =library, character.only = TRUE)

## importation de la base olist_order_reviews_dataset
base=read.csv(file.choose(),header = TRUE,sep = ",")

## construction de la base pour textmining
review=select(base,review_comment_message)

## Remplacement des colonnes vides par les NA
review$review_comment_message[review$review_comment_message==""] <- NA

# Nettoyage de la base
review = review %>% drop_na()

## Suppression des caractères spéciaux de la base review
review = review %>% as.data.frame(str_replace_all(review$review_comment_message,"[^[:alnum:]]"," ")) 

## convertaire les observations en collonnes
review = review %>% rownames_to_column(var = "commentaire")
review %>% as.data.frame()
review1 = select(review,commentaire)

## méthode tidytext
stop_words=stopwords("portuguese")

## Définition des thèmes du graphique
tkrtheme <- theme(plot.title=element_text(margin=margin(0,0,20,0), size=20, hjust = 0.5),
                  plot.subtitle = element_text(margin=margin(0,0,20,0), size = 15, hjust = 0.5),
                  panel.background = element_rect(fill = "white"), 
                  panel.grid.major = element_line(colour = "grey"), 
                  plot.margin = margin(20,50,20,50)) 

##Analyse de fréquence des mots 
tidytext <- review1 %>%
  unnest_tokens(word, commentaire)
clear_wods = tidytext %>% anti_join(get_stopwords("por","snowball")) %>% count(word, sort = TRUE) 


## suppression des mots vides. 
## le mot produto est souvent accompagné de tous les commentaires puisqu'il s'agit 
## d'un commentaire sur les produits achetés. par conséquents nous le supprimons avec les autres 
##  mots vides.
clear_wods = clear_wods[c(-1,-2,-3,-23,-31,-44,-48,-60,-73,-88,-101,-121,-204),]

## représentation graphique des 25 mots les plus frequents avec ggplot2
library(stopwords)
ggplot(clear_wods[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "#E3693E") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots réccurrents dans les commentaire.",
       caption = "textmining") + 
  tkrtheme 

## Nuage des mots 
## les librairies à charger
library(wordcloud2)
library(RColorBrewer)
## le nuage
wordcloud2(clear_wods)
