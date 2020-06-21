library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

library(lda)
library(Matrix)


corpus_an = Corpus(DirSource('~/Desktop/Cours/palais de tokyo/out/16-19'),
                    readerControl = list(reader = readPlain, language = "fr"))
corpus_an
#charger le corpus au nom de "corpus_an"

#http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corpus_an <- tm_map(corpus_an, toSpace, "[[:punct:] ]+")
#couper les phrases en mots 

corpus_an_clean <- tm_map(corpus_an, content_transformer(tolower))#transfer tous en miniscule
corpus_an_clean = tm_map(corpus_an_clean, removeWords, stopwords('fr'))#suprimmer les mots vide en francais
corpus_an_clean = tm_map(corpus_an_clean, removeWords, stopwords('en'))#suprimmer les mots vide en anglais
corpus_an_clean <- tm_map(corpus_an_clean, removeWords, 
                           c("palais", "tokyo","dans","les","entre","cadre","occasion","alors",
                             "très","celles","sous","où","non","telles","autant","toujours","www",
                              "comme","ça","autres","être","deux","plus","new","h","elles","après",
                             "depuis","façon","aussi","numéro","celle","jusqu","quelque","facebook",
                             "infos","personne","formes","point","choses","chaque","notamment",
                             "permettant","plusieurs","avant","pendant","également","propos"
                             ,"ans","hui","aujourd","ainsi","the","http","fois","com","autre",
                             "ème","dont","autour","lors","faire","fait","fond","tout","tous"
                             ,"sein","tels","afin","toute","bien","dès", "agit","etc","là","selon"
                             ,"ceux","chez","tant","celui","puis","feront","certains","plutôt",
                             "delà","donne","avoir","tout","propose","travers")) 
#les liste des mots vide dans le package ne sont pas complètes 
corpus_an_clean <- tm_map(corpus_an_clean, removeWords, 
                          c("michel","charle","avril","août","nicolas","facecbook","twitter"
                            ,"octobre","septembre","jean","rendez","juin","mai", "alain",
                            "camille","mars","alix","anne","yves","alice","laure","ben",
                            "sara","sarah","julie","paul","laurant","emile","adrien","denis",
                            "alexis","cécile","robert","jeudi","lundi","samedi","mardi",
                            "vendredi","mercredi","dimanche","delphine","gilles","février",
                            "décembre","novembre","ensemble","julius","christophe")) 
corpus_an_clean <- tm_map(corpus_an_clean, removeWords, 
                          c("artiste","artistes")) 
#suprimmer les mots selon nos besions de recherches 
corpus_an_clean = tm_map(corpus_an_clean, removeNumbers)
corpus_an_clean <- tm_map(corpus_an_clean, removePunctuation)
corpus_an_clean <- tm_map(corpus_an_clean, stripWhitespace)
#suprimmer les nombres, les ponctuations encore une fois, pour assurer la qualité des matériaux
#corpus_an_clean <- tm_map(corpus_an_clean, stemDocument, "french")
#fusioner les formes singulières et plusieurs par leurs racines 
corpus_an_clean
#corpus nettoyé au nom "corpus_an_clean"

tdm <- TermDocumentMatrix(corpus_an_clean)#une matrice de terms et documents 
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)
print(head(d,100))
#set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, radom.color=FALSE,rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#obtenir le nuage de mots générale

#https://www.tidytextmining.com/topicmodeling.html#word-topic-probabilities
library(topicmodels)
dtm <- DocumentTermMatrix(corpus_an_clean,
                          control = list(wordLengths=c(5, Inf),
                          bounds = list(global = c(3,Inf)),    
                          removeNumbers = TRUE,                
                          weighting = weightTf,
                          encoding = "UTF-8"))
terms <- Terms(dtm)
head(terms)
terms



library(dplyr)
library(tidytext)
ap_td <- tidy(dtm)
ap_td
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
ap_sentiments


#ap_lda <- LDA(dtm, k = 5, control = list(seed = 1234), )
ap_lda <- LDA(dtm, method = "Gibbs", k = 8,  control = list(alpha = 0.1))
ap_lda

t(topics(ap_lda,2))
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()+ 
  labs(x = " ", y = " ", title = "Événements 2012-2015 
(Gibbs sans 'artiste(s)' non racinisation)") 

#l'association entre les mots dans le texte, c'est utile!!
findAssocs(tdm, terms = "malaise", corlimit = 0.3)

#dessiner les nuages de mots selon les termes dans les suejets
topic = 8
words = posterior(ap_lda)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)
library(wordcloud)
wordcloud(names(topwords), topwords)

#Pour voir la qualité du modele, alors, pas très bien.
#Mais mieux pour 2016-2019 que pour tous les huit ans et pour 2012-2015 
perplexity(ap_lda,dtm, use_theta = TRUE,
           estimate_theta = TRUE)

#Pour obtenir les mots de chaque sujets 
sujet_mot <- get_terms(ap_lda,20)
dfsm <- data.frame(sujet_mot)
print.data.frame(dfsm)

#Pour obtenir  les nombres de chaque sujets dans le modèle 
dftopics <- data.frame(get_topics(ap_lda))
colnames(dftopics) <- c("topic")
dftopics %>%
  group_by(topic) %>%
  summarise(
    count=n(),) %>%
  knitr::kable()

#dessiner le diagramme circulaire des sujets
dt = data.frame(A = c(61, 80, 65, 91, 145, 70, 7, 57), 
                Topics = c('1 Atelier(d’exposition)','2 Musique'
                           ,'3 Film','4 Performance (d’exposition)'
                           ,'5 Rencontre','6 Ville','7 Contemporain','8 Fête'))
p = ggplot(dt, aes(x = "", y = A, fill = Topics)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +
  labs(x = "", y = "", title = "") +
  theme(axis.ticks = element_blank())+
  theme(axis.text.x = element_blank())
p  
