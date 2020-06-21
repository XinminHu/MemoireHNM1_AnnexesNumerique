list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2019')
list_an
df2019 <- data.frame(list_an, row.names = NULL)
df2019['year'] <- 2019

list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2018')
list_an
df2018 <- data.frame(list_an, row.names = NULL)
df2018['year'] <- 2018

list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2017')
list_an
df2017 <- data.frame(list_an, row.names = NULL)
df2017['year'] <- 2017

list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2016')
list_an
df2016 <- data.frame(list_an, row.names = NULL)
df2016['year'] <- 2016

list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2015')
list_an
df2015 <- data.frame(list_an, row.names = NULL)
df2015['year'] <- 2015

list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2014')
list_an
df2014 <- data.frame(list_an, row.names = NULL)
df2014['year'] <- 2014

list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2013')
list_an
df2013 <- data.frame(list_an, row.names = NULL)
df2013['year'] <- 2013

list_an = list.files('~/Desktop/Cours/palais de tokyo/out/years/2012')
list_an
df2012 <- data.frame(list_an, row.names = NULL)
df2012['year'] <- 2012

df1912 <- rbind(df2019,df2018,df2017,df2016,df2015,df2014,df2013,df2012)
write.csv(df1912,file = "~/Desktop/Cours/palais de tokyo/years.csv")
write.csv(topics,file = "~/Desktop/Cours/palais de tokyo/topics.csv")

dfyears <- read.csv("~/Desktop/Cours/palais de tokyo/dfyears.csv")
dftopics <- read.csv("~/Desktop/Cours/palais de tokyo/dftopics.csv")
library(dplyr)
dfyeartopic <- inner_join(dfyears,dftopics)
dfyeartopic <- dfyeartopic[,-1]
dfyeartopic <- dfyeartopic[,-3]
names(dfyeartopic)[2] <- "year"
names(dfyeartopic)[3] <- "topic"
dfyeartopic
write.csv(dfyeartopic, file = "~/Desktop/Cours/palais de tokyo/yeartopics.csv")

str(dfyeartopic)

#dessiner le diagramme pour montre la répétition de sujet par ans
dfyeartopic_yearly <- dfyeartopic %>% group_by(year)
dfyeartopic_yearly <- dfyeartopic_yearly[,-1]
ggplot(dfyeartopic_yearly,aes(x=year)) + 
  geom_bar(aes(fill=factor(topic))) +
  geom_text(aes(label=..count..),
            stat="count", color = "black", 
            vjust = -0.5, size = 3)+
  labs(title = "Sujets par ans", x = "Année", y = "Sujets", color = "Legend Title\n")+
  scale_fill_discrete(labels = c('1 Atelier(d’exposition)','2 Musique'
             ,'3 Film','4 Performance (d’exposition)'
             ,'5 Rencontre','6 Ville','7 Contemporain','8 Fête'))
  
#en vie de dessiner le diagramme par la proportion, mais pas réussit
ggplot(dfyeartopic_yearly,aes(x=class,y=..prop..)) + 
  geom_bar(aes(fill=factor(topic)),position="fill") +
  geom_text(aes(label=..count..),
            stat="count", color = "black", 
            vjust = -0.5, size = 3)+
  labs(title = "Sujets par ans", x = "Année", y = "Sujets", color = "Legend Title\n")+
  scale_fill_discrete(labels = c('1 Atelier(d’exposition)','2 Musique'
                                 ,'3 Film','4 Performance (d’exposition)'
                                 ,'5 Rencontre','6 Ville','7 Contemporain','8 Fête'))

