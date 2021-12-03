library(academictwitteR)
library(stringr)
library(dplyr)
options(scipen=999)
setwd("~/Tesis/Notebooks/DB_finales/")


load("prueba_red.RData")

#importar datos de twitter
data<-bind_tweets("DATA/data_181_190/", verbose = TRUE, user = FALSE)
user_data<-bind_tweets("DATA/data_181_190/", verbose = TRUE, user = TRUE)

#importar base de datos de usuarios identificados

dBderecha<-read.csv("./cuentasDerechaDB.csv", header = T, sep=",", encoding = "UTF-8", stringsAsFactors =FALSE)

ids_users_derecha<-dBderecha$id
ids_users_derecha<-as.character(ids_users_derecha)

#entender cómo funciona la base
mention<-data$entities$mentions[[6]] #MENCIONESSSSS !! 

mention$id
data$text[6]

data$entities$mentions[[10]]
      
data$referenced_tweets[6]  
data[10,]
data$author_id[1]
user_data$name[1]
user_data$id[1]

#listas vacía para crear data.frame
tipo_mencion<-c()
source<-c()
target<-c()
day<-c()

data$referenced_tweets[2][[1]]

#for loop para crear las listas
#ESTO TIENE EL EROOR QUE MIRA LOS IDS DE LOS TWEETS --> 
for (i in 1:nrow(data)) {
  temp<-nrow(data$referenced_tweets[i][[1]])
  if (is.integer(temp)) {
    for(k in 1:temp){
      tipo_mencion<-c(tipo_mencion,data$referenced_tweets[i][[1]]$type[k])
      source<-c(source,data$author_id[i])
      target<-c(target,data$referenced_tweets[i][[1]]$id[k] )
      day<-c(day,data$created_at[i] )
    }
    
  }
  
}

#crear base de aristas 
red<-do.call(rbind, Map(data.frame, tipo_mencioncol=tipo_mencion, sourcecol=source, targetcol=target, daycol=day))


library(dplyr)
#filter only in dBderecha
red2<-filter(red, targetcol %in% ids_users_derecha)

typeof(red$targetcol)
typeof(ids_users_derecha)

dBderecha$screen_name[dBderecha$id==red$sourcecol[25145]]




mention[1]

data<-dB_temporal

nrow(referencia)
nrow(data)

data$referenced_tweets[1][[1]]
mentions<-c()
source<-c()
id<-c()
day<-c()

for (n in 1:nrow(data)) {
  
  mention<-data$entities$mentions[[n]]
  nrmention<- nrow(mention)
  
  if (is.integer(nrmention)) {
    
    for (nr in 1:nrmention) {
      temp_mention<- mention$id[nr]
      if(temp_mention %in% ids_users_derecha){
        mentions<-c(mentions, temp_mention)
        source<-c(source,data$author_id[n])
        day<-c(day,data$created_at[n] )
        id<-c(id, data$id[n])
      }
    }
    

  }
  
}

red_mencion<-do.call(rbind, Map(data.frame, tweetaid=id, tipo_mencioncol="mencion", sourcecol=source, targetcol=mentions, daycol=day))
red_mencion2<-filter(red_mencion, !sourcecol == targetcol)

gonorreq<-dB_temporal$referenced_tweets[dB_temporal$id==red_mencion$tweetaid[50]][[1]]$id

gonorreq == red_mencion$tweetaid[50]


dBderecha$screen_name[dBderecha$id== red_mencion2$targetcol[1235]]





### menciones todos los archivos 
setwd("~/Tesis/Notebooks/DB_finales/DATA")


files<-list.files()


directory="~/Tesis/Notebooks/DB_finales/DATA"

mentions<-c()
source<-c()
day<-c()
tweet_id<-c()

hola<-c(1)
hola<-c(2,hola)
hola

for (file in files){
  directory_temp=paste(directory, file, sep="/")
  setwd(directory_temp)
  number<-length(list.files())
  if (number>1) {
    setwd(directory)
    dB_temporal<-bind_tweets(file, verbose = TRUE, user=FALSE)
    
    for (n in 1:nrow(dB_temporal)) {
      
      mention<-dB_temporal$entities$mentions[[n]]
      nrmention<- nrow(mention)
      
      tweetid<-dB_temporal$id[n]
      
      if (is.integer(nrmention)) {
        
        for (nr in 1:nrmention) {
          temp_mention<- mention$id[nr]
          if(temp_mention %in% ids_users_derecha){
            mentions<-c(mentions,temp_mention)
            source<-c(source, dB_temporal$author_id[n])
            day<-c(day,dB_temporal$created_at[n] )
            tweet_id<-c(tweet_id, tweetid)
        
          }
          
        }
        
        
      }
      
    }
    
    
  }
  setwd(directory)
}

x<-length(unique(tweet_id))

save(mentions, source, day, tweet_id, file = "listado_parared.RData")

red_mencion<-do.call(rbind, Map(data.frame, sourcecol=source, targetcol=mentions, daycol=day, tweet_id=tweet_id))

red_mencion<-filter(red_mencion, !targetcol == sourcecol)

red_mencion$parafiltrar<-paste(red_mencion$sourcecol, red_mencion$targetcol, red_mencion$tweet_id, sep="")
red_mencion<- distinct(red_mencion, tweet_id, .keep_all = TRUE)

save(red_mencion, file="../dB_aristas.RData")

time<-str_split_fixed(red_mencion$daycol, "T", 2)[,2]
time<-gsub('.{1}$','',time)
date<-str_split_fixed(red_mencion$daycol, "T", 2)[,1]


red_mencion$dia<-date
red_mencion$tiempo<-time

red_mencion$datetime<-paste(red_mencion$dia, red_mencion$tiempo, sep=" ")

red_mencion$datetime<-as.POSIXlt(red_mencion$datetime)

red_mencion<-red_mencion[,c(-4,-5,-6)]

mentions<- unique(mentions)
sources<- unique(prueba_red_mencion$sourcecol)
targets<-unique(prueba_red_mencion$targetcol)

users<-unique(c(sources,targets))

nodes<-filter(dBderecha, id_str %in% users)

prueba_red_mencion<-distinct(red_mencion, sourcecol, targetcol, .keep_all= TRUE)
prueba_red_mencion<-prueba_red_mencion[,c(-1)]



users<-c(mentions,source) 
users<-unique(users)

red_mencion27n<-filter(red_mencion, daycol == n27)
red_mencion$daycol<-format(as.Date(red_mencion$daycol, format="%d/%m/%Y %H:%M"), format="%d/%m/%Y %H:%M")


## ver fechas 
lista_fechas<-red_mencion$daycol
lista_fechas<-as.data.frame(lista_fechas)
lista_fechas$lista_fechas<-format(as.Date(lista_fechas$lista_fechas, format="%Y-%m-%d"), format="%Y-%m-%d")




frequencies <- with(lista_fechas, table( lista_fechas))
frequencies <- as.data.frame(frequencies)
n19<- as.Date("2019-11-19")

lista_fechas<-lista_fechas$lista_fechas
red_mencion$daycol<-lista_fechas

frequencies$lista_fechas <- as.Date(frequencies$lista_fechas)


library(ggplot2)
ggplot(data=frequencies, aes(x=lista_fechas, y=Freq)) +
  geom_line() +
  geom_point() +
  expand_limits(y=0) +
  xlab("Day") + ylab("Frequency") +
  ggtitle("Number of tweets per day")+
  geom_vline(xintercept = n21 , linetype="dashed", color = "red")+scale_color_brewer(palette="Dark2")


red_mencion$source<-red_mencion$sourcecol

red_mencion$target<-red_mencion$targetcol

red_mencion<-red_mencion[,c(-2,-3)]


write.csv(red_mencion,"./red_menciones.csv", row.names = FALSE)

n19<- as.Date("2019-11-19")

red_mencion_uno<-filter(red_mencion, daycol<=n19)
write.csv(red_mencion_uno,"./red_menciones_primercohorte.csv", row.names = FALSE)


n27<- as.Date("2019-11-27")

red_mencion_dos<-filter(red_mencion, daycol<=n27 & daycol>n19)
write.csv(red_mencion_dos,"./red_menciones_segundocohorte.csv", row.names = FALSE)


red_mencion_tres<-filter(red_mencion, daycol>=n27)
write.csv(red_mencion_tres,"./red_menciones_tercercohorte.csv", row.names = FALSE)

#creacion tabla de nodos
dbDerecha_paragrafo<-as.data.frame( dBderecha$id)

dbDerecha_paragrafo$username<-dBderecha$screen_name
dbDerecha_paragrafo<-dbDerecha_paragrafo[,c(-1)]

dbDerecha_paragrafo$id<-dBderecha$id
dbDerecha_paragrafo$followers<-dBderecha$followers_count

dbDerecha_paragrafo$following<-dBderecha$friends_count

dbDerecha_paragrafo<-filter(dbDerecha_paragrafo, id %in% users)

write.csv(dbDerecha_paragrafo,"./dBDerecha_cuentasmencionadas.csv", row.names = FALSE)
