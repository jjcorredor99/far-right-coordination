library(academictwitteR)
library(stringr)
library(dplyr)
library(data.table)
options(scipen=999)

setwd("~/Tesis/Notebooks/DB_finales/DATA")


files<-list.files()





lista_fechas<-c()
tweetsid<-c()
directory="~/Tesis/Notebooks/DB_finales/DATA"



for (file in files[1:1480]){
  directory_temp=paste(directory, file, sep="/")
  setwd(directory_temp)
  number<-length(list.files())
  if (number>1) {
    setwd(directory)
    dB_temporal<-bind_tweets(file, verbose = TRUE, user=FALSE)

    
  
    
    lista_fechas2<-dB_temporal$created_at
    id_t<- dB_temporal$id
    
    for(fech in 1:nrow(dB_temporal)){
      if(!id_t[fech] %in% tweetsid){
        lista_fechas<-c(lista_fechas, lista_fechas2[fech])
        tweetsid<-c(tweetsid, id_t[fech])
        }
    
    }

  }
  setwd(directory)
}
save(lista_fechas,tweetsid ,file="../FECHAS_TWEETS.RData")
lista_fechas[1]
rm(id_t, lista_fechas2)

a<-c(1,2)

!1 %in% a

setwd("~/Tesis/Notebooks/DB_finales")
load("FECHAS_TWEETS.RData")


fecha_id<-data.table(
  fecha=lista_fechas,
  id_tweet=tweetsid
)


fecha_id<-distinct(fecha_id, id_tweet, .keep_all= TRUE)

rm(lista_fechas, tweetsid)
time<-str_split_fixed(fecha_id$fecha, "T", 2)[,2]
time<-gsub('.{1}$','',time)
date<-str_split_fixed(fecha_id$fecha, "T", 2)[,1]


fecha_id$dia<-date
fecha_id$tiempo<-time
rm(date, time)

fecha_id$datetime<-paste(fecha_id$dia, fecha_id$tiempo, sep=" ")

fecha_id<-fecha_id[,c(-3,-4)]


lista_fechas<-fecha_id$datetime

lista_fechas<-as.data.frame(lista_fechas)

lista_fechas$lista_fechas<-as.POSIXlt(lista_fechas$lista_fechas)
lista_fechas$lista_fechas<- lista_fechas$lista_fechas-(3600*5)

lista_fechas$day <- format(lista_fechas$lista_fechas, '%Y-%m-%d')

lista_fechas_freq<-lista_fechas$day 

lista_fechas_freq<-as.character(lista_fechas_freq)
lista_fechas_freq<-as.data.frame(lista_fechas_freq)
frequencies <- with(lista_fechas_freq, table( lista_fechas_freq))
frequencies <- as.data.frame(frequencies)
n21<- as.Date("2019-11-21")


frequencies$lista_fechas_freq <- as.Date(frequencies$lista_fechas_freq)

save(frequencies, file="../freq_perday.RData")

prueba<- merge()


library(ggplot2)
ggplot(data=frequencies, aes(x=lista_fechas_freq, y=Freq)) +
  geom_line() +
  geom_point() +
  expand_limits(y=0) +
  xlab("Day") + ylab("Frequency") +
  ggtitle("Number of tweets per day")+
  geom_vline(xintercept = n21 , linetype="dashed", color = "red")+scale_color_brewer(palette="Dark2")


rm(fecha_id, lista_fechas, lista_fechas_freq)
