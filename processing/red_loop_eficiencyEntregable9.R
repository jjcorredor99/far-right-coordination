library(academictwitteR)
library(stringr)
library(dplyr)
library(tsna)
library(networkDynamic)
options(scipen=999)
setwd("~/Tesis/Notebooks/DB_finales/")
load("dB_aristas.RData")

red_toda<-red_mencion
rm(red_mencion)

red_toda<-do.call(rbind, Map(data.frame, sourcecol=source, targetcol=mentions, daycol=day))

red_toda<-filter(red_toda, !targetcol == sourcecol)


#crear los tiempos
time<-str_split_fixed(red_toda$daycol, "T", 2)[,2]
time<-gsub('.{1}$','',time)
date<-str_split_fixed(red_toda$daycol, "T", 2)[,1]
red_toda$dia<-date
red_toda$tiempo<-time

red_toda$datetime<-paste(red_toda$dia, red_toda$tiempo, sep=" ")


red_toda$datetime<-as.POSIXlt(red_toda$datetime)
red_toda$datetime<- red_toda$datetime-(3600*5)
red_toda$datetime<-as.POSIXlt(red_toda$datetime)

red_toda$dia<-format(red_toda$datetime, "%d/%m/%Y")

red_toda$tiempo<-format(red_toda$datetime, "%H:%M:%S")

#importar base de datos de usuarios identificados

dBderecha<-read.csv("./cuentasDerechaDB.csv", header = T, sep=",", encoding = "UTF-8", stringsAsFactors =FALSE)

ids_users_derecha<-dBderecha$id
ids_users_derecha<-as.character(ids_users_derecha)


#crear la lista de nodos de la red global -> esto no va a cambiar
sources<- unique(red_toda$sourcecol)
targets<-unique(red_toda$targetcol)
users<-unique(c(sources,targets))
nodos_general<-filter(dBderecha, id_str %in% users)

dias_iter<-unique(red_toda$dia)

eficiencia_lista<-c()
dias_listados<-c()

for(dia in dias_iter){
  diax<-dia
  
  aristas_toda_dia<-filter(red_toda, dia == diax)
  aristas_unica_dia<-distinct(aristas_toda_dia, sourcecol, targetcol, .keep_all= TRUE)
  
  aristas_unica_dia$line<-1:nrow(aristas_unica_dia)
  
  aristas_toda_dia$startime<-aristas_toda_dia$datetime$hour
  aristas_toda_dia$endtime<-(aristas_toda_dia$datetime$hour +1)
  
  red_dia<- network(aristas_unica_dia, vertex.attr = nodos_general[,c(4,1,11,12)],matrix.type = "edgelist", ignore.eval = FALSE)
  
  for (i in 1:nrow(aristas_toda_dia)) {
    
    ## inicio
    onset<- as.double(aristas_toda_dia$startime[i])
    #final
    termin<- as.double(aristas_toda_dia$endtime[i])
    
    v_source<- as.double(aristas_toda_dia$sourcecol[i])
    v_target<-as.double( aristas_toda_dia$targetcol[i])
    
    #base de datos temporal que muestra todas las relaciones entre dos nodos
    filtertemp<-filter(aristas_unica_dia, targetcol == as.character(v_target))
    filtertemp<-filter(filtertemp, sourcecol == as.character(v_source))
    
    #activa 
    activate.edges(red_dia,onset=onset, terminus=termin, e=filtertemp$line)
    
  }
  
  eficiencia=0
  for (elquesecalculo in 1:length(red_dia$val)) {
    temppath<-tPath(red_dia,v=elquesecalculo,start=0, graph.step.time = 0, active.default=FALSE)
    temppath$efic<-1/temppath$tdist
    
    for (numero in 1:length(temppath$efic)) {
      if (numero==elquesecalculo) {
        p=1+1
      } 
      else{
        a=(temppath$efic[numero])
        if(a!=Inf){
          eficiencia=eficiencia+a
          
        }
        
        
      }
      
      
      
    }
    
  }
  
  eficiencia_lista<-c(eficiencia_lista,eficiencia)
  dias_listados<-c(dias_listados, diax)
  print(diax)
  
  
}
save(eficiencia_lista, dias_listados, file="../DIAS_eficiencia.RData")


load("../DIAS_eficiencia.RData")
eficienciadB<-do.call(rbind, Map(data.frame, eficienciasumatoria=eficiencia_lista, days=dias_listados))

n=nrow(nodos_general)
eficienciadB$eficienciadia<- eficienciadB$eficienciasumatoria/(n*(n-1))


eficienciadB$diasformat<-paste(str_split_fixed(eficienciadB$days, "/", 3)[,3], str_split_fixed(eficienciadB$days, "/", 3)[,2], str_split_fixed(eficienciadB$days, "/", 3)[,1], sep="-")
eficienciadB$diasformat<- as.Date(eficienciadB$diasformat)


names(eficienciadB)[names(eficienciadB) == 'diasformat'] <- 'lista_fechas_freq'

total <- merge(eficienciadB,frequencies,by="lista_fechas_freq")

total$division_100<- (total$eficienciadia*100)/total$Freq

n21<- as.Date("2019-11-21")


library(ggplot2)
ggplot(data=total, aes(x=lista_fechas_freq, y=division_100))  +
  geom_line() +
  geom_point() +
  expand_limits(y=0) +
  xlab("Day") + ylab("Efficiency") +
  ggtitle("Efficiency per day")+
  geom_vline(xintercept = n21 , linetype="dashed", color = "red")+scale_color_brewer(palette="Dark2")








##calculo de eficiencia 
dia22<-dias_iter[25]
dia22
red_toda$dia[1]
red22<-filter(red_toda, dia == dia22)

format(red_toda$datetime, "%H:%M:%S")

red22<-distinct(red22, sourcecol, targetcol, .keep_all= TRUE)


red22$line<-1:nrow(red22)

red22$datetime[1]$hour
p<-red22$datetime[1]
red22$startime<-red22$datetime$hour
red22$endtime<-(red22$datetime$hour +1)

red_dia<- network(red22, vertex.attr = nodos_general[,c(4,1)],matrix.type = "edgelist", ignore.eval = FALSE)


for (i in 1:nrow(red22)) {
  
  ## inicio
  onset<- as.double(red22$startime[i])
  #final
  termin<- as.double(red22$endtime[i])
  
  v_source<- as.double(red22$sourcecol[i])
  v_target<-as.double( red22$targetcol[i])
  
  #base de datos temporal que muestra todas las relaciones entre dos nodos
  filtertemp<-filter(red22, targetcol == as.character(v_target))
  filtertemp<-filter(filtertemp, sourcecol == as.character(v_source))
  
  #activa 
  activate.edges(red_dia,onset=onset, terminus=termin, e=filtertemp$line)
  
}


eficiencia=0
for (elquesecalculo in 1:length(red_dia$val)) {
  temppath<-tPath(red_dia,v=elquesecalculo,start=0, graph.step.time = 0, active.default=FALSE)
  temppath$efic<-1/temppath$tdist
  
  for (numero in 1:length(temppath$efic)) {
    if (numero==elquesecalculo) {
      p=1+1
    } 
    else{
      a=(temppath$efic[numero])
      if(a!=Inf){
        eficiencia=eficiencia+a
        
      }
      
      
    }
   
    
    
  }

}
n=nrow(nodos_general)
print(eficiencia)
print(eficiencia/(n*(n-1)))
eficiencia2<eficiencia/(n*(n-1))

=eficiencia/(n*(n-1))

temppath<-tPath(red_dia,v=3244,start=0, graph.step.time = 0, active.default=FALSE)
