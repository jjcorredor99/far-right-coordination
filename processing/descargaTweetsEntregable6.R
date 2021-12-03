library(academictwitteR)


setwd("~/Tesis/Notebooks/DB_finales/")


df<-read.csv("cuentasDerechaDB.csv", header = T, sep=",", encoding = "UTF-8", stringsAsFactors =FALSE) 




bearer_token = "XXXXXXXXXXXXXXXXXXXXXXXXXXX"


x=1
y=10

users <- dplyr::pull(df, id)
dt="data_"



for (i in 1:nrow(df)) {
  users_2<-users[x:y]
  nombre<-paste(c(dt,x,"_",y,"/"), collapse = "")

  query<-get_all_tweets(users = users_2,
                        start_tweets = "2019-10-21T00:00:00Z", #fechas descargadas
                        end_tweets = "2019-12-21T00:00:00Z",
                        data_path = nombre,
                        bearer_token = bearer_token,
                        bind_tweets = FALSE,
                        n=Inf
  )
  x=x+10
  y=y+10
}

binded_tweets<- bind_tweets("bind/", user = FALSE, verbose = TRUE, output_format = NA)

rm(binded_tweets)
