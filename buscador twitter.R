##### paquetes #####
library(rtweet)
library(tidyverse)
library(tm)
library(qdap)
library(qdapRegex)
library(stringi)
library(stringr)

#limpiador de tweets
removeUsername <- function(x) gsub('@[^[:space:]]*', '', x) 

limpiador <- function(text) {
  #elimina emojis
  text_f <- str_remove_all(string = text, pattern = '[:emoji:]')
  #elimina urls
  text_f <- rm_twitter_url(text_f)
  #elimina tildes
  text_f <- stri_trans_general(text_f, "Latin-ASCII")
  text_f <- removeUsername(text_f)
  return(text_f)
}


##### busqueda general #####
# del total de tuits descargable, cuantos hablan de temas relacionados al delito? #

yucatan <- search_tweets(n=17000, geocode = lookup_coords_nominatim("yucatan"))

save_as_csv(yucatan, "yucatan.csv")

california_sur <- search_tweets(n=17000, geocode = lookup_coords_nominatim("baja california sur"))

save_as_csv(california_sur, "california_sur.csv")

ciudad_mexico <- search_tweets(n=17000, geocode = lookup_coords_nominatim("ciudad de mexico"))

save_as_csv(ciudad_mexico, "ciudad_mexico.csv")

puebla <- search_tweets(n=17000, geocode = lookup_coords_nominatim("puebla"))

save_as_csv(puebla, "puebla.csv")

durango <- search_tweets(n=17000, geocode = lookup_coords_nominatim("durango"), retryonratelimit = T)

save_as_csv(durango, "durango.csv")

nuevo_leon <- search_tweets(n=17000, geocode = lookup_coords_nominatim("nuevo leon"), retryonratelimit = T)

save_as_csv(nuevo_leon, "nuevo_leon.csv")

nayarit <- search_tweets(n=17000, geocode = lookup_coords_nominatim("nayarit"), retryonratelimit = T)

save_as_csv(nayarit, "nayarit.csv")

aguas_calientes <- search_tweets(n=17000, geocode = lookup_coords_nominatim("aguascalientes, mexico"), retryonratelimit = T)

save_as_csv(aguas_calientes, "aguas_calientes.csv")

##########

# carga y une bases

aguas_calientes <- read_csv("aguas_calientes.csv") #1
nayarit <- read_csv("nayarit.csv") #2
nuevo_leon <- read_csv("nuevo_leon.csv") #3
durango <- read_csv("durango.csv") #4
puebla <- read_csv("puebla.csv") #5
ciudad_mexico <- read_csv("ciudad_mexico.csv") #6
california_sur <- read_csv("california_sur.csv") #7
yucatan <- read_csv("yucatan.csv") #8

#variable que indica origen del tweet
aguas_calientes$origen <- "aguas_calientes" #1
nayarit$origen <- "nayarit" #2
nuevo_leon$origen <- "nuevo_leon" #3
durango$origen <- "durango" #4
puebla$origen <- "puebla" #5
ciudad_mexico$origen <- "ciudad_mexico" #6
california_sur$origen <- "california_sur" #7
yucatan$origen <- "yucatan" #8

mexico <- rbind(aguas_calientes, nayarit, nuevo_leon, durango, puebla,
                ciudad_mexico, california_sur, yucatan)

# crea variable que selecciona tweets que mencionan palabras clave en seguridad
mexico$text2 <- limpiador(mexico$text)

mexico$seguridad <- str_detect(mexico$text2, 
                               "delito|delin|delict|seguridad|
                                    crimen|crimi|violen|^robo\\|^roba\\|homici|
                                    asesin|narco|estaf|acoso|
                                    acosa|agres|agred|secuestr|asalt|rater|
                                    tracala|ladron")
# filtra celebridades y prensa (cuartil 4 en numero de seguidores y verificados)
mexico_filtered <- mexico %>%
  filter(verified == F)

#elimino el quintil 5 de seguidores
quantile(mexico$followers_count)

mexico_filtered <- mexico %>%
  filter(followers_count < 1296)

########

#Longitudinal

#la API gratuita de twitter no permite busquedas de tweets anteriores a 7 días
#sin embargo es posible pedir hasta 3200 tweets de un individuo - sin importar su origen temporal
#aqui se realiza un testeo inicial: se seleccionan 35 cuentas por estado con 
  # creacion anterior al 2018
  # que poseen algun tuit asociado al delito o la seguridad
#en base a esto se seleccionan los usuarios para los que hay datos desde
  #2018
  #2019
  #2020
#permitiendo un análisis temporal exploratorio

aux <- filter(mexico_filtered, format(account_created_at, format="%Y") < 2018)
aux <- filter(aux, seguridad == T)
aux2 <- split(aux, aux$origen)

#de cada localidad selecciono 35 cuentas random que hayan discutido delito

california_users <- aux2[["california_sur"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)
aguas_calientes_users <- aux2[["aguas_calientes"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)
ciudad_mexico_users <- aux2[["ciudad_mexico"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)
durango_users <- aux2[["durango"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)
nayarit_users <- aux2[["nayarit"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)
nuevo_leon_users <- aux2[["nuevo_leon"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)
puebla_users <- aux2[["puebla"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)
yucatan_users <- aux2[["yucatan"]]$screen_name %>% unique() %>% sample(size = 35, replace = F)

nombres_usuarios <- c(california_users, aguas_calientes_users, ciudad_mexico_users, durango_users,
                      nayarit_users, nuevo_leon_users, puebla_users, yucatan_users)

usuarios <- get_timeline(nombres_usuarios, n = 500, token = my_token)

usuarios2 <- usuarios %>% 
  mutate(origen = case_when(
    screen_name %in% california_users ~ "california_sur",
    screen_name %in% aguas_calientes_users ~ "aguas_calientes",
    screen_name %in% ciudad_mexico_users ~ "ciudad_mexico",
    screen_name %in% durango_users ~ "durango",
    screen_name %in% nayarit_users ~ "nayarit",
    screen_name %in% nuevo_leon_users ~ "nuevo_leon",
    screen_name %in% puebla_users ~ "puebla",
    screen_name %in% yucatan_users ~ "yucatan"
  ))

#hay truncamiento por izquierda (algunos twitean mucho)
#necesito filtrar para aquellos para los que recogimos al menos un tweet del correspondiente a?o

#Guardo estos datos para diferentes cortes (desde 2018, desde 2019, desde 2020)
usuarios2$aux18 <- format(usuarios2$created_at, format="%Y") == 2018
usuarios2$aux19 <- format(usuarios2$created_at, format="%Y") == 2019
usuarios2$aux20 <- format(usuarios2$created_at, format="%Y") == 2020

#setNames(aggregate(usuarios2$aux1, by=list(screen_name=usuarios2$screen_name), FUN=sum), c("screen_name","desde2018"))

usuarios2 <- setNames(aggregate(usuarios2$aux18, by=list(screen_name=usuarios2$screen_name), FUN=sum), c("screen_name","desde2018")) %>% 
  merge(usuarios2, by="screen_name")

usuarios2 <- setNames(aggregate(usuarios2$aux19, by=list(screen_name=usuarios2$screen_name), FUN=sum), c("screen_name","desde2019")) %>% 
  merge(usuarios2, by="screen_name")

usuarios2 <- setNames(aggregate(usuarios2$aux20, by=list(screen_name=usuarios2$screen_name), FUN=sum), c("screen_name","desde2020")) %>% 
  merge(usuarios2, by="screen_name")

#data por mes para los estados desde 2018

desde2018_plot <- usuarios2 %>%
  filter(seguridad == T) %>%
  filter(desde2018 > 0) %>%
  filter(format(created_at, format="%Y") > 2017) %>%
  filter(format(created_at, format="%Y") < 2021) %>%
  group_by(origen) %>%
  ts_plot("months")

#data por mes para los estados desde 2019

desde2019_plot <- usuarios2 %>%
  filter(seguridad == T) %>%
  filter(desde2019 > 0) %>%
  filter(format(created_at, format="%Y") > 2018) %>%
  filter(format(created_at, format="%Y") < 2021) %>%
  group_by(origen) %>%
  ts_plot("months")

#data por mes para los estados desde 2020

desde2020_plot <- usuarios2 %>%
  filter(seguridad == T) %>%
  filter(desde2020 > 0) %>%
  filter(format(created_at, format="%Y") > 2019) %>%
  filter(format(created_at, format="%Y") < 2021) %>%
  group_by(origen) %>%
  ts_plot("months")




