##### inicio #####

library(rtweet)
library(tidyverse)
library(tm)
library(qdap)
library(qdapRegex)
library(stringi)
library(stringr)
library(readr)
library(udpipe)
library(BTM)
library(textplot)
library(ggraph)
library(stm)
library(readxl)

# Requiere cargar las bases creadas con el codigo "buscador twitter"

expss::cro(mexico$origen, mexico$seguridad)
expss::fre(mexico$seguridad)
expss::fre(mexico$origen, mexico$seguridad)

#1.2% de los tweets mencionan elementos asociados a la seguridad

seguridad.glm <- 
  glm(seguridad ~ relevel(factor(origen), ref = 4), data = mexico, family = "binomial")

seguridad.glm %>%
  broom::tidy(exponentiate = T, conf.int = T) %>%
  mutate_if(is.numeric, ~ round(.,3)) %>%
  knitr::kable(align = c("l", rep("c", 6)))

# qu? pasa si intento filtrar lo que puede ser prensa?

#elimino verificados

mexico_filtered <- mexico %>%
  filter(verified == F)

#elimino el quintil 5 de seguidores
quantile(mexico$followers_count)

mexico_filtered <- mexico %>%
  filter(followers_count < 1296)

unique(mexico_filtered$screen_name) %>% length()

#un total de 42505 usuarios diferentes y unos 104943 tweets

expss::cro(mexico_filtered$origen, mexico_filtered$seguridad)

seguridad_filtered.glm <- 
  glm(seguridad ~ relevel(factor(origen), ref = 4), data = mexico_filtered, family = "binomial")

seguridad_filtered.glm %>%
  broom::tidy(exponentiate = T, conf.int = T) %>%
  mutate_if(is.numeric, ~ round(.,3)) %>%
  knitr::kable(align = c("l", rep("c", 6)))

#qu? pasa si adem?s incluyo sentimientos?
mexico$sentiment <- syuzhet::get_sentiment(mexico$text, method = "nrc", language = "spanish") #puede tomar su rato

mexico$seguridad_sentiment <- mexico$seguridad * mexico$sentiment

mexico_sentiment <- mexico %>%
  filter(followers_count < 1296) %>%
  filter(verified == F) %>% 
  filter(seguridad_sentiment != 0)

#642 observaciones
#distribuidos:
table(mexico_sentiment$origen)

mexico_sentiment$seguridad_sentiment.r <- mexico_sentiment$seguridad_sentiment + 13

lm(seguridad_sentiment.r ~ relevel(factor(origen), ref = 4), data = mexico_sentiment) %>% summary()

#no parece haber diferencia en la intensidad con la que hablan (a nivel de sentimento)

#pondero por sentimiento

## rehago mexico_filtered

mexico_filtered <- mexico %>%
  filter(verified == F)

mexico_filtered <- mexico %>%
  filter(followers_count < 1296)

mexico_filtered$seguridad_sentiment.r <- mexico_filtered$seguridad_sentiment + 13
lm(seguridad_sentiment.r ~ relevel(factor(origen), ref = 4), data = mexico) %>% summary()

#ponderando las noticias por su sentimiento ciudad de mexico y puebla parecen estar en peores condiciones

## Longitudinal

#selecciono solo cuentas creadas antes de 2018 (para evitar truncamiento)

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

my_token <- get_token()
usuarios <- get_timeline(nombres_usuarios, n = 500, token = my_token)

save_as_csv(usuarios, "usuarios.csv")

#creo origen

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

#save_as_csv(usuarios2, "usuarios2")

usuarios2 %>%
  group_by(origen) %>%
  summarise(n_distinct(screen_name))

usuarios2$text2 <- limpiador(usuarios2$text)

usuarios2$seguridad <- str_detect(usuarios2$text2, 
                                    "delito|delin|delict|seguridad|
                                    crimen|crimi|violen|^robo\\|^roba\\|homici|
                                    asesin|narco|estaf|acoso|
                                    acosa|agres|agred|secuestr|asalt|rater|
                                    tracala|ladron")
table(usuarios2$seguridad)

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

desde2018_plot$data %>% write.csv("desde2018.csv")

desde2019_plot <- usuarios2 %>%
  filter(seguridad == T) %>%
  filter(desde2019 > 0) %>%
  filter(format(created_at, format="%Y") > 2018) %>%
  filter(format(created_at, format="%Y") < 2021) %>%
  group_by(origen) %>%
  ts_plot("months")

desde2019_plot$data %>% write.csv("desde2019.csv")

desde2020_plot <- usuarios2 %>%
  filter(seguridad == T) %>%
  filter(desde2020 > 0) %>%
  filter(format(created_at, format="%Y") > 2019) %>%
  filter(format(created_at, format="%Y") < 2021) %>%
  group_by(origen) %>%
  ts_plot("months")

desde2020_plot$data %>% write.csv("desde2029.csv")


# Topic Modelling - 2021
text2021_vector <- filter(mexico, seguridad == T)$text2

text2021_vector <- removeWords(text2021_vector, stopwords("spanish"))

txt_udpipe <- udpipe(text2021_vector, object = "spanish")

biterms <- data.table::as.data.table(txt_udpipe)
biterms <- biterms[, cooccurrence(x = lemma, 
                                  relevant = upos %in% c("NOUN", "PROPN", "ADJ"),
                                  skipgram = 2), 
                   by = list(doc_id)]

set.seed(123)
x <- subset(txt_udpipe, upos %in% c("NOUN", "PROPN", "ADJ"))
x <- x[, c("doc_id", "lemma")]
model <- BTM(x, k = 10, beta = 0.01, iter = 2000, background = TRUE, 
             biterms = biterms, trace = 100)
topicterms <- terms(model, top_n = 5)
topicterms

plot(model, top_n = 5)

# Topic Modelling - 2020
usuarios_2020 <- usuarios2 %>% 
  filter(seguridad == T) %>%
  filter(desde2020 > 0) %>%
  filter(format(created_at, format="%Y") > 2019) %>%
  filter(format(created_at, format="%Y") < 2021)

text2020_vector <- removeWords(usuarios_2020$text2, stopwords("spanish"))

txt_udpipe2020 <- udpipe(text2020_vector, object = "spanish")

biterms2020 <- data.table::as.data.table(txt_udpipe2020)
biterms2020 <- biterms2020[, cooccurrence(x = lemma, 
                                  relevant = upos %in% c("NOUN", "PROPN", "ADJ"),
                                  skipgram = 2), 
                   by = list(doc_id)]

set.seed(123)
x <- subset(txt_udpipe2020, upos %in% c("NOUN", "PROPN", "ADJ"))
x <- x[, c("doc_id", "lemma")]
model <- BTM(x, k = 8, beta = 0.01, iter = 2000, background = TRUE, 
             biterms = biterms2020, trace = 100)
topicterms <- terms(model, top_n = 5)
topicterms


plot(model, top_n = 5)

#Topic Modelling STM - 2021

mexico_seguridad <- filter(mexico_filtered, seguridad == T) %>% select(text,origen)
mexico_seguridad$origen <- as.factor(mexico_seguridad$origen)

stm2021.pro <- textProcessor(mexico_seguridad$text, language = "es", metadata = mexico_seguridad)
stm2021.prep <- prepDocuments(stm2021.pro$documents, stm2021.pro$vocab, stm2021.pro$meta, lower.thresh = 5)
stm2021.out <- stm(documents = stm2021.prep$documents, vocab = stm2021.prep$vocab, 
                   data = stm2021.prep$meta, K = 10, prevalence =~ origen)

labelTopics(stm2021.out)
findThoughts(stm2021.out, texts = stm2021.prep$meta$text)

plot(stm2021.out, type = "summary", xlim = c(0, 0.3))

toLDAvisJson(stm2021.out, docs = stm2021.prep$documents) %>% LDAvis::serVis(open.browser = T)

toLDAvisJson(stm2021.out, docs = stm2021.prep$documents) %>% LDAvis::serVis()
