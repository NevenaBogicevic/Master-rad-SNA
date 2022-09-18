

### 1. UCITAVANJE PAKETA I FUNKCIJA ######################################### 
library(twitteR)
library(rtweet)
library(tidyverse)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(igraph)
library(visNetwork)
library(httpuv)
library(dplyr)
library(p2distance)
library(corrplot)
source("Funkcije.R")


###2. POVEZIVANJE SA TWitterom

api_key <- "xxxxxxxxxxxx"
api_secret <- "xxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


### 3. Povlacenje podataka sa tw

## Osnovni uzorak

osnovni_uzorak <- read.csv("osnovni_uzorak",
                           header = TRUE,
                           encoding = "UTF-8",
                           stringsAsFactors = F)

view(osnovni_uzorak)
tvitovi_ouz <- ekstrakcija_er(spisak = osnovni_uzorak$Nalog,
                              od = "2022-03-03 00:00:00 CET",
                              do = "2022-04-03 00:00:00 CET")


#Cuvanje skinutih tvitova u .csv fajlu

write.csv(tvitovi_ouz, "tvitovi_ouz.csv", row.names = F)

#Ucitavanje tvitova iz .csv fajla

tvitovi_ou <- read.csv("tvitovi_ouz.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = F)

view(tvitovi_ou)

nrow(tvitovi_ou)


#Proveravanje koji nalozi iz osnovnog uzorka nisu usli u tvitovi_ou

setdiff(osnovni_uzorak$Nalog, unique(tvitovi_ou$screenName))

#svi nalozi su tu


### 3.2 EKSTRAKCIJA TVITOVA PROSIRENOG UZORKA ### 

#Pravljenje spiska imena naloga koji su nadprosecno cesto komunicirali sa nalozima iz osnovnog uzorka

prosireni_uzorak <- snowball(tvitovi_ou)

view(sort(prosireni_uzorak))

length(prosireni_uzorak)


tvitovi_pu <- ekstrakcija_er(prosireni_uzorak,
                             pocetak = "2022-03-03 00:00:00 CET",
                             kraj = "2022-04-03 23:59:59 CET")



#Cuvanje tvitovi_pu.csv fajla

write.csv(tvitovi_pu, "tvitovi_pu.csv", row.names = F)

#Ucitavanje tvitovi_pu iz csv fajla

tvitovi_pu <- read.csv("tvitovi_pu.csv",
                       header = TRUE,
                       encoding = "UTF-8",
                       stringsAsFactors = F)

n_distinct(tvitovi_pu$screenName)

#Proveravanje koji nalozi iz prosirenog uzorka nisu usli u tvitovi_pu

setdiff(prosireni_uzorak, unique(tvitovi_pu$screenName)) 

### 3.3 Skidanje dodatnih podataka o nalozima iz osnovnog uzorka (osnovne metrike) ###


metrika_ou <- naloziMeta(osnovni_uzorak$Nalog)

### 3.5 Skidanje podataka o tvitovima osnovnog uzorka u toku posmatranog perioda ###

metrika_tvitovi_ou <- retfav(tvitovi_ou)

view(metrika_tvitovi_ou)

### 3.6 Pravljenje tabela za izvestaj (Dodatak 1: Podaci o nalozima osnovnog uzorka i Dodatak 2: Klasicne metrike osnovnog uzorka) ###

tab_ou <- cbind(osnovni_uzorak, metrika_ou["DatumOtvaranja"])

write.csv(tab_ou, "tabela_podaci_ou.csv", row.names = F)


tab_klas_met <- osnovni_uzorak[,c(1,2,4,5,6)]

tab_klas_met <- cbind(tab_klas_met, metrika_ou[,2:5])

tab_klas_met <- cbind(tab_klas_met, metrika_tvitovi_ou[,2:5])


write.csv(tab_klas_met, "tabela_klas_met.csv", row.names = F)


tab_klas_met <- read.csv("tabela_klas_met.csv",
                         header = TRUE,
                         encoding = "UTF-8",
                         stringsAsFactors = F)


### 3.7 Vizualizacija prosecnog retvita po listama


izb <- c(4, 11, 12)

pros_ret_lis <- tab_klas_met[,izb] %>% 
  group_by(Lista) %>% 
  summarize(Pros.Ret = sum(Retvitovan) / sum(Tvitovi.1))

pros_ret_lis <- arrange(pros_ret_lis, desc(pros_ret_lis$Pros.Ret))

skraceni_nazivi <- c("UZSP", "NADA","MORAMO", "DVERI-POKS","ZAVETNICI","SNS", "SPS")

pros_ret_lis$Lista <- skraceni_nazivi

ggplot(pros_ret_lis, aes(x = Pros.Ret, y = reorder(Lista, Pros.Ret), fill = Lista)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= round(Pros.Ret, digits = 2)), vjust=0) +
  labs(y= "Lista", x = "Prosecni retvit", title= "Prosecni retvit po listama") +
  theme_bw()+ theme(legend.position = "none")

### 3.8 Vizualizacija prosecnog retvita po frakcijama

izb1 <- c(5, 11, 12)

pros_ret_fra <- tab_klas_met[,izb1] %>% 
  group_by(Frakcija) %>% 
  summarize(Pros.Ret = sum(Retvitovan) / sum(Tvitovi.1))

pros_ret_fra <- arrange(pros_ret_fra, desc(pros_ret_fra$Pros.Ret))

ggplot(pros_ret_fra, aes(x = Pros.Ret, y = reorder(Frakcija, Pros.Ret), fill = Frakcija)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= round(Pros.Ret, digits = 2)), vjust=0) +
  labs(y= "Frakcija", x = "Prosecni retvit", title= "Prosecni retvit po frakcijama") +
  theme_bw()+ theme(legend.position = "none")

#############################################################################
### 4. PRAVLJENJE MATRICA POVEZANOSTI #######################################
#############################################################################


### Pravljenje matrice poveznosti za osnovni i prosireni uzorak ###

matpov_ou <- matrica_povezanosti_simpler(tvitovi_ou)

matpov_pu <- matrica_povezanosti_simpler(tvitovi_pu)


#############################################################################
### 5. KREIRANJE GRAFOVA ####################################################
#############################################################################

### 5.1. kreiranje mreže osnovnog i prosirenog uzorka ###

mreza_ou <- graph.adjacency(matpov_ou, mode="directed", weighted = TRUE)
mreza_pu <- graph.adjacency(matpov_pu, mode="directed", weighted = TRUE)

#čuvanje mreže osnovnog i mreže proširenog uzorka

saveRDS(mreza_ou, "nets/mreza_ou.RDS")
saveRDS(mreza_pu, "nets/mreza_pu.RDS")


#učitavanje mreža

mreza_ou <- readRDS("nets/mreza_ou.RDS")

mreza_pu <- readRDS("nets/mreza_pu.RDS")



### 5.2 Dodeljivanje atributa čvorovima osnovnog uzorka ###


V(mreza_ou)$ime <- osnovni_uzorak$Ime
V(mreza_ou)$prezime <- osnovni_uzorak$Prezime
V(mreza_ou)$stranka <- osnovni_uzorak$Stranka
V(mreza_ou)$lista <- osnovni_uzorak$Lista
V(mreza_ou)$frakcija <- osnovni_uzorak$Frakcija
V(mreza_ou)$color <- osnovni_uzorak$Boja




### 6. VIZUALIZACIJA GRAFOVA ################################################


### 6.1 Vizualizacija mreze osnovnog uzorka pomocu visNetwork

vis_ou <- toVisNetworkData(mreza_ou)
head(vis_ou$nodes)
head(vis_ou$edges)

vis_ou$nodes <- vis_ou$nodes %>% select(-label)
vis_ou$nodes <- vis_ou$nodes %>% rename(group = lista)
vis_ou$nodes <- vis_ou$nodes %>% rename(label = prezime)
vis_ou$nodes$value <- degree(mreza_ou, mode = "total")

vis_ou$edges$arrows <- rep("to", 221)

visNetwork(nodes = vis_ou$nodes, edges = vis_ou$edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(selectedBy = "frakcija") 

### 6.2 Vizualizacija mreze prosirenog uzorka pomocu visNetwork


vis_pu <- toVisNetworkData(mreza_pu)
head(vis_pu$nodes)
head(vis_pu$edges)

for (i in 1: length(vis_pu$nodes$id)) {
  if (!(vis_pu$nodes$id[i] %in% osnovni_uzorak$Nalog)) {
    vis_pu$nodes$frakcija[i] <- 'Ostali_tviterasi'
    vis_pu$nodes$color[i] <- 'gray'
    vis_pu$nodes$uzorak[i] <- 2
  }
  else {
    if (vis_pu$nodes$id[i] %in% osnovni_uzorak$Nalog[osnovni_uzorak$Frakcija == 'Opozicija']) {
      vis_pu$nodes$frakcija[i] <- 'Opozicija'
      vis_pu$nodes$color[i] <- 'lightblue'
      vis_pu$nodes$uzorak[i] <- 1
    }
    
    if (vis_pu$nodes$id[i] %in% osnovni_uzorak$Nalog[osnovni_uzorak$Frakcija == 'Vlast']) {
      vis_pu$nodes$frakcija[i] <- 'Vlast'
      vis_pu$nodes$color[i] <- 'lightcoral'
      vis_pu$nodes$uzorak[i] <- 1
    }
  }
}  


vis_pu$nodes <- vis_pu$nodes %>% arrange(uzorak)

#Izbacivanje ivica koje ne ukljucuju cvorove osnovnog uzorka 
vis_pu_edges <- vis_pu$edges %>% filter(((vis_pu$edges$from %in% osnovni_uzorak$Nalog) | 
                                           (vis_pu$edges$to %in% osnovni_uzorak$Nalog)))

#Izbacivanje ivica izmedju cvorova osnovnog uzorka
vis_pu_edges <- vis_pu_edges %>% filter(!((vis_pu_edges$from %in% osnovni_uzorak$Nalog) & 
                                            (vis_pu_edges$to %in% osnovni_uzorak$Nalog)))

#Dodeljivanje ivica cvorovima osnovnog uzorka
for (i in 1:length(vis_pu_edges$from)) {
  if (!(vis_pu_edges$from[i] %in% osnovni_uzorak$Nalog)) {
    a <- vis_pu_edges$from[i]
    b <- vis_pu_edges$to[i]
    vis_pu_edges$from[i] <- b
    vis_pu_edges$to[i] <- a
  }
}

#Vizualizacija
visNetwork(nodes = vis_pu$nodes, edges = vis_pu_edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(selectedBy = "frakcija") 


### 7.1 Globalne metrike mreze osnovnog i prosirenog uzorka ###

#uklanjanje dva naloga koji ne pripadaju gigantskoj komponenti

ou_components <- components(mreza_ou)

brisanje <- names(ou_components$membership[ou_components$membership != 1])

mreza_ou_con <- delete.vertices(mreza_ou, brisanje)


#Racunanje globalnih metrika mreza osnovnog uzorka, prosirenog uzorka i prosirenog uzorka


glob_met_ou <- met_graf(mreza_ou_con)

glob_met_pu <- met_graf(mreza_pu)  #mreza proširenog uzorka je nepovezana



#izdvajanje gigantske komponente mreže proširenog uzorka

mreza_pu_comp <- components(mreza_pu, mode = 'weak')

mreza_pu_giant_comp_size <- max(mreza_pu_comp$csize)#najveća komponenta se sastoji od 179 čvorova

#indentifikujmeo koja je to komponenta koja ima 179 cvora
mreza_pu_giant_comp_comp_index <- which(mreza_pu_comp$csize == mreza_pu_giant_comp_size)

# Izdvajamo cvorove koji pripadaju toj komponenti
nodes_in_gc_mreza_pu <- which(mreza_pu_comp$membership==mreza_pu_giant_comp_comp_index)
nodes_in_gc_mreza_pu

## izdvajamo gc

mreza_pu_giant_comp <- induced_subgraph(mreza_pu, vids = nodes_in_gc_mreza_pu)

##ispitijujemo koji je čvor nepovezan 
disconnected_node <- names(mreza_pu_comp$membership[mreza_pu_comp$membership != mreza_pu_giant_comp_comp_index])
disconnected_node#nepovezani cvor je "sindikatisloga"

tvitovi_ou <- read.csv("tvitovi_ouz.csv", encoding = "UTF-8")
View(tvitovi_ou[!is.na(tvitovi_ou$replyToSN) & 
                  tvitovi_ou$replyToSN==disconnected_node,])

summary(mreza_pu_giant_comp)
is_connected(mreza_pu_giant_comp, mode = 'weak')

glob_met_pu <- met_graf(mreza_pu_giant_comp)  

glob_met_pu


### 7.2 Metrike centralnosti noudova ###

#Osnovni uzorak

cent_met_ou_con <- met_cent(mreza_ou_con)

write.csv(cent_met_ou_con, "cent_met_ou_con.csv", row.names = T)

cent_met_ou_con <- read.csv("cent_met_ou_con.csv",
                            header = TRUE,
                            encoding = "UTF-8",
                            stringsAsFactors = F)


#Metrike centrealnosti aktera iz osnovnog uzorka u mrezi prosirenog uzorka

cent_met_pu <- met_cent(mreza_pu_giant_comp)

osnovni_uzorak <- read.csv("osnovni_uzorak")
cent_met_ou_in_pu <- cent_met_pu %>% filter(row.names(cent_met_pu) %in% osnovni_uzorak$Nalog)
write.csv(cent_met_ou_in_pu, "cent_met_ou_in_pu.csv", row.names = T)

cent_met_ou_in_pu <- read.csv("cent_met_ou_in_pu.csv",
                              header = TRUE,
                              encoding = "UTF-8",
                              stringsAsFactors = F)
view(cent_met_ou_in_pu)

### 8. IZRACUNAVANJE KORELACIJE IZMEDJU SVIH METRIKA (MREZNIH I KLASICNIH) ##


#Pravljenje dejtafrejma koji sadrzi: 1) metrike centralnosti noudova osnovnog
#uzorka u okviru PU; 2) klasicne metrike naloga i 3) metrike naloga za vreme 
#kampanje

cent_met <- cent_met_ou_in_pu
cent_met <- cent_met %>% arrange(rownames(cent_met))

tab_klas_met <- read.csv("tabela_klas_met.csv",
                         header = TRUE,
                         encoding = "UTF-8",
                         stringsAsFactors = F)

#ukolanjanje nepovezanog naloga "UdruzenjeSindikatiSrbijeSloga"

osnovni_uzorak1<- osnovni_uzorak %>% 
  filter(!str_detect(Prezime, "UdruzenjeSindikatiSrbijeSloga"))

tab_klas_met1<-tab_klas_met %>% 
  filter(!str_detect(Prezime, "UdruzenjeSindikatiSrbijeSloga"))


klas_met <- tab_klas_met1
klas_met <- klas_met[, 6:13]
rownames(klas_met) <- osnovni_uzorak1$Nalog
klas_met <- klas_met %>% arrange(rownames(klas_met))
View(klas_met)


sve_metrike <- cbind(cent_met, klas_met)

view(sve_metrike)
sve_metrike <- sve_metrike %>%
  rename("SviTvitovi" = "Tvitovi",
         "TvitoviKamp" = "Tvitovi.1")
sve_metrike$X <- NULL
sve_metrike$Pros.Ret[is.na(sve_metrike$Pros.Ret)] <- 0 

#Vizualizacija matrice korelacija svih pokazatelja
mat_cor_sve1 <- cor(sve_metrike, method = "spearman")

mat_cor_sve <- cor.mtest(sve_metrike, method = "spearman", conf.level = 0.95, exact= FALSE)$p

corrplot(mat_cor_sve1, p.mat= mat_cor_sve, type = "upper", method = "number", sig.level = 0.5)


# Izracunavanje i vizualizacija sveukupne centralnosti prema listama za naloge
# OU u okviru PU
ou_arranged <- osnovni_uzorak %>% 
  arrange(Nalog) %>% 
  filter(!str_detect(Prezime, "UdruzenjeSindikatiSrbijeSloga"))

g <- aggregate(
  x = cent_met$P2odstojanje,
  FUN = sum,
  by = list(
    ou_arranged$Lista)
)
g$Broj <- as.numeric(table(ou_arranged$Lista))
colnames(g) <- c("Lista", "Centralnost", "Broj")
g$CentStand <- g$Centralnost / g$Broj
g_ord <- g[order(- g$CentStand), ]

ggplot(g_ord, aes(x = CentStand, y = reorder(Lista, CentStand), fill = Lista)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(size=8,aes(label= round(`CentStand`, digits = 2)), vjust=0) +
  ylab("Lista") +
  xlab("Prosecna sveukupna centralnost") +
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=26),axis.title=element_text(size=18,face="bold")) +
                                                     theme(axis.title = element_text(size = 24)) 



# Izracunavanje i vizualizacija sveukupne centralnosti prema frakcijama za naloge
# OU u okviru PU
f <- aggregate(
  x = cent_met$P2odstojanje,
  FUN = sum,
  by = list(
    ou_arranged$Frakcija)
)
f$Broj <- as.numeric(table(ou_arranged$Frakcija))
colnames(f) <- c("Frakcija", "Centralnost", "Broj")
f$CentStand <- f$Centralnost / f$Broj
f_ord <- f[order(- f$CentStand), ]

ggplot(f_ord, aes(x = CentStand, y = reorder(Frakcija, CentStand), fill = Frakcija)) +
  geom_bar(stat = "identity", width = 0.3) +
  geom_text(size=8,aes(label= round(`CentStand`, digits = 2)), vjust=0) +
  ylab("Frakcija") +
  xlab("Prosecna sveukupna centralnost") +
  theme_bw()+theme(axis.text=element_text(size=26),
                   axis.title=element_text(size=18,face="bold"))  + 
  theme(legend.position = "none")+ 
  theme(axis.title = element_text(size = 24))

### 9. UTVRDJIVANJE HOMOFILIJE ##############################################

### Vizualizacija subgrafova osnovnog uzorka prema listi i frakciji i racunanje homofilije i tranzitivnosti ###


#Sredjivanje igraph objekta ou_hom koji ce biti input
ou_hom <- mreza_ou

V(ou_hom)$group <- V(ou_hom)$lista
V(ou_hom)$shape <- rep(NA, length(V(ou_hom)$name))


#PROVERA
ou_hom_vis <- toVisNetworkData(ou_hom)
View(ou_hom_vis$nodes) #sve je u redu

#Vizualizacija subgrafova po listama

for (i in 1:length(unique(V(ou_hom)$lista))) {
  podgraf <- podgraf_hom_lis(ou_hom, lis = i)
  if (!is.null(podgraf))
    print(podgraf)
}

#Vizualizacija subgrafova po frakcijama

for (i in 1:length(unique(V(ou_hom)$frakcija))) {
  podgraf <- podgraf_hom_fra(ou_hom, fra = i)
  print(podgraf)
}

### 10. KLASTER ANALIZA #####################################################



#Primena "cluster_optimal()" funcije na mrezu osnovnog uzorka

klaster_ou <- cluster_optimal(mreza_ou)
membership(klaster_ou)
modularity(klaster_ou)

#Printanje rezultata

for (i in 1:length(klaster_ou)) {
  print(klaster_ou[i])
}

#Sredjivnaje igraph objekta ou_klast i dodeljivanje clanstva u klasterima
#cvorovima
ou_klast <- mreza_ou

V(ou_klast)$group <- V(ou_klast)$lista

V(ou_klast)$klaster <- klaster_ou$membership

#PROVERA
ou_klast_vis <- toVisNetworkData(ou_klast)
ou_klast_vis$nodes #sve je u redu


#Vizualizacija podele po klasterima
ivice <- ou_klast_vis$edges
ivice$color <- rep("gainsboro", length(ivice$from))

visNetwork(nodes = ou_klast_vis$nodes, edges = ivice, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(selectedBy = "klaster")




