#############################################################################
#1. UCITAVANJE PAKETA I DEFINISANJE IZVORA FUNKCIJA
#############################################################################

library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(xlsx)
#Packages recommeded by the course
library(rtweet)
library(tidyverse)
library(httpuv)
library(dplyr)
library(visNetwork)
library(igraph)
library(threejs)
library(readxl)
library(p2distance)
library(corrplot)
library(RColorBrewer)
library(tm)
library(wordcloud)
library(qdap)

setwd("")
source("")

registerTwitterOAuth('.httr-oauth')
#############################################################################
#2. POVEZIVANJE SA TVITEROM
#############################################################################

#2.1 Autentikacija TW tokena
twitter_token <- create_token(
  app = "xxxx", 
  consumer_key = "xxxx",
  consumer_secret = "xxxx",
  access_token = "xxxx",
  access_secret = "xxxx"
)

api_key <- "xxxx"
api_secret <- "xxxx"
access_token <- "xxxx"
access_token_secret <- "xxxx"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#2.2 Cuvanje tokena kao fajla:
saveRDS(twitter_token, "C:/Users/User/Desktop/Dokumenta/Fakulteti/Master 2/Zavrsni rad/twitter_token.rds")

#2.3 Cuvanje linije sa adresom tviter.rds:
file.edit(".Renviron")

get_token()
###########################################################################################################
#3. EKSTRAKCIJA TVITOVA
###########################################################################################################

#3.1 Ucitavanje uzorka profila iz uzorak Druga_Srbija.xlsx
Druga_Srbija <- read_excel("C:/Users/User/Desktop/Dokumenta/Fakulteti/Master 2/RADM/Ispit/Prva verzija konacno/Uzorak Druga Srbija.xlsx",
                           col_names = TRUE,
                           col_types = NULL,
                           skip = 0)

View(Druga_Srbija)

#3.2 Ekstrakcija tvitova uzorka Druga Srbija za period izmedju 28.07.2020. i 28.08.2020.
tvitovi_DS1 <- ekstrakcija_er(Druga_Srbija$Nalog,
                             pocetak = "2020-07-28 00:00:00 CET",
                             kraj = "2020-08-28 23:59:59 CET")

#Cuvanje tvitovi_DS.csv fajla
write.csv(tvitovi_DS1, "tvitovi_DS1.csv", row.names = F)


#Ucitavanje tvitovi_DS iz .csv fajla
tvitovi_DS1 <- read.csv("tvitovi_DS1.csv",
                        header = TRUE,
                        encoding = "UTF-8",
                        stringsAsFactors = F)

#Proveravanje koji nalozi iz uzorka nisu usli u tvitovi_DS
setdiff(Druga_Srbija$Nalog, unique(tvitovi_DS$screenName))

#Nisu prisutni Sonja Biserko (SBiserko) i Biljana Srbljanovic (leyakelIer) usled neaktivnosti u navedenom periodu


#3.3 Ekstrakcija tvitova uzorka Druga Srbija za period izmedju 29.08.2020. i 29.09.2020.
tvitovi_DS2 <- ekstrakcija_er(Druga_Srbija$Nalog,
                             pocetak = "2020-08-29 00:00:00 CET",
                             kraj = "2020-09-29 23:59:59 CET")


#Cuvanje tvitovi_DS2.csv fajla
write.csv(tvitovi_DS2, "tvitovi_DS2.csv", row.names = F)


#Ucitavanje tvitovi_DS2 iz .csv fajla
tvitovi_DS2 <- read.csv("tvitovi_DS2.csv",
                       header = TRUE,
                       encoding = "UTF-8",
                       stringsAsFactors = F)


#Proveravanje koji nalozi iz uzorka nisu usli u tvitovi_DS
setdiff(Druga_Srbija$Nalog, unique(tvitovi_DS2$screenName))

#Nisu prisutni Sonja Biserko (SBiserko), Jovana Gligorijevic (Jovanana) i Biljana Srbljanovic (leyakelIer) usled neaktivnosti u navedenom periodu


#3.4 Skidanje dodatnih podataka o nalozima iz uzorka Druga Srbija (osnovne metrike)
metrika_DS <- naloziMeta(Druga_Srbija$Nalog)

metrika_DS <- metrika_DS[-c(7,9),] #izbacivanje informacija o neaktivnim nalozima (Biserko, Srbljanovic)

write.csv(metrika_DS, "metrika_DS.csv", row.names = F)

write.xlsx(metrika_DS, "metrika_DS.xlsx", row.names = F)

#3.5 Skidanje podataka o tvitovima uzorka u toku prvog posmatranog perioda
metrika_tvitovi_DS1 <- retfav(tvitovi_DS1)

write.csv(metrika_tvitovi_DS1, "metrika_tvitovi_DS1.csv", row.names = F)


#3.6 Skidanje podataka o tvitovima uzorka u toku posmatranog perioda
metrika_tvitovi_DS2 <- retfav(tvitovi_DS2)

write.csv(metrika_tvitovi_DS2, "metrika_tvitovi_DS2.csv", row.names = F)


#3.7 Pravljenje tabela za izvestaj za prvi period
tab_klas_met1 <- Druga_Srbija[-c(7, 9),c(1,2,4)]

met1 <- metrika_DS[,2:6]
tab_klas_met1 <- cbind(tab_klas_met1, met1[,1:4])
tab_klas_met1 <- cbind(tab_klas_met1, metrika_tvitovi_DS1[,2:5])

write.csv(tab_klas_met1, "tabela1.csv", row.names = F)
write.xlsx(tab_klas_met1, "tabela1.xlsx", row.names = F)

tab_DS1 <- cbind(Druga_Srbija[-c(7,9),], metrika_DS[, 6])

write.csv(tab_DS1, "tabela2.csv", row.names = F)

colnames(tab_klas_met1)[6] <- "Tvitovi_sve"

#3.8 Pravljenje tabela za izvestaj za drugi period
tab_klas_met2 <- Druga_Srbija[-c(7:9), c(1,2,4)]

met2 <- metrika_DS[-7,2:6]

tab_klas_met2 <- cbind(tab_klas_met2, met2[,1:4])
tab_klas_met2 <- cbind(tab_klas_met2, metrika_tvitovi_DS2[,2:5])

write.csv(tab_klas_met2, "tabela_drugiperiod1.csv", row.names = F)
write.xlsx(tab_klas_met2, "tabela_drugiperiod11.xlsx", row.names = F)

tab_DS2 <- cbind(Druga_Srbija[-c(7:9),], metrika_DS[-7, 6])

write.csv(tab_DS2, "tabela_drugiperiod2.csv", row.names = F)

colnames(tab_klas_met2)[6] <- "Tvitovi_sve"


#3.9 Vizualizacija broja tvitova po danu u dva perioda, udela profesija u uzorku i prosecnog retvita po profesiji

Druga_Srbija_filtrirano_1 <- Druga_Srbija[-c(7,9),]

Druga_Srbija_filtrirano_2 <- Druga_Srbija[-c(7:9),]

#Vizualizacija tvitova po danu prvi period
tvitovi_DS1$dan <- format(as.POSIXct(tvitovi_DS1$created, format = '%Y-%m-%d %H:%M:%S'), format = "%Y-%m-%d")

broj_po_danu1 <- tvitovi_DS1 %>%
  group_by (dan) %>%
  count()

colnames(broj_po_danu1)[2] <- "Broj_tvitova"

ggplot(broj_po_danu1[-1,], aes(x = dan , y = Broj_tvitova)) +
  geom_bar(stat = "identity") +
  labs(y = "Ukupno tvitova", x = "Dan", title = "Broj tvitova po danu za prvi period") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Vizualizacija tvitova po danu drugi period
tvitovi_DS2$dan <- format(as.POSIXct(tvitovi_DS2$created, format = '%Y-%m-%d %H:%M:%S'), format = "%Y-%m-%d")

broj_po_danu2 <- tvitovi_DS2 %>%
  group_by (dan) %>%
  count()

colnames(broj_po_danu2)[2] <- "Broj_tvitova"

ggplot(broj_po_danu2[-1,], aes(x = dan , y = Broj_tvitova)) +
  geom_bar(stat = "identity") +
  labs(y = "Ukupno tvitova", x = "Dan", title = "Broj tvitova po danu za drugi period") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

display.brewer.all(colorblindFriendly = T)

brewer.pal(n = 7, name = "Dark2")

#Vizualizacija udela profesija
Broj_profesija <- within(Druga_Srbija_filtrirano_1, 
                         Pozicija <- factor(Profesija, 
                                            levels=names(sort(table(Profesija), 
                                                              decreasing=TRUE))))

ggplot(Broj_profesija, aes(x = Pozicija)) +
  geom_bar(aes(fill = Profesija), colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Broj clanova", x = "Profesija", title= "Broj clanova po profesiji") +
  theme_bw(base_size = 30)


tab_klas_met1 <- tab_klas_met1 %>%
  mutate(Period = 1)

tab_klas_met2 <- tab_klas_met2 %>%
  mutate(Period = 2)


tab_klas_met_sve <- rbind(tab_klas_met1, tab_klas_met2)


pros_ret_profesije <- pros_ret_profesije %>%
  mutate(Period = "Period 1")

pros_ret_profesije2 <- pros_ret_profesije2 %>%
  mutate(Period = "Period 2")

pros_ret_profesije_sve <- rbind(pros_ret_profesije, pros_ret_profesije2)

#Vizualizacija prosecnog retvita po profesiji period 1
prof <- c(3, 11)

pros_ret_profesije <- tab_klas_met1[,prof] %>% 
  group_by(Profesija) %>%
  summarize(mean(Pros.Ret))

pros_ret_profesije <- arrange(pros_ret_profesije, desc(pros_ret_profesije$`mean(Pros.Ret)`))

ggplot(pros_ret_profesije_sve, aes(x = reorder(Profesija, -`mean(Pros.Ret)`), y = `mean(Pros.Ret)`, fill = Profesija)) +
  geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Prosecni retvit", x = "Profesija", title= "Prosecni retvit po profesiji za dva perioda") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_grid(cols = vars(Period))

#Vizualizacija prosecnog retvita po profesiji period 2
prof <- c(3, 11)

pros_ret_profesije2 <- tab_klas_met2[,prof] %>% 
  group_by(Profesija) %>% 
  summarize(mean(Pros.Ret))

pros_ret_profesije2 <- arrange(pros_ret_profesije2, desc(pros_ret_profesije2$`mean(Pros.Ret)`))

ggplot(pros_ret_profesije2, aes(x = reorder(Profesija, -`mean(Pros.Ret)`), y = `mean(Pros.Ret)`, fill = Profesija)) +
  geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Prosecni retvit", x = "Profesija", title= "Prosecni retvit po profesiji za drugi period") +
  theme_bw(base_size = 30)


#Vizualizacija ukupnog broja tvitova po profesiji period 1
uk_tvit_profesije1 <- uk_tvit_profesije1 %>%
  mutate(Period = "Period 1")

uk_tvit_profesije2 <- uk_tvit_profesije2 %>%
  mutate(Period = "Period 2")

uk_tvit_profesije_sve <- rbind(uk_tvit_profesije1, uk_tvit_profesije2)


prof_tvit <- c(3, 10)

uk_tvit_profesije1 <- tab_klas_met1[,prof_tvit] %>% 
  group_by(Profesija) %>% 
  summarize(sum(Tvitovi))

uk_tvit_profesije1 <- arrange(uk_tvit_profesije1, desc(uk_tvit_profesije1$`sum(Tvitovi)`))

ggplot(uk_tvit_profesije_sve, aes(x = reorder(Profesija, -`sum(Tvitovi)`), y = `sum(Tvitovi)`, fill = Profesija)) +
  geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Ukupno tvitova", x = "Profesija", title= "Ukupno tvitova po profesiji za dva perioda") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_grid(cols = vars(Period))

#Vizualizacija ukupnog broja tvitova po profesiji period 2
prof_tvit <- c(3, 10)

uk_tvit_profesije2 <- tab_klas_met2[,prof_tvit] %>% 
  group_by(Profesija) %>% 
  summarize(sum(Tvitovi))

uk_tvit_profesije2 <- arrange(uk_tvit_profesije2, desc(uk_tvit_profesije2$`sum(Tvitovi)`))

ggplot(uk_tvit_profesije2, aes(x = reorder(Profesija, -`sum(Tvitovi)`), y = `sum(Tvitovi)`, fill = Profesija)) +
  geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Ukupno tvitova", x = "Profesija", title= "Ukupno tvitova po profesiji za drugi period") +
  theme_bw(base_size = 30)

###################################################################################################################################
#4. PRAVLJENJE MATRICA POVEZANOSTI ZA DVA VREMENSKA PERIODA
###################################################################################################################################

matpov_period1 <- matrica_povezanosti(tvitovi_DS1)
matpov_period2 <- matrica_povezanosti(tvitovi_DS2)

###################################################################################################################################
#5. KREIRANJE GRAFOVA 
###################################################################################################################################

#5.1 Sastavljanje igraph objekta od matrica povezanosti za dva vremenska perioda

mreza_period1 <- graph.adjacency(matpov_period1, mode="directed", weighted = TRUE) 
mreza_period2 <- graph.adjacency(matpov_period2, mode="directed", weighted = TRUE)


#5.2 Dodeljivanje atributa cvorovima

V(mreza_period1)$Profesija <- Druga_Srbija_filtrirano_1$Profesija
V(mreza_period1)$prezime <- Druga_Srbija_filtrirano_1$Prezime
V(mreza_period1)$boja <- Druga_Srbija_filtrirano_1$Boja
V(mreza_period1)$skup <- Druga_Srbija_filtrirano_1$Skup
V(mreza_period1)$skup_boja <- Druga_Srbija_filtrirano_1$Skup.boja

V(mreza_period2)$Profesija <- Druga_Srbija_filtrirano_2$Profesija
V(mreza_period2)$prezime <- Druga_Srbija_filtrirano_2$Prezime
V(mreza_period2)$boja <- Druga_Srbija_filtrirano_2$Boja
V(mreza_period2)$skup <- Druga_Srbija_filtrirano_2$Skup
V(mreza_period2)$skup_boja <- Druga_Srbija_filtrirano_2$Skup.boja

###################################################################################################################################
#6. VIZUALIZACIJA GRAFOVA
###################################################################################################################################

#6.1 Vizualizacija mreze prvog perioda pomocu visNetwork-a
vis_period1 <- toVisNetworkData(mreza_period1)
head(vis_period1$nodes)
head(vis_period1$edges)

vis_period1$nodes <- vis_period1$nodes %>% rename(color = boja)
vis_period1$nodes <- vis_period1$nodes %>% rename(group = Profesija)
vis_period1$nodes <- vis_period1$nodes %>% select(-label)
vis_period1$nodes <- vis_period1$nodes %>% rename(label = prezime)
vis_period1$nodes$value <- degree(mreza_period1, mode = "total")

vis_period1$edges$arrows <- rep("to", 184)

oblici <- c("square", "triangle", "box", "circle", "star",
            "database", "diamond")

#Prva vizualizacija
visNetwork(nodes = vis_period1$nodes, edges = vis_period1$edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visNodes(color = "color", shape = oblici) %>%
  visGroups(groupname = "Analiticar", color = "#1B9E77", shape = "square") %>%
  visGroups(groupname = "Umetnik", color = "#A6761D", shape = "triangle") %>%
  visGroups(groupname = "Pravnik", color = "#66A61E", shape = "box") %>%
  visGroups(groupname = "Politicar", color = "#E7298A", shape = "circle") %>%
  visGroups(groupname = "NVO", color = "#7570B3", shape = "star") %>%
  visGroups(groupname = "Novinar", color = "#D95F02", shape = "database") %>%
  visGroups(groupname = "Profesor", color = "#E6AB02", shape = "diamond") %>%
  visOptions(selectedBy = "group") %>%
  visLegend()

kornes_period1 <- coreness(mreza_period1)

kornes_period2 <- coreness(mreza_period2)

#Vizualizacija podele intelektualci - inteligencija

vis_period1.2 <- toVisNetworkData(mreza_period1)
head(vis_period1.2$nodes)
head(vis_period1.2$edges)

vis_period1.2$nodes <- vis_period1.2$nodes %>% select(-boja)
vis_period1.2$nodes <- vis_period1.2$nodes %>% rename(color = skup_boja)
vis_period1.2$nodes <- vis_period1.2$nodes %>% rename(group = skup)
vis_period1.2$nodes <- vis_period1.2$nodes %>% select(-label)
vis_period1.2$nodes <- vis_period1.2$nodes %>% select(-Profesija)
vis_period1.2$nodes <- vis_period1.2$nodes %>% rename(label = prezime)
vis_period1.2$nodes$value <- degree(mreza_period1, mode = "total")

vis_period1.2$edges$arrows <- rep("to", 184)

shape <- c("triangle", "square")

visNetwork(nodes = vis_period1.2$nodes, edges = vis_period1.2$edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visNodes(color = "color") %>%
  visGroups(groupname = "Intelektualci", shape = "triangle", color = "#E1BE6A") %>%
  visGroups(groupname = "Inteligencija", shape = "square", color = "#40B0A6") %>%
  visOptions(selectedBy = "group") %>%
  visLegend()
                     
#Druga vizualizacija ("pokretna mreza")

visNetwork(nodes = vis_period1$nodes, edges = vis_period1$edges, width = "100%", height ="100vh") %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -100)) %>%
  visOptions(selectedBy = "group") 

#6.2 Vizualizacija mreze drugog perioda pomocu visNetwork-a

vis_period2 <- toVisNetworkData(mreza_period2)
head(vis_period2$nodes)
head(vis_period2$edges)

vis_period2$nodes <- vis_period2$nodes %>% rename(color = boja)
vis_period2$nodes <- vis_period2$nodes %>% select(-label)
vis_period2$nodes <- vis_period2$nodes %>% rename(group = Profesija)
vis_period2$nodes <- vis_period2$nodes %>% rename(label = prezime)
vis_period2$nodes$value <- degree(mreza_period2, mode = "total")

vis_period2$edges$arrows <- rep("to", 141)

#Prva vizualizacija
visNetwork(nodes = vis_period2$nodes, edges = vis_period2$edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visNodes(color = "color") %>%
  visGroups(groupname = "Analiticar", color = "#1B9E77") %>%
  visGroups(groupname = "Umetnik", color = "#A6761D") %>%
  visGroups(groupname = "Pravnik", color = "#66A61E") %>%
  visGroups(groupname = "Politicar", color = "#E7298A") %>%
  visGroups(groupname = "NVO", color = "#7570B3") %>%
  visGroups(groupname = "Novinar", color = "#D95F02") %>%
  visGroups(groupname = "Profesor", color = "#E6AB02") %>%
  visOptions(selectedBy = "group") %>%
  visLegend()

#Vizualizacija podele intelektualci - inteligencija

vis_period2.2 <- toVisNetworkData(mreza_period2)
head(vis_period2.2$nodes)
head(vis_period2.2$edges)

vis_period2.2$nodes <- vis_period2.2$nodes %>% select(-boja)
vis_period2.2$nodes <- vis_period2.2$nodes %>% rename(color = skup_boja)
vis_period2.2$nodes <- vis_period2.2$nodes %>% rename(group = skup)
vis_period2.2$nodes <- vis_period2.2$nodes %>% select(-label)
vis_period2.2$nodes <- vis_period2.2$nodes %>% select(-Profesija)
vis_period2.2$nodes <- vis_period2.2$nodes %>% rename(label = prezime)
vis_period2.2$nodes$value <- degree(mreza_period2, mode = "total")

vis_period2.2$edges$arrows <- rep("to", 141)


visNetwork(nodes = vis_period2.2$nodes, edges = vis_period2.2$edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visNodes(shape = shape) %>%
  visGroups(groupname = "Intelektualci", shape = "triangle", color = "#E1BE6A") %>%
  visGroups(groupname = "Inteligencija", shape = "square", color = "#40B0A6") %>%
  visOptions(selectedBy = "group") %>%
  visLegend()

#Druga vizualizacija ("pokretna mreza")
visNetwork(nodes = vis_period2$nodes, edges = vis_period2$edges, width = "100%", height ="100vh") %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -100)) %>%
  visOptions(selectedBy = "group")  

#############################################################################
#7. IZRACUNAVANJE MREZNIH METRIKA
#############################################################################

#7.1 Uklanjanje naloga koji ne pripadaju klasterima u dva perioda
brisanje_period1 <- "NedimSejdinovic"
mreza_period1_skr <- delete.vertices(mreza_period1, brisanje_period1)


brisanje_period2 <- c("NedimSejdinovic", "ZoranDirektno", "YIHRSerbia", "Ceda_Jovanovic", "NDNV", "autonomijandnv")
mreza_period2_skr <- delete.vertices(mreza_period2, brisanje_period2)

#7.2 Globalne metrike mreze za dva perioda

glob_met_period1 <- met_graf(mreza_period1_skr)
glob_met_period2 <- met_graf(mreza_period2_skr)  

glob_met_period1
glob_met_period2

get.vertex.attribute(mreza_period1, "Profesija")
get.vertex.attribute(mreza_period2, "Profesija")

#7.3 Metrike centralnosti cvorova za prvi period

met_cent_period1 <- met_cent(mreza_period1_skr)

met_cent_period1

write.csv(met_cent_period1, "Cent_period1.csv", row.names = T)
write.xlsx(met_cent_period1, "Cent_period1.xlsx", row.names = F)

#7.4 Metrike centralnosti cvorova za drugi period

met_cent_period2 <- met_cent(mreza_period2_skr)

met_cent_period2

write.csv(met_cent_period2, "Cent_period2.csv", row.names = T)
write.xlsx(met_cent_period2, "Cent_period2.xlsx", row.names = F)

#############################################################################
#8. IZRACUNAVANJE KORELACIJE IZMEDJU SVIH METRIKA (MREZNIH I KLASICNIH)
#############################################################################

#Pravljenje dejtafrejma koji sadrzi: 1) metrike centralnosti cvorova uzorka
#2) klasicne metrike naloga i 3) metrike naloga za posmatrani period
#bez naloga koji ne pripada dominatnom klasteru

#8.2 Period 1

metrika_period1_skr <- metrika_DS %>%
  filter(!(metrika_DS$Nalog %in% brisanje_period1))

metrika_tvitovi_period1_skr <- metrika_tvitovi_DS1 %>%
  filter(!(metrika_tvitovi_DS1$Nalog %in% brisanje_period1))

#Sortiranje sve tri tabele po nalogu abecedno jer su u met_cent_period1
#nalozi row names i nisu po redu

d1 <- met_cent_period1
Nalog <- rownames(d1)
rownames(d1) <- NULL
met_cent_period1 <- cbind(Nalog,d1)

mcp1 <- arrange(met_cent_period1, met_cent_period1$Nalog)
mp1s <- arrange(metrika_period1_skr, metrika_period1_skr$Nalog)
mtp1s <- arrange(metrika_tvitovi_period1_skr, metrika_tvitovi_period1_skr$Nalog)

mcp1$Profesija <- Period1_skr$Profesija

#Pravljenje tabele sa svim pokazateljima naloga u dominantnom klasteru za period 1

svipokazatelji_period1 <-
  cbind(mcp1[,-1], #metrike centralnosti grafa
        mp1s[,-1], #metrike naloga na Tviteru
        mtp1s[,-1]) #metrika naloga za ukupan period

#Izracunavanje starosti naloga u nedeljama
svipokazatelji_period1$DatumOtvaranja <-
  as.numeric(abs(
    difftime(svipokazatelji_period1$DatumOtvaranja, Sys.time(), units = "weeks")
  ))

#Vizualizacija matrice korelacija svih pokazatelja
mat_cor_sve1_period1 <- cor(svipokazatelji_period1, method = "spearman")

mat_cor_sve_period1 <- cor.mtest(svipokazatelji_period1, method = "spearman", conf.level = 0.95)$p

corrplot(mat_cor_sve1_period1, p.mat= mat_cor_sve_period1, type = "upper", method = "number", sig.level = 0.5, diag = FALSE)


#Izracunavanje sveukupne centralnosti prema profesiji za mreza_period1_skr

`%not_in%` <- purrr::negate(`%in%`) #kreiranje nove funkcije zbog problema sa postojecim relacionim operatorom (!=)

Period1_skr <- Druga_Srbija_filtrirano_1 %>%
  filter(Nalog %not_in% brisanje_period1)

Period1_skr <- arrange(Period1_skr, Period1_skr$Nalog)

setdiff(Period1_skr$Nalog, mcp1$Nalog) 

g1 <- aggregate(
  x = mcp1$P2odstojanje,
  FUN = sum,
  by = list(
    Period1_skr$Profesija)
)
g1$Broj <- as.numeric(table(Period1_skr$Profesija))
colnames(g1) <- c("Profesija", "Centralnost", "Broj")
g1$CentStand <- g1$Centralnost / g1$Broj

g1

g1_ord <- g1[order(- g1$CentStand), ]

g1_ord

#Vizualizacija metrika centralnosti po profesiji za prvi perioda
ggplot(g1_ord, aes(x = reorder(Profesija, -CentStand), y = CentStand, fill = Profesija)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Prosecna sveukupna centralnost", x = "Profesija", title= "Prosecna sveukupna centralnost Period1") +
  theme_bw(base_size = 30)

ggplot(mcp1, aes(x = Profesija, y = P2odstojanje, fill = Profesija)) +
  geom_boxplot(show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "P2 odstojanje", x = "Profesija", title= "Raspodela vrednosti P2 odstojanja unutar profesija Period1") +
  theme_bw(base_size = 30)


#Period 2

metrika_period2_skr <- metrika_DS %>%
  filter(!(metrika_DS$Nalog %in% brisanje_period2))

metrika_period2_skr <- metrika_period2_skr %>%
  filter(metrika_period2_skr$Nalog != "Jovanana")

metrika_tvitovi_period2_skr <- metrika_tvitovi_DS2 %>%
  filter(!(metrika_tvitovi_DS2$Nalog %in% brisanje_period2))

#Sortiranje sve tri tabele po nalogu abecedno jer su u met_cent_period2
#nalozi row names i nisu po redu

d2 <- met_cent_period2
Nalog <- rownames(d2)
rownames(d2) <- NULL
met_cent_period2 <- cbind(Nalog,d2)

mcp2 <- arrange(met_cent_period2,met_cent_period2$Nalog)
mp2s <- arrange(metrika_period2_skr, metrika_period2_skr$Nalog)
mtp2s <- arrange(metrika_tvitovi_period2_skr, metrika_tvitovi_period2_skr$Nalog)

mcp2$Profesija <- Period2_skr$Profesija

#Pravljenje tabele sa svim pokazateljima naloga u dominantnom klasteru za period 2

svipokazatelji_period2 <-
  cbind(mcp2[,-1], #metrike centralnosti grafa
        mp2s[,-1], #metrike naloga na Tviteru
        mtp2s[,-1]) #metrika naloga za ukupan period

#Izracunavanje starosti naloga u nedeljama
svipokazatelji_period2$DatumOtvaranja <-
  as.numeric(abs(
    difftime(svipokazatelji_period2$DatumOtvaranja, Sys.time(), units = "weeks")
  ))

#Vizualizacija matrice korelacija svih pokazatelja
mat_cor_sve1_period2 <- cor(svipokazatelji_period2, method = "spearman")

mat_cor_sve_period2 <- cor.mtest(svipokazatelji_period2, method = "spearman", conf.level = 0.95)$p

corrplot(mat_cor_sve1_period2, p.mat= mat_cor_sve_period2, type = "upper", method = "number", sig.level = 0.5, diag = FALSE)

### Izracunavanje sveukupne centralnosti prema profesiji za mreza_period2_skr

Period2_skr <- Druga_Srbija_filtrirano_2 %>%
  filter(Nalog %not_in% brisanje_period2)

Period2_skr <- arrange(Period2_skr, Period2_skr$Nalog)

setdiff(Period2_skr$Nalog, mcp2$Nalog) 

g2 <- aggregate(
  x = mcp2$P2odstojanje,
  FUN = sum,
  by = list(
    Period2_skr$Profesija)
)
g2$Broj <- as.numeric(table(Period2_skr$Profesija))
colnames(g2) <- c("Profesija", "Centralnost", "Broj")
g2$CentStand <- g2$Centralnost / g2$Broj

g2

g2_ord <- g2[order(- g2$CentStand), ]

g2_ord

#Vizualizacija metrika centralnosti po profesiji za mrezu drugog perioda

ggplot(g2_ord, aes(x = reorder(Profesija, -CentStand), y = CentStand, fill = Profesija)) +
  geom_bar(stat = "identity", width = 0.5, show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Prosecna sveukupna centralnost", x = "Profesija", title= "Prosecna sveukupna centralnost Period2") +
  theme_bw(base_size = 30)

ggplot(mcp2, aes(x = Profesija, y = P2odstojanje, fill = Profesija)) +
  geom_boxplot(show.legend = F) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "P2 odstojanje", x = "Profesija", title= "Raspodela vrednosti P2 odstojanja unutar profesija Period2") +
  theme_bw(base_size = 30)

###########################################################################################################################
#9. PREGLED TROUGLOVA I KLIKA
###########################################################################################################################

#9.1 Pregled maksimalnog broja klika i najvecih klika za dva vremenska perioda

max_cliques(mreza_period1_skr)
largest_cliques(mreza_period1_skr)

max_cliques(mreza_period2_skr)
largest_cliques(mreza_period2_skr)


#9.2 Pregled trouglova za dva vremenska perioda

svi_nalozi_period1 <- unique(tvitovi_DS1$screenName)
trouglovi_period1 <- count_triangles(mreza_period1, vids = svi_nalozi_period1)
names(trouglovi_period1) <- svi_nalozi_period1

svi_nalozi_period2 <- unique(tvitovi_DS2$screenName)
trouglovi_period2 <- count_triangles(mreza_period2, vids = svi_nalozi_period2)
names(trouglovi_period2) <- svi_nalozi_period2

###########################################################################################################################
#10. PRORACUN HOMOFILIJE
##########################################################################################################################

#Napomena - u ovom delu rada se autor oslonio na kod koji je razvio Marko Galjak (Galjak, 2020)

Advokati <- c("olenikadvokat", "misamajic")

#10.2 Homofilija Period 1

mreza_period1_ba <- delete.vertices(mreza_period1_skr, Advokati)

homofilija <- vector()
tranzitivnost <- vector()
for (i in 1:length(unique(V(mreza_period1_ba)$Profesija))) {
  x <- subgraph.edges(mreza_period1_ba, 
                      as.vector(E(mreza_period1_ba)[V(mreza_period1_ba)
                                                    [V(mreza_period1_ba)$Profesija == unique(V(mreza_period1_ba)$Profesija)[i]] %--%
                                                      1:length(V(mreza_period1_ba))]))
  x <- as.undirected(x, mode = "mutual")
  x <- delete.vertices(x, v=degree(x)==0)
  V(x)$Profesija <- as.factor(V(x)$Profesija)
  homofilija[i] <- assortativity.nominal(x, types = V(x)$Profesija)
  tranzitivnost[i] <- transitivity(x)
  intermed <- rep(NA, vcount(x))
  intermed[which(betweenness(x) == max(betweenness(x)))] <- "#ff5658"
  svg(
    paste0(unique(V(mreza_period1_ba)$Profesija)[i], ".svg"),
    height = 4.13,
    family = "Arial"
  )
  plot(
    x,
    edge.curved = F,
    layout = layout.davidson.harel(
      x,
      weight.node.dist = 70,
      weight.node.edge.dist = 70
    ),
    edge.width = log(E(x)$weight),
    vertex.color = ifelse(V(x)$boja==unique(V(mreza_period1_ba)$boja)[i],  
                          unique(V(mreza_period1_ba)$boja)[i], "#bdbdbd"),
    vertex.label.color = ifelse(is.na(intermed), "black", "red"),
    vertex.label.dist = 1.3,
    vertex.frame.color = intermed,
    vertex.label = V(x)$prezime,
    vertex.size = rescale(x=degree(x), to=c(10,30)),
    vertex.label.cex = rescale(x=degree(x), to=c(0.8,1.2)),
    vertex.label.family = "Arial",
    main = unique(V(mreza_period1_ba)$Profesija)[i]
  )
  text(0.7, 1.6, paste("r = ", gsub("*\\.", ",", as.character(
    round(homofilija[i], 2)
  ))), pos = 4)
  text(0.7, 1.4, paste("T = ", gsub("*\\.", ",", as.character(
    round(tranzitivnost[i], 2)
  ))), pos = 4)
  dev.off()
}

#10.3 Homofilija Period 2

homofilija <- vector()
tranzitivnost <- vector()
for (i in 1:length(unique(V(mreza_period2)$Profesija))) {
  x <- subgraph.edges(mreza_period2, 
                      as.vector(E(mreza_period2)[V(mreza_period2)
                                                    [V(mreza_period2)$Profesija == unique(V(mreza_period2)$Profesija)[i]] %--%
                                                      1:length(V(mreza_period2))]))
  x <- as.undirected(x, mode = "mutual")
  x <- delete.vertices(x, v=degree(x)==0)
  V(x)$Profesija <- as.factor(V(x)$Profesija)
  homofilija[i] <- assortativity.nominal(x, types = V(x)$Profesija)
  tranzitivnost[i] <- transitivity(x)
  intermed <- rep(NA, vcount(x))
  intermed[which(betweenness(x) == max(betweenness(x)))] <- "#ff5658"
  svg(
    paste0(unique(V(mreza_period2)$Profesija)[i], ".svg"),
    height = 4.13,
    family = "Arial"
  )
  plot(
    x,
    edge.curved = F,
    layout = layout.davidson.harel(
      x,
      weight.node.dist = 70,
      weight.node.edge.dist = 70
    ),
    edge.width = log(E(x)$weight),
    vertex.color = ifelse(V(x)$boja==unique(V(mreza_period2)$boja)[i],  
                          unique(V(mreza_period2)$boja)[i], "#bdbdbd"),
    vertex.label.color = ifelse(is.na(intermed), "black", "red"),
    vertex.label.dist = 1.3,
    vertex.frame.color = intermed,
    vertex.label = V(x)$prezime,
    vertex.size = rescale(x=degree(x), to=c(10,30)),
    vertex.label.cex = rescale(x=degree(x), to=c(0.8,1.2)),
    vertex.label.family = "Arial",
    main = unique(V(mreza_period2)$Profesija)[i]
  )
  text(0.7, 1.6, paste("r = ", gsub("*\\.", ",", as.character(
    round(homofilija[i], 2)
  ))), pos = 4)
  text(0.7, 1.4, paste("T = ", gsub("*\\.", ",", as.character(
    round(tranzitivnost[i], 2)
  ))), pos = 4)
  dev.off()
}

###########################################################################################################
#11. ANALIZA KLASTERA
###########################################################################################################

#11.1 Primena funkcije klast_eval() na (neusmerenu) mrezu dva perioda i dostupne klaster metode

metode <-
  c(
    "cluster_edge_betweenness",
    "cluster_label_prop",
    "cluster_spinglass",
    "cluster_walktrap",
    "cluster_infomap",
    "cluster_optimal"
  )

#Period 1

for (i in 1:length(metode)) {
  print(paste(metode[i], klast_eval(as.undirected(mreza_period1_skr), "Profesija", metode[i])))
}

#11.2 Primena razlièitih klaster metoda 

klaster_period1_wt <- cluster_walktrap(mreza_period1_skr)

klaster_period1_im <- cluster_infomap(mreza_period1_skr)

klaster_period1_eb <- cluster_edge_betweenness(mreza_period1_skr)

klaster_period1_lp <- cluster_label_prop(mreza_period1_skr)

klaster_period1_sg <- cluster_spinglass(mreza_period1_skr)

klaster_period1_op <- cluster_optimal(mreza_period1_skr)

membership(klaster_period1_op)


#11.3. Vizualizacija klastera pomocu igraph paketa 

#Funkcija za generisanje rasporeda na osnovu klastera  Izvor: (Turei, 2013) prema (Galjak, 2017) i (Nikolic, 2020)


layout.modular <- function(graf, klaster) {
  graf$layout <- layout.auto(graf) #inicijalizacija rasporeda
  nm <-
    length(levels(as.factor(klaster$membership))) #broja klastera
  gr <- 2
  while (gr ^ 2 < nm) {
    #u koliko redova ce klasteri biti prikazani
    gr <- gr + 1
  }
  i <- j <- 0
  for (cc in levels(as.factor(klaster$membership))) {
    #za svaki klaster
    F <-
      delete.vertices(graf, klaster$membership != cc) #samo klaster
    F$layout <- layout.kamada.kawai(F)
    F$layout <-
      layout.norm(F$layout, i, i + 0.5, j, j + 0.5) #skaliran raspored unutar klastera
    graf$layout[klaster$membership == cc,] <- F$layout
    if (i == gr) {
      #brojaci na za polozaj cvorova u rasporedu
      i <- 0
      if (j == gr) {
        j <- 0
      } else{
        j <- j + 1
      }
    } else{
      i <- i + 1
    }
  }
  return(graf$layout)
}


#11.4 Vizualizacija mreze prvog perioda sa klasterima

graspored1 <- layout.modular(mreza_period1_skr, klaster_period1_op)

plot.igraph(
  mreza_period1_skr,
  layout = graspored1,
  edge.width = log(E(mreza_period1_skr)$weight),
  edge.curved = FALSE,
  edge.color = alpha("gray", rescale(
    x = log(E(mreza_period1_skr)$weight), to = c(0.3, 1)
  )),
  edge.arrow.size = 0.1,
  edge.lty = 1,
  vertex.color = V(mreza_period1_skr)$boja,
  vertex.label.color = "black",
  vertex.label.family = "Arial",
  vertex.frame.color = "black",
  vertex.label = V(mreza_period1_skr)$prezime,
  vertex.size = rescale(degree(mreza_period1_skr), to = c(5, 15)),
  vertex.label.cex = 0.7,
  vertex.label.dist = 0.5,
  mark.groups = klaster_period1_op
)

#Period 2

for (i in 1:length(metode)) {
  print(paste(metode[i], klast_eval(as.undirected(mreza_period2_skr), "Profesija", metode[i])))
}

#11.2 Primena Infomap klaster algoritama na mrezu drugog perioda

klaster_period2_wt <- cluster_walktrap(mreza_period2_skr)

klaster_period2_im <- cluster_infomap(mreza_period2_skr)

klaster_period2_eb <- cluster_edge_betweenness(mreza_period2_skr)

klaster_period2_lp <- cluster_label_prop(mreza_period2_skr)

klaster_period2_sg <- cluster_spinglass(mreza_period2_skr)

klaster_period2_op <- cluster_optimal(mreza_period2_skr)

membership(klaster_period2_op)


#11.3. Vizualizacija klastera pomocu igraph paketa 

graspored2 <- layout.modular(mreza_period2_skr, klaster_period2_op)

plot.igraph(
  mreza_period2_skr,
  layout = graspored2,
  edge.width = log(E(mreza_period2_skr)$weight),
  edge.curved = FALSE,
  edge.color = alpha("gray", rescale(
    x = log(E(mreza_period2_skr)$weight), to = c(0.3, 1)
  )),
  edge.arrow.size = 0.1,
  edge.lty = 1,
  vertex.color = V(mreza_period2_skr)$boja,
  vertex.label.color = "black",
  vertex.label.family = "Arial",
  vertex.frame.color = "black",
  vertex.label = V(mreza_period2_skr)$prezime,
  vertex.size = rescale(degree(mreza_period2_skr), to = c(5, 15)),
  vertex.label.cex = 0.7,
  vertex.label.dist = 0.5,
  mark.groups = klaster_period2_op
)
############################################################################################################
#12. VIZUALIZACIJA TEKSTA
############################################################################################################

tvitovi_DS_sve <- rbind(tvitovi_DS1, tvitovi_DS2)

tvitovi_DS_sve_tekst <- tvitovi_DS_sve$text

tvitovi_korpus <- tvitovi_DS_sve_tekst %>%
  VectorSource() %>%
  Corpus()

tvitovi_korpus_mala <- tm_map(tvitovi_korpus, tolower)
tvitovi_korpus_mala <- tm_map(tvitovi_korpus_mala, removeWords, zaustavne)
tvitovi_korpus_mala <- tm_map(tvitovi_korpus_mala, removeWords, zaustavne2)
tvitovi_korpus_mala <- tm_map(tvitovi_korpus_mala, removeWords, filtriranje)


zaustavne <- c("u", "da", "i", "je", "se", "f", "ne", "na", "a", "to", "e", "za", "sa", "su", "httpst","httpstco", "od",
               "o", "sam", "nije", "ali", "du", "ja", "mi", "koji", "sve", "au", "pa", "fu", "samo", "iz", "kao",
               "the", "kako", "bu", "bi", "ako", "ima", "ovo", "kad", "ni", "ko", "cu", "smo", "ste", "c", "tako",
               "ili", "li", "po", "ti", "b", "on", "of", "me", "nisam", "sad", "vi", "d", "do", "kada", "znam", "bilo",
               "s", "što", "bio", "ga", "ae", "vas", "in", "vam", "jer", "sta", "ata", "koje", "tu", "nas", "and",
               "nego", "svi", "n", "si", "nisu", "meni", "dobro", "koja", "ih", "te", "mu", "oni", "treba", "ee", "mou",
               "mou", "ie", "zbog", "neko", "bih", "gde", "mogu", "šta", "onda", "no", "sada", "pre", "malo", "evo",
               "koliko", "im", "ovde", "biti", "uvek", "mislim", "vee", "bez", "æe", "is", "ok", "ovaj", "mnogo",
               "kod", "ma", "zato", "mene", "bila", "jedna", "vue", "bude", "nau", "naravno", "moj", "nam", "for",
               "tome", "neka", "h", "taj", "jeste", "moze", "ce", "dok", "posle", "g", "k", "kau", "neki", "niko",
               "mora", "vise", "oko", "protiv", "vau", "l", "m", "j", "bas", "jou", "isto", "jos", "bili", "ono", "viu",
               "koju", "vec", "još", "v", "postoji", "toga", "vidim", "p", "...", "ove", "dve", "iu", "dalje", "dru", "zna")

zaustavne2 <- c("f", "a", "e", "rt", "httpst", "d", "...", "b", "c", "š", "fe", "nema", "o", "ž", "n", "jedan", "g",
                "m", "p", "v", "j", "z", "s", "l", "nakon", "podr", "k", "y", "imam", "molim", "h", "prvi", "u",
                "toliko", "imaju", "rekao", "w", "q", "that", "this", "t", "bolje", "nemam", "r", "tamo", "niti",
                "novi", "znate", "njih", "i", "imamo", "niste", "svoje", "ovog", "ovako", "with", "koga", "prvo", 
                "x", "dosta", "najbolje", "nikad", "vama", "zato", "izgleda", "prema", "moja", "neke", "fa", "preko",
                "nista", "recanja", "stvarno", "svako", "opet", "drugi", "hedi", "jednom", "nikad", "puta", "nismo",
                "vidi", "isti", "stvari", "svaki", "mozda", "nesto", "stvar", "dobar", "imali", "jednog", "rade",
                "veoma", "verujem", "upravo", "dfe", "from", "pred", "mislite", "jasno", "moje", "nove", "veliki",
                "ipak", "jako", "odmah", "imao", "njega", "imate", "kakav", "osim", "nama", "nekoliko", "svih", "onaj",
                "moram", "puno", "sebe", "vidimo", "zanima", "drugo")


filtriranje <- c("f", "a", "e", "rt", "httpst", "d", "…", "b", "c", "š", "fe", "hvala", "o", "n", "ž", "g", "j",
                 "m", "l", "z", "p", "s", "v", "h", "w", "u", "y", "k", "q", "r", "i", "x", "t", "radi", "zaista",
                 "veze", "sutra", "vlasti", "lepo", "fa", "hedi", "zasto", "dfe", "bravo", "politi", "razumem",
                 "podr", "nikada", "tekst", "uprkos", "svoj","ea", "druge", "svoju", "ovih", "kome", "dobra", "ikan",
                 "manje", "vrlo", "will", "umesto", "about", "jedno", "postoje", "pravi", "trebalo", "vodi", "potpuno",
                 "svaka", "svojim", "nadam", "dovoljno", "gledam", "kroz", "miliona", "nekog", "pitam", "znaju", "iako",
                 "tema", "zajedno", "budu", "kakva", "rekla", "sigurno", "strane", "tada", "“", "druga", "kraj", "pogledajte",
                 "pored", "pravu", "jedini", "kojima", "nisi", "ovom", "dakle", "mnogi", "svakako", "tokom", "volim", "nova", "izmeðu", "nemaju", "svim",
                 "velika", "broj", "govori", "hajde", "jednu", "kojoj", "pola", "ispod", "jedino", "lako", "neæe", "normalno", "pitanja", "prva", "vremena",
                 "zanimljivo", "reci", "skoro", "svima", "valjda", "dobio", "dragi", "have", "kakve", "ponovo", "posla", "primer", "mesto", "moraju", "ovim", "valja", "vidite", 
                 "srdjandrago", "ksenija", "biancoroso", "kazitibi", "veceivan", "slobageorgiev", "vristecav", "gujkan",
                 "newsmaxadriabg", "maremilanoit", "infobg", "marinikatepic", "vestomatomy", "jelenalengold", "dusicakrasojev",
                 "palihboraca", "bjutidingospo", "djordjenspm", "tanjahenry", "purujarvi", "urosvasiljevic", "ljubofil", "antonmilan",
                 "rihaantonela", "krckole", "novastranka", "avucic", "batta", "demokrate", "almarmc", "mosimisemac", "izvucicemose",
                 "paunovicsasa", "tvnovas", "ceasserbia", "zoranlutovac", "amfetaminolohie", "jelmilosevic", "pajticbojan",
                 "onlinedanas", "slobodaipravda", "iggyb", "petarbeljic", "sutanovacdragan", "borkostef", "balshone", "dragandjilas",
                 "kubatrodoljub", "pravachamburg", "vladislavdimi", "svepamtilica", "tonistankovic", "wiznews", "usranemotke", "boristadic",
                 "radosav", "daviddbabic", "goodneighbor", "mikimarjanovic", "predragbrajovic", "rakicv", "kosovoonline", "titogradjaninm",
                 "usernamens", "balkangp", "brojevimaja", "jovanana", "osfserbia", "vpavicevic", "vladimirgajic", "æemo", "š…", "milanstrongman",
                 "zikaziv", "taèno", "krezavasrbija", "marymerlak", "bojanamala", "ispred", "stevanoccrp", "super", "hitno", "nitropolit",
                 "tomdeer", "žš", "brankicast", "gjesic", "kaolimunzut", "nodogma", "onih", "peckopivo", "otpisana", "avgusta", "serbianinfidel", "spreèavanje", "gori",
                 "ovek", "gore", "odli", "https", "poku", "aingtonu", "izve", "pavlegrbovic", "bospolicylab", "ukoliko", "vesi", "zavr", "atouchofspice", "jaopet",
                "spahulja", "bulajicsnezana", "eivot", "izvuèiæemose", "avuèiæ")


profili <- c("srdjandrago", "ksenija", "biancoroso", "kazitibi", "veceivan", "slobageorgiev", "vristecav", "gujkan",
             "newsmaxadriabg", "maremilanoit", "infobg", "marinikatepic", "vestomatomy", "jelenalengold", "dusicakrasojev",
             "palihboraca", "bjutidingospo", "djordjenspm", "tanjahenry", "purujarvi", "urosvasiljevic", "ljubofil", "antonmilan",
             "rihaantonela", "krckole", "novastranka", "avucic", "batta", "demokrate", "almarmc", "mosimisemac", "izvucicemose",
             "paunovicsasa", "tvnovas", "ceasserbia", "zoranlutovac", "amfetaminolohie", "jelmilosevic", "pajticbojan",
             "onlinedanas", "slobodaipravda", "iggyb", "petarbeljic", "sutanovacdragan", "borkostef", "balshone", "dragandjilas",
             "kubatrodoljub", "pravachamburg", "vladislavdimi", "svepamtilica", "tonistankovic", "wiznews", "usranemotke", "boristadic",
             "radosav", "daviddbabic", "goodneighbor", "mikimarjanovic", "predragbrajovic", "rakicv", "kosovoonline", "titogradjaninm",
             "usernamens", "balkangp", "brojevimaja", "jovanana", "osfserbia", "vpavicevic")

reci_broj <- freq_terms(tvitovi_korpus_mala, 200)

profili_broj <- reci_broj %>%
  filter(WORD %in% profili)

reci_broj <- reci_broj %>%
  filter(WORD %not_in% filtriranje)

wordcloud(profili_broj$WORD, profili_broj$FREQ, scale = c(2.5, 1), random.order = FALSE, colors = brewer.pal(7, "Dark2"))

wordcloud(reci_broj$WORD, reci_broj$FREQ, scale = c(3, 1), random.order = FALSE, colors = brewer.pal(7, "Dark2"))

write.xlsx(profili_broj, "profili broj.xlsx", row.names = FALSE)

ggplot(profili_broj, aes(reorder(x = WORD, -FREQ), y = FREQ)) +
  geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Frekventnost", x = "Nalog", title= "Frekventnost pojavljivanja naloga") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## Period1 ##
 
tvitovi_DS1_tekst <- tvitovi_DS1$text

tvitoviDS1_korpus <- tvitovi_DS1_tekst %>%
  VectorSource() %>%
  Corpus()

tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus, tolower)
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, removeWords, zaustavne)
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, removeWords, zaustavne2)
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, content_transformer(function(x) gsub(x, pattern = "godine", replacement = "godina")))
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, content_transformer(function(x) gsub(x, pattern = "srbije", replacement = "srbija")))
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, content_transformer(function(x) gsub(x, pattern = "srbiji", replacement = "srbija")))
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, content_transformer(function(x) gsub(x, pattern = "vucic", replacement = "vuèiæ")))
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, content_transformer(function(x) gsub(x, pattern = "vuèiæa", replacement = "vuèiæ")))
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, content_transformer(function(x) gsub(x, pattern = "covek", replacement = "èovek")))
tvitoviDS1_korpus_mala <- tm_map(tvitoviDS1_korpus_mala, content_transformer(function(x) gsub(x, pattern = "predsednika", replacement = "predsednik")))


reci_broj_period1 <- freq_terms(tvitoviDS1_korpus_mala, 200)

reci_broj_period1 <- reci_broj_period1 %>%
  filter(WORD %not_in% filtriranje)

wordcloud(reci_broj_period1$WORD, reci_broj_period1$FREQ, scale = c(4, 1), random.order = FALSE, colors = brewer.pal(7, "Dark2"))

write.xlsx(reci_broj_period1, "reci period1.xlsx", row.names = FALSE)

reci_broj_period1_30 <- reci_broj_period1 %>%
  filter(FREQ >= 33)

ggplot(reci_broj_period1, aes(reorder(x = WORD, -FREQ), y = FREQ)) +
  geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Frekventnost", x = "Reè", title= "Frekventnost pojavljivanja reèi prvi period") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


dtm_period1 <- DocumentTermMatrix(tvitovi_DS1_filtrirano)

rowTotals_period1 <- apply(dtm_period1, 1, sum)

## Period2 ##

tvitovi_DS2_tekst <- tvitovi_DS2$text

tvitoviDS2_korpus <- tvitovi_DS2_tekst %>%
  VectorSource() %>%
  Corpus()

tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus, tolower)
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, removeWords, zaustavne)
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, removeWords, zaustavne2)
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "sporazuma", replacement = "sporazum")))
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "srbije", replacement = "srbija")))
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "srbiji", replacement = "srbija")))
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "srbiju", replacement = "srbija")))
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "srbiji", replacement = "srbija")))
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "vlade", replacement = "vlada")))
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "predsednika", replacement = "predsednik")))
tvitoviDS2_korpus_mala <- tm_map(tvitoviDS2_korpus_mala, content_transformer(function(x) gsub(x, pattern = "godine", replacement = "godina")))

reci_broj_period2 <- freq_terms(tvitoviDS2_korpus_mala, 200)

reci_broj_period2 <- reci_broj_period2 %>%
  filter(WORD %not_in% filtriranje)

wordcloud(reci_broj_period2$WORD, reci_broj_period2$FREQ, scale = c(3, 1), random.order = FALSE, colors = brewer.pal(7, "Dark2"))

write.xlsx(reci_broj_period2, "reci period2.xlsx", row.names = FALSE)

reci_broj_period2_30 <- reci_broj_period2 %>%
  filter(FREQ >= 33)

ggplot(reci_broj_period2, aes(reorder(x = WORD, -FREQ), y = FREQ)) +
  geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Frekventnost", x = "Reè", title= "Frekventnost pojavljivanja reèi drugi period") +
  theme_bw(base_size = 30) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

############################################################################################################
################################################# KRAJ #####################################################
############################################################################################################