#1 ekstrakcija_er()

#Funkcija za prikupljanje tvitova sa vise naloga, tvitovanih u izabranom vremenskom periodu
#Funkcija ekstrakcija() kao ulaz prima spisak naloga, i dva vremenska perioda pocetak i kraj
#vremenskog intervala za koji zelimo tvitove (Galjak, 2017). Ako naidje na privatni profil, 
#ne prekida lup erorom, vec ga samo preskace (er - error resistant).
ekstrakcija_er <- function(uzorak, pocetak = NULL, kraj = NULL) 
{                                                                             
  ekstrakcija <- function(spisak, od = NULL, do = NULL) {                       
    tvitovi <- data.frame()
    for (i in 1:length(spisak)) {
      a <- userTimeline(spisak [i], n = 3200, includeRts = TRUE)
      a <- twListToDF(a)
      if (!is.null(od)) {
        a <- a[a$created > od,]
      }
      if (!is.null(do)) {
        a <- a[a$created < do,]
      }
      tvitovi <- rbind(tvitovi, a)
      if (as.numeric(getCurRateLimitInfo()[39, 3]) < 5) {
        Sys.sleep(60 * as.numeric(getCurRateLimitInfo()[39, 4] - Sys.time()) + 10)
      }
    }
    return(tvitovi)
  }                                                                             
  neuspeh <- c()                                                                
  tw <- data.frame()                                                                                                                      
  for (i in 1:length(uzorak))                                                                       
  {
    if 
    (class(try(b <- ekstrakcija(uzorak[i], od = pocetak, do = kraj), silent = T)) == "try-error")
    {neuspeh <- c(neuspeh, uzorak[i])}
    else
    {b <- ekstrakcija(uzorak[i], od = pocetak, do = kraj)
    tw <- rbind(tw, b)
    }
  }                                                                                                                                                                     
  return(tw)                                                                    
}    


#2 naloziMeta()

#Funkcija naloziMeta skida meta podatke o svakom profilu iz definisanog uzorka (Galjak, 2017)
naloziMeta <- function(spisak) {
  poltab <- matrix(nrow = length(spisak), ncol = 6)
  for (i in 1:length(spisak)) {
    a <- getUser(spisak[i])
    poltab[i, 1] <- a$screenName
    poltab[i, 2] <- a$followersCount
    poltab[i, 3] <- a$friendsCount
    poltab[i, 4] <- a$statusesCount
    poltab[i, 5] <- a$favoritesCount
    poltab[i, 6] <- as.character(a$created)
  }
  poltab <- as.data.frame(poltab)
  colnames(poltab) <-
    c("Nalog",
      "Pratioci",
      "Prijatelji",
      "Tvitovi",
      "Favorisao",
      "DatumOtvaranja")
  poltab$Pratioci <- as.numeric(as.character(poltab$Pratioci))
  poltab$Prijatelji <- as.numeric(as.character(poltab$Prijatelji))
  poltab$Tvitovi <- as.numeric(as.character(poltab$Tvitovi))
  poltab$Favorisao <- as.numeric(as.character(poltab$Favorisao))
  poltab$DatumOtvaranja <- as.Date(poltab$DatumOtvaranja)
  return(poltab)
}


#3 retfav()

#Funkcija retfav uzima klasicne Tviter metrike favorisan, retvitovan i tvitovi za
#tvitove tvitovane u toku posmatranog perioda za svaki nalog iz definisanog uzorka. (Galjak, 2017)
retfav <- function(tvitovi) {
  sp <- unique(tvitovi$screenName)
  retfav <- matrix(ncol = 4, nrow = length(sp))
  for (i in 1:length(sp)) {
    l <- tvitovi[tvitovi$screenName == sp[i], ]
    retfav[i, 1] <- l$screenName[1]
    retfav[i, 2] <- sum(l[l$isRetweet == F, 3])
    retfav[i, 3] <- sum(l[l$isRetweet == F, 12])
    retfav[i, 4] <- length(l[l$isRetweet == F, 12])
  }
  retfav <- as.data.frame(retfav)
  colnames(retfav) <- c("Nalog", "Favorisan", "Retvitovan", "Tvitovi")
  retfav$Favorisan <- as.numeric(as.character(retfav$Favorisan))
  retfav$Retvitovan <- as.numeric(as.character(retfav$Retvitovan))
  retfav$Tvitovi <- as.numeric(as.character(retfav$Tvitovi))
  retfav$Pros.Ret <- retfav$Retvitovan / retfav$Tvitovi
  return(retfav)
}


#4 matrica_povezanosti()

#Funkcija matrica_povezanosti od liste tvitova pravi matricu povezanosti (Galjak, 2017)
matrica_povezanosti <- function(x) {
  sp <- unique(x$screenName)
  lsp <- length(sp)
  mp <-
    matrix(nrow = lsp, ncol = lsp)
  colnames(mp) <- sp
  row.names(mp) <- sp
  for (i in 1:lsp) {
    l <- x[x$screenName == sp[i], ]
    for (j in 1:lsp) {
      if (j == i) {
        j <- j + 1
      }
      if (j > lsp) {
        break
      }
      k <- x[x$screenName == sp[j], ]
      if (any(grepl(k$screenName[1], l$text))) {
        mp[i, j] <- table(grepl(k$screenName[1], l$text))["TRUE"]
      }
    }
  }
  mp[is.na(mp)] <- 0
  return(mp)
}


#5 met_graf()

# Funkcija met_graf racuna globalne metrike izabrane mreze (Galjak, 2017)
met_graf <- function(graf, atribut=NULL) {
  met_graf <- list(
    "Broj cvorova" = vcount(graf),
    "Broj ivica" = ecount(graf),
    "Gustina" = graph.density(graf),
    "Gustina neusmereni" = graph.density(as.undirected(graf, mode =
                                                         "collapse")),
    "Povezanost" = vertex.connectivity(graf),
    "Povezanost neusmereni" = vertex.connectivity(as.undirected(graf, mode =
                                                                  "collapse")),
    "Dijametar" = diameter(graf, weights = NA),
    "Najudaljeniji" = paste(
      farthest_vertices(graf, weights = NA)$vertices$name[1],
      farthest_vertices(graf, weights = NA)$vertices$name[2],
      sep = " -- "),
    "Prosecna duzina putanje" = average.path.length(graf),
    "Tranzitivnost" = transitivity(graf),
    "Asortativnost stepena" = assortativity.degree(graf),
    "Centralizacija intermedijanosti" = centralization.betweenness(graf,
                                                                   directed = TRUE, normalized = TRUE)$centralization,
    "Centralizacija stepena" = centralization.degree(graf, normalized = TRUE,
                                                     mode = "all")$centralization,
    "Centralizacija bliskosti" = centralization.closeness(graf, mode = "all")$centralization,
    "Centralizacija svojstvenog vektora" = centralization.evcent(graf,
                                                                 directed = TRUE, normalized = TRUE)$centralization
  )
  return(met_graf)
}


#7 met_cent()

#Funkcija met_cent racuna metrike centralnosti za svaki pojedinacni cvor izabrane mreze
#i sortira ih po p2 odstojanju (Galjak, 2017)
require(p2distance)

met_cent <- function(graf) {
  cent <- as.matrix(
    cbind(
      betweenness(graf),
      degree(graf, mode = "in"),
      degree(graf, mode = "out"),
      closeness(graf, mode = "in"),
      closeness(graf, mode = "out"),
      eigen_centrality(graf)$vector
    )
  )
  colnames(cent) <-
    c(
      "intermedijarnosti",
      "dolaznog stepena",
      "odlaznog stepena",
      "dolazne bliskosti",
      "odlazne bliskosti",
      "svojstvenog vektora"
    )
  p2d <- p2distance::p2distance(cent, reference_vector_function = min)
  cent <- as.data.frame(cent)
  cent$P2odstojanje <- as.vector(p2d$p2distance)
  print(paste("Redosled pokazatalja: ",p2d$variables_sort))
  return(cent[order(-cent$P2odstojanje), ])
}


#8 klast_eval()

#Funkcija klast_eval za evaluaciju podobnosti razlicitih metoda klasterovanja (Galjak, 2017)

klast_eval <- function(graf, atribut, metod) {
  klaster <- eval(parse(text = metod))(graf)
  l <- unique(na.omit(get.vertex.attribute(graf, atribut)))
  y <- vector()
  x <- matrix(ncol = max(klaster$membership), nrow = length(l))
  for (i in 1:max(klaster$membership)) {
    for (j in 1:length(l)) {
      y[j] <-
        table(unlist(communities(klaster)[i]) %in% V(graf)$name[get.vertex.attribute(graf, atribut) ==
                                                                  l[j]])["TRUE"]
    }
    x[, i] <- y
  }
  x[is.na(x)] <- 0
  return(sum(apply(x, 2, sum) - apply(x, 2 , max)))
} #izlaz je razlika br. cvorova klastera i br. cvorova najbrojnije grupacije klastera


#9 layout.modular()

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