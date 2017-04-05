
rm(list = ls(all.names = TRUE))

#packages
library(xml2)
library(rvest)
library(magrittr)
library(stringr)
library(dplyr)
library(data.table)

## dossier de stockage des fichiers par département
dir.create("./geo-sirene")
serveur <- "./geo-sirene"

#lien url où sont stockés les fichiers sirene géocodés  via BAN et BANO par Etalab
url_source <- "http://212.47.238.202/geo_sirene/last/"
home_page <- read_html(url_source)
# liens url des fichiers
home_links <- home_page %>% html_nodes("a") %>% html_attr("href")

#téléchargement des fichiers sources
for(thisLink in home_links){
  
  nom_fichier <- thisLink 
  url<-paste0(url_source,thisLink)
  
  if( grepl("geo-sirene", thisLink)){
    if(!file.exists(paste0(serveur,nom_fichier))){
      download.file(url,destfile=paste0(serveur,'/',thisLink),mode="wb",quiet=F) 
    }
  }
}


#extraction des csv depuis les fichiers zippés 7z
# doit être faisable depuis R
# pour l'instant, executé directement depuis le dossier

# import des csv
# construction du fichier geosirene unique
files = list.files(path = serveur, pattern="*.csv$")
geosirene = do.call(rbind, lapply(files, function(x) fread(paste0(serveur,"/",x), stringsAsFactors = FALSE,sep=",", dec=".",colClasses="character")))

# quelques variables à convertir en numerique
# le siret est l'identifiant unique d'un établissement, concaténation du siren et du nic
# le code commune est la concaténation du code departement et du code commune
geosirene <-
  geosirene %>%
  distinct(SIREN,NIC,.keep_all = TRUE) %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         geo_score = as.numeric(geo_score),
         SIRET = paste0(SIREN,NIC),
         DEPCOM = paste0(DEPET,COMET)) %>%
  as.data.frame()

# sélection des debits de boisson
geosirene.db <-
  geosirene %>%
  filter(APET700 %in% '5630Z') %>%
  mutate(DEPCOM = paste0(DEPET,COMET),
        TYPVOIE = ifelse( TYPVOIE %in% 'CRS' ,"COURS",
                          ifelse( TYPVOIE %in% 'BD' ,"BOULEVARD",
                                  ifelse( TYPVOIE %in% 'RTE' ,"ROUTE",
                                          ifelse( TYPVOIE %in% c('AVE','AV') ,"AVENUE",
                                                  ifelse( TYPVOIE %in% 'PL' ,"PLACE",TYPVOIE)))))) %>%
  mutate(RUE = paste0(TYPVOIE, " ",LIBVOIE)) %>%
  mutate(id_rue = paste0(DEPCOM,"_",RUE)) %>%
  as.data.frame()

# suppression des fichiers temporaires
rm(list = c("files", "home_links","home_page", "nom_fichier","url","url_source","geosirene"))

