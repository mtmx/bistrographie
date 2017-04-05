
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# noms les plus courants
geosirene.db.cpt <-
  geosirene.db %>%
  mutate(ENSEIGNE = trim(ENSEIGNE)) %>%
  filter(!ENSEIGNE %in% '') %>%
  group_by(ENSEIGNE) %>%
  summarise(cpt=n()) %>%
  filter(cpt>10) %>%
  mutate(cpt= as.numeric(cpt)) %>%
  arrange( desc(cpt))

#wordlcoud

library(slam)
library(RColorBrewer)
library(wordcloud2)

colorVec = rep(c('#000000','#323232','#4c4c4c','#666666','#7f7f7f'), length.out=nrow(geosirene.db.cpt))
wc1 <-
  wordcloud2(geosirene.db.cpt,
             color = colorVec,
             backgroundColor = "white",
             size = 0.36,
             #fontWeight = "bold",
             fontFamily = "Helvetica",
             #size = 6,
             shape ='pentagon',
             ellipticity =0.8,
             widgetsize = c(640,300),
             rotateRatio = 0)
wc1

# Export de la carte en html
library(htmltools)
save_html(wc1, './wc_noms_bars.html',libdir = "./deps_noms")

# sortie directe vers sous dossier /sorties ne fonctionne pas...
#Error in normalizePath(basepath, "/", TRUE) : 
#  path[1]="./sorties": No such file or directory
