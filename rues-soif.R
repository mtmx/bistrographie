
#######
# nombre de db par rue

cpt_bars_rue <-
  geosirene.db %>%
  group_by(DEPCOM,RUE, id_rue) %>%
  summarise(cpt=n()) %>%
  filter(cpt >= 10) %>%
  mutate(cpt = as.numeric(cpt)) %>%
  as.data.frame()


# récupération des points db sur ces rues
# au préalable : script 'carto-db-villes' 

library(spdplyr)
DB_soif <-
  geosirene.geoL %>%
  left_join(cpt_bars_rue,
             by = c("id_rue" ="id_rue" )) %>%
  filter(!is.na(DEPCOM.y))

####
# Load packages
library('sp')
library('TSP')

##### fonctions pour itinéraire optimal entre dbs d'une rue

TSP_depuis_points = function(coords) {
  
  tsp <- TSP(dist(coords))
  tsp <- insert_dummy(tsp, label = "cut")
  tour <- solve_TSP(tsp)
  path.tsp <- unname(cut_tour(tour, "cut"))  
  return(path.tsp)

}

longueur_rues_soif <- function(id)
{
  DB_soif.rue <-
    DB_soif %>%
    filter(id_rue %in% id)
  
  # TSP
  path.tsp = TSP_depuis_points(DB_soif.rue@coords)
  
  # CONSTRUCTION DE l'OBJET
  spatialLinesObject <- SpatialLines(LinesList = list(Lines(slinelist = Line(DB_soif.rue[path.tsp,]@coords), ID = "1")), proj4string = CRS("+init=epsg:2154"))
  spatialLinesObject$id <- id
  spatialLinesObject$longueur = SpatialLinesLengths(spatialLinesObject)
  spatialLinesObject.df <- as.data.frame(spatialLinesObject)
  plot(spatialLinesObject)
  
  return(spatialLinesObject.df)
  
  }

# calcul pour toutes les rues
out = lapply(unique(DB_soif$id_rue), longueur_rues_soif) 

# aggrégation des éléments dans la list
iti.rues.soif = do.call(rbind, out) 

# noms de communes
options(java.parameters = "-Xmx8g")
library(XLConnect)
library(XLConnectJars)
fichier_excel <- loadWorkbook("./data/Fichier_poplegale_6813.xls", create = TRUE)
RP_POP2013 <- readWorksheet (fichier_excel , "2013" ,header = TRUE, startRow = 7 )



#noms communes
# stats rues de la soif
cpt_bars_rue.long <- 
  cpt_bars_rue %>%
  left_join(
    iti.rues.soif %>%
      mutate(longueur = as.numeric(longueur)),
    by = c("id_rue" = "id")
  ) %>%
  left_join(RP_POP2013 %>%
              dplyr::select(COM, NCC) %>%
              mutate(NCC = gsub(" arrondissement","",NCC)),
            by = c("DEPCOM" = "COM")) %>%
  mutate(NCC = ifelse(DEPCOM %in% '13201', "Marseille 1er",
                      ifelse(DEPCOM %in% '13202', "Marseille 2ème",
                             ifelse(DEPCOM %in% '13203', "Marseille 3ème",
                                    ifelse(DEPCOM %in% '13204', "Marseille 4ème",
                                           ifelse(DEPCOM %in% '13205', "Marseille 5ème",
                                                  ifelse(DEPCOM %in% '13215', "Marseille 15ème",
                                                         ifelse(DEPCOM %in% '69385', "Lyon 5ème",NCC))))))))

# deux rues mal géocodées dont il faut récupérer la longueur manuellement
cpt_bars_rue.long <- 
  cpt_bars_rue.long %>%
  mutate(longueur = ifelse(id_rue %in% ("2B050_QUAI LANDRY"), 480,
                               ifelse(id_rue %in% ("34129_ROUTE DE PALAVAS"), 5400,longueur))) %>%
  mutate(dens_db = longueur / cpt) %>%
  mutate(NOM_RUE = paste0(RUE, " (",NCC,")")) %>%
  arrange(desc(dens_db)) %>%
  as.data.frame() %>%
  mutate(n_id =  row_number())
 
 
# graphique de contrôle
library(ggplot2)

p <- ggplot(cpt_bars_rue.long, aes(x=reorder(NOM_RUE, n_id),y=n_id, fill=cpt)) +
  geom_bar(stat="identity")+  
  coord_flip() + 
  #theme(axis.text.y = element_text(  size =6.5, colour="darkgrey", face="bold")) +
  scale_fill_gradientn(colours = c("#f6f6f3","red","black"),space = "Lab",breaks=quantile(cpt_bars_rue.long$cpt,seq(0, 1, 0.1) )) +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 52))
p

# graphique final interactif
library(ggiraph)

cpt_bars_rue.long$tip <- paste0(
  "<b>", cpt_bars_rue.long$NOM_RUE, "</b> ", "<br>",
  "1 débit de boisson tous les ",round(cpt_bars_rue.long$dens_db,0) , " mètres", "<br>",
  "<i>", cpt_bars_rue.long$cpt, " débits de boissons </i> "
  
)
# css
tooltip_css <- "background-color:black;padding:2px;font-size: 80%;color: white"

p <- ggplot(cpt_bars_rue.long, aes(x=reorder(NOM_RUE, desc(dens_db)),y=n_id, fill=round(cpt, digits = 0), tooltip = tip,data_id = NOM_RUE)) +
  geom_bar_interactive(stat="identity")+  
  coord_flip() + 
  theme(axis.text.y = element_text(  size =6.5, colour="darkgrey", face="bold")) +
 # scale_fill_gradientn(colours = c("#f6f6f3","red","black"),space = "Lab",breaks=quantile(cpt_bars_rue.long$cpt,seq(0, 1, 0.1) )) +
  scale_fill_gradientn(limits = c(10, 24),colours = c("#f6f6f3","red","black"), name="Nombre de\ndébits de boissons", breaks = c(10,15,20,23)) +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 52)) +
  theme(
    axis.title = element_blank(),
    legend.title = element_text( size=7, face="bold"),
    legend.text = element_text(size=7),
    legend.background = element_rect(colour = "grey"),
    legend.key.size = unit(0.3, "cm"),
   # panel.grid.major = element_line(linetype = "dashed",colour = "grey80"),
   panel.grid.major = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = c(0.8, 0.4),
   plot.title = element_text(size = 12, face = "bold"),
   plot.subtitle = element_text(size = 9, face = "italic"),
   plot.caption = element_text(size = 8, face = "italic")
  )  +
  labs(
    title = "Palmarès des rues de la soif les plus denses",
    subtitle = "(rues comptant au moins 10 débits de boissons)",
    caption = "Source : base Sirene Insee Avril 2017"
  )

ggid <- ggiraph(code = {print(p)}, hover_css = "stroke:red;",tooltip_opacity = .9,
                tooltip_offx = 5,
                tooltip_offy = -40,
                #width=0.2,
                #height=0.2
                tooltip_extra_css = tooltip_css,
                width = 1, height = 6
)
ggid

library(htmlwidgets)
saveWidget(ggid, file = "./palmares_rues_soif.html", selfcontained = F, libdir = "./deps_rues",
           background = "white", knitrOptions = list())

# sortie directe vers sous dossier /sorties ne fonctionne pas...
#Error in normalizePath(basepath, "/", TRUE) : 
#  path[1]="./sorties": No such file or directory

