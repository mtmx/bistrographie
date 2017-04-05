# comptage par grande ville

library(dplyr)
library(tidyr)
options(java.parameters = "-Xmx8g")
library(XLConnect)
library(XLConnectJars)
# import du fichier de population légale Insee
fichier_excel <- loadWorkbook("./data/Fichier_poplegale_6813.xls", create = TRUE)
RP_POP2013 <- readWorksheet (fichier_excel , "2013" ,header = TRUE, startRow = 7 )


# comptage du nombre de db par commune

geosirene.df.depcom.plm <- 
  geosirene.db %>%
  mutate(DEPCOM = paste0(DEPET,COMET)) %>%
  mutate(DEPCOM.PLM = ifelse(substr(DEPCOM,1,2) %in% '75' ,'75056', 
                             ifelse(substr(DEPCOM,1,3) %in% '132' ,'13055', 
                                    ifelse(substr(DEPCOM,1,4) %in% '6938' ,"69123", DEPCOM)))) %>%
  group_by(DEPCOM.PLM, APET700) %>%
  summarise(cpt=n()) %>%
  mutate(cpt = as.numeric(cpt)) %>%
  as.data.frame()

# comptage de la population par commune

POP.depcom.plm <- 
  RP_POP2013 %>%
  mutate(DEPCOM.PLM = ifelse(substr(COM,1,2) %in% '75' ,'75056', 
                             ifelse(COM %in% '132' ,'13055', 
                                    ifelse(COM %in% '6938' ,"69123", COM)))
  ) %>%
  group_by(DEPCOM.PLM) %>%
  summarise(PMUN13=sum(PMUN13))

# changement des libellés de communes

POP.depcom.plm <-
  POP.depcom.plm %>%
  left_join(RP_POP2013 %>%
              dplyr::select(COM,NCC),
            by = c("DEPCOM.PLM" = "COM")) %>%
  mutate(NCC = ifelse(substr(DEPCOM.PLM,1,2) %in% '75' ,'Paris', 
                      ifelse(DEPCOM.PLM %in% '132' ,'Marseille', 
                             ifelse(DEPCOM.PLM %in% '6938' ,"Lyon", NCC)))) %>%
  mutate(NCC = ifelse(DEPCOM.PLM %in% "93066" ,"Saint-Denis (93)", 
                      ifelse(DEPCOM.PLM %in% "97411" ,"Saint-Denis (974)", NCC))
  )

# table communale finale

COM_bars <-
  POP.depcom.plm %>%
  left_join(geosirene.df.depcom.plm, by = c("DEPCOM.PLM" = "DEPCOM.PLM")) %>%
  mutate_each(funs(replace(., is.na(.), 0)))


####################################
# cartographie de la densité de bars toute France

library(maptools)
library(rgdal)
library(rgeos)
# géographie des communes
GEOCOMM <- readShapeSpatial("./data/Geofla2015_communes_FRMET" ,proj4string=CRS("+init=epsg:2154"))
names(GEOCOMM)[names(GEOCOMM)=="INSEE_COM"] <- "CODGEO"
GEOCOMM <- gBuffer(GEOCOMM, width = 0,byid=T)
# centroides des communes
GEOCOMM_pts <- SpatialPointsDataFrame(gCentroid(GEOCOMM, byid=TRUE),  GEOCOMM@data, match.ID=FALSE)

# jointure pour shape points
COMM_geosirene <- GEOCOMM_pts
COMM_geosirene@data=data.frame(COMM_geosirene@data, COM_bars[match(COMM_geosirene@data$CODGEO, COM_bars$DEPCOM.PLM),])


#### génére les rasters et les stocke en csv (traitements longs...)
#library(devtools)
#install_github("Groupe-ElementR/SpatialPosition")
library(SpatialPosition)
FRAmet_ZT50km <- readShapeSpatial("./data/FRMET_ZT50km" ,proj4string=CRS("+init=epsg:2154"))

# création de la grid commune
com <- COMM_geosirene
biggrid <- CreateGrid(w = FRAmet_ZT50km, resolution = 3000)
row.names(com) <- as.character(com@data$CODGEO)

# sequence pour découper la grille
sequence <- c(seq(1,nrow(biggrid), 500),nrow(biggrid)) 

# repertoire de sortie
dir.create("./rasters")

# projection de sortie
prj <- CRS("+init=epsg:2154")

# fonction de création des rasters

rast_stew <- function(v,t)
{
  # le df dans lequel on rentrera les Données
  x <- biggrid@data
  x$OUTPUT <- NA
  
  lseq <- length(sequence)
  pb <- txtProgressBar(min = 1, max  = lseq, initial = 0, char = ".", width = 50, style = 3)
  for (i in 1:(lseq-1)){
    # "petite" matrice
    mat <- CreateDistMatrix(knownpts = com, unknownpts = biggrid[sequence[i]:sequence[i+1],])
    st <- stewart(knownpts = com, unknownpts = biggrid[sequence[i]:sequence[i+1],],
                  matdist = mat,typefct = "exponential", span = 10000, beta = 3, 
                  mask = FRAMET,
                  var = v)
    # alimentation du df
    x[sequence[i]:sequence[i+1],"OUTPUT"] <-  st@data$OUTPUT
    setTxtProgressBar(pb, i)
  }
  #stockage du raster en sortie
  y <- rasterFromXYZ(xyz = x[,2:4], res = c(3000,3000))
  names(y) <- paste(names(y),sep = "_",t,v)
  projection(y) <- prj
  z <- as(y, 'SpatialGridDataFrame')
  z_df <- as.data.frame(z)
  write.csv(z_df, file =  paste("./rasters/raster_", t,"_", v,".csv", sep="") )
}

ll <- c(
  
  "cpt",
  "PMUN13"
)
tt <- c("SIRENE_10km_beta3_") 

mapply(rast_stew, v=ll, t=tt) 


# récupération des sources

load_data <- function(path) { 
  files <- dir(path , pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  #do.call(rbind, tables)
}
rasters <- load_data( "./rasters")
rasters_df <- as.data.frame(rasters)

rasters_df$OUTPUT_SIRENE_10km_beta3_ratio <- (rasters_df$OUTPUT_SIRENE_10km_beta3__cpt /  rasters_df$OUTPUT_SIRENE_10km_beta3__PMUN13) * 10000

# enlever variables useless
rasters_df <- rasters_df[, -grep("^X.", colnames(rasters_df))]
rasters_df <- rasters_df[, -grep("^s1.", colnames(rasters_df))]
rasters_df <- rasters_df[, -grep("^s2.", colnames(rasters_df))]

# conversion en raster
coordinates(rasters_df)=~s1+s2
rasters_df <- as(rasters_df, "SpatialPixelsDataFrame")

proj4string(rasters_df) <- prj 

# extraire sur contours France
FRAMET <- subset(FRAmet_ZT50km, FID_FRMET_ == 1)
rasters_df_d <- rasters_df[FRAMET, ]

# vérification de l'allure de l'indicateur
plot(spplot(rasters_df_d[rasters_df_d,"OUTPUT_SIRENE_10km_beta3_ratio"],
            main="test",
            col.regions = colorRampPalette(c('white','dodgerblue','black')),
            par.settings =list(axis.line = list(col =  'transparent'))))

#############################################
##### carte finale  

rasters_df_c <- as.data.frame(rasters_df)
rasters_df.sp <- SpatialPointsDataFrame(coords = rasters_df_c[,5:6], data = rasters_df_c, proj4string = rasters_df@proj4string)

# Transformation en raster
rasters_df.r <- rasterFromXYZ(xyz = rasters_df.sp@data[,c("s1","s2","OUTPUT_SIRENE_10km_beta3_ratio")])
rasters_df.r@crs <- CRS("+init=epsg:2154")

# discrétisation
bks <- c(0,2,5,8,12,20,1000)

# conversion en spatial polygons
xx <- rasterToContourPoly(rasters_df.r, breaks = bks, mask = FRAMET)
contPolygons <- xx
proj4string(contPolygons) <- prj 

# carte avec ggplot
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
#conversion pour ggplot
contPolygons.f <- fortify(contPolygons, region = "id")
contPolygons.f <-
  contPolygons.f %>%
  mutate(id = as.numeric(id), 
         val = ifelse(id == 1 , "< 2", ifelse(id==2, "2-5", ifelse(id==3, "5-8", ifelse(id==4, "8-12", ifelse(id==5, "12-20", ">20"))))) )

# ordonner facteurs
contPolygons.f$val <- factor(contPolygons.f$val, levels = c("< 2", "2-5", "5-8", "8-12", "12-20", ">20"))

# contours départements
DEP <- gUnaryUnion(GEOCOMM, GEOCOMM$CODE_DEPT)
DEP$id <- row.names(DEP) 
#simplifier contours
library(rmapshaper)
DEP.s <- ms_simplify(DEP, keep = 0.02)
#conversion pour ggplot
DEP.f <- fortify(DEP.s, region = "id")

# carte statique de la densité
ggplot() +
geom_polygon(data = contPolygons.f, aes(x = long, y = lat, group = group, fill = val)) +
geom_path(data = DEP.f, aes(x = long, y = lat, group = group), color = "black", alpha=0.4, size=0.3) +
  coord_equal() +
  theme_tufte(ticks = FALSE) + 
  theme(
    axis.text = element_blank(), 
    axis.title = element_blank()
  ) + 
  scale_fill_brewer(palette = "Reds") +
  ggtitle("Densité de débits de boissons")


######################
# palmares des villes de plus de 50 000 habitants

palma_bars_ville <-
  geosirene.df.depcom.plm %>%
  left_join(POP.depcom.plm, by = c("DEPCOM.PLM" = "DEPCOM.PLM")) %>%
  filter(!is.na(PMUN13)) %>%
  filter(PMUN13 >50000) %>%
  mutate(ratio_APET_10khabs = (cpt/PMUN13) * 10000) 

# palmares villes en graphique en barres

p <- ggplot(subset(palma_bars_ville, APET700 %in% '5630Z'), aes(x=reorder(NCC, ratio_APET_10khabs),y=ratio_APET_10khabs)) +
  geom_bar(stat="identity", fill="grey")+  
  coord_flip() + 
  theme(axis.text.y = element_text(  size =8, face="bold")) +
  scale_fill_manual(name ="Catégories",values = c("sandybrown","steelblue", "darkseagreen", "powderblue", "lightgray")) +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 20))  +
  #theme_bw() + 
  theme(
    #axis.text = element_blank(), 
    axis.title = element_blank(),
    panel.grid.major = element_line(linetype = "dashed",colour = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = c(0.7, 0.4)
  )  +
  labs(
    title = "Palmarès des villes les plus équipées en débits de boissons",
    subtitle = "Nombre de débits de boissons pour 10 000 habitants",
    caption = "Source : base Sirene Insee Décembre 2016 - communes de plus de 50 000 habitants"
  )
p

##########################
library(ggiraph)
library(spdplyr)

GEOCOMM_pts.villes <- GEOCOMM_pts
GEOCOMM_pts.villes@data <-
  GEOCOMM_pts.villes@data %>%
  left_join(COM_bars,
            by = c("CODGEO" = "DEPCOM.PLM"))
 
GEOCOMM_pts.villes <- GEOCOMM_pts.villes %>% 
  filter(PMUN13 > 50000) %>%
  mutate(ratio_10k = (cpt/PMUN13) * 10000) %>%
  arrange(desc(ratio_10k)) %>%
  slice(1:10)

# tooltip à afficher pour les villes
GEOCOMM_pts.villes$tip <- paste0(
  "<b>", GEOCOMM_pts.villes$NCC, "</b> ", "<br>",
  "<i>" , GEOCOMM_pts.villes$cpt," débits de boissons", "</i> ","<br>",
  "<i>" , round(GEOCOMM_pts.villes$ratio_10k,0)," pour 10 000 habitants", "</i> "
)
GEOCOMM_pts.villes.df <- as.data.frame(GEOCOMM_pts.villes) %>%   mutate(rang = as.numeric(dense_rank(desc(ratio_10k)))) 

#css
tooltip_css <- "background-color:black;padding:2px;font-size: 80%;color: white"

# carto finale interactive
gg <- ggplot() +
  geom_polygon(data = contPolygons.f, aes(x = long, y = lat, group = group, fill = val)) +
  geom_path(data = DEP.f, aes(x = long, y = lat, group = group), color = "black", alpha=0.4, size=0.3) +
  geom_text(data = GEOCOMM_pts.villes.df, aes(x = X_CHF_LIEU, y = Y_CHF_LIEU, size = ratio_10k, label = rang) , color="black") + 
  geom_point_interactive(data = GEOCOMM_pts.villes.df, aes(x = X_CHF_LIEU, y = Y_CHF_LIEU, size = ratio_10k, tooltip = tip, data_id = CODGEO), shape =21, fill = "grey", colour="black",alpha =0.5 ) + 
  coord_equal() +
  theme_tufte(ticks = FALSE) + 
  theme(
    axis.text = element_blank(), 
    axis.title = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, face = "bold"),
    plot.caption = element_text(size = 8, face = "italic"),
    legend.position = c(0.9,0.5),
    legend.key.size = unit(0.3, "cm")
  ) + 
  scale_fill_brewer(palette = "Reds",name="") +
  scale_size(range = c(3, 8),name="5 villes les\nplus équipées",guide=FALSE) +
labs(
  title = "Densité de débits de boissons pour 10 000 habitants",
  subtitle = "et palmarès des 10 villes les plus équipées en débits de boissons",
  caption = "Source : base Sirene Insee Décembre 2016 (villes : communes > 50 000 habs)"
)

ggi <- ggiraph(code = {print(gg)}, hover_css = "stroke:red;",zoom_max = 1,tooltip_opacity = .9,
               tooltip_offx = 0,
               tooltip_offy = 20,
               width=1,
               height=4,
               tooltip_extra_css = tooltip_css
)
# sortie

# repertoire de sortie
dir.create("./sorties")

library(htmlwidgets)
saveWidget(ggi, file = "./carto_db_fr.html", selfcontained = F, libdir = "./deps_map",
           background = "white", knitrOptions = list())

# sortie directe vers sous dossier /sorties ne fonctionne pas...
#Error in normalizePath(basepath, "/", TRUE) : 
#  path[1]="./sorties": No such file or directory