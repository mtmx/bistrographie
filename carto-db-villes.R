# sélection des débits de boissons géocodés
geosirene.db.geo <-
  geosirene.db %>%
  mutate(n = 1) %>%
  filter(!is.na(longitude)) %>%
  as.data.frame()

### création d'un spatial points dataframe à partir des coordonnées en WGS84
  library(sp)
geosirene.geo <- SpatialPointsDataFrame(coords = subset(geosirene.db.geo, select = c(longitude, latitude)),
                                        data = geosirene.db.geo,
                                        proj4string = CRS("+init=epsg:4326"))    
# pràjection en Lambert carto
geosirene.geoL <- spTransform(geosirene.geo, CRS("+init=epsg:2154"))  

# dataframe 
geosirene.db.geo <-
  geosirene.geoL %>%
  as.data.frame() %>%
  rename(x = longitude.1,
         y = latitude.1)

# spatial polygons data frame des communes françaises
library(maptools)
GEOCOMM <- readShapeSpatial("./data/Geofla2015_communes_FRMET" ,proj4string=CRS("+init=epsg:2154"))
names(GEOCOMM)[names(GEOCOMM)=="INSEE_COM"] <- "CODGEO"

# sélection par ville, exemple sur marseille
# uniquement les db inclus dans les communes situées à moins de 20 km de la commune centre
library(rgeos)
geosirene.db.ville <-
  geosirene.db.geo %>%
  filter( DEPCOM %in% as.vector(as.data.frame(unique(as.data.frame(subset(raster::intersect(GEOCOMM, gBuffer(subset(GEOCOMM, CODGEO %in% '13201'), width = 20000)), selec=c(CODGEO)))))[,1])) 

geosirene.geo.ville <- SpatialPointsDataFrame(coords = subset(geosirene.db.ville, select = c(x, y)),
                                              data = geosirene.db.ville,
                                              proj4string = CRS("+init=epsg:2154"))   

# calcul de la densité de débits de boissons
library(SpatialPosition)
globalAccessibility <- stewart(knownpts = geosirene.geo.ville, varname = "n",
                               typefct = "exponential", span = 200, beta = 4,
                               resolution = 80 )

rasterAccessibility <- rasterStewart(x = globalAccessibility, mask = gBuffer(subset(GEOCOMM, CODGEO %in% '13201'), width = 20000))

pot.spdf <- rasterToContourPoly(r = rasterAccessibility, 
                                #breaks = plotStewart(x = rasterAccessibility, add = FALSE, nclass = 10), 
                                breaks = seq(from = 0, to = 45, by =3),
                                mask = gBuffer(subset(GEOCOMM, CODGEO %in% '13201'), width = 20000))
pot.spdf <- gBuffer(pot.spdf, width = 0, byid = T)
pot.spdf <- spTransform(pot.spdf, CRS("+init=epsg:4326")) 

#######################################

# marseille 13201
#5.380774, 43.302066

# rennes 35238
#-1.678610, 48.110523

# lyon 69381
#4.838119, 45.761901

# lille 59350
#3.059704, 50.631354

#nice 06088
#7.271552,43.700755

#bordeaux 33063
#-0.571328,44.841355, 

#nantes 44109
#-1.552433,47.214374, 

#strasbourg 67482
#7.753051, 48.579961, 

# paris 75101
#2.345899, 48.859467


# Construction de la carte

library(leaflet)
## Initialisation 
m <- leaflet(padding = 0)
# tuiles et crédits
m <- addWMSTiles(map = m, "Stamen.Toner", options=tileOptions(minZoom=12,maxZoom=15),attribution = "Base Sirene Insee Avril 2017 / Géocodage BAN+BANO par Etalab")
m <- addProviderTiles(map = m, "CartoDB.Positron", options=providerTileOptions(minZoom=12,maxZoom=15))
# coordonnées du centre de l'affichage
m <- setView(map = m,5.380774, 43.302066, zoom = 13)
# affichage des points et définition du popup
m <- addCircleMarkers(map = m, 
                      data = geosirene.db.ville,
                      lng = ~longitude, 
                      lat = ~latitude, 
                      radius = 2.5, weight = 0.5, 
                      stroke = F, opacity = 50,
                      fill = T, fillColor = "	red", 
                      fillOpacity = 0.5,
                      group = "Bars-Cafés",
                      popup = paste0( "<b>", geosirene.db.ville$ENSEIGNE, "</b>" ,"<br>", geosirene.db.ville$L4_NORMALISEE,"<br>",geosirene.db.ville$L6_NORMALISEE ,"<br>"),color = "black")
# affichage de la densité et définition de sa palette de couleur
pal <- colorNumeric(palette = "OrRd",domain = pot.spdf$max)
m <- addPolygons( map = m ,data = pot.spdf, stroke = F,
                  group = "Densité",
                  fillOpacity = 0.5 , smoothFactor = 0, color= ~pal(max))
# affichage de la légende
m <- addLegend(map = m ,"topright", pal = pal, values = pot.spdf$max,
               title = "MARSEILLE<br>Nb de bars-cafés<br>à moins de 200m",
               #labFormat = labelFormat(prefix = "$"),
               opacity = 1)
# bornes maximales de l'affichage correspondant aux limites du fichier affiché
m <- setMaxBounds(map = m, 
                  lng1 = min(geosirene.db.ville$longitude),
                  lat1 = min(geosirene.db.ville$latitude),
                  lng2 = max(geosirene.db.ville$longitude), 
                  lat2 = max(geosirene.db.ville$latitude))
# contrôle des couches
m <- addLayersControl(map = m,  baseGroups = c("Bars-Cafés","Densité"), options = layersControlOptions(collapsed = F, autoZIndex = TRUE), position =  "topright")

# dimensions de la carte
m$width <- 640
m$height <- 600

# version xxl
#m$width <- 1090
#m$height <- 700

m

# Export de la carte en html
library(htmlwidgets)
#saveWidget(m, './sorties/cartomicro_bars_marseille_1.html', selfcontained = F,libdir = "./sorties/deps")
saveWidget(m, './cartomicro_bars_marseille_1.html', selfcontained = F,libdir = "./sorties/deps")

# sortie directe vers sous dossier /sorties ne fonctionne pas...
#Error in normalizePath(basepath, "/", TRUE) : 
#  path[1]="./sorties": No such file or directory
