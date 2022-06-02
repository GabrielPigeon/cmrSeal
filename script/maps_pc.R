library(sf) # GIS package
library(tidyverse) # tidyverse packages, dplyr and ggplot2 among others
theme_set(theme_minimal(base_size = 14)) # set ggplot theme 

# setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal")
# install.packages("rcartocolor")
# install.packages("remotes")
# remotes::install_github("Nowosad/rcartocolor")
# devtools::install_github("yutannihilation/ggsflabel")

library(ggsflabel)# for arrows 
library(rcartocolor)

#  studysites_raw <- st_read("data/raw/shp/quebec_coastline.shp") # misses NF-labrador

sf::st_crs(4326) # sf way to get the same

# get province
quebec <- st_read("shp/lpr_000b16a_e.shp") # not so nice
# glimpse(quebec) # project is lambert conical

studysites <- quebec %>%
    filter(PRUID == '10'|PRUID =='13'|PRUID =='24')
studysites
glimpse(studysites)

#tidy colnames 
colnames(studysites) <- colnames(studysites) %>% 
    str_to_lower() #<<
colnames(studysites)

# put in same coordonates
studysites <- studysites %>% 
    st_transform(crs = st_crs(4326)) 
studysites # GCS_Assumed_Geographic_1

# quick graph
studysites %>%
    ggplot() + 
    geom_sf() +
    labs(title = 'Quebec province',
         subtitle = 'Source: Statistics Canada')


# get bathymetry ----------------------------------------------------------
bath <- st_read("shp/bathylines_E&GSL.shp")
glimpse(bath)

# put in same coordonates

bath <- bath %>% 
    st_transform(crs = st_crs(4326)) 
bath # GCS_Assumed_Geographic_1

#tidy colnames 
colnames(bath) <- colnames(bath) %>% 
    str_to_lower() #<<
colnames(bath)

# quick bath
bath %>%
    ggplot() + 
    geom_sf()

# crop the gulf - too bic 
gulf_cropped <- st_crop(bath, xmin = -70, xmax =-62,
                          ymin = 47, ymax = 50.5)
#coord_sf(xlim = c(-70, -62), ylim = c(47, 50.5), expand = FALSE) +
  
ggplot() + geom_sf(data = gulf_cropped) + theme_bw() # + coord_sf(expand = FALSE)


# crop qc to match gulf boundaries 
cropped_qc <- studysites %>% 
    st_crop(st_bbox(gulf_cropped)) 

ggplot() + geom_sf(data = cropped_qc) + theme_void()


# create mini map ---------------------------------------------------------

# draw borders of cropped gulf for inset map
gsl = st_as_sfc(st_bbox(gulf_cropped))

# map the first qc 
map1 <-  ggplot() + 
    geom_sf(data = studysites, fill = "grey") + 
    geom_sf(data = gsl, fill = NA, color = "red", size = 1.2) +
    theme_void()
map1


# points and study area  ------------------------------------------------------------------
pts <- read.csv('df_site.csv')

pts <- na.omit(pts) %>% mutate(newSite=case_when(
  site=='bic'~'Bic Island',
  site=='metis'~'Métis'
))

random_pts <- tibble(
    id_point = seq_len(10),
    lat = c(42.96, 42.71, 42.72, 42.95, 42.96, 42.72, 42.68, 42.82, 42.79, 42.85),
    lon = c(0.31, 0.66, 0.75, 1.60, 0.58, 1.87, 1.07, 1.05, 0.36, 0.06)
)

sf_pts <- st_as_sf( #<<
    pts, coords = c("long", "lat"), crs=4326, remove=FALSE)  #<<
sf_pts

sf_pts <- sf_pts %>% 
    st_transform(st_crs(bath))

centroids_sf <- sf_pts %>%
    group_by(newSite) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_centroid 
  
as_tibble(centroids_sf)

# centroids <- sf_pts %>% 
#     st_centroid() #<<
# as_tibble(centroids)
centroids_sf %>%
    ggplot() + 
    geom_sf() +
    geom_sf_label(aes(label = newSite), colour = 'grey') 
# plot them with bathy 
ggplot() +
    geom_sf(data = bath) + 
    geom_sf(data = centroids_sf, colour='blue') +
    labs(title = 'Study area and some random points')

# which point are in qc - should be all 
# result <- st_intersects(centroids_sf, studysites) #<<
# result

# cities ------------------------------------------------------------------

cities <- st_read("shp/lieuhabite.shp")
glimpse(cities)

# project
cities <- cities %>% 
    st_transform(crs = st_crs(bath))  %>% 
    st_crop(st_bbox(bath)) 


bigcities <- cities %>%
    select('city' = nomcartrou,  
           'name' = mus_nm_mun, 
           'type'=mus_co_des) %>% 
filter(type =='V') %>% filter(city=='Sept-Îles'|city=='Le Bic'|city=='Rimouski'|city=='Rivière-du-Loup'|city=='Gaspé'|city=='Matane')# %>% as_tibble()


ggplot() +
    geom_sf(data = bigcities, colour = 'grey') +
    geom_sf_text(data = bigcities,aes(label = city), colour = 'grey') 



# start mapping -----------------------------------------------------------
ggplot() +
    geom_sf(data = cropped_qc) + 
    geom_sf(data = gulf_cropped, aes(colour = prof)) + 
    geom_sf(data = bigcities, colour = 'black') +
    geom_sf_text(data = bigcities,aes(label = city), colour = 'black') +
    geom_sf_label(data = centroids_sf, aes(label = newSite)) +
    labs(title = 'Study area and some random points')+
  #  scale_colour_continuous(name = "Depth (m)") + 
    scale_colour_carto_c(palette = "Teal", 
                         name='Depth (m)') 
    
ggplot() +
  geom_sf(data = gulf_cropped, aes(colour = prof)) + 
  geom_sf(data = cropped_qc) + 
  geom_sf(data = bigcities, colour = 'grey') +
  geom_sf_text(data = bigcities,aes(label = city), colour = 'black') +
  geom_sf(data = centroids_sf, colour='black', size=2) +
    geom_sf_label_repel(data=centroids_sf, 
                        aes(label = newSite),
                        force = 150, nudge_x = 0.2, nudge_y = -0.3,seed = 10) + 
    labs(title = 'Study area and some random points')+
  #  scale_colour_continuous(name = "Depth (m)") 
    scale_colour_carto_c(palette = "Teal", 
                     name='Depth (m)') 


# join the 2 maps ---------------------------------------------------------
library(ggspatial)


load("01_gg_inset_map.RData")


map2 <- ggplot() +
  geom_sf(data = gulf_cropped, aes(colour = prof)) + 
  geom_sf(data = cropped_qc) + 
 # geom_sf(data = bigcities, colour = 'grey') +
 # geom_sf_text(data = bigcities,aes(label = city), colour = 'black', size = 3) +
 # geom_sf_text_repel(data=bigcities,
  #                  aes(label = city)) +
  geom_sf(data = centroids_sf, colour='black', size=2) +
  geom_sf_label_repel(data=centroids_sf, 
                      aes(label = newSite),size=3,
                      force = 150, nudge_x = 0.2, nudge_y = -0.3,seed = 10) + 
  labs(y = 'Latitude', x='Longitude') +
  coord_sf(xlim = c(-70, -62), ylim = c(47.1, 50.2), expand = FALSE) +
  scale_colour_carto_c(palette = "Teal", 
                       name='Depth (m)', 
                       direction = -1) +theme_bw(base_family = "ArcherPro Book")+ 
  annotate(geom="label", y=49.5, x=-64.5, label="Gulf of St. Lawrence",
           color="black", fontface='italic') + 
  annotate(geom="label", y=49.1, x=-67.5, label="St. Lawrence Estuary",
           color="black", fontface='italic')

map2
   
# theme(#legend.position = c(0.4, 0.05),
    #      legend.direction = "horizontal",
     #     legend.key.width = unit(10, "mm")) 

map2 <- map2 + ggspatial::annotation_scale(
  location = "br",
  bar_cols = c("grey60", "white"),
  text_family = "ArcherPro Book"
) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  ) 


# create inset map --------------------------------------------------------

library(cowplot)


gg_inset_map1 = ggdraw() +
    draw_plot(map2) +
    draw_plot(map1, x = 0.05, y = 0.7, width = 0.3, height = 0.3)

gg_inset_map1


ggsave(filename = "01_gg_inset_map.png", 
       plot = gg_inset_map1,
       width = 9, 
       height = 5,
       dpi = 300)

ggsave(filename = "01_gg_inset_map.tiff", 
       plot = gg_inset_map1,
       width = 9, 
       height = 5,
       dpi = 300)

# save objects 
save(list = ls(), file='01_gg_inset_map.RData')


# east coast too heavy --------------------------------------------------------------
# coast <- st_read("data/raw/shp/coast_eastcanada.shp")
# glimpse(coast)
# coast %>%
#   ggplot() +
#   geom_sf() +
#   #geom_sf(aes(fill = bearpresence)) +
#   labs(title = 'Brown bear presence in the French Pyrenees mountains',
#        subtitle = 'Source: French Biodiversity Agency') +
#   scale_fill_manual(values = c('gray90','steelblue1','steelblue4'), #<<
#                     name = "Bear presence", #<<
#                     labels = c("Absent", "Occasional", "Regular")) #<<
# 






