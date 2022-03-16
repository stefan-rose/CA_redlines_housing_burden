# script to vizualize correlations between CA cities housing burden and formerly redlined areas
install.packages("pacman")
pacman::p_load(tidyverse,tigris,spdep,sf,tmap,leaflet,GGally,ggmap,grid)

# CES data -------
# Load the datasets into R 
#CalEnvrioScreen 3.0 data c/o CA OEHHA, can be downloaded here: https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30
ces <- read_csv("calenviroscreen-3.0-results-june-2018-update.csv") #this will only work if the file is called this in your directory that you are working in 

# Examine newly loaded data and tidy or rename any vars if necessary 

View(ces) # CES data looks good overall

#now let's load some geographic data using tigris 
county_shp <- counties(state=06,cb=T) %>% st_as_sf()
state_shp <- states(cb=F) %>% filter_state("California") %>% st_as_sf()
tracts_shp <- tracts(state = 06, cb=F) %>% st_as_sf()
county_shp <- county_shp %>% mutate("GEOID" = as.numeric(GEOID)) # change variable format to fit join key 
tracts_shp <- tracts_shp %>% mutate("GEOID" = as.numeric(GEOID)) # change var format to fit join key 

tm_shape(county_shp) + tm_polygons(col="grey60",border.col = NULL)
tm_shape(tracts_shp) + tm_polygons(col="grey60",border.col = NULL)

#now let's join our datasets with the geo data 
ces_shp <- left_join(ces,tracts_shp, by=c("Census Tract"="GEOID"))
ces_shp <- st_as_sf(ces_shp)

# CA cities redlining data -----
#Redlining shapefiles c/o University of Richmond 
#can be downloaded here: https://dsl.richmond.edu/panorama/redlining/#loc=5/39.215/-98.372&text=downloads
#load in shapefiles 
SF_poly <- st_read("CASanFrancisco1937")
OAK_poly <- st_read("CAOakland1937")
LA_poly <- st_read("CALosAngeles1939")
SAC_poly <- st_read("CASacramento1937")
SJ_poly <- st_read("CASanJose1937")
SD_poly <- st_read("CASanDiego1938")
roads <- st_read("tl_2015_06_prisecroads")
roads <- roads %>% filter(RTTYP=="I") %>% st_as_sf()
tm_shape(LA_poly) + tm_polygons(col="holc_grade",border.col = NULL)
sf::st_is_valid(LA_poly)

LA_shp <- ces_shp %>% filter(`California County`=="Los Angeles")
Oak_shp <- ces_shp %>% filter(`California County`=="Alameda")
SD_shp <- ces_shp %>% filter(`California County`=="San Diego")
LA_shp <- LA_shp %>% filter(`Census Tract`!=6037599100) #remove unpopulated tract
SF_shp <- SF_shp %>% filter(`Census Tract`!=6075060100) #remove unpopulated tract

SF_CD <- SF_poly %>% filter(holc_grade=="C"|holc_grade=="D")
Oak_CD <- OAK_poly %>% filter(holc_grade=="C"|holc_grade=="D")
LA_CD <- LA_poly %>% filter(holc_grade=="C"|holc_grade=="D")
SJ_CD <- SJ_poly %>% filter(holc_grade=="C"|holc_grade=="D")
SD_CD <- SD_poly %>% filter(holc_grade=="C"|holc_grade=="D")
SAC_CD <- SAC_poly %>% filter(holc_grade=="D")

county_shp <- county_shp %>% filter(COUNTYFP!="073")
#create inset maps 
LA_region = st_bbox(c(xmin = -118.61036, xmax = -117.70285,
                      ymin = 33.70563, ymax = 34.30388),
                    crs = st_crs(LA_CD)) %>% 
  st_as_sfc()

Oak_region = st_bbox(c(xmin = -122.700, xmax = -122.000,
                       ymin = 37.000, ymax = 37.800),
                     crs = st_crs(Oak_CD)) %>% 
  st_as_sfc()

SD_region = st_bbox(c(xmin = -117.700, xmax = -117.000,
                      ymin = 32.500, ymax = 33.000),
                    crs = st_crs(SD_CD)) %>% 
  st_as_sfc()
tmap_mode("plot")

LA_redlines <- 
tm_shape(LA_shp) +
  tm_polygons("Housing Burden Pctl",border.col = NULL,style="quantile",
              n=4,palette="-Greys",alpha = 0.9,title="Housing Burden \n(Percentile)",
              colorNA="black") + 
  tm_layout(main.title="Los Angeles Housing Burden and Redlined Areas",main.title.size = 0.92,
            main.title.position = "center",
            frame = T,fontfamily = "Times",
            bg.color = "lightblue",inner.margins = 0.05,
            legend.position =c("left","bottom"),legend.bg.color = "lightblue",
            legend.text.size = 0.8) +
  tm_compass(type="rose",position = c("right","top"),color.light = "white",
             text.color = "white",size=4) + 
  tm_scale_bar(position = c("right","bottom"),color.dark = "black",text.color = "white",
               width = 0.15)+
  tm_legend(outside=F,scale=0.85)+
  #tm_shape(roads)+ #uncomment if you want freeways included
  #tm_lines(col="white")+
  tm_shape(LA_CD, is.master = TRUE) +
  tm_polygons(alpha = 0,border.col="red",lwd = 0.92,
              legend.show=T) +
  tm_layout(outer.bg.color = "white",
            space.color = "white") +
  tm_shape(county_shp) +
  tm_polygons(col="black",border.col = "white",
              alpha=0.9,border.alpha = 0.5)

#st_bbox(LA_CD)
#inset map var
CA_map = tm_shape(state_shp) + tm_polygons() +
  tm_shape(LA_region) + tm_borders(col="red",lwd = 1.3) 

#load these two together to get map with inset
LA_redlines
print(CA_map, vp = viewport(0.87, 0.175, width = 0.2, height = 0.2))


