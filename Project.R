# load packages
packages=c('corrplot',
           'ggpubr',
           'plotly',
           'tidyverse',
           'readxl',
           'Hmisc', 
           'geojsonio',
           'sf', 
           'tmap',
           'spData',
           'maptools',
           'shiny',
           'shinythemes',
           'leaflet',
           'RColorBrewer',
           'rnaturalearth',
           'rnaturalearthdata')

for(p in packages){library
  if (!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}  

# and next
# try to draw a map
# here, argument country can change the country 
# also we can choose the continent to see.
#world <- ne_countries(scale = 'medium', returnclass = "sf")
worldcountry = geojson_read("data/50m.geojson", what = "sp")
#worldcountry<-subset(worldcountry, NAME_LONG!="Antarctica")
#country_geoms = read_csv("data/country_geoms.csv")

HDI = read_csv('data/data_cleaned/HDI/0_HDI.csv') 
HDI_selected = subset(HDI,Year==2018)
worldCountry_HDI <- merge(worldcountry, HDI_selected, by.x = "NAME_LONG", by.y = "Country")

##### HDI Distribution   #####
distribution_HDI = worldcountry %>%
    merge(filter(HDI,Year==2018),by.x = "NAME_LONG", by.y = "Country")

distribution_HDI$HDI

ggplot(filter(HDI,Year==2018),aes(x = reorder(Country,-HDI), 
                                  y = HDI,#color = Country,#fill = region
                                  ))+
  geom_bar(position="stack", stat="identity",fill = "#cc4c02")+
  ylab("HDI INDEX") + 
  #scale_x_categorical(breaks=seq(0, 10, 1))
  theme_bw() + 
  scale_fill_manual(values=c("#cc4c02")) +
  #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
  scale_y_continuous(expand = c(0, 0))+
  theme(legend.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "", 
        plot.title = element_text(size=10), 
        plot.margin = margin(5, 12, 5, 5))
        
  
  




#plot_map <- worldcountry
# create plotting parameters for map
#bins = c(0,1,10,50,100,500,1000,Inf)
#HDI_pal <- colorBin("Blues", domain = worldCountry_HDI$"2018")#, bins = bins)
HDI_pal <- colorQuantile("Blues", domain =  worldCountry_HDI$HDI)
#plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

labels <- sprintf(
  "<strong>%s</strong><br/> 
   Index: %g",
  worldCountry_HDI$NAME_LONG, worldCountry_HDI$HDI
) %>% lapply(htmltools::HTML)

popup = sprintf(
  "<strong>%g</strong><br/>",worldCountry_HDI$HDI
) %>% lapply(htmltools::HTML)



# weight: the thickness of the boundary lines in pixels
# color: the color of the polygons
# label: the information to appear on hover
# highlightOptions: options to highlight a polygon on hover

#++++++++++++++++++  WORLD MAP AREA  ++++++++++++++++++++++++++
basemap = leaflet(worldCountry_HDI) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(#stroke = FALSE, # use to turn off the broad 
              smoothFactor = 0.2, 
              fillColor = ~HDI_pal(worldCountry_HDI$HDI), # 是轮廓内的颜色
              fillOpacity = 0.7,
              color="white", #stroke color
              weight = 1, # stroke width in pixels
              highlight = highlightOptions(
                #weight = 5,
                color = "#666",
                #dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE #Whether the shape should be brought to front on hover
                ),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              popup = popup,
              popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
              group = "Human Development Index"
              ) %>%
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Human Development Index","Gender Development Index"),
    #overlayGroups = c("Human Development Index","Gender Development Index"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Human Development Index","Gender Development Index"))  %>%
  addLegend("bottomright", pal = HDI_pal, values = ~worldCountry_HDI$HDI,title = "<small>Index Value</small>")

basemap
  #addPolygons(
    #smoothFactor = 0.2,
    #fillOpacity = 1,
    #fillColor = ~HDI_pal(worldCountry_HDI$"2018"),
    #weight = 0.5,
    #opacity = 0.1,
    #color = "white",
    #dashArray = "1",
    #fillOpacity = 0.7,
    #highlight = highlightOptions(
     # weight = 0.5,
      #color = "#666",
      #dashArray = "—",
      #fillOpacity = 0.5,
      #bringToFront = TRUE),
    #label = labels,
    #labelOptions = labelOptions(
      #style = list("font-weight" = "normal", padding = "3px 8px"),
      #textsize = "15px",
      #direction = "auto")) %>%
  


#=================================================================
states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "list"
  )
class(states)


bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")
#========================================================================================================
# count the number of countries in different level of development of HDI
temp=all_data %>%
  filter(Year == 2018) %>%
  group_by(Level) %>%
  summarise(
    HDI = mean(HDI),
    number_of_distinct_orders = length(unique(Country))) %>%
  ungroup()

