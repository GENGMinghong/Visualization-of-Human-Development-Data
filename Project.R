library(shiny)
library(WDI)

library(ggvis)
```

```{r}
library(WDI)
indicator2 <- WDI(country="all", indicator=c("NY.GDP.PCAP.CD", "SP.POP.TOTL", "SP.DYN.LE00.IN"), start=2011, end=2018, extra = TRUE)
drops <- c("iso2c","iso3c", "capital", "longitude", "latitude", "income", "lending")
indicator2 <- indicator2[ , !(names(indicator2) %in% drops)]
colnames(indicator2) <- c("country","year", "GDP_per_capita", "population_total", "life_expectancy", "region")
indicator2 <- indicator2[-c(1, 2, 3, 4, 5, 6, 19, 66, 67, 159, 178, 179, 180, 181, 182, 201, 202, 203, 204, 205, 206, 207, 225, 226, 227, 228, 236, 237, 238, 239, 240, 241, 242, 243, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 262, 263), ]

```

```{r}
ui <- fluidPage(
  sliderInput(inputId = "Year",
              label = h5("Select Year"),
              min = 2011,#as.Date(cv_min_date,"%Y-%m-%d"),
              max = 2018,#as.Date(current_date,"%Y-%m-%d"),
              value = 2018, #as.Date(current_date),
              timeFormat = '%Y',
              #timeFormat = "%d %b", 
              #animate=animationOptions(interval = 3000, loop = FALSE),
  ),
  plotOutput("graph")
)
```

```{r}
server <- function(input, output) {
  
  output$graph = renderPlot({
    
    indicator2 %>%
      ggvis(~GDP_per_capita, ~life_expectancy, fill=~factor(region)) %>%
      layer_points(size= ~population_total/1000000,opacity:=0.6) %>%
      add_legend(scales = "size", properties = legend_props(legend = list(y = 200))) %>%
      scale_numeric("y", domain = c(0, 90), nice = FALSE) %>%
      add_axis("x", title = "GDP per Capita(USD)") %>%
      add_axis("x", orient = "top", ticks = 0, title = "GDP per Capita vs. Life Expectancy 2019",
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0)))
  })
  
}


```

```{r}
shinyApp(ui = ui, server = server)
```
=======
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
worldcountry<-subset(worldcountry, NAME_LONG!="Antarctica")
country_geoms = read_csv("data/country_geoms.csv")

HDI = read_csv('data/data_cleaned/HDI/0_HDI.csv') 
HDI_selected = subset(HDI,Year==2018)
worldCountry_HDI <- merge(worldcountry, HDI_selected, by.x = "NAME_LONG", by.y = "Country")

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
              fillColor = ~HDI_pal(worldCountry_HDI$HDI), # ÊÇÂÖÀªÄÚµÄÑÕÉ«
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
      #dashArray = "¡ª",
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
            position = "bottomright"）
