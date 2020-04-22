# load required packages
packages=c(#'corrplot',
           'ggpubr',
           'plotly',
           'tidyverse',
           'readxl',
           'Hmisc', 
           'geojsonio',
           'sf', 
           'tmap',
           #'spData',
           'maptools',
           'shiny',
           'shinythemes',
           'leaflet',
           'RColorBrewer')
           #'rnaturalearth',
           #'rnaturalearthdata'

for(p in packages){library
    if (!require(p,character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}  

# Import Data
worldcountry = geojson_read("data/50m.geojson", what = "sp")
HDI = read_csv('data/data_cleaned/HDI/0_HDI.csv') 
#worldcountry<-subset(worldcountry, NAME_LONG!="Antarctica")

HDI_selected = subset(HDI,Year==2018)
worldCountry_HDI <- merge(worldcountry, HDI_selected, by.x = "NAME_LONG", by.y = "Country")

# color scheme
HDI_pal <- colorQuantile("Blues", domain =  worldCountry_HDI$HDI)
#plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# set label content
labels <- sprintf(
    "<strong>%s</strong><br/> 
   Index: %g",
    worldCountry_HDI$NAME_LONG, worldCountry_HDI$HDI
) %>% lapply(htmltools::HTML)

# set popup
popup = sprintf(
    "<strong>%g</strong><br/>",worldCountry_HDI$HDI
) %>% lapply(htmltools::HTML)


# create basemap
basemap = leaflet(worldcountry) %>% 
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron)
basemap




#######    DATA PRECESSING    #########
# Extract Year from Table
# 现在已经弃用，使用固定的时间范围 
# min_year = min(HDI$Year)
# max_year = 2018#max(HDI$year)

ui <- bootstrapPage(
    #shinythemes::themeSelector(),
    tags$head("Human Development Report"),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, "Human Development Report", id="nav",
               tabPanel("World mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")), #使悬浮边栏变透明
                            leafletOutput("mymap",width = "100%", height = "100%"), # output World Map
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 180, left = 20, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto", # draggable 控制能否移动
                                          sliderInput(inputId = "Year",
                                                      label = h5("Select Year"),
                                                      min = 1990,#as.Date(cv_min_date,"%Y-%m-%d"),
                                                      max = 2018,#as.Date(current_date,"%Y-%m-%d"),
                                                      value = 2018, #as.Date(current_date),
                                                      timeFormat = '%Y',
                                                      #timeFormat = "%d %b", 
                                                      #animate=animationOptions(interval = 3000, loop = FALSE))),
                                          )),
                            # add a school logo
                            #absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          #tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80')))
                            )),
               tabPanel("HDI"),
               tabPanel("Gender Development Index"),
               tabPanel("Poverty Index"),
               tabPanel("Population"),
               tabPanel("Health"),
               tabPanel("Education"),
               tabPanel("Data"),
               tabPanel("About us")
    )
)

# Define server logic required to draw a histogram
server <- function(input,output,session) {
    reactive_db = reactive({
        worldcountry %>%
            merge(filter(HDI,Year==input$Year),by.x = "NAME_LONG", by.y = "Country") # here we can change the input of data
            
    })
    #reactive_polygons = reactive({
     #   reactive_db
        #worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
    #})
   
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$Year, {
        leafletProxy("mymap") %>% 
            addPolygons(data = reactive_db(), 
                        stroke = FALSE, 
                        smoothFactor = 0.1, 
                        fillOpacity = 0.15, 
                        fillColor = ~HDI_pal(reactive_db()$HDI),
                        ) %>%
            addLegend("bottomright", pal = HDI_pal, values = ~reactive_db()$HDI,title = "<small>Index Value</small>")
        
                  #group = "2019-COVID (cumulative)",
                  #label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$recovered, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                  #labelOptions = labelOptions(
                   #            style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                    #          textsize = "15px", direction = "auto")
                      })
}

# Run the application 
shinyApp(ui = ui, server = server)
