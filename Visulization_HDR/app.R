# load required packages
packages=c('ggpubr',
           'plotly',
           'tidyverse',
           'readxl',
           'Hmisc', 
           'geojsonio',
           'sf', 
           'tmap',
           'maptools',
           'shiny',
           'shinythemes',
           'leaflet',
           'rgdal',
           'RColorBrewer')
        
for(p in packages){library
    if (!require(p,character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}  

# Import Data
worldcountry = geojson_read("data/50m.geojson", what = "sp")
worldcountry@data$NAME_LONG[worldcountry@data$NAME_LONG %in% c('Taiwan','Macao')] <- 'China'
all_data = read_csv('data/data_cleaned/All_data.csv')


# set label content
#labels <- sprintf(
#    "<strong>%s</strong><br/> 
#   Index: %g",
#    worldCountry_HDI$NAME_LONG, worldCountry_HDI$HDI
#) %>% lapply(htmltools::HTML)

# set popups
#popup = sprintf(
#    "<strong>%g</strong><br/>",worldCountry_HDI$HDI
#) %>% lapply(htmltools::HTML)


# create basemap
basemap = leaflet(worldcountry) %>% 
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron)

# Prepare for HDI page
#names(all_data)[4]="Country"
names(all_data)[1]='Continent'
choice <- colnames(all_data)[1:4]
head(all_data)
print(choice)






#######    DATA PRECESSING    #########
# Extract Year from Table
# 现在已经弃用，使用固定的时间范围 
# min_year = min(HDI$Year)
# max_year = 2018#max(HDI$year)


##### SHINT APP #####
ui <- bootstrapPage(
    #shinythemes::themeSelector(),
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, "Human Development Report", id="nav",
               tabPanel("World mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")), #使悬浮边栏变透明
                            leafletOutput("mymap",width = "100%", height = "100%"), # output World Map
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 80, left = 20, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto", # draggable 控制能否移动
                                          
                                          h3(textOutput("HDI_text"), align = "right"),
                                          
                                          #h4(textOutput("reactive_death_count"), align = "right"),
                                          #span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                          #span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                          #h6(textOutput("clean_date_reactive"), align = "right"),
                                          #h6(textOutput("reactive_country_count"), align = "right"),
                                          #tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                          #tags$i(h6("Reported cases are subject to significant variation in testing capacity between countries.")),
                                          plotOutput("distribution_HDI", height="130px", width="100%"),
                                          plotOutput("distribution_GDI", height="130px", width="100%"),
                                          sliderInput(inputId = "Year",
                                                      label = h5("Select Year"),
                                                      min = 1990,
                                                      max = 2018,
                                                      value = 2018,
                                                      timeFormat = '%Y',
                                                      #animate=animationOptions(interval = 3000, loop = FALSE),
                                          ),
                                          selectInput('Countries', NULL, choices = sort(as.character(all_data$Country) %>% unique)),
                                          ))),
               tabPanel("HDI",
                            sidebarLayout(
                                sidebarPanel(top = 80, left = 20,# width = 250,
                                             width = 3,
                                             selectInput('level','Choose a Level', choices = choice),
                                             selectInput("country","Choose countries",choices = unique(all_data$Country), multiple = TRUE),
                                             sliderInput("year",'choose year', min = 1990, max = 2018, value = c(1990,2018),step = 1),
                                             actionButton("Search", "Search"),
                                             actionButton("Help","About")
                                ),
                                mainPanel(
                                    fluidRow(column(plotly::plotlyOutput(outputId = "LEtrend"),width = 4, height = 3),
                                             column(plotly::plotlyOutput(outputId = "MStrend"),width = 4),
                                             column(plotly::plotlyOutput(outputId = "EStrend"),width = 4)),
                                    fluidRow(column(plotly::plotlyOutput(outputId = "GNItrend"),width = 4),
                                             column(plotly::plotlyOutput(outputId = "HDItrend"),width = 4))
                                )
                            )
                        ),
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
            merge(filter(all_data,Year==input$Year),by.x = "NAME_LONG", by.y = "Country") # here we can change the input of data
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    # this can be used in later graphs
    #observe({
    #    mapdata <- subset(worldcountry, NAME_LONG == input$Countries)
    #    rgn <- mapdata@bbox %>% as.vector()
    #    leafletProxy("mymap", session) %>% clearShapes() %>%
    #        flyToBounds(rgn[1], rgn[2], rgn[3], rgn[4])
    #})
    observeEvent(input$Year, {
        leafletProxy("mymap") %>% 
            addPolygons(data = reactive_db(), 
                        smoothFactor = 0.2, 
                        fillColor = ~colorQuantile("Blues",domain = reactive_db()$HDI)(reactive_db()$HDI), # 是轮廓内的颜色
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
                        label = sprintf(
                            "<strong>%s</strong><br/>HDI Index: %g",
                            reactive_db()$NAME_LONG, reactive_db()$HDI
                        ) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"),
                        group = "Human Development Index",
                        #popup = popup,
                        #popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
                       
                        ) %>%
            addPolygons(data = reactive_db(), 
                        smoothFactor = 0.2, 
                        fillColor = ~colorQuantile("Greens",domain = reactive_db()$Gender_Development_Index
                                                   )(reactive_db()$Gender_Development_Index), # 是轮廓内的颜色
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
                        label = sprintf(
                            "<strong>%s</strong><br/>GDI Index: %g",
                            reactive_db()$NAME_LONG, reactive_db()$Gender_Development_Index
                        ) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"),
                        group = "Gender Development Index") %>%
                        #popup = popup,
                        #popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
            addLayersControl(
                position = "bottomright",
                baseGroups = c("Human Development Index","Gender Development Index"),
                #overlayGroups = c("Human Development Index","Gender Development Index"),
                options = layersControlOptions(collapsed = FALSE))
                      })
    output$distribution_HDI = renderPlot({
        all_data %>%
            filter(Year==input$Year) %>% 
            ggplot(aes(x = reorder(Country,-HDI), 
                       y = HDI,
                       #fill = Region #color = Country,
            ))+
            geom_bar(position="stack", stat="identity",fill = "#cc4c02")+#fill = "#cc4c02")+
            ylab("") + 
            xlab("Country")+
            ggtitle("Human Development Index")+
            #scale_x_categorical(breaks=seq(0, 10, 1))
            theme_bw() + 
            #scale_fill_manual(values=c("#cc4c02")) +
            scale_y_continuous(expand = c(0, 0))+
            #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
            theme(legend.title = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "", 
                  plot.title = element_text(size=10), 
                  plot.margin = margin(5, 12, 5, 5))
    })
    
    output$distribution_GDI = renderPlot({
        all_data %>%
            filter(Year==input$Year) %>% 
            ggplot(aes(x = reorder(Country,-Gender_Development_Index), 
                       y = Gender_Development_Index,
                       #fill = Region#color = Country,
            ))+
            geom_bar(position="stack", stat="identity",fill = "#cc4c02")+#fill = "#cc4c02")+
            ylab("") +
            xlab("Country")+
            ggtitle("Gender Development Index")+
            #scale_x_categorical(breaks=seq(0, 10, 1))
            theme_bw() + 
            #scale_fill_manual(values=c("#cc4c02")) +
            scale_y_continuous(expand = c(0, 0))+
            #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
            theme(legend.title = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "", 
                  plot.title = element_text(size=10), 
                  plot.margin = margin(5, 12, 5, 5))
    })
    
    output$HDI_text <- renderText({
        paste0("In ", input$Year, "xx countries are high development countries")
    })
    
    ## filter data
    extract_data <- reactive({
        all_data %>%
            filter(Country == input$country,
                   Year >= input$year[1],
                   Year <= input$year[2])
    })
    
    ## HDI trend plot
    reactive_HDI <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~HDI, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, HDI)) %>%
            add_lines()%>%
            layout(showlegend=TRUE)
    })
    
    output$HDItrend <- renderPlotly({reactive_HDI()})
    
    ## Life Expectancy trend plot
    reactive_LifeExpectancy <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Life_Expectancy, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, Life_Expectancy)) %>%
            add_lines()%>%
            layout(showlegend=TRUE)
    })
    output$LEtrend <- renderPlotly({reactive_LifeExpectancy()})
    
    ## Expected Schooling trend plot
    reactive_ExpectedSchooling <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Expected_Years_of_Schooling, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, Expected_Years_of_Schooling)) %>%
            add_lines()%>%
            layout(showlegend=TRUE)
    })
    output$EStrend <- renderPlotly({reactive_ExpectedSchooling()})
    
    ## Mean Schooling trend plot
    reactive_MeanSchooling <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Mean_Years_of_Schooling, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, Mean_Years_of_Schooling)) %>%
            add_lines()%>%
            layout(showlegend=TRUE)
    })
    output$MStrend <- renderPlotly({reactive_MeanSchooling()})
    
    ## GNI per capita trend plot
    reactive_GNI <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~GNI_per_capita, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, GNI_per_capita)) %>%
            add_lines()%>%
            layout(showlegend=TRUE)
    })
    output$GNItrend <- renderPlotly({reactive_GNI()})
    
}


# Run the application 
shinyApp(ui = ui, server = server)
