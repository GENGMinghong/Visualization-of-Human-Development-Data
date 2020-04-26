# load required packages
library(ggpubr)
library(plotly)
library(tidyverse)
library(readxl)
library(Hmisc)
library(geojsonio)
library(sf)
library(tmap)
library(maptools)
library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(heatmaply)
library(shinyHeatmaply)
library(gapminder)
library(ggalt)
library(seriation)
library(dendextend)
#packages=c('ggpubr',plotly','tidyverse','readxl','Hmisc','geojsonio','sf','tmap',
#           'maptools','shiny','shinythemes','leaflet','rgdal','RColorBrewer',
#           'heatmaply','shinyHeatmaply','gapminder','ggalt','seriation', 'dendextend')

#for(p in packages){library
#    if (!require(p,character.only = T)){
#        install.packages(p)
#    }
#    library(p,character.only = T)
#}  

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
names(all_data)[1]='Continent'
head(all_data)


# Prepare for choosing index
indexchoice <- colnames(all_data[,6:ncol(all_data)])

# Prepare for heatmap page
scale_choice <- c('none','row','column')
hcluster_choice <- c('ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty','median', 'centroid')
# alter hcluster choice (the later four are same with the choices in hcluster_Choice)
other_hcluster_choice <- c('ward.D', 'ward.D2', 'single', 'complete', 'UPGMA','WPGMA', 'WPGMC','UPGMC')
distribution_choice <- c('euclidean', 'maximum', 'manhattan', 'canberra', 'binary','minkowski')
seriate_choice <- c("OLO", "mean", "none", "GW")

# Prepare for scatter plot
colorchoice <- c('Continent','Region','Level')

#######    DATA PRECESSING    #########
# Extract Year from Table
# min_year = min(HDI$Year)
# max_year = 2018#max(HDI$year)


##### SHINY APP #####
ui<- bootstrapPage(
    #shinythemes::themeSelector(),
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, "Human Development Report", id="nav",
               tabPanel("Indicator Map",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")), 
                            leafletOutput("mymap",width = "100%", height = "100%"), # output World Map
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 80, left = 20, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto", # draggable 
                              
                                          h4(textOutput("HDI_text"), align = "right",style="color:#cc4c02"),
                                          #h4(textOutput("reactive_death_count"), align = "right"),
                                          #span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                          #span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                          #h6(textOutput("clean_date_reactive"), align = "right"),
                                          #h6(textOutput("reactive_country_count"), align = "right"),
                                          #tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                          #tags$i(h6("Reported cases are subject to significant variation in testing capacity between countries.")),
                                          plotOutput("distribution_HDI", height="130px", width="100%"),
                                          plotOutput("distribution_GDI", height="130px", width="100%"),
                                          plotOutput("distribution_GII", height="130px", width="100%"),
                                          sliderInput(inputId = "Year",
                                                      label = h5("Select Year"),
                                                      min = 1990,
                                                      max = 2018,
                                                      value = 2018,
                                                      timeFormat = '%Y',
                                                      #animate=animationOptions(interval = 3000, loop = FALSE),
                                          ),
                                          #selectInput('indicator', "Indicator", choices = colnames(all_data[, 6:ncol(all_data)])),
                                          #selectInput('Countries', NULL, choices = sort(as.character(all_data$Country) %>% unique)),
                                          ),
                            #absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                            #              tags$img(src='logo.jpg',height='40',width='80')),
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 80, right = 20, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto",
                                          #id = "controls2", top = 80, left = "auto", right = 20, bottom = "auto",
                                          #class = "panel panel-default",
                                          #fixed=TRUE, 
                                          #width = 250, height = "auto",draggable = FALSE,
                                          selectInput(inputId = "select_map_of_a_country",
                                                      label = h4("Country of Scope:"), 
                                                      choices = c("All country" = "All",
                                                                  sort(as.character(all_data$Country) %>% unique))),
                                          #uiOutput("secondselection")
                                          leafletOutput("map_on_the_right",width="240",height = "150"),
                                          h4(textOutput('choosen_year'),style="color:#006d2c"),
                                          h6(textOutput("country_HDI")),
                                          h6(textOutput("country_GDI")),
                                          h6(textOutput("country_GII"))
                                          ),
                                          #style = "opacity: 0.65; z-index: 10;", ## z-index modification
                            )),
               #tabPanel("Indexes",
               #        sidebarLayout(
               #            sidebarPanel(top = 80, left = 20,# width = 250,
               #                           width = 3,
               #                         selectInput(inputId = 'GraphType','Graph Type:', choices = c("Heatmap",
               #                                                                                       "Line",
               #                                                                                      "Density",
               #                                                                                       "Dumbbell"))
               #            ),
               #             mainPanel(
               #                h6("To be added...")
               #              )
               #),
               #),
               tabPanel("HDI and its Components",
                            sidebarLayout(
                                sidebarPanel(top = 80, left = 20,# width = 250,unique(all_data$Country)
                                             width = 3,
                                             selectInput("country","Choose Countries", 
                                                         choices = unique(all_data$Country),
                                                         selected = c('Singapore','China','India','United States'),
                                                         multiple = TRUE),
                                             sliderInput("year",'choose a year range', min = 1990, max = 2018, value = c(1990,2018),step = 1),
                                             actionButton("Search", "Search"),
                                             actionButton("Help","About")
                                             ),
                                mainPanel(
                                    tabsetPanel(
                                        tabPanel(
                                            absolutePanel(id = "HDI", plotlyOutput(outputId = "HDItrend"),class = "panel panel-default",
                                                          top = 400, left = 20, width = 250, fixed=TRUE, 
                                                          draggable = FALSE, height = "auto"),
                                                 fluidRow(class = 'row1', 
                                                          column(6, plotlyOutput(outputId = "LEtrend", height =200)),
                                                          column(6, plotlyOutput(outputId = "MStrend", height =200))),
                                                 fluidRow(class = 'row2', 
                                                          column(6, plotlyOutput(outputId = "EStrend", height = 210)),
                                                          column(6, plotlyOutput(outputId = "GNItrend", height = 210))))
                                        )
                                    )
                                )
                        ),
               tabPanel("Correlation",
                        sidebarLayout(
                            sidebarPanel(top = 80, left = 20, width = 3,
                                         selectInput('heatcountry','Choose Countries', 
                                                     choices = append('All',unique(all_data$Country)), 
                                                     multiple = TRUE,
                                                     selected = c('Singapore','China','India','United States')
                                                     ),
                                         selectInput('heatindex','Choose Indexes', 
                                                     choices = indexchoice, 
                                                     multiple = TRUE,
                                                     selected = c("HDI",
                                                                  "Gender_Development_Index",
                                                                  "Gender_Inequality_Index",
                                                                  "Total_Unemployment_Rate",
                                                                  "Education_Index")
                                                     ),
                                         sliderInput('heatyear','Choose a year', min = 1990, max = 2018, value = 2018, step = 1),
                                         textInput('cluster', 'Input the Number of Clusters'),
                                         selectInput('scale','Choose Scale',choices = scale_choice),
                                         selectInput('hcluster', 'Choose Cluster Method', choices = hcluster_choice),
                                         selectInput('distribution', 'Choose Distribution Method', choices = distribution_choice),
                                         selectInput('seriate','Choose seriate Method',choices = seriate_choice)
                                         ),
                            mainPanel(plotlyOutput('heatmap',width = "100%", height = "150%"))
                            
                        )),
               #tabPanel("Dumbbell Chart",
               #        sidebarLayout(
               #             sidebarPanel(top = 80, left = 20, width = 3,
               #                         selectInput('dumbbellcountry','Choose Countries', choices = unique(all_data$Country), multiple = TRUE),
               #                         selectInput('dumbellindex','Choose Indexes', choices = indexchoice)),
               #              mainPanel(plotlyOutput('dumbbell'))
               #          )),
               tabPanel("Bubble plot",
                        sidebarLayout(
                            sidebarPanel(top = 80, left = 20, width = 3,
                                         selectInput('xaxis','Choose an Index at x axis', choices = indexchoice),
                                         selectInput('yaxis','Choose an Index at y axis', choices = indexchoice),
                                         selectInput('size','Choose an Index to represent size', choices = indexchoice),
                                         selectInput('color','Choose an Index to represent color', choices = colorchoice),
                                         sliderInput('bubbleyear','Choose a year', min = 1995, max = 2018, value = 2018, step = 5, animate = TRUE)
                            ),
                            mainPanel(plotlyOutput('bubble')
                        )),
               tabPanel("Data",
                        numericInput("maxrows", "Rows to show", 25),
                        verbatimTextOutput("rawtable"),
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        "The dataset is downloaded from Human Development Report and cleaned by the team members."),
               tabPanel("About",
                        tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(Sys.Date())),
                            "The data used in this Shint Application is gathered from",
                            tags$a(href="http://hdr.undp.org/en/data", "United Natioms Development Programme (UNDP)"),
                            ", in the research of Human Development Report.", tags$br(),
                            #tags$br(),
                            tags$h4("User Guide"),
                            tags$h5("Indicator Mapper"),
                            "This indicator map provides a overview of the world to users. 
                            3 most important indicators are provided in this map, namely, Human Development Index, 
                            Gender Development Index and Gender Inequality Index. In the lower-right corner there is a 
                            control option, which can be used to control which map to be shown.", tags$br(),tags$br(),
                            "In the left panel, there are 3 mini-bar charts to show the distribution of 3 indicators 
                            of all countries in the world. And the slider bar controls the year of which the data we 
                            want to see. Once the input of year is changed, the map will be re-plotted. 
                            This process will last for about 30 seconds.", tags$br(),tags$br(),
                            "The main map is designed as a choropleth map and colored with blue. 
                            The darker the color, the higher the value it stands for.",tags$br(),tags$br(),
                            "There is a mini map in the upper right corner. In this map, user can 
                            select a certain country of his/her interest and then the view of map will "fly" to 
                            the country which is chosen by user. And the summarized information of this country 
                            in the selected year will appear under the mini map.",tags$br(),tags$br(),
                            "Note: Once open this Shiny app, it will take about 30 seconds to 
                            finish the calculation of the page indicator map. And re-plotting this map requires equal time length."
                            ,tags$br(),tags$br(),
                            tags$h5("HDI and its Components"),tags$br(),
                            "Human Development Index is composited of 4 components: Life Expectancy, Mean of Schooling Year, 
                            Expected Schooling Year and Gross National Income. In this page, the user can dig into the Human Development Index 
                            and explore more details. In the control panel, the user can choose a list of countries and a time-period. 
                            In the main panel, 5 tabs are provided. User can choose one of them to see the change of each aspect individually."
                            ,tags$br(),tags$br(),
                            tags$h5("Correlation"),tags$br(),
                            "The design of this page is to use heatmap to reveal the correlation across a variety of indicators. For countries, 
                            user can choose any number of countries (can be all countries) to draw a heatmap. The choice of indexes is also flexible. 
                            In the slider bar, the input of year and number of clusters are changeable. Also we can choose the scale, cluster method, 
                            distribution method and seriate method."
                            ,tags$br(),tags$br(),
                            tags$h5('Data'),tags$br(),
                            'In this page, the overview of the whole dataset is placed. Users can download the dataset as a CSV format file by 
                            clicking the button 鈥淒ownload as CSV鈥?.'
                            ,tags$br(),tags$br(),
            
                            tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/GENGMinghong/Visualization-of-Human-Development-Data", "Github."),
                            tags$br(),tags$br(),
                            
                            tags$h4("Sources"),
                            tags$b("Human Development Report"), tags$a(href="http://www.hdr.undp.org/en/2019-report", "United Nations Development Programme"),
                            tags$br(),tags$br(),
                            
                            tags$h4("Authors"),
                            "GENG Minghong, Master of IT in business, Singapore Management University.","   mhgeng.2019@mitb.smu.edu.sg",tags$br(),
                            "JI Xiaojun, Master of IT in business, Singapore Management University.","   xiaojun.ji.2019@mitb.smu.edu.sg",tags$br(),
                            "ZHU Honglu, Master of IT in business, Singapore Management University.","   hlzhu.2019@mitb.smu.edu.sg",tags$br(),
                            tags$br(),tags$br()
                        )
               )
) # finish navbarPage
) # finish ui

# Sever Design
server <- function(input,output,session) {
    #________Mapper Page, Author: GENG Minghong__________________________________________________________
    reactive_db = reactive({
        worldcountry %>%
            merge(filter(all_data,Year==input$Year),by.x = "NAME_LONG", by.y = "Country") # here we can change the input of data
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$Year, {
        leafletProxy("mymap") %>% 
            addPolygons(data = reactive_db(), 
                        smoothFactor = 0.2, 
                        fillColor = ~colorQuantile("Blues",domain = reactive_db()$HDI)(reactive_db()$HDI), 
                        fillOpacity = 0.7,
                        color="white", #stroke color
                        weight = 1, # stroke width in pixels
                        highlight = highlightOptions(
                            #weight = 5,
                            color = "#666",
                            #dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE # Whether the shape should be brought to front on hover
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
                        fillColor = ~colorQuantile("Blues",domain = reactive_db()$Gender_Development_Index
                        )(reactive_db()$Gender_Development_Index), 
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
            # add gender inequality index 
            addPolygons(data = reactive_db(), 
                        smoothFactor = 0.2, 
                        fillColor = ~colorQuantile("Blues",domain = reactive_db()$Gender_Inequality_Index
                        )(reactive_db()$Gender_Inequality_Index), 
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
                            "<strong>%s</strong><br/>GII Index: %g",
                            reactive_db()$NAME_LONG, reactive_db()$Gender_Inequality_Index
                        ) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"),
                        group = "Gender Equality Index") %>%
            #popup = popup,
            #popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
            addLayersControl(
                position = "bottomright",
                baseGroups = c("Human Development Index","Gender Development Index","Gender Inequality Index"),
                #overlayGroups = c("Human Development Index","Gender Development Index"), 
                options = layersControlOptions(collapsed = FALSE)) %>%
            #addLegend("bottomright", pal = colorQuantile("Blues",domain = reactive_db()$HDI),
            #          values = ~reactive_db()$HDI,title = "<small>Index Value</small>") %>%
            #    position = "bottomright"
            hideGroup(c('Human Development Index',"Gender Development Index","Gender Inequality Index"))
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
    
    output$distribution_GII = renderPlot({
        all_data %>%
            filter(Year==input$Year) %>% 
            ggplot(aes(x = reorder(Country,-Gender_Inequality_Index), 
                       y = Gender_Inequality_Index,
                       #fill = Region #color = Country,
            ))+
            geom_bar(position="stack", stat="identity",fill = "#cc4c02")+#fill = "#cc4c02")+
            ylab("") + 
            xlab("Country")+
            ggtitle("Gender Inequality Index")+
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
        paste0("In ", input$Year, " the distribution of HDI,GDI and GII of all countries")
    })
    
    output$map_on_the_right <- renderLeaflet({ 
        basemap
    })
    
    # Draw a map of one country of interest in the right floting panel
    reactive_db_onecountry = reactive({
        if(input$select_map_of_a_country=='All')
            worldcountry %>%
            merge(filter(all_data,Year==input$Year),by.x = "NAME_LONG", by.y = "Country")
        else
            worldcountry %>%
            merge(filter(all_data,Year==input$Year, Country ==input$select_map_of_a_country),by.x = "NAME_LONG", by.y = "Country") # here we can change the input of data
    })
    # this can be used in later graphs
    #observe({
    #    mapdata <- subset(worldcountry, NAME_LONG == input$Countries)
    #    rgn <- mapdata@bbox %>% as.vector()
    #    leafletProxy("mymap", session) %>% clearShapes() %>%
    #        flyToBounds(rgn[1], rgn[2], rgn[3], rgn[4])
    #})
    
    observeEvent(input$select_map_of_a_country, {
        mapdata <- subset(worldcountry, NAME_LONG == input$select_map_of_a_country)
        rgn <- mapdata@bbox %>% as.vector()
        leafletProxy("map_on_the_right", session) %>% clearShapes() %>%
            flyToBounds(rgn[1], rgn[2], rgn[3], rgn[4]) %>%
            addPolygons(data = mapdata, fill = T, fillColor = 'gold', color = 'white')
    })
    # Right Panel Function 2: HDI, GDI and GII for a country in a certain year.
    # Build a dynamic data table
    reactive_all_data_one_country = reactive({
        all_data %>%
            filter(Country == input$select_map_of_a_country, Year == input$Year)
    })
    output$choosen_year <- renderText({
        paste0('In ',(reactive_all_data_one_country()$Year))
    })
    
    # for a certain year, what is the HDI for a country 
    output$country_HDI <- renderText({
        paste0("HDI : ", (reactive_all_data_one_country()$HDI))
    })
    
    # for a certain year, what is the GDI for a country
    output$country_GDI <- renderText({
        paste0("GDI : ", (reactive_all_data_one_country()$Gender_Development_Index))
    })
    
    # for a certain year, what is the GII for a country
    output$country_GII <- renderText({
        paste0("GII  : ", (reactive_all_data_one_country()$Gender_Inequality_Index))
    })
    
    #__________Page 2 : HDI__________________________________________________
    #_______ Writer: JI Xiao Jun ____________________________________________
    
    ## filter data6
    #updateSelectInput(session,inputId='country', label = 'Choose a Country',choices= c(sort(as.character(all_data$Country) %>% unique)))
    extract_data <- reactive({
        all_data %>%
            filter(Country == input$country,
                   Year >= input$year[1],
                   Year <= input$year[2])
    })
    
    ## HDI trend plot
    reactive_HDI <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y = ~HDI, color = ~Country) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Human Development Index')
    })
    
    output$HDItrend <- renderPlotly({reactive_HDI()})
    
    ## Life Expectancy trend plot
    reactive_LifeExpectancy <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y = ~Life_Expectancy_at_Birth, color = ~Country) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Life Expectancy at Birth')
    })
    output$LEtrend <- renderPlotly({reactive_LifeExpectancy()})
    
    ## Expected Schooling trend plot
    reactive_ExpectedSchooling <- eventReactive(input$Search,{
        extract_data()%>%

            plot_ly(x = ~Year, y=~Expected_Years_of_Schooling, color = ~Country) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Expected Years of Schooling')
    })
    output$EStrend <- renderPlotly({reactive_ExpectedSchooling()})
    
    ## Mean Schooling trend plot
    reactive_MeanSchooling <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Mean_Years_of_Schooling, color = ~Country) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Mean Years of Schooling')
    })
    output$MStrend <- renderPlotly({reactive_MeanSchooling()})
    
    ## GNI per capita trend plot
    reactive_GNI <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Gross_National_Income_per_capita, color = ~Country) %>%
            add_lines()%>%
            layout(legend = list(orientation = 'h'), height =200,  title = 'Gross National Income per capita')
    })
    output$GNItrend <- renderPlotly({reactive_GNI()})
    
    
    #_________Page : Data _______________________
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("Human_Development_Report", ".csv", sep="")
        },
        content = function(file) {
            write.csv(all_data, file)
        }
    )
    
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(all_data, input$maxrows), row.names = FALSE)
        options(orig)
    })
    
    #______________Heatmap Page__________________________________________________
    #_______ Writer: JI XIAOJUN________________________________________________
    output$heatmap <- renderPlotly({
        if (input$heatcountry == 'All')
            {heatmap <- all_data %>%
                        filter(Year == input$heatyear)%>%
                        column_to_rownames(var = "Country")%>%
                        select(input$heatindex)}
        else 
            {heatmap <- all_data %>%
                        filter(Year == input$heatyear) %>%
                        filter(Country %in% input$heatcountry) %>%
                        column_to_rownames(var = "Country") %>%
                        select(input$heatindex)}
        heatmaply(normalize(heatmap),
                  scale = input$scale,
                  dist_method = input$distribution,
                  hclust_method = input$hcluster, 
                  seriate = input$seriate,
                  #input$cluster,
                  colors = Blues,
                  Colv=NA,
                  #margins = c(10,50,50,NA),
                  fontsize_row = 4,
                  fontsize_col = 5,
                  #main="World Happiness Score and Variables by Country, 2018 \nDataTransformation using Normalise Method",
                  xlab = "Indicators",
                  ylab = "Countries",
                  main = ~paste("World Human Development Level and Variables by Country,",input$heatyear))%>%
            layout(height = 600, width = 800)

        #heatmap_data <- all_data %>%
        #    filter(Year == input$heatyear) %>%
        #    filter(Country %in% input$heatcountry)%>%
        #    select(input$heatindex)
        
        #heatmap_data <- filter_year%>%
        #    filter(Country %in% input$heatcountry)%>%
        #    select(input$heatindex)
        #row.names(heatmap_data) <- heatmap_data$Country
        
        #heatmap_matrix <- data.matrix(heatmap_data)
        #row.names(heatmap_matrix) <- heatmap_data$Country
        #heatmaply(normalize(heatmap_matrix),
        #          scale = input$scale,
        #          dist_method = input$distribution,
        #          hclust_method = input$hcluster, 
        #          seriate = input$seriate)
    })
    
    
    #______________Dumbbel Chart Page____________________________________________
    #_______ Writer: JI XIAOJUN  ________________________________________________ 
    
    # Prepare for Dumbbell chart
    output$dumbbell<-renderPlotly({
        dumbbell_data <- data %>%
            select(Country, Year,input$dumbellindex) %>%
            filter(Year == 1995 | Year == 2018, Country %in% input$dumbbellcountry) %>%
            spread(Year, input$dumbellindex) %>%
            group_by(Country)%>%
            mutate(gap = `2018`-`1995`)%>%
            arrange(desc(gap))
        
        dumbbell_data  %>% 
            plot_ly(height = 550) %>% 
            add_segments( x = ~`1995`, xend = ~`2018`, 
                          y = ~input$dumbbellcountry, yend = ~input$dumbbellcountry, showlegend= FALSE) %>% 
            add_markers(x = ~`1995`, y = ~input$dumbbellcountry, name = "index:1995", color = I("pink")) %>% 
            add_markers(x = ~`2018`, y = ~input$dumbbellcountry, name = "index:2018", color = I("blue")) %>% 
            layout(title = "Journey of All Countries in xxx", 
                   xaxis = list(title = input$dumbellindex, 
                                tickfont = list(color = "#e6e6e6")), 
                   yaxis = list(title = "Countries", tickfont = list(color = "#e6e6e6")))
    })
  
    #__________Page 5 : Bubble Plot _________________________________________
    #__________Writer: Zhu Honglu ___________________________________________
    output$bubble <- renderPlotly({
        
        req(input$xaxis)
        req(input$yaxis)
        
        scatter_data <- all_data %>% 
            filter(Year == input$bubbleyear)
        
        ggplotly(
            ggplot(scatter_data, 
                   aes_string(x = paste0("`", input$xaxis,"`"), 
                              y = paste0("`",input$yaxis,"`"), 
                              col = paste0("`",input$color,"`"), 
                              size = paste0("`", input$size,"`"))) + 
                geom_point(alpha = 0.5) + 
                theme_classic()+
                geom_text(aes(x = 0.5, y= 0.5, label = Year), size = 10, color = 'lightgrey',family = 'Oswald')
               )
        
    })
    
    #output$bubbleplot = renderPlotly({
        #wbstats::wb(indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.CD", "SP.POP.TOTL"), 
        #            country = "countries_only", startdate = 1990, enddate = 2018)  %>% 
        # pull down mapping of countries to regions and join
        #   dplyr::left_join(wbstats::wbcountries() %>% 
        #                     dplyr::select(iso3c, region)) %>% 
        # spread the three indicators
        #   tidyr::pivot_wider(id_cols = c("date", "country", "region"), names_from = indicator, values_from = value) %>% 
        # plot the data
        # ggplot2::ggplot(aes(x = log(`GDP per capita (current US$)`), y = `Life expectancy at birth, total (years)`,
        #                        size = `Population, total`)) +
        #    ggplot2::geom_point(alpha = 0.5, aes(color = region)) +
        #    ggplot2::scale_size(range = c(.1, 16), guide = FALSE) +
        #    ggplot2::scale_x_continuous(limits = c(2.5, 12.5)) +
        #    ggplot2::scale_y_continuous(limits = c(30, 90)) +
        #    viridis::scale_color_viridis(discrete = TRUE, name = "Region", option = "plasma") +
        #    ggplot2::labs(x = "Log GDP per capita",
        #                  y = "Life expectancy at birth") +
        #    ggplot2::theme_classic() +
        #    ggplot2::geom_text(aes(x = 7.5, y = 60, label = date), size = 14, color = 'lightgrey', family = 'Oswald') +
            # animate it over years
        #    gganimate::transition_states(date, transition_length = 1, state_length = 1) +
        #    gganimate::ease_aes('cubic-in-out')
    #})
}


# Run the application
shinyApp(ui = ui, server = server)

