# load required packages
library(shiny)
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
           'RColorBrewer',
           'heatmaply',
           'shinyHeatmaply',
           'gapminder',
           'ggalt',
           'seriation', 
           'dendextend', 
           'heatmaply')

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
#all_data = read_csv('data/data_cleaned/All_data.csv', row.names=NULL) # for heatmap part
#rownames(all_data)

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


#######    DATA PRECESSING    #########
# Extract Year from Table
# 鐜板湪宸茬粡寮冪敤锛屼娇鐢ㄥ浐瀹氱殑鏃堕棿鑼冨�? 
# min_year = min(HDI$Year)
# max_year = 2018#max(HDI$year)


##### SHINY APP #####
ui <- bootstrapPage(
    #shinythemes::themeSelector(),
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, "Human Development Report", id="nav",
               tabPanel("World mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")), #浣挎偓娴竟鏍忓彉閫忔槑
                            leafletOutput("mymap",width = "100%", height = "100%"), # output World Map
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 80, left = 20, width = 250, fixed=TRUE,
                                          draggable = FALSE, height = "auto", # draggable 鎺у埗鑳藉惁绉诲姩
                                          
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
               tabPanel("Indexes",
                        sidebarLayout(
                            sidebarPanel(top = 80, left = 20,# width = 250,
                                         width = 3,
                                         selectInput(inputId = 'GraphType','Graph Type:', choices = c("Heatmap",
                                                                                                      "Line",
                                                                                                      "Density",
                                                                                                      "Dumbbell"))
                            ),
                            mainPanel(
                                h6("To be added...")
                            )
               ),
               ),
               tabPanel("HDI and its Components",
                            sidebarLayout(
                                sidebarPanel(top = 80, left = 20,# width = 250,unique(all_data$Country)
                                             width = 3,
                                             selectInput("country","Choose Countries", choices = unique(all_data$Country), multiple = TRUE),
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
                                         selectInput('heatcountry','Choose Countries', choices = append('All',unique(all_data$Country)), multiple = TRUE),
                                         selectInput('heatindex','Choose Indexes', choices = indexchoice, multiple = TRUE),
                                         sliderInput('heatyear','Choose a year', min = 1990, max = 2018, value = 2018, step = 1),
                                         sliderInput('cluster', 'Input the Number of Clusters', min = 2, max = 4, value = 3),
                                         selectInput('scale','Choose Scale',choices = scale_choice),
                                         selectInput('hcluster', 'Choose Cluster Method', choices = hcluster_choice),
                                         selectInput('distribution', 'Choose Distribution Method', choices = distribution_choice),
                                         selectInput('seriate','Choose seriate Method',choices = seriate_choice)
                                         ),
                            mainPanel(plotlyOutput('heatmap',width = "100%", height = "150%"))
                            
                        )),
               tabPanel("Dumbbell Chart",
                        sidebarLayout(
                            sidebarPanel(top = 80, left = 20, width = 3,
                                         selectInput('dumbbellcountry','Choose Countries', choices = unique(all_data$Country), multiple = TRUE),
                                         selectInput('dumbellindex','Choose Indexes', choices = indexchoice)),
                            mainPanel(plotlyOutput('dumbbell'))
                        )),
               tabPanel("ZhuHonglu",
                        
                        
                        
                        
                        
                        
                        ),
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
                            tags$a(href="http://hdr.undp.org/en/data", "United Natioms Development Programme (UNDP)"),", in the research of Human Development Report.", tags$br(),
                            #tags$br(),
                            tags$h4("User Guide"),tags$br(),
                            tags$h5("World Mapper"),tags$br(),
                            "This panel provides...", tags$br(),
                            
                            
                            
                            
                            
                            
                            
                            
                            tags$h4("Background"), 
                            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                            tags$br(),tags$br(),
                            "In isolation, these headlines can be hard to interpret. 
                        How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                        This site is updated daily based on data published by Johns Hopkins University. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                            tags$br(),tags$br(),
                            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            tags$br(),tags$br(),
                            tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                            tags$b("2003-SARS cases: "), tags$a(href="https://www.who.int/csr/sars/country/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 confirmed deaths: "), tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 projected deaths: "), "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
                            tags$b("2009-H1N1 cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("2009-H1N1 case fatality rate: "), "a systematic review by ", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24045719", "Wong et al (2009)"), "identified 
                        substantial variation in case fatality rate estimates for the H1N1 pandemic. However, most were in the range of 10 to 100 per 100,000 symptomatic cases (0.01 to 0.1%).
                        The upper limit of this range is used for illustrative purposes in the Outbreak comarisons tab.",tags$br(),
                            tags$b("2014-Ebola cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),tags$br(),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                        )
               )
   ) # finish navbarPage
) # finish ui



# Define server logic required to draw a histogram
server <- function(input,output,session) {
    
    #________Mapper Page____________________________________________________________
    #______Writer: GENG Minghong____________________________________________________
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
                        fillColor = ~colorQuantile("Blues",domain = reactive_db()$HDI)(reactive_db()$HDI), # 是轮廓内的颜�?
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
                        fillColor = ~colorQuantile("Blues",domain = reactive_db()$Gender_Development_Index
                        )(reactive_db()$Gender_Development_Index), # 是轮廓内的颜�?
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
                        )(reactive_db()$Gender_Inequality_Index), # 是轮廓内的颜�?
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
                baseGroups = c("Human Development Index","Gender Development Index","Gender Inequality Index"),# 只可以选择一个的group
                #overlayGroups = c("Human Development Index","Gender Development Index"), # 可以堆叠的group
                options = layersControlOptions(collapsed = FALSE)) %>%
            #addLegend(
            #    pal = ~colorQuantile("Blues",domain = reactive_db()$HDI, values = ~density, 
            #                         opacity = 0.7, title = NULL,
            #    position = "bottomright"
            hideGroup(c("Gender Development Index","Gender Inequality Index"))
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
    
    extract_data <- reactive({
        all_data %>%
            filter(Country == input$country,
                   Year >= input$year[1],
                   Year <= input$year[2])
    })
    
    ## HDI trend plot
    reactive_HDI <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y = ~HDI, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, HDI)) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Human Development Index')
    })
    
    output$HDItrend <- renderPlotly({reactive_HDI()})
    
    ## Life Expectancy trend plot
    reactive_LifeExpectancy <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y = ~Life_Expectancy_at_Birth, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, Life_Expectancy_at_Birth)) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Life Expectancy at Birth')
    })
    output$LEtrend <- renderPlotly({reactive_LifeExpectancy()})
    
    ## Expected Schooling trend plot
    reactive_ExpectedSchooling <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Expected_Years_of_Schooling, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, Expected_Years_of_Schooling)) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Expected Years of Schooling')
    })
    output$EStrend <- renderPlotly({reactive_ExpectedSchooling()})
    
    ## Mean Schooling trend plot
    reactive_MeanSchooling <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Mean_Years_of_Schooling, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, Mean_Years_of_Schooling)) %>%
            add_lines()%>%
            layout(showlegend=FALSE, height =200, title = 'Mean Years of Schooling')
    })
    output$MStrend <- renderPlotly({reactive_MeanSchooling()})
    
    ## GNI per capita trend plot
    reactive_GNI <- eventReactive(input$Search,{
        extract_data()%>%
            plot_ly(x = ~Year, y=~Gross_National_Income_per_capita, color = ~Country, hoverinfo = "text",
                    text = ~paste(input$country, Gross_National_Income_per_capita)) %>%
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
                  k_row = input$cluster,
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
                   yaxis = list(title = "Countries", tickfont = list(color = "#e6e6e6")), 
                   plot_bgcolor = "#808080", paper_bgcolor ="#808080")
    })
}


# Run the application s
shinyApp(ui = ui, server = server)

