packages=c('shiny','shinythemes','leaflet','tidyverse','RColorBrewer')

for(p in packages){library
    if (!require(p,character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}  

# Define UI for application that draws a histogram
ui <- bootstrapPage(
    #shinythemes::themeSelector(),
    tags$head("Human Development Report"),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               "Human Development Report", id="nav",
               tabPanel("World mapper",
                        div(leafletOutput("mymap"),
                            p(),
                            actionButton("recalc", "New points")
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
server <- function(input, output,session) {
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = points())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                              value = range(quakes$mag), step = 0.1
                  ),
                  selectInput("colors", "Color Scheme",
                              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                  ),
                  checkboxInput("legend", "Show legend", TRUE)
    )
)

server <- function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
    })
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
        colorNumeric(input$colors, quakes$mag)
    })
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(quakes) %>% addTiles() %>%
            fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        pal <- colorpal()
        
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                       fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
            )
    })
    
    # Use a separate observer to recreate the legend as needed.
    observe({
        proxy <- leafletProxy("map", data = quakes)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend(position = "bottomright",
                                pal = pal, values = ~mag
            )
        }
    })
}

shinyApp(ui, server)
