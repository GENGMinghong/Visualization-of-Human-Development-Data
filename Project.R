```{r}
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