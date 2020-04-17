# load packages
packages=c('corrplot','ggpubr','plotly','tidyverse','shiny','readxl','Hmisc','sf', 'tmap', 'tidyverse')

for(p in packages){library
  if (!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}  

# load data
data_HDI=read_excel('data/Table1.HDI&Components.xlsx')
# Change the data type
data_HDI %>% rename('HDI'='Human development index (HDI)')
data_HDI$`HDI rank...1` = as.numeric(data_HDI$`HDI rank...1`)
data_HDI$`Human development index (HDI)` = as.numeric(data_HDI$`Human development index (HDI)`)
data_HDI$`Life expectancy at birth` = as.numeric(data_HDI$`Life expectancy at birth`)
data_HDI$`Expected years of schooling` = as.numeric(data_HDI$`Expected years of schooling`)
data_HDI$`Mean years of schooling` = as.numeric(data_HDI$`Mean years of schooling`)
data_HDI$`Gross national income (GNI) per capita` = as.numeric(data_HDI$`Gross national income (GNI) per capita`)
data_HDI$`GNI per capita rank minus HDI rank` = as.numeric(data_HDI$`GNI per capita rank minus HDI rank`)
data_HDI$`HDI rank...9` = as.numeric(data_HDI$`HDI rank...9`)

# rename columns
data_HDI %>% 
  rename(
    "HDI rank...1" = "HDI_rank2018",
    "HDI rank...9" = "HDI_rank2017")

data_HDI = rename(data_HDI, "HDI_rank2018" = "HDI rank...1",
                 "HDI_rank2017" = "HDI rank...9")

# r
HDI_Country <-ggplot(data_HDI, aes(x=reorder(Country,-`Human development index (HDI)`),y=`Human development index (HDI)`))+
  geom_bar(stat="identity",color='skyblue',fill='steelblue')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
