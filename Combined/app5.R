library(tidyverse)

#Load the required packages to run the app
rm(list = ls())
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(htmltools)
library(rgdal)
library(leaflet)
library(plotrix)

#Load the csv files
all_forests <- read_csv("Data/CSVs/all_forests.csv")
dat_molten<- read_csv("Data/CSVs/dat_molten.csv")
dat_sum<- read_csv("Data/CSVs/dat_sum.csv")
dataFinal<- read_csv("Data/CSVs/dataFinal.csv")
mammals<- read_csv("Data/CSVs/mammals.csv")
newData<- read_csv("Data/CSVs/newData.csv")
divFinal<- read_csv("Data/CSVs/divFinal.csv")
Activity <- read_csv("~/Documents/Shiny/Activity/Data/Activity.csv")


#Load the shapefiles to prep the map
Forests<-readOGR("Data/Shapefiles/Study Forest Locations.shp", layer = "Study Forest Locations")

#Project shape file
Forests_proj<-spTransform(Forests, CRS("+proj=longlat +datum=WGS84"))

#Add column with unabbreviated forest name
Forests_proj@data$Forest <- with(Forests@data, ifelse(
  ForestCode == "SH", 'South Hammond', ifelse(
    ForestCode == "BC", 'Beaver Creek', ifelse(
      ForestCode == "DON", 'Donnerville', ifelse(
        ForestCode == "WHIP", 'Whippoorwill Corners', ifelse(
          ForestCode == "WF", 'Whiskey Flats', ifelse(
            ForestCode == "DEG", 'Degrasse', 'whoops')))))))


ui <- fluidPage(
  
  # App title ----
  titlePanel("North Country Wild Zooniverse Project"),
  tabsetPanel(
    #Mammal Activity Patterns 
    tabPanel("Mammal Activity Patterns", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("mammals", h3("Choose your Species"),
                                        choices = c("Black Bear",
                                                    "Bobcat",
                                                    "Chipmunk",
                                                    "Coyote",
                                                    "Fisher",
                                                    "Flying Squirrel",
                                                    "Gray Squirrel",
                                                    "Mink",
                                                    "Opossum",
                                                    "Other Small Mammal",
                                                    "Porcupine",
                                                    "Raccoon",
                                                    "Red Fox",
                                                    "Red Squirrel",
                                                    "River Otter",
                                                    "Snowshoe Hare",
                                                    "Striped Skunk",
                                                    "Weasel",
                                                    "White-tailed Deer"),
                                        selected = "Black Bear"),
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("Mammals are not all active at the same time! Select a mammal using the tab in the 
               panel to the left and learn what times of day that mammal was detected by our cameras.
               The graph is shaped like a clock with 0-23 symbolizing the hour we detected the mammal.
               The x-axis lets you know how many detections we had during that hour."),
                         plotOutput(outputId = "activity"), imageOutput(outputId = "image"))   
             )
    )
    
  )
)
server <- function(input, output){
  #Species Activity Patterns
  output$activity<-renderPlot({
    
    title<- sprintf( "%s Activity Patterns", input$mammals) %>% lapply(htmltools::HTML)
  
  data<-switch(input$mammals, 
               "White-tailed Deer" = Activity %>% filter(bin == "DEERWHITETAILED"),
               "Chipmunk" = Activity %>% filter(bin == "CHIPMUNK"),
               "Coyote" = Activity %>% filter(bin == "COYOTE"),
               "Fisher" = Activity %>% filter(bin=="FISHER"),
               "Raccoon" = Activity %>% filter(bin =="RACCOON"),
               "Red Squirrel" = Activity %>% filter(bin == "SQUIRRELRED"),
               "Gray Squirrel" = Activity %>% filter(bin =="SQUIRRELGRAY"),
               "Black Bear" = Activity %>% filter(bin == "BLACKBEAR"),
               "Red Fox" = Activity %>% filter(bin == "FOXRED"),
               "Porcupine" = Activity %>% filter(bin == "PORCUPINE"),
               "Bobcat" = Activity %>% filter( bin == "BOBCAT"),
               "Weasel" = Activity %>% filter(bin == "WEASEL"),
               "Striped Skunk" = Activity %>% filter(bin == "SKUNKSTRIPED"),
               "Flying Squirrel" = Activity %>% filter(bin== "SQUIRRELFLYING"),
               "Snowshoe Hare" = Activity %>% filter(bin == "SNOWSHOEHARE"),
               "River Otter" = Activity %>% filter(bin == "RIVEROTTER"),
               "Mink" = Activity %>% filter(bin == "MINK"),
               "Other Small Mammal" = Activity %>% filter(bin == "OTHERSMALLMAMMAL"),
               "Opossum" = Activity %>% filter(bin == "OPOSSUM"))
  
  clock<-c(0:23)
  clock24.plot(data$NumObs, clock, show.grid = T, lwd = 2, line.col = "#165970", cex.lab = 0.5, main = title)
  
})
}
  shinyApp(ui = ui, server = server)