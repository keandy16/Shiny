#Load the required packages to run the app
rm(list = ls())
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(htmltools)

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
  
 
    # Main panel for displaying outputs ----
 tabsetPanel(
   tabPanel(
     headerPanel("Map of Study Sites"),
     sidebarLayout(
       sidebarPanel(h6("Powered by:"),
                    img(src = "NatureUpNorth.png", height = 100, width = 300),
                    ),
         checkboxGroupInput("species", 
                                       h3("Choose your Species"), 
                                       choices = list("All Mammals",
                                                      "Black Bear",
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
                                       selected = "Black Bear"))
     ),
     mainPanel(
       leafletOutput("speciesmap")
     )
         
   
     ),
   
 
 
 
  
 tabPanel(
   headerPanel("Number of Detections per Species"),
            sidebarLayout(
   sidebarPanel(h6("Powered by:"),
                img(src = "NatureUpNorth.png", height = 100, width = 300),
     selectInput("forest", h3("Choose your Forest"),
                        choices = c("All Forests", "South Hammond", "Donnerville", "Beaver Creek", "Whippoorwill Corners", "Whiskey Flats", "Degrasse"), selected = "All Forests"),
    
            checkboxGroupInput("species", 
                                          h3("Choose your Species"), 
                                          choices = list("All Mammals",
                                                         "Black Bear",
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
                                                          selected = "Black Bear"))
            ),        
 mainPanel(
   plotOutput(outputId = "foresthist")
   )
 ),
 tabPanel(
   headerPanel("Mammal Activity Patterns"),
   sidebarLayout(         
   sidebarPanel(h6("Powered by:"),
                img(src = "NatureUpNorth.png", height = 100, width = 300),
     selectInput("species", h3("Choose your Species"),
                                   choices = c("All Mammals",
                                               "Black Bear",
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
                                               "White-tailed Deer"), selected = "Black Bear"))
   ),
 mainPanel(
   plotOutput(outputId = "activity")
   )
 ),
 tabPanel(
   headerPanel("Species Trophic Levels"),
   sidebarLayout(         
   sidebarPanel(h6("Powered by:"),
                img(src = "NatureUpNorth.png", height = 100, width = 300),
     selectInput("forest", h3("Choose your Forest"),
                                   choices = c("All Forests", "South Hammond", "Donnerville", "Beaver Creek", "Whippoorwill Corners", "Whiskey Flats", "Degrasse"), selected = "All Forests"))
   ),
          mainPanel(
            plotOutput(outputId = "trophic")
            ) 
 ),
  
 tabPanel(
   headerPanel("Forest Composition"),
               sidebarLayout(
     sidebarPanel(h6("Powered by:"),
                  img(src = "NatureUpNorth.png", height = 100, width = 300),
       selectInput("forest", h3("Choose your Forest"),
                                            choices = c("All Forests", "South Hammond", "Donnerville", "Beaver Creek", "Whippoorwill Corners", "Whiskey Flats", "Degrasse"), selected = "All Forests"))
               ),       
              mainPanel(
                plotOutput(outputId = "covariates")
                )
 ),
tabPanel(
  headerPanel("Forest Diversity"),
                sidebarLayout(
   sidebarPanel(h6("Powered by:"),
                img(src = "NatureUpNorth.png", height = 100, width = 300),
     selectInput("forest", h3("Choose your Forest"),
                                            choices = c("All Forests", "South Hammond", "Donnerville", "Beaver Creek", "Whippoorwill Corners", "Whiskey Flats", "Degrasse"), selected = "All Forests"))
            
                ),
             mainPanel(plotOutput(outputId = "diversity"))
               )
)

      
    
 


server <- function(input, output){
  #Map of Study Sites
  output$speciesmap<-renderLeaflet({
    
if("All Mammals" %in% input$species){
    data<-dat_sum
    #Join data to shape file
    Forests_proj@data <- left_join(Forests_proj@data, data, by = c("Forest"= "Forest"))
    
    pal <- colorNumeric("Blues", domain= data$number_det) 
    
    labels<-sprintf( "%s, Number of Detections %s", 
                     Forests_proj$Forest, data$number_det) %>% lapply(htmltools::HTML)
    
    leaflet() %>% addTiles() %>% 
      setView(lng = -75.169395, lat = 44.595466, zoom = 8) %>% 
      addPolygons(
        data = Forests_proj, 
        fillColor = ~pal(data$number_det),
        fillOpacity = 0.5,
        weight = 1, 
        col = 'red',
        highlight = highlightOptions(#highlight lets you mouse over a county and have it change color
          weight = 5,
          color = "orange", 
          bringToFront = T),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) 
}
    else{
    
  choices<-c(input$species)
  data<-dat_sum %>% filter(Species %in% choices)
 
  
  #Join data to shape file
  Forests_proj@data <- left_join(Forests_proj@data, data, by = c("Forest"= "Forest"))
  
  pal <- colorNumeric("Blues", domain= data$number_det) 
  
  labels<-sprintf( "%s, Number of Detections %s", 
                   Forests_proj$Forest, data$number_det) %>% lapply(htmltools::HTML)
  
  leaflet() %>% addTiles() %>% 
    setView(lng = -75.169395, lat = 44.595466, zoom = 8) %>% 
    addPolygons(
      data = Forests_proj, 
      fillColor = ~pal(data$number_det),
      fillOpacity = 0.5,
      weight = 1, 
      col = 'red',
      highlight = highlightOptions(#highlight lets you mouse over a county and have it change color
        weight = 5,
        color = "orange", 
        bringToFront = T),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) 
    }

  })
  
  
  
  
  output$foresthist <- renderPlot({
    
    if("All Mammals" %in% input$species){
      choices<-c(unique(newData$Species))
      data<-newData %>% filter(Species %in% choices)
      study<-reactive(switch(input$forest,
                             "All Forests" = data,
                             "South Hammond" = data %>% filter(Forest=="South Hammond"),
                             "Beaver Creek" = data %>% filter(Forest=="Beaver Creek"),
                             "Donnerville" = data %>% filter(Forest == "Donnerville"),
                             "Whippoorwill Corners" = data %>% filter(Forest == "Whippoorwill Corners"),
                             "Whiskey Flats" = data %>% filter(Forest == "Whiskey Flats"),
                             "Degrasse" = data %>% filter(Forest == "Degrasse"))
      )
      ggplot(study(), aes(Species)) +
        geom_histogram(stat = "count", position = "dodge") +
        theme_bw() +
        xlab("Species") +
        ylab("Number of Detections") +
        theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
      
  }
    else{
    choices<-c(input$species)
    data<-newData %>% filter(Species %in% choices)
    study<-reactive(switch(input$forest,
              "All Forests" = data,
             "South Hammond" = data %>% filter(Forest=="South Hammond"),
             "Beaver Creek" = data %>% filter(Forest=="Beaver Creek"),
             "Donnerville" = data %>% filter(Forest == "Donnerville"),
             "Whippoorwill Corners" = data %>% filter(Forest == "Whippoorwill Corners"),
             "Whiskey Flats" = data %>% filter(Forest == "Whiskey Flats"),
             "Degrasse" = data %>% filter(Forest == "Degrasse"))
    )
    ggplot(study(), aes(Species)) +
          geom_histogram(stat = "count", position = "dodge") +
          theme_bw() +
          xlab("Species") +
          ylab("Number of Detections") +
          theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
    }
    })
    
  #Species Activity Patterns
    output$activity<-renderPlot({
      
      data<-switch(input$species, 
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
      clock24.plot(data$NumObs, clock, show.grid = T, lwd = 2, line.col = "blue", cex.lab = 0.5)
      
    })



#Species Trophic Levels
output$trophic <- renderPlot({
  
  data<-switch(input$forest, 
               "All Forests" = mammals,
               "Whiskey Flats" = mammals %>% filter(ForestName=="WF"),
               "South Hammond" = mammals %>% filter(ForestName=="SH"),
               "Donnerville" = mammals %>% filter(ForestName == "DON"),
               "Beaver Creek" = mammals %>% filter(ForestName == "BC"),
               "Degrasse" = mammals %>% filter(ForestName == "DEG"),
               "Whippoorwill Corners" = mammals %>% filter(ForestName == "WHIP"),)
  
  ggplot(data, aes(Trophic)) + 
    geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') + 
    labs(title = "Trophic Levels per Forest", x="Trophic Level", y="Number of Detections") +
    theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5))
  
  
})
#Forest Composition
output$covariates <- renderPlot({
  
  data<-switch(input$forest, 
               "All Forests" = all_forests,
               "South Hammond" = dat_molten %>% filter(Forest=="SH"),
               "Beaver Creek" = dat_molten %>% filter(Forest=="BC"),
               "Donnerville" = dat_molten %>% filter(Forest=="DON"),
               "Degrasse" = dat_molten %>% filter(Forest=="DEG"),
               "Whippoorwill Corners" = dat_molten %>% filter(Forest=="WHIP"),
               "Whiskey Flats" = dat_molten %>% filter(Forest=="WF"),)
  
  
  ggplot(data, aes(x="", y=value, fill= variable)) + 
    geom_bar(stat="identity") + 
    coord_polar("y", start = 0) + 
    geom_text(aes(label = paste0(round(value), "%")), color = "white", size=2)+
    theme_void() +
    theme(axis.line = element_blank(),
          plot.title = element_text(hjust=0.5)) + 
    labs(x = NULL,
         y = NULL,
         title = "Forest Composition")+
    scale_fill_manual(values = c("Water" = "#165970",
                                 "Mixed" = "#543b1f",
                                 "Evergreen" = "#C6ABE1",
                                 "Deciduous" = "#39541e",
                                 "Development" = "#ABC4E0",
                                 "Agriculture" = "#E9A68F",
                                 "Barren" = "#b9da97",
                                 "Shrub" = "#d4b18a",
                                 "Wetland" = "#69c3e1",
                                 "Herbaceous" = "Gray"))
  
  
})
#Forest Diversity 
output$diversity <- renderPlot({
  
  data<-switch(input$forest, 
               "All Forests" = divFinal,
               "South Hammond" = divFinal %>% filter(Forest == "SH"),
               "Beaver Creek" = divFinal %>% filter(Forest == "BC"),
               "Donnerville" = divFinal %>% filter(Forest == "DON"),
               "Degrasse" = divFinal %>% filter(Forest == "DEG"),
               "Whippoorwill Corners" = divFinal %>% filter(Forest == "WHIP"),
               "Whiskey Flats" = divFinal %>% filter(Forest == "WF"),)
  
  ggplot(data, aes(x= Forest, y = Index, fill = Diversity_Index)) + 
    geom_bar(stat = "identity",position= position_dodge(), width = 0.7) +
    labs(title = "Diversity Indices per Forest", x= "Forest", y= "Diversity Index") + theme (plot.title =element_text(hjust = 0.5)) + 
    scale_fill_manual(values = c("Shannon Index" = "#165970",
                                 "Inverse Simpson Index" = "#543b1f",
                                 "Species Richness" = "#C6ABE1"))
  
})




}
shinyApp(ui = ui, server = server)