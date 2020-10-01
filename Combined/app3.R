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
Activity <- read_csv("Data/CSVs/Activity.csv")


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
                                                    "Cottontail Rabbit",
                                                    "Coyote",
                                                    "Fisher",
                                                    "Flying Squirrel",
                                                    "Gray Squirrel",
                                                    "Opossum",
                                                    "Other Small Mammal",
                                                    "Porcupine",
                                                    "Raccoon",
                                                    "Red Fox",
                                                    "Red Squirrel",
                                                    "Weasel",
                                                    "White-tailed Deer"),
                                        selected = "Black Bear"),
                           
                            checkboxInput("rare", "Not Frequently Dectected Species", value = FALSE),
                        
                            
                            img(src = "NatureUpNorth.png", height = 100, width = 240)
               ),
               mainPanel(h5("Mammals are not all active at the same time! Select a mammal using the tab in the 
               panel to the left and learn what times of day that mammal was detected by our cameras.
               The graph is shaped like a clock with 0-23 symbolizing the hour we detected the mammal.
               The x-axis lets you know how many detections we had during that hour."),
                         plotOutput(outputId = "activity"), uiOutput(outputId = "image"))   
             )
    )
    
  )
)





server <- function(input, output){
  output$activity<-renderPlot({
    
    if(input$rare){
       Rare<-Activity %>% filter(bin == "MINK" | bin == "RIVEROTTER" |bin == "SNOWSHOEHARE" |bin == "SKUNKSTRIPED")
       
       ggplot(Rare, aes(x= truncHour, y = NumObs, fill = Species)) + 
         geom_bar(stat = "identity",position= position_dodge(), width = 0.7) +
         labs(title = "Number of Detections per Hour", x= "Time of Detection (hour)", y= "Number of Detections") +
         theme (plot.title =element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) + 
         scale_fill_manual(values = c("Mink" = "#165970",
                                      "River Otter" = "#543b1f",
                                      "Striped Skunk" = "#C6ABE1",
                                      "Snowshoe Hare" = "#39541e")) 
       
       
    }
    
    #Individual graphs for activity
    # if(input$mammals == "Mink"| input$mammals== "River Otter" | input$mammals == "Snowshoe Hare"| input$mammals == "Striped Skunk"){
    #   data<-switch(input$mammals,
    #                "Snowshoe Hare" = Activity %>% filter(bin == "SNOWSHOEHARE"),
    #                "River Otter" = Activity %>% filter(bin == "RIVEROTTER"),
    #                "Mink" = Activity %>% filter(bin == "MINK"),
    #                "Striped Skunk" = Activity %>% filter(bin == "SKUNKSTRIPED")
    #                )
    #   ggplot(data, aes(truncHour))+
    #     geom_histogram(stat = "count", position = "dodge", fill = '#165970', colour = '#543b1f') +
    #     theme_bw() +
    #     xlab("Time of Detection (hour)") +
    #     ylab("Number of Detections") +
    #     theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5)) + 
    #     labs(title = "Number of Detections per Hour") +
    #     theme(plot.title = element_text(hjust=0.5))
    # }
    else{
      data<-switch(input$mammals, 
                   "White-tailed Deer" = Activity %>% filter(bin == "DEERWHITETAILED"),
                   "Chipmunk" = Activity %>% filter(bin == "CHIPMUNK"),
                   "Cottontail Rabbit" = Activity %>% filter(bin == "COTTONTAILRABBIT"),
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
                   "Flying Squirrel" = Activity %>% filter(bin== "SQUIRRELFLYING"),
                   "Other Small Mammal" = Activity %>% filter(bin == "OTHERSMALLMAMMAL"),
                   "Opossum" = Activity %>% filter(bin == "OPOSSUM"))
      
      clock<-c(0:23)
      clock24.plot(data$NumObs, clock, show.grid = T, lwd = 2, line.col = "blue", cex.lab = 0.5)
      
    }
    
  })

    output$image <- renderUI({    
  
      if(input$rare){
        img(src = "Rare.png", height = 220, width = 600, align = "left")
      }

      else if(input$mammals == "White-tailed Deer"){
        img(src = "Deer.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Chipmunk"){
        img(src = "Chipmunk.png", height = 240, width = 300, align = "left")
      }
      else if (input$mammals == "Cottontail Rabbit"){
        img(src = "Cottontail.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Coyote"){
        img(src = "Coyote.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Fisher"){
        img(src = "Fisher.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Raccoon"){
        img(src = "Raccoon.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Red Squirrel"){
        img(src = "RedSquirrel.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Other Small Mammal"){
        img(src = "Mole.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Gray Squirrel"){
        img(src = "GraySquirrel.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Black Bear"){
        img(src = "blackbear.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Red Fox"){
        img(src = "RedFox.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Porcupine"){
        img(src = "Porcupine.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Bobcat"){
        img(src = "Bobcat.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Opossum"){
        img(src = "Opossum.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Weasel"){
        img(src = "Weasel.png", height = 240, width = 300, align = "left")
      }
      else if(input$mammals == "Flying Squirrel"){
        img(src = "FlyingSquirrel.png", height = 240, width = 300, align = "left")
      }
     
     
     })
}
shinyApp(ui = ui, server = server)
