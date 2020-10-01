#Shiny app showing activity patterns among mammals

rm(list=ls())
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(chron) #works with chronological objects like dates and times
library(reshape) #restructures and aggregates data in a flexible way
library(vegan) #ecological analysis
library(plotrix) #visualize circular data
library(ggplot2) #producing graphics

#Import data frame from Activity.Rmd
Activity <- read_csv("~/Documents/Shiny/Activity/Data/Activity.csv")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Mammal Activity Patterns"),
  
  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
    
    # Sidebar panel for inputs ----
    # sidebarPanel(
    #   
    #   # Input: Slider for the number of bins ----
    #   selectInput("species", h3("Choose your Species"),
    #               choices = c("White-tailed Deer", "Chipmunk", "Coyote", "Fisher", "Raccoon", "Red Squirrel", "Other Small Mammal", "Gray Squirrel", "Black Bear", "Red Fox", "Porcupine", "Bobcat", "Opossum", "Weasel", "Striped Skunk", "Flying Squirrel", "Snowshoe Hare", "River Otter", "Mink"), selected = "White-tailed Deer"),
    #   
    # ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("activity"),
      selectInput("species", h3("Choose your Species"),
                  choices = c("White-tailed Deer", "Chipmunk", "Coyote", "Fisher", "Raccoon", "Red Squirrel", "Other Small Mammal", "Gray Squirrel", "Black Bear", "Red Fox", "Porcupine", "Bobcat", "Opossum", "Weasel", "Striped Skunk", "Flying Squirrel", "Snowshoe Hare", "River Otter", "Mink"), selected = "White-tailed Deer"),
      
      
    )
  )
#)


#define server logic ----
server<-function(input, output) {
  
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
  
}
# Run the app ----
shinyApp(ui = ui, server = server)