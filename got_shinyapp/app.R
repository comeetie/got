#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(sf)
library(ggplot2)

load("./got_data.RData")

colforest="#c0d7c2"
colriver="#7ec9dc"
colland="ivory"
borderland = "ivory3"  


character_name_list = appearences %>% pull(name) %>% unique()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Got Geography"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("charactername", "Character name:",character_name_list)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mapPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapPlot <- renderPlot({
    
      character_duration= appearences %>% filter(name==input$charactername) %>% 
        left_join(scenes) %>%
        group_by(location) %>% 
        summarize(duration= sum(duration/60)) %>% 
        left_join(scenes_loc) %>% 
        st_as_sf()
      
      ggplot() + geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
        geom_sf(data=islands,fill=colland,col="ivory3") +
        geom_sf(data=character_duration,aes(size=duration), color="purple")+
        scale_size_area("Time on screen",breaks = c(0,30,60,120,240))+
        theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
        theme(panel.background = element_rect(fill = colriver,color=NA)) +
        labs(title = paste(input$charactername," time on screen per location"),caption = "Etiennne Côme, 2020",x="",y="")
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
