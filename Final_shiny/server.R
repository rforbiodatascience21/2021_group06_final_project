library("shiny")
library("tidyverse")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$Heatmap <- renderPlot({
        
        full_data <- filter(full_data,year==input$year)
        
        country_plot <- ggplot()+
            geom_polygon(data=full_data, 
                                    aes(x=long,y=lat,group=group, fill=confirmed), 
                                    color="grey50")+
            coord_map(xlim=c(-180,180),ylim=c(-90,90))
        
        country_plot + scale_fill_distiller(name="cases/pop",palette="Oranges",direction=1)+
            labs(x="",y="",title = "\example: number of confirmed cumulative cases")+
            theme_classic()+
            theme(axis.ticks.x = element_blank(), 
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_blank(), 
                  axis.text.y = element_blank(),
                  axis.title.y= element_blank(),
                  axis.title.x= element_blank(),
                  axis.line.x = element_blank(),
                  axis.line.y = element_blank())
        

    })

})
