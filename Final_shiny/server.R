library("shiny")
library("tidyverse")
#test
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$Heatmap <- renderPlot({
        
        country_plot <- ggplot()+
            geom_polygon(data=augmented_map_data, 
                         aes(x=long,y=lat,group=group, fill=Age_median), 
                         color="grey50")+
            coord_map(xlim=c(-180,180),ylim=c(-55,90))+ 
            theme_classic()+
            theme(axis.ticks.x = element_blank(), 
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_blank(), 
                  axis.text.y = element_blank(),
                  axis.title.y= element_blank(),
                  axis.title.x= element_blank(),
                  axis.line.x = element_blank(),
                  axis.line.y = element_blank())
        
        country_plot

    })

})
