library("shiny")
library("tidyverse")
library("maps")
library(rlang)
#test
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$Heatmap <- renderPlot({
        
        country_plot <- ggplot(data=augmented_map_data, 
                               aes(x=long,
                                   y=lat,
                                   group=group,
                                   fill=!!sym(
                                       str_c(input$fill_selection,
                                             "_per_100k_citizen"))))+
            geom_polygon()+
            scale_fill_gradient(low = "grey",
                                high = "red")+
            coord_map(xlim=c(-180,180),ylim=c(-55,90))+ 
            theme_classic()+
            theme(axis.ticks.x = element_blank(), 
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_blank(), 
                  axis.text.y = element_blank(),
                  axis.title.y= element_blank(),
                  axis.title.x= element_blank(),
                  axis.line.x = element_blank(),
                  axis.line.y = element_blank())+
            labs(fill = str_c(input$fill_selection, " per 100k citizen"))
        
        country_plot

    })
    output$closest_match <- renderText(map.where(database = "world",
                                                 x = input$map_click$x,
                                                 y = input$map_click$y))
})

