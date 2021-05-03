library("shiny")
library("tidyverse")
library("maps")
library("rlang")
library("wesanderson")

#test
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    country <- reactive({map.where(database = "world",
                                  x = input$map_click$x,
                                  y = input$map_click$y) %>%
                            str_extract("[^:]+")})
    
    output$country <- renderText(country)
    output$Heatmap <- renderPlot({
        
        pal <- wes_palette("Zissou1", 100, type = "continuous")
        
        country_plot <- ggplot(data=augmented_map_data, 
                               aes(x=long,
                                   y=lat,
                                   group=group,
                                   fill=!!sym(
                                       str_c(input$status,
                                             "_per_100k_citizen"))))+
            geom_polygon()+
            scale_fill_gradientn(colours = pal)+
            #scale_fill_gradient(low = "grey",
             #                   high = "red")+
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
            labs(fill = str_c(input$status, " per 100k citizen"))
        
        country_plot

    })
    
    output$timeseries_plot <- renderPlot({
        validate(
            need(input$map_click$x, "Click map for Timeseries data"))
        
    timeseries_plot <- 
        timeseries %>%
        filter(`Country/Region` == country()) %>%
            
            ggplot(mapping = aes(x = Date,
                                 y = !!sym(input$status)))+
                geom_point()+
                labs(x = ' ', 
                     y = input$status,
                     title = country())+ 
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            theme_minimal()+
            theme(axis.text.x = element_text(angle=45, hjust = 1))
            
        if(input$yLog){
            timeseries_plot <- timeseries_plot + 
                scale_y_continuous(trans="log2",
                                   labels=scales::comma,
                                   name= input$status)
        } else {
            timeseries_plot <- timeseries_plot + 
                scale_y_continuous(trans="identity",
                                   labels=scales::comma,
                                   name= input$status)
        }
        
    timeseries_plot
    })
     
})

