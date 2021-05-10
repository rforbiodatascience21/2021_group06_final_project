
# Load Libraries ----------------------------------------------------------

library("shiny")
library("tidyverse")
library("maps")
library("mapproj")
library("rlang")
library("wesanderson")


# Server Code -------------------------------------------------------------

# colorpalette for plotting
pal <- wes_palette("Zissou1", 100, type = "continuous")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    country <- reactive({map.where(database = "world",
                                  x = input$map_click$x,
                                  y = input$map_click$y) %>%
                        str_extract("^[^:]+")})
    
    output$country <- renderText(country)
    output$Heatmap <- renderPlot({
        
        augmented_map_data %>%
        ggplot(aes(x = long,
                   y = lat,
                   group = group,
                   fill = !!sym(str_c(input$status, "_per_100k_citizen"))))+
            geom_polygon()+
            scale_fill_gradientn(colours = pal)+
            coord_map(xlim=c(-180, 180), 
                      ylim=c(-55, 90))+ 
            theme_classic()+
            theme(axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  axis.title= element_blank(),
                  axis.line = element_blank())+
            labs(fill = str_c(input$status, " per 100k citizen"))
    })
    
    output$timeseries_plot <- renderPlot({
        validate(need(expr = country() != c("NA", ""),
                      message = "Please click valid country"))
        
    timeseries_plot <- 
        timeseries %>%
        filter(`Country/Region` == country()) %>%
            
            ggplot(mapping = aes(x = Date,
                                 y = !!sym(input$status),
                                 color = "red"))+
                geom_point()+
                labs(y = input$status,
                     title = str_c("Timeseries of ",
                                   input$status,
                                   " in ",
                                   country()))+ 
            scale_x_date(date_breaks = "1 month", 
                         date_labels =  "%b %Y") +
            theme_minimal()+
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none")
            
        if(input$yLog){
            timeseries_plot <- timeseries_plot + 
                scale_y_continuous(trans = "log2",
                                   labels = scales::comma,
                                   name = input$status)
        } else {
            timeseries_plot <- timeseries_plot + 
                scale_y_continuous(trans = "identity",
                                   labels = scales::comma,
                                   name = input$status)
        }
        
    timeseries_plot
    })
     
})

