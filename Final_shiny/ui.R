library("shiny")
library("shinythemes")


#test
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # App title ----
    titlePanel("Hello Shiny!"),
    sidebarLayout(
        
        sidebarPanel(
            selectInput(inputId = "fill_selection",
                        label = "Choose status",
                        choices = c("Cases" = "Cases", 
                                    "Deaths" = "Deaths",
                                    "Recovered" = "Recovered"))
        ),
    
    
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: global map ----
            plotOutput(outputId = "Heatmap", click = "map_click"),
            textOutput(outputId = "closest_match")   
            
        ),
        position = c("left", "right")
)
))

