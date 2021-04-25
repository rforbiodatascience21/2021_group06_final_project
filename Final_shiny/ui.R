library("shiny")
library("shinythemes")

#test
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # App title ----
    titlePanel("Hello Shiny!"),
    
    
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: global map ----
            plotOutput(outputId = "Heatmap", click = "map_click")
            
        ),
        mainPanel(
            textOutput(outputId = "closest_match")   
        )
    )
)
    

