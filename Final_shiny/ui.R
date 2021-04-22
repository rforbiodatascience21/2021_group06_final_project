library("shiny")
library("shinythemes")


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Exploring Covid Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("Year",
                        "Current Year",
                        min = 2020,
                        max = 2021,
                        value = 2020,
                        sep="",
                        animate=animationOptions(interval=2000))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("linePlot"), # a time series plot??
            plotOutput("Heatmap")   # global map
        )
    )
))
