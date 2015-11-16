library(shiny)

shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel("House price in Greenwood IN"),
        sidebarPanel(

            
            radioButtons('numbeds',"Select Number of Bedrooms", c("2" = 2, "3" = 3,
                                                                  "4" = 4, "All" = 5))
        ),
        mainPanel(
            h3('Plot the house price by number of beds'),

            plotOutput("plot")
        ) )
)