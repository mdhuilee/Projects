library(shiny)

shinyUI(
    pageWithSidebar(
 
        headerPanel("Baseball data analysis using PITCHf/x"),
        sidebarPanel(
            dateInput("pickdate", "Choose a game date", format = "yyyy-mm-dd", value = "2013-06-01"),
            p(strong("Take less than a minute to load"))
        ),
        mainPanel(
            
            h5("Figure 1. Proportion of pitches that are called strikes among all pitches outside the strikezone by count"),
            plotOutput("plot"),
            h5("Table 1. Proportion of pitches that are called strikes among all pitches outside the strikezone"),
            tableOutput("table_all"),
            h5("Table 2. Proportion of pitches that are called strikes among all pitches outside the strikezone by count"),
            tableOutput("table_count")
            
            
        ) )
)