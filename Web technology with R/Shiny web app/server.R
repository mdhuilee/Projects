 
library(ggplot2)
library(ggmap)
library(dplyr)
library(shiny)

house_date <- readRDS(
          file = "houseData.rds")

#           file = "/Users/lei/Desktop/one/Practice/GIT_first/Projects/Web technology with R/Shiny web app/houseData.rds")

shinyServer( function(input, output) {
    
    output$plot <- renderPlot({
        if (input$numbeds != 5){
        plot_data <- filter(house_date, numbeds == as.numeric(input$numbeds))}
        else{
            plot_data <- house_date
        }
        theme_set(theme_bw(16))
        gw_in <- qmap("Greenwood IN", color = "bw", zoom = 12)

        gw_in +
            stat_bin2d(
                aes(x = lon, y = lat, fill = price_cat),
                size = .5, bins = 25, alpha = 1/2,
                data = plot_data
            ) + 
            scale_fill_manual(
                values = c("1" = "#c7e9c0","2" = "#74c476","3" = "#31a354",
                           "4" = "#006d2c"),  
                labels = c("<100,000", "100,000-200,000", "200,000-300,000", ">300,000")) +
            
            theme(    axis.line = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.text = element_text(size=16))
        
        
    })
    
    
    
}
)

 
