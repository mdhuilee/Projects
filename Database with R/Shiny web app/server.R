library(shiny)
library(pitchRx)
library(dplyr)
library(ggplot2)
 
shinyServer( function(input, output) {
    data <- reactive({
    files <- c("inning/inning_all.xml", "players.xml")
    dat2 <- scrape(start = as.character(input$pickdate), end = as.character(input$pickdate), suffix = files)

    pitch_dt <- dat2[["pitch"]]
    umpire_dt <- dat2[["umpire"]]
        
    pitch_umpire <- 
        umpire_dt %>%
        filter(position == "home") %>%
        inner_join(pitch_dt, by ="gameday_link") %>%
        distinct()
    
    #outside_zone
    out_zn <- pitch_umpire %>%
        filter((px < -.85 | px > .85 | pz < sz_bot | pz > sz_top), 
               des %in% c("Called Strike", "Ball")) %>%
        mutate(cs_ind = as.numeric(des == "Called Strike")) %>%
        select(position, name, id.x, des, cs_ind, px, pz, sz_top, sz_bot, num, count, gameday_link) %>%
        collect()
    
    #proportion of pitches that are "called strikes" among all pitches outside the strikezone.
    
    cs_out_zn_all <-
        out_zn %>%
        summarize(sum = sum(cs_ind), n = n()) %>%
        collect() %>%
        mutate(cs_out_zn = round(100 * sum / n, 1))
    
    names(cs_out_zn_all) <- c("# Called Strikes","# Pitches","Called strikes %")
    
    cs_out_zn_count <-
        out_zn %>%
        group_by(count) %>%
        summarize(sum = sum(cs_ind), n = n()) %>%
        collect() %>%
        mutate(cs_out_zn = round(100 * sum / n, 1))
    
    figure1 <- ggplot(data=cs_out_zn_count,aes(x=count,y=cs_out_zn, fill=count)) + geom_bar(stat = "identity") + ylab("Probalilities by count") + theme(legend.position="none")
    
    names(cs_out_zn_count) <- c("Count","# Called Strikes","# Pitches","Called strikes %")
    })
    
    output$table_all <- renderTable({
        data()
        as.matrix(names(dat2))
        #head(pitch_dt)
        #head(out_zn)
        #cs_out_zn_all
    }, include.rownames=FALSE)
    
    output$table_count <- renderTable({
        data()
        cs_out_zn_count
    }, include.rownames=FALSE)
 
    output$plot <- renderPlot({
        data()
        figure1
    })
    
    
}
)