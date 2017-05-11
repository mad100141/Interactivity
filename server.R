library(shiny)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(tidytext)
library(wordcloud)
library(readr)
library(plotly)


data(stop_words)
terror <- read_csv("../globalterrorismdb_0616dist.csv")
word_temp <- read_csv("terror_wordFinal.csv")
map_data_julien <- read_csv("terror_map_julien.csv")
terror_scatter <- read_csv("terror_scatter.csv")
terror_bar <- read_csv("terror_bar.csv")
usa_links <- read_csv("alex_usa_links.csv")
usa_nodes <- read_csv("alex_usa_nodes.csv")

# terror_data.recent <- terror %>% filter(iyear > 1997)
# terror_data.recent.usa <- terror_data.recent %>% dplyr::filter(country == 217 & gname != "Unknown")
# usa_links <- terror_data.recent.usa %>%
#   dplyr::group_by(gname, targtype1_txt) %>%
#   dplyr::summarise(weight = sum(success)) %>%
#   dplyr::arrange(gname) %>% dplyr::rename(from = gname, to = targtype1_txt)
#
# # usa_links
# usa_nodes <-
#   rbind(data.frame(
#     name = unique(terror_data.recent.usa$gname),
#     type = 1
#   ),
#   data.frame(
#     name =
#       unique(terror_data.recent.usa$targtype1_txt),
#     type = 2
#   ))
#
#
# #zero_index links
# usa_links$from <- match(usa_links$from, usa_nodes$name) - 1
# usa_links$to <- match(usa_links$to, usa_nodes$name) - 1


shinyServer(function(input, output) {

  output$julien1 <- renderPlot({

    map_df  <- map_data(map = "world")
    new_dat <- subset(map_data_julien, iyear == input$year1)

    map1 <- ggplot() + geom_polygon(data = map_df, aes(x = long, y = lat, group = group)) +
      geom_density2d(data = new_dat, (aes(x = longitude, y = latitude)), size = 0.1) +
      stat_density2d(data = new_dat, aes(x = longitude, y = latitude,
                                         fill = ..level.., size = 0.01),
                     alpha = 0.5, geom="polygon", bins = 5) +
      geom_point(data = new_dat, aes(x = longitude, y = latitude,
                                     colour = nkill, size = nkill)) +
      scale_color_gradient(low = "blue", high = "red", name = 'Number of Deaths') +
      scale_fill_gradient(low = "blue", high = "red", name = 'Density of Attacks')

    map1
  })

  output$word_cloud <- renderPlot({
    words <- word_temp %>%
      filter(iyear == input$year) %>%
      with(wordcloud(word, n, max.words = 50))
    words
  })
  
  output$us_map <- renderPlot({
    
    terror_us <- terror_dat %>% filter(country_txt == "United States")
    terror_us <- terror_us %>% filter(latitude > 25 & latitude < 50)
    terror_us <- terror_us %>% filter(longitude > -150 & longitude < -50)
    terror_us$suicide[terror_us$suicide == 1] <- "Yes"
    terror_us$suicide[terror_us$suicide == 0] <- "No"
    terror_us$success[terror_us$success == 1] <- "Yes"
    terror_us$success[terror_us$success == 0] <- "No"
    
    state_borders <- map_data("state") 
    
    if (input$person_map == "Deaths") {
      
      mapp <- ggplot() +
        geom_polygon(data = state_borders, aes(x = long, y = lat, group = group), 
                     color = "black")
      mapp <- mapp + geom_point(data = terror_us, aes(x = longitude, 
                                                      y = latitude, 
                                                      fill = nkill, size = nkill), 
                                color = "white", shape = 21, stroke=0.2) +
        scale_fill_gradient2(low = "purple", high = "deeppink3", 
                             mid = "orange", midpoint = 1) +
        labs(
          title = "US Map of Terrorist attacks, by kills",
          x = "Longitude",
          y = "Latitude",
          fill = "Number of Casualties",
          size = "Number of Casualties"
        ) + theme_void() + 
        scale_size(range = c(2, 10))
      mapp
    }
    
    else if (input$person_map == "Successful Attacks and Deaths") {#Success
      mapp <- ggplot() +
        geom_polygon(data = state_borders, aes(x = long, y = lat, group = group), 
                     color = "black")
      mapp <- mapp + geom_point(data = terror_us, aes(x = longitude, 
                                                      y = latitude, 
                                                      fill = success, size = nkill), 
                                color = "white", shape = 21, stroke=0.2) +
        scale_fill_manual(values=c("purple", "orange")) +
        labs(
          title = "US Map of Terrorist attacks, by kills",
          x = "Longitude",
          y = "Latitude",
          fill = "Success",
          size = "Number of Casualties"
        ) + theme_void() + 
        scale_size(range = c(2, 10))
      mapp
    }
    else {#Suicide
      mapp <- ggplot() +
        geom_polygon(data = state_borders, aes(x = long, y = lat, group = group), 
                     color = "black")
      mapp <- mapp + geom_point(data = terror_us, aes(x = longitude, 
                                                      y = latitude, 
                                                      fill = suicide, size = nkill), 
                                color = "white", shape = 21, stroke=0.2) +
        scale_fill_manual(values=c("purple", "orange")) +
        labs(
          title = "US Map of Terrorist attacks, by kills",
          x = "Longitude",
          y = "Latitude",
          fill = "Suicide",
          size = "Number of Casualties"
        ) + theme_void() + 
        scale_size(range = c(2, 10))
      mapp
    }
  })

  output$scatter <- renderPlotly({
    terror_scatter$gname <- str_replace_all(terror_scatter$gname, "[\r\n]" , "")
    scatterG <- ggplot(terror_scatter, aes(x = date, y = nkill)) +
      geom_point(aes(text = paste(terror_scatter$country_txt,
                                  'Number of Kills:', terror_scatter$nkill,
                                  "\n Terrorist Organization:",
                                  gname))) + theme_bw() +
      labs(x = "Date", y = "Number of Deaths")
    ggplotly(scatterG)

  })

  output$barChartWeapon <- renderDataTable({
    tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99",
                       "#117733", "#999933", "#DDCC77", "#661100",
                       "#CC6677", "#AA4466", "#882255", "#AA4499")

    f <- list(
      family = "Palatino",
      size = 12,
      color = "red"
    )
    x <- list(
      title = "Attack Type",
      titlefont = f,
      tickangle = 25
    )
    y <- list(
      title = "Number of Fatalities",
      titlefont = f
    )

    barG <- ggplot(terror_bar, aes(x = attacktype1_txt)) +
      geom_bar(stat = "count",aes(fill = weaptype1_txt,
                                  text = paste("Attack Type:",attacktype1_txt,
                                               "\n Weapon Type:", weaptype1_txt))) +
      labs(x = "Attack Type", y = "Number of Attacks",
           title = "Attacks Used by Terrorists") +
      theme_linedraw() + theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
      theme(legend.position="none") + scale_fill_manual(values = tol12qualitative)

    b <- ggplotly(barG, tooltip = c("text"))
    b
  })
  output$barChartSuccess <- renderDataTable({#Does Attack Success Differ By Attack Type?
    #NonInteractive
    barS <- ggplot(terror_bar) +
      geom_bar(position = "dodge", aes(x = attacktype1_txt,
                                       fill = success)) +
      labs(x = "Attack Type", y = "Number of Attacks",
           title = "Attack Success by Methods Used", fill = "Success") +
      theme_linedraw() + theme(axis.text.x = element_text(angle = 20, hjust = 1))
    barS
  })
  output$barChartRegion <- renderDataTable({#Region
    barRegion <- ggplot(terror_bar) +
      geom_bar(aes(fill = attacktype1_txt,x = region_txt)) +
      labs(x = "Region", y = "Number of Attacks",
           title = "Attack Methods by Region", fill = "Method of Terror") +
      theme_linedraw() + theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
      scale_fill_manual(values = tol12qualitative)

    barRegion
  })

  
  usa_links <- as.data.frame(usa_links)
  output$sankey <- renderSankeyNetwork({
    sankeyNetwork(Links = usa_links, Nodes = usa_nodes, Source = "from", Target = "to",
                  Value = "weight", NodeID = "name",  fontSize = 12, nodeWidth = 30)

  })
  
  
  output$groupTimeSeries <- renderggplot(time_s_data_2, aes(x = iyear+imonth/12, y = kills, color = gname)) + 
    geom_line() + theme(legend.position="bottom") + labs(title = "Rising Terror ",
                                                         color = "Terrorist Group", y = "Kills", x = 'Date')
  ggplotly(p)
  



})