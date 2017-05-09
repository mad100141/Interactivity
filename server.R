library(shiny)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(tidytext)
library(wordcloud)
library(readr)
library(plotly)


data(stop_words)
word_temp <- read_csv("terror_wordFinal.csv")
map_data_julien <- read_csv("terror_map_julien.csv")
terror_scatter <- read_csv("terror_scatter.csv")
terror_bar <- read_csv("terror_bar.csv")



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

    output$scatter <- renderPlotly({
      terror_scatter$gname <- str_replace_all(terror_scatter$gname, "[\r\n]" , "")
      scatterG <- ggplot(terror_scatter, aes(x = date, y = nkill)) +
        geom_point(aes(text = paste(terror_scatter$country_txt,
                               'Number of Kills:', terror_scatter$nkill,
                               "\n Terrorist Organization:",
                               gname))) + theme_bw() +
        labs(x = "Date", y = "Number of Deaths")
      ggplotly(scatterG)

      # scatter1 <- plot_ly(terror_scatter,type = "scatter",
      #              x = terror_scatter$date, y = terror_scatter$nkill,
      #              marker = list(color="#264E86"),
      #              hoverinfo = "text",
      #              text = ~paste(terror_scatter$country_txt,
      #                            'Number of Kills:', terror_scatter$nkill,
      #                            "\n Terrorist Organization:",
      #                            gname))
      # #Add Number of Kills, Country, Yr
      # scatter1
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

      terror_data.recent <- terror %>% filter(iyear > 1997)
      terror_data.recent.usa <- terror_data.recent %>% filter(country == 217 & gname != "Unknown")
      usa_links <- terror_data.recent.usa %>% group_by(gname, targtype1_txt) %>% summarise(weight =
                                                                                             sum(success)) %>% arrange(gname) %>% rename(from = gname, to = targtype1_txt)

      # usa_links
      usa_nodes <-
        rbind(data.frame(
          name = unique(terror_data.recent.usa$gname),
          type = 1
        ),
        data.frame(
          name =
            unique(terror_data.recent.usa$targtype1_txt),
          type = 2
        ))


      #zero_index links
      usa_links$from <- match(usa_links$from, usa_nodes$name) - 1
      usa_links$to <- match(usa_links$to, usa_nodes$name) - 1

      output$sankey <- renderSankeyNetwork({
        sankeyNetwork(Links = usa_links, Nodes = usa_nodes, Source = "from", Target = "to",
                      Value = "weight", NodeID = "name",  fontSize = 12, nodeWidth = 30)

      })



})