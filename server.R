library(shiny)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(tidytext)
library(wordcloud)
library(readr)
library(plotly)


data(stop_words)
terror <- read.csv("globalterrorismdb_0616dist.csv")
word_tempI <- dplyr::select(terror, eventid, iyear, summary)
word_tempI$summary <- as.character(word_tempI$summary)

word_temp <-  word_tempI %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words) %>%
  count(word, iyear)

terror_scatter <- terror %>% select(iyear, imonth, iday, nkill, gname, country_txt)

terror_scatter$date <-
  as.Date(with(terror_scatter, paste(iyear, imonth, iday, sep="-")), "%Y-%m-%d")

shinyServer(function(input, output) {

  output$julien1 <- renderPlot({

     map_df  <- map_data(map = "world")
     new_dat <- subset(terror, iyear == input$year1)

     midA <- range(new_dat$nkill, na.rm = TRUE)
     mid <- (midA[2] - midA[1])/2

     map1 <- ggplot() +
       geom_polygon(data = map_df, aes(x = long, y = lat, group = group)) +
       geom_point(data = new_dat, aes(x = longitude, y = latitude,
                                      color = nkill, size = nkill)) +
       scale_color_gradient2(low = "blue", high = "red", mid = "white",
                             name='Number of Deaths',
                            midpoint = mid)
     map1
  })

    output$word_cloud <- renderPlot({
      words <- word_temp %>%
        filter(iyear == input$year) %>%
        with(wordcloud(word, n, max.words = 50))
      words
    })

    output$scatter <- renderPlot({
      scatter1 <- plot_ly(terror_scatter,type = "scatter",
                   x = terror_scatter$date, y = terror_scatter$nkill,
                   marker = list(color="#264E86"), mode = "none",
                   hoverinfo = "text",
                   text = ~paste(terror_scatter$country_txt,
                                 'Number of Kills:', terror_scatter$nkill))
      #Add Number of Kills, Country, Yr
      scatter1
    })

})

  # output$julien2 <- renderPlot({
  #   plot(1:5,1:5)
  # })

#   output$alex1 <- renderPlot({
#     plot(1:5,1:5)
#   })
#
#   output$alex2 <- renderPlot({
#     plot(1:5,1:5)
#   })
#
#   output$johnny1 <- renderPlot({
#     plot(1:5,1:5)
#   })
#
#   output$jonny2 <- renderPlot({
#     plot(1:5,1:5)
#   })
#
#   output$maria1 <- renderPlot({
#     plot(1:5,1:5)
#   })
#
#   output$maria1 <- renderPlot({
#     plot(1:5,1:5)
#   })
#
#   output$main_plot <- renderPlot({
#
#     p <- ggplot(faithful, aes(x = eruptions)) +
#       geom_histogram(aes(y = ..density..), bins = as.numeric(input$n_breaks),
#                      fill = "black", color = "white") +
#       scale_x_continuous(breaks = seq(1.5,5, by=0.5)) +
#       labs(x = "Duration (minutes)", y = "Density",
#            title = "Geyser eruption duration") + theme_linedraw()
#
#     if (input$individual_obs) {p <- p + geom_rug(aes(faithful$eruptions))}
#
#     if (input$density) {p <- p + geom_density(adjust = input$bw_adjust,color = "blue")}
#
#     p
#   })
#
#   output$scatter_plot <- renderPlot({
#
#     p1 <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
#       geom_point(size = input$size_adjust) +
#       labs(
#         title = "Geyser eruption duration vs Waiting time",
#         x = "Duration (minutes)",
#         y = "Waiting (minutes)"
#       ) + theme_linedraw()
#     if (input$scatter_trend) {p1 <- p1 + geom_smooth(se = F)}
#     p1
#   })
#
#   output$contour_plot <- renderPlot({
#
#     p2 <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
#       stat_density2d(aes(fill = ..density..), geom = "tile", contour = F,
#                      h = c(input$sliderX,input$sliderY)) +
#       scale_fill_gradient2(low = "white", mid = "orange", high = "red", midpoint= .1) +
#       labs(title = "Geyser eruption duration vs Waiting time",
#            x = "Duration (minutes)",y = "Waiting (minutes)") + theme_linedraw()
#     p2
#   })
#
# })
