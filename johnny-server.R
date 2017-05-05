library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  output$main_plot <- renderPlot({

    p <- ggplot(faithful, aes(x = eruptions)) +
      geom_histogram(aes(y = ..density..), bins = as.numeric(input$n_breaks),
                     fill = "black", color = "white") +
      scale_x_continuous(breaks = seq(1.5,5, by=0.5)) +
      labs(x = "Duration (minutes)", y = "Density",
           title = "Geyser eruption duration") + theme_linedraw()

    if (input$individual_obs) {p <- p + geom_rug(aes(faithful$eruptions))}

    if (input$density) {p <- p + geom_density(adjust = input$bw_adjust,color = "blue")}

    p
  })

  output$scatter_plot <- renderPlot({

  p1 <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
    geom_point(size = input$size_adjust) +
  labs(
    title = "Geyser eruption duration vs Waiting time",
    x = "Duration (minutes)",
    y = "Waiting (minutes)"
  ) + theme_linedraw()
  if (input$scatter_trend) {p1 <- p1 + geom_smooth(se = F)}
  p1
  })

  output$contour_plot <- renderPlot({

  p2 <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_density2d(aes(fill = ..density..), geom = "tile", contour = F,
                 h = c(input$sliderX,input$sliderY)) +
    scale_fill_gradient2(low = "white", mid = "orange", high = "red", midpoint= .1) +
  labs(title = "Geyser eruption duration vs Waiting time",
    x = "Duration (minutes)",y = "Waiting (minutes)") + theme_linedraw()
  p2
  })
  
  library(tidytext)
  library(wordcloud)
  library(tidyverse)
  data(stop_words)
  terror <- read_csv("globalterrorismdb_0616dist.csv")
  word_temp <- dplyr::select(terror, eventid, iyear, summary) %>%
    unnest_tokens(word, summary) %>%
    anti_join(stop_words) %>%
    count(word, iyear)
  output$word_cloud <- renderPlot({
    words <- word_temp %>%
      filter(iyear == input$year) %>%
      with(wordcloud(word, n, max.words = 50))
  })
  
  #library(ggmap)
  #terror_us <- terror %>% filter(country_txt == "United States")
  #output$us_map <- renderPlot({
    #map_df <- map_data(map = "world")
    #ggplot() + geom_polygon(data = map_df, aes(x = long, y = lat, group = group)) + 
    #  geom_point(data = terror_dat, aes(x = longitude, y = latitude, color = country_txt), show.legend = F)
    #state_borders <- map_data("state") 
    #damap <- ggplot() + geom_polygon(data = state_borders, aes(x = long, y = lat, group = group)) 
    
    #if (input$euro_checks == terror$nkill) {
    #  damap <- damap + geom_point(data = terror_us, aes(x = longitude, y = latitude, color = nkill), show.legend = F)
    #} else if (input$euro_checks == terror$success) {
    #  damap <- damap + geom_point(data = terror_us, aes(x = longitude, y = latitude, color = success), show.legend = F)
    #} else {
    #  damap <- damap + geom_point(data = terror_us, aes(x = longitude, y = latitude, color = suicide), show.legend = F)
    #}
    #damap
  #})

})
