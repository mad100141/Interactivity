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

})
