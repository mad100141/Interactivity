library(shiny)
library(ggplot2)
library(tidyverse)
library(shinydashboard)
terror <- read_csv("globalterrorismdb_0616dist.csv")
shinyUI(bootstrapPage(

  dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Part (a)", tabName = "part_a", icon = icon("dashboard")),
        menuItem("Part (b)", tabName = "part_b", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      tabItems(
          tabItem(tabName = "part_a",
            selectInput(inputId = "n_breaks",
                        label = "Number of bins in histogram (approximate):",
                        choices = c(10, 20, 35, 50),
                        selected = 20),

            checkboxInput(inputId = "individual_obs",
                          label = strong("Show individual observations"),
                          value = FALSE),

            checkboxInput(inputId = "density",
                          label = strong("Show density estimate"),
                          value = FALSE),

            plotOutput(outputId = "main_plot", height = "300px"),

            # Display this only if the density is shown
            conditionalPanel(condition = "input.density == true",
                             sliderInput(inputId = "bw_adjust",
                                         label = "Bandwidth adjustment:",
                                         min = 0.2, max = 2, value = 1, step = 0.2)
            )
          ),

          tabItem(tabName = "part_b",
            headerPanel("Tabsets"),
                  checkboxInput(inputId = "scatter_trend",
                                label = strong("Show Trend Line"),
                                value = FALSE),

                  conditionalPanel(condition = "input.scatter_trend == true",
                                   sliderInput(inputId = "size_adjust",
                                               label = "Point Size Adjustment:",
                                               min = 0.5, max = 4, value = 1, step = .5)),

                  checkboxInput(inputId = "bandwidth_adj",
                          label = strong("Bandwidth Adjustments"),
                          value = FALSE),
                  sliderInput(inputId = "year", label = "Year", 
                              min = 1970, max = 2015, value = 1970),
                  #selectInput(inputId = "euro_checks", label = "Variable",
                   #                  choices = list("# kills" = terror$nkill, "success" = terror$success,
                    #                                "suicide" = terror$suicide), selected = terror$nkill),

            conditionalPanel(condition = "input.bandwidth_adj == true",
                  box(title = "X Bandwidth Adjustment",
                      sliderInput("sliderX", "Number of observations:", .2, 4, .5)),
                  box(title = "Y Bandwidth Adjustment",
                      sliderInput("sliderY", "Number of observations:", .2, 4, .5))),

                  mainPanel(
                    tabsetPanel(
                      tabPanel("Scatter Plot", plotOutput(outputId = "scatter_plot", height = "300px")),
                      tabPanel("Contour Plot", plotOutput(outputId = "contour_plot", height = "300px")),
                      tabPanel("Word Cloud", plotOutput(outputId = "word_cloud", height = "300px"))
                      #tabPanel("USA", plotOutput(outputId = "us_map", height = "300px"))
                      )
                    )
                  )
      )
    )
  )

))
