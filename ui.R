library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(ggmap)

shinyUI(bootstrapPage(

  dashboardPage(
    dashboardHeader(title = "Terrorism WorldWide"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Julien", tabName = "julien", icon = icon("map")),
        menuItem("Johnny", tabName = "johnny", icon = icon("newspaper-o")),
        menuItem("Maria", tabName = "maria", icon = icon("heart"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "julien",
                  sliderInput(inputId = "year1",
                              label = "Year Scale:",
                              min = 1970, max = 2015, value = 1970,step = 1),
                  plotOutput(outputId = "julien1")

      ),
      tabItem(tabName = "johnny",
              sliderInput(inputId = "year", label = "Year",
                          min = 1970, max = 2015, value = 1970),
              tabPanel("Word Cloud", plotOutput(outputId = "word_cloud",
                                                height = "300px"))
      ),
      tabItem(tabName = "maria",
              sliderInput(inputId = "year", label = "Year",
                          min = 1970, max = 2015, value = 1970),
              tabPanel("Scatter", plotOutput(outputId = "scatter",
                                                height = "300px"))
#                plotOutput(outputId = "scatter")

      )
    )
  )
 )
))
