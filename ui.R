library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(ggmap)
library(networkD3)

shinyUI(bootstrapPage(

  dashboardPage(
    dashboardHeader(title = "Terrorism WorldWide"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Through Time", tabName = "julien", icon = icon("map")),
        menuItem("In the Media", tabName = "johnny", icon = icon("newspaper-o")),
        menuItem("Worst Attacks", tabName = "maria", icon = icon("heart")),
        menuItem("Success and Region", tabName = "maria2", icon = icon("bar-chart")),
        menuItem("Attack Targets", tabName = "alex1", icon = icon("bar-chart"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "julien",
                  sliderInput(inputId = "year1",
                              label = "Year Scale:",
                              min = 1970, max = 2015, value = 1970,step = 1,
                              animate=animationOptions(interval=500, loop=T)),
                  plotOutput(outputId = "julien1")

      ),
      tabItem(tabName = "johnny",
              sliderInput(inputId = "year", label = "Year",
                          min = 1970, max = 2015, value = 1970),
              tabPanel("Word Cloud", plotOutput(outputId = "word_cloud",
                                                height = "300px"))
      ),
      tabItem(tabName = "maria", title = "Group Name",
              tabPanel("Scatter", plotlyOutput(outputId = "scatter", width = "600px"))

      ),
      tabItem(tabName = "maria2",
              fluidRow(
                selectInput(inputId = 'bar_option', label = 'Select Variables',
                            choices = c("Weapon Type",
                                        "Success",
                                        "Region"),
                            selected = "Weapon Type"),
                tabPanel("Bar Chart Choose",
                         conditionalPanel(condition = "input.bar_option" == "Weapon Type",
                                          plotlyOutput(outputId = "barChartWeapon")),
                         conditionalPanel(condition = "input.bar_option" == "Success",
                                          plotOutput(outputId = "barChartSuccess")),
                         conditionalPanel(condition = "input.bar_option" == "Region",
                                          plotOutput(outputId = "barChartRegion"))
                )
              )
      ),
      tabItem(tabName = "alex1",
              fluidRow(
                tabPanel("Attack Targets", sankeyNetworkOutput("sankey")
              )
      )))
  )
 )
))
