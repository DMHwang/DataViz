#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", tabName = "intro", icon = icon("plane", lib = "glyphicon")),
    menuItem("Summary of Cities", tabName = "citiess", icon = icon("list-alt", lib = "glyphicon"),
             menuSubItem("General City Information", tabName = "cities"),
             selectInput(inputId = "columnSel",
                         label = "Choose Factors of Interest",
                         choices = NULL,
                         multiple = TRUE,
                         )),
    menuItem("City", tabName = "city", icon = icon("stats", lib = "glyphicon"),
             menuSubItem("Single Factor", tabName = "d1"),
             selectInput(inputId = "citysel",
                         label = "Choose a city",
                         choices = NULL,
                         multiple = FALSE),
             selectInput(inputId = "colSel",
                         label = "Choose a Factor",
                         choices = NULL,
                         multiple = FALSE)
             ),
    menuItem("Cost", tabName = "price", icon = icon("usd", lib = "glyphicon"),
             menuSubItem("Price behaviors", tabName = "pricetab"),
             selectInput(inputId = "citysel2",
                         label = "Choose a city",
                         choices = NULL,
                         multiple = FALSE),
             selectInput(inputId = "numColSel",
                         label = "Factor 1",
                         choices = NULL,
                         multiple = FALSE),
             selectInput(inputId = "numColSel2",
                         label = "Factor 2",
                         choices = NULL,
                         multiple = FALSE)
             ),
    menuItem("Attraction", tabName = "attract", icon = icon("glass", lib = "glyphicon"),
             menuSubItem("Attractions", tabName = "attracttab"),
             selectInput(inputId = "citysel3",
                         label = "Choose a city",
                         choices = NULL,
                         multiple = FALSE),
             selectInput(inputId = "numColSel3",
                         label = "Factor 1",
                         choices = NULL,
                         multiple = FALSE),
             selectInput(inputId = "numColSel4",
                         label = "Factor 2",
                         choices = NULL,
                         multiple = FALSE)
    ),
    menuItem("Appendix", tabName = "appendix", icon = icon("hamburger", lib = "glyphicon"),
             menuSubItem("Full Data", tabName = "fulldf"),
             menuSubItem("Two Factors", tabName = "d2any"),
             menuSubItem("Three Factors", tabName = "d3any"),
             selectInput(inputId = "numCol",
                         label = "Factor 1",
                         choices = NULL,
                         multiple = FALSE),
             selectInput(inputId = "numCol2",
                         label = "Factor 2",
                         choices = NULL,
                         multiple = FALSE),
             selectInput(inputId = "numCol3",
                         label = "Factor 3",
                         choices = NULL,
                         multiple = FALSE)
             )
  )
)

body <- dashboardBody(
  tabItems(
    ## tab 1 Intro
    tabItem(tabName = "intro",
            fluidRow(
              box(width = 12,
                  status = "info",
                  title = "Introduction",
                  solidHeader = TRUE,
                  htmlOutput("introText")),
              box(width = 12,
                  status = "primary",
                  title = "Map of Europe",
                  solidHeader =TRUE,
                  imageOutput("EuropeImage")
              )
            )
    ),
    ## tab 2 average of cities
    tabItem(tabName = "cities",
            fluidRow(
              box(width = 12,
                  status = "info",
                  title = "Median City Data",
                  solidHeader = TRUE,
                  textOutput("medExplain"))
            ),
            fluidRow(
              box(width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("avgdata"))
            )
      
    ),
    
    ## tab 3 looking at 1d data
    tabItem(tabName = "d1",
            fluidRow(
              box(width = 12,
                  status = "info",
                  title = "City Information",
                  solidHeader = TRUE,
                  textOutput("cityDescription"))
            ),
            fluidRow(
              box(width = 12,
                  status = "primary",
                  title = "Visualization of Chosen Factor",
                  solidHeader = TRUE,
                  plotlyOutput("city1dPlot"))
              )
            ),
    
    ## tab 4 looking at price info
    tabItem(tabName = "pricetab",
            fluidRow(
              box(width = 12,
                  status = "info",
                  title = "Price Information",
                  solidHeader = TRUE,
                  htmlOutput("priceDescription")
                  )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Distribution of Price and Factor 1",
                solidHeader = TRUE,
                plotlyOutput("price2d")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Density of Price and Factor 1",
                solidHeader = TRUE,
                plotlyOutput("price2dens")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Plot of Price and Two Other Chosen Factors",
                solidHeader = TRUE,
                plotlyOutput("price3d")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Density of Two Chosen Factors",
                solidHeader = TRUE,
                plotlyOutput("price3den")
              )
            )
      
    ),
    
    ## tab 5 looking at attraction info
    tabItem(tabName = "attracttab",
            fluidRow(
              box(width = 12,
                  status = "info",
                  title = "Attraction Summary",
                  solidHeader = TRUE,
                  htmlOutput("attractDescription")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Distribution of Attraction Index and Factor 1",
                solidHeader = TRUE,
                plotlyOutput("attract2d")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Density of Attraction Index and Factor 1",
                solidHeader = TRUE,
                plotlyOutput("attract2den")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Plot of Attraction Index and Two Other Chosen Factors",
                solidHeader = TRUE,
                plotlyOutput("attract3d")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Density of Two Chosen Factors",
                solidHeader = TRUE,
                plotlyOutput("attract3den")
              )
            )
            
    ),
    
    
    ## appendix
    tabItem(tabName = "fulldf",
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Full Data",
                solidHeader = TRUE,
                DTOutput("df_full")
              )
            )),
    tabItem(tabName = "citydf",
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "City Data",
                solidHeader = TRUE,
                DTOutput("df_city")
              )
            )),
    tabItem(tabName = "d2any",
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Scatterplot of First Two Factors",
                solidHeader = TRUE,
                plotlyOutput("scatter2d")
              )
            ),
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "Density Plot of First Two Factors",
                solidHeader = TRUE,
                plotlyOutput("densityall")
              )
            )
            ),
    tabItem(tabName = "d3any",
            fluidRow(
              box(
                width = 12,
                status = "primary",
                title = "3D Scatterplot of All Three Factors",
                solidHeader = TRUE,
                plotlyOutput("scatter3d")
              )
            ))
  )
  
  
)

dashboardPage(
  dashboardHeader(title = "Explore Europe"),
  sidebar,
  body
)