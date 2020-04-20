library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)


dashboardPage(skin="green",
              dashboardHeader(title="Visualization Sandbox"),
              dashboardSidebar(
                
                sidebarMenu(
                  menuItem( text="Bar chart", icon=icon("signal"),
                            checkboxGroupInput("aesthetics", label = "Aesthetics", choices=c("fill", "flip", "dodge")),
                            actionButton("bargraph", "Graph", icon= icon("play-circle"), width="85%")),
                  menuItem(text="Line chart", icon=icon("chart-line"),
                           checkboxGroupInput("lineaesthetics", label = "Aesthetics", choices=c("color","linetype", "fill", "ribbon")),
                           actionButton("linegraph", "Graph", icon =icon("play-circle"), width="85%")),
                  menuItem(text="Scatter plot", icon=icon("circle"),
                           checkboxGroupInput("scatteraesthetics", label="Aesthetics", choices=c("color", "shape", "jitter")),
                           actionButton("scatter", "Graph", icon=icon("play-circle"), width="85%")),
                  menuItem(text="Box plot", icon = icon("box-open"),
                           checkboxGroupInput("boxaesthetics", label="Aesthetics", choices=c("color", "outlier color", "jitter")),
                           actionButton("boxPlot", "Graph", icon=icon("play-circle"), width="85%")),
                           
                
                actionButton("cheatsheet", "Cheatsheet", icon=icon("book"), width="85%"),
                
                HTML('<div align="center">'),
                radioButtons("package", "", c("ggplot", "plotly"), inline=T, width="100%"),
                HTML('</div>')),
                htmlOutput("imageHTML")),
              dashboardBody(
                verbatimTextOutput("code"), 
                fluidRow(
                  column(width=12, align="center",
                         uiOutput("sheet"))),
                box(collapsible=T, width="100%", conditionalPanel(condition="input.package == 'ggplot'",plotOutput('plot')),
                                                 conditionalPanel(condition="input.package == 'plotly'", plotlyOutput('plot2')), DTOutput('Table'))
              )  
)
              
