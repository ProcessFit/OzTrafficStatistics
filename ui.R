#########################
# Lindy Woodburn
# Coursera Project - Shiny Applicaton and Reproducible Pitch
# UI.R

library(shiny)


# Define UI for application
shinyUI(fluidPage(includeCSS("www/style.css"),titlePanel('',windowTitle="Oz Road Traffic Statistics"),
  navbarPage(tags$h2(class="panel-title1","Australian Road Traffic Statistics     "),
             tabPanel("Explore the Data",

      fluidRow(
            column(3,tags$div(class="panel panel-primary",
                      # Column 1: 2 units wide. For selectors
                      # Selector lists are populated from the data defined in the Server file.
                      tags$div(class="panel-heading",tags$h3(class="panel-title","Filters")),
                      tags$div(class = "well",
                               fluidRow(style = "padding: 12px;",
                                               uiOutput("Box1"),
                                               uiOutput("Box1b"),
                                               uiOutput("Box2"),
                                               uiOutput("Box3"),
                                               uiOutput("Box4")
                                        ),
                              textOutput("outText")
                      )
              )), # end of first column in first row 
    
            # Tabbed display of charts and data
            column(8,tags$div(class="panel panel-primary",
                      tabsetPanel(
                        
                        tabPanel(p(icon("line-chart"), "Trends by Year"),
                                 tags$div(class="panel-heading", tags$h3(class="panel-title","Time Series:"),
                                          textOutput("outText3")),
                                 tags$div(class = "well", plotOutput("plot2", height = 600))
                        ), # end of tab panel1
                        
                        tabPanel(p(icon("clock-o"), "By Hour of Day"),
                        tags$div(class="panel-heading", tags$h3(class="panel-title","Road Fatalities by Hour of Day:"),
                                                                              textOutput("outText2")),
                        tags$div(class = "well", plotOutput("plot", height = 600))
                        ), # end of tab panel2
                    
                        tabPanel(
                        p(icon("table"), "Data"),
                        column(dataTableOutput(outputId="table"), width = 12),
                        downloadButton('downloadData', 'Download')
                      )) # End of tab panel 3
                      
                  )) # end of second column
            )), # end of row and tab panel
         
      tabPanel("About",     fluidRow(column(1,
                                            textOutput("outTextX")
                                            ),
        column(9,
        includeMarkdown("about.md")
      )))) # end of Navbar
  
)) # shiny/page close brackets