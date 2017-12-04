#########################
# Lindy Woodburn
# Coursera Project - Shiny Applicaton and Reproducible Pitch
# Server.R
library(shiny)
library(tidyverse)
library(lubridate)
library(markdown)
#Sys.setenv(CKANR_DEFAULT_URL="http://data.gov.au")
#get_default_url()
## [1] "http://data.gov.au"




## -----------------------------------------------------------------------------
##   Data preparation
## -----------------------------------------------------------------------------


d <- read.csv("./data/road_oz.csv")
users <- unique(d$Road_User)

## -----------------------------------------------------------------------------
##          SHINY SERVER 
## -----------------------------------------------------------------------------

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


# Setup user selection
output$Box1 = renderUI(selectInput(inputId  = 'input_user', 
                   label    = "Select User Type:", 
                   c("All",as.character(users))
                   ))

output$Box1b = renderUI(selectInput("input_state", "Select State:",
                                   c("All","QLD", "WA", "NSW", "VIC", "SA", "TAS","NT","ACT")))


output$Box2 = renderUI(radioButtons("rb_gender", "Select Gender:",
                                    choiceNames = list(
                                      "All","Male","Female"
                                    ),
                                    choiceValues = list(
                                      "All","Male","Female"
                                    )))

output$Box3 = renderUI(radioButtons("rb_age", "Select Age Group:",
                                    choiceNames = list(
                                      "All","<18","18-25","25-40","40-65","65+"
                                    ),
                                    choiceValues = list(
                                     1,2,3,4,5,6
                                    )))



output$Box4 = renderUI(radioButtons("rb_plot", "Plot by:",
                                    choiceNames = list(
                                      "Road User Type", "State","Gender", "Age Range"
                                    ),
                                    choiceValues = list(
                                      1,2,3,4
                                    )))


# Create Headers
output$outText2 <- renderText({ 
  tryCatch(buildplot(0,1),TRUE)
})
output$outText3 <- renderText({ 
  try(buildplot(0,1),TRUE)
})

buildplot <- function(plot_type, return_title = FALSE) {
  
  # Escape --> prevent error on startup
  if(is.null(input$rb_plot) || is.na(input$rb_plot))
  {
    return()
  }
  # Function to apply filters and selections - returns data set, chart or titles.
  df1 <- d[d$Gender=='Female' | d$Gender == "Male",]
  if(input$rb_plot==1){
    df1 <- df1 %>% mutate(plot_by = Road_User)
    plotby = "Road User Type"
  } else if (input$rb_plot==2){
    df1 <- df1 %>% mutate(plot_by = State)
    plotby = "State"
  } else if (input$rb_plot==3){
    df1 <- df1 %>% mutate(plot_by = Gender)
    plotby = "Gender"
  } else {
    df1 <- df1%>% mutate(Age_Range = cut(df1$Age,breaks = c(0,18,25,40,65,110))) %>% mutate(plot_by = Age_Range)
    plotby= "Age Range"
  }
  
  title_age = 'All Age Groups'
  if (input$rb_age ==1) {
    df1 <- df1
  } else if (input$rb_age == 2) {
    df1 <- df1[ (df1$Age <= 18 ),]
    title_age = 'Under 18'
  } else if (input$rb_age == 3) {
    df1 <- df1[ (df1$Age > 18 ) & (df1$Age <=25),]
    title_age = '18 - 25 years'
  } else if (input$rb_age == 4) {
    df1 <- df1[ (df1$Age > 25 ) & (df1$Age <=40),]
    title_age = '25-40 years'
  } else if (input$rb_age == 5) {
    df1 <- df1[ (df1$Age > 40 ) & (df1$Age <=65),]
    title_age = '40 - 65 years'
  } else {
    df1 <- df1[df1$Age >65,]
    title_age = 'Over 65 years'
  }
  
  title_state = 'Australia'
  if(input$input_state!="All") {
    df1 = df1[df1$State==input$input_state,]
    title_state = input$input_state
  }
  
  
  title_user = 'All Road User Types'
  if(input$input_user!="All") {
    df1 = df1[df1$Road_User==input$input_user,]
    title_user = input$input_user
  }
  
  title_gender = 'Male and Female'
  if(input$rb_gender!="All") {
    df1 = df1[df1$Gender ==input$rb_gender,]
    title_gender = input$rb_gender
  } 
  
  if (return_title==1){
     return(paste(title_state,': ',title_user,' - ',title_age,' - ', title_gender))
  }
  if (return_title==2){
    df1 = df1 %>% mutate(plot_by = plotby) %>% select(DateTime, State, Gender, Road_User, Age, Speed_Limit)
    return(df1)
  }
  
  if(plot_type==0){
    dd <- df1 %>% select(hour, plot_by) %>% group_by(hour,plot_by) %>% summarise(No_fatalities = n())
    g <- ggplot(dd, aes(x = hour, y = No_fatalities))+geom_line(aes(colour = plot_by), size = 1.2)
    title_text = 'Hour of Day'
  } else {
    dd <- df1 %>% select(Year, plot_by) %>% group_by(Year,plot_by) %>% summarise(No_fatalities = n())
    g <- ggplot(dd, aes(x = Year, y = No_fatalities))+geom_line(aes(colour = plot_by), size = 1.2)
    g <- g + scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015))
    title_text = 'Year'
  }
  
  g <- g + theme(axis.text=element_text(angle=0, size = 12, face = 'bold'),
                 #axis.title.y=element_blank(),
                 axis.title=element_text(size = 16, face = 'bold'),
                 legend.position="right",
                 legend.text=element_text(size=14),
                 legend.title=element_text(size=16, face = 'bold'),
                 strip.text = element_text(size = 14, face="bold", colour = "white")) 
  g <- g + xlab(title_text) + ylab("No. of Fatalities") + guides(colour=guide_legend(title=plotby))
  g
}  # end of setup function


# Send output to plots
output$plot <-  renderPlot({
    g <- try(buildplot(0),TRUE)
    try(print(g),TRUE)
})  
output$plot2 <-  renderPlot({
  g <- try(buildplot(1),TRUE)
  
  try(print(g),TRUE)
})  

# Render data table and create download handler
output$table <- renderDataTable(
  buildplot(0,2), options = list(bFilter = FALSE, iDisplayLength = 10))
  
})
