## app.R ##
library(shinydashboard)
library(shiny)
library(shinythemes)
source("helpcode.R")

movies2 <- read.csv("data/movies.csv", header = TRUE, stringsAsFactors=FALSE)
#movies4 <- read.csv("/Users/jaynilvora/Desktop/moviesfinal.csv", header = TRUE, stringsAsFactors=FALSE)
movies2016 <- read.csv("data/newimdb2016.csv", header = TRUE, stringsAsFactors=FALSE)
movies2015 <- read.csv("data/newimdb2015.csv", header = TRUE, stringsAsFactors=FALSE)
movies2014 <- read.csv("data/newimdb2014.csv", header = TRUE, stringsAsFactors=FALSE)
movies2013 <- read.csv("data/newimdb2013.csv", header = TRUE, stringsAsFactors=FALSE)
movies2012 <- read.csv("data/newimdb2012.csv", header = TRUE, stringsAsFactors=FALSE)
movies2011 <- read.csv("data/newimdb2011.csv", header = TRUE, stringsAsFactors=FALSE)
movies2010 <- read.csv("data/newimdb2010.csv", header = TRUE, stringsAsFactors=FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "Movies dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "dash", icon = icon("th")),
      menuItem("Movies Information", tabName = "dashboard", icon = icon("th")),
      menuItem("Recommendation System", tabName = "widget", icon = icon("th")),
      menuItem("Sentiment Analysis", tabName = "widge", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash", 
              fluidRow( 
                box(width = 14, img(src='movie2.png', height = 350, width = 600), img(src='movie3.png', height = 350,width = 300) , img(src='movie5.png',height = 350,width = 280)
                ),                 
                
                img(src='movie1.png', height = 350, width = 720),
                
                img(src='movie6.png', height = 350, width = 480, align = "left")
              )
      ),
      
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                
                box(title = "SELECT YEAR", width = 14, height =30,status = "primary", solidHeader = TRUE ),
                tabBox(
                  title = "",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", width= 14, height = "600px",  
                  
                  tabPanel("2016", 
                           box(
                             title = "Top Movies Information", background = "black", width = 12, height = 500 , status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             shinyUI(fluidPage( 
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectnew", label = h5(""),
                                                    choices = as.character(movies2016$Title[1:33])),
                                        submitButton("Submit")
                                        
                                 ),
                                 
                                 column(width = 7, offset = 0, 
                                        h5(""),
                                        tableOutput("tablenew")) 
                               )
                             )
                             )
                           )),
                  tabPanel("2015", 
                           box(
                             title = "Top Movies Information",background = "black", width = 12, height = 500 ,status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             shinyUI(fluidPage( 
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectnew2015", label = h5(""),
                                                    choices = as.character(movies2015$Title[1:33])),
                                        submitButton("Submit")
                                        
                                 ),
                                 
                                 column(width = 7, offset = 0, 
                                        h5(""),
                                        tableOutput("tablenew2015")) 
                                 
                                 
                               )
                             )
                             )
                           )
                  ),
                  tabPanel("2014", 
                           box(
                             title = "Top Movies Information",background = "black", width = 12, height = 500 ,status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             shinyUI(fluidPage( 
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectnew2014", label = h5(""),
                                                    choices = as.character(movies2014$Title[1:33])),
                                        submitButton("Submit")
                                        
                                 ),
                                 
                                 column(width = 7, offset = 0, 
                                        h5(""),
                                        tableOutput("tablenew2014")) 
                                 
                                 
                               )
                             )
                             )
                           )
                  ),
                  tabPanel("2013", 
                           box(
                             title = "Top Movies Information",background = "black", width = 12, height = 500 ,status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             shinyUI(fluidPage( 
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectnew2013", label = h5(""),
                                                    choices = as.character(movies2013$Title[1:33])),
                                        submitButton("Submit")
                                        
                                 ),
                                 
                                 column(width = 7, offset = 0, 
                                        h5(""),
                                        tableOutput("tablenew2013")) 
                                 
                                 
                               )
                             )
                             )
                           )
                  ),
                  tabPanel("2012", 
                           box(
                             title = "Top Movies Information",background = "black", width = 12, height = 500 ,status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             shinyUI(fluidPage( 
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectnew2012", label = h5(""),
                                                    choices = as.character(movies2012$Title[1:33])),
                                        submitButton("Submit")
                                        
                                 ),
                                 
                                 column(width = 7, offset = 0, 
                                        h5(""),
                                        tableOutput("tablenew2012")) 
                                 
                                 
                               )
                             )
                             )
                           )
                  ),
                  tabPanel("2011", 
                           box(
                             title = "Top Movies Information",background = "black", width = 12, height = 500 ,status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             shinyUI(fluidPage( 
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectnew2011", label = h5(""),
                                                    choices = as.character(movies2011$Title[1:33])),
                                        submitButton("Submit")
                                        
                                 ),
                                 
                                 column(width = 7, offset = 0, 
                                        h5(""),
                                        tableOutput("tablenew2011")) 
                                 
                                 
                               )
                             )
                             )
                           )
                  ),
                  tabPanel("2010", 
                           box(
                             title = "Top Movies Information", background = "black",width = 12, height = 500 ,status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             shinyUI(fluidPage( 
                               fluidRow(
                                 column(width = 4, 
                                        selectInput("selectnew2010", label = h5(""),
                                                    choices = as.character(movies2010$Title[1:33])),
                                        submitButton("Submit")
                                        
                                 ),
                                 
                                 column(width = 7, offset = 0, 
                                        h5(""),
                                        tableOutput("tablenew2010")) 
                                 
                                 
                               )
                             )
                             )
                           )
                  )
                  
                  
                  
                  
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widget",
              
              shinyUI(fluidPage( 
                #titlePanel("Movie Recommendation Engine"),
                box(title = "Movie Recommendation Engine", width = 14, height =30,status = "primary", solidHeader = TRUE ),
                fluidRow(
                  column(width = 4, 
                         selectInput("select", label = h3("Choose Three Movies You Like"),
                                     choices = as.character(movies2$title[1:1000])),
                         
                         selectInput("select2", label = NA,
                                     choices = as.character(movies2$title[1:1000])),
                         
                         selectInput("select3", label = NA,
                                     choices = as.character(movies2$title[1:1000])),
                         
                         submitButton("Submit")
                         
                  ),
                  
                  column(width = 4, offset = 1, 
                         h3("You Might Like These Too!"),
                         tableOutput("table")) 
                  
                  
                )
              ))
      ),
      
      tabItem(tabName = "widge",
              
              shinyUI(fluidPage( 
                #titlePanel("Movie Recommendation Engine"),
                box(title = "Sentiment Analysis of Movie",  width = 14 ,status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    textInput("moviename1", "Enter movie name :", value="#inception") ,
                    submitButton("Submit")  ),
                
                box(title = "Wordcloud",  width = 6,status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("wordcloud")),
                box(title = "Histogram",  width = 6 ,status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("histogram"))
                
                #HTML('<p><img src="/Users/jaynilvora/Downloads/myphoto.png"/></p>')
                
              ))
      )
      
      
    )
  )
)

server <- function(input, output) {
  
  output$table <- renderTable({
    movie_recommendation(input$select, input$select2, input$select3)
  })
  
  output$tablenew <- renderTable({
    movie_new(input$selectnew)
  })
  
  output$tablenew2015 <- renderTable({
    movie_new2015(input$selectnew2015)
  })
  
  output$tablenew2014 <- renderTable({
    movie_new2014(input$selectnew2014)
  })
  output$tablenew2013 <- renderTable({
    movie_new2013(input$selectnew2013)
  })
  output$tablenew2012 <- renderTable({
    movie_new2012(input$selectnew2012)
  })
  output$tablenew2011 <- renderTable({
    movie_new2011(input$selectnew2011)
  })
  output$tablenew2010 <- renderTable({
    movie_new2010(input$selectnew2010)
  })
  output$wordcloud <- renderPlot({wordcloud1(input$moviename1)})
  output$histogram <- renderPlot({histogram1(input$moviename1)})
  
}

shinyApp(ui, server)