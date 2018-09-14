library(shiny)
library(data.table)
library(plotly)
library(shinydashboard)
library(circlepackeR)
library(data.tree)
library(treemap)

stock_data <- read.table("./stock_data.csv",head=TRUE,sep=",")
fundamentals<-read.table("./fundamentals2.csv",head=TRUE,sep=",")
new_securities<-read.table("./new_securities.csv",head=TRUE,sep=",")


dashboardPage(
  dashboardHeader(title = "S&P 500 Fundemantal Analysis"),
  dashboardSidebar(
    
    sidebarUserPanel("Tracey Xu"),
                     #image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("Stock", tabName = "stock", icon = icon("dollar")),
      menuItem("HeatMap", tabName = "heatmap", icon = icon("map")),
      menuItem("Median", tabName = "median", icon = icon("calculator")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    )
  
    
  ),
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    # ),
    tabItems(
      
      tabItem(tabName = "stock",
              #Nedd to add this fluidRow before adding tabBox
              fluidRow(
                tabBox(
                  title = "Stock",
                  id = "tabset1", height = "12", width = "12", #220px
                  tabPanel("Sector Structure",
                           fluidRow(
                             column(12, div(style ="height: 100px; position: relative"), circlepackeROutput("circle1", width = "100%", height = "800px"), height = 400)
                             
                           )
                  ),
                  tabPanel("Stock Summary",
                           fluidRow(
                             column(6,
                             selectizeInput(inputId = "option1",
                                            label = "View by Sector/State/Country",
                                            choices = c("GICS.Sector","State","Country"), multiple=FALSE,
                                            selected = "GICS.Sector")
                                   )
                             #column(6,
                             # selectizeInput(inputId = "option2",
                             #                 label = "View by Sector/State/Country",
                             #                  choices = c("GICS.Sector","State","Country"), multiple=FALSE,
                             #                  selected = "GICS.Sector")
                             #)
                             
                           
                  ),
                           
                           fluidRow(
                               column(6,plotlyOutput("display1"), height = 400),
                               column(6,plotlyOutput("display2"), height = 400)
                             )
                  ),
                  tabPanel("Sample Analysis",
                           fluidRow(
                             column(6,
                                    selectizeInput(inputId = "option3",
                                                   label = "Choose Year",
                                                   choices = c(2012,2013,2014,2015,2016), multiple=FALSE,
                                                   selected = 2015)
                             )
                             
                           ),
                           
                           fluidRow(
                             column(12,plotlyOutput("display3"), height = 400)
                           )
                  )
                )
              )
      ),
              
      tabItem(tabName = "heatmap",
              fluidRow(
                column(6,
                       selectizeInput(inputId = "option4",
                                      label = "Select Sector",
                                      choices = unique(as.character(stock_data$GICS.Sector)), multiple=FALSE,
                                      selected = 'Health Care')
                )),
              fluidRow(column(12,
                              plotlyOutput("heat"), height = 600)),
              fluidRow(column(12,
                              plotlyOutput("scatterplot"), height = 300)),
              fluidRow(column(12,
                              verbatimTextOutput("selection"), height = 100))
              
      ),
      tabItem(tabName = "median",
              fluidRow(
                tabBox(
                  title = "Analysis",
                  id = "tabset2", height = "220px", width = "12",
              tabPanel("Detail",
                       fluidRow(
                         column(4,
                                selectizeInput(inputId = "ratio",
                                               label = "Select Y Factor",
                                               choices = c("PE","market_cap","Price","leverage","Total.Revenue","Total.Liabilities","Capital.Expenditures","Cash.and.Cash.Equivalents","Net.Cash.Flow.Operating","Net.Income","Profit.Margin","R.and.D"), 
                                               multiple=FALSE,
                                               selected = 'Total.Revenue')
                         ),
                         column(4,
                                selectizeInput(inputId = "year",
                                               label = "Select year",
                                               choices = c(2012,2013,2014,2015,2016),
                                               selected = 2015)
                         )),
                       
                       fluidRow(plotlyOutput("boxplot"), width = 12),
                       
                       fluidRow(
                         column(4,
                                selectizeInput(inputId = "sector",
                                               label = "Select Sector(s)",
                                               choices = unique(as.character(stock_data$GICS.Sector)), multiple=TRUE,
                                               selected = 'Health Care')
                         ),
                         column(4,
                                selectizeInput(inputId = "ratio2",
                                               label = "Select X Factor",
                                               choices = c("PE","market_cap","Price"), 
                                               multiple=FALSE,
                                               selected = 'PE')
                         )),
                       fluidRow(plotlyOutput("display"), width = 12)
                       ),
              tabPanel("Summary",
                       fluidRow(
                         column(4,
                                selectizeInput(inputId = "ratio11",
                                               label = "Select Factor",
                                               choices = c("PE","market_cap","Price","leverage","Total.Revenue","Total.Liabilities","Capital.Expenditures","Cash.and.Cash.Equivalents","Net.Cash.Flow.Operating","Net.Income","Profit.Margin","R.and.D"), 
                                               multiple=FALSE,
                                               selected = 'Total.Revenue')
                         ),
                         column(4,
                                selectizeInput(inputId = "sector11",
                                               label = "Select Sector(s)",
                                               choices = unique(as.character(stock_data$GICS.Sector)), multiple=TRUE,
                                               selected = 'Health Care')
                         )),
                       
                       fluidRow(plotlyOutput("lineplot1"), width = 12),
                       fluidRow(tableOutput("table11"), width = 12))
                )
                      #  fluidRow(
                      #    column(4,
                      #           selectizeInput(inputId = "sector22",
                      #                          label = "Select Sector(s)",
                      #                          choices = unique(as.character(stock_data$GICS.Sector)), multiple=FALSE,
                      #                          selected = 'Health Care')
                      #    ),
                      #    column(4,
                      #           selectizeInput(inputId = "ratio22",
                      #                          label = "Select Factor",
                      #                          choices = c("PE","market_cap","Price","leverage","Total.Revenue","Total.Liabilities","Capital.Expenditures","Cash.and.Cash.Equivalents","Net.Cash.Flow.Operating","Net.Income","Profit.Margin","R.and.D"), 
                      #                          multiple=TRUE,
                      #                          selected = 'PE')
                      #    )),
                      #  
                      # fluidRow(plotlyOutput("lineplot2"), width = 12)
                      #  )
      )),
      
      tabItem(tabName = "data",
              fluidRow(
                column(4,
                       selectInput("sector1",
                                   "Sector:",
                                   c("All",
                                     unique(as.character(stock_data$GICS.Sector))))
                ),
                column(4,
                       selectInput("year1",
                                   "Year:",
                                   c("All",
                                     c(2012,2013,2014,2015,2016)))
                ),
                column(4,
                       downloadButton("downloadData", "Download")
                )
                
              ),
              # Create a new row for the table.
              fluidRow(
                column(12,
                DT::dataTableOutput("table"))
              )
      )
    )
  )
)
