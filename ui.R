library(shiny)
library(data.table)
library(plotly)
library(shinydashboard)

stock_data <- fread("./stock_data.csv")
fundamentals<-read.table("C:\\Users\\XuT\\Desktop\\DS\\shiny project data\\sp500\\fundamentals2.csv",head=TRUE,sep=",")
new_securities<-read.table("C:\\Users\\XuT\\Desktop\\DS\\shiny project data\\sp500\\new_securities.csv",head=TRUE,sep=",")


dashboardPage(
  dashboardHeader(title = "S&P 500 Fundemantal Analysis"),
  dashboardSidebar(
    
    sidebarUserPanel("NYC DSA",
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("Stock", tabName = "stock", icon = icon("dollar")),
      menuItem("HeatMap", tabName = "heatmap", icon = icon("map")),
      menuItem("Median", tabName = "median", icon = icon("calculator")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    ),
  
    selectizeInput(inputId = "sector",
                   label = "Select Sector(s)",
                   choices = unique(stock_data[, GICS.Sector]), multiple=TRUE,
                   selected = 'Health Care'),
    selectizeInput(inputId = "year",
                   label = "Select year",
                   choices = unique(stock_data[, For.Year]),
                   selected = 2015)
  ),
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    # ),
    tabItems(
      
      tabItem(tabName = "stock",
              fluidRow(
                tabBox(
                  title = "Stock",
                  id = "tabset1", height = "220px", width = "12",
                  tabPanel("Stock Summary",
                           fluidRow(
                             column(6,
                             selectizeInput(inputId = "option1",
                                            label = "View by Sector/State/Country",
                                            choices = c("GICS.Sector","State","Country"), multiple=FALSE,
                                            selected = "GICS.Sector")
                                   ),
                             column(6,
                               selectizeInput(inputId = "option2",
                                              label = "View by Sector/State/Country",
                                              choices = c("GICS.Sector","State","Country"), multiple=FALSE,
                                              selected = "GICS.Sector")
                             )
                             
                           
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
                                                   choices = c(2013,2014,2015,2016), multiple=FALSE,
                                                   selected = 2016)
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
                                      choices = unique(stock_data[, GICS.Sector]), multiple=FALSE,
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
                column(6,
                       selectizeInput(inputId = "ratio",
                                      label = "Select Factor",
                                      choices = c("PE","market_cap","Price","leverage","Debt_to_EBITDA","Total.Revenue","Total.Liabilities","Capital.Expenditures","Cash.and.Cash.Equivalents","Earnings.Before.Interest.and.Tax","Gross.Profit","Net.Cash.Flow.Operating","Net.Income","Operating.Income","Profit.Margin","R.and.D","SGA"), 
                                      multiple=FALSE,
                                      selected = 'Total.Revenue')
                )),
              fluidRow(plotlyOutput("boxplot"), width = 12),
              fluidRow(plotlyOutput("display"), width = 12)),
      
      
      tabItem(tabName = "data",
              fluidRow(
                column(6,
                       selectInput("sector1",
                                   "Sector:",
                                   c("All",
                                     unique(as.character(stock_data$GICS.Sector))))
                ),
                column(6,
                       selectInput("year1",
                                   "Year:",
                                   c("All",
                                     c(2013,2014,2015,2016)))
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
