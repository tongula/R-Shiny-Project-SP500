#devtools::install_github("jeromefroe/circlepackeR") #install circlepackeR from Github

library(ggplot2)
library(plotly)
library(data.table)
library(dplyr)
library(DT)
library(shiny)
library(gridExtra)
library(tidyr)
library(reshape)
library(Hmisc)
library(circlepackeR)
library(data.tree)
library(treemap)

stock_data <- read.table("./stock_data.csv",head=TRUE,sep=",")
fundamentals<-read.table("./fundamentals2.csv",head=TRUE,sep=",")
new_securities<-read.table("./new_securities.csv",head=TRUE,sep=",")

new_securities$For.Year = "Current"
  

shinyServer(function(input, output, session){
  
  #data part
  counttable_geo= reactive({
    new_securities %>%
    group_by_(input$option1) %>%
    summarise(count = n())
  })
  
  counttable_marketcap= reactive({
    new_securities %>%
    group_by_(input$option1) %>%
    summarise(MarketCap=sum(marketcap)/1000000000)
  })
  
  #1.3Graph for comparing companies with finanical in each year with the latest S&P500
  #Count companies for different sectors in the S&P500
  counttable1<-new_securities %>%
      group_by(GICS.Sector,For.Year) %>%
      summarise(count = n())
  #add column to show current year
  
  #Count companies for different sectors in different years for the sample
  sample_counttable1<-fundamentals %>%
    group_by(GICS.Sector,For.Year) %>%
    summarise(count = n())
  
  sample_counttable1$For.Year<-as.character(sample_counttable1$For.Year)
  
  #merge
  counttable1_final<-full_join(counttable1, sample_counttable1, by=c("For.Year","GICS.Sector","count"))
  
  #spread and gather to make missing value show 0
  counttable1_final<-spread(counttable1_final,For.Year,count)
  counttable1_final[is.na(counttable1_final)]<-0
  counttable1_final<-counttable1_final %>% gather(Year,Count,-GICS.Sector)
  
  #make year as input to compare with current s&p 500
  counttable1_graph_data=reactive({
    counttable1_final %>% filter(Year == input$option3 | Year == "Current")
  })
  
  output$display3 <- renderPlotly({
    plot_ly(data = counttable1_graph_data(), 
            y = ~reorder(GICS.Sector, Count),x=~Count, 
            color = ~Year,
            type = 'bar',
            orientation = 'h') %>%
      layout(title = 'Sample in a specific year compared to current S&P 500 group',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)) %>%
      add_annotations(xref = 'x', yref = 'y',
                      text = ~Count,
                      font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                      showarrow = FALSE)
    
  })
            
  
  
  #display Pie map
  output$display1 <- renderPlotly({
    plot_ly(data=counttable_geo(), values = ~count,text = ~get(input$option1),type = "pie")%>%
      layout(title = 'S&P Stock Count Pie Chart',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$display2 <- renderPlotly({
    plot_ly(data=counttable_marketcap(), values = ~MarketCap,text = ~get(input$option1),type = "pie")%>%
      layout(title = 'S&P MarketCap Pie Chart (Unit: Billion)',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  #Circle Graph
  
  output$circle1 <- renderCirclepackeR({
    
    summary_data<-new_securities %>% 
      group_by(GICS.Sector,GICS.Sub.Industry) %>%
      summarise(population=n())
    
    summary_data$pathString <- paste("world", 
                                     summary_data$GICS.Sector, 
                                     summary_data$GICS.Sub.Industry, 
                                     sep = "/")
    
    population <- as.Node(summary_data)
    
    circlepackeR(population, size = "population", color_min = "hsl(56,80%,80%)", 
               color_max = "hsl(341,30%,40%)")                       
  
  })

  #Process and select ratio for analysis
  fundamentals$market_cap = fundamentals$Price*fundamentals$Estimated.Shares.Outstanding
  fundamentals$leverage = fundamentals$Total.Liabilities/fundamentals$Total.Assets
  
  selectedratiotable = fundamentals %>%
    mutate(market_cap=market_cap/1000000000,
           Total.Revenue = Total.Revenue/1000000000,
           Total.Liabilities = Total.Liabilities/1000000000,
           Capital.Expenditures = Capital.Expenditures/1000000,
           Cash.and.Cash.Equivalents = Cash.and.Cash.Equivalents/1000000,
           Net.Cash.Flow.Operating = Net.Cash.Flow.Operating/1000000,
           Net.Income = Net.Income/1000000,
           Profit.Margin,
           R.and.D = R.and.D/1000000)
  
  ratiotable1 = dplyr::select(selectedratiotable,
                              PE,
                              market_cap,
                              Price,
                              leverage,
                              Total.Revenue,
                              Total.Liabilities,
                              Capital.Expenditures,
                              Cash.and.Cash.Equivalents,
                              Net.Cash.Flow.Operating,
                              Net.Income,
                              Profit.Margin,
                              R.and.D,
                              For.Year,
                              GICS.Sector,
                              Security)
  
  ratiotable2 = ratiotable1[ratiotable1[,1] > (mean(ratiotable1[,1])-sd(ratiotable1[,1])) & ratiotable1[,1] < (mean(ratiotable1[,1])+sd(ratiotable1[,1])) &
                              ratiotable1[,2] > (mean(ratiotable1[,2])-sd(ratiotable1[,2])) & ratiotable1[,2] < (mean(ratiotable1[,2])+sd(ratiotable1[,2])) &
                              ratiotable1[,3] > (mean(ratiotable1[,3])-sd(ratiotable1[,3])) & ratiotable1[,3] < (mean(ratiotable1[,3])+sd(ratiotable1[,3])) &
                              ratiotable1[,4] > (mean(ratiotable1[,4])-sd(ratiotable1[,4])) & ratiotable1[,4] < (mean(ratiotable1[,4])+sd(ratiotable1[,4])) &
                              ratiotable1[,5] > (mean(ratiotable1[,5])-sd(ratiotable1[,5])) & ratiotable1[,5] < (mean(ratiotable1[,5])+sd(ratiotable1[,5])) &
                              ratiotable1[,6] > (mean(ratiotable1[,6])-sd(ratiotable1[,6])) & ratiotable1[,6] < (mean(ratiotable1[,6])+sd(ratiotable1[,6])) &
                              ratiotable1[,7] > (mean(ratiotable1[,7])-sd(ratiotable1[,7])) & ratiotable1[,7] < (mean(ratiotable1[,7])+sd(ratiotable1[,7])) &
                              ratiotable1[,8] > (mean(ratiotable1[,8])-sd(ratiotable1[,8])) & ratiotable1[,8] < (mean(ratiotable1[,8])+sd(ratiotable1[,8])) &
                              ratiotable1[,9] > (mean(ratiotable1[,9])-sd(ratiotable1[,9])) & ratiotable1[,9] < (mean(ratiotable1[,9])+sd(ratiotable1[,9])) &
                              ratiotable1[,10] > (mean(ratiotable1[,10])-sd(ratiotable1[,10])) & ratiotable1[,10] < (mean(ratiotable1[,10])+sd(ratiotable1[,10])) &
                              ratiotable1[,11] > (mean(ratiotable1[,11])-sd(ratiotable1[,11])) & ratiotable1[,11] < (mean(ratiotable1[,11])+sd(ratiotable1[,11])),]
  
  
  corr_ratiotable = reactive({
    selectedratiotable %>%
      filter(GICS.Sector==input$option4) %>%
      select(Z.PE=PE,
           Z.market_cap=market_cap,
           Z.Price=Price,
           leverage,
           Total.Revenue,
           Total.Liabilities,
           Capital.Expenditures,
           Cash.and.Cash.Equivalents,
           Net.Cash.Flow.Operating,
           Net.Income,
           Profit.Margin,
           R.and.D)
  })
  
  
  
  name_corr = c("Z.PE","Z.market_cap","Z.Price","leverage","Total.Revenue","Total.Liabilities","Capital.Expenditures","Cash.and.Cash.Equivalents","Net.Cash.Flow.Operating","Net.Income","Profit.Margin","R.and.D")
  
  output$heat <- renderPlotly({
    plot_ly(x = name_corr, y = name_corr, z = round(cor(corr_ratiotable(), method = "spearman"), 3), 
            key = round(cor(corr_ratiotable(),method = "spearman"), 3), type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(corr_ratiotable()[vars], c("x", "y"))
      yhat <- fitted(lm(y ~ x, data = d))
      plot_ly(d, x = ~x) %>%
        add_markers(y = ~y) %>%
        add_lines(y = ~yhat) %>%
        layout(xaxis = list(title = s[["x"]]), 
               yaxis = list(title = s[["y"]]), 
               showlegend = FALSE)
    } else {
      plotly_empty()
    }
  })

  

#box plot

  #remove outlier
  #remove_outliers <- function(x, na.rm = TRUE, ...) {
   # qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    #H <- 1.5 * IQR(x, na.rm = na.rm)
   # y <- x
   # y[x < (qnt[1] - H)] <- NA
   # y[x > (qnt[2] + H)] <- NA
   # y
  #}
  
  # ratiotable3 = ratiotable2 %>%
  #   mutate(For.Year=as.character(For.Year))
  
  graph_data_2 = reactive ({
    ratiotable2 %>%
    select(Security, GICS.Sector,For.Year,input$ratio) %>%
      filter(For.Year == input$year)
  })
    
#filter((For.Year==input$year) & (GICS.Sector==input$sector) ) %>%
#filter(input$ratio<mean(input$ratio)+sd(input$ratio)) %>%
#filter(input$ratio>mean(input$ratio)-sd(input$ratio))
  
  output$boxplot <- renderPlotly({
    print(graph_data_2())
    plot_ly(data=graph_data_2(), x=~GICS.Sector,y=~get(input$ratio), color =~GICS.Sector,type = "box") %>%
      layout(xaxis = list(title = "Sector"), 
             yaxis = list(title = as.character(input$ratio)))
  })
  # 
  # p <- plot_ly(ggplot2::diamonds, x = ~cut, y = ~price, color = ~clarity, type = "box") %>%
  #   layout(boxmode = "group")

#scatter plot
  temp_data = reactive({
    ratiotable2 %>%
      select(Security,GICS.Sector,For.Year,input$ratio2,input$ratio) %>%
      filter((For.Year==input$year) & (GICS.Sector==input$sector))
    #& (Revenue<100) & (PE<50) & (PE>-10)
  })
  
  output$display <- renderPlotly({
    plot_ly(temp_data(), x = ~get(input$ratio2), y = ~get(input$ratio), color = ~GICS.Sector,text = ~Security, type = "scatter") %>%
      layout(xaxis = list(title = as.character(input$ratio2)), 
            yaxis = list(title = as.character(input$ratio)))
  })
  
  
#Over Year - Ratio
  
  mediantable<-aggregate(ratiotable1[,1:11],list(ratiotable1$GICS.Sector,ratiotable1$For.Year),median)
  colnames(mediantable)[1:5] <-c("GICS.Sector","For.Year","PE","market_cap","Price")
  mediantable$For.Year<-as.character(mediantable$For.Year)
  
  temp2_data = reactive({
    mediantable %>%
      select(GICS.Sector,For.Year,input$ratio11) %>%
      filter(GICS.Sector %in% input$sector11) 
  })
  
  output$lineplot1 <-renderPlotly({
    plot_ly(temp2_data(), x=~For.Year, y=~get(input$ratio11), group=~GICS.Sector,
            type="scatter",color=~GICS.Sector, mode="lines+markers")
  })
  
  output$table11 <- renderTable({
    temp2_data()
  })
  
  #data table
  datasetInput<- reactive({
    data <- ratiotable1
  if (input$sector1 != "All") {
    data <- data[data$GICS.Sector == input$sector1,]
  }
  if (input$year1 != "All") {
    data <- data[data$For.Year == input$year1,]
  }
    data
  })
  
  # show data using DataTable
  output$table <- DT::renderDataTable(DT::datatable({
    datasetInput()},options = list(scrollX = TRUE)))
  #formatStyle(input$year,input$sector, background="skyblue", fontWeight='bold')

  #output for download button based on selection
   output$downloadData <- downloadHandler(
    filename = function() {
     paste("dataset-sector-",input$sector1,"year-",input$year1, ".csv", sep="")
    },
    content = function(file) {
     write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
})
