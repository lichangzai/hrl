####################################
#### Google Analytics - server.R ###
####################################

library(shiny)
library(plyr)
library(ggplot2)
library(sqldf)

#load("analytics.Rdata") # load the dataframe
# import hotel data
analytics0 <- read.table("bi_hrl_jd.csv",header=TRUE,sep=",",stringsAsFactors = TRUE)

dateHour <- as.character(analytics0$dateHour)
networkDomain <- as.character(analytics0$networkDomain)
visitors <- as.numeric(analytics0$visitors)
visits <- as.numeric(analytics0$visits)
bounces <-  as.numeric(analytics0$bounces)
timeOnSite <- as.numeric(analytics0$timeOnSite)
Date <- as.Date(analytics0$Date)
Hour <- as.numeric(analytics0$Hour)
Domain <- analytics0$Domain

analytics0 <- data.frame(dateHour,networkDomain,visitors,visits,bounces,timeOnSite,Date,Hour,Domain)

# import hotel data
analytics1 <- read.table("bi_hrl_jp.csv",header=TRUE,sep=",",stringsAsFactors = TRUE)

dateHour <- as.character(analytics1$dateHour)
networkDomain <- as.character(analytics1$networkDomain)
visitors <- as.numeric(analytics1$visitors)
visits <- as.numeric(analytics1$visits)
bounces <-  as.numeric(analytics1$bounces)
timeOnSite <- as.numeric(analytics1$timeOnSite)
Date <- as.Date(analytics1$Date)
Hour <- as.numeric(analytics1$Hour)
Domain <- analytics1$Domain

analytics1 <- data.frame(dateHour,networkDomain,visitors,visits,bounces,timeOnSite,Date,Hour,Domain)


packageCheck = unlist(packageVersion("shiny"))

if(packageCheck[1] == 0 & packageCheck[2] < 9){
  
  shinyOld = TRUE
} else {
  shinyOld = FALSE
}

shinyServer(function(input, output, session) { # server is defined within these parentheses
  
  # prep data once and then pass around the program
  
  observe({
    
    if(class(input$domainShow) != "character"){
      
      updateCheckboxGroupInput(session, "domainShow",
                               choices = list("实际话量" = "Actual",
                                              "预测话量" = "Predict"),
                               selected = ifelse(shinyOld, "实际话量", "Actual")
      )
      
    }
    
  })
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "hotel" = analytics0,
           "ticket" = analytics1)
  })
  
  
  
  passData <- reactive({
    
    analytics <- datasetInput()
    
    analytics <- analytics[analytics$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
    
    #analytics <- analytics[analytics$Hour %in% as.numeric(input$minimumTime) : as.numeric(input$maximumTime),]
    
    if(class(input$domainShow)=="character"){
      
      analytics <- analytics[analytics$Domain %in% unlist(input$domainShow),]
      
    }
    
    analytics
    
  })
  
  passData2 <- reactive({
    
    analytics <- datasetInput()
    
    analytics <- analytics[analytics$Date %in% as.Date(input$dateselect),]
    
    analytics <- analytics[analytics$Hour %in% as.numeric(input$timerange[1]) : as.numeric(input$timerange[2]),]
    
    if(class(input$domainShow)=="character"){
      
      analytics <- analytics[analytics$Domain %in% unlist(input$domainShow),]
      
    }
    
    analytics
    
  })
  
  output$monthGraph <- renderPlot({
    
    graphData <- ddply(passData(), .(Domain, Date), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("呼入量")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("接起率 %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Date, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("排队呼损数")
      
    }
    
    if(input$smoother){
      
      theGraph <- theGraph + geom_smooth()
      
    }
    
    
    
    print(theGraph)
    

  })
  
  output$hourGraph <- renderPlot({
    
    graphData = ddply(passData2(), .(Domain, Hour), numcolwise(sum))
    
    if(input$outputType == "visitors"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = visitors, group = Domain, colour = Domain)) + geom_line() +
        ylab("呼入量")
      
    }
    
    if(input$outputType == "bounceRate"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = bounces / visits * 100, group = Domain, colour = Domain)) +
        geom_line() + ylab("接起率 %")
      
    }
    
    if(input$outputType == "timeOnSite"){
      
      theGraph <- ggplot(graphData, aes(x = Hour, y = timeOnSite / visits, group = Domain, colour = Domain)) +
        geom_line() + ylab("排队呼损数")
      
    }
        
    if(input$smoother){
      
      theGraph <- theGraph + geom_smooth()
      
    }
    
    print(theGraph)
    
  })
  
  
  output$textDisplay <- renderText({ 
    
    t1 <- passData()
    sum1 <- sqldf("select sum(visitors) from t1 where Domain='Actual'")
    
    paste("统计时间范围为：",
      length(seq.Date(input$dateRange[1], input$dateRange[2], by = "days")),
      " 天. 此段时间的实际呼入量为：", sum1, "."
    )
    
  })
  
  output$textDisplay2 <- renderText({ 
    
    t1 <- passData2()
    sum1 <- sqldf("select sum(visitors) from t1 where Domain='Actual'")
    
    paste("此天 ",as.numeric(input$timerange[1]),"时到",as.numeric(input$timerange[2]),"时的呼入量为：",
           sum1, "."
    )
    
  })
  
  tableData1 <- reactive({
    t1 <- passData()
    t1 <- sqldf("select substr(dateHour,1,8) 日期,sum(visitors) 实际呼入量,avg(bounces) 接起率,sum(timeOnSite) 呼损数 
                from t1 where Domain='Actual' group by substr(dateHour,1,8)
               ")
    
  })
  
  
  tableData2 <- reactive({
    t1 <- passData2()
    t1 <- sqldf("select dateHour 时间,visitors 实际呼入量,bounces 接起率,timeOnSite 呼损数 from t1 where Domain='Actual'")
    
  })
    
  output$view1 <- renderTable({
    tableData1()
  })
  
  output$view2 <- renderTable({
    tableData2()
  })
  
  output$downloadData1 <- downloadHandler(

    filename = function() { 
      paste(input$dataset,'_按天数据', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(tableData1(), file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    
    filename = function() { 
      paste(input$dataset,'_按时数据', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(tableData2(), file)
    }
  )
  
})
