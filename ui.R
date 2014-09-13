###############################
### Google Analytics - ui.R ###
###############################

library(shiny) 

packageCheck = unlist(packageVersion("shiny"))

if(packageCheck[1] == 0 & packageCheck[2] < 9){
  
  shinyOld = TRUE
} else {
  shinyOld = FALSE
}

shinyUI(
  
  fluidPage( 
  img(src = "12580.jpg", height = 80, width = 80,h3(strong("话务预测分析系统"))), 
  #h6(titlePanel("话务量预测分析系统")), 
  
  sidebarLayout(
   sidebarPanel( 
    
    selectInput("dataset", strong("选择数据类型:"),
                c("酒店话量预测" = "hotel",
                  "机票话量预测" = "ticket"
                )),
    br(),
    
    
    conditionalPanel(
      condition = "input.theTabs != 'hourly'",
      dateRangeInput(inputId = "dateRange",  
                   label = strong(h5("选择日期范围:")), 
                   start = Sys.Date() - 60,
                   end = Sys.Date() + 7,
                   max = Sys.Date() + 7 )
    ),
    
    conditionalPanel(
      condition = "input.theTabs != 'monthly'",      
      dateInput(inputId = "dateselect",  
                     label = strong(h5("选择日期:")), 
                     value = '2014-04-01',
                     max = Sys.Date() + 7),
      sliderInput(inputId ="timerange", 
                  label = strong(h5("选择时间范围:")),
                  min = 0, max = 23, value = c(0,23))
      
    ),
    
    br(),
    checkboxInput(inputId = "smoother",
                  label = "添加平滑趋势？",
                  value = FALSE),
    

    checkboxGroupInput(inputId = "domainShow",
                       label = strong("实际和预测选择:"),
                       choices = list("实际话量" = "Actual",
                                      "预测话量" = "Predict"),
                       selected = c(ifelse(shinyOld, "预测话量", "Actual"), "Predict")
    ),
    
    radioButtons(inputId = "outputType",
                 label = strong("输出统计指标:"),
                 choices = list("呼入量" = "visitors",
                                "接起率" = "bounceRate",
                                "排队呼损数" = "timeOnSite"))
  ),
  
  mainPanel(
    tabsetPanel(id ="theTabs",
                
                tabPanel("按日话务预测", 
                         textOutput("textDisplay"),
                         plotOutput("monthGraph"),
                         downloadButton("downloadData1","数据下载"),
                         tableOutput("view1"),
                         value = "monthly"),
                tabPanel("按时话务预测", 
                         textOutput("textDisplay2"),
                         plotOutput("hourGraph"),
                         downloadButton("downloadData2","数据下载"),
                         tableOutput("view2"),
                         value = "hourly"),
                tabPanel("按周话务趋势", 
                         textOutput("queryText"), value = "summary"),
                tabPanel("按地区话务地图", plotOutput("animateGraph"),
                         value = "animTab")

    )
  )
  )
))
