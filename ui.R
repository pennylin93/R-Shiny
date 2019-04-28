library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Checkmarx"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset",
                  label = "選擇客戶:",
                  choices = dir()
      )),
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("使用環境", tableOutput("table")),
                  tabPanel("專案等級", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("語言分佈", plotOutput("Lan")),
                  tabPanel("總弱點狀態", plotOutput("pie")),
                  tabPanel("登入方式", plotOutput("pieO")),
                  tabPanel("每週登入次數", plotOutput("hist"))
      )
    )
    
  )
)
