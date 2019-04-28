library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Checkmarx"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset",
                  label = "��ܫȤ�:",
                  choices = dir()
      )),
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("�ϥ�����", tableOutput("table")),
                  tabPanel("�M�׵���", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("�y�����G", plotOutput("Lan")),
                  tabPanel("�`�z�I���A", plotOutput("pie")),
                  tabPanel("�n�J�覡", plotOutput("pieO")),
                  tabPanel("�C�g�n�J����", plotOutput("hist"))
      )
    )
    
  )
)