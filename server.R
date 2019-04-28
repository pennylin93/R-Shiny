library(shiny)
library(ggplot2)
#================
set.seed(929)
#эヘ魁
setwd("D:\\0_Work\\Checkmax\\CU_report\\STAT")
#getwd()
dir()

#paste0("I love",name, "yaya")

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  d <- reactive({
    read.csv(paste0("D:\\0_Work\\Checkmax\\CU_report\\STAT\\",input$dataset,"\\ConsumptionDetails.csv"), header=T, sep=",")
  })
  LPK <- reactive({
    read.csv(paste0("D:\\0_Work\\Checkmax\\CU_report\\STAT\\",input$dataset,"\\LoginsPerWeek.csv"), header=T, sep=",")
  })
  PA <- reactive({
    data.frame(read.csv(paste0("D:\\0_Work\\Checkmax\\CU_report\\STAT\\",input$dataset,"\\ProjectActivity.csv"), header=T, sep=","))
  })
  RCBS <- reactive({
    data.frame(read.csv(paste0("D:\\0_Work\\Checkmax\\CU_report\\STAT\\",input$dataset,"\\ResultCountingBySeverity.csv"), header=T, sep=","))
  })
  UO <- reactive({
    data.frame(read.csv(paste0("D:\\0_Work\\Checkmax\\CU_report\\STAT\\",input$dataset,"\\Users_Origin.csv"), header=T, sep=","))
  })
  URR <- reactive({
    data.frame(read.csv(paste0("D:\\0_Work\\Checkmax\\CU_report\\STAT\\",input$dataset,"\\Users_ResultReviewed.csv"), header=T, sep=","))
  })
  
  
  ###块膀セ戈癟
  output$table <- renderTable({
    d()
  })
  
  
  #祘Α絏︽计 vs畓翴计
  output$plot <- renderPlot({
    total = PA()$ToVerify+PA()$NotExploitable+PA()$Confirmed+
      PA()$Urgent+PA()$ProposedNE
    #plot(data=PA(),x=PA()$LOC,y=total,
    #     mail="祘Α絏︽计 vs 畓翴计",
    #   xlab="祘Α絏︽计",
    #    ylab="畓翴计",col.main="red", col.lab="blue")
    p <- ggplot(PA(), aes(x=total, y=PA()$LOC, label=PA()$ProjectId)) +
      geom_text(size=5)
    #陪ボ夹絬
    p +expand_limits(y=c(0,1000000))+
      labs(x="畓翴羆计(┮Τ篈)",y="祘Α絏羆︽计",title="盡篈")
    
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(LPK())
  })
  
  #粂ēだ  兵瓜
  output$Lan <- renderPlot({
    d = unlist(strsplit(as.character(PA()$Languages),split=","))
    #Languages
    #非称礶ガ
    Lan = data.frame(table(d))
    names(Lan) = c("祘Α粂ē","盡计秖")
    ggplot(data=Lan, aes(x=祘Α粂ē, y=盡计秖,fill=祘Α粂ē))+
      # 礶bar plot
      geom_bar(stat = "identity")+
      labs(title="祘Α粂ēだ")
  })
  
  
  
  #畓翴篈 蛾绘瓜
  output$pie <- renderPlot({
    ca <- cbind(PA()$ToVerify,
                PA()$NotExploitable,
                PA()$Confirmed,
                PA()$Urgent,
                PA()$ProposedNE)
    #璸衡畓翴篈κだゑ
    total = sum(PA()$ToVerify+PA()$NotExploitable+PA()$Confirmed+
                  PA()$Urgent+PA()$ProposedNE)
    TV = round(sum(PA()$ToVerify)/total,2)*100
    NE = round(sum(PA()$NotExploitable)/total,2)*100
    C = round(sum(PA()$Confirmed)/total,2)*100
    U = round(sum(PA()$Urgent)/total,2)*100
    PNE = round(sum(PA()$ProposedNE)/total,2)*100
    
    ##挡﹃ず
    TV_word = paste("ToVerify",":",TV,"%")
    NE_word = paste("NotExploitable",":",NE,"%")
    C_word = paste("Confirmed",":",C,"%")
    U_word = paste("Urgent",":",U,"%")
    PNE_word = paste("ProposedNE",":",PNE,"%")
    
    sumva = apply(ca,2,sum)
    df <- data.frame(
      group = c("ToVerify","NotExploitable","Confirmed","Urgent","ProposedNE"),
      value = sumva
    )
    bp<- ggplot(df, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")
    
    pie <- bp + coord_polar("y", start=0)
    #pie + scale_fill_manual(values=c("red", "yellow", "green","blue","black"))
    pie +theme_void()+labs(title="畓翴篈")+
      #pie(sumva,label="",main="畓翴繧单篈")
      scale_fill_discrete(breaks=c("ToVerify","NotExploitable","Confirmed","Urgent","ProposedNE"),
                          labels=c(TV_word, NE_word, C_word,U_word,PNE_word))
    
  })
  
  ##祅よΑ
  output$pieO <- renderPlot({
    pie(table(LPK()$Origin),main="祅よΑ")
  })
  
  ##–秅祅Ω计
  output$hist <- renderPlot({
    #options(scipen = 999)
    wek = tapply(LPK()$NumberOfLogins,as.factor(LPK()$WEEK),sum)
    data_wek = data.frame(wek)
    cdata = cbind(rownames(data_wek),data_wek)
    names(cdata)=c("丁","祅Ω计")
    ggplot(data=cdata, aes(x=丁, y=祅Ω计,fill=丁))+
      expand_limits(y=c(0,120))+
      # 礶bar plot
      geom_bar(stat = "identity")+
      labs(title="–秅祅Ω计")+guides(fill=FALSE)+
      theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))
  })
  
}