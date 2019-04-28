library(shiny)
library(ggplot2)
#================
set.seed(929)
#���u�@�ؿ�
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
  
  
  ###��X�򥻸�T
  output$table <- renderTable({
    d()
  })
  
  
  #�{���X��� vs�z�I��
  output$plot <- renderPlot({
    total = PA()$ToVerify+PA()$NotExploitable+PA()$Confirmed+
      PA()$Urgent+PA()$ProposedNE
    #plot(data=PA(),x=PA()$LOC,y=total,
    #     mail="�{���X��� vs �z�I��",
    #   xlab="�{���X���",
    #    ylab="�z�I��",col.main="red", col.lab="blue")
    p <- ggplot(PA(), aes(x=total, y=PA()$LOC, label=PA()$ProjectId)) +
      geom_text(size=5)
    #�u��ܼнu
    p +expand_limits(y=c(0,1000000))+
      labs(x="�z�I�`��(�]�t�Ҧ����A)",y="�{���X�`���",title="�M�ת��A")
    
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(LPK())
  })
  
  #�y�����G  ������
  output$Lan <- renderPlot({
    d = unlist(strsplit(as.character(PA()$Languages),split=","))
    #Languages
    #�ǳƵe��
    Lan = data.frame(table(d))
    names(Lan) = c("�{���y��","�M�׼ƶq")
    ggplot(data=Lan, aes(x=�{���y��, y=�M�׼ƶq,fill=�{���y��))+
      # ���ebar plot
      geom_bar(stat = "identity")+
      labs(title="�{���y�����G")
  })
  
  
  
  #�z�I���A ����
  output$pie <- renderPlot({
    ca <- cbind(PA()$ToVerify,
                PA()$NotExploitable,
                PA()$Confirmed,
                PA()$Urgent,
                PA()$ProposedNE)
    #�p��z�I���A�ʤ���
    total = sum(PA()$ToVerify+PA()$NotExploitable+PA()$Confirmed+
                  PA()$Urgent+PA()$ProposedNE)
    TV = round(sum(PA()$ToVerify)/total,2)*100
    NE = round(sum(PA()$NotExploitable)/total,2)*100
    C = round(sum(PA()$Confirmed)/total,2)*100
    U = round(sum(PA()$Urgent)/total,2)*100
    PNE = round(sum(PA()$ProposedNE)/total,2)*100
    
    ##���X��r�ꤺ
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
    pie +theme_void()+labs(title="�z�I���A")+
      #pie(sumva,label="",main="�z�I���I���Ū��A")
      scale_fill_discrete(breaks=c("ToVerify","NotExploitable","Confirmed","Urgent","ProposedNE"),
                          labels=c(TV_word, NE_word, C_word,U_word,PNE_word))
    
  })
  
  ##�n�J�覡
  output$pieO <- renderPlot({
    pie(table(LPK()$Origin),main="�n�J�覡")
  })
  
  ##�C�g�n�J����
  output$hist <- renderPlot({
    #options(scipen = 999)
    wek = tapply(LPK()$NumberOfLogins,as.factor(LPK()$WEEK),sum)
    data_wek = data.frame(wek)
    cdata = cbind(rownames(data_wek),data_wek)
    names(cdata)=c("�ɶ�","�n�J����")
    ggplot(data=cdata, aes(x=�ɶ�, y=�n�J����,fill=�ɶ�))+
      expand_limits(y=c(0,120))+
      # ���ebar plot
      geom_bar(stat = "identity")+
      labs(title="�C�g�n�J����")+guides(fill=FALSE)+
      theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))
  })
  
}