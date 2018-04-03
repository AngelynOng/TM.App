rm(list=ls())
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,readxl,lettercase,shiny)  


data_2<-read_xls("./Data/Abstracts.xls",sheet=1)%>%as.tibble()
data_1<-read_xlsx("./Data/topics2.xlsx",sheet=1)%>%as.tibble()
#data_3<-read_xlsx("Results.xlsx",sheet=3)%>%as.tibble()
data_2$RecordNum<-as.integer(data_2$RecordNum)
data_2$Year<-as.integer(data_2$Year)
# data_2$Title<-str_lower_case(data_2$Title)
# data_2$Title<-str_title_case(data_2$Title)
# 
# categories<-data_1%>%filter(Categories!="")%>%select(Categories)%>%unique()
topics<-data_1%>%select(`Suggested theme`,Topic)%>%unique()
# terms<-data_1%>%filter(`Suggested Terms`!="")%>%select(Topic,`Suggested Terms`)%>%unique()
# 

topicList<-list()
for (i in 1:nrow(topics)){
  topicList[[i]]<-topics$Topic[i]
  val<-paste0(topics$Topic[i],":",topics$`Suggested theme`[i])
  names(topicList)[[i]]<-val
}


yearList<-list()
years<-min(data_2$Year):max(data_2$Year)

for (i in 1:length(years) ){
  yearList[[i]]<-years[i]
  names(yearList)[[i]]<-years[i]
}

library(shiny)

# Define UI for application that draws a histogram
ui=shinyUI(fluidPage(
  
  # Application title
  titlePanel("Topic Modelling"),
  
  # # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("topic", label = h4("Topics"),
                  choices = topicList, 
                  selected = 1),
      sliderInput("topGamma", label = h4("Top records"), min = 1, 
                  max = 100, value = 5),
      tableOutput('table2')
      
      ,
      downloadLink('downloadData', 'Download')
      
    ),
    mainPanel(
      # Show a plot of the generated distribution
      tableOutput('table')
    )
  )
)
)



library(shiny)

server=shinyServer(function(input, output) {
  
  outputTA<-reactive(data_2%>%filter(Topic==input$topic)%>%top_n(input$topGamma,gamma)%>%
                       select(RecordNum,Title,Year,Journal,gamma))
  
  outputT<-reactive(data_2%>%filter(Topic==input$topic)%>%top_n(input$topGamma,gamma)%>%
                      select(RecordNum,Title,Year,Journal,gamma))
  
  
  output$table <- renderTable({
    outputT()  
    
  })
  
  outputTerms<-reactive(data_1%>%filter(Topic==input$topic)%>%
                          select(`Suggested term`)%>%unique()%>%
                          na.omit())
  
  output$table2 <- renderTable({
    outputTerms()
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-',outputTerms(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(outputTA(), con,row.names = FALSE)
    }
  )
  
})


shinyApp(ui,server)


