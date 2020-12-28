#Building Recommender System

library(shiny)
library(plotly)
library(arules)
library(igraph)
library(arulesViz)


server <- function(input, output) {
  
#get transaction object
  trans <- reactive({
    data <- input$datafile
    transaction <- read.transactions(file = data$datapath, format = "single", 
                                          header=T,
                                          sep = ",",
                                          cols = c("order_id", "product_id"),
                                          quote = "")
    return(transaction)
  })

#get transaction dataframe
  trans.df <- reactive({
    data <- input$datafile
    trans.df <- read.csv(data$datapath)
    return(trans.df)
  })

#get apriori rules
  get.rules <- reactive({
    transaction <- trans()
    support    <- input$Support
    confidence<-input$Confidence
    
    parameters = list(
      support = support,
      confidence=confidence,
      minlen  = 2,  
      maxlen  = 2, 
      target  = "rules")
    
    original.rules <- apriori(transaction, parameter = parameters)
    others <- interestMeasure(original.rules, transactions = transaction)
    rules.df <- cbind(data.frame(rules = labels(original.rules), original.rules@quality),
                      others[,c('conviction','leverage')])
    top.rules <- head(rules.df[order(-rules.df$leverage),],10)#get top 10 rules
    return(top.rules)
  })

#output transaction dataframe
  output$transactions <- renderDataTable({
    trans.df()
  })
  
#output rules
  output$rules <- renderDataTable({
    get.rules()
  })

#output community relationship member
  output$community <- renderPlot({
    final.rules<-get.rules()
    final.rules$rules<-as.character(final.rules$rules)
    final.items<-separate(data=final.rules,col = rules,into = c("items1","items2"),sep = "=>")
    edges<-final.items[,c("items1","items2","support")]
    names(edges)<-c("from","to","weight")
    graph <- graph_from_data_frame(edges)
    community <- walktrap.community(graph)
    plot(community,graph)
  })

#output itemfrequency hist
  output$itemfrequencyhist<-renderPlot({
    itemFrequencyPlot(trans(),topN=25)
  })
  
}



ui <- fluidPage(
  navbarPage("Recommender System",
             tabPanel("Transactions"
                      , fileInput("datafile", "select transactions csv file",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
                      , dataTableOutput("transactions")
                      , plotOutput("itemfrequencyhist")),
             tabPanel("Rules",
                      sliderInput("Support", "Support threshold:", min = 0.01, max = 1.0, value = 0.01),
                      sliderInput("Confidence", "Support threshold:", min = 0.05, max = 1.0, value = 0.05),
                      dataTableOutput("rules")),
             tabPanel("Community"
                      ,plotOutput("community")))
)


shinyApp(ui = ui, server = server)





