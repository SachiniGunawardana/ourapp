#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shiny)
library(plyr)
library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)
library(e1071) 
library("caret")
library(SnowballC)

#Header
header <- dashboardHeader(title = "YouTube Trending Videos Analysis ",titleWidth=350)


#side Bar
sidebar <- dashboardSidebar(width=350,collapsed  = TRUE,
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
   # menuSubItem("Bar Chart", tabName = "Bar Chart", 
               # icon = icon("bar-chart-o",lib = "font-awesome"))),
    menuItem("Wordcloud", tabName = "Wordcloud", icon = icon("cloud",lib = "font-awesome")),
    menuItem("Authors", tabName = "Authors", icon = icon("users",lib = "font-awesome"))
    
    
  )
)


#body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            fluidRow(
              tabBox(
                title = h3("Analysis"),
                id = "analise", height = "550px",width = "1000px",
                tabPanel(h4("Charts") ,
                         tabBox(
                           title = "",
                           id = "coudType", height = "150px",width = "1000px",
                           tabPanel("Bar Chart",plotOutput("barplot"),icon = icon("bar-chart-o",lib = "font-awesome")
                                    
                           ),
                           tabPanel("Pie Chart",plotOutput("piechart"),icon = icon(" fa-pie-chart",lib = "font-awesome")
                                    
                           )
                           
                         )
                ),
                tabPanel(h4("Accuracy"),
                         fluidRow(
                           box(
                             title = h3(" ACCURACY TABLE WITH PRDEICTED AND TEST LABELS  :"), solidHeader = TRUE,collapsible =TRUE, background = "navy", tableOutput('table')),
                            box(
                              title = h3(" ACCURACY IN PERCENTAGE :"), solidHeader = TRUE,collapsible =TRUE, background = "navy", h4(textOutput('Accuracy'))                              #textOutput("text1", h4("Enter your video's comment"))
                            )
                           
                         )
                         
                )
              )
            )
            
           
            
            
            
    ),
    
      tabItem(tabName = "Wordcloud",
            
              fluidRow(
                box(
                  title = h3("Check The Category of Your Comment "), solidHeader = TRUE,collapsible = FALSE, background = "navy",
                  height=300,
                  textInput("text", h4("Enter your video's comment")),
                  actionButton("update",h4("Submit"),width = 100)
                #  p("Click on the Submit button to submit the comments")
                  
                ),
                box(
                  title = h3(" Category of Your Comment :"), solidHeader = TRUE,collapsible =TRUE, background = "navy",h4(textOutput('predicted'))
                  #textOutput("text1", h4("Enter your video's comment"))
                )
                
                
              ),
              fluidRow(
                box(
                  title = h3("Change The Wordcloud"), solidHeader = TRUE,collapsible =FALSE ,
                  sliderInput("freq",h4("Minimam Frequency"),min=3,max=20,value=15),
                  sliderInput("max",h4("Maximum number of words"), min = 10, max = 30, value = 50)
                  
                ),
                box(
                  title = h3("Wordcloud"), solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,background = "navy",plotOutput("plot")
                  
                )
              )
              
      ),
    tabItem(tabName = "Authors",
            fluidRow(
              ## Coloumn 01 - User 01 Details ##
              column(
                width = 4,
                box(
                  width = NULL,
                  icon("user", "fa-3x"),
                  h3("  M.P.EDIRISINGHE"),
                  h3("    142054")
                )
              ),
              ## Coloumn 01 - End ##
              ## Coloumn 02 - User 02 Details ##
              column(
                width = 4,
                box(
                  width = NULL,
                  icon("user", "fa-3x"),
                  h3("  K.G.U.D.JAYAKODY"),
                  h3("    142064")
                )
              ),
              ## Coloumn 02 - End ##
              ## Coloumn 03 - User 03 Details ##
              column(
                width = 4,
                box(
                  width = NULL,
                  icon("user", "fa-3x"),
                  h3("  L.A.S.GUNAWARDANA"),
                  h3("    142140")
                )
              )
              ## Coloumn 03 - End ##
            )
            )
    
  )
    
  
)



ui <- dashboardPage(header, sidebar, body)


# Define server 
server <- function(input, output) {
  ## set Working Directory
  setwd("D:/cloud/project/ourapp")
  
  ## load Data set
  text1<-read.csv("modified.csv",stringsAsFactors = FALSE,encoding = "UTF-8")
  data1<-read.csv("modified _rand.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
  data<-read.csv("modified_rand - Copy.csv",encoding = "UTF-8") 
  data<-data[,c(3,6)]
  
  ##Graphs##
  
  #plot bar chart
  output$barplot<-renderPlot({
    video<-unique(text1$video_id)
    Vcategory<-c("vlog","nigger","iphone","iphone","movie","politics","iphone","vlog","movie","social conversation","funny","game","social conversation","magic show","iphone","funny","korean","food","movie","music","social conversation","music","vlog","music","game")
    table<-data.frame(video,Vcategory)
    freq<-count(table,'Vcategory')
    ggplot(freq,aes(x=Vcategory,y=freq))+stat_summary(fun.y = mean,geom = "bar",fill=c("red","orange","yellow","chartreuse","green","springgreen","cyan1","cyan4","blue","blueviolet","deeppink","deeppink3"),show.legend = TRUE)+xlab("categories")+ylab("No of videos")+ggtitle("Category vs No of videos")+  theme( plot.title = element_text(color="red", size=20, face="bold.italic",hjust=0.5), axis.title.x = element_text(color="blue", size=14, face="bold"), axis.title.y = element_text(color="#993333", size=14, face="bold") )
  }
   )
  
  
  #plot pie chart
  output$piechart<-renderPlot({
    video<-unique(text1$video_id)
    Vcategory<-c("vlog","nigger","iphone","iphone","movie","politics","iphone","vlog","movie","social conversation","funny","game","social conversation","magic show","iphone","funny","korean","food","movie","music","social conversation","music","vlog","music","game")
    table<-data.frame(video,Vcategory)
    freq<-count(table,'Vcategory')
    labels <- c("food","funny","game","iphone","korean","magic show","movie","music","nigger","politics","social conversation
","vlog")
    pct <- round(freq$freq/sum(freq$freq)*100)
    lbls <- paste(labels, pct) # add percents to labels
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    pie(freq$freq,labels = lbls, col=rainbow(length(lbls)),main=" Video Categories in Youtube trending list ")
  }
  )
  
  ##Functions###
  
  #cleaning
  clean<-function(data){
    corpus = VCorpus(VectorSource(data))
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeNumbers)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, removeWords, stopwords())
    corpus = tm_map(corpus, stemDocument)
    corpus = tm_map(corpus, stripWhitespace)
    return(corpus)
  }
  
  #create DTM
  createDTM<-function(Corpus){
 
    myDTM = DocumentTermMatrix(Corpus,control = list(minWordLength = 1))
    m = as.matrix(myDTM)
    
    #find the most frequent words
    sort(colSums(m), decreasing = TRUE)
    #d<-data.frame(word=names(v),freq=v)
  }
  
  ##create classifier##
  createClassifier<-function(comment){
    # Creating the Bag of Words model
    dtm = DocumentTermMatrix(clean(comment))
    dtm = removeSparseTerms(dtm, 0.999)
    
    #partitioning dataset into training and test sets
    dtm_train <- dtm[1:2000, ] #training set
    dtm_test <- dtm[2001:2498, ] #test set
    
    train_labels <- data[1:2000,]$category # categories in training set 
    test_labels <- data[2001:2498, ]$category #categories in test set
    
    #creating indicator features for frequent words from dtm_train
    #Take a document term matrix and returns a character vector containing the words appearing at least 5 times.
    freq_words <- findFreqTerms(dtm_train, 5)
    
    #Filter DTM to include only the terms appearing in a specified vector.
    dtm_freq_train<- dtm_train[ , freq_words] 
    dtm_freq_test <- dtm_test[ , freq_words]
    
    # change to a categorical variable that simply indicates yes or no depending on whether the  word appears at all.
    convert_counts <- function(x) { x <- ifelse(x > 0, "Yes", "No") } 
    train <- apply(dtm_freq_train, MARGIN = 2, convert_counts) 
    test <- apply(dtm_freq_test, MARGIN = 2, convert_counts) 
    
    #train a model  
    classifier <- naiveBayes(train, train_labels)
    #Evaluate model performance 
    test_pred <- predict(classifier, test)
    
    #library(gmodels)
    ## Warning: package 'gmodels' was built under R version 3.2.5
    
    #CrossTable(test_pred, test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))
    conf.mat<-table(test_pred,test_labels)
    
    output$table <- renderTable(table(test_pred,test_labels))
    # output$matrix<-print(confusionMatrix(table(test_pred,test_labels)), mode = confusionMatrix(table(test_pred,test_labels))$everything, digits = max(3,
    #                                                     getOption("digits") - 3), printStats = TRUE)
    accuracy <- sum(diag(conf.mat)) / 498 * 100
    output$Accuracy<-renderPrint(accuracy)
    return(classifier)
  }
  
  ##testing
  TestClassifier<-function(comment){
    # Creating the Bag of Words model
    dtm = DocumentTermMatrix(clean(comment))
    dtm = removeSparseTerms(dtm, 0.999)
    
    #partitioning dataset into training and test sets
    dtm_train <- dtm[1:2000, ] #training set
    dtm_test <- dtm[2001:2499, ] #test set
    
    train_labels <- data[1:2000,]$category # categories in training set 
    test_labels <- data[2001:2498, ]$category #categories in test set
    
    #creating indicator features for frequent words from dtm_train
    #Take a document term matrix and returns a character vector containing the words appearing at least 5 times.
    freq_words <- findFreqTerms(dtm_train, 5)
    
    #Filter DTM to include only the terms appearing in a specified vector.
    #dtm_freq_train<- dtm_train[ , freq_words] 
    dtm_freq_test <- dtm_test[ , freq_words]
    
    # change to a categorical variable that simply indicates yes or no depending on whether the  word appears at all.
    convert_counts <- function(x) { x <- ifelse(x > 0, "Yes", "No") } 
    #train <- apply(dtm_freq_train, MARGIN = 2, convert_counts) 
    test <- apply(dtm_freq_test, MARGIN = 2, convert_counts) 
    
    #train a model  
    # classifier <- naiveBayes(train, train_labels)
    NBClassifier<-createClassifier(data$comment_text)
    #Evaluate model performance 
    test_pred <- predict(NBClassifier, test)
    
    
    output$predicted<-renderPrint(test_pred[499])
  }
  
  
  #train model
  createClassifier(data$comment_text)
 
   
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        
        
        f<-data.frame(data1$comment_text,stringsAsFactors = FALSE)
        f$data1.comment_text[2499]=input$text
        TestClassifier(f$data1.comment_text)
        createDTM(clean(input$text))
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

