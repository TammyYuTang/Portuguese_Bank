library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Science Dashboard for bank customer"),
  
  # Set up sidebar for entering customer's information
  sidebarLayout(
    sidebarPanel(
      helpText("Please input the new customer details here and select the technique on the tabpanel for prediction.
               Then, It will show the prediction and additional information for the customer."),
      br(),
      wellPanel(
        numericInput("age","Enter age(16~100):", 25, min = 16, max = 100),
        selectInput("job","Select type of job",c("admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown")),
        selectInput("marital","Select martial status",c("divorced","married","single","unknown")),
        selectInput("education","Select the level of education",c("basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown")),
        selectInput("default","Has credit in default?",c("yes","no","unknown")),
        selectInput("housing","Has housing loan?",c("yes","no","unknown")),
        selectInput("loan","Has personal loan?",c("yes","no","unknown")),
        selectInput("contact","Contact communication type",c("cellular","telephone")),
        selectInput("month","Last contact month of year",c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec")),
        selectInput("day_of_week","Last contact day of the week",c("mon","tue","wed","thu","fri")),
        numericInput("campaign","Number of contacts performed during this campaign and for this client(0~60)", 1, min = 0, max = 60),
        numericInput("pdays","Number of days that passed by after the client was last contacted from a previous campaign(0~999)", 0, min = 0, max = 999),
        numericInput("previous","Number of contacts performed before this campaign and for this client(0~10)", 0, min = 0, max = 10),
        selectInput("poutcome","Outcome of the previous marketing campaign",c("success","failure","nonexistent")),
        numericInput("emp.var.rate","Employment variation rate(quarterly)(-3.5~2)", 1.1, min = -3.5, max = 2),
        numericInput("cons.price.idx","Consumer price index(monthly)(90~100)", 93.75, min = 90, max = 100),
        numericInput("cons.conf.idx","Consumer confidence index(monthly)(-52~-26)", -41.8, min = -52, max = -26),
        numericInput("euribor3m","Euribor 3 month rate(daily)(0.5~5.5)", 3.6, min = 0.5, max = 5.5),
        numericInput("nr.employed","Number of employees(quarterly)(4950~5250)", 5191, min = 4950, max = 5250),
        actionButton("submit","Submit")
      )
      
    ),
    
    # Set up the models
    mainPanel(
      
       h2(uiOutput("text")),
       h5(uiOutput("table")),
       
       tabsetPanel(type="tab", 
              tabPanel("Decision Trees", plotOutput("treedata")),
              
              tabPanel("Logistic Regression", plotOutput("logdata")),
              
              tabPanel("Knn model",verbatimTextOutput("knndata")), 
              
              tabPanel("Customer Clustering", plotOutput("cludata")),
              
              tabPanel("Tree Respones", verbatimTextOutput("treeresult")),
              
              tabPanel("Log Respones", verbatimTextOutput("logresult")),
              
              tabPanel("Knn Respones", verbatimTextOutput("knnresult")),
              
              tabPanel("Classification", verbatimTextOutput("cluresult")),
              
        h3(uiOutput("text2")),
        h4(uiOutput("text3"))
      
              
            
     )
    )
   )
))