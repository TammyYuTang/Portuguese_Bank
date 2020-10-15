# Import the dataset from Desktop
dir <- "C:/Users/Tammy Lin/Desktop"
setwd(dir)
filedata <- read.csv(file = 'Portuguese_Bank.csv', header = TRUE)

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input,output){
    
    bank <- filedata
    bank[,c(2:10,14,20)] <- lapply(bank[,c(2:10,14,20)],as.factor)
    bank[,c(1,11:13,15:19)] <- lapply(bank[,c(1,11:13,15:19)],as.numeric)
  
  output$treedata <- renderPlot({
    
    # Make the decision tree 
    library(party)
    
    ct = ctree(Y~., data = bank,controls=ctree_control(maxdepth=3))
    
    # simpler version of plot
    plot(ct, type="simple", sub = "The probability of responding yes",           # no terminal plots
         inner_panel=node_inner(ct,
                                abbreviate = FALSE,            # short variable names
                                pval = FALSE,                 # no p-values
                                id = FALSE),                  # no id of node
         terminal_panel=node_terminal(ct, 
                                      abbreviate = TRUE,
                                      digits = 1,                   # few digits on numbers
                                      fill = c("white"),            # make box white not grey
                                      id = FALSE)
    )
    
    
  })
  
  Newdata = reactive({
    if (input$submit > 0) {
      df <- data.frame(age=input$age, job=input$job, marital=input$marital, education=input$education,
                       default=input$default, housing=input$housing, loan=input$loan,
                       contact=input$contact, month=input$month, day_of_week=input$day_of_week,
                       campaign=input$campaign, pdays=input$pdays, previous=input$previous,
                       poutcome=input$poutcome, emp.var.rate=input$emp.var.rate, 
                       cons.price.idx=input$cons.price.idx, cons.conf.idx=input$cons.conf.idx,
                       euribor3m=input$euribor3m, nr.employed=input$nr.employed,y="")
      return(list(df=df))
    }
  })
  

  output$logdata <- renderPlot({
    # Make the logistic regression
    regression <- function(formula, input_data)
    {
      glm(formula,family=binomial,data=bank)
    }
    a <- formula(Y~.)
    bank2 <- bank[,c(15:17,19,20)]
    
    library(caret)
    library(pROC)
    
    logistic <- glm(Y~., data = bank2, family = binomial("logit"))
    logistic.roc <- roc(bank2$Y, predict(logistic, newdata = bank2,type="response")) 
    par(mfrow=c(1,1)) 
    
    plot.roc(logistic.roc,print.auc=TRUE)
    
  })
  
  output$knndata <- renderPrint({
    # Make the knn model
    
    datanorm <- function(x) {(x-min(x))/(max(x)-min(x))}
    bank2 <- bank
    bank2[,c(2:10,14)] <- lapply(bank2[,c(2:10,14)],as.numeric)
    bank2[,c(1,11:13,15:19)] <- lapply(bank2[,c(1,11:13,15:19)], datanorm)
    
    model <- train(Y~., data=bank2, method='knn', tuneGrid=expand.grid(.k=13:16), metric='Accuracy', trControl=trainControl(method='repeatedcv', number=10, repeats=3))
    
    model
    confusionMatrix(model)
    
  })
  
  output$cludata <- renderPlot({
    # Do the clustering groups about the customers
    library(e1071)
    datanorm <- function(x) {(x-min(x))/(max(x)-min(x))}
    bank2 <- bank
    bank2[,c(1,11:13,15:19)] <- lapply(bank2[,c(1,11:13,15:19)], datanorm)
    
    # Rename some categorical data for reducing the level of factor
    bank2$job <- sub("admin.","employed",bank2$job)
    bank2$job <- sub("blue-collar","employed",bank2$job)
    bank2$job <- sub("entrepreneur","employed",bank2$job)
    bank2$job <- sub("housemaid","employed",bank2$job)
    bank2$job <- sub("management","employed",bank2$job)
    bank2$job <- sub("retired","unemployed",bank2$job)
    bank2$job <- sub("self-employed","employed",bank2$job)
    bank2$job <- sub("services","employed",bank2$job)
    bank2$job <- sub("student","unemployed",bank2$job)
    bank2$job <- sub("technician","employed",bank2$job)
    bank2$job <- as.factor(bank2$job)
    
    # redefine the categories of education to decrease the number of levels
    bank2$education <- sub("basic.4y","educated",bank2$education)
    bank2$education <- sub("basic.6y","educated",bank2$education)
    bank2$education <- sub("basic.9y","educated",bank2$education)
    bank2$education <- sub("high.school","educated",bank2$education)
    bank2$education <- sub("professional.course","well-educated",bank2$education)
    bank2$education <- sub("university.degree","well-educated",bank2$education)
    bank2$education <- as.factor(bank2$education)
    
    # redefine the categories of month(quarterly) to decrease the number of levels
    bank2$month <- sub("jan","Q1",bank2$month)
    bank2$month <- sub("feb","Q1",bank2$month)
    bank2$month <- sub("mar","Q1",bank2$month)
    bank2$month <- sub("apr","Q2",bank2$month)
    bank2$month <- sub("may","Q2",bank2$month)
    bank2$month <- sub("jun","Q2",bank2$month)
    bank2$month <- sub("jul","Q3",bank2$month)
    bank2$month <- sub("aug","Q3",bank2$month)
    bank2$month <- sub("sep","Q3",bank2$month)
    bank2$month <- sub("oct","Q4",bank2$month)
    bank2$month <- sub("nov","Q4",bank2$month)
    bank2$month <- sub("dec","Q4",bank2$month)
    bank2$month <- as.factor(bank2$month)
    
    bank2 <- bank2[,c(-(11:19))]
    bank2[,c(2:10)] <- lapply(bank2[,c(2:10)],as.numeric)
    
    set.seed(41188)
    idx <- sample(1:dim(bank2)[1], 41188)
    sbank <- bank2[idx,]
    sbank$y <- NULL
    kc <- kmeans(sbank,5)
    
    # Plot the sample of each clusterning group
    group1 <- sbank[kc$cluster==1,]
    group2 <- sbank[kc$cluster==2,]
    group3 <- sbank[kc$cluster==3,]
    group4 <- sbank[kc$cluster==4,]
    group5 <- sbank[kc$cluster==5,]
    
    set.seed(35)
    idx1 <- sample(1:dim(group1)[1], 35)
    s1 <- group1[idx1,]
    
    set.seed(31)
    idx2 <- sample(1:dim(group2)[1], 31)
    s2 <- group2[idx2,]
    
    set.seed(45)
    idx3 <- sample(1:dim(group3)[1], 45)
    s3 <- group3[idx3,]
    
    set.seed(50)
    idx4 <- sample(1:dim(group4)[1], 50)
    s4 <- group4[idx4,]
    
    set.seed(43)
    idx5 <- sample(1:dim(group5)[1], 43)
    s5 <- group5[idx5,]
    
    ssbank = rbind(s1,s2,s3,s4,s5)
    kc2 <- kmeans(ssbank,5)
    library(cluster)
    
    clusplot(ssbank, kc2$cluster, color = TRUE)
    
  })
  
  
  
  output$text <- renderText({
    paste("Customer Information")
  })
  
  output$table <- renderTable({
    
    if (is.null(Newdata())) {return()}
   
     print(Newdata()$df)
    
  }, 'include.rownames' = FALSE, 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  )
 
  output$treeresult <- renderPrint({
    
    # Make the tree prediction for new customers
    
    if (is.null(Newdata())) {return()}
    
    bank[, 2][bank[, 2] == "unknown"] <- NA
    bank[, 3][bank[, 3] == "unknown"] <- NA
    bank[, 4][bank[, 4] == "unknown"] <- NA
    bank[, 5][bank[, 5] == "unknown"] <- NA
    bank[, 6][bank[, 6] == "unknown"] <- NA
    bank[, 7][bank[, 7] == "unknown"] <- NA
    bank[, 12][bank[, 12] == 999] <- NA
    
    library(party)
    
    ct = ctree(Y ~.,data=bank)
    a = rbind(bank,Newdata()$df)
    test <- a[41189,]
    result <- predict(ct,newdata=test, type="prob")
    print(result)
    
  }
  
  ) 
  
  output$logresult <- renderPrint({
    
    #Make the logistic regression prediction for new customers
    
    if (is.null(Newdata())) {return()}
    
    regression <- function(formula, input_data)
    {
     glm(formula,family=binomial,data=bank)
    }
    a <- formula(Y~.)
  
    library(caret)
    library(pROC)
  
    logistic <- glm(Y~., data = bank, family = binomial("logit"))
    result2 <- predict(logistic,newdata=Newdata()$df, type="response")
    print(result2)
    
  })
  
  output$knnresult <- renderPrint({
    
    #Make the knn prediction for new customers
    if (is.null(Newdata())) {return()}
    
    datanorm <- function(x) {(x-min(x))/(max(x)-min(x))}
    bank2 <- bank
    bank2[,c(2:10,14)] <- lapply(bank2[,c(2:10,14)],as.numeric)
    bank2[,c(1,11:13,15:19)] <- lapply(bank2[,c(1,11:13,15:19)], datanorm)
    
    model <- train(Y~., data=bank2, method='knn', tuneGrid=expand.grid(.k=13:16), metric='Accuracy', trControl=trainControl(method='repeatedcv', number=10, repeats=3))
    
    kdata = rbind(bank,Newdata()$df)
    kdata[,c(2:10,14)] <- lapply(kdata[,c(2:10,14)],as.numeric)
    kdata[,c(1,11:13,15:19)] <- lapply(kdata[,c(1,11:13,15:19)], datanorm)
    
    k <- kdata[41189,]
    pred <- predict(model, newdata=k)
    pred <- as.numeric(pred)
    testp <- pred
    
    print(testp)
    
  })
  
  output$cluresult <- renderPrint({
    
    # Make the prediction of the classification for new customers
    if (is.null(Newdata())) {return()}
    
    library(e1071)
    datanorm <- function(x) {(x-min(x))/(max(x)-min(x))}
    bank2 <- bank[,c(-(11:19))]
    
    # redefine the categories of job to decrease the number of levels
    bank2$job <- sub("admin.","employed",bank2$job)
    bank2$job <- sub("blue-collar","employed",bank2$job)
    bank2$job <- sub("entrepreneur","employed",bank2$job)
    bank2$job <- sub("housemaid","employed",bank2$job)
    bank2$job <- sub("management","employed",bank2$job)
    bank2$job <- sub("retired","unemployed",bank2$job)
    bank2$job <- sub("self-employed","employed",bank2$job)
    bank2$job <- sub("services","employed",bank2$job)
    bank2$job <- sub("student","unemployed",bank2$job)
    bank2$job <- sub("technician","employed",bank2$job)
    bank2$job <- as.factor(bank2$job)
    
    # redefine the categories of education to decrease the number of levels
    bank2$education <- sub("basic.4y","educated",bank2$education)
    bank2$education <- sub("basic.6y","educated",bank2$education)
    bank2$education <- sub("basic.9y","educated",bank2$education)
    bank2$education <- sub("high.school","educated",bank2$education)
    bank2$education <- sub("professional.course","well-educated",bank2$education)
    bank2$education <- sub("university.degree","well-educated",bank2$education)
    bank2$education <- as.factor(bank2$education)
    
    # redefine the categories of month(quarterly) to decrease the number of levels
    # redefine the categories of month(quarterly) to decrease the number of levels
    bank2$month <- sub("jan","Q1",bank2$month)
    bank2$month <- sub("feb","Q1",bank2$month)
    bank2$month <- sub("mar","Q1",bank2$month)
    bank2$month <- sub("apr","Q2",bank2$month)
    bank2$month <- sub("may","Q2",bank2$month)
    bank2$month <- sub("jun","Q2",bank2$month)
    bank2$month <- sub("jul","Q3",bank2$month)
    bank2$month <- sub("aug","Q3",bank2$month)
    bank2$month <- sub("sep","Q3",bank2$month)
    bank2$month <- sub("oct","Q4",bank2$month)
    bank2$month <- sub("nov","Q4",bank2$month)
    bank2$month <- sub("dec","Q4",bank2$month)
    bank2$month <- as.factor(bank2$month)
    
    # convert the factor data into numeric data except for y
    bank2[,c(2:10)] <- lapply(bank2[,c(2:10)],as.numeric)
    bank2[,1]=datanorm(bank2[,1])
    
    # Use the k-means method to do the clustering
    set.seed(41188)
    idx <- sample(1:dim(bank2)[1], 41188)
    sbank <- bank2[idx,]
    sbank$y <- NULL
    kc <- kmeans(sbank,5)
    
    
    newbank = rbind(bank, Newdata()$df)
    newbank[,c(2:10,14)] <- lapply(newbank[,c(2:10,14)],as.factor)
    newbank[,c(1,11:13,15:19)] <- lapply(newbank[,c(1,11:13,15:19)],as.numeric)
    newbank <- newbank[,c(-(11:20))]
    
    # reduce the level of factor and convert into numeric data
    newbank$job <- sub("admin.","employed",newbank$job)
    newbank$job <- sub("blue-collar","employed",newbank$job)
    newbank$job <- sub("entrepreneur","employed",newbank$job)
    newbank$job <- sub("housemaid","employed",newbank$job)
    newbank$job <- sub("management","employed",newbank$job)
    newbank$job <- sub("retired","unemployed",newbank$job)
    newbank$job <- sub("self-employed","employed",newbank$job)
    newbank$job <- sub("services","employed",newbank$job)
    newbank$job <- sub("student","unemployed",newbank$job)
    newbank$job <- sub("technician","employed",newbank$job)
    newbank$job <- as.factor(newbank$job)
    
    newbank$education <- sub("basic.4y","educated",newbank$education)
    newbank$education <- sub("basic.6y","educated",newbank$education)
    newbank$education <- sub("basic.9y","educated",newbank$education)
    newbank$education <- sub("high.school","educated",newbank$education)
    newbank$education <- sub("professional.course","well-educated",newbank$education)
    newbank$education <- sub("university.degree","well-educated",newbank$education)
    newbank$education <- as.factor(newbank$education)
    
    newbank$month <- sub("jan","Q1",newbank$month)
    newbank$month <- sub("feb","Q1",newbank$month)
    newbank$month <- sub("mar","Q1",newbank$month)
    newbank$month <- sub("apr","Q2",newbank$month)
    newbank$month <- sub("may","Q2",newbank$month)
    newbank$month <- sub("jun","Q2",newbank$month)
    newbank$month <- sub("jul","Q3",newbank$month)
    newbank$month <- sub("aug","Q3",newbank$month)
    newbank$month <- sub("sep","Q3",newbank$month)
    newbank$month <- sub("oct","Q4",newbank$month)
    newbank$month <- sub("nov","Q4",newbank$month)
    newbank$month <- sub("dec","Q4",newbank$month)
    newbank$month <- as.factor(newbank$month)
    
    newbank[,c(2:10)] <- lapply(newbank[,c(2:10)],as.numeric)
    newbank[,1]=datanorm(newbank[,1])
    
    sample1 <- newbank[41189,]
    library(clue)
    kcresult <- cl_predict(object = kc,newdata=sample1)
    
    print(kcresult)
    
  })
  
  output$text2 <- renderText({
    paste("Note:")
  })
  
  output$text3 <- renderText({
    
    a <- "Decision trees graph will show the most relevant variables that affects the response of the customer and the results give the probability of (yes,no)."
    b <- "Logistic regression graph will show the accurancy of this model and the result gives the probability that a customer will take the term deposit."
    c <- "The describution of Knn model will show the accurancy of this model and the result will show the probability."
    d <- "The plotting graph of clusterning will provide the additional information about the custome and the result will identify the possible classification for the new customer."
    e <- "If it belongs to group1, the customer is more likely to have home loan. If it belongs to group 2, the customer may not have home loan. 
          If it belongs to group 3 or 4, the customer is more likely to have university degree.
          If it belongs to group 5, the customer is more likely to receive the last contact on either Monday or Friday."
    
    ex = rbind(a,b,c,d,e)     
    paste(ex)
  
  })
  

  
})

