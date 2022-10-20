library(shiny)
library(mlbench)
library(caret)
library(tree)
library(shinythemes)
library(png)
library(shinydashboard)
library(shinyWidgets)

##################################Define Function ############################

# Define function: downsize dataset
diabetes.data <- function() {
  data(PimaIndiansDiabetes)
  sum(is.na(PimaIndiansDiabetes)) #check no missing values
  # Down sizing the dataframe to 500 observations
  pos <-
    subset(PimaIndiansDiabetes,
           PimaIndiansDiabetes$diabetes %in% c("pos"))
  neg <-
    subset(PimaIndiansDiabetes,
           PimaIndiansDiabetes$diabetes %in% c("neg"))
  set.seed(688)
  #Select 250 of each classes to obtain a balanced dataset
  pos1 <- pos[sample(nrow(pos), 250),]
  neg1 <- neg[sample(nrow(neg), 250),]
  diabetes <- rbind(pos1, neg1)
  diabetes$diabetes = factor(diabetes$diabetes)
  #summary(diabetes$diabetes)
  return(diabetes)
}

# Define function: Decision tree
decision.tree <- function() {
  # Random split to training and test set
  diabetes <- diabetes.data()
  
  set.seed(688)
  
  train.index = createDataPartition(diabetes$diabetes, p = 0.7, list = FALSE)
  train = diabetes[train.index, ]
  test = diabetes[-train.index, ]
  
  ########caret for decision tree########
  fitcontrol = trainControl(method = "repeatedcv",
                            number = 7,
                            repeats = 3)
  set.seed(688)
  diabetes.rpart = train(
    train[, -ncol(diabetes)],
    train[, ncol(diabetes)],
    method = "rpart",
    tuneLength = 5,
    trControl = fitcontrol
  )
  
  return(diabetes.rpart)
}

# Define function: Random Forest
random.forest <- function() {
  # random split to training and test set
  diabetes <- diabetes.data()
  
  set.seed(688)
  
  train.index = createDataPartition(diabetes$diabetes, p = 0.7, list = FALSE)
  train = diabetes[train.index, ]
  test = diabetes[-train.index, ]
  
  ######### caret for Random Forest ########
  
  fitControl.rf = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 3)
  set.seed(688)
  rfFit = train(
    diabetes ~ .,
    data = train,
    method = "rf",
    metric = "Accuracy",
    trControl = fitControl.rf,
    tuneLength = 5
  )
  return(rfFit)
}


##################################Shiny Application ##########################
# UI Design
ui <- dashboardPage(
  dashboardHeader(title = 'Diabetes Classification'),
  
  dashboardSidebar(
    sidebarMenu(
      id = "Semi_collapsible_sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Value Import", tabName = "value", icon = icon("th")),
      menuItem(
        "Classfication model",
        tabName = "model",
        icon = icon("network-wired"),
        menuSubItem("Decision Tree", tabName = "treetab"),
        menuSubItem("Random Forest", tabName = "rftab")
      ),
      menuItem(
        "Model Analysis",
        tabName = "compare",
        icon = icon("bar-chart-o")
      ),
      menuItem("FAQs", tabName = "faq", icon = icon("question"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #Overview tab content
      tabItem(
        tabName = 'overview',
        h2(strong('Overview')),
        h5(
          'The main function of this Shiny application is to predict whether a patient has diabetes or not,
              based on Pima Indian Diabetes dataset. The methods implemented are tree-based classifiers,
              Decision Tree and Random Forest, which are straightforward to understand and interpret the results.'
        ),
        h5(
          'The observations of this dataset are all females of at least 21 years and of Pima Indian heritage.
                 There are 8 diagnostic variables used for the prediction, which are mainly the number of times pregnant,
                 the BMI index and glucose level, among others. Through adjusting these parameters in the Value Import tab,
                 users can obtain results of the prediction in Classification Model tab, as well as some further knowledge
                 regarding the functioning of these methods.'
        ),
        h5(
          'The correlation of each pair of variables is showed in the plot below,
                 for users to have a better understanding regarding the actual dataset.'
        ),
        hr(),
        fluidRow(plotOutput('cor', width = "70%"), align = 'center')
      ),
      
      #Assessment tab content
      tabItem(
        tabName = 'value',
        h2(strong('Choose the values of the parameters:')),
        fluidRow(
          chooseSliderSkin('Flat', color = 'lightskyblue'),
          box(
            h5(strong('Pregnant times')),
            h6(em('Number of times pregnant')),
            sliderInput(
              inputId = "pre",
              label = NULL,
              min = 0,
              max = 17,
              value = 0,
              step = 1
            )
          ),
          
          box(
            h5(strong('Glucose level')),
            h6(em(
              'Plasma glucose concentration in an oral glucose test'
            )),
            sliderInput(
              inputId = "g",
              label = NULL,
              min = 0,
              max = 199,
              value = 0,
              step = 1
            )
          ),
          
          box(
            h5(strong('Pressure level')),
            h6(em('Diatolic blood pressure (mm Hg')),
            sliderInput(
              inputId = "p",
              label = NULL,
              min = 0,
              max = 122,
              value = 0,
              step = 1
            )
          ),
          
          box(
            h5(strong('Triceps level')),
            h6(em('Triceps skin fold thickness (mm)')),
            sliderInput(
              inputId = "t",
              label = NULL,
              min = 0,
              max = 99,
              value = 0,
              step = 1
            )
          ),
          
          
          box(
            h5(strong('Insulin level')),
            h6(em('2-hour serum insulin (mu U/mL')),
            sliderInput(
              inputId = "i",
              label = NULL,
              min = 0,
              max = 846,
              value = 0,
              step = 1
            )
          ),
          
          box(
            h5(strong('Mass')),
            h6(em('BMI index (weight in kg/(height in m)^2')),
            sliderInput(
              inputId = "m",
              label = NULL,
              min = 0,
              max = 67.1,
              value = 0,
              step = 0.1
            )
          ),
          
          
          box(
            h5(strong('Pedigree')),
            h6(em(
              'Diabetes pedigree function demonstrates genetic history'
            )),
            sliderInput(
              inputId = "pe",
              label = NULL,
              min = 0.078,
              max = 2.420,
              value = 0.078,
              step = 0.1
            )
          ),
          
          
          box(
            h5(strong('Age')),
            h6(em('Age (years)')),
            sliderInput(
              inputId = "a",
              label = NULL,
              min = 21,
              max = 81,
              value = 21,
              step = 1
            )
          ),
          
          submitButton('Submit', width = 300),
          helpText('Here are the values you have selected, please move to next tab for the results'),
          verbatimTextOutput('value'),
          align = 'center'
        )
        
      ),
      
      #Predictive Result tab content
      tabItem(
        tabName = 'treetab',
        h2(strong('Decision Tree')),
        h5(
          'Decision tree displays continuous splitting process of the observations in a branch-like manner to predict binary type of outcome.'
        ),
        h5(
          'The Decision tree of our model starts with top split assigning observations having glucose level < 123 to the left side branch whereas with glucose >= 123
                 are assigned to the right side of the branch, leading to the prediction outcome of positive. Left branch is then further subdivided into categories based on
                 different medical predictor features, resulting in multiple terminal nodes being formed at the bottom of the tree as the prediction outcome. The darker the color of the node reflects
                 more confidence of the classification result.'
        ),
        hr(),
        box(h4('Decision Tree Process'), plotOutput('dt'), width =
              7),
        box(h4('Result'), plotOutput('predict'), width = 5),
        uiOutput('text2')
      ),
      
      tabItem(
        tabName = 'rftab',
        
        h2(strong('Random Forest')),
        h5(
          'Random forest method consists of selecting random variables of the dataset to build several numbers of different decision trees to determine the prediction result.
                   This final result is based on the majority vote between the outcomes from each tree.'
        ),
        h5(
          'Our random forest model randomly selects two variables of the dataset to build different subtrees. In this case, we built 500 subtrees to proceed
                   majority voting to classify the final prediction outcome.'
        ),
        hr(),
        fluidRow(
          box(
            h4('Random Forest Process'),
            plotOutput('image', height = '400px', width = '100%'),
            width = 7
          ),
          box(h4('Result'), plotOutput('rf', height = '400px'), width =
                5),
          align = 'center'
        ),
        uiOutput('text3')
      ),
      
      # Compare predictive result tab content
      tabItem(
        tabName = 'compare',
        h2(strong('Variable Importance Comparison')),
        h5(
          'The variable importance represents the level of contribution of each variable to the classification process. 
                 In this case, the variable importance measure of both methods indicate that the most relevant feature is glucose, 
          whereas insulin and triceps are the least important ones on the prediction.'
        ),
        h5('The plots show the ranking of importance of the variables in the two classifiers when predicting the outcome.'),
        hr(),
        box(plotOutput('dtvar')),
        box(plotOutput('rfvar'))
      ),
      
      tabItem(
        tabName = 'faq',
        h2(strong('FAQs')),
        br(),
        h4(strong(
          '1.	Why cannot I choose input values outside the slide bar?'
        )),
        h4(
          'Because the model was built according to the values of the dataset, we cannot assure the accuracy of the outcome if the input is outside this range. '
        ),
        br(),
        h4(
          strong(
            '2.	How long do I need to wait to see the result in random forest prediction?'
          )
        ),
        h4(
          'Our random forest model needs to compute the prediction outcome of 500 trees, which takes longer than decision tree model, therefore, please wait patiently.'
        )
      )
    )
    
    
  )
)

# Server Output
server <- function(input, output) {
  #Output in the Overview tab content
  #Correlation plot output
  output$cor = renderPlot({
    diabetes <- diabetes.data()
    
    cols <- c("lightskyblue", "lightsalmon")
    # set the symbols for the three classes
    pchs <- c(1, 2)
    # use pairs() to create the pairs plot, with different colours and symbols
    # for different classes
    
    pairs(
      diabetes[, 1:8],
      pch = pchs[diabetes$diabetes],
      cex = 0.5,
      col = cols[diabetes$diabetes],
      lower.panel = NULL
    )
    # create a legend for the plot
    par(xpd = TRUE)
    legend(
      "bottomleft",
      legend = c("Positive", "Negative"),
      col = cols,
      pch = pchs,
      cex = 1,
      text.font = 4
    )
  })
  
  #Output in import value tab content
  output$value <- renderText({
    paste(
      'pregnant' ,
      input$pre,
      '  glucose' ,
      input$g,
      '  pressure' ,
      input$p,
      '  triceps' ,
      input$t,
      '  insulin' ,
      input$i,
      '  mass',
      input$m,
      '  pedigree' ,
      input$pe,
      '  age',
      input$a
    )
    
  })
  
  
  # Output in decision tree tab content
  # Decision Tree Plot Output
  output$dt = renderPlot({
    diabetes.rpart <- decision.tree()
    
    
    library(rattle)
    fancyRpartPlot(
      diabetes.rpart$finalModel,
      palettes = c('BuPu', 'OrRd'),
      sub = NULL
    )
    
    
  })
  
  # Decision Tree Prediction Plot Output
  output$predict = renderPlot({
    diabetes.rpart <- decision.tree()
    
    df <- data.frame(
      'pregnant' = input$pre,
      'glucose' = input$g,
      'pressure' = input$p,
      'triceps' = input$t,
      'insulin' = input$i,
      'mass' = input$m,
      'pedigree' = input$pe,
      'age' = input$a
    )
    
    predict.caret <-
      predict(diabetes.rpart, newdata = df, type = 'prob')
    predict.caret1 <- predict(diabetes.rpart, newdata = df)
    
    x <- c(predict.caret$neg, predict.caret$pos)
    label <- c('Negative', 'Positive')
    pie_labels <-
      paste0(label, " : ", round(100 * x / sum(x), 2), "%")
    pie(
      x,
      labels = pie_labels,
      col = c('cornflowerblue', 'orange'),
      radius = 0.7
    )
    
    output$text2 <- renderUI({
      if (predict.caret1 == 'neg') {
        return(
          h4(
            'The prediction of your result is: Negative, with a probability of',
            round(predict.caret$neg * 100, 2),
            '%'
          )
        )
        
      }
      else{
        return(
          h4(
            'The prediction of your result is: Positive, with a probability of',
            round(predict.caret$pos * 100, 2),
            '%'
          )
        )
      }
      
    })
    
  })
  
  
  # Output in random forest tab content
  # Output randomforest image interpretation
  output$image = renderPlot({
    pic <- readPNG("randomforest.png")
    plot.new()
    rasterImage(pic, 0, 0, 1, 1)
  })
  
  
  #output randomforest prediction plot
  output$rf = renderPlot({
    rf.caret <- random.forest()
    
    df <- data.frame(
      'pregnant' = input$pre,
      'glucose' = input$g,
      'pressure' = input$p,
      'triceps' = input$t,
      'insulin' = input$i,
      'mass' = input$m,
      'pedigree' = input$pe,
      'age' = input$a
    )
    
    predict.rf <- predict(rf.caret, newdata = df, type = 'prob')
    predict.rf1 <- predict(rf.caret, newdata = df)
    
    x <- c(predict.rf$neg, predict.rf$pos)
    label <- c('Negative', 'Positive')
    pie_labels <- paste0(label, " : ", round(100 * x, 2), "%")
    pie(
      x,
      labels = pie_labels,
      col = c('#0099FF', '#66cc33'),
      radius = 0.7
    )
    
    output$text3 <- renderUI({
      if (predict.rf1 == 'neg') {
        return(
          h4(
            'The prediction of your result is: Negative, with a probability of',
            round(predict.rf$neg * 100, 2),
            '%'
          )
        )
      }
      else{
        return(
          h4(
            'The prediction of your result is: Positive, with a probability of',
            round(predict.rf$pos * 100, 2),
            '%'
          )
        )
      }
      
    })
    
  })
  
  output$dtvar <- renderPlot({
    dtFit <- decision.tree()
    
    import.dt <- varImp(dtFit, scale = FALSE)$importance
    new.df <- cbind(Features = rownames(import.dt), import.dt)
    import.sort <- new.df[order(new.df$Overall), ]
    barplot(
      height = import.sort$Overall,
      names = import.sort$Features,
      col = rgb(0.2, 0.7, 0.8, 0.5),
      horiz = T,
      las = 1,
      xlab = "Mean Decrease Gini",
      border = F,
      cex.names = 0.8,
      main = 'Decision Tree Variable Importance'
    )
  })
  
  output$rfvar <- renderPlot({
    rfFit <- random.forest()
    
    import.rf <- varImp(rfFit, scale = FALSE)$importance
    new.df <- cbind(Features = rownames(import.rf), import.rf)
    import.sort <- new.df[order(new.df$Overall), ]
    barplot(
      height = import.sort$Overall,
      names = import.sort$Features,
      col = rgb(0.2, 0.7, 0.8, 0.5),
      horiz = T,
      las = 1,
      xlab = "Mean Decrease Gini",
      border = F,
      cex.names = 0.8,
      main = 'Random Forest Variable Importance'
    )
  })
  
}

shinyApp(ui, server)
