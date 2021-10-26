#### Library######
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(lessR)
library(dplyr)
library(reshape2)
library(DiscriMiner)
library(caret)

library('rcompanion')
library(dummies)
library(dplyr)
library(e1071)



columnslist <- c("Age" = "Age",
                        "Sex" = "Sex",
                        "ChestPainType" = "ChestPainType",
                        "RestingBP" = "RestingBP",
                        "Cholesterol" = "Cholesterol",
                        "FastingBS" = "FastingBS",
                        "RestingECG" = "RestingECG",
                        "MaxHR" = "MaxHR",
                        "ExerciseAngina" = "ExerciseAngina",
                        "Oldpeak" = "Oldpeak",
                        "ST_Slope" = "ST_Slope",
                        "HeartDisease" = "HeartDisease")

##########################################################
########## L'interface##################################

ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Chargement des données", tabName = "lireDonnées", icon = icon("readme")),
                menuItem("Statistiques", tabName = "visualisation", icon = icon("poll"),
                         
                         menuSubItem("Univariées",
                                     tabName = "Uni"),
                         menuSubItem("Bivariées",
                                     tabName = "Bi"),
                         selected = TRUE),
                menuItem("Prediction", tabName="Prediction",icon=icon("fas fa-filter"),
                         menuSubItem("Régression Logistique",
                                     tabName = "logit"),
                         menuSubItem("SVM", tabName = "SVM"),
                         
                         selected = TRUE)
                
                
                )

    ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .my_table .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
        border-top: 3px solid #6495ED; 
      },
    "))
    ),
    tabItems(
      
     tabItem(tabName = "lireDonnées",
            h1("Chargement des données"),
            fileInput(inputId = "datafile", label = "Choisissez un fichier CSV",
                      accept = c("text/plain", ".csv")),
            dataTableOutput("contents")
            
            
      
           ),
     
     tabItem(tabName="Uni",
            h1("Statistiques univariées"),
            tags$hr(),
            fluidRow(
              column(4, 
                     selectInput(inputId = "column1", 
                                 "Variable", 
                                 columnslist))
            ),
            fluidRow(
              column(6, plotOutput("gender")),
              column(6,class="my_table" ,tableOutput("table_quali")),
              conditionalPanel(
                condition = "input.column1 == 'Age' || input.column1 == 'RestingBP'  || input.column1 == 'Cholesterol' || input.column1 == 'MaxHR' ||  input.column1 == 'Oldpeak'",
                column(6, plotOutput("boxPlot"))
                
              )
             
            ),
            tags$hr(),
            fluidRow(
              column(12,align="center",class="my_table", tableOutput("summary"))
            )
          ),
     tabItem(tabName="Bi",
             h1("Statistiques Bivariées"),
             tags$hr(),
             fluidRow(
               column(4, 
                      selectInput(inputId = "columnA", 
                                  "Variable 1", 
                                  columnslist)),
               column(4, 
                      selectInput(inputId = "columnB", 
                                  "Variable 2", 
                                  columnslist))
               
             ),
             fluidRow(
               conditionalPanel(condition = "output.typeOfMix == 'QuantiQuanti'", 
                                column(12, plotOutput("nuagePoints")), 
                                column(12,class="cor",align="center",htmlOutput("correlation"))
               ),
               conditionalPanel(condition = "output.typeOfMix == 'QuantiQuali'", 
                                column(4, 
                                       fluidRow(column(8, class="my_table",tableOutput("statsSummaryBivar"))),
                                       fluidRow(column(8,align="center", htmlOutput("correlationRatio")))),
                                column(8, plotOutput("boxPlotsParalleles"))
                                
               ),
               conditionalPanel(condition = "output.typeOfMix == 'QualiQuali'", 
                                column(12, plotOutput("qauliVSquali")),
                                column(12,align="center", textOutput('cramer'))
                              
               )

             )
     
            
        
  ),
  
  tabItem(tabName="SVM",
    h1("SVM"),
    h4("Prédiction de HeartDisease en fonction des autres variables",tags$br(),
       "Le dataset est divisé en training set (75%) et test set (25%)"),
    checkboxGroupInput(
      "svmColumns",
      "Colonnes utilisées pour le svm",
      choices = columnslist[-12],
      selected = columnslist[-12],
      inline = TRUE
    ),
    tags$hr(),
    column(6,
           fluidRow(valueBoxOutput("SVMtrainingAcc")),
           p(strong(h4("Matrice de confusion Training Set", align="center"))),
           fluidRow(plotOutput("svmconf"))
    ),
    column(6,
           fluidRow(valueBoxOutput("SVMvalAcc")),
           p(strong(h4("Matrice de confusion Test Set", align="center"))),
           fluidRow(plotOutput("SVMvalMatrix"),
           tags$hr())
           
    ),
    tags$hr(),
    fluidRow(
      column(12,align="center", verbatimTextOutput("summSVModel")),
    )
  ),
  tabItem(tabName="logit",
          h1("Régression logistique"),
          h4("Prédiction de HeartDisease en fonction des autres variables",tags$br(),
             "Le dataset est divisé en training set (75%) et test set (25%)"),
          checkboxGroupInput(
            "logistColumns",
            "Colonnes utilisées pour la régression logistique",
            choices = columnslist[-12],
            selected = columnslist[-12],
            inline = TRUE
          ),
          tags$hr(),
          column(6,
                 fluidRow(valueBoxOutput("trainingAcc")),
                 p(strong(h4("Matrice de confusion Training Set", align="center"))),
                 fluidRow(plotOutput("trainingMatrix"))
                 ),
          column(6,
                 fluidRow(valueBoxOutput("valAcc")),
                 p(strong(h4("Matrice de confusion Test Set", align="center"))),
                 fluidRow(plotOutput("valMatrix"),
                 tags$hr())
          ),
          tags$hr(),
          fluidRow(
            column(4, verbatimTextOutput("summRegModel")),
            column(4, plotOutput("regModel2")),
            column(4, plotOutput("regModel5"))
            
          )
  )
)
))









######################################################
###################Serveur########################
server <- function(input, output){
  
  
  output$contents <- renderDataTable({
    
    req(input$datafile)
    
     read.csv(input$datafile$datapath, header = TRUE, check.names = FALSE)
  },  options = list(scrollX = TRUE , dom = 't'))
  
  data <- reactive({
    read.csv(input$datafile$datapath, header = TRUE, check.names = FALSE)
  })
  
  columnChosen <- eventReactive(input$column1, {
    input$column1
  })
  
  rv <- reactiveValues(hist_isFreq = FALSE,
                       hist_ylabel = "Effectifs",
                       hist_col= "blue",
                       hist_colIndex = 2,
                       typeOfData = "quanti",
                       argsNames = NULL)
  
  # Determine le type de la variable (qualitative / quantitative)
  observeEvent(input$column1, {
    if (input$column1 %in% c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")){
      rv$typeOfData = "quanti"
    } else {
      rv$typeOfData = "quali"
    }
  })
  
  tabStats <- reactive({
    table.tmp <- as.data.frame(table(data()[columnChosen()]))
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data()) * 100)
    colnames(table.tmp) <- c("Classe", "Effectifs", "Pourcentage")
    print(table.tmp)
    table.tmp
  })
  
  tabCentreDisp <- reactive({
    # Noms des caractéristiques
    names.tmp <- c("Maximum", "Minimum", "Moyenne", "Médiane",
                   "1e quartile", "3e quartile", "Variance", "Ecart-type")
    
    chosenData <- na.omit(data()[,columnChosen()])
    # Calcul des caractéristiques
    summary.tmp <- c(max(chosenData), min(chosenData), mean(chosenData), median(chosenData),
                     quantile((chosenData))[2], quantile((chosenData))[4],
                     var(chosenData), sqrt(var(chosenData)))
    # Ajout des nomes au vecteur de valeurs
    summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
    # Ajout des noms de colonnes
    colnames(summary.tmp) <- c("Caractéristique", "Valeur")
    
    summary.tmp
  })
  
  output$gender <- renderPlot({ 
    if (rv$typeOfData == "quanti") {
      print(rv$typeOfData)
        column <- sym(columnChosen())
        ggplot(data()) + geom_histogram(aes(x = !!column), fill = "cornflowerblue")
      
    } else {
      print(rv$typeOfData)
        column <- sym(columnChosen())
        ggplot(data()) + geom_bar(aes(x = !!column), fill = "cornflowerblue")
    }
    
  })
  
  output$table_quali <- renderTable({
    if (rv$typeOfData == "quali") {
      tabStats()
    } else {
      NULL
    }
  })
  
  output$boxPlot <- renderPlot({
    if (rv$typeOfData == "quanti") {
      column <- sym(columnChosen())
      ggplot(data()) + geom_boxplot(aes(y = !!column))
    }else {
      NULL
    }})
  
  output$summary <- renderTable({
    if (rv$typeOfData == "quanti") { 
      tabCentreDisp()
    } else {
      NULL
    }
  })
  
  
  ###### Analyse Bivariée #####
  columnA <- eventReactive(input$columnA, {
    input$columnA
  })
  
  columnB <- eventReactive(input$columnB, {
    input$columnB
  })
  

  rvBivar <- reactiveValues(
                       typeOfDataA = "quanti",
                       typeOfDataB = "quanti",
                       typeOfMix = "QuantiQuanti"
                       )
  
  # Determine le type de la variable (qualitative / quantitative)
  observeEvent(input$columnA, {
    if (input$columnA %in% c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")){
      rvBivar$typeOfDataA = "quanti"
    } else {
      rvBivar$typeOfDataA = "quali"
    }
  })
  
  # Determine le type de la variable (qualitative / quantitative)
  observeEvent(input$columnB, {
    if (input$columnB %in% c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")){
      rvBivar$typeOfDataB = "quanti"
    } else {
      rvBivar$typeOfDataB = "quali"
    }
  })
  
  
  QuantiQuali <- reactive({
    affiche <- FALSE
    columnQuali <- NULL
    columnQuanti <- NULL
    if (rvBivar$typeOfDataA == "quali" && rvBivar$typeOfDataB == "quanti") {
      affiche <- TRUE
      columnQuali <- columnA()
      columnQuanti <- columnB()
    }
    else if (rvBivar$typeOfDataA == "quanti" && rvBivar$typeOfDataB == "quali") {
      affiche <- TRUE
      columnQuali <- columnB()
      columnQuanti <- columnA()
    }
    list(affiche, columnQuali, columnQuanti)
  })
  
  # Table Stats Quanti pour chaque classe de variable A quali
  output$statsSummaryBivar <- renderTable({
    quantiQualiValues <- QuantiQuali()
    affiche <- quantiQualiValues[[1]]
    columnQuali <- quantiQualiValues[[2]]
    columnQuanti <- quantiQualiValues[[3]]

    if (affiche) {
      columnBSelection <- data()[,columnQuanti]
      totalMean <- mean(columnBSelection)
      totalSd <- sd(columnBSelection)
      
      tableStats <- data() %>% 
        group_by_at(columnQuali) %>%
        summarise(moyenne = mean(!!!syms(columnQuanti)), ecartType = sd(!!!syms(columnQuanti)))
      tableStats = as.data.frame(tableStats)
      colnames(tableStats) <- c(columnQuali, paste("Moyenne de ", columnQuanti), paste("Ecart-type de ", columnQuanti))
      tableStats <- rbind(tableStats, c("Total", totalMean, totalSd))
      
      tableStats[,c(2,3)] <- tableStats[,c(2,3)] %>% mutate_if(is.character,as.numeric)
      tableStats
    }
    else {
      NULL
    }
  }, digits = 2)
  
  
  output$boxPlotsParalleles <- renderPlot({
    quantiQualiValues <- QuantiQuali()
    affiche <- quantiQualiValues[[1]]
    columnQuali <- quantiQualiValues[[2]]
    columnQuanti <- quantiQualiValues[[3]]
    if (affiche) {
      newData <- data()
      newData[, columnQuali] <- lapply(newData[, columnQuali], as.character)
      qplot(x = !!sym(columnQuali), y = !!sym(columnQuanti), data = data(),
            xlab = columnQuali, ylab = columnQuanti,
            geom=c("boxplot", "jitter"), fill = factor(!!sym(columnQuali))) +
        theme(legend.title=element_blank())

    }
    
    else {
      NULL
    }
  })
  
  
  output$correlationRatio <- renderText({
    quantiQualiValues <- QuantiQuali()
    affiche <- quantiQualiValues[[1]]
    columnQuali <- quantiQualiValues[[2]]
    columnQuanti <- quantiQualiValues[[3]]
    if (affiche) {

      paste("<font color=\"#FF0000\"><b>","Rapport de correlation: ","<font color=\"#FF0000\"><b>", format(round(corRatio(data()[,columnQuanti], data()[,columnQuali]), 4), nsmall = 4))
    }
    else {
      NULL
    }

  })
  
  typeOfMix <- reactive({
    if (rvBivar$typeOfDataA == "quali" && rvBivar$typeOfDataB == "quali") {
      "QualiQuali"
    } else if (rvBivar$typeOfDataA == "quanti" && rvBivar$typeOfDataB == "quanti") {
      'QuantiQuanti'
    } else {
      "QuantiQuali"
    }
  })
  
  output$typeOfMix <- reactive({
    typeOfMix()
  })
  outputOptions(output, 'typeOfMix', suspendWhenHidden=FALSE)

  output$nuagePoints <- renderPlot({
    # Simple nuage de point EF vs CA
    options(scipen=999)
    x.var = columnA(); y.var = columnB();
    plot(x = data()[, x.var], y = data()[, y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    options(scipen=0)
  })


  
  output$correlation<- renderText({
    x.var = columnA(); y.var = columnB();
    paste("<font color=\"#FF0000\"><b>","Coefficient de correlation: ","<font color=\"#FF0000\"><b>", cor(data()[, x.var], y = data()[, y.var],use="complete.obs"))
  })
  
  columnC <- eventReactive(input$columnC, {
    input$columnC
  })
  

  
  output$qauliVSquali <- renderPlot({
    columnA <- sym(columnA())
    columnB <- sym(columnB())
    ggplot(data())+ geom_bar(aes(x = !!columnA, fill= factor(!!columnB)), position=position_dodge())
    
    
    
  })
  
  ### Regression logistique ###
  predLogist <- reactiveValues(confMatTrain = NULL,
                               accuracyTrain = NULL,
                               confMatVal = NULL,
                               accuracyVal = NULL,
                               regressionModel = NULL)
  

  logistColumns <- eventReactive(input$logistColumns, {
    input$logistColumns
  })

  logisPred <- reactive({
    dataFrame = data()
    
    # split data between training and validation set
    spec = c(train = .75, validate = .25)
    
    set.seed(1)
    g = sample(cut(
      seq(nrow(dataFrame)), 
      nrow(dataFrame)*cumsum(c(0,spec)),
      labels = names(spec)
    ))
    res = split(dataFrame, g)

    ### Training  ###
    dfWithoutHeartDisease = subset(res$train, select = -c(HeartDisease))
    # convert all columns to numeric
    matrix <- data.matrix(data.frame(unclass(dfWithoutHeartDisease)))
    # normalize data
    df = data.frame(scale(matrix))
    df = cbind(df, HeartDisease = res$train[,'HeartDisease'])
    # train model
    columsForRegression = paste(logistColumns(), collapse = " + ")
    myFormula <- as.formula(paste("HeartDisease ~ ", columsForRegression))
    
    glm.fit <- glm(myFormula,
                   data = df, family = binomial)

    glm.probs <- predict(glm.fit,type = "response")
    glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
    confMat = confusionMatrix(factor(glm.pred), factor(res$train[,'HeartDisease']))
    #confMatDf = as.data.frame.matrix(confMat$table)
    
    ### Validation ###
    dfWithoutHeartDiseaseVal = subset(res$validate, select = -c(HeartDisease))
    # convert all columns to numeric
    matrixVal <- data.matrix(data.frame(unclass(dfWithoutHeartDiseaseVal)))
    # normalize data
    dfVal = data.frame(scale(matrixVal))
    # predict
    glm.probsVal <- predict(glm.fit, newdata = dfVal ,type = "response")
    glm.predVal <- ifelse(glm.probsVal > 0.5, 1, 0)
    confMatVal = confusionMatrix(factor(glm.predVal), factor(res$validate[,'HeartDisease']))
    
    predLogist$confMatTrain = as.data.frame(confMat$table)
    predLogist$confMatVal = as.data.frame(confMatVal$table)
    predLogist$accuracyTrain = confMat$overall['Accuracy']
    predLogist$accuracyVal = confMatVal$overall['Accuracy']
    predLogist$regressionModel = glm.fit

  })
  
  output$trainingMatrix <- renderPlot({
    logisPred()
    ggplot(data =  predLogist$confMatTrain,
           mapping = aes(x = Reference,
                         y = Prediction)) +
      geom_tile(aes(fill = Freq)) +
      geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "cornflowerblue",
                          high = "green",
                          trans = "log")
  })
  
  output$valMatrix <- renderPlot({
    ggplot(data =  predLogist$confMatVal,
           mapping = aes(x = Reference,
                         y = Prediction)) +
      geom_tile(aes(fill = Freq)) +
      geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "cornflowerblue",
                          high = "green",
                          trans = "log")
  })
  
  
  output$trainingAcc <- renderValueBox({
    valueBox(
      paste(round(predLogist$accuracyTrain, 4) * 100, "%"), "Training accuracy", icon = icon(accIcon(predLogist$accuracyTrain), lib = "glyphicon"),
      color = accColor(predLogist$accuracyTrain)
    )
  })
  
  output$valAcc <- renderValueBox({
    valueBox(
      paste(round(predLogist$accuracyVal, 4) * 100, "%"), "Test accuracy", icon = icon(accIcon(predLogist$accuracyVal), lib = "glyphicon"),
      color = accColor(predLogist$accuracyVal)
    )
  })
  
  output$regModel5 <- renderPlot({
    plot(predLogist$regressionModel, which=c(5))
  })
  
  output$regModel2 <- renderPlot({
    plot(predLogist$regressionModel, which=c(2))
  })
  
  output$summRegModel <- renderPrint({
    print(summary(predLogist$regressionModel))
  })

  
  accColor = function(accuracy){
    if (accuracy  > 0.8) {
      return("green")
    } 
    if (accuracy > 0.7) {
      return("yellow")
    }
    return("red")
  }
  
  accIcon = function(accuracy){
    if (accuracy > 0.7) {
      return("thumbs-up")
    }
    return("thumbs-down")
  }

##############################SVM##################################
predSvm <- reactiveValues(SVMconfMatTrain = NULL,
                               SVMaccuracyTrain = NULL,
                               SVMconfMatVal = NULL,
                               SVMaccuracyVal = NULL,
                               SVMModel = NULL,
                             
              
                              )
  
svmColumns <- eventReactive(input$svmColumns, {
    input$svmColumns
  })

svmPred <- reactive({
  n=dim(data())[1]
  index = sample(n, 0.75 * n)
  trainingSet = data()[index, ]
  valSet = data()[-index, ]
 
  
  ### Training  ###
  dfWithoutHeartDisease = subset(trainingSet, select = -c(HeartDisease))
  # convert all columns to numeric
  matrix <- data.matrix(data.frame(unclass(dfWithoutHeartDisease)))
  # normalize data
  df.training = data.frame(scale(matrix))
  df.training = cbind(df.training, HeartDisease = trainingSet[,'HeartDisease'])
  
  
  dfValWithoutHeartDisease = subset(valSet, select = -c(HeartDisease))
  # convert all columns to numeric
  Valmatrix <- data.matrix(data.frame(unclass(dfValWithoutHeartDisease)))
  # normalize data
  df.val = data.frame(scale(Valmatrix))
  df.val = cbind(df.val, HeartDisease = valSet[,'HeartDisease'])
  
  
  
  columsForSVM = paste(svmColumns(), collapse = " + ")
  myFormula <- as.formula(paste("HeartDisease ~ ", columsForSVM))
 
  # build svm  model
  svm.model <- svm(myFormula, data = df.training)
  pred <- predict(svm.model, type = "response")
  svm.pred <- ifelse(pred > 0.5, 1, 0)
  Test.mod <- cbind(trainingSet, svm.pred)
  confMat=confusionMatrix(factor(Test.mod$svm.pred), factor(Test.mod$HeartDisease))
  p=plot(svm.model, data=df.training)
  
  ##############Testing the svm model#######################
  predVal <- predict(svm.model, newdata=df.val, type = "response")
  svm.predVal <- ifelse(predVal > 0.5, 1, 0)
  Test.modVal <- cbind(valSet, svm.predVal)
  confMatVal=confusionMatrix(factor(Test.modVal$svm.predVal), factor(Test.modVal$HeartDisease))
  
  
  #####################Stat of the model############
  
  predSvm$SVMconfMatTrain = as.data.frame(confMat$table)
  predSvm$SVMconfMatVal = as.data.frame(confMatVal$table)
  predSvm$SVMaccuracyTrain = confMat$overall['Accuracy']
  predSvm$SVMaccuracyVal = confMatVal$overall['Accuracy']
  predSvm$SVMModel= svm.model
 

 
  
  
  
})



output$SVMtrainingAcc <- renderValueBox({
  valueBox(
    paste(round(predSvm$SVMaccuracyTrain, 4) * 100, "%"), "Training accuracy", icon = icon(accIcon(predSvm$SVMaccuracyTrain), lib = "glyphicon"),
    color = accColor(predSvm$SVMaccuracyTrain)
  )
})

output$SVMvalAcc <- renderValueBox({
  valueBox(
    paste(round(predSvm$SVMaccuracyVal, 4) * 100, "%"), "Test accuracy", icon = icon(accIcon(predSvm$SVMaccuracyVal), lib = "glyphicon"),
    color = accColor(predSvm$SVMaccuracyVal)
  )
})

output$summSVModel <- renderPrint({
  print(summary(predSvm$SVMModel))
})


output$svmconf<-renderPlot({
  svmPred()
  ggplot(data =  predSvm$SVMconfMatTrain,
         mapping = aes(x = Reference,
                       y = Prediction)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low = "cornflowerblue",
                        high = "green",
                        trans = "log")
})

output$SVMvalMatrix <- renderPlot({
  ggplot(data =  predSvm$SVMconfMatVal,
         mapping = aes(x = Reference,
                       y = Prediction)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low = "cornflowerblue",
                        high = "green",
                        trans = "log")
})

  
  
  
  
  
  
  
  output$cramer <- renderText({
   v1 = columnA(); v2 = columnB();
    paste("La valeur du test de Cramer est: ", cramerV(data()[, v1], data()[, v2], bias.correct = TRUE))
  })
  
}

# Association interface & commandes
shinyApp(ui = ui, server = server)
