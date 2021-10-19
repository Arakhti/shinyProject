#### Library######
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(lessR)
library(dplyr)
library(reshape2)
library(DiscriMiner)

library('rcompanion')
library(dummies)
library(dplyr)
##########################################################
########## L'interface##################################

ui <- dashboardPage(
  dashboardHeader(title = "My first APP"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Chargement des données", tabName = "lireDonnées", icon = icon("readme")),
                menuItem("Statistiques", tabName = "visualisation", icon = icon("poll"),
                         
                         menuSubItem("Univariées",
                                     tabName = "Uni"),
                         menuSubItem("Bivariées",
                                     tabName = "Bi"),
                         selected = TRUE),
                menuItem("Prediction", tabName="Prediction")
                
                
                )

    ),
  dashboardBody(
    tabItems(
      
     tabItem(tabName = "lireDonnées",
            h1("Chargement des données"),
            fileInput(inputId = "datafile", label = "Choisissez un fichier CSV",
                      accept = c("text/plain", ".csv")),
            checkboxInput("header", "Header", TRUE),
            dataTableOutput("contents")
            
            
      
           ),
     
     tabItem(tabName="Uni",
            h1("Statistiques univariées"),
            fluidRow(
              column(4, 
                     selectInput(inputId = "column1", 
                                 "Colonne", 
                                 c("Age" = "Age",
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
                                   "HeartDisease" = "HeartDisease")))
            ),
            fluidRow(
              column(6, plotOutput("gender")),
              column(6, tableOutput("table_quali")),
              conditionalPanel(
                condition = "input.column1 == 'Age' || input.column1 == 'RestingBP'  || input.column1 == 'Cholesterol' || input.column1 == 'MaxHR' ||  input.column1 == 'Oldpeak'",
                column(6, plotOutput("boxPlot"))
                
              )
             
            ),
            tags$hr(),
            fluidRow(
              column(12,align="center", tableOutput("summary"))
            )
          ),
     tabItem(tabName="Bi",
             h1("Statistiques Bivariées"),
             fluidRow(
               column(4, 
                      selectInput(inputId = "columnA", 
                                  "Variable 1", 
                                  c("Age" = "Age",
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
                                    "HeartDisease" = "HeartDisease"))),
               column(4, 
                      selectInput(inputId = "columnB", 
                                  "Variable 2", 
                                  c("Age" = "Age",
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
                                    "HeartDisease" = "HeartDisease")))
               
             ),
             fluidRow(
               conditionalPanel(condition = "output.typeOfMix == 'QuantiQuanti'", 
                                column(12, plotOutput("nuagePoints")), 
                                column(12,align="center",textOutput("correlation"))
               ),
               conditionalPanel(condition = "output.typeOfMix == 'QuantiQuali'", 
                                column(4, 
                                       fluidRow(column(8, tableOutput("statsSummaryBivar"))),
                                       fluidRow(column(8,align="center", textOutput("correlationRatio")))),
                                column(8, plotOutput("boxPlotsParalleles")),
                                
               ),
               conditionalPanel(condition = "output.typeOfMix == 'QualiQuali'", 
                                column(12, plotOutput("qauliVSquali")),
                                column(12,align="center", textOutput('cramer'))
                              
               )

             )
     
            
        
  ),
  
  tabItem(tabName="Prediction",
    h1("Prediction using SVM"),
    column(6,tableOutput("new"))
  )
)
)
)








######################################################
###################Serveur########################
server <- function(input, output){
  
  
  output$contents <- renderDataTable({
    
    req(input$datafile)
    
     read.csv(input$datafile$datapath, header = input$header, check.names = FALSE)
  },  options = list(scrollX = TRUE , dom = 't'))
  
  data <- reactive({
    read.csv(input$datafile$datapath, header = input$header, check.names = FALSE)
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

      paste("Rapport de correlation : ", format(round(corRatio(data()[,columnQuanti], data()[,columnQuali]), 4), nsmall = 4))
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

  
  #For prediction
  newdata <- reactive({
    categories= c('Sex', 'ChestPainType','RestingECG', 'ExerciseAngina','ST_Slope')
    continuousVra= c('Age', "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")
    data=dummy.data.frame(data(), names=categories, sep="_")
    for(i in continuousVra) {
      data[, i]=data()[, i] -( mean(data()[, i]) / sd(data()[, i]))
    }
    print(data)
    data
    
    
    })
  
  output$correlation<- renderText({
    x.var = columnA(); y.var = columnB();
    paste("coefficient de correlation est:", cor(data()[, x.var], y = data()[, y.var],use="complete.obs"))
  })
  
  columnC <- eventReactive(input$columnC, {
    input$columnC
  })
  

  
  output$qauliVSquali <- renderPlot({
    columnA <- sym(columnA())
    columnB <- sym(columnB())
    ggplot(data())+ geom_bar(aes(x = !!columnA, fill= factor(!!columnB)), position=position_dodge())
    
    
    
  })
  
  output$cramer <- renderText({
   v1 = columnA(); v2 = columnB();
    paste("La valeur du test de Cramer est: ", cramerV(data()[, v1], data()[, v2], bias.correct = TRUE))
  })
  
  output$new <- renderTable({
         newdata()#it's just a test
  })
}

# Association interface & commandes
shinyApp(ui = ui, server = server)
