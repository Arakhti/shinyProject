#### Library######
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(lessR)
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
                         selected = TRUE)
                
                
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
              column(6, plotOutput("boxPlot"))
            ),
            fluidRow(
              column(6, tableOutput("summary"))
            )
          ),
     tabItem(tabName="Bi",
             h1("Statistiques Bivariées"),
             fluidRow(
               column(12, plotOutput("AgeSex")),
               column(12, plotOutput("ChestPainTypeSex"))
              
             )
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
  
  
  
  
  
  
  
}

# Association interface & commandes
shinyApp(ui = ui, server = server)
