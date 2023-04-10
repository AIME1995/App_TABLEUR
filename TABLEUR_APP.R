library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(googleVis)
library(colourpicker)
## CREATION DE L'APPLICATION 

ui <- dashboardPage(
  dashboardHeader(title = "bioinfo-fr"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lecture des données", tabName = "readData",
               menuSubItem(
                 "CSV", tabName = "readData" 
               ),
               menuSubItem("excel", tabName = "dataexcel" )),
      menuItem("Visualisation des données", tabName = "visualization"),
      menuItem("Résumé statistique", tabName = "StatistiqueDescriptive"),
      menuItem("Graphique", tabName = "Graphiques",
               menuSubItem(
                 "nuagedepoint", tabName = "nuagepoint"),
                 menuSubItem("histogramme", tabName = "Histogramme"),
                          menuSubItem("barplott", tabName = "barplot"),
                          menuSubItem("boxplott", tabName = "boxplot")
               ),
      menuItem("regression linéaire",
               tabName = "regression"))
      
      
    ),
  dashboardBody(
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              h1("Lecture des données CSV"),
              fileInput(inputId =  "dataFile",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              fluidRow(
                column(3,
                       h3("Parameters"),
                       # Input: Checkbox if file has header
                       radioButtons(
                         "header",
                         label = "Header",
                         choices = c("yes" = TRUE,
                                     "no" = FALSE),
                         selected = TRUE, inline = TRUE),
                       # Input: Select separator ----
                       radioButtons( "sep", 
                                     label = "Separator",
                                     choices = c(Comma = ",",
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = "\t", inline=T),
                       # Input: Select quotes ----
                       radioButtons( "quote", 
                                     label= "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = "", inline=T),
                       tags$br(),
                       div(actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play") ), align = "center")),
              
                column(9,
                       h3("Prévisualisation des données"),
                       dataTableOutput(outputId = "prévisualisation")        ))
              
                
              ),
      tabItem(
        tabName = "dataexcel", h3("Lecture de données Excel"),
        fluidRow(
          column(3,
                 fileInput(inputId = "excel", label =  "Charger un fichier Excel",
                           accept = c(".xlsx", ".xls" )),
                 column(9,
                        h3("Prévisualisation des données"),
                        dataTableOutput(outputId = "prévisualisationEXCEL"),
                        tags$br(),
                        div(actionButton(inputId = "actionButonVisualisationEXCEL", label = "Visualisation",icon = icon("play") ), align = "center")))
        )
      ),
      
      
      # visualization
      tabItem(tabName = "visualization", 
              h1("Visualisation des données"),
              h2("Exploration du tableau"),
              dataTableOutput(outputId =  'datatable')
      ),
      tabItem(
        tabName = "StatistiqueDescriptive", h3("Les statistiques descriptives et la structure des données"),
        verbatimTextOutput(outputId = "summary"), 
        br(),
        verbatimTextOutput("str")
      ),
      tabItem(
        tabName = "nuagepoint", h3("Visualisation de la dispertion des observations"),
        selectInput(inputId = "xasis", label = "variable_x", choices = NULL),
        selectInput(inputId = "yasis", label = "variable_y", choices = NULL),
        
        plotOutput(outputId = "nuage"),
        textOutput(outputId = "textes")
      ),
      tabItem(
        tabName = "barplot", h3("Barplot"),
        selectInput(inputId = "y_asis", label = "varibles Y", choices = NULL),
        plotOutput(outputId = "bar"),
        textOutput(outputId = "tex")
      ),
      
      tabItem(
        tabName = "Histogramme", h3("Histogramme"),
        selectInput(inputId = "x", label = "variable x", choices = NULL ),
        sliderInput("bin", label = "nombre_de_classe", min = 3, max = 100 ,value =  50),
        plotOutput(outputId = "Histogramme"),
        textOutput(outputId = "text")
      ),
      tabItem(
        tabName = "boxplot",
        h3("Réalisation boxplot"),
        selectInput(inputId =  "variable_x", label =  "Choisissez une variable pour l'axe des X", choices = NULL),
        amChartsOutput(outputId = "boxplot"),
        textOutput(outputId = "texte")
        
      ),
      tabItem(
        tabName = "regression",
        h3("Regression linéaire"),
        selectInput(inputId = "independant", label = "Varibales indépendantes", choices = NULL),
        selectInput(inputId = "dependant", label = "variables dépendantes", choices = NULL),
        verbatimTextOutput(outputId = "regression"),
        plotOutput(outputId = "plot") 
        
      )
  )
))


server <- function(input, output, session) { 
  ## DESCRIPTION
  ## req(input$dataFile) : bloque la suite du code si la zone d’import de fichier est vide
  ## df <- read.csv() : on stocke dans df la lecture du fichier
  ## input$dataFile$datapath : chemin d’accès au fichier importé
  ## header = as.logical(input$header) : récupération de la réponse de l’utilisateur pour savoir si présence ou absence d’un header. Le as.logical permet de convertir un TRUE ou FALSE en booléen.
  ## sep = input$sep, quote = input$quote : récupération du paramétrage de l’utilisateur pour le séparateur et les quotes. Ces informations sont données aux arguments de la fonction read.csv()
  ## nrows=10 : Nous ne souhaitons pas lire tout le fichier. Seules les premières lignes sont nécessaires pour savoir si le tableau est lu correctement ou non. Nous lisons donc les 10 premières lignes.
  ## options = list(scrollX = TRUE , dom = 't') : Si le tableau a de nombreuses colonnes, cette option permet d’avoir un scroll horizontal
  
  output$prévisualisation <- renderDataTable({
    
    
    req(input$dataFile)
    
    dff <- read.csv(input$dataFile$datapath,
                    header = as.logical(input$header),
                    sep = input$sep,
                    quote = input$quote,
                    nrows = 10)
  }, options = list(scrllX = TRUE, dom = 't'))
  
  data = reactiveValues()
  
  observeEvent(input$actBtnVisualisation, {
    data$table = read.csv(input$dataFile$datapath,
                          header = as.logical(input$header),
                          sep = input$sep,
                          quote = input$quote)
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Le fichier a bien été lu !", 
      type = "success"
    )
    
    updateTabItems(session  , inputId =  "readData" , selected =  "visualization" )
    
  })
  output$datatable = DT::renderDataTable(data$table)
  
  updateTabItems(session  , inputId =  "readData" , selected =  "StatistiqueDescriptive" )
  
  output$prévisualisationEXCEL <- renderDataTable({
    req(input$excel)
    dffexcel <- read_excel(input$excel$datapath)
  }, options = list(scrllX = TRUE, dom = 't'))
  
  data = reactiveValues()
  
  observeEvent(input$actionButonVisualisationEXCEL, {
    data$table <- read_excel(input$excel$datapath)
    
    sendSweetAlert( session = session,
                    title = "Fait",
                    text = "Le fichier a bien été lu !",
                    type = "success")
    
    updateTabItems(session  , inputId =  "readData" , selected =  "visualization" )
    
  })
  output$datatable = DT::renderDataTable(data$table) 
  updateTabItems(session  , inputId =  "readData" , selected =  "StatistiqueDescriptive" )  
  
  
  output$summary <- renderPrint({
    summary(data$table)
  })
 output$str <- renderPrint({
   str(data$table)
 })
 updateTabItems(session  , inputId =  "readData" , selected =  "nuagepoint" )
 observe({
   updateSelectInput(session, "xasis", choices = names(data$table))
 })
 
 updateTabItems(session  , inputId =  "readData" , selected =  "nuagepoint" )
 observe({
   updateSelectInput(session, "yasis", choices = names(data$table))
 })
 
 output$nuage <- renderPlot( {
   
   ggplot(data$table, aes_string(x = input$xasis, y = input$yasis, color = input$color)) +  
     geom_point() + labs(title = "Nuage de points interactif",
                         x = input$xasis,
                         y = input$yasis,
                         color = input$color)  + theme_classic() 
 }) 
 
 
 updateTabItems(session  , inputId =  "readData" , selected =  "Graphiques" )
 observe({
   updateSelectInput(session, "y_asis", choices = names(data$table))
 })
 
output$bar <- renderPlot({
  y_asis <- table(data$table[, input$y_asis])
  bins <- seq(min(y_asis), max(y_asis))
  barplot(y_asis, breaks = bins, border = "black", main = input$y_asis) 
})
output$tex <- renderText({
  paste("nous avons réalisé le diagramme en bar de la variable :", input$y_asis)
})

updateTabItems(session  , inputId =  "readData" , selected =  "Graphiques" )
observe({
  updateSelectInput(session, "x", choices = names(data$table))
})

output$Histogramme <- renderPlot({
  x <-  data$table[, input$x]
  bin <- seq(min(x), max(x), length.out = input$bin + 1) 
  hist(x, breaks = bin, border = "black", main = paste("Histogramme de ", input$x )  , coul = "blue")
}) 

 
   updateTabItems(session  , inputId =  "readData" , selected =  "boxplot" )
 observe({
   updateSelectInput(session, "variable_x", choices = names(data$table))
 })
 output$boxplot <- renderAmCharts({ 
   amBoxplot(data$table[, input$variable_x])
 })
 output$texte <- renderText({
   paste("Boxplot de la variable : ", input$variable_x)
 })
 
 updateTabItems(session  , inputId =  "readData" , selected =  "regression" )
 observe({
   updateSelectInput(session, "dependant", choices = names(data$table))
 })
 
 updateTabItems(session  , inputId =  "readData" , selected =  "regression" )
 observe({
   updateSelectInput(session, "independant", choices = names(data$table))
 })
 
 model <- reactive({
   formulaa <- paste(input$dependant, "~",  paste(input$independant, collapse = "+"))
   lm(formulaa, data = data$table)
 })
 output$regression <- renderPrint( {
   summary(model())
   
   
 } )
 
 output$plot <- renderPlot({
   plot(summary(model()))
 })
 
 
 
   } 

shinyApp(ui, server)
