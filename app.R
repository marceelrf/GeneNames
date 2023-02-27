library(shiny)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Rn.eg.db)
library(dplyr)


ui <- fluidPage(
  includeCSS(path = "style.css"),
  fluidRow(id = "titulo",
    h1("Gene Names checker")
  ),
  fluidRow(
    column(width = 3,
         wellPanel(
           h2(("Enter with your genes symbols")),
           textAreaInput(inputId = "genesIn",label = "",
                     value = "",
                     placeholder = "Enter your genes separated by tabs or spaces."),
           hr(),
           h2("Choose the identifier of your genes"),
           selectInput(inputId = "indGene",label = "",
                       choices = c("SYMBOL","ENTREZID","ENSEMBL"),
                       selected = "SYMBOL"),
           hr(),
           h2("Choose the species of your genes"),
           selectInput(inputId = "species",label = "",
                       choices = c("Human","Mouse","Rat"),selected = "Human"),
           hr()
           )
         ),
    column(width = 8,offset = 1,
           textOutput(outputId = "textOut"),
           dataTableOutput(outputId = "finalTable")
           )
    )
)

server <- function(input, output, session) {
  #Which Database to use
  db <- reactive({
    if(input$species == "Human") {
      org.Hs.eg.db
    } else {
      if(input$species == "Mouse") {
        org.Mm.eg.db
      } else {
        org.Rn.eg.db
      }
    }
  })

  #Get gene names
  Genes <- reactive({
    if(input$genesIn == ""){
      return(NULL)
    } else {
      stringr::str_split_1(string = input$genesIn,pattern = "[:blank:]")
    }
  })

  #Get Annotations
  Annot <- reactive({
    if(input$genesIn == ""){
      return(NULL)
    } else {
      Sys.sleep(1)
      AnnotationDbi::select(x = db(), #Database selected by user. Human is default
                            keys = Genes(),
                            columns = c("SYMBOL","ENTREZID","ENSEMBL","ALIAS"),
                            keytype = input$indGene)
    }
  })

  output$textOut <- renderText({
    glue::glue("Your gene list is from {input$species}")
  })

  output$finalTable <- renderDataTable({
    Annot()

  })
}

shinyApp(ui, server)
