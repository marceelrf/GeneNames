library(shiny)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Rn.eg.db)
library(dplyr)
library(BiocManager)
library(tidyr)
library(purrr)

options(repos = BiocManager::repositories())

ui <- fluidPage(
  includeCSS(path = "style.css"),
  fluidRow(id = "titulo",
    h1("Gene Names checker - Marshall's tools")
  ),
  fluidRow(
    column(width = 3,
         wellPanel(
           h2(("Enter with your genes symbols")),
           textAreaInput(inputId = "genesIn",label = "",
                     value = "",
                     placeholder = "Enter your genes separated by spaces or enters."),
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
           tabsetPanel(
             tabPanel("Main",
                      textOutput(outputId = "textOut"),
                      dataTableOutput(outputId = "finalTable"),
                      hr(),
                      h3("Download"),
                      downloadButton('download',"Download your table")),
             tabPanel("How to use",includeHTML(path = "howtouse.html")),
             tabPanel("About",includeHTML(path = "about.html"))
           )

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
      stringr::str_split_1(string = input$genesIn,pattern = "[:blank:]|\n")
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
    if ((input$genesIn) =="") {
      tibble("SYMBOL","ENTREZID","ENSEMBL","ALIAS")
    } else  if(is.null(Annot())) {
      return(NULL)
    } else {
      Annot() %>%
        group_by(SYMBOL, ENTREZID,ENSEMBL) %>%
        nest() %>%
        mutate(ALIAS = map(data, ~paste(.x$ALIAS,collapse = ", "))) %>%
        unnest(ALIAS) %>%
        select(-data) %>%
        ungroup()
      }
  })

  output$download <- downloadHandler(
    filename = function(){"TableOfYourGenes.csv"},
    content = function(fname){
      write.csv(Annot() %>% dplyr::select(-ALIAS) %>% dplyr::distinct()
                , fname,row.names = F)
    }
  )
}

shinyApp(ui, server)
