library(shiny)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Rn.eg.db)
library(dplyr)
library(BiocManager)
library(tidyr)
library(purrr)
library(stringr)

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
                      uiOutput("genesNOT"),
                      h3("Download"),
                      downloadButton('download',"Download your table")),
             tabPanel("Aliases",
                      h1("Coming soon")),
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
      tibble(Genes = stringr::str_split_1(string = input$genesIn,
                           pattern = "[:blank:]|\n|[:space:]")) %>%
        filter(!is.na(Genes)) %>%
        pull(Genes)
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
        dplyr::filter(str_detect(SYMBOL,pattern = regex("[:blank:]|\n|[:space:]"),
                                 negate = T)) %>%
        group_by(SYMBOL, ENTREZID,ENSEMBL) %>%
        nest() %>%
        mutate(ALIAS = map(data, ~paste(.x$ALIAS,collapse = ", "))) %>%
        unnest(ALIAS) %>%
        select(-data) %>%
        ungroup() %>%
        dplyr::filter(!is.na(ALIAS))
      }
  })

  output$download <- downloadHandler(
    filename = function(){"TableOfYourGenes.csv"},
    content = function(fname){
      write.csv(Annot() %>% dplyr::select(-ALIAS) %>% dplyr::distinct()
                , fname,row.names = F)
    }
  )

  ##Output of the Genes not matched
  output$genesNOT <- renderUI({

    if(input$genesIn == ""){
      return(NULL)
    } else {
      textOutput(outputId = "textgenesNOT")

      Annot_mod <- Annot() %>%
        filter(!is.na(ALIAS))

      n_out <- (length(unique(Genes())) - length(unique(Annot_mod$SYMBOL)[unique(Annot_mod$SYMBOL) %in% unique(Genes())]))

      Notgenes <- str_subset(string = Genes(),
                             pattern = paste0(Annot_mod$SYMBOL,
                                              collapse = "|"),
                             negate = T)


      Notgenes <- paste0(Notgenes,collapse = "\n")

      gtest <- paste0(Genes(),collapse = ", ")
      output$textgenesNOT <- renderText({
        glue::glue("Your gene list contains {n_out} unmapped gene.
                   {Notgenes} of {gtest}")

    })
      }
    })
}

shinyApp(ui, server)
