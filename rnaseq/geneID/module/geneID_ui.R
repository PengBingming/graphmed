
# library('rhandsontable')
library('DT')
source('module/global.R')
geneName <- c("ACCNUM","ALIAS","ENSEMBL","ENSEMBLPROT","ENSEMBLTRANS","ENTREZID","GO",
              "ENZYME","EVIDENCE","EVIDENCEALL","GENENAME","GENETYPE","GOALL","IPI",
              "ONTOLOGY","ONTOLOGYALL","PATH","PFAM","PMID","PROSITE","REFSEQ",
              "SYMBOL","UNIPROT" )
lang <- Translator$new(translation_csvs_path = "./lang/info/")

geneIDUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("exp_orgin") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data", 
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(inputId = ns("species"), lang$t("species"),selected = "hsa",
                                 choices = c('human'= "hsa", 'mouse' = "mmu", "rat" ="rat") ),
                     downloadButton(ns("downloadSampleData"), lang$t("Sample data")) 
                 ) )
               ),
      tabPanel(title = 'Result',
               fluidRow(
                 box(title=lang$t("Result data"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),dataTableOutput(ns("exp_change"),height = 500 ) ) ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data", 
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("from_geneName"), lang$t("ID.geneName"), c(geneName,"unknown") , selected = "SYMBOL"),
                     selectInput(ns("to_geneName")  , lang$t("to.geneName")  , geneName , selected = "ENTREZID"),
                     downloadButton(ns("downloadResultData"), lang$t("Result data"))
                 )
               ) # fluidRow
      ),
      tabPanel(title = 'Help',helpUI("geneID") )
      )
  ) # tagList
} # function(id)

