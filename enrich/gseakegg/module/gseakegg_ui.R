
library('DT')
source('module/global.R')

geneName <- c("ACCNUM","ALIAS","ENSEMBL","ENSEMBLPROT","ENSEMBLTRANS","ENTREZID","GO",
              "ENZYME","EVIDENCE","EVIDENCEALL","GENENAME","GENETYPE","GOALL","IPI",
              "ONTOLOGY","ONTOLOGYALL","PATH","PFAM","PMID","PROSITE","REFSEQ",
              "SYMBOL","UNIPROT" )
# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

gseakeggUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("DEG") ) ) 
                 ),
                 box(width = 3,status="success",
                     fileInput(inputId = ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",style = "fill",
                                 color = "primary", size = "sm" ), hr(),
                     fluidRow(
                       column(width = 6,selectInput(inputId = ns("ID"),label = h6(lang$t('GeneID'),style = "color:orange"),c("")) ),
                       column(width = 6,selectInput(inputId = ns("species"), h6(lang$t("Species"),style = "color:orange"),selected = "hsa",
                                                    choices = c('human'= "hsa", 'mouse' = "mmu", "rat" ="rat") )),
                       column(width = 12, selectInput( inputId = ns("logFC_col"), label = h6(lang$t('logFC column'),style = "color:orange"),  c("")))
                     ),
                     downloadButton(outputId = ns("downloadSampleData"), lang$t("Sample data"))
                 ) ) 
               ),
      tabPanel(title = 'GSEA',
               fluidRow(
                 box(title=lang$t("GSEA analysis"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),
                                 shinycssloaders::withSpinner(
                                   plotOutput(ns("plot_gsea0"),height = 500 ) 
                                 ) )
                     ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("geneid"),label = h6(lang$t('ID.geneName'),style = "color:orange"), c(geneName,"unknown") , selected = "SYMBOL"),
                     selectInput( inputId = ns("plot_gsea_id1"), lang$t("Graphic selection"),c( "p_dot","p_bar","p_emap") ),
                     numericInput(inputId = ns("gsea_num0"),  label = lang$t('Number of pathways'), value = 5 ), 
                     dropdownButton(circle=FALSE,label=lang$t("Enrich parameters"),  br(),br(),
                                    numericInput(inputId = ns("gsea_min"),  label = 'minGSSize', value = 10 ), 
                                    numericInput(inputId = ns("gsea_max"),  label = 'maxGSSize', value = 500 ),
                                    numericInput(inputId = ns("pvalue_gsea"),  label = lang$t('Pathway pvalue'), value = 1 )
                     ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w0'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h0'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi0'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata0"), label = "Rdata", size='sm', block=TRUE )
                     )
                 ) )
      ),
      tabPanel(title = 'Pathways',
               fluidRow(
                 box(title=lang$t("Select pathways"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot_gsea1"),height = 500 ) ) ),
                 box(width = 3,status="success",
                     actionBttn(inputId = ns("submit_gsea1"), lang$t("Start drawing"),style = "fill",
                                color = "primary", size = "sm"),hr(),
                     selectInput(inputId = ns("gsea_num1"),  label = lang$t('Select pathways'), c(""), multiple = T ) ,br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w1'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h1'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi1'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf1") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png1") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg1"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff1"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata1"), label = "Rdata", size='sm', block=TRUE )
                     )
                 ) )
      ),
      tabPanel(title = 'Genes',
               fluidRow(
                 box(title=lang$t("Pathway Genes"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot_gsea2"),height = 500 ) ) ),
                 box(width = 3,status="success",
                     actionBttn(inputId = ns("submit_gsea2"), lang$t("Start drawing"),style = "fill", 
                                color = "primary", size = "sm"),hr(),
                     selectInput(inputId = ns("gsea_kk_gse"), lang$t("Select pathways"), c(""), multiple = T ) ,
                     selectInput( ns("plot_gsea_id2"),lang$t("Graphic selection"),  c("p_gsea",'cnetplot'='p1', 'cnetplot_circ'= 'p2') ) , br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w2'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h2'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi2'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf2") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png2") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg2"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff2"), label = "TIFF", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("rdata2"), label = "Rdata", size='sm', block=TRUE )
                     ) )
               ) # fluidRow
      ),
      tabPanel(title = 'Help',helpUI("gseakegg"))
      )
  ) # NS(id)
} # function(id) 
