
# library('DT')
# library('shinycssloaders')
source('module/global.R')
geneName <- c("ACCNUM","ALIAS","ENSEMBL","ENSEMBLPROT","ENSEMBLTRANS","ENTREZID","GO",
              "ENZYME","EVIDENCE","EVIDENCEALL","GENENAME","GENETYPE","GOALL","IPI",
              "ONTOLOGY","ONTOLOGYALL","PATH","PFAM","PMID","PROSITE","REFSEQ",
              "SYMBOL","UNIPROT" )

# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

keggUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data',
               fluidRow(
                 box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("DEG") ) )
                 ),
                 box(width = 3,status="success",
                     fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                     h6('Format: .csv .xlsx .xls'),
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     fluidRow(
                       column(width = 6,selectInput(inputId = ns("ID"),h6(lang$t('GeneID'), style = "color:orange"),c("")) ),
                       column(width = 6,selectInput(inputId = ns("species"),h6(lang$t("Species"), style = "color:orange"),selected = "hsa",
                                   choices = c('human'= "hsa", 'mouse' = "mmu", "rat" ="rat") ) )
                       ),
                     selectInput(ns('type'),lang$t('Input data type'),selected = 'gene',
                                 choices = c('diff gene'='gene','diff matrix'='DEG')),
                     conditionalPanel(
                       condition = "input.type=='DEG'",ns = NS(id),
                       dropdownButton(circle=FALSE,label=lang$t("Matrix Parameters"),  br(),br(),
                                      fluidRow(
                                        column(width = 7, selectInput( inputId = ns("pvalue_col"),  lang$t('pvalue column'), c("")) ),
                                        column(width = 5, numericInput(inputId = ns("pvalue"),      label = "pvalue",    value = 0.05)),
                                        column(width = 7, selectInput( inputId = ns("padj_col"),    lang$t('padj column'),   c(""))),
                                        column(width = 5, numericInput(inputId = ns("padj")  ,      label = "padj"  ,    value = 0.1)),
                                        column(width = 7, selectInput( inputId = ns("logFC_col"),   lang$t('logFC column'),  c(""))),
                                        column(width = 5, numericInput(inputId = ns("logFC_cutoff"),label = "logFC",     value = 1)),
                                        column(width = 12, selectInput( inputId = ns("kegg_gene"),   lang$t("Genes of enrich"),  selected = 'diff',
                                                                       choices =  c('diff gene'='diff', 'up gene'='up','down gene'='down')  ) ))
                       ) ) ,hr(),
                     downloadButton(ns("downloadSampleData"), lang$t("Sample data"))
                 ) )
               ),
      tabPanel(title = 'KEGG',
               fluidRow(
                 box(title=lang$t("KEGG analysis"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),
                                 shinycssloaders::withSpinner(
                                   plotOutput(ns("plot_kegg0"),height = 500 )
                                 ) )
                     ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submit1"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ), hr(),
                     selectInput(ns("geneid"),label = h6(lang$t('ID.geneName'), style = "color:orange"), c(geneName,"unknown") , selected = "SYMBOL"),
                     selectInput(ns("plot_kegg_id1"), lang$t("Graphic selection"), choices = c("p_dot","p_bar", "p_emap") ),br(),
                     numericInput(inputId = ns("kegg_num0"),  label = lang$t('Number of pathways'), value = 10),
                     dropdownButton(circle=FALSE,label=lang$t("Enrich parameters"),  br(),br(),
                                    numericInput(inputId = ns("kegg_min"),   label = 'minGSSize', value = 10 ),
                                    numericInput(inputId = ns("kegg_max"),   label = 'maxGSSize', value = 500 ),
                                    numericInput(inputId = ns("pvalue_kegg"),label = lang$t('Pathway pvalue'), value = 1 ),
                                    numericInput(inputId = ns("padj_kegg"),  label = lang$t('Pathway padj'), value = 1 )
                     ),hr(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w0'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h0'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi0'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf0") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png0") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg0"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff0"), label = "TIFF", size='sm', block=TRUE )
                     )
                 ) )
      ),
      tabPanel(title = 'Pathways',
               fluidRow(
                 box(title=lang$t("Select pathways"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot_kegg1"),height = 500 ) ) ),
                 box(width = 3,status="success",
                     actionBttn(ns("submit_kegg1"), lang$t("Start drawing"),style = "fill",
                                color = "primary", size = "sm"),hr(),
                     selectInput( inputId = ns("kegg_num1"),  label = lang$t('Select pathways'), c(""),multiple = T ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w1'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h1'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi1'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf1") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png1") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg1"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff1"), label = "TIFF", size='sm', block=TRUE )
                     )
                 ) )
      ),
      tabPanel(title = 'Genes',
               fluidRow(
                 box(title=lang$t("Pathway Genes"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot_kegg2"),height = 500 ) ) ),
                 box(width = 3,status="success",
                     actionBttn(ns("submit_kegg2"), lang$t("Start drawing"),style = "fill",
                                color = "primary", size = "sm"),hr(),
                     selectInput(ns("kegg_kk"), lang$t("Select pathways"), c(""), multiple = T ),
                     selectInput( ns("plot_kegg_id2"), lang$t("Graphic selection"), c('cnetplot'='p1', 'cnetplot_circ'= 'p2') ),br(),
                     dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                    br(),br() ,
                                    numericInput(inputId = ns('w2'),label = lang$t('plot.weight'),value = 15),
                                    numericInput(inputId = ns('h2'),label = lang$t('plot.high'),value = 15),
                                    numericInput(inputId = ns('ppi2'),label = lang$t('plot.dpi'),value = 72),
                                    downloadBttn(outputId = ns("pdf2") , label = "PDF" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("png2") , label = "PNG" , size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("jpeg2"), label = "JPEG", size='sm', block=TRUE ),
                                    downloadBttn(outputId = ns("tiff2"), label = "TIFF", size='sm', block=TRUE )
                     )
                 )
               ) # fluidRow
      ),
      tabPanel(title = 'Help',helpUI("kegg"))
      )
  ) # NS(id)
} # function(id)
