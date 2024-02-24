
# library('rhandsontable')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

color_type <- c("npg","aaas","nejm","gsea","lancet", "rickandmorty","futurama", "tron",
                "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

anova_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title=lang$t("Help"),solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，列名大小写保持一致，不要改变。")),
                 tags$h6(lang$t("2、首先选择两组还是多组（参数设置）。")),
                 tags$h6(lang$t("3、两组可选择t检验和秩和检验，多组可选择方差分析和秩和检验。")),
                 tags$h6(lang$t("4、多组还可得到两两比较的结果。"))
                 
    )
    ) )
  
}

anovaUI <- function(id) {
    ns <- NS(id)
    shiny.i18n::usei18n(lang)
    tagList(
      bs4Dash::tabsetPanel(
        tabPanel(title = 'Data',
                 fluidRow(
                   box(title=lang$t("Input data"),width=9,solidHeader=TRUE,status='primary',background = "white",
                       splitLayout(cellWidths = c("100%"), rhandsontable::rHandsontableOutput(ns("table") ) ) ),
                   box(width = 3,status="success",
                       fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
                       h6('Format: .csv .xlsx .xls'),
                       actionBttn( inputId = ns("show"), label = "Show Data",
                                   style = "fill", color = "primary", size = "sm" ),
                       hr(),
                       dropdownButton(circle=FALSE, label=lang$t("Parameter settings"), br(),br(),
                                      selectInput(inputId = ns('num'),label = lang$t('number of groups'),
                                                  c("two group" = 'two', "multi group" = 'multi'), selected = 'multi' ),
                                      selectInput(inputId = ns('method'), label = lang$t('compare methods'), selected = 't_anova',
                                                  c("t.test/anova" = 't_anova', "wilcox" = 'wilcox')),
                                      selectInput(inputId = ns('pair'),label = lang$t('paired or not'), 
                                                  c("pair" = 'T', "unpair" = 'F'),selected = 'F' ),
                                      selectInput(inputId = ns('alt'),label = lang$t('alternative'), selected = 'two.sided', 
                                                  c("two.sided" = 'two.sided', "less" = 'less', "greater"="greater") ),
                                      selectInput(ns('p.level'),"p.level",selected = '0.001',choices = c("0.001","NULL")) ,
                                      selectInput(inputId = ns('padj'),label = lang$t('correction method'),selected = 'bonferroni' , 
                                                  c("Bonferroni" = 'bonferroni',  "Holm" = 'holm','hochberg'='hochberg',
                                                    'BH'='BH', 'BY'='BY','fdr'='fdr', 'hommel'='hommel',"none"="none") ),
                                      h6(lang$t("(Correction method for comparison when there are differences between multiple groups)")) 
                                      ),br(),
                       downloadButton(ns("downloadSampleData"),lang$t("Sample data")) ) )
                 ),
        tabPanel(title = 'Plot',
                 fluidRow(
                   box(title=lang$t("图形"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
                   box(width=3,status="success",
                       actionBttn( inputId = ns("submit"), label = "Analyze Data",
                                   style = "fill", color = "primary", size = "sm" ),hr(),
                       dropdownButton(circle=FALSE,icon=icon('location-pin'), label=lang$t("dot line size"),  br(),br(),
                                      numericInput(inputId = ns('point'),label = lang$t('scatter'), value = 0.5,  min = 0,max = 5),
                                      numericInput(inputId = ns('errorbar'), label = lang$t('error bars'), value = 0.5,   min = 0,  max = 2) ,
                                      numericInput(inputId = ns('box'), label = lang$t('boxplot lines'), value = 0.5,min = 0,max = 2 ) ,
                                      numericInput(inputId = ns('violin'), label = lang$t('violin lines'), value = 0.5, min = 0, max = 2 ) ,
                                      numericInput(inputId = ns('star_size'), label = lang$t('star size'),max = 20,min = 0,value = 5 ) ), br(),
                       dropdownButton(circle=FALSE, label=lang$t("Graphic labels"),icon = icon('image'),  br(),br(),
                                      textInput(inputId = ns('title'), label = lang$t('title'),value ="title" ),
                                      textInput(inputId = ns('value'), label = lang$t('ylab'), value ='value' ),
                                      textInput(inputId = ns('group'), label = lang$t('xlab'), value ='group' ),
                                      selectInput(inputId = ns('star'), label = lang$t('difference bar'),c('show'='T','hide'="F"),
                                                  selected = "T" ),
                                      selectInput(ns('theme'),lang$t('Theme'),selected = 'bw',choices = theme_select ) ,
                                      selectInput(ns("color_type"), 'color_type', color_type ) ),br(),
                       dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                                      br(),br() ,
                                      numericInput(inputId = ns('w'),label = lang$t('plot.weight'),value = 15),
                                      numericInput(inputId = ns('h'),label = lang$t('plot.high'),value = 15),
                                      numericInput(inputId = ns('ppi'),label = lang$t('plot.dpi'),value = 72),
                                      downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                                      downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE )
                       )  ) )
        ),
        tabPanel(title = 'Result',
                 fluidRow(
                   box(title=lang$t("Result data"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"), dataTableOutput(ns("test_table") ) )  ),
                   box(width=3,
                       downloadButton(ns("downloadcontent1"), lang$t("Result data")) ),
                   box(title=lang$t("Two groups comparison"),width=9,solidHeader=TRUE,status = "primary",background = "white",
                       splitLayout(cellWidths = c("100%"), dataTableOutput(ns("stat.test") )  )  ),
                   box(width=3,solidHeader=TRUE,
                       downloadButton(ns("downloadcontent2"), lang$t("Two groups comparison")) )
                 ) # fluidRow
        ),
        tabPanel(title = 'Help',anova_helpUI("anova"))
        )
    ) # tagList
} # function(id)
