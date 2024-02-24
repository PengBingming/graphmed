
library('shiny') 
library('bs4Dash')
library('shinyWidgets')
library('shinycssloaders')
library('shinyFiles')
library('rhandsontable')
library("pROC")
library("waiter") # 加载界面

library('DT')
library('shinyjs')
library('readxl')

library("tidyr") 
library("showtext") #中文问题
showtext_auto()

library('grid')
library('waiter')
library('shiny.i18n') # 语言切换

# source("module/info.R",encoding = "utf-8")
source_code <- lapply(dir('./module/info', pattern = '\\.R', full.names = TRUE),
                      function(x){
                        source(x,encoding = "utf-8")
                      }
) ; rm( source_code )

source_code <- lapply(dir('./module/rnaseq', pattern = '\\.R', full.names = TRUE),
                      function(x){
                        source(x,encoding = "utf-8")
                      }
) ; rm( source_code )

source_code <- lapply(dir('./module/enrich', pattern = '\\.R', full.names = TRUE),
                      function(x){
                        source(x,encoding = "utf-8")
                      }
) ; rm( source_code )

source_code <- lapply(dir('./module/clinic', pattern = '\\.R', full.names = TRUE),
                      function(x){
                        source(x,encoding = "utf-8")
                      }
) ; rm( source_code )

source_code <- lapply(dir('./module/ggplot', pattern = '\\.R', full.names = TRUE),
                      function(x){
                        source(x,encoding = "utf-8")
                      }
) ; rm( source_code )

source_code <- lapply(dir('./module/lab', pattern = '\\.R', full.names = TRUE),
                      function(x){
                        source(x,encoding = "utf-8")
                      }
) ; rm( source_code )

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

lang$set_translation_language("cn") # here you select the default translation to display

ui <- bs4DashPage(
  # preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  title = "GraphMed",
  fullscreen = T,
  header = bs4DashNavbar(
    shiny.i18n::usei18n(lang),
    column(width =  2, 
           materialSwitch(inputId = "lang",label = lang$t("中文"), 
                          status = "primary",value = F, right = T )
    ),
    title = dashboardBrand(
      title = lang$t("重医儿院"),
      color = "primary",
      href = "https://stu.chcmu.aisa",
      image = "./logo_chcmu.png", 
      opacity=1
    ),
    disable = FALSE,
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("table-cells"),
    fixed = FALSE,
    # Dropdown menu for notifications
    leftUi = dropdownMenu(type = "notifications", badgeStatus = "warning",
                          
                          notificationItem(icon = icon("user", lib = "glyphicon"),
                                           status = "danger", "Bingm"
                          ),
                          notificationItem(icon = icon("lungs",lib = "font-awesome"), status = "info",
                                           "Lab. of Pediatric Respir. Medicine",
                                           href = "https://stu.chcmu.asia"
                          ),
                          notificationItem(icon = icon("envelope", lib = "glyphicon"), status = "danger",
                                           "2020111042@stu.cqmu.edu.cn"
                          )
    )
    
  ),
  ## Sidebar content
  sidebar = bs4DashSidebar(width = 1,
    skin = "light",
    status = "primary",
    elevation = 2 ,
    bs4SidebarUserPanel(
      image = "./doupi.jpg", 
      name = lang$t("欢迎使用 GraphMed")
    ),
    sidebarMenu(id="sidebar",
                sidebarHeader(title = lang$t("医学数据可视化") ),
                menuItem("Information", tabName = "info", icon = ionicon(name="information-circle")),
                
                menuItem("RNA-seq", icon = ionicon(name="flask") ,
                         menuItem("RNAseq：limma", tabName = "limma", icon = ionicon(name="flask") ),
                         menuItem("RNAseq：DESeq2", tabName = "deseq2", icon = ionicon(name="flask") ),
                         menuItem(lang$t("表达箱图"), tabName = "expr", icon = ionicon(name="image") ),
                         menuItem("PCA", tabName = "pca", icon = ionicon(name="bed") ),
                         menuItem(lang$t("热图"), tabName = "heatmap", icon = ionicon(name="image") ),
                         menuItem(lang$t("火山图"), tabName = "volcano", icon = ionicon(name="image")),
                         menuItem(lang$t("基因 ID 转换"), tabName = "geneID", icon = ionicon(name="flask") ),
                         menuItem(lang$t("基因去重复"), tabName = "reps", icon = ionicon(name="flask") )

                         ),

                menuItem("Enrichment",icon = ionicon(name="flask"),
                         menuItem(lang$t("KEGG 分析"), tabName = "kegg", icon = ionicon(name="flask") ),
                         menuItem(lang$t("GSAE 分析(KEGG 库)"), tabName = "gseakegg", icon = ionicon(name="flask") ),
                         menuItem(lang$t("GO 分析"), tabName = "go", icon = ionicon(name="flask") ),
                         menuItem(lang$t("GSEA 分析(GO库)"), tabName = "gseago", icon = ionicon(name="flask") ),
                         menuItem(lang$t("ORA 分析"), tabName = "ora", icon = ionicon(name="flask") ),
                         menuItem("pathway bubble", tabName = "bubble", icon = ionicon(name="bed") )
                         ),

                menuItem("Clinic tools", icon = ionicon(name="bed"),
                         menuItem(lang$t("单因素差异分析"), tabName = "anova", icon = ionicon(name="bed") ),
                         menuItem(lang$t("两变量相关性"), tabName = "lm", icon = ionicon(name="bed") ),
                         menuItem(lang$t("Logistics 回归"), tabName = "logis", icon = ionicon(name="bed") ),
                         
                         menuItem("pROC single", tabName = "proc", icon = ionicon(name="bed") ),
                         menuItem("pROC multi", tabName = "proc_multi", icon = ionicon(name="bed") ),
                         menuItem("pROC logistic", tabName = "proc_logis", icon = ionicon(name="bed") ),
                         menuItem("multipleROC", tabName = "roc", icon = ionicon(name="bed") ),
                         menuItem(lang$t("生存分析"), tabName = "surv", icon = ionicon(name="bed") ),
                         menuItem(lang$t("简易森林图"), tabName = "forestplot", icon = ionicon(name="bed") ),
                         menuItem("rose plot", tabName = "rose", icon = ionicon(name="bed") ),
                         menuItem(lang$t("中国地图"), tabName = "map", icon = ionicon(name="bed") ),
                         menuItem(lang$t("省市地图"), tabName = "province", icon = ionicon(name="bed") ),
                         menuItem(lang$t("世界地图"), tabName = "worldmap", icon = ionicon(name="bed") )
                         ) ,

                menuItem('Basic plots',icon = ionicon(name="image"),
                         menuItem("Barplot", tabName = "barplot", icon = ionicon(name="bed") ),
                         menuItem("Errorbar", tabName = "errorbar", icon = ionicon(name="bed") ),
                         menuItem("Boxplot", tabName = "boxplot", icon = ionicon(name="image") ),
                         menuItem("Violin", tabName = "violin", icon = ionicon(name="image")  ),
                         menuItem("Scatter", tabName = "scatter", icon = ionicon(name="image")),
                         menuItem("Merge"  , tabName = "merge", icon = ionicon(name="image")  ),

                         menuItem("Histogram", tabName = "histogram", icon = ionicon(name="image") ),
                         menuItem("Density", tabName = "density", icon = ionicon(name="image") ),
                         menuItem("Dotplot", tabName = "dotplot", icon = ionicon(name="image")),
                         menuItem("ggplot", tabName = "ggplot", icon = ionicon(name="image"))
                ),
                
                menuItem("Lab. tools", icon = ionicon(name="flask"),
                         menuItem(lang$t("Elisa 分析"), tabName = "elisa", icon = ionicon(name="flask")),
                         menuItem(lang$t("Q-PCR 结果分析"), tabName = "qpcr", icon = ionicon(name="flask")),
                         menuItem('Lung function', tabName = "lung", icon = ionicon(name="flask")  )
                         ),

                menuItem("Someone else's tools", tabName = "other", icon = ionicon(name="flask") ) 
   
    )
  ),
  footer = dashboardFooter(
    left = a(
      href = "https://shiny.chcmu.com.cn/graphmed/",
      target = "_blank", "GraphMed |" ,a(
        href = "https://beian.miit.gov.cn/",
        target = "_blank", "渝ICP备2023006607号"
      ),
    ),
    right = "@2023"
    
  ),
  controlbar = dashboardControlbar(),
  body  = dashboardBody(
    tabItems(
      tabItem(tabName= "info",       infoUI("info")),
      
      # rnaseq
      tabItem(tabName= "limma",     limmaUI("limma")),
      tabItem(tabName= "deseq2",    deseq2UI("deseq2")),
      tabItem(tabName= "reps",      repsUI("reps")),

      tabItem(tabName= "pca",       pcaUI("pca")),
      tabItem(tabName= "heatmap",   heatmapUI("heatmap")),
      tabItem(tabName= "volcano",   volcanoUI("volcano")),

      tabItem(tabName= "geneID",    geneIDUI("geneID")),
      tabItem(tabName= "expr",      exprUI("expr")),
      
      # enrichment
      tabItem(tabName= "kegg",      keggUI("kegg")),
      tabItem(tabName= "gseakegg",  gseakeggUI("gseakegg")),
      tabItem(tabName= "go",        goUI("go")),
      tabItem(tabName= "gseago",    gseagoUI("gseago")),
      tabItem(tabName= "ora",       oraUI("ora")),
      tabItem(tabName= "bubble",    bubbleUI("bubble")),
      
      # clinic
      tabItem(tabName= "anova",      anovaUI("anova")),
      tabItem(tabName= "lm",         lmUI("lm")),
      tabItem(tabName= "logis",      logisUI("logis")),
      
      tabItem(tabName= "proc",       procUI("proc")),
      tabItem(tabName= "proc_multi", roc_multiUI("proc_multi")),
      tabItem(tabName= "proc_logis",   roc_logiUI("proc_logis")),
      
      tabItem(tabName= "roc",        rocUI("roc")),
      tabItem(tabName= "surv",       survUI("surv")),
      tabItem(tabName= "forestplot", forestplotUI("forestplot")),

      tabItem(tabName= "rose",       roseUI("rose")),
      tabItem(tabName= "province",   provinceUI("province")),
      tabItem(tabName= "map",        mapUI("map")),
      tabItem(tabName= "worldmap",   worldmapUI("worldmap")),
     

      # ggplot
      tabItem(tabName= "barplot",    barplotUI("barplot")),
      tabItem(tabName= "errorbar",   errorbarUI("errorbar")),
      tabItem(tabName= "boxplot",    boxplotUI("boxplot")),
      tabItem(tabName= "violin",     violinUI("violin")),
      tabItem(tabName= "scatter",    scatterUI("scatter")),
      tabItem(tabName= "merge",     mergeUI("merge")),
      
      tabItem(tabName= "histogram",     histogramUI("histogram")),
      tabItem(tabName= "density",     densityUI("density")),
      tabItem(tabName= "dotplot",     dotplotUI("dotplot")),

      tabItem(tabName= "ggplot",     ggplotUI("ggplot")),
      
      # lab
      tabItem(tabName= "elisa",      elisaUI("elisa") ),    
      tabItem(tabName= "qpcr",       qpcrUI("qpcr")),
      tabItem(tabName= "lung",       lungUI("lung")),
      
      # other
      tabItem(tabName= "other", SeuratV3WizardUI("SeuratV3Wizard") )
      
    )
  )
)

server <- function(input, output, session) {
  waiter_hide()
  
  observeEvent(input$lang, {
    # observeEvent(input$selected_language, {
    if(input$lang==T){ 
      shiny.i18n::update_lang('cn' )
    }
    else{
      shiny.i18n::update_lang('en' )
    }
    
  })
  
  limmaServer("limma")
  deseq2Server("deseq2")
  exprServer("expr")
  
  pcaServer("pca")
  heatmapServer("heatmap")
  volcanoServer("volcano")

  geneIDServer("geneID")
  repsServer("reps")

  keggServer("kegg")
  gseakeggServer("gseakegg")
  goServer("go")
  gseagoServer("gseago")
  oraServer("ora")
  bubbleServer("bubble")

  anovaServer("anova")
  lmServer("lm")
  logisServer("logis")
  
  procServer("proc")
  roc_multiServer("proc_multi")
  roc_logiServer("proc_logis")
   
  rocServer("roc")
  survServer("surv")
  forestplotServer("forestplot")
  
  roseServer("rose")
  mapServer("map")
  provinceServer("province")
  worldmapServer("worldmap")

  
  barplotServer("barplot")
  errorbarServer("errorbar")
  boxplotServer("boxplot")
  violinServer("violin")
  scatterServer("scatter")
  mergeServer("merge")
  
  histogramServer("histogram")
  densityServer("density")
  dotplotServer("dotplot")

  ggplotServer("ggplot")
  
  elisaServer("elisa") 
  qpcrServer("qpcr")

}

shinyApp(ui, server)

