
library('shiny') 
library('bs4Dash')
library('shinyWidgets')
library('shinycssloaders')
library('shinyFiles')
library('rhandsontable')

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

source("module/info.R",encoding = "utf-8")

source("module/enrichment.R",encoding = "utf-8")

source("module/rnaseq.R",encoding = "utf-8")

source("module/clinic.R",encoding = "utf-8")

source("module/ggplot2.R",encoding = "utf-8")

source("module/lab.R",encoding = "utf-8")

source("module/other.R",encoding = "utf-8")

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")

lang$set_translation_language("cn") # here you select the default translation to display

ui <- bs4DashPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  title = "GraphMed",
  fullscreen = T,
  header = bs4DashNavbar(
    shiny.i18n::usei18n(lang),
    column(width = 2, 
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
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    bs4SidebarUserPanel(
      image = "./doupi.jpg", 
      name = lang$t("欢迎使用 GraphMed")
    ),
    sidebarMenu(id="sidebar",
                sidebarHeader(title = lang$t("医学数据可视化") ),
                menuItem("Information", tabName = "info", icon = ionicon(name="information-circle")),
                
                menuItem("RNA-seq", tabName = "rnaseq", icon = ionicon(name="flask") ),
                
                menuItem("Enrichment",tabName ="enrichment", icon = ionicon(name="flask") ),
                         
                menuItem("Clinic tools", tabName = "clinic", icon = ionicon(name="bed")) ,
                
                menuItem('ggplot2', tabName = "ggplot2",icon = ionicon(name="image") ),
                
                menuItem("Lab. tools", tabName = "lab", icon = ionicon(name="flask") ),

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
      
      tabItem(tabName= "rnaseq",     rnaseqUI("rnaseq") ),
      
      tabItem(tabName= "enrichment", enrichmentUI("enrichment") ),
      
      tabItem(tabName= "clinic",     clinicUI("clinic") ),
      
      tabItem(tabName= "ggplot2",     ggplot2UI("ggplot2") ),
      
      tabItem(tabName= "lab",        labUI("lab") ),

      tabItem(tabName= "other", SeuratV3WizardUI("SeuratV3Wizard") )
      
    )
  )
)

server <- function(input, output, session) {
  waiter_hide()
  
  observeEvent(input$lang, {
    # observeEvent(input$selected_language, {
    if(input$lang==F){ 
      shiny.i18n::update_lang('cn' )
    }
    else{
      shiny.i18n::update_lang('en' )
    }
    
  })

}

shinyApp(ui, server)

