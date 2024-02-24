
library('shiny') 
library('bs4Dash')
library('shinyWidgets')

library('rhandsontable')
library('readxl')
library('ggplot2')     # 画图
library("showtext") #中文问题
showtext_auto()

library('shiny.i18n') # 语言切换

source("module/roc.R",encoding = "utf-8")

# File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")
lang$set_translation_language("cn") # here you select the default translation to display

ui <- bs4DashPage(
  preloader <- list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
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
                menuItem(lang$t("ROC 曲线"), tabName = "roc", icon = ionicon(name="bed"),selected = T )
   
    )
  ),
  footer = dashboardFooter(
    left = a(
      href = "https://shiny.chcmu.com.cn/graphmed",
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
      tabItem(tabName= "roc",        rocUI("roc"))
 
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$lang, {
    if(input$lang==F){ 
      shiny.i18n::update_lang('cn' )
    }
    else{
      shiny.i18n::update_lang('en' )
    }
  })

  rocServer("roc")

}

shinyApp(ui, server)

