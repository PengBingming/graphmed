
library('shiny') 
library('bs4Dash')
library('shinyWidgets')
# library('shiny.i18n') # 语言切换

source("module/anova_ui.R",encoding = "utf-8")

# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")
lang$set_translation_language("en") # here you select the default translation to display

ui <- bs4DashPage(
  fullscreen = T,
  header = bs4DashNavbar(
    shiny.i18n::usei18n(lang),
    column(width = 2, br(),
           materialSwitch(inputId = "lang",label = lang$t("English"), 
                          status = "primary",value = F, right = T )
    ),
    disable = FALSE, skin = "light",  status = "white",  border = T, fixed = F
  ),
  ## Sidebar content
  sidebar = bs4DashSidebar(disable = T ),
  # controlbar = dashboardControlbar(),
  body  = dashboardBody(
    anovaUI("anova")
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$lang, {
    if(input$lang==F){ 
      shiny.i18n::update_lang('en' )
    }
    else{
      shiny.i18n::update_lang('cn' )
    }
  })
  
  source('./module/anova_server.R')
  anovaServer("anova")
}

shinyApp(ui, server)
