
library('rhandsontable')
source('module/global.R')
theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')
# # File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

elisaUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data', dataUI("elisa") ),
      tabPanel(title = 'Result', resultUI("elisa")),
      tabPanel(title = 'Plot', plotUI("elisa") ),
      tabPanel(title = "Help", helpUI("elisa") )
      )
)
}

