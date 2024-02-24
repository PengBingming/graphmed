
library('rhandsontable')
source('module/global.R')

# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

qpcrUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data', dataUI("qpcr") ),
      tabPanel(title = 'Result', resultUI("qpcr") ),
      tabPanel(title = 'Plot', plotUI("qpcr") ),
      tabPanel(title = "Help", helpUI("qpcr") )
    )
  ) # NS(id)
} # function(id)
