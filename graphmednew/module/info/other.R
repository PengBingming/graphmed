

SeuratV3WizardUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    fluidRow(
    box(width = 4, title= "SeuratV3Wizard", background = "white",height = 400,
        solidHeader=TRUE,status='primary',
        tags$a(tags$img(src = "other_pc.jpg" , width="100%",height="100%"),
               href="https://shiny.chcmu.com.cn/SeuratV3Wizard/shiny/")
    ),
    box(title = "ggplotGUI", width=4, background = "white",height = 400,
        solidHeader=TRUE,status='primary',
        tags$a(tags$img(src = "ggplotGui.jpg" , width="100%"),
               href="https://shiny.chcmu.com.cn/ggplot2/")
    )
    )
  )
}

