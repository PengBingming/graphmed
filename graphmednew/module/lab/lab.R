
labUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 4, title= lang$t("Elisa 分析"), background = "white",height = 400,
            solidHeader=TRUE,status='primary',
            tags$a(tags$img(src = "elisa.png" , width="100%",height="100%"),
                   href="https://shiny.chcmu.com.cn/elisa/")
            ),
      box(width = 8, title=lang$t("Q-PCR 结果分析"), background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          fluidRow(
            column(width = 6,
                   tags$a(tags$img(src = "qpcr_table.jpg" , width="100%"),
                          href="https://shiny.chcmu.com.cn/qpcr/") ),
            column(width = 6,
                   tags$a(tags$img(src = "qpcr.png", width="100%"),
                          href="https://shiny.chcmu.com.cn/qpcr/") )
          )
          ),
      box(width = 4, title= "Mouse lung function", background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "lungfunction.png" , width="100%",height="100%"),
                 href="https://shiny.chcmu.com.cn/lung")
      )
      )

    )
}


