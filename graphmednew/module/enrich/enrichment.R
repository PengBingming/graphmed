
enrichmentUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = lang$t("KEGG 分析"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "kegg.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/kegg/")
      ),
      box(title = lang$t("GSAE 分析(KEGG 库)"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "gseakegg.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/gseakegg/")
      ),
      box(title = lang$t("GO 分析"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "go.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/go/")
      ),
      box(title = lang$t("GSEA 分析(GO库)"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "gseago_cnet.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/gseago/")
      ),
      box(title = lang$t("ORA 分析"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ora.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ora/")
      ),
      box(title = lang$t("通路气泡图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "pathwaybubble.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/pathwaybubble/")
      )
    )
  )
}