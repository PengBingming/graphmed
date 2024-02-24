
clinicUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = lang$t("单因素差异分析"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "anova.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/anova/")
      ),
      box(title = lang$t("两变量相关性"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "cor.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/lm/")
      ),
      box(title = lang$t("Logistics 回归"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "logisfp.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/logistics/")
      ),
      box(title = "pROC", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "proc.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/proc/")
      ),
      box(title = "mutliROC", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "mutliroc.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/mutliroc/")
      ),
      box(title = lang$t("生存分析"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "surv.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/surv/")
      ),
      box(title = lang$t("简易森林图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "forestplot.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/forestplot/")
      ),
      box(title = "rose plot", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "rose.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/rose/")
      ),
      box(title = lang$t("中国地图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "map.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/map/")
      ),
      box(title = lang$t("省市地图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "provincemap.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/provincemap/")
      ),
      box(title = lang$t("世界地图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "worldmap.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/worldmap/")
      )
    )
  )
}