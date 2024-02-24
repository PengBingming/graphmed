
ggplot2UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Barplot", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggbarplot.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggbarplot/")
      ),
      box(title = "Errorbar", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggerrorbar.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggerrorbar/")
      ),
      box(title = "Boxplot", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggboxplot.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggboxplot/")
      ),
      box(title = "Violin", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggviolin.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggviolin/")
      ),
      box(title = "Scatter", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggscatter.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggscatter/")
      ),
      box(title = "Merge: boxplot + violin + scatter", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggmerge.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggmerge/")
      ),
      box(title = "Histogram", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "gghistogram.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/gghistogram/")
      ),
      box(title = "Density", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggdensity.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggdensity/")
      ),
      box(title = "Dotplot", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggdotplot.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggdotplot/")
      ),
      box(title = "ggplot", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "ggplot.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/ggplot/")
      )
    )
  )
}