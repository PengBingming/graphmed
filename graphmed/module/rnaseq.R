
rnaseqUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "RNAseq：limma", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "limma_pca.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/limma/")
      ),
      box(title = "RNAseq：DESeq2", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "deseq2_volcano.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/deseq2/")
      ),
      box(title = lang$t("表达箱图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "expr_boxplot.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/expr/")
      ),
      box(title = "PCA", width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "pca.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/pca/")
      ),
      box(title = lang$t("热图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "heatmap.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/heatmap/")
      ),
      box(title = lang$t("火山图"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "volcano.png" , width="100%"),
                 href="https://shiny.chcmu.com.cn/volcano/")
      ),
      box(title = lang$t("基因 ID 转换"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "geneid.jpg" , width="100%"),
                 href="https://shiny.chcmu.com.cn/geneID/")
      ),
      box(title = lang$t("基因去重复"), width=4, background = "white",height = 400,
          solidHeader=TRUE,status='primary',
          tags$a(tags$img(src = "reps.jpg" , width="100%"),
                 href="https://shiny.chcmu.com.cn/reps/")
      )
    )
  )
}