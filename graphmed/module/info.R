# # File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")


infoUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  
  bs4Dash::tabsetPanel(
    tabPanel(title = 'Overview',
             tabItem(tabName= "help",        helpUI("help"))
    ),
    tabPanel(title = "Lab.'s website",
             tabItem(tabName= "contact",        contactUI("contact"))
    ),
    tabPanel(title = 'FAQ',
             tabItem(tabName= "faq",        faqUI("faq"))
    )
  )
}

contactUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  fluidRow(box(width=12,
               title="联系我们",solidHeader=TRUE,status='primary',background = "white",height=2000,
               tags$h3("重庆医科大学附属儿童医院呼吸病学研究室(非官方)"),
               tags$a(href="http://stu.chcmu.asia", "Laboratory of Pediatric Respiratory Medicine,
                           Children's Hospital of Chongqing Medical University"),
               tags$hr(),
               tags$p("重庆医科大学附属儿童医院呼吸专业成立于80年代初，
                    1992年获重庆市儿童哮喘防治中心，
                    2005年成立重庆医科大学小儿过敏性疾病诊疗中心，
                    2007年成立重庆医科大学附属儿童医院呼吸中心，
                    2008年获重庆市医学专业重点学科，2011年获得卫生部临床重点专科。
                    目前正致力于建设成为西部一流、国内领先的小儿呼吸系统疾病
                    诊断、治疗、研究与培训中心。（官网摘抄！）"),
               tags$iframe(src="https://stu.chcmu.asia/",
                           width="100%", height="100%", id="fullb",
                           seamless="seamless", scrolling="auto",
                           frameborder="0", allowfullscreen="true" )

  )
  )
}

helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title="Workflow", width=7, height=600, background = "white", 
          status='primary', solidHeader=TRUE,
          tags$img(src = "tools.jpg", width = "95%",style= "display:block;margin:auto;")
      ),
      box(width=5,title="使用说明",solidHeader=TRUE,status='primary',
          background = "white",height=600,
          tags$h3("GitHub"),
          tags$a(href="https://github.com/PengBingming/GraphMed",h6("https://github.com/PengBingming/GraphMed")),
          tags$hr(),
          tags$h3("Latest version"),
          tags$a("https://shiny.chcmu.com.cn/graphmed/",href="https://shiny.chcmu.com.cn/graphmed/"),
          tags$hr(),
          # tags$video(src="./video/elisa.mp4",type = "video/mp4",width="80%"),
          tags$h3("使用说明"),
          tags$h6(lang$t("1、参考参考数据：点击“show data”，或下载各自工具的对应参考数据；")),
          tags$h6(lang$t("2、上传数据，格式为 csv/xlsx/xls。")),
          tags$h6(lang$t("3、点击“Analysis”/“Start drawing”进行分析。。")),
          tags$h6(lang$t("4、调节分析或图形参数。")),
          tags$h6(lang$t("5、地图可不用下载参考数据，直接点击运行后修改示例数据即可。")),
          tags$h6(lang$t("6、其他图形，更新中......") )
      )
    )  )
  
}

faqUI <-  function(id) {
  ns <- NS(id)
  fluidRow(box(width=12,
               title="常见问题",solidHeader=TRUE,status='primary',background = "white",height=800,
               tags$a(href="https://hiplot.com.cn/cloud-tool/hiker-land/list",h2("Hiplot: GraphMed")),
               tags$br(),
               tags$p("GraphMed 是由重医儿院研究生 彭炳明（Hiplot：豆皮喵）编写的一个数据分析及可视化的网页工具，
                    旨在提供简便易用、实用性高的数据分析与可视化服务，降低相关技能学习成本，
                    使得研究人员可将精力集中于“科学问题”而非“技能学习”，帮助其更加便捷、快速地处理数据。
                    部分工具上传至 Hiplot 平台（https://hiplot.com.cn），收到良好反馈，使用次数已过万。
                    对于使用中存在的困惑，可在我们实验室网站留言，也可通过 Hiplot 给我留言。
                    若有shiny 工具想布置在此处或感兴趣者，均可通过邮箱 2020111042@stu.cqmu.edu.cn 联系。"),
               tags$hr(),
               tags$h2("使用过程中的常见问题："),
               tags$br(),
               tags$p("1. 上传格式报错？地图上传数据框在哪里？"),
               tags$p("若上传格式报错，请下载并参照“参考数据”，测序数据尤其注意“ID”列，分组信息等。"),
               tags$p("若内容有中文，csv文件设置为“GB18030”编码格式。"),
               tags$p("地图不需要上传数据，先点击画图后，会有图形及画图数据，将画图数据的表格内容修改为自己的数据。"),
               tags$p("2. 是否有使用教程？"),
               tags$p("目前仅写有简单的“使用说明”，后续会找时间录制视频。"),
               tags$p("......")
  )
  )
  
}
