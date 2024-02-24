
lungUI <- function(id) {
  ns <- NS(id)
  # shiny.i18n::usei18n(lang)
  tagList(
    box(title=lang$t("肺功能"),width=12, height = 700, background = "white",
        solidHeader=T,status='primary',
        tags$iframe(id="lungfunction",seamless="seamless", src="https://shiny.chcmu.com.cn/lung",
                    allowfullscreen="true", scrolling="auto", height="100%", width="100%")
    )
  )
}