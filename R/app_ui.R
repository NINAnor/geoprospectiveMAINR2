#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      useShinyjs(),
      titlePanel(title =  div(img(src="www/wendy_logo.png", width ='120'), 'Geoprospective round 2'), windowTitle = "Geopros 2" ),
      tabsetPanel(id = "inTabset",
                  tabPanel(
                    title = "Load study", value = "p0",
                    h5("Please provide your study id that you received from the admin"),
                    br(),
                    textInput("site_id","Enter the site id from your invitation"),
                    br(),
                    uiOutput("cond_0")
                  ),
                  # tabPanel(
                  #   title = "instructions", value = "p1",
                  #   mod_mod_training2_ui("mod_training2_1")
                  # )
      ),
      uiOutput("tabs")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "geoprospectiveMAINR2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
