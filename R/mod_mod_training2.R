#' mod_training2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_training2_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mod_training2 Server Functions
#'
#' @noRd 
mod_mod_training2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mod_training2_ui("mod_training2_1")
    
## To be copied in the server
# mod_mod_training2_server("mod_training2_1")
