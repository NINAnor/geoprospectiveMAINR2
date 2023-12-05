#' mod_resultsR1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_resultsR1_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mod_resultsR1 Server Functions
#'
#' @noRd 
mod_mod_resultsR1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mod_resultsR1_ui("mod_resultsR1_1")
    
## To be copied in the server
# mod_mod_resultsR1_server("mod_resultsR1_1")
