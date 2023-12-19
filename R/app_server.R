#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @import dplyr
#' @import sf
#' @import stringi
#' @import rgee
#' @import leaflet
#' @import leafem
#' @import leaflet.extras
#' @import mapedit
#' @import bigrquery
#' @import DT
#' @import shinycssloaders
#' @import tibble
#' @import leafpop
#' @import mapview
#' @import leaflet.extras2
#' @import shinyWidgets
#' @import tidyverse
#' @import shinyBS
#' @import shinyWidgets
#' @import purrr
#' @noRd

bq_auth(
  path = "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/eu-wendy-92974cdf189d.json"
)
mode<-"integrated"
project<-"eu-wendy"
table_con<-data.frame(
  project = project,
  dataset = paste0(mode,"_wendy"),
  billing =project
)

# rgee::ee_Initialize(user = 'reto.spielhofer@nina.no')


con <- dbConnect(
  bigrquery::bigquery(),
  project = table_con$project,
  dataset = table_con$dataset,
  billing = table_con$billing
)


labels <- c("low", "moderate", "intermediate", "high","very high")
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
cols_diff   <- c("#81ab1f", "#c4f25a", "#d8e03f", "#fc8803","#e80909")

cols_cv   <- c("#81ab1f","#fc8803")
vis_ind <- list(min = 0, max = 1, palette = cols, values = labels)

vis_cv <- list(min = 0, max = 0.5, palette = cols_cv)
vis_diff <- list(min = -1, max = 1, palette = cols_diff)


# rgee::ee_Initialize(user = 'reto.spielhofer@nina.no')

app_server <- function(input, output, session) {
  # Your application server logic
  
  hideTab(inputId = "inTabset", target = "p1")

  studies<-eventReactive(input$check_site,{
    study_site<-tbl(con, "study_site")
    studies<-study_site%>%select(siteID,siteTYPE,siteNMAPPING,siteCREATETIME,siteSTATUS)%>%collect()
    # studies$siteCREATETIME<-as.POSIXct(studies$siteCREATETIME)
  })
  
  observeEvent(input$check_site,{
    req(studies)
    studies<-studies()
    if(input$site_id %in% studies$siteID){
      studies<-studies%>%filter(siteID == input$site_id)%>%
        arrange(desc(as.POSIXct(siteCREATETIME)))%>%first()
      if(studies$siteSTATUS == "round2_open"){
        removeUI("#check_site")
        output$cond_0<-renderUI({
          tagList(
            textInput("user_mail","Enter the email you have been contacted with"),
            actionButton("check_mail","check mail"),
            uiOutput("cond_1")
          )
        })
        
      }else{
        output$cond_0<-renderUI({
          h5("Second Delphi round is not open yet, contact the administrator")
        })
      }
    }else{
        output$cond_0<-renderUI({
                    h5("siteID not found, contact the administrator")
        })
    }
  })
  

  user_conf<-eventReactive(input$check_mail,{
    user_conf<-tbl(con, "user_conf")%>%collect()
    user_conf<-user_conf%>%filter(siteID == input$site_id)
  })
  
  observeEvent(input$check_mail,{
    req(user_conf)
    user_conf<-user_conf()
    
    if(input$user_mail %in% user_conf$userMAIL){
      removeUI("#check_mail")
      output$cond_1<-renderUI({
        actionButton("load","load your data")
      })
    }else{
      output$cond_1<-renderUI({
        h5("invalid email address for this study, contact the administrator")
      })
    }
  })
  
  
  ###load study

  observeEvent(input$load,{
    rgee::ee_Initialize(user = 'reto.spielhofer@nina.no')
  })
  
  user_id<-eventReactive(input$load,{
    user_conf<-user_conf()
    user_conf<-user_conf%>%filter(userMAIL == input$user_mail)
    user_id<-as.character(user_conf$userID)
    
  })
  
  print("A")
  
  userES<-eventReactive(input$load,{
    req(user_id)
    user_id<-user_id()
    mapping<-tbl(con, "es_mappingR1")
    # join the rest of the es infos from the es_descr table
    es_descr<-tbl(con,"es_descr")
    # ind_es_map<-left_join(ind_es_map,es_descr,by="esID")
    
    userES<-mapping%>%select(userID, esID, siteID, poss_mapping, expert_trust)%>%
      filter(userID==user_id)%>%
      left_join(es_descr,by="esID")%>%collect()
    userES<-as.data.frame(userES)
    #shuffle rows randomly that not all the participants have the same order of mapping es
    # userES<-userES[sample(nrow(userES)),]
  })
  
  site_type<-eventReactive(input$load,{
    req(studies)
    studies<-studies()
    site_type<-as.character(studies%>%filter(siteID==input$site_id)%>%select(siteTYPE)%>%first())
  })
  

  sf_bound<-eventReactive(input$load,{
    assetid <- paste0('projects/eu-wendy/assets/study_sites/', input$site_id)
    bound <- ee$FeatureCollection(assetid)
    sf_bound<-ee_as_sf(bound)
  })
 
  coords<-eventReactive(input$load,{
    req(sf_bound)
    sf_bound<-sf_bound()
    coords <- st_coordinates(sf_bound)
    coords<-as.data.frame(coords[,c(1,2)])
  })

    comb<-eventReactive(input$load,{
    req(site_type)
    site_type<-site_type()
    site_geom_ee<- paste0('projects/eu-wendy/assets/study_sites/', input$site_id)
    site_geom_ee <- ee$FeatureCollection(site_geom_ee)
    if(site_type == "onshore"){
      # landcover
      on_lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
      on_lulc<-on_lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
      on_lulc<-on_lulc$clip(site_geom_ee)$rename("on_lulc")
      
      #Hill et al., 2022: ecosystem integrity (structure, composition, and function against current actual potential as baseline) (0-1)
      on_int<-"projects/eu-wendy/assets/ON_INT"
      on_int<-ee$Image(on_int)
      on_int<-on_int$clip(site_geom_ee)
      on_int<-on_int$resample("bilinear")$reproject(crs= "EPSG:4326")
      on_int<-on_int$rename("on_int")
      
      # global friction surface (land-based accessibility using motorized vehicles)
      on_acc = ee$Image('Oxford/MAP/friction_surface_2019')$select("friction")
      on_acc<-on_acc$resample("bilinear")$reproject(crs= "EPSG:4326")
      on_acc<-on_acc$clip(site_geom_ee)$rename("on_acc")
      
      comb<-ee$Image$cat(on_lulc,on_int,on_acc)
    }else{
      
      #bathymetry
      off_bat = ee$Image('NOAA/NGDC/ETOPO1')$select('bedrock')
      off_bat<-off_bat$resample("bilinear")$reproject(crs= "EPSG:4326")
      off_bat<-off_bat$clip(site_geom_ee)
      
      ## lulc sea
      off_lulc<-"projects/eu-wendy/assets/OFF_LULC"
      off_lulc<-ee$Image(off_lulc)
      off_lulc<-off_lulc$clip(site_geom_ee)
      off_lulc<-off_lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=1000)
      off_lulc<-off_lulc$rename("off_lulc")
      
      # dist coast
      off_acc<-"projects/eu-wendy/assets/OFF_ACC"
      off_acc<-ee$Image(off_acc)
      off_acc<-off_acc$clip(site_geom_ee)
      off_acc<-off_acc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=1000)
      off_acc<-off_acc$rename("off_acc")
      
      #Human Impacts to Marine Ecosystems
      off_nat<-"projects/eu-wendy/assets/OFF_NAT"
      off_nat<-ee$Image(off_nat)
      off_nat<-off_nat$clip(site_geom_ee)
      off_nat<-off_nat$resample("bilinear")$reproject(crs= "EPSG:4326",scale=1000)
      off_nat<-off_nat$rename("off_nat")
      
      comb<-ee$Image$cat(off_bat,off_lulc,off_acc,off_nat)
    }
  })

  bands<-eventReactive(input$load,{
    req(site_type)
    site_type<-site_type()
    
    if(site_type == "onshore"){
      bands<-list("on_lulc","on_int","on_acc")
    }else{
      bands<-list("off_bat","off_lulc","off_ac","off_nat")
    }
    
    
  })
  
  num_tabs<-eventReactive(input$load,{
    req(userES)
    userES<-userES()
    num_tabs<-as.integer(nrow(userES))
  })
  
### create N remapping tabs
  # print(num_tabs)
  observeEvent(input$load,{
    req(num_tabs)
    hideTab(inputId = "inTabset",
            target = "p0")
    num_tabs<-num_tabs()
    output$tabs <- renderUI({
      do.call(tabsetPanel, c(id="tabs_content",
                             lapply(1:num_tabs, function(i) {
                               tabPanel(title = paste("Remapping ", i), value = paste0("remap_", i),
                                        mod_mod_delphi_round2_ui(paste0("remapping_",i))%>% withSpinner()
                                        # h4("test")

                               )#/tabpanel
                             })#/lapply
      ))#/do.call
    })#/UI render
  })

  ## hide tabs
  observeEvent(input$tabs_content, {
    num_tabs<-num_tabs()
    userES<-userES()
    sf_bound<-sf_bound()
    comb<-comb()
    bands<-bands()
    userID<-user_id()
    site_type<-site_type()
    coords<-coords()


    for (i in 2:num_tabs) {
      runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
    }

    lapply(1:num_tabs, function(i) {
      rv<-reactiveValues(
        a = reactive({})
      )

      rv$a<-mod_mod_delphi_round2_server(paste0("remapping_",i),
                                         userES,
                                         sf_bound,
                                         vis_ind,
                                         vis_cv,
                                         vis_diff,
                                         as.numeric(i),
                                         comb,
                                         bands,
                                         table_con,
                                         coords,
                                         site_type
                                         )
      #reactive value from module as event
      observeEvent(rv$a(), {
        next_tab <- i+1
        runjs(paste("$('.nav-tabs li:nth-child(", next_tab, ")').show();"))
        if(next_tab<=num_tabs){
          updateTabsetPanel(session, "tabs_content", selected=paste0("remap_",next_tab) )
          runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
        }else{
          removeUI("#tabs")
          output$ahp_group<-renderUI({
            tagList(
              # mod_ahp_group_ui("ahp_group_1")
              h4("Done!")
            )
          })
        }

      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  }, once = TRUE)

}
