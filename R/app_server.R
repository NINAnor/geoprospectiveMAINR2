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


con <- dbConnect(
  bigrquery::bigquery(),
  project = table_con$project,
  dataset = table_con$dataset,
  billing = table_con$billing
)
study_site<-tbl(con, "study_site")
studies<-study_site%>%select(siteID,siteTYPE,siteNMAPPING,siteCREATETIME)%>%collect()
studies$siteCREATETIME<-as.POSIXct(studies$siteCREATETIME)

user_conf<-tbl(con, "user_conf")
mapper<-tbl(con, "mapper")
mapper_mail<-left_join(user_conf,mapper,by="userID")
mapper_mail <- select(mapper_mail, userID, userMAIL, siteID)%>%filter(userMAIL!="")%>%collect()

labels <- c("low", "moderate", "intermediate", "high","very high")
cols   <- c("#e80909", "#fc8803", "#d8e03f", "#c4f25a","#81ab1f")
vis_ind <- list(min = 0, max = 1, palette = cols, values = labels)

bands <- list("landcover")
# rgee::ee_Initialize(user = 'reto.spielhofer@nina.no')

app_server <- function(input, output, session) {
  # Your application server logic
  
  hideTab(inputId = "inTabset", target = "p1")
  # check site id and email
  observeEvent(input$site_id,{
    if(input$site_id %in% mapper_mail$siteID){
      output$cond_0<-renderUI({
        tagList(
          textInput("user_mail","Enter the email you have been contacted with"),
          uiOutput("cond_1")
        )
      })}else{
        output$cond_0<-renderUI({
                    h5("invalid siteID, contact the administrator")
        })
    }
  })
  
  observeEvent(input$user_mail,{
    
    if(input$user_mail %in% mapper_mail$userMAIL){
      output$cond_1<-renderUI({
        actionButton("load","load_data")
      })
    }else{
      output$cond_1<-renderUI({
        h5("invalid email address, contact the administrator")
      })
    }
  })
  
  observeEvent(input$load,{
    rgee::ee_Initialize(user = 'reto.spielhofer@nina.no')
  })
  
  user_id<-eventReactive(input$load,{
    uid<-mapper_mail%>%filter(siteID==input$site_id & userMAIL == input$user_mail)%>%select(userID)
    user_id<-as.character(uid[1,])
    
  })
  
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
  })
  
  site_type<-eventReactive(input$load,{
    site_type<-as.character(studies%>%filter(siteID==input$site_id)%>%select(siteTYPE))
  })

  
  blog_dat_all<-eventReactive(input$load,{
    req(userES)
    userES<-userES()
    blog_dat<-tbl(con, "es_mappingR1")
    blog_dat_all<-blog_dat%>%select(userID,esID,siteID,blog)%>%collect()
    blog_dat_all<-blog_dat_all%>%filter(esID %in% userES$esID & siteID == input$site_id)

  })
  
  sf_stud_geom<-eventReactive(input$load,{
    # sf_stud_geom<-st_as_sf(studies%>%filter(studyID==input$study_id)%>%select(geometry))
    assetid <- paste0('projects/eu-wendy/assets/study_sites/', input$site_id)
    stud_geom <- ee$FeatureCollection(assetid)
    sf_stud_geom<-ee_as_sf(stud_geom)
  })

  coords<-eventReactive(input$load,{
    req(sf_stud_geom)
    sf_stud_geom<-sf_stud_geom()
    coords <- st_coordinates(sf_stud_geom)
    coords<-as.data.frame(coords[,c(1,2)])
  })
  
  comb<-eventReactive(input$load,{
    site_geom_ee<- paste0('projects/eu-wendy/assets/study_sites/', input$site_id)
    site_geom_ee <- ee$FeatureCollection(site_geom_ee)
    lulc <- ee$Image("COPERNICUS/CORINE/V20/100m/2018")
    lulc<-lulc$resample("bilinear")$reproject(crs= "EPSG:4326",scale=100)
    lulc<-lulc$clip(site_geom_ee)

    comb<-ee$Image$cat(lulc)
    
  })
  
  num_tabs<-eventReactive(input$load,{
    req(userES)
    userES<-userES()
    num_tabs<-as.numeric(nrow(userES))
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
                                        mod_mod_delphi_round2_ui(paste0("remapping_",i))
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
    sf_bound<-sf_stud_geom()
    comb<-comb()
    userID<-user_id()
    site_type<-site_type()
    blog_dat_all<-blog_dat_all()
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
                                         as.numeric(i),
                                         comb,
                                         bands,
                                         blog_dat_all,
                                         table_con,
                                         coords)
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
