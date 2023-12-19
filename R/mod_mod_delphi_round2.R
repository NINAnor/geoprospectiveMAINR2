#' mod_delphi_round2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_delphi_round2_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_es")),
    br(),
    fluidRow(
      column(5,
             uiOutput(ns("descr_es"))),
      column(2),
      column(5,
             # uiOutput(ns("image_es"))
             )
    ),
    br(),
    conditionalPanel(
      condition = "userES_sel$mapping == 'Yes'",
      ns=ns,
      textOutput(ns("text0")),
      br(),
      fluidRow(
        leafletOutput(ns("map_res_ind"))
      ),
      DTOutput(ns("blog")),
      br(),
      uiOutput(ns("remap_poss")),
      br(),
      uiOutput(ns("cond_b1"))
      
      
      # actionButton(ns("confirm"),'confirm')
    ),#/cond ui 1
    conditionalPanel(
      condition = "userES_sel$mapping == 'No'",
      ns = ns,
      textOutput(ns("text1")),
      br(),
      leafletOutput(ns("map_res_all")),
      br(),
      # do you want to initiallz map?
      uiOutput(ns("map_ini")),
      uiOutput(ns("cond_b2"))
    ),#/cond ui 2
 
  )
}

callback <- c(
  '$("#remove").on("click", function(){',
  '  table.rows(".selected").remove().draw();',
  '});'
)
    
#' mod_delphi_round2 Server Functions
#'
#' @noRd 
mod_mod_delphi_round2_server <- function(id, userES, sf_bound, vis_ind, vis_cv,vis_diff, mapping_round,comb, bands, table_con, coords, site_type){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## for the buttons
    rv1<-reactiveValues(
      u = reactive({})
    )
    
    #### get data:
    userID_sel<-unique(userES$userID)
    studyID<-unique(userES$siteID)
    
    ## make ee geom
    geometry <- ee$Geometry$Rectangle(
      coords = c(min(coords$X), min(coords$Y), max(coords$X), max(coords$Y)),
      proj = "EPSG:4326",
      geodesic = FALSE
    )
    
    userES_sel<-userES%>%dplyr::filter(userID == userID_sel)%>%slice(mapping_round)
    esID_sel<-userES_sel$esID
    
    #blog
    blog_dat<-tbl(con, "es_mappingR1")
    blog_dat_all<-blog_dat%>%select(userID,esID,siteID,blog)%>%collect()
    blog_dat_all<-blog_dat_all%>%filter(esID_sel %in% userES$esID & siteID == studyID)
    

    output$title_es<-renderUI(h3(userES_sel$esNAME))
    blog_dat_all<-blog_dat_all%>%dplyr::filter(esID %in% esID_sel & (blog !="NA"))%>%filter(blog!="")%>%select(blog)
    output$blog<-renderDT(blog_dat_all,rownames= FALSE, colnames="Why people choosed their sites")
    output$descr_es<-renderUI(userES_sel$esDESCR)
    
    output$remap<-renderUI(h4(paste0("Modify, add or delete areas that provide good ",userES_sel$esNAME)))
    output$es_quest_how<-renderUI(h6(paste0("How do you rate the quality of ",userES_sel$esNAME, " for your adjusted areas?")))
    
    # output$image_es<-renderUI({
    #   tags$figure(
    #     class = "centerFigure",
    #     tags$img(
    #       src = paste0(esID_sel,".jpg"),
    #       width = 600,
    #       alt = "Picture of an astragalus (bone die)"
    #     ),
    #     tags$figcaption("Image of Astragalus by Yaan, 2007")
    #   )
    # })#/output image
    
    ## here comes the convergence zone map from R1 (ADJUST this)
    # img_all_path<-paste0(ee_get_assethome(), '/es_map_all/',studyID,"_","userES_sel$esID", "_1")
    # img_all<-ee$Image(img_all_path)$select("probability")
    
    if(userES_sel$poss_mapping == "Yes"){
      
      img_assetid_ind<-"projects/eu-wendy/assets/es_mapping/es_map_ind/"
      img_assetid_all <- "projects/eu-wendy/assets/es_mapping/es_map_all/"
      
      img_ind_R1<-ee$Image(
        paste0(img_assetid_ind,studyID,"_",userES_sel$esID, "_", userID_sel, "_1")
      )

      img_CV1<-ee$Image(
        paste0(img_assetid_all,studyID,"_",userES_sel$esID, "CV_1")
      )
      img_CV1<-img_CV1$select("probability_stdDev")$updateMask(img_CV1$lte(0.5))
      

      
      Map$setCenter(mean(coords$X), mean(coords$Y),10)
      m1<-Map$addLayer(
        eeObject = img_ind_R1,
        vis_ind,
        opacity = 0.4,
        name = "Your map from round 1"
      )| Map$addLayer(
        eeObject = img_CV1,
        vis_cv,
        opacity = 0.4,
        name = "Areas of consensus round 1")+
        Map$addLegend(vis_ind, name =paste0("Probability to benefit from ",userES_sel$esNAME) , color_mapping = "character")
      # 
      output$text0<-renderText(paste0("In the previous mapping round you have mapped ", userES_sel$esNAME, ". The maps below show the areas of high probability to benefit form ",userES_sel$esNAME,
                                      " based on your individual contribution and all other participants contribution."))
      
      output$map_res_ind <- renderLeaflet({
        m1
      })
      ### ask if part wants to adjust polygons  
      output$remap_poss<-renderUI({
        tagList(
          h4(paste0("Do you want to adjust your areas for good ", userES_sel$esNAME,"?")),
          selectizeInput(ns("remap_poss"),label="",choices = c("Yes","No"),options = list(
            onInitialize = I('function() { this.setValue(""); }')
          ))
        )
      })
      ### render confirm btn as soon as selection of remapping has been made
      output$cond_b1<-renderUI({
        validate(
          need(input$remap_poss, 'Please select an option above')
        )
        actionButton(ns("confirm1"), "confirm")
      })
      
      # loading the individual polys  
      assetid <- paste0('projects/eu-wendy/assets/es_poly_ind/', studyID,"_",userES_sel$esID,"_",userID_sel,"_1")
      poly_r1 <- ee$FeatureCollection(assetid)
      poly_r1<-ee_as_sf(poly_r1)
      poly_r1 <- st_transform(
        poly_r1,
        crs = 4326
      )
      cent_poly <- st_centroid(poly_r1)
      # prepare map for editing
      
      map_edit<-leaflet(poly_r1)%>%
        addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0, group = "poly_r1")%>%
        addLabelOnlyMarkers(data = cent_poly,
                            lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = paste0("Your valuation in round 1: ",cent_poly$es_value),
                            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                        style = list(
                                                          "color" = "red",
                                                          "font-family" = "serif",
                                                          "font-style" = "bold",
                                                          "font-size" = "20px"
                                                        )))%>%
        addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 14))%>%
        leaflet.extras::addDrawToolbar(targetGroup='poly_r1',
                                       polylineOptions = F,
                                       polygonOptions = F,
                                       circleOptions = F,
                                       markerOptions = F,
                                       circleMarkerOptions = F,
                                       rectangleOptions = T,
                                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
                                       singleFeature = F)+
        m1
      
      
    }else{
      Map$setCenter(mean(coords$X), mean(coords$Y),10)
      m1<-Map$addLayer(
        eeObject = img_CV1,
        vis_cv,
        opacity = 0.4,
        name = "Areas of consensus round 1")+
        Map$addLegend(vis_ind, name =paste0("Probability to benefit from ",userES_sel$esNAME) , color_mapping = "character")
      
      
      output$map_res_all <- renderLeaflet({
        m1
      })
      output$text1<-renderText(paste0("In the previous mapping round you have not mapped ", userES_sel$esNAME))
      lable<-paste0("Based on the map you see, do you now feel able to map good spots for ", userES_sel$esNAME,"?")
      map_edit<-leaflet()
      
      
      ### ask if now mapping possible
      output$map_ini<-renderUI({
        selectizeInput(ns("map_ini"),label=lable,choices = c("Yes","No"),options = list(
          placeholder = '',
          onInitialize = I('function() { this.setValue(""); }')
        ))
      })
      
      ### based on that the confirm btn only when a selection has been made
      output$cond_b2<-renderUI({
        validate(
          need(input$input$map_ini, 'Please select an option above')
        )
        actionButton(ns("confirm2"), "proceed")
      })
    }#/if no mapping R1
    
    
    
    
    observeEvent(input$confirm2,{
      if(input$map_ini == "Yes"){
        #mapping light?
        insertUI(
          selector = paste0("#",ns("confirm2")),
          where = "afterEnd",
          ui=tagList(
            mod_map_light_ui(ns("newmap_R1"))
          )
        )
        ## trigger reactive value as a feedback of the maplight module
        rv1$u <- mod_map_light_server("newmap_R1",sf_bound, comb, bands, esID_sel, userID_sel, studyID, img_CV1, coords, vis_ind, userES_sel,table_con,mapping_round,site_type)
        
      }else{
        
        ## switch in main app and continue!
        insertUI(
          selector = paste0("#",ns("confirm2")),
          where = "afterEnd",
          ui=tagList(
            "We will use the data of experts as you have indicated in R1",
            br(),
            ### btn to exit the module and trigger the main app to change pages
            actionButton(ns("confirm_main"), "Next task", class='btn-primary')
          )
        )
        
        
      }
      removeUI(
        selector = paste0("#",ns("map_res_all"))
      )
      removeUI(
        selector = paste0("#",ns("text1"))
      )
      removeUI(
        selector = paste0("#",ns("confirm2"))
      )
      removeUI(
        selector = paste0("#",ns("map_ini"))
      )
    })
    
    ## should work for if not mapped in R1 and no mapping now, mapped in R1 and no adjustment now & for the adjustment procedure
    observeEvent(input$confirm_main,{
      rv1$u <-reactive({1})
    })
    
    ## initialize rv to store edits
    rv<-reactiveValues(
      edits = reactive({})
    )
    # edits<-mapedit::editMap(map_edit, targetLayerId = "poly_r1", record = T,sf = T,editor = c("leaflet.extras", "leafpm"))
    
    observeEvent(input$confirm1,{
      if(input$remap_poss == "Yes"){
        rv$edits<-callModule(
          module = editMod,
          leafmap = map_edit,
          id = "map_sel",
          targetLayerId = "poly_r1",
          record = T,
          sf = T,
          editor = c("leaflet.extras", "leafpm")) 
        
        
        output$blog2<-renderDT(blog_dat_all,rownames= FALSE, colnames="Blog entries")
        insertUI(
          selector = paste0("#",ns("confirm1")),
          where = "afterEnd",
          ui = tagList(
            uiOutput(ns("remap")),
            br(),
            editModUI(ns("map_sel")),
            htmlOutput(ns("overlay_result")),
            uiOutput(ns("btn1")),
            column(5,DTOutput(ns("blog2")),
            )
          ))
        
        
      }else{
        insertUI(
          selector = paste0("#",ns("confirm1")),
          where = "afterEnd",
          ui = tagList(
            "We will use your data from the previous round!",
            br(),
            actionButton(ns("confirm_main"), "Next task", class='btn-primary')
          )
        )
        
        
        ## write information to poly 2
        mapping_param <-
          list(
            esID = esID_sel,
            userID = userID_sel,
            siteID = studyID,
            area = as.integer(sum(st_area(poly_r1))),
            n_poly = as.integer(nrow(poly_r1)),
            blog = "test_blog",
            map_adjust = input$remap_poss,
            mapping_order = as.integer(mapping_round),
            extrap_RMSE = 0,
            extrap_accIMP = 0,
            extrap_lulcIMP = 0,
            extrap_natIMP = 0,
            edited = "No"
          )
        mapping_param<-as.data.frame(mapping_param)
        
        # write to bq
        insert_upload_job(table_con$project, table_con$dataset, "es_mappingR2", mapping_param)
        
      }#/else
      removeUI(
        selector = paste0("#",ns("remap_poss"))
      )
      removeUI(
        selector = paste0("#",ns("blog"))
      )
      removeUI(
        selector = paste0("#",ns("map_res_ind"))
      )
      removeUI(
        selector = paste0("#",ns("confirm1"))
      )
      removeUI(
        selector = paste0("#",ns("text0"))
      )
      
    })#/observeEvent
    
    ## test if edited polys intersect with each other or study area -- inform user!
    observe({
      req(rv$edits)
      # fin<-rv$edits()$finished
      # edi<-rv$edits()$finished
      rectangles <-rv$edits()$all
      
      if(site_type == "onshore"){
        resolution = 250^2
      }else{
        resolution = 500^2
      }
      
      n_poly<-nrow(as.data.frame(rectangles))
      
      #with res of 250m grid we can sample at least 10 pts with variaton within 0.6km2
      A_min<-resolution*sqrt(10)
      A_max<-0.05*round(as.numeric(st_area(sf_bound)),0)
      
      if(n_poly==1){
        n_within<-nrow(as.data.frame(st_within(rectangles,sf_bound)))
        if(n_within<n_poly){
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
          })
          removeUI(
            selector = paste0("#",ns("savepoly")))
        }else{
          area<-round(as.numeric(st_area(rectangles)),0)
          min_train<-min(area)
          max_train<-max(area)
          if(min_train<A_min & max_train<=A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Your area is too small<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))
            
          }else if(min_train>A_min & max_train>A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Your area is too big<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))
            
            
          }else{
            output$btn1<-renderUI(
              actionButton(ns("savepoly"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further polygons"
            })
            
          }
          
        }
        
      }else if (n_poly>1){
        n_within<-nrow(as.data.frame(st_within(rectangles,sf_bound)))
        n_inter<-nrow(as.data.frame(st_intersects(rectangles)))
        q=n_inter-n_poly
        if(q!=0 & n_within<n_poly){
          removeUI(
            selector = paste0("#",ns("savepoly")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b><li>Place your polygon completely into the the study area<li/><li>Remove overlays<li/></font>")
            
          })
        }else if(q==0 & n_within<n_poly){
          removeUI(
            selector = paste0("#",ns("savepoly")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Place your polygon completely into the the study area<li/></font>")
            
          })
        }else if(q!=0 & n_within==n_poly){
          removeUI(
            selector = paste0("#",ns("savepoly")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Remove overlays<li/></font>")
            
          })
        }else if(q==0 & n_within==n_poly){
          area<-round(as.numeric(st_area(rectangles)),0)
          min_train<-min(area)
          max_train<-max(area)
          if(min_train<A_min & max_train<=A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Your area is too small<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))
            
          }else if(min_train>A_min & max_train>A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the polygons:","</b> <li>Your area is too big<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("savepoly")))
            
            
          }else{
            output$btn1<-renderUI(
              actionButton(ns("savepoly"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further polygons"
            })
            
          }
        }
      }
      
    })
    
    final_edits<-eventReactive(input$savepoly,{
      ## get geometries R1 and make them not edited by default
      final_edits<-poly_r1
      final_edits$status<-rep(NA,nrow(final_edits))
      # replace delphi round
      final_edits$delphi_round<-rep(2,nrow(final_edits))
      final_edits<-final_edits%>%select(X_leaflet_id,delphi_round,esID,es_value,feature_type,studyID,userID,status)
      
      ## get edits R2
      r2_edits<-rv$edits()

      ##### removed geoms
      if(!is_empty(r2_edits$deleted)){
        p_del<-r2_edits$deleted%>%select(layerId)
        ## spatial join to obtain original layer ID
        p_del<-st_join(p_del,final_edits,st_intersects)
        leaf_id_remove<-as.vector(p_del$X_leaflet_id)
        final_edits<-final_edits%>%mutate(status =
                                            case_when(X_leaflet_id %in% leaf_id_remove ~"removed",
                                                      !X_leaflet_id %in% leaf_id_remove ~NA))
        
        
      }
      
      ### not edited
      p_n_edit<-r2_edits$all%>%filter(is.na(feature_type)&is.na(`_leaflet_id`))
      if(!is.null(p_n_edit)){
        p_n_edit<-st_join(p_n_edit,final_edits,st_intersects)
        leaf_id_n_edit<-p_n_edit$X_leaflet_id
        final_edits<-final_edits%>%mutate(status =
                                            case_when(X_leaflet_id %in% leaf_id_n_edit ~"not_edited",
                                                      !X_leaflet_id %in% leaf_id_n_edit ~status))
        
      }
      if(!is_empty(r2_edits$finished)){
        p_new<-r2_edits$finished
        p_new$X_leaflet_id<-p_new$`_leaflet_id`
        p_new$feature_type<-rep("rectangle",nrow(p_new))
        p_new$es_value<-rep(NA,nrow(p_new))
        p_new$esID<-rep(esID_sel,nrow(p_new))
        p_new$userID<-rep(userID_sel,nrow(p_new))
        p_new$studyID<-rep(studyID,nrow(p_new))
        p_new$mppng_r<-rep(mapping_round,nrow(p_new))
        p_new$delphi_round<-rep(2,nrow(p_new))
        p_new$drwng_r<-rep(NA,nrow(p_new))
        p_new$status<-rep("new_drawn",nrow(p_new))
        p_new<-p_new%>%select(X_leaflet_id,delphi_round,esID,es_value,feature_type,studyID,userID,status)
        final_edits<-rbind(final_edits,p_new)
        
        
      }
      ### new draw (just add)
      #new drawn polys
            
      ### edits
      ## the edited old geom:
      final_edits<-final_edits%>%mutate(status =
                                          case_when(is.na(status) ~"edits_old_geom",
                                                    !is.na(status) ~status))
      
      # the edits
      if("layerId" %in% colnames(r2_edits$all))
      {
        p_edits<-r2_edits$all%>%filter(layerId == "{ }")
        p_edits$X_leaflet_id<-p_edits$`_leaflet_id`
        p_edits$feature_type<-rep("rectangle",nrow(p_edits))
        p_edits$es_value<-rep(NA,nrow(p_edits))
        p_edits$esID<-rep(esID_sel,nrow(p_edits))
        p_edits$userID<-rep(userID_sel,nrow(p_edits))
        p_edits$studyID<-rep(studyID,nrow(p_edits))
        p_edits$mppng_r<-rep(mapping_round,nrow(p_edits))
        p_edits$delphi_round<-rep(2,nrow(p_edits))
        p_edits$drwng_r<-rep(NA,nrow(p_edits))
        p_edits$status<-rep("edits_new_geom",nrow(p_edits))
        p_edits<-p_edits%>%select(X_leaflet_id,delphi_round,esID,es_value,feature_type,studyID,userID,status)
        final_edits<-rbind(final_edits,p_edits)
      }
      final_edits<-final_edits
      
    })
    
    ## prepare the final edits, not changed, adjusted, new polys
    # final_edits<-eventReactive(input$savepoly,{
    #   final_edits<-poly_r1
    #   r2_edits<-rv$edits()
    #   
    #   
    #   ## indicate all R1 as old_geom
    #   final_edits$status<-rep("no_edit_R2",nrow(final_edits))
    #   # replace delphi round
    #   final_edits$delphi_round<-rep(2,nrow(final_edits))
    #   final_edits<-final_edits%>%select(X_leaflet_id,delphi_round,esID,es_value,feature_type,studyID,userID,status)
    #   
    #   
    #   if(!is_empty(r2_edits$all)){
    #     # for(i in 1: nrow(final_edits)){
    #     #   if(st_geometry(final_edits[i,]) %in% st_geometry(r2_edits$all)){
    #     #     final_edits[i,]$status<-"no_edit_R2"
    #     #   }else{
    #     #     final_edits[i,]$status<-"old_geom_r1"
    #     #   }#/if else
    #     # }#/for
    #     ##drawings
    #     if(!is.null(r2_edits$drawn)){
    #       for(k in 1:nrow(r2_edits$drawn)){
    #         if(st_geometry(r2_edits$drawn[k,])%in%st_geometry(r2_edits$all) & !st_geometry(r2_edits$drawn[k,])%in%st_geometry(final_edits)){
    #           p_new<-r2_edits$finished[k,]
    #           p_new<-r2_edits$drawn[k,]
    #           p_new$X_leaflet_id<-p_new$`_leaflet_id`
    #           p_new$feature_type<-"rectangle"
    #           p_new$es_value<-NA
    #           p_new$esID<-esID_sel
    #           p_new$userID<-userID_sel
    #           p_new$studyID<-studyID
    #           p_new$mppng_r<-mapping_round
    #           p_new$delphi_round<-2
    #           p_new$drwng_r<-NA
    #           p_new$status<-"new_drawn"
    #           p_new<-p_new%>%select(X_leaflet_id,delphi_round,esID,es_value,feature_type,studyID,userID,status)
    #           final_edits<-rbind(final_edits,p_new)
    #         }#/if geom is in final edits
    #       }#/for
    #     }#/not null edits
    #     if(!is.null(r2_edits$edited)){
    #       for(l in 1:nrow(r2_edits$edited)){
    #         if(st_geometry(r2_edits$edited[l,])%in%st_geometry(r2_edits$all)){
    #           p_edit<-r2_edits$edited[l,]
    #           p_edit<-r2_edits$edited[l,]
    #           p_edit$X_leaflet_id<-p_edit$`_leaflet_id`
    #           p_edit$feature_type<-"rectangle"
    #           p_edit$es_value<-NA
    #           p_edit$esID<-esID_sel
    #           p_edit$userID<-userID_sel
    #           p_edit$studyID<-studyID
    #           p_edit$mppng_r<-mapping_round
    #           p_edit$delphi_round<-2
    #           p_edit$drwng_r<-NA
    #           p_edit$status<-"edited"
    #           p_edit<-p_edit%>%select(X_leaflet_id,delphi_round,esID,es_value,feature_type,studyID,userID,status)
    #           final_edits<-rbind(final_edits,p_edit)
    #         }#/if geom is in final edits
    #       }#/for
    #     }#/not null edits
    #   }#/ not null any edits
    #   final_edits<-final_edits%>%filter(status!="old_geom_r1")
    #   final_edits
    # })

    
    ## only for drawings and new drawn polys:
    observeEvent(input$savepoly, {
      
      final_edits<-final_edits()
      ## not consider the deleted polys
      poly_values<-final_edits%>%dplyr::filter(status == "edits_new_geom" | status == "new_drawn" | status == "not_edited")
      #background map to display edited polys
      back_map1<-leaflet(sf_bound)%>%
        addProviderTiles(providers$CartoDB.Positron,options = tileOptions(minZoom = 10, maxZoom = 15))%>%
        addDrawToolbar(targetGroup='drawPoly',
                       polylineOptions = F,
                       polygonOptions = F,
                       circleOptions = F,
                       markerOptions = F,
                       circleMarkerOptions = F,
                       rectangleOptions = F,
                       singleFeature = FALSE,
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
      
      # create slider for edited, new and unchanged polys, but indicate that:
      cent_poly <- st_centroid(poly_values)
      tbl<-poly_values%>%st_drop_geometry()
      
      # final map with edited polys to adjust sliders
      output$map_res<-renderLeaflet(back_map1 %>%
                                      addPolygons(data=poly_values) %>%
                                      addLabelOnlyMarkers(data = cent_poly,
                                                          lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$X_leaflet_id,
                                                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                                      style = list(
                                                                                        "color" = "red",
                                                                                        "font-family" = "serif",
                                                                                        "font-style" = "bold",
                                                                                        "font-size" = "20px"
                                                                                      ))))
      
      
      output$slider <- renderUI({
        ns <- session$ns
        tagList(
          paste0("The Nr. of the slider refer to the number of the rectangle in the map"),
          br(),
          lapply(1:nrow(tbl),function(n){

            polynr <- tbl[n,]$X_leaflet_id
            id<-paste0("id_",polynr)
            R1_value <-tbl[n,]$es_value
            ## for new drawn poly (initial value)
            if(tbl[n,]$status == "edits_new_geom"){
              lable<-paste0("Polygon Nr: ",polynr, " has been edited in R2 - please rate the area")
              sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 1)
              
            }else if(tbl[n,]$status=="not_edited"){
              lable<-paste0("Polygon Nr: ",polynr, " has NOT been edited in R2 - please adjust your old rating if you want")
              sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = as.integer(tbl[n,]$es_value))
              
            }else{
              lable<-paste0("Polygon Nr: ",polynr, " was newly drawn in R2 - please rate the area")
              sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 1)
            }
            
            
          })
        )
        
        
      })#/slider
      
      insertUI(
        selector = paste0("#",ns("savepoly")),
        where = "afterEnd",
        ui = tagList(
          uiOutput(ns("es_quest_how")),
          br(),
          leafletOutput(ns("map_res")),
          br(),
          uiOutput(ns("slider")),
          br(),
          actionButton(ns("save_val"),"save values")
          
        )
      )
      
      removeUI(
        selector = paste0("#",ns("map_sel"),"-map"))
      removeUI(
        selector = paste0("#",ns("blog2"))
      )
      removeUI(
        selector = paste0("#",ns("savepoly"))
      )
      removeUI(
        selector = paste0("#",ns("remap"))
      )
      removeUI(
        selector = paste0("#",ns("overlay_result"))
      )
      
    })#/observeEvent
    

    observeEvent(input$save_val, {
      
      insertUI(
        selector = paste0("#",ns("save_val")),
        where = "afterEnd",
        ui = tagList(
          fluidRow(
            column(5,
                   leafletOutput(ns("gee_map1"))
            ),
            column(1),
            column(5,
                   leafletOutput(ns("gee_map2"))
            )
          ),#/row
          br(),
          
          ## conditional button as soon as selection has been made
          uiOutput(ns("map_quest_fin")),
          uiOutput(ns("cond_fin"))
          
        )#/taglist
      )#/inserUI
      
      
      removeUI(
        selector = paste0("#",ns("map_res"))
      )
      removeUI(
        selector = paste0("#",ns("es_quest_how"))
      )
      removeUI(
        selector = paste0("#",ns("slider"))
      )
      removeUI(
        selector = paste0("#",ns("save_val"))
      )
      
    })#/observeEvent

    ## save the values of the polys in bq - recalculation of map will be postprocessing R2...
    img_ind_R2<-eventReactive(input$save_val,{
      
      withProgress(message = "save your data",value = 0.1,{
        poly_all<-final_edits()
        poly_train<-poly_all%>%dplyr::filter(status == "edited" | status == "new_drawn" | status == "no_edit_R2")
        print("-------------ENTER MOD 2.2")
        poly_train<-as.data.frame(poly_train)
        poly_train$es_value<-rep(1,nrow(poly_train))
        sliderval<-list()
        
        # extract the values from the slider
        res<-lapply(1:nrow(poly_train),function(a){
          var<-paste0("id_",poly_train[a,]$X_leaflet_id)
          sliderval[[a]]<-input[[var]]
          return(sliderval)
        })
        vecA <- unlist(res)
        poly_train$es_value<-vecA
        
        # write attributes to geometry

        # poly_train$es_value <- vecA
        poly_train$esID <- rep(esID_sel,nrow(poly_train))
        poly_train$userID <- rep(userID_sel,nrow(poly_train))
        poly_train$studyID <- rep(studyID,nrow(poly_train))
        poly_train$delphi_round<-rep(2, nrow(poly_train))
        
        # print(poly_train$es_value)
        
        n_polys <-nrow(poly_train)
        poly_train<-st_as_sf(poly_train)
        
        
        poly_area<-as.numeric(sum(st_area(poly_train)))

        # make ee object and save
        gee_poly<-rgee::sf_as_ee(poly_train, via = "getInfo")

        assetId<-paste0("projects/eu-wendy/assets/es_poly_ind/",as.character(studyID),"_",as.character(esID_sel),"_",userID_sel,"_2")
        start_time<-Sys.time()

        task_tab <- ee_table_to_asset(
          collection = gee_poly,
          description = "test upload ind_area2",
          overwrite = T,
          assetId = assetId
        )
        
        task_tab$start()

        incProgress(amount = 0.2,message = "prepare training data")

        #cellsize
        if(site_type == "onshore"){
          resolution<-250^2
        }else{
          resolution<-500^2
        }
        
        
        ## N background (outside poly points) according to area of extrapolation
        A_roi<-as.numeric(st_area(sf_bound))
        
        # max pts for efficient extrapolation each 250x250 cell
        all_back_pts<- round(A_roi/resolution,0)
        
        ## although zooming on the map while drawing is limited, we assure that at least 10pts are within a poly
        min_in_pts<-10
        
        pts_out = st_sample(sf_bound, all_back_pts,type="random")
        
        # don`t allow intersection with polygons
        pts_out <- st_difference(st_combine(pts_out), st_combine(poly_train)) %>% st_cast('POINT')

        pts_out<-st_as_sf(pts_out)

        pts_out$inside<-rep(0,nrow(pts_out))
        
        print("-------------ENTER MOD 2.10")
        # inside pts are area + es value weighted
        for (i in 1:nrow(poly_train)) {
          print(i)
          A_tmp <- as.numeric(st_area(poly_train[i,]))
          print("-------------ENTER MOD 2.10.1")
          tmp_ratio<-A_tmp/A_roi
          print("-------------ENTER MOD 2.10.2")
          tmp_pts<-round(all_back_pts*tmp_ratio,0)
          print("-------------ENTER MOD 2.10.3")
          
          if(tmp_pts<=min_in_pts){
            tmp_pts<-min_in_pts
          }else{
            tmp_pts<-tmp_pts
          }
          print("-------------ENTER MOD 2.10.4")
          # npts in this poly must be max_pts*tmp_ratio*es_value
          tmp_pts = st_sample(poly_train[i,], tmp_pts*poly_train[i,]$es_value,type="random")
          print("-------------ENTER MOD 2.10.5")
          tmp_pts<-st_as_sf(tmp_pts)
          print("-------------ENTER MOD 2.10.6")
          tmp_pts$inside<-rep(1,nrow(tmp_pts))
          if(i==1){
            pts_in<-tmp_pts
          }else{
            pts_in<-rbind(pts_in,tmp_pts)
          }
          print("i done")
        }
        pts_ee<-rbind(pts_out,pts_in)
        print("-------------ENTER MOD 2.11")
        # ee object of sampling pts 6k pts = 7sec
        pts_ee<-rgee::sf_as_ee(pts_ee, via = "getInfo")
        
        
        # define target bands of comb (indep. var) and sample vars by pts
        pts_ee = comb$select(bands)$sampleRegions(collection= pts_ee,
                                                  properties = list("inside"),
                                                  geometries = T
        )
        
        ############ maxent
        incProgress(amount = 0.2,message = "calculate map")
        
        mEntclass = ee$Classifier$amnhMaxent()$train(
          features = pts_ee,
          classProperty = 'inside',
          inputProperties = bands
        )
        
        imageClassified = comb$select(bands)$classify(mEntclass)
        print("maxent done---")
        
        incProgress(amount = 0.4,message = "save your data")
        ## write information to poly 2
        mapping_param <-
          list(
            esID = esID_sel,
            userID = userID_sel,
            siteID = studyID,
            area = as.integer(sum(st_area(poly_train))),
            n_poly = as.integer(nrow(poly_train)),
            blog = "test_blog",
            map_adjust = as.character(input$remap_poss),
            mapping_order = as.integer(mapping_round),
            extrap_RMSE = 0,
            extrap_accIMP = 0,
            extrap_lulcIMP = 0,
            extrap_natIMP = 0,
            edited = "Yes"
          )
        incProgress(amount = 0.6,message = "save your data")
        
        mapping_param<-as.data.frame(mapping_param)
        
        # write to bq
        insert_upload_job(table_con$project, table_con$dataset, "es_mappingR2", mapping_param)
        
        
        img_ind_R2<-imageClassified$select("probability")
        
        ############ save map
        incProgress(amount = 0.7,message = "store your map")
        img_assetid <- paste0("projects/eu-wendy/assets/es_mapping/es_map_ind/",studyID,"_",esID_sel, "_", userID_sel, "_2")
        #
        # #set features of img
        img_ind_R2 <- img_ind_R2$set('esID', esID_sel,
                                     'userID', userID_sel,
                                     'siteID', studyID,
                                     'delphi_round', 2,
                                     'mapping_order', mapping_round)
        # # 
        start_time<-Sys.time()
        task_img <- ee_image_to_asset(
          image = img_ind_R2,
          assetId = img_assetid,
          overwrite = T,
          region = geometry
        )

        task_img$start()
        # 
        
        
      })#/progress ini
      return(img_ind_R2)
    })#/eventReactive prediction
    
    observe({
      req(img_ind_R2)
      img_ind_R2<-img_ind_R2()
      withProgress(message = "prepare interactive map 1",value = 0.8,{
        
        
        Map$setCenter(mean(coords$X), mean(coords$Y),10) 
        result<-Map$addLayer(
          eeObject = img_ind_R2,
          vis_ind,
          "Your new probability of ES",
          opacity = 0.4)|
          Map$addLayer(
            eeObject = img_ind_R1,
            vis_ind,
            "Your old probability of ES",
            opacity = 0.4)+
          Map$addLegend(vis_ind, name =paste0("Probability to benefit from ",userES_sel$esNAME) , color_mapping = "character",  position = "topright")
        
        incProgress(amount = 0.9,message = "prepare interactive map 2")
        
        ind_diff<-img_ind_R2$subtract(img_ind_R1)
        # ind_diff<-ind_diff$multiply(-1)
        
        Map$setCenter(mean(coords$X), mean(coords$Y),10)
        result2<- Map$addLayer(
          eeObject = ind_diff,
          vis_diff,
          "Difference between R1 - R2",
          opacity = 0.4)+
          Map$addLegend(vis_ind, name ="Relative difference new and old map" , color_mapping = "character", position = "topright")
        
        output$gee_map1 <- renderLeaflet({
          result
          # addControl(h3("Your new probability of ES"), position = "bottomleft", className="map-title")%>%
          # addControl(h3("Your old probability of ES"), position = "bottomright", className="map-title")
          
        })
        output$gee_map2 <- renderLeaflet({
          result2
        })
        incProgress(amount = 1,message = "done")
        
        ## render the confirm btn to proceed further in the main app
        output$map_quest_fin<-renderUI({
          req(img_ind_R2)
          h4(paste0("Which of your maps do you think represents the probability to benefit from ", userES_sel$esNAME," better?"))
          selectizeInput(ns("select_map"),label="",choices = c("Map round 1","Map round 2", "I don`t know"),options = list(
            onInitialize = I('function() { this.setValue(""); }')
          ))
          
          # actionButton(ns("confirm_main"), "Next task", class='btn-primary')
        })#/map quest fin
        
        output$cond_fin<-renderUI({
          req(img_ind_R2)
          validate(
            need(input$select_map, 'Please select an option above')
          )
          actionButton(ns("confirm_main"), "Next task", class='btn-primary')
        })
        
      })
    })
    ## the modules output is dep. on the status of reactive value rv1 (can be the map result or the btn status)
    cond <- reactive({rv1$u()})
    
    return(cond)
 
  })
}
    
## To be copied in the UI
# mod_mod_delphi_round2_ui("mod_delphi_round2_1")
    
## To be copied in the server
# mod_mod_delphi_round2_server("mod_delphi_round2_1")
