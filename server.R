server<- function(input, output, session) {
  
  options(digits = 1, big.mark = ",", scipen = 999)
  #####################  
  ##### Map ###########
  
  esri <- list("Esri","Esri.WorldImagery")
  l <- leaflet("mymap") %>%
    addTiles() %>% 
    setView(lng=-70.64724, lat=-33.47269 , zoom=7) %>% 
    addMarkers(reg_data$`1`,reg_data$`2`,popup=htmlEscape(reg_data$popup),group = "Regiones") %>% 
    addMarkers(provi_data$`1`,provi_data$`2`,popup=htmlEscape(provi_data$popup),group = "Provincias") %>% 
    addMarkers(comuna_data$`1`,comuna_data$`2`,popup=htmlEscape(comuna_data$popup),group = "Comunas") %>% 
    addMarkers(distrito_data$`X1`,distrito_data$`X2`,popup=htmlEscape(distrito_data$popup),group = "Distritos") %>% 
    addDrawToolbar(targetGroup = "drawnPoly", 
                   rectangleOptions = F, 
                   polylineOptions = F, 
                   markerOptions = F, 
                   editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                   circleOptions=F,
                   polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(stroke =T , fillColor="red",clickable = F,noClip = T ))) %>%
    addStyleEditor() %>% 
    addProviderTiles("Esri", group = "Esri") %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>% 
    addSearchOSM() %>%
    addLayersControl(baseGroups = esri,
                     overlayGroups = c("Regiones","Provincias","Comunas","Distritos"),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("Regiones","Provincias","Comunas","Distritos")) %>% 
    addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
               position = "bottomleft", minimized = T) %>%
    htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
   
  
  output$mymap <- renderLeaflet({
    l
  })
  
  
  latlongs<-reactiveValues()   #temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  #########
  #empty reactive spdf
  tablas <- reactiveValues()
  value<-reactiveValues()
  SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))->value$drawnPoly
  #fix the polygon to start another
  
  observeEvent(input$mymap_draw_new_feature, {
    
    coor<-unlist(input$mymap_draw_new_feature$geometry$coordinates)
    
    Longitude<-coor[seq(1,length(coor), 2)] 
    
    Latitude<-coor[seq(2,length(coor), 2)]
    
    isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
    
    poly<-Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
    polys<-Polygons(list(poly),    ID=input$mymap_draw_new_feature$properties$`_leaflet_id`)
    spPolys<-SpatialPolygons(list(polys))
    
    
    #
    value$drawnPoly<-rbind(value$drawnPoly,SpatialPolygonsDataFrame(spPolys, 
                                                                    data=data.frame(notes=NA, row.names=
                                                                                      row.names(spPolys))))
    
    
    ###plot upon ending draw
    observeEvent(input$mymap_draw_stop, {
      
      #replot it - take off the DrawToolbar to clear the features and add it back and use the values from the SPDF to plot the polygons
      leafletProxy('mymap') %>%  
        removeDrawToolbar(clearFeatures=TRUE) %>% 
        removeShape('temp') %>% 
        clearGroup('drawnPoly') %>% 
        addPolygons(data=value$drawnPoly,group='drawnPoly',color="blue", layerId=row.names(value$drawnPoly)) %>% 
        
        addDrawToolbar(targetGroup = "drawnPoly", 
                       rectangleOptions = F, 
                       polylineOptions = F, 
                       markerOptions = F, 
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                       circleOptions=F,
                       polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=T, shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE)))
      
    })
    
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df
    
  })
  
  ########################
  ### edit polygons / delete polygons
  
  observeEvent(input$mymap_draw_edited_features, {
    
    f <- input$mymap_draw_edited_features
    
    coordy<-lapply(f$features, function(x){unlist(x$geometry$coordinates)})
    
    Longitudes<-lapply(coordy, function(coor) {coor[seq(1,length(coor), 2)] })
    
    Latitudes<-lapply(coordy, function(coor) { coor[seq(2,length(coor), 2)] })
    
    polys<-list()
    for (i in 1:length(Longitudes)){polys[[i]]<- Polygons(
      list(Polygon(cbind(Longitudes[[i]], Latitudes[[i]]))), ID=f$features[[i]]$properties$layerId
    )}
    
    spPolys<-SpatialPolygons(polys)
    
    
    SPDF<-SpatialPolygonsDataFrame(spPolys, 
                                   data=data.frame(notes=value$drawnPoly$notes[row.names(value$drawnPoly) %in% row.names(spPolys)], row.names=row.names(spPolys)))
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% row.names(SPDF),]
    value$drawnPoly<-rbind(value$drawnPoly, SPDF) 
    
    #long_input <- value$DrawnPoly@bbox[1,1] 
    #lat_input <- value$DrawnPoly@bbox[2,1]
  })
  
  observeEvent(input$mymap_draw_deleted_features, { 
    
    f <- input$mymap_draw_deleted_features
    
    ids<-lapply(f$features, function(x){unlist(x$properties$layerId)})
    
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% ids ,] 
    
  }) 
  
  
  #####################  
  ##### Data upload ###  
  
  
  
  observeEvent(input$file,{
      path <-  input$file$datapath
      value$poly_2 <- readOGR(path)
      if(length(value$drawnPoly)<=0){
        value$drawnPoly <- value$poly_2
        m <- leaflet("mymap") %>%
          setView(lng=-70.395401, lat=-23.6523609 , zoom=10) %>% 
          #addPolygons(data = r2_dc_shp, color="orange",group = "Distritos") %>% 
          addStyleEditor() %>% 
          addProviderTiles("Esri", group = "Esri") %>%
          addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>% 
          addSearchOSM() %>%
          addLayersControl(baseGroups = esri,
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
                     position = "bottomleft", minimized = T) %>%
          htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }") %>% 
          addPolygons(data = value$drawnPoly, color="blue") %>% 
          setView(lng=value$drawnPoly@bbox[1,1], lat=value$drawnPoly@bbox[2,1] , zoom=10)
        output$mymap <- renderLeaflet({
          m
        })
      } else {
        reset("file")
        showModal(
          modalDialog(
            title = 'Error',
            p("No se puede cargar un polígono cuando ya dibujaste uno.")
          )
        )
      }
  })
  
  
  observeEvent(input$init, {
    mensajes <- c("No has dibujado el polígono, eres estúpido y tu mamá es guatona.", 
                  "No has dibujado polígono, trata de no ser tan tonto para la otra.",
                  "El archivo word está vacío, como yo por dentro, es porque no dibujaste el polígono.",
                  "No ɥɐs pᴉqnɾɐpo ǝl dolíƃouo˙",
                  "You have not dibujaded the polígono.",
                  "No has dibujado el polígono.",
                  "Retard alert!",
                  "La próxima vez que ejecutes esto, y no haya polígono, un gatito morirá, y será por tu culpa.",
                  "'Hombre local no sabe dibujar un polígono.'",
                  "Sigue intentandolo, puede que no hayas dibujado un polígono, pero este mensaje no es para insultarte.")
    if (length(value$drawnPoly@polygons)==0) {
      u <- runif(1,0,1)
      ev<-case_when(u>=0.2 & u<0.75 ~ mensajes[6],
                    u>=0 & u<0.5 ~ mensajes[1],
                    u>=0.5 & u<0.1 ~ mensajes[10],
                    u>=0.1 & u<0.15 ~ mensajes[7],
                    u>=0.15 & u<0.2 ~ mensajes[2],
                    u>=0.75 & u<0.8~mensajes[3],
                    u>=0.8 & u<0.85~mensajes[4],
                    u>=0.85 & u<0.9~mensajes[5],
                    u>=0.9 & u<0.95~mensajes[8],
                    u>=0.95 & u<1~mensajes[9])
      showModal(
        modalDialog(
          title = 'Error',
          p(ev)
        )
      )
    } else {
      shinyjs::runjs("document.getElementById('downloadData').click();")
    }
  })
  
  
################### 
#### Download #####

  pais <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/shps/all_in_one.rds")
  
  output$downloadData<-downloadHandler(
    filename = 'tabla_mkd2.docx',
    content = function(file) {
      withProgress(message = 'Creando documento, por favor espere', value = 0.1, {
        Sys.sleep(0.1)
      if (length(Sys.glob("tabla_mkd2.docx"))>0){
        file.remove(Sys.glob("tabla_mkd2.docx"))
      }
      if (length(Sys.glob("shpExport1.*"))>0){
        file.remove(Sys.glob("shpExport1.*"))
      }
        if (length(Sys.glob("shpExport2.*"))>0){
          file.remove(Sys.glob("shpExport2.*"))
        }
        if (length(Sys.glob("shpExport3.*"))>0){
          file.remove(Sys.glob("shpExport3.*"))
        }
        incProgress(0.15)
      new_t<-isolate(value$drawnPoly)
      proj4string(new_t)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      new_t<-raster::intersect(new_t,pais)

      viviendas <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/vivienda.rds")
      viviendas$ZC_LOC <- viviendas$ZC_LOC %>% as.numeric()
      viviendas$DC <- viviendas$DC %>% as.factor()
      viviendas$COMUNA <- viviendas$COMUNA %>% as.factor()
      viviendas$PROVINCIA <- viviendas$PROVINCIA %>% as.factor()
      viviendas$REGION <- viviendas$REGION %>% as.factor()
      casen <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/casen.rds")
      casen$COMUNA <- as_factor(casen$COMUNA)
      v <- isolate(input$inSelect)
      incProgress(0.3)
      if(v=="ZC_LOC"){
        personas <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas.rds")
        personas$COMUNA <- personas$COMUNA %>% as.factor()
        tt <- new_t@data %>%
        mutate(ZC_LOC=as.numeric(LOC_ZON))
        
        tablas$personas <- tt %>%
          inner_join(personas, by=c("COMUNA","DISTRITO","ZC_LOC","AREA"))
        tablas$viviendas <- tt %>% 
          inner_join(viviendas, by=c("COMUNA","DISTRITO","ZC_LOC"))
      }else{
        if(v=="DC"){
          personas <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_dc.rds")
          personas$COMUNA <- personas$COMUNA %>% as.factor()
          tt <- new_t@data %>% 
            group_by(COMUNA,NOM_COMUNA,DISTRITO) %>% 
            summarise()
          
          tablas$personas <- tt %>% 
            inner_join(personas, by=c("COMUNA","DISTRITO"))
          tablas$viviendas <- tt %>% 
            inner_join(viviendas, by=c("COMUNA","DISTRITO"))
        }else{
          if(v=="COMUNA"){
            personas <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_comuna.rds")
            personas$COMUNA <- personas$COMUNA %>% as.factor()
            tt <- new_t@data %>% 
              group_by(REGION,NOM_REGION,COMUNA,NOM_COMUNA) %>% 
              summarise()
            
            tablas$personas <- tt %>% 
              inner_join(personas, by=c("COMUNA"))
            tablas$viviendas <- tt %>% 
              inner_join(viviendas, by=c("COMUNA"))
          }else{
            if(v=="PROVINCIA"){
              personas <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_provincia.rds")
              personas$REGION <- personas$REGION %>% as.factor()
              personas$PROVINCIA <- personas$PROVINCIA %>% as.factor()
              
              tt <- new_t@data %>% 
                group_by(REGION,NOM_REGION,PROVINCIA,NOM_PROVIN) %>% 
                summarise()
                
              
              tablas$personas <- tt %>% 
                inner_join(personas, by=c("REGION","PROVINCIA"))
              tablas$viviendas <- tt %>% 
                inner_join(viviendas, by=c("REGION","PROVINCIA"))
            }else{
              personas <- read_rds("C:/Users/M/Desktop/GAC/APP MH/APP/data/esencial/personas_region.rds")
              personas$REGION <- personas$REGION %>% as.factor()
              tt <- new_t@data %>% 
                group_by(REGION,NOM_REGION) %>% 
                summarise()
              
              tablas$personas <- tt %>% 
                inner_join(personas, by=c("REGION"))
              tablas$viviendas <- tt %>% 
                inner_join(viviendas, by=c("REGION"))
            }
          }
        }
      }
      incProgress(0.4)  
      
      tablas$casen <- new_t@data %>% 
        right_join(casen, by=c("COMUNA"))
      saveRDS(tablas$personas, "shpExport1.rds", compress=F)
      saveRDS(tablas$viviendas, "shpExport2.rds", compress=F)
      saveRDS(tablas$casen, "shpExport3.rds", compress=F)
      rm(new_t,personas,casen,viviendas,pais)
      incProgress(0.5)
      Sys.setenv(RSTUDImemO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
      if(input$info){
        if(plyr::empty(tablas$personas)==F){
          doc1 <- read_docx("C:/Users/M/Desktop/GAC/APP MH/APP/mapa/prueba.docx")
          rmarkdown::render("tabla_mkd.Rmd")
          doc1 %>% 
            body_add_docx("C:/Users/M/Desktop/GAC/APP MH/APP/mapa/tabla_mkd.docx") %>% 
            print(target = "tabla_mkd2.docx")
        } else {
          doc1 <- read_docx("C:/Users/M/Desktop/GAC/APP MH/APP/mapa/prueba.docx")
          rmarkdown::render("tabla_mkd_no_ppl.Rmd")
          doc1 %>% 
            body_add_docx("C:/Users/M/Desktop/GAC/APP MH/APP/mapa/tabla_mkd_no_ppl.docx") %>% 
            print(target = "tabla_mkd2.docx")
        }
      } else {
        rmarkdown::render("tabla_mkd2.Rmd")
      }
      file.copy("tabla_mkd2.docx", file)
      if (length(Sys.glob("tabla_mkd2.docx"))>0){
        file.remove(Sys.glob("tabla_mkd2.docx"))
      }
      if (length(Sys.glob("shpExport1.*"))>0){
        file.remove(Sys.glob("shpExport1.*"))
      }
      if (length(Sys.glob("shpExport2.*"))>0){
        file.remove(Sys.glob("shpExport2.*"))
      }
      if (length(Sys.glob("shpExport3.*"))>0){
        file.remove(Sys.glob("shpExport3.*"))
      }        
      setProgress(1)
      reset('file')
      output$mymap <- renderLeaflet({
        l 
      })
      value$drawnPoly <- SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))
      })
    }
  )
}